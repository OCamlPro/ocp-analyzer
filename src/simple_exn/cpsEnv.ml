(**************************************************************************)
(*                                                                        *)
(*   Typerex Tools                                                        *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU General Public License version 3 described in the file       *)
(*   LICENSE.                                                             *)
(*                                                                        *)
(**************************************************************************)

open Utils
open Common_types
open Map_utils

module Make (ProgId: Set.OrderedType) =
struct
  module XIdm = Map.Make(ProgId)
  module I = Cps.Ints

  exception Mismatching_args of string

  let mismatch str =
    raise (Mismatching_args str)

  type t = Cps.t XIdm.t

  let bottom = XIdm.empty

  let empty = bottom

  let is_bottom = XIdm.is_empty

  let set_top i m =
    (* This is a bit problematic, but the ints domain should take care of
       doing things properly *)
    XIdm.remove i m

  let set_bottom i m =
    XIdm.remove i m

  let var_is_bottom i m =
    try
      let v = XIdm.find i m in
      Cps.is_bottom v
    with
    | Not_found -> true

  let meet x y = failwith "Meet unimplemented"

  let join =
    XIdm.merge (map_merge_join Cps.join)

  let widening =
    XIdm.merge (map_merge_join Cps.widening)

  let leq =
    let module M =
      MapLeq (struct
        module M = XIdm type v = Cps.t let vleq = Cps.leq
      end)
    in
    M.comp

  let unary_prim func i m arg =
    try
      let v_arg = XIdm.find arg m in
      let v_i = I.fold (fun n acc -> I.add (func n) acc) v_arg I.empty in
      XIdm.add i v_i m
    with
    | Not_found -> m

  let binary_prim func i m arg1 arg2 =
    try
      let v_1 = XIdm.find arg1 m in
      let v_2 = XIdm.find arg2 m in
      let v_i =
        I.fold
          (fun n1 acc ->
             I.fold
               (fun n2 acc ->
                  I.add (func n1 n2) acc)
               v_2
               acc)
          v_1
          I.empty
      in
      XIdm.add i v_i m
    with
    | Not_found -> m

  let primitive i m p args =
    (* We want to handle arithmetic on constant pointers,
       but we do not want to track arbitrary values.
       To do this, if variable i is already defined, it will be removed from
       the map. (Technically, this means setting the variable to bottom,
       and is unsound, but hopefully the variable is also tracked in the
       integer domain. This whole bottom semantics for absent values is getting
       annoying.) *)
    if not (var_is_bottom i m) then
      XIdm.remove i m
    else
      let cond_add b n acc = if b then I.add n acc else acc in
      match p, args with
      | XPnegint, [j] -> unary_prim (~-) i m j
      | XPaddint, [j;k] -> binary_prim (+) i m j k
      | XPsubint, [j;k] -> binary_prim (-) i m j k
      | XPmulint, [j;k] -> binary_prim ( * ) i m j k
      | XPdivint, [j;k] -> binary_prim (/) i m j k
      | XPmodint, [j;k] -> binary_prim (mod) i m j k
      | XPandint, [j;k] -> binary_prim (land) i m j k
      | XPorint, [j;k] -> binary_prim (lor) i m j k
      | XPxorint, [j;k] -> binary_prim (lxor) i m j k
      | XPlslint, [j;k] -> binary_prim (lsl) i m j k
      | XPlsrint, [j;k] -> binary_prim (lsr) i m j k
      | XPasrint, [j;k] -> binary_prim (asr) i m j k
      | XPintcomp c, [j;k] ->
        begin
          try
            let v1 = XIdm.find j m in
            let v2 = XIdm.find k m in
            let exists2 binop =
              I.exists (fun n1 -> I.exists (fun n2 -> binop n1 n2) v2) v1
            in
            let open Lambda in
            let has_true, has_false =
              match c with
              | Ceq -> exists2 (=), exists2 (<>)
              | Cneq -> exists2 (<>), exists2 (=)
              | Clt -> exists2 (<), exists2 (>=)
              | Cgt -> exists2 (>), exists2 (<=)
              | Cle -> exists2 (<=), exists2 (>)
              | Cge -> exists2 (>=), exists2 (<)
            in
            let v_i = cond_add has_true 1 (cond_add has_false 0 I.empty) in
            XIdm.add i v_i m
          with
          | Not_found -> m
        end
      | XPoffsetint off, [j] -> unary_prim ((+) off) i m j
      | XPisout, [j;k] ->
        begin
          try
            let v1 = XIdm.find j m in
            let v2 = XIdm.find k m in
            let exists2 binop =
              I.exists (fun n1 -> I.exists (fun n2 -> binop n1 n2) v2) v1
            in
            let has_true = exists2 (fun n1 n2 -> n1 < 0 || n1 > n2) in
            let has_false = exists2 (fun n1 n2 -> n1 >= 0 && n1 <= n2) in
            let v_i = cond_add has_true 1 (cond_add has_false 0 I.empty) in
            XIdm.add i v_i m
          with
          | Not_found -> m
        end
      | XPnot, [j] ->
        (* Assuming 0 for false and 1 for true *)
        begin
          try
            let v = XIdm.find j m in
            let has_true = I.mem 0 v in
            let has_false = I.mem 1 v in
            let v_i = cond_add has_true 1 (cond_add has_false 0 I.empty) in
            XIdm.add i v_i m
          with
          | Not_found -> m
        end
      | _ -> failwith "CpsEnv: Unsupported primitive"

  let var i m j =
    try XIdm.add i (XIdm.find j m) m
    with Not_found -> m

  let const i m cst =
    let open Lambda in
    match cst with
    | Const_base (Asttypes.Const_int cp)
    (* Constant ints can also be generated for
       expressions (or patterns) of variant type *)
    | Const_pointer cp ->
      XIdm.add i (I.singleton cp) m
    | _ -> m (* Unsupported constant *)

  let constr i m cstr =
    match cstr with
    | Ccp cp ->
      (* Note: In an ideal world, we shouldn't have to specifically test for
         bottom or explicitly return bottom, but since we are stuck with an
         union semantics for maps, it is necessary to actually detect unreachable
         states. *)
      begin
        try
          let v = XIdm.find i m in
          let new_v = Cps.meet v (I.singleton cp) in
          if Cps.is_bottom new_v then bottom else XIdm.add i new_v m
        with
        | Not_found -> bottom
      end
    | Cbool true ->
      (* Note: while boolean values can only take 0 or 1 as values,
         the constraint Cbool can apply to more or less anything, due to some
         compilation schemes that generate if statements with non-boolean guard
         expressions. *)
      begin
        try
          let v = XIdm.find i m in
          let new_v = I.remove 0 v in
          if Cps.is_bottom new_v
          then bottom
          else XIdm.add i new_v m
        with
        | Not_found -> bottom
      end
    | Cbool false ->
      begin
        try
          let v = XIdm.find i m in
          let new_v = Cps.meet v (I.singleton 0) in
          if Cps.is_bottom new_v
          then bottom
          else XIdm.add i new_v m
        with
        | Not_found -> bottom
      end
    | Ctag _ | Ctype _ | Cstring _ | Cnotstring _ -> m (* Not handled *)

  let intv_constr i m (l, h) =
    let rec mk_set acc l h =
      if l > h then acc
      else
        let acc = Cps.join (I.singleton l) acc in
        mk_set acc (l+1) h
    in
    let intv = mk_set Cps.bottom l h in
    try
      let v = XIdm.find i m in
      let newv = Cps.meet v intv in
      if Cps.is_bottom newv then bottom
      else XIdm.add i newv m
    with
    | Not_found -> XIdm.add i intv m

  let set_absbool i m ab =
    let open IEsig in
    let res =
      match ab with
      | Bot -> Cps.bottom
      | True -> Cps.Ints.singleton 1
      | False -> Cps.Ints.singleton 0
      | Top -> Cps.Ints.add 0 (Cps.Ints.singleton 1)
    in
    XIdm.add i res m

  let print ppf e =
    XIdm.print Cps.print ppf e

  let print_var e ppf x =
    try
      let v = XIdm.find x e in
      Cps.print ppf v
    with
    | Not_found -> Format.fprintf ppf "<Bot>"
end

