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

module Make (Vars: IEsig.Vars) =
struct

  module XIdm = Map.Make(Vars)

  exception Mismatching_args of string

  let mismatch str =
    raise (Mismatching_args str)

  (* Semantics: union of interval and constant values *)
  type values =
    { interv: Int_interv.t;
      const: Cps.t;
    }

  let to_interv v =
    if Int_interv.is_bottom v.interv
    then
      Cps.Ints.fold
        (fun k acc -> Int_interv.join (Int_interv.cst k) acc)
        v.const
        Int_interv.bottom
    else v.interv

  (* Semantics: not in map -> bottom *)
  (* Invariant: no value in the map is bottom *)
  type t = values XIdm.t

  let bottom = XIdm.empty

  let empty = bottom

  let is_bottom = XIdm.is_empty

  let var_is_bottom i m =
    (* Technically, the invariant should make the is_bottom checks redundant,
       but it doesn't hurt much to check anyway *)
    try
      let v = XIdm.find i m in
      Int_interv.is_bottom v.interv && Cps.is_bottom v.const
    with
    | Not_found -> true

  let meet x y = failwith "Meet unimplemented"

  let merge_join va vb =
    { interv = Int_interv.join va.interv vb.interv;
      const = Cps.join va.const vb.const;
    }

  let join =
    XIdm.merge (map_merge_join merge_join)

  let merge_widening va vb =
    { interv = Int_interv.widening va.interv vb.interv;
      const = Cps.widening va.const vb.const;
    }

  let widening x y =
    XIdm.merge (map_merge_join merge_widening) x y

  let vleq vx vy =
    (* Warning: this is not optimal
       (e.g. leq ([1;1],{}) (bot,{1,2}) will return false).
       However, leq x (join x y) and leq x (widening x y) will
       always return true, so this is not a big problem. *)
    Int_interv.leq vx.interv vy.interv && Cps.leq vx.const vy.const

  let leq =
    let module M =
      MapLeq (struct
        module M = XIdm type v = values let vleq = vleq
      end)
    in
    M.comp

  let translate_un intf i m pname args =
    match args with
    | [j] ->
      begin
        try
          let v = XIdm.find j m in
          let interv = intf (to_interv v) in
          if Int_interv.is_bottom interv then m
          else XIdm.add i { interv; const = Cps.bottom } m
        with
        | Not_found -> m
      end
    | _ -> mismatch pname

  let translate_bin intf i m pname args =
    match args with
    | [j; k] ->
      begin
        try
          let v1 = XIdm.find j m in
          let v2 = XIdm.find k m in
          let interv = intf (to_interv v1) (to_interv v2) in
          if Int_interv.is_bottom interv then m
          else XIdm.add i { interv; const = Cps.bottom } m
        with
        | Not_found -> m
      end
    | _ -> mismatch pname

  let intcomp m c j k =
    let v1 = XIdm.find j m in
    let v2 = XIdm.find k m in
    (* TODO: more precise comparison using Cps *)
    match Int_interv.comp c (to_interv v1) (to_interv v2) with
    | None -> IEsig.Top
    | Some b -> if b then IEsig.True else IEsig.False

  let primitive i m p args =
    match p with
    | XPbuiltin -> m (* TODO: can be more precise ? *)
    | XPnegint ->
      translate_un Int_interv.uminus i m "negint" args
    | XPaddint ->
      translate_bin Int_interv.add i m "addint" args
    | XPsubint ->
      translate_bin Int_interv.sub i m "subint" args
    | XPmulint ->
      translate_bin Int_interv.mul i m "mulint" args
    | XPdivint ->
      translate_bin Int_interv.div i m "divint" args
    | XPmodint ->
      translate_bin Int_interv.modulo i m "modint" args
    | XPandint ->
      translate_bin Int_interv.band i m "andint" args
    | XPorint ->
      translate_bin Int_interv.bor i m "orint" args
    | XPxorint ->
      translate_bin Int_interv.bxor i m "xorint" args
    | XPlslint ->
      translate_bin Int_interv.blsl i m "lslint" args
    | XPlsrint ->
      translate_bin Int_interv.blsr i m "lsrint" args
    | XPasrint ->
      translate_bin Int_interv.basr i m "asrint" args
    | XPintcomp c ->
      begin
        match args with
        | [j; k] ->
          begin
            try
              let comp = intcomp m c j k in
              let const =
                match comp with
                | IEsig.Top -> Cps.Ints.add 1 (Cps.Ints.singleton 0)
                | IEsig.Bot -> Cps.Ints.empty
                | IEsig.True -> Cps.Ints.singleton 1
                | IEsig.False -> Cps.Ints.singleton 0
              in
              XIdm.add i { interv = Int_interv.bottom; const } m
            with
            | Not_found -> m
          end
        | _ -> mismatch "intcomp"
      end
    | XPoffsetint off ->
      let add_off v = Int_interv.add (Int_interv.cst off) v in
      translate_un add_off i m "offsetint" args
    (* This is an awful trick that makes an unsigned less-than comparison
       to encode "x < y || y < 0" *)
    | XPisout ->
      begin
        match args with
        | [j; k] ->
          begin
            try
              let v1 = XIdm.find j m in
              let v2 = XIdm.find k m in
              let complt =
                Int_interv.comp Lambda.Clt (to_interv v1) (to_interv v2)
              in
              let compzero =
                Int_interv.comp Lambda.Clt (to_interv v2) (Int_interv.cst 0)
              in
              let const =
                match complt, compzero with
                | Some true, _ | _, Some true ->
                  Cps.Ints.singleton 1
                | Some false, Some false ->
                  Cps.Ints.singleton 0
                | _, _ ->
                  Cps.Ints.add 1 (Cps.Ints.singleton 0)
              in
              XIdm.add i { interv = Int_interv.bottom; const } m
            with
            | Not_found -> m
          end
        | _ -> mismatch "isout"
      end
    | XPnot ->
      begin
        match args with
        | [j] ->
          begin
            try
              let v = XIdm.find j m in
              let has_int n =
                Cps.Ints.mem n v.const || Int_interv.mem n v.interv
              in
              let const =
                match has_int 0, has_int 1 with
                | false, false -> Cps.bottom
                | false, true -> Cps.Ints.singleton 0
                | true, false -> Cps.Ints.singleton 1
                | true, true -> Cps.Ints.add 1 (Cps.Ints.singleton 0)
              in
              XIdm.add i { interv = Int_interv.bottom; const } m
            with
            | Not_found -> m
          end
        | _ -> mismatch "not"
      end
    | _ -> failwith "IntEnv: Unsupported primitive"

  let var i m j =
    try XIdm.add i (XIdm.find j m) m
    with Not_found -> m

  let update strong i m j =
    if strong then var i m j
    else
      try
        let vi = XIdm.find i m in
        try
          let vj = XIdm.find j m in
          let v =
            { interv = Int_interv.join vi.interv vj.interv;
              const = Cps.join vi.const vj.const;
            }
          in
          XIdm.add i v m
        with
        | Not_found -> m
      with
      | Not_found -> var i m j

  let set_top i m =
    (* TODO: implement it *)
    failwith "IntEnv.set_top not implemented"

  let set_bottom i m =
    XIdm.remove i m

  let const i m cst =
    let open Lambda in
    match cst with
    | Const_base bcst ->
      begin
        let open Asttypes in
        match bcst with
        | Const_int ci ->
          let v =
            { interv = Int_interv.cst ci;
              const = Cps.bottom;
            }
          in
          XIdm.add i v m
        | Const_char cc ->
          let ci = int_of_char cc in
          let v =
            { interv = Int_interv.cst ci;
              const = Cps.bottom;
            }
          in
          XIdm.add i v m
        | _ -> m (* Unsupported constant *)
      end
    | Const_pointer cp ->
      (* Unintuitively, it is more precise to put the intervals to
         bottom than to the singleton interval *)
      let v =
        { interv = Int_interv.bottom;
          const = Cps.Ints.singleton cp;
        }
      in
      XIdm.add i v m
    | _ -> m (* Unsupported constant *)

  let constr i m cstr =
    match cstr with
    | Ccp cp ->
      begin
        try
          let v = XIdm.find i m in
          let v' =
            { interv = Int_interv.meet v.interv (Int_interv.cst cp);
              const = Cps.meet v.const (Cps.Ints.singleton cp);
            }
          in
          if Int_interv.is_bottom v'.interv && Cps.is_bottom v'.const
          then bottom
          else XIdm.add i v' m
        with
        | Not_found -> bottom
      end
    | Cbool true ->
      begin
        try
          let v = XIdm.find i m in
          let v' =
            { interv =
                Int_interv.join
                  (Int_interv.leqcst v.interv (-1))
                  (Int_interv.geqcst v.interv 1);
              const = Cps.Ints.remove 0 v.const;
            }
          in
          if Int_interv.is_bottom v'.interv && Cps.is_bottom v'.const
          then bottom
          else XIdm.add i v' m
        with
        | Not_found -> bottom
      end
    | Cbool false ->
      begin
        try
          let v = XIdm.find i m in
          let v' =
            { interv = Int_interv.meet v.interv (Int_interv.cst 0);
              const = Cps.meet v.const (Cps.Ints.singleton 0);
            }
          in
          if Int_interv.is_bottom v'.interv && Cps.is_bottom v'.const
          then bottom
          else XIdm.add i v' m
        with
        | Not_found -> bottom
      end
    | Ctag _ | Ctype _ | Cstring _ | Cnotstring _ -> m (* Not handled *)

  let intv_constr i m (l,h) =
    (* TODO: do the implementation *)
    m

  let print_val ppf v =
    Format.fprintf ppf "@[Interval:@ %a@.Cps:@ %a@]"
      Int_interv.print v.interv
      Cps.Ints.print v.const

  let print ppf e =
    XIdm.print print_val ppf e

  let print_var e ppf x =
    try
      let v = XIdm.find x e in
      print_val ppf v
    with
    | Not_found -> Format.fprintf ppf "<Bot>"

  let renaming l m =
    List.fold_left
      (fun acc (i,j) ->
         try
           XIdm.add j (XIdm.find i m) acc
         with
         | Not_found -> acc)
      XIdm.empty
      l

  let partial_renaming l m =
    List.fold_left
      (fun acc (i,j) ->
         try
           XIdm.add j (XIdm.find i m) (XIdm.remove i m)
         with
         | Not_found -> acc)
      m
      l

  let proj l m =
    XIdm.filter (fun i _ -> not (List.mem i l)) m

end
