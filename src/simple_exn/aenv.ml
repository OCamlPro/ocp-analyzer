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

module type ProgId =
sig
  include Set.OrderedType

  (* val ret_id : t *)

  (* val exn_id : t *)
end

module MakeProgId (Stack:Stack_types.Stack) =
struct
  type t = xid * Stack.t

  let compare (id1,s1) (id2,s2) =
    let c = XId.compare id1 id2 in
    if c = 0
    then Stack.compare s1 s2
    else c

  let print_gen ?(print_xid = XId.print) b_print_stack ppf (id,s) =
    if b_print_stack
    then
      Format.fprintf ppf "%a\\{%a\\}" print_xid id Stack.print s
    else
      print_xid ppf id

  let print ppf x = print_gen true ppf x

  (* let ret_id = (ret_xid,Stack.empty) *)

  (* let exn_id = (exn_xid,Stack.empty) *)
end

module MakeIntVar (ProgId:ProgId) =
struct
  type vname =
    | Xid of ProgId.t
    | Field of vname * int
    | Anon

  type t =
    { id : int;
      name : vname;
    }

  type ivars =
    | IVvar of t
    | IVblock of (ivars option) array

  let next_id = ref 0

  let compare x y =
    let c = y.id - x.id in
    if c > 0 then 1 else
    if c < 0 then -1 else
      0

  let rec print_vname ppf = function
    | Xid x -> ProgId.print ppf x
    | Field (vn, off) -> Format.fprintf ppf "%a%@%d" print_vname vn off
    | Anon -> Format.fprintf ppf "__"

  let print ppf x =
    Format.fprintf ppf "%a(%d)" print_vname x.name x.id

  let mk ~name () =
    let id = !next_id in
    incr next_id;
    { id; name }

  let get_name x =
    x.name

  let to_string x = Format.asprintf "%a" print x
end

module Env (Stack: Stack_types.Stack) (Ints: IEsig.IE) = struct
  module ProgId = MakeProgId(Stack)
  module XIdm = Map.Make(ProgId)
  module Vals = AbsVals.Make(ProgId)
  module BE = BlockEnv.Make(ProgId)
  module CE = CpsEnv.Make(ProgId)
  module TE = TopEnv.Make(ProgId)

  module IntVar = MakeIntVar(ProgId)
  open IntVar

  module IE = Ints(IntVar)

  type special_register = ivars option * Vals.t

  type t =
    {
      ints : IE.t;
      iv_map : ivars XIdm.t;
      cps : CE.t;
      blocks : BE.t;
      alias : Vals.t XIdm.t;
      other : TE.t;
      app : (special_register * special_register) option;
      return : special_register option;
      exn_return : special_register option;
      empty : bool; (* To allow treating empty as not bottom *)
    }

  (* Argument types: src:ivars dst:vname *)
  let duplicate_ivar ~src ~dst ints =
    let rec aux name ints = function
      | IVvar v ->
        let v' = IntVar.mk ~name () in
        (IVvar v', IE.var v' ints v)
      | IVblock arr ->
        let ints_ref = ref ints in
        let arr' =
          Array.init
            (Array.length arr)
            (fun i ->
               match arr.(i) with
               | None -> None
               | Some iv ->
                 let (iv', ints) = aux (Field (name, i)) !ints_ref iv in
                 ints_ref := ints;
                 Some iv')
        in
        (IVblock arr', !ints_ref)
    in
    aux dst ints src

  let print_alias ppf al =
    XIdm.print Vals.print ppf al

  let rec print_ivars ppf = function
    | IVvar v -> IntVar.print ppf v
    | IVblock arr ->
      begin
        Format.fprintf ppf "@[[| ";
        Array.iteri
          (fun i vopt ->
             match vopt with
             | None -> ()
             | Some iv -> Format.fprintf ppf "%d:%a@ " i print_ivars iv)
          arr;
        Format.fprintf ppf "|]@]";
      end

  let print_iv_map ppf map =
    XIdm.print print_ivars ppf map

  let pivo ppf = function
    | None -> Format.fprintf ppf "_"
    | Some iv -> Format.fprintf ppf "%a" print_ivars iv

  let print_register ppf (ivo,vals) =
    Format.fprintf ppf "(%a,%a)" pivo ivo Vals.print vals

  let print_app ppf = function
    | None -> Format.fprintf ppf "(No app)"
    | Some (f,arg) ->
      Format.fprintf ppf "%a,%a" print_register f print_register arg

  let print_return msg ppf = function
    | None -> Format.fprintf ppf "%s" msg
    | Some r ->
      Format.fprintf ppf "%a" print_register r

  let print_env ppf e =
    Format.fprintf ppf
      "@.@[Integer_map:@ %a@.\
       Integers:@ %a@.\
       Cps:@ %a@.\
       Blocks:@ %a@.\
       Alias:@ %a@.\
       App:@ %a@.\
       Return:@ %a@.\
       Retexn:@ %a@.@]"
      print_iv_map e.iv_map
      IE.print e.ints
      CE.print e.cps
      BE.print e.blocks
      print_alias e.alias
      print_app e.app
      (print_return "No return") e.return
      (print_return "No retexn") e.exn_return

  let bottom =
    {
      ints = IE.bottom;
      iv_map = XIdm.empty;
      cps = CE.bottom;
      blocks = BE.bottom;
      alias = XIdm.empty;
      other = TE.bottom;
      app = None;
      return = None;
      exn_return = None;
      empty = false;
    }

  let is_bottom e =
    (* Alias information is not relevant to bottom status *)
    (* TODO: Verify that the integer bottom test works as expected *)
    IE.is_bottom e.ints
    || (CE.is_bottom e.cps
        && BE.is_bottom e.blocks
        && TE.is_bottom e.other
        && not e.empty)

  let empty = {bottom with empty = true; ints = IE.empty}

  let unalias i e =
    let rec aux acc = function
      | [] -> acc
      | xid :: tail ->
        (* DEBUG *)
        (* Format.printf "unalias: %a -> %a@." *)
        (*   ProgId.print i *)
        (*   ProgId.print xid; *)
        begin
          if Vals.mem xid acc then aux acc tail
          else
            try
              let vals = XIdm.find xid e.alias in
              (* DEBUG *)
              (* Format.printf "Found %a: " ProgId.print xid; *)
              (* Vals.fold (fun xid () -> Format.printf " %a" ProgId.print xid) vals (); *)
              (* Format.printf "@."; *)
              aux
                acc
                (Vals.fold (fun x l -> x :: l) vals tail)
            with
            | Not_found ->
              let acc = Vals.join acc (Vals.singleton xid) in
              aux acc tail
        end
    in
    aux Vals.bottom [i]

  let meet e1 e2 = failwith "Meet not implemented"

  (* convert_ivar creates an integer variable (or variables) which abstracts
     both its arguments vars1 and vars2. In addition to the new variable
     (or None if the arguments are not compatible), it returns the corresponding
     renaming of variables to convert the integer domain (for convenience,
     these association lists are passed as accumulators and added to). *)
  let rec convert_ivar (acc1, acc2) name vars1 vars2 =
    match vars1, vars2 with
    | IVvar v1, IVvar v2 ->
      let v3 = IntVar.mk ~name () in
      (((v1,v3)::acc1, (v2,v3)::acc2), Some (IVvar v3))
    | IVblock arr1, IVblock arr2 ->
      let len = Array.length arr1 in
      if Array.length arr2 = len
      then
        let l1 = ref acc1 in
        let l2 = ref acc2 in
        let arr3 =
          Array.init
            len
            (fun i ->
               match arr1.(i), arr2.(i) with
               | None, _ | _, None -> None
               | Some vars1', Some vars2' ->
                 let (acc1, acc2), v3_opt =
                   convert_ivar (!l1, !l2) (Field (name, i)) vars1' vars2'
                 in
                 l1 := acc1;
                 l2 := acc2;
                 v3_opt
            )
        in
        ((!l1, !l2), Some (IVblock arr3))
      else
        ((acc1, acc2), None)
    | IVvar _, IVblock _ | IVblock _, IVvar _ ->
      ((acc1, acc2), None)

  (* This function takes as arguments two states, and outputs
     a tuple (ints1, ints2, iv_map, app_vars_opt),
     where iv_map is a map from xids to fresh int variables that has as
     domain the intersection of the domain of the iv_maps of the inputs,
     and ints1 and ints2 are the integer domains
     that correspond to e1.ints and e2.ints, renamed to match iv_map.
     app_vars_opt is a pair of ivars option values corresponding to the integer
     variables for the prepared application. *)
  let ints_to_common_env e1 e2 =
    let chain_serial f g substs x =
      let (substs', fx) = f substs x in
      g substs' fx
    in
    let chain_parallel f g substs (x, y) =
      let (substs', fx) = f substs x in
      let (substs'', gy) = g substs' y in
      (substs'', (fx, gy))
    in
    let subst1, subst2, iv_map =
      XIdm.fold
        (fun xid vars1 (acc1, acc2, ivm) ->
           try
             let vars2 = XIdm.find xid e2.iv_map in
             let (acc1, acc2), vars3_opt =
               convert_ivar (acc1, acc2) (Xid xid) vars1 vars2
             in
             match vars3_opt with
             | None -> (acc1, acc2, ivm)
             | Some vars3 -> (acc1, acc2, XIdm.add xid vars3 ivm)
           with
           | Not_found -> (acc1, acc2, ivm))
        e1.iv_map
        ([],[],XIdm.empty)
    in
    let handle_register substs = function
      | (None, _), _ | _, (None, _) -> (substs, None)
      | (Some iv1, _), (Some iv2, _) ->
         convert_ivar substs Anon iv1 iv2
    in
    let comp_app_substs substs () =
      match e1.app, e2.app with
      | None, None -> (substs, (None, None))
      | None, Some _ | Some _, None ->
        (* An error will be thrown later anyway *)
        (substs, (None, None))
      | Some (fr1, ar1), Some (fr2, ar2) ->
         chain_parallel handle_register handle_register substs ((fr1, fr2), (ar1, ar2))
    in
    let comp_genret_substs substs = function
      | None, None -> (substs, None)
      | None, Some _ | Some _, None ->
         (* As above, will be handled later *)
         (substs, None)
      | Some r1, Some r2 -> handle_register substs (r1, r2)
    in
    let comp_return_retexn_substs substs () =
      chain_parallel
        comp_genret_substs comp_genret_substs
        substs
        ((e1.return, e2.return), (e1.exn_return, e2.exn_return))
    in
    let (subst1, subst2), ((f_iv_opt, a_iv_opt), (r_iv_opt, e_iv_opt)) =
      chain_parallel comp_app_substs comp_return_retexn_substs
                     (subst1, subst2) ((),())
    in
    let ints1 = IE.renaming subst1 e1.ints in
    let ints2 = IE.renaming subst2 e2.ints in
    (ints1, ints2, iv_map, (f_iv_opt, a_iv_opt, r_iv_opt, e_iv_opt))

  let join e1 e2 =
    try
      if is_bottom e1 then e2
      else if is_bottom e2 then e1
      else
        let ints1, ints2, iv_map, (fiv, aiv, riv, eiv) =
          ints_to_common_env e1 e2
        in
        let ints = IE.join ints1 ints2 in
        let cps = CE.join e1.cps e2.cps in
        let blocks = BE.join e1.blocks e2.blocks in
        let other = TE.join e1.other e2.other in
        let alias = XIdm.merge (map_merge_join Vals.join) e1.alias e2.alias in
        let app =
          match e1.app, e2.app with
          | None, None -> None
          | Some ((_,f1),(_,arg1)), Some ((_,f2),(_,arg2)) ->
            Some ((fiv, Vals.join f1 f2), (aiv, Vals.join arg1 arg2))
          | _, _ -> None (* TODO: Add a warning/error *)
        in
        let return =
          match e1.return, e2.return with
          | None, _ | _, None -> None
          | Some (_, r1), Some (_, r2) -> Some (riv, Vals.join r1 r2)
        in
        let exn_return =
          match e1.exn_return, e2.exn_return with
          | None, _ | _, None -> None
          | Some (_, r1), Some (_, r2) -> Some (eiv, Vals.join r1 r2)
        in
        let empty = false in
        {ints; iv_map; cps; blocks; other; alias; app; return; exn_return; empty}
    with
    | exn ->
      Format.eprintf "Exception in join:@.\
                      Left side:@[@.%a@]@.\
                      Right side:@[@.%a@]@."
        print_env e1
        print_env e2;
      raise exn

  let widening e1 e2 =
    if is_bottom e1 then e2
    else if is_bottom e2 then e1
    else
      let ints1, ints2, iv_map, (fiv, aiv, riv, eiv) = ints_to_common_env e1 e2 in
      let ints = IE.widening ints1 ints2 in
      let cps = CE.widening e1.cps e2.cps in
      let blocks = BE.widening e1.blocks e2.blocks in
      let other = TE.widening e1.other e2.other in
      let alias =
        XIdm.merge (map_merge_join Vals.widening) e1.alias e2.alias
      in
      let app =
        match e1.app, e2.app with
        | None, None -> None
        | Some ((_,f1),(_,arg1)), Some ((_,f2),(_,arg2)) ->
          Some ((fiv, Vals.widening f1 f2), (aiv, Vals.widening arg1 arg2))
        | _, _ -> None
      in
      let return =
        match e1.return, e2.return with
        | None, _ | _, None -> None
        | Some (_, r1), Some (_, r2) -> Some (riv, Vals.widening r1 r2)
      in
      let exn_return =
        match e1.exn_return, e2.exn_return with
        | None, _ | _, None -> None
        | Some (_, r1), Some (_, r2) -> Some (eiv, Vals.widening r1 r2)
      in
      let empty = false in
      {ints; iv_map; cps; blocks; other; alias; app; return; exn_return; empty}

  let leq e1 e2 =
    if is_bottom e1 then true
    else if is_bottom e2 then false (* Since e1 is not bottom here *)
    else
      let reg_leq (_,v1) (_,v2) = Vals.leq v1 v2 in
      let app_leq =
        match e1.app, e2.app with
        | None, None -> true
        | Some (fr1, ar1), Some (fr2, ar2) ->
           reg_leq fr1 fr2 && reg_leq ar1 ar2
        | _, _ -> failwith "Mismatching app status in leq"
      in
      let return_leq =
        match e1.return, e2.return with
        | None, None -> true
        | Some r1, Some r2 ->
           reg_leq r1 r2
        | _, _ -> failwith "Mismatching return status in leq"
      in
      let retexn_leq =
        match e1.exn_return, e2.exn_return with
        | None, None -> true
        | Some r1, Some r2 ->
           reg_leq r1 r2
        | _, _ -> failwith "Mismatching retexn status in leq"
      in
      let module M =
        MapLeq (struct
          module M = XIdm type v = Vals.t let vleq = Vals.leq
        end)
      in
      let ints1, ints2, _, _ = ints_to_common_env e1 e2 in
      let alias_leq = M.comp in
      app_leq
      && IE.leq ints1 ints2
      && CE.leq e1.cps e2.cps
      && BE.leq e1.blocks e2.blocks
      && TE.leq e1.other e2.other
      && return_leq && retexn_leq
      && alias_leq e1.alias e2.alias

  let var ~hedge ~s ~x ~env:e var =
    let xid = (x,s) in
    let var = (var,s) in
    let ints, iv_map =
      try
        let iv = XIdm.find var e.iv_map in
        let (iv', ints') = duplicate_ivar ~src:iv ~dst:(Xid xid) e.ints in
        (ints', XIdm.add xid iv' e.iv_map)
      with
      | Not_found -> e.ints, e.iv_map
    in
    let aliases = unalias var e in
    assert (not (Vals.mem xid aliases));
    {e with ints; iv_map; alias = XIdm.add xid aliases e.alias}

  let rec set_top_rec acc = function
    | IVvar v -> IE.set_top v acc
    | IVblock arr ->
      Array.fold_left
        (fun acc iv_opt ->
           match iv_opt with
           | None -> acc
           | Some iv -> set_top_rec acc iv)
        acc
        arr

  let load_from_register ?func xid (iv_opt, ids) e =
    let vals =
      match func with
      | None -> ids
      | Some f ->
         Vals.filter
           (fun id -> BE.has_tag (BE.Tag_fun f) id e.blocks)
           ids
    in
    let ints, iv_map =
      match iv_opt with
      | None -> (e.ints, e.iv_map)
      | Some iv ->
         let xiv, ints =
           duplicate_ivar ~src:iv ~dst:(Xid xid) e.ints
         in
         (ints, XIdm.add xid iv e.iv_map)
    in
    {e with ints; iv_map; alias = XIdm.add xid vals e.alias}

  let primitive ~hedge ~s ~x ~env:e p args =
    let xid = (x,s) in
    let args = List.map (fun x -> (x,s)) args in
    match p, args with
    | XPbuiltin, [i] ->
      (* Builtin initialisation is missing, so do nothing *)
      e
    | XPfield off, [i]
    | XPfloatfield off, [i]
    | XPfunfield off, [i] ->
      let ids = unalias i e in
      (* DEBUG *)
      (* Format.printf "funfield: unalias: %a -> %a@." *)
      (*   ProgId.print i *)
      (*   Vals.print ids; *)
      let vals_of_id xid = BE.get_field xid off e.blocks in
      let vals =
        Vals.fold
          (fun x acc -> Vals.join (vals_of_id x) acc)
          ids
          Vals.bottom
      in
      let ints, iv_map =
        try
          let field_ivar =
            match XIdm.find i e.iv_map with
            | IVvar _ -> raise Not_found
            | IVblock arr ->
              begin
                match arr.(off) with
                | None -> raise Not_found
                | Some iv -> iv
              end
          in
          let iv', ints' =
            duplicate_ivar ~src:field_ivar ~dst:(Xid xid) e.ints
          in
          (ints', XIdm.add xid iv' e.iv_map)
        with
        | Not_found -> e.ints, e.iv_map
      in
      if Vals.is_bottom vals then e
      else
        begin
          assert (not (Vals.mem xid vals));
          {e with ints; iv_map; alias = XIdm.add xid vals e.alias}
        end
    | XPsetfield (off,_), [i; j]
    | XPsetfloatfield off, [i; j] ->
      let v = Vals.singleton j in
      let ids = unalias i e in
      let bids =
        Vals.filter
          (fun id -> XIdm.mem id e.blocks)
          ids
      in
      let strong_update =
        match Vals.as_singleton bids with
        | Some _ -> true
        | None -> false
      in
      let update =
        if strong_update
        then BE.set_field
        else BE.may_set_field
      in
      let blocks =
        Vals.fold
          (fun xid acc -> update xid off acc v)
          bids
          e.blocks
      in
      let ints =
        try
          let field_ivar =
            match XIdm.find i e.iv_map with
            | IVvar _ -> raise Not_found
            | IVblock arr ->
              begin
                match arr.(off) with
                | None -> raise Not_found
                | Some v -> v
              end
          in
          try
            let rhs_ivar = XIdm.find j e.iv_map in
            let rec do_update acc iv_src iv_dst =
              match iv_src, iv_dst with
              | IVvar v1, IVvar v2 ->
                IE.update strong_update v2 acc v1
              | IVblock arr1, IVblock arr2 ->
                let acc_ref = ref acc in
                let len = Array.length arr1 in
                assert (Array.length arr2 = len);
                for i = 0 to len - 1
                do
                  match arr1.(i), arr2.(i) with
                  | None, None -> ()
                  | Some iv1, Some iv2 ->
                    acc_ref := do_update !acc_ref iv1 iv2
                  | Some _, None -> ()
                  | None, Some iv2 ->
                    acc_ref := set_top_rec !acc_ref iv2
                done;
                !acc_ref
              | IVvar _, IVblock _ | IVblock _, IVvar _ ->
                set_top_rec acc iv_dst
            in
            do_update e.ints rhs_ivar field_ivar
          with
          | Not_found -> set_top_rec e.ints field_ivar
        with
        | Not_found -> e.ints
      in
      (* Add the unit value *)
      let cps =
        CE.const xid e.cps (Lambda.Const_pointer 0)
      in
      {e with ints; cps; blocks}
    | XPduprecord (_, _), [i] ->
      let ids = unalias i e in
      let update =
        match Vals.as_singleton ids with
        | Some _ -> BE.duprecord
        | None -> BE.may_duprecord
      in
      let blocks =
        Vals.fold (fun id acc -> update xid acc id) ids e.blocks
      in
      let ints, iv_map =
        try
          let ivar = XIdm.find i e.iv_map in
          let (ivar', ints') = duplicate_ivar ~src:ivar ~dst:(Xid xid) e.ints in
          (ints', XIdm.add xid ivar' e.iv_map)
        with
        | Not_found -> e.ints, e.iv_map
      in
      {e with ints; iv_map; blocks}
    | (XPnot|XPnegint|XPoffsetint _), [i] ->
      let ids = unalias i e in
      let cps =
        Vals.fold
          (fun id acc ->
             CE.join
               acc
               (CE.primitive xid e.cps p [id])
          )
          ids
          CE.bottom
      in
      let ints, iv_map =
        try
          let ivar =
            match XIdm.find i e.iv_map with
            | IVvar v -> v
            | IVblock _ -> raise Not_found
          in
          let res_ivar = IntVar.mk ~name:(Xid xid) () in
          (IE.primitive res_ivar e.ints p [ivar],
           XIdm.add xid (IVvar res_ivar) e.iv_map)
        with
        | Not_found -> (e.ints, e.iv_map)
      in
      {e with ints; iv_map; cps}
    | (XPaddint|XPsubint|XPmulint|XPdivint|XPmodint|XPandint|XPorint
      |XPxorint|XPlslint|XPlsrint|XPasrint|XPisout), [i; j] ->
      let ids1 = unalias i e in
      let ids2 = unalias j e in
      let ints, iv_map =
        try
          let iv1 =
            match XIdm.find i e.iv_map with
            | IVvar v -> v
            | IVblock _ -> raise Not_found
          in
          let iv2 =
            match XIdm.find j e.iv_map with
            | IVvar v -> v
            | IVblock _ -> raise Not_found
          in
          let res_ivar = IntVar.mk ~name:(Xid xid) () in
          (IE.primitive res_ivar e.ints p [iv1; iv2],
           XIdm.add xid (IVvar res_ivar) e.iv_map)
        with
        | Not_found -> (e.ints, e.iv_map)
      in
      let cps =
        Vals.fold
          (fun id1 acc ->
             Vals.fold
               (fun id2 acc ->
                  CE.join
                    acc
                    (CE.primitive xid e.cps p [id1;id2])
               )
               ids2
               acc)
          ids1
          CE.bottom
      in
      {e with ints; iv_map; cps}
    | XPintcomp cmp, [i; j] ->
      let ids1 = unalias i e in
      let ids2 = unalias j e in
      let allids = Vals.join ids1 ids2 in
      begin
        let ints, iv_map, compres =
          try
            let iv1 =
              match XIdm.find i e.iv_map with
              | IVvar v -> v
              | IVblock _ -> raise Not_found
            in
            let iv2 =
              match XIdm.find j e.iv_map with
              | IVvar v -> v
              | IVblock _ -> raise Not_found
            in
            let res_ivar = IntVar.mk ~name:(Xid xid) () in
            let ints = IE.primitive res_ivar e.ints p [iv1; iv2] in
            let iv_map = XIdm.add xid (IVvar res_ivar) e.iv_map in
            let compres = IE.intcomp e.ints cmp iv1 iv2 in
            (ints, iv_map, compres)
          with
          | Not_found ->
            (* Note: Top is a safer solution than Bot, but since we are here
               in the case were there were no corresponding integer variables,
               it is correct (I believe) to use Bot in this context.
               Besides, using Top would mean that if we are doing a comparison
               on values not tracked by the integer domain (like const pointers),
               we would return Top globally, even if more precise information is
               stored elsewhere. *)
            (e.ints, e.iv_map, IEsig.Bot)
        in
        let ints_cps =
          CE.set_absbool xid e.cps compres
        in
        let cps_base =
          if Vals.exists
              (fun x -> not (CE.var_is_bottom x e.cps))
              allids
          then
            Vals.fold
              (fun id1 acc ->
                 Vals.fold
                   (fun id2 acc ->
                      CE.join
                        acc
                        (CE.primitive xid e.cps p [id1;id2]))
                   ids2
                   acc)
              ids1
              CE.bottom
          else
            CE.bottom (* Will later be joined with the other results *)
        in
        let cps_scalars = CE.join ints_cps cps_base in
        (* Assuming that Ceq and Cneq can be used for structural equality,
           but the other comparisons are only used for integers *)
        let open Lambda in
        match cmp with
        | Ceq | Cneq ->
          (* This is structural (in)equality, so using aliases we can make
             the following decisions :
             - If their alias sets are disjoint, they are not equal as blocks
             (but we still need to test for integer equality)
             - If their alias sets are both a singleton with the same variable
             _and_ the singleton variable is not a non-unique block,
             then they are equal (no integer test needed)
             - Otherwise, we don't know *)
          let disjoint = Vals.is_bottom (Vals.meet ids1 ids2) in
          let def_equal =
            match (Vals.as_singleton ids1, Vals.as_singleton ids2) with
            | Some i1, Some i2 ->
              ProgId.compare i1 i2 = 0
              &&
              begin
                match BE.var_is_unique i1 e.blocks with
                | Some b -> b
                | None -> true
              end
            | _, _ -> false
          in
          let reverse =
            match cmp with
            | Ceq -> false
            | Cneq -> true
            | _ -> assert false
          in
          let true_cps = CE.const xid e.cps (Lambda.Const_pointer 1) in
          let false_cps = CE.const xid e.cps (Lambda.Const_pointer 0) in
          let top_cps = CE.join true_cps false_cps in
          let cps =
            if def_equal
            (* Definitely equal *)
            then if reverse then false_cps else true_cps
            else
              (* If there is a value abstracted to top, return top *)
            if Vals.exists
                (fun x -> not (TE.var_is_bottom x e.other))
                allids
            then top_cps
            else
              (* Check whether we have to take blocks into account *)
            if Vals.exists
                (fun x -> not (BE.var_is_bottom x e.blocks))
                allids
            then
              let block_cps =
                if disjoint
                then
                  if reverse then true_cps else false_cps
                else top_cps
              in
              CE.join cps_scalars block_cps
            else (* Only integer variables are compared *)
              cps_scalars
          in
          {e with ints; iv_map; cps}
        | _ ->
          {e with ints; iv_map; cps = cps_scalars}
      end
    | XPoffsetref off, _ -> assert false (* XPoffsetref are not generated
                                            by Mk_xlambda *)
    | XPisint, [i] ->
      let ids = unalias i e in
      let ivmap_is_int id =
        try
          match XIdm.find id e.iv_map with
          | IVvar _ -> true
          | IVblock _ -> false
        with
        | Not_found -> false
      in
      let ivmap_is_block id =
        try
          match XIdm.find id e.iv_map with
          | IVvar _ -> false
          | IVblock _ -> true
        with
        | Not_found -> false
      in
      let cps_is_int =
        if
          Vals.exists
            (fun id ->
               not (CE.var_is_bottom id e.cps)
               || (ivmap_is_int id)
            )
            ids
        then CE.const xid e.cps (Lambda.Const_pointer 1)
        else e.cps
      in
      let cps_is_block =
        if
          Vals.exists
            (fun id ->
               not (BE.var_is_bottom id e.blocks)
               || (ivmap_is_block id)
            )
            ids
        then CE.const xid e.cps (Lambda.Const_pointer 0)
        else e.cps
      in
      {e with cps = CE.join cps_is_int cps_is_block}
    | (XPintoffloat|XPfloatofint|XPnegfloat|XPabsfloat|XPaddfloat
      |XPsubfloat|XPmulfloat|XPdivfloat|XPfloatcomp _), _
    | (XPstringlength|XPstringrefu|XPstringsetu|XPstringrefs|XPstringsets
      |XParraylength _|XParrayrefu _|XParraysetu _), _
    | XPbittest, _
    | (XPbintofint _| XPintofbint _|XPcvtbint (_,_)|XPnegbint _|XPaddbint _
      |XPsubbint _|XPmulbint _|XPdivbint _|XPmodbint _|XPandbint _|XPorbint _
      |XPxorbint _|XPlslbint _|XPlsrbint _|XPasrbint _|XPbintcomp (_,_)), _
    | (XPbigarrayref (_,_,_,_)|XPbigarrayset (_,_,_,_)|XPbigarraydim _), _
    | (XPstring_load_16 _|XPstring_load_32 _|XPstring_load_64 _
      |XPstring_set_16 _|XPstring_set_32 _|XPstring_set_64 _), _
    | (XPbigstring_load_16 _|XPbigstring_load_32 _|XPbigstring_load_64 _
      |XPbigstring_set_16 _|XPbigstring_set_32 _|XPbigstring_set_64 _), _
    | XPctconst _, _
    | (XPbswap16|XPbbswap _), _
      -> {e with other = TE.primitive xid e.other p args}
    | XPgetfun f, [] ->
      (* DEBUG *)
      (* Format.eprintf "getfun %a:@.%a@." *)
      (*   F.print f *)
      (*   print_env e; *)
      begin
        match e.app with
        | None -> failwith "Missing prepared application"
        | Some (freg,_) ->
           load_from_register ~func:f xid freg e
      end
    | XPgetarg, [] ->
      begin
        match e.app with
        | None -> failwith "Missing prepared application"
        | Some (_,areg) ->
           load_from_register xid areg e
      end
    | p, args ->
      let msg =
        Format.asprintf
          "Wrong number of arguments to primitive:@.%a %d"
          Print_xlambda.primitive p
          (List.length args)
      in
      failwith msg

  let const ~hedge ~s ~x ~env:e cst =
    let xid = (x,s) in
    let open Lambda in
    match cst with
    | Const_base _ ->
      (* TODO: The integer domain can handle non-integer base constants, but
         optimally we should only feed it integers. *)
      let iv = IntVar.mk ~name:(Xid xid) () in
      let iv_map = XIdm.add xid (IVvar iv) e.iv_map in
      let ints = IE.const iv e.ints cst in
      let cps = CE.const xid e.cps cst in
      {e with ints; iv_map; cps}
    | Const_pointer _ ->
      {e with cps = CE.const xid e.cps cst}
    | Const_block (_, csts) ->
      (* Assuming constant blocks were translated away *)
      assert false
    | Const_float_array _
    | Const_immstring _
      -> {e with other = TE.const xid e.other cst}

  (* Restrict the variable's integer values between 0 and max - 1 *)
  let restrict_ints_to_range max x e =
    let ints, iv_map =
      try
        let iv =
          match XIdm.find x e.iv_map with
          | IVvar v -> v
          | IVblock _ -> raise Not_found
        in
        let ints = IE.intv_constr iv e.ints (0, max - 1) in
        ints, e.iv_map
      with
      | Not_found ->
        let iv = IntVar.mk ~name:(Xid x) () in
        let ints = IE.intv_constr iv (IE.set_top iv e.ints) (0, max - 1) in
        let iv_map = XIdm.add x (IVvar iv) e.iv_map in
        ints, iv_map
    in
    let cps = CE.intv_constr x e.cps (0, max - 1) in
    {e with ints; iv_map; cps}

  let restrict_to_blocks x e =
    (* Assumes that x is already the result of unalias *)
    let ints, iv_map =
      try
        let iv =
          match XIdm.find x e.iv_map with
          | IVvar v -> v
          | IVblock _ -> raise Not_found
        in
        let ints = IE.set_bottom iv e.ints in
        ints, XIdm.remove x e.iv_map
      with
      | Not_found -> e.ints, e.iv_map
    in
    let cps = CE.set_bottom x e.cps in
    {e with ints; iv_map; cps}

  let restrict_to_ints optmax x e =
    let e =
      match optmax with
      | None -> e
      | Some max -> restrict_ints_to_range max x e
    in
    let blocks = BE.set_bottom x e.blocks in
    let other = TE.set_bottom x e.other in
    {e with blocks; other}

  let constr ~hedge ~s ~x ~env:e cstr =
    let xid = (x,s) in
    (* DEBUG *)
    (* Format.eprintf "Constraint on %a:@.%a@." *)
    (*   ProgId.print xid *)
    (*   print_env e; *)
    let ids = unalias xid e in
    if Vals.exists (fun x -> not (TE.var_is_bottom x e.other)) ids
    then e
    else
      match cstr with
      | Ccp _
      | Cbool false -> (* Cbool false is more or less equivalent to Ccp 0 *)
        let fold_fun id acc =
          let cps = CE.constr id e.cps cstr in
          if CE.is_bottom cps then acc
          else CE.join acc cps
        in
        let cps = Vals.fold fold_fun ids CE.bottom in
        let ints =
          try
            match XIdm.find xid e.iv_map with
            | IVvar v ->
              IE.constr v e.ints cstr
            | IVblock _ ->
              (* As far as I can tell, it should be safe to return bottom *)
              IE.bottom
          with
          | Not_found -> e.ints
        in
        {e with ints; cps}
      | Ctag t ->
        let fold_fun id acc =
          let blocks = BE.constr id e.blocks cstr in
          if BE.is_bottom blocks then acc
          else join acc {e with blocks}
        in
        Vals.fold fold_fun ids bottom
      | Cbool true ->
        (* Cbool true means non-zero integer or block *)
        let fold_fun id acc =
          if not (BE.var_is_bottom id e.blocks)
          then join acc e
          else
            let cps = CE.constr id e.cps cstr in
            if CE.is_bottom cps then acc
            else join acc {e with cps}
        in
        let res =
          Vals.fold fold_fun ids bottom
        in
        let ints =
          try
            match XIdm.find xid e.iv_map with
            | IVvar v ->
              IE.constr v e.ints cstr
            | IVblock arr ->
              e.ints
          with
          | Not_found -> e.ints
        in
        {res with ints}
      | Ctype typ ->
        (* TODO: make it an external, recursive function *)
         begin
           let open Type_description in
           match typ with
           | Tyvar _ | Tyother -> e
           | Tyarrow (_,_) ->
             (* Closure: restrict to blocks *)
             Vals.fold restrict_to_blocks ids e
           | Tytuple l ->
             (* TODO: Deeper exploration *)
             Vals.fold restrict_to_blocks ids e
           | Tyconstr (tcstr, _) ->
             begin
               match tcstr.tydecl.tykind with
               | Tyabstract -> (* TODO: special cases for ints, ... *) e
               | Tyvariant l ->
                 let cps_only, blocks_only, cp_len =
                   List.fold_left
                     (fun (c, b, l) cstr ->
                        if cstr.cargs = [] then (c, false, l + 1)
                        else (false, b, l)
                     )
                     (true, true, 0)
                     l
                 in
                 if cps_only then
                   Vals.fold (restrict_to_ints (Some cp_len)) ids e
                 else if blocks_only then Vals.fold restrict_to_blocks ids e
                 else Vals.fold (restrict_ints_to_range cp_len) ids e
               | Tyrecord _
               | Tyopen -> Vals.fold restrict_to_blocks ids e
             end
         end
      | Cstring _ | Cnotstring _ -> e

  let allocations ~hedge ~s ~env:e alloc_list =
    let trans_args args = List.map (fun x -> unalias (x,s) e) args in
    let blocks =
      List.fold_left
        (fun acc (x, _alloc_id, alloc, args) ->
           BE.allocate acc (x,s) alloc (trans_args args))
        e.blocks
        alloc_list
    in
    let ints, iv_map =
      let one_alloc (iacc, macc) (x, _alloc_id, _alloc, args) =
        let xid = (x,s) in
        let args = List.map (fun x -> (x,s)) args in
        let len = List.length args in
        let iv_arr = Array.make len None in
        let rec init acc off = function
          | [] -> acc
          | y :: tl ->
            begin
              try
                let y_var = XIdm.find y e.iv_map in
                let (iv, acc') =
                  duplicate_ivar ~src:y_var ~dst:(Field (Xid xid, off)) acc
                in
                iv_arr.(off) <- Some iv;
                init acc' (off+1) tl
              with
              | Not_found -> init acc (off+1) tl
            end
        in
        let ints = init iacc 0 args in
        let iv_map = XIdm.add xid (IVblock iv_arr) macc in
        (ints, iv_map)
      in
      List.fold_left one_alloc (e.ints, e.iv_map) alloc_list
    in
    {e with ints; iv_map; blocks}

  let pure_unknown_result ~xid ~eid ~env =
    let topped v = {env with other = TE.top v env.other} in
    (topped xid, topped eid)

  let impure_unknown_result ~xid ~eid ~env args =
    let env =
      List.fold_left
        (fun acc arg ->
           let ints =
             try
               let iv = XIdm.find arg env.iv_map in
               set_top_rec acc.ints iv
             with
             | Not_found -> acc.ints
           in
           let cps = CE.set_top arg acc.cps in
           let blocks = BE.set_top arg acc.blocks in
           let alias = XIdm.add arg Vals.Top acc.alias in
           let other = TE.top arg env.other in
           {acc with ints; cps; blocks; alias; other})
        env
        args
    in
    pure_unknown_result ~xid ~eid ~env

  let ccall ~hedge ~s ~x ~e ~env:env pdesc args =
    let xid = (x,s) in
    let eid = (e,s) in
    (* let args = List.map (fun x -> (x,s)) args in *)
    (* For now, make the unsound assumption that C primitives are pure *)
    pure_unknown_result ~xid ~eid ~env

  let lazy_force ~hedge ~s ~x ~e ~env:env var =
    let xid = (x,s) in
    let eid = (e,s) in
    let var = (var,s) in
    (* Lazy is uncommon enough that we can be sound about it *)
    impure_unknown_result ~xid ~eid ~env [var]

  let send ~hedge ~s ~x ~e ~env:env obj meth =
    let xid = (x,s) in
    let eid = (e,s) in
    let obj = (obj,s) in
    let meth = (meth,s) in
    (* Objects are unhandled *)
    impure_unknown_result ~xid ~eid ~env [obj; meth]

  let to_register e xid =
    let ints, ivo =
      try
        let xiv = XIdm.find xid e.iv_map in
        let iv, ints =
          duplicate_ivar ~src:xiv ~dst:Anon e.ints
        in
        (ints, Some iv)
      with
      | Not_found -> (e.ints, None)
    in
    let ids = unalias xid e in
    ints, (ivo, ids)

  let app_prep ~hedge ~s ~x ~env:e ~f ~arg =
    let f = (f,s) in
    let arg = (arg,s) in
    let ints, freg = to_register e f in
    let ints, areg = to_register {e with ints} arg in
    {e with ints; app = Some (freg, areg)}

  let gen_app_ret xid e var =
    let ints, iv_map =
      try
        let iv = XIdm.find var e.iv_map in
        let (iv', ints') = duplicate_ivar ~src:iv ~dst:(Xid xid) e.ints in
        (ints', XIdm.add xid iv' e.iv_map)
      with
      | Not_found -> e.ints, e.iv_map
    in
    let iv_map = XIdm.remove var iv_map in
    let aliases = unalias var e in
    assert (not (Vals.mem xid aliases));
    let alias = XIdm.add xid aliases e.alias in
    let alias = XIdm.remove var alias in
    {e with ints; iv_map; alias}

  let app_return ~hedge ~s ~x ~env:e =
    let xid = (x,s) in
    match e.return with
    | None -> failwith "Missing return register"
    | Some reg ->
       load_from_register xid reg e

  let app_exn ~hedge ~s ~x ~env:e =
    let xid = (x,s) in
    match e.exn_return with
    | None -> failwith "Missing retexn register"
    | Some reg ->
       load_from_register xid reg e

  let app ~hedge ~s ~x ~e ~env =
    let f_ids =
      match env.app with
      | None -> failwith "Missing application preparation"
      | Some ((_,f_ids),_) -> f_ids
    in
    Vals.fold
      (fun id acc ->
         match BE.get_fun id env.blocks with
         | None -> acc
         | Some f -> if List.mem f acc then acc else f::acc)
      f_ids
      []

  let return ~hedge ~s ~env:e var =
    let var = (var,s) in
    let ints, reg = to_register e var in
    {e with ints; return = Some reg}

  let retexn ~hedge ~s ~env:e var =
    let var = (var,s) in
    let ints, reg = to_register e var in
    {e with ints; exn_return = Some reg}

  let print_var e ppf xid =
    let int_v =
      try
        match XIdm.find xid e.iv_map with
        | IVvar v ->
          Some (
            Format.asprintf "Variable %a@ in@ %a"
              IntVar.print v
              IE.print e.ints)
        | IVblock _ ->
          None
      with
      | Not_found -> None
    in
    let cp_v =
      if CE.var_is_bottom xid e.cps
      then None
      else
        Some (Format.asprintf "%a" (CE.print_var e.cps) xid)
    in
    let block_v =
      if BE.var_is_bottom xid e.blocks
      then None
      else
        Some (Format.asprintf "%a" (BE.print_var e.blocks) xid)
    in
    let other_v =
      if TE.var_is_bottom xid e.other
      then None
      else
        Some (Format.asprintf "%a" (TE.print_var e.other) xid)
    in
    let list =
      List.fold_left
        (fun acc -> function None -> acc | Some v -> v :: acc)
        []
        [int_v; cp_v; block_v; other_v]
    in
    let rec print_list ppf = function
      | [] -> Format.fprintf ppf "<Bot>"
      | [v] -> Format.fprintf ppf "%s" v
      | v :: tail -> Format.fprintf ppf "%s@ \\/@ %a" v print_list tail
    in
    print_list ppf list

  let print ?(print_xid = XId.print) ppf x e =
    let xid = (x,Stack.empty) in
    let p_id = ProgId.print_gen ~print_xid false in
    let aliases = unalias xid e in
    let print_one ppf x =
      Format.fprintf ppf "@[as %a:@ %a@]" p_id x (print_var e) x
    in
    let print_aliases ppf al =
      Vals.print_abs print_one ppf al
    in
    Format.fprintf ppf "Var %a:@ %a" p_id xid print_aliases aliases

  let endlet ~hedge ~s ~env l =
    let remove_xid env xid =
      (* We only remove the xid from the integer map,
         since other parts of the environment need
         the ability to refer to xids that went out of scope. *)
      let rec all_vars = function
        | IVvar v -> [v]
        | IVblock arr ->
          Array.fold_left
            (fun acc ivo ->
               match ivo with
               | None -> acc
               | Some iv -> (all_vars iv) @ acc
            )
            []
            arr
      in
      try
        let iv = XIdm.find (xid, s) env.iv_map in
        let vars = all_vars iv in
        let iv_map = XIdm.remove (xid, s) env.iv_map in
        let ints = IE.proj vars env.ints in
        {env with ints; iv_map}
      with
      | Not_found -> env
    in
    List.fold_left remove_xid env l
end

module Maker
    (Ints: IEsig.IE)
    (H: Hgraph_types.CloneOrderedHashedType)
    (Stack: Stack_types.Stack) :
  AnalEnv.S with type hedge = H.t
             and type t = Env(Stack)(Ints).t
             and module Stack = Stack
=
struct
  type hedge = H.t
  include (Env(Stack)(Ints))
  module Stack = Stack
end
