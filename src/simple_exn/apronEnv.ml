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

module type ApronDom = sig
  type t

  val man: t Apron.Manager.t
end

module Make (AD: ApronDom) (Vars: IEsig.Vars) = struct
  module AA = Apron.Abstract1
  module AE = Apron.Texpr1
  module AC = Apron.Tcons1

  module Varm = Map.Make(Vars)

  type expr =
    | E of AE.t
    | C of AC.earray
    | N

  type t =
    {
      aval: AD.t AA.t;
      exprs: expr Varm.t;
    }

  let avar x = Apron.Var.of_string (Vars.to_string x)

  let find_term_by_xid x m =
    let rec aux = function
      | N -> N
      | C arr -> C arr
      | E e ->
        begin
          match AE.to_expr e with
          | AE.Var v ->
            let m' = Varm.filter (fun x _ -> avar x = v) m in
            if Varm.is_empty m' then E e
            else let (_,ex) = Varm.choose m' in aux ex 
          | _ -> E e
        end
    in
    try
      aux (Varm.find x m)
    with
    | Not_found -> N

  let rec convert_texpr_expr var_conv = function
    | AE.Cst c -> AE.Cst c
    | AE.Var v -> AE.Var (var_conv v)
    | AE.Unop (op, ex, t, r) ->
      AE.Unop (op, convert_texpr_expr var_conv ex, t, r)
    | AE.Binop (op, ex1, ex2, t, r) ->
      let e1 = convert_texpr_expr var_conv ex1 in
      let e2 = convert_texpr_expr var_conv ex2 in
      AE.Binop (op, e1, e2, t, r)

  exception Removed_var

  let comp_var_conv renamings b_partial v =
    match List.filter (fun (old_iv,_) -> avar old_iv = v) renamings with
    | [] -> if b_partial then v else raise Removed_var
    | (_, iv) :: _ -> avar iv

  let convert_texpr renamings b_partial env e =
    let var_conv = comp_var_conv renamings b_partial in
    let ex = AE.to_expr e in
    let ex' = convert_texpr_expr var_conv ex in
    AE.of_expr env ex'

  let convert_tcons_array renamings b_partial env carr =
    let len = AC.array_length carr in
    let r = AC.array_make env len in
    for i = 0 to len - 1
    do
      let cons = AC.array_get carr i in
      let e = AC.get_texpr1 cons in
      let typ = AC.get_typ cons in
      let e' = convert_texpr renamings b_partial env e in
      AC.array_set r i (AC.make e' typ)
    done;
    r

  let convert_expr renamings b_partial env = function
    | N -> N
    | E e ->
      begin
        try E (convert_texpr renamings b_partial env e)
        with
        | Removed_var -> N
      end
    | C carr ->
      begin
        try C (convert_tcons_array renamings b_partial env carr)
        with
        | Removed_var -> N
      end

  let env_print = fun fmt -> Apron.Environment.print fmt
  let env_add x env =
    let module E = Apron.Environment in
    if E.mem_var env x then env
    else E.add env [|x|] [||]

  exception Environment_incompatibility

  let texpr_with_new_env env e =
    let rec aux = function
      | AE.Cst coeff -> AE.Cst coeff
      | AE.Var var ->
        if Apron.Environment.mem_var env var
        then AE.Var var
        else raise Environment_incompatibility
      | AE.Unop (unop, expr, typ, round) ->
        AE.Unop (unop, aux expr, typ, round)
      | AE.Binop (binop, expr1, expr2, typ, round) ->
        AE.Binop (binop, aux expr1, aux expr2, typ, round)
    in
    AE.of_expr env (aux (AE.to_expr e))

  let convert_to_env y v env =
    try
      match Varm.find y v.exprs with
      | N | C _ -> AE.var env (avar y)
      | E t ->
        try
          texpr_with_new_env env t
        with
        | Environment_incompatibility -> AE.var env (avar y)
    with
    | Not_found -> AE.var env (avar y)

  let int_coeff = Apron.Coeff.s_of_int

  let man = AD.man

  let empty_env = Apron.Environment.make [||] [||]

  let print_expr ppf = function
    | N -> Format.fprintf ppf "<Unknown>"
    | E e -> AE.print ppf e
    | C a ->
      let print_a ppf a = AC.array_print ~sep:"/\\" ppf a in
      Format.fprintf ppf "Not(@[%a@])" print_a a

  let print_exprs =
    let print_expr ppf e = Format.fprintf ppf "@ %a" print_expr e in
    Varm.print_sep (fun ppf -> Format.fprintf ppf ";@ ") print_expr

  let print ppf v =
    Format.fprintf ppf "Apron:@ @[%a@]@.with exprs:@ @[%a@]"
      AA.print v.aval
      print_exprs v.exprs

  let debug_env msg var env =
    Format.eprintf "%s: %a in %a@." msg Vars.print var env_print env

  let bottom =
    {
      aval = AA.bottom man empty_env;
      exprs = Varm.empty;
    }

  let empty =
    {
      aval = AA.top man empty_env;
      exprs = Varm.empty;
    }

  let is_bottom v = AA.is_bottom man v.aval

  let var_is_bottom x v =
    if Varm.mem x v.exprs
    then
      (* Apron abstract domains have intersection semantics, so if a variable
         has no possible value then the whole state is bottom *)
      not (Apron.Environment.mem_var (AA.env v.aval) (avar x))
      || AA.is_bottom man v.aval
    else
      true

  let equal_constr_arr a1 a2 =
    if AC.array_length a1 = AC.array_length a2
    then
      begin
        let l = AC.array_length a1 in
        let res = ref true in
        for i = 0 to l - 1
        do
          let tc1 = AC.array_get a1 i in
          let tc2 = AC.array_get a2 i in
          let te1 = AC.get_texpr1 tc1 in
          let te2 = AC.get_texpr1 tc2 in
          res := !res && AE.to_expr te1 = AE.to_expr te2
        done;
        !res
      end
    else false

  let merge_expr e1 e2 =
    match e1, e2 with
    | N, _ | _, N -> N
    | E t1, E t2 ->
      (* Convert to Apron.Texpr1.expr for better equality semantics *)
      if AE.to_expr t1 = AE.to_expr t2
      then E t1
      else N
    | C a1, C a2 ->
      if equal_constr_arr a1 a2 then C a1 else N
    | E _, C _ | C _, E _ -> N

  let op_with_same_env op v1 v2 =
    let module E = Apron.Environment in
    let env1 = AA.env v1.aval in
    let env2 = AA.env v2.aval in
    if E.equal env1 env2
    then op man v1.aval v2.aval
    else
      (* Project every variable appearing in one side only *)
      let (iv1, rv1) = E.vars env1 in
      let ivs =
        Array.fold_right
          (fun v acc -> if E.mem_var env2 v then v :: acc else acc)
          iv1
          []
      in
      let rvs =
        Array.fold_right
          (fun v acc -> if E.mem_var env2 v then v :: acc else acc)
          rv1
          []
      in
      let env = E.make (Array.of_list ivs) (Array.of_list rvs) in
      let a1 = AA.change_environment man v1.aval env false in
      let a2 = AA.change_environment man v2.aval env false in
      op man a1 a2

  let join v1 v2 =
    if is_bottom v1 then v2
    else if is_bottom v2 then v1
    else
      {
        aval = op_with_same_env AA.join v1 v2;
        exprs = Varm.merge (map_merge_join merge_expr) v1.exprs v2.exprs;
      }

  let widening v1 v2 =
    {
      aval = op_with_same_env AA.widening v1 v2;
      exprs = Varm.merge (map_merge_join merge_expr) v1.exprs v2.exprs;
    }

  let leq v1 v2 =
    op_with_same_env AA.is_leq v1 v2

  let neg_constr tc =
    let te = AC.get_texpr1 tc in
    let nte = AE.unop AE.Neg te AE.Int AE.Near in
    let typ = AC.get_typ tc in
    match typ with
    | AC.EQ -> AC.make te AC.DISEQ
    | AC.DISEQ -> AC.make te AC.EQ
    | AC.SUP -> AC.make nte AC.SUPEQ
    | AC.SUPEQ -> AC.make nte AC.SUP
    | AC.EQMOD _ -> assert false

  let neg_constr_arr a =
    if AC.array_length a = 1
    then
      begin
        let tc = neg_constr (AC.array_get a 0) in
        let a_cons = AC.array_make (AC.get_env tc) 1 in
        AC.array_set a_cons 0 tc;
        C a_cons
      end
    else
      N

  let primitive x v p args =
    let x_v = avar x in
    let env = env_add x_v (AA.env v.aval) in
    let from_expr expr =
      let v_ext = AA.change_environment man v.aval env false in
      {
        aval = AA.assign_texpr man v_ext x_v expr None;
        exprs = Varm.add x (E expr) v.exprs;
      }
    in
    let unhandled () =
      (* Unknown operation : add x to the environment, unconstrained *)
      let v_ext = AA.change_environment man v.aval env false in
      {
        aval = v_ext;
        exprs = Varm.add x N v.exprs;
      }
    in
    let mk_neg y =
      let y_t = convert_to_env y v env in
      AE.unop AE.Neg y_t AE.Int AE.Near
    in
    let mk_bin binop y z =
      let y_t = convert_to_env y v env in
      let z_t = convert_to_env z v env in
      (* DEBUG *)
      (* Format.eprintf "mk_bin: %a %a -> %a %a@." *)
      (*   Vars.print y *)
      (*   Vars.print z *)
      (*   AE.print y_t *)
      (*   AE.print z_t; *)
      AE.binop binop y_t z_t AE.Int AE.Near
    in
    let mk_add = mk_bin AE.Add in
    let mk_sub = mk_bin AE.Sub in
    let mk_mul = mk_bin AE.Mul in
    let mk_div = mk_bin AE.Div in
    let mk_mod = mk_bin AE.Mod in
    let mk_off off y =
      let y_t = convert_to_env y v env in
      let z_t = AE.cst env (int_coeff off) in
      AE.binop AE.Add y_t z_t AE.Int AE.Near
    in
    match p, args with
    | XPbuiltin, _ -> v
    | XPnegint, [y] -> from_expr (mk_neg y)
    | XPaddint, [y; z] -> from_expr (mk_add y z)
    | XPsubint, [y; z] -> from_expr (mk_sub y z)
    | XPmulint, [y; z] -> from_expr (mk_mul y z)
    | XPdivint, [y; z] -> from_expr (mk_div y z)
    | XPmodint, [y; z] -> from_expr (mk_mod y z)
    | XPandint, [y; z] -> unhandled ()
    | XPorint, [y; z] -> unhandled ()
    | XPxorint, [y; z] -> unhandled ()
    | XPlslint, [y; z] -> unhandled ()
    | XPlsrint, [y; z] -> unhandled ()
    | XPasrint, [y; z] -> unhandled ()
    | XPintcomp c, [y; z] ->
      (* NOTE: because of the isout case, it is easier to store the negation
         of the boolean expression *)
      begin
        let dir_t = mk_sub y z in
        let rev_t = mk_sub z y in
        let open Lambda in
        let t_cons =
          match c with
          | Ceq -> AC.make dir_t AC.DISEQ
          | Cneq -> AC.make dir_t AC.EQ
          | Clt -> AC.make dir_t AC.SUPEQ
          | Cgt -> AC.make rev_t AC.SUPEQ
          | Cle -> AC.make dir_t AC.SUP
          | Cge -> AC.make rev_t AC.SUP
        in
        let a_cons = AC.array_make env 1 in
        AC.array_set a_cons 0 t_cons;
        (* DEBUG *)
        Format.eprintf "Expr: %a@.Constraint: %a@.As array:%a@."
          AE.print dir_t
          AC.print t_cons
          (fun fmt -> AC.array_print fmt) a_cons
        ;
        {
          aval = AA.change_environment man v.aval env false ;
          exprs = Varm.add x (C a_cons) v.exprs;
        }
      end
    | XPoffsetint off, [y] -> from_expr (mk_off off y)
    | XPisout, [y; z] ->
      (* isout is basically a disjunction, but AC.earray are interpreted as
         conjunctions; to make the code easier, we will store the negation
         instead. *)
      begin
        let y_t = convert_to_env y v env in
        let z_t = convert_to_env z v env in
        let sub_t = AE.binop AE.Sub y_t z_t AE.Int AE.Near in
        (* y >= z *)
        let tc0 = AC.make sub_t AC.SUPEQ in
        (* z >= 0 *)
        let tc1 = AC.make z_t AC.SUPEQ in
        let a_cons = AC.array_make env 2 in
        AC.array_set a_cons 0 tc0;
        AC.array_set a_cons 1 tc1;
        {
          aval = AA.change_environment man v.aval env false ;
          exprs = Varm.add x (C a_cons) v.exprs;
        }
      end
    | XPnot, [y] ->
      begin
        try
          let not_y_t =
            match Varm.find y v.exprs with
            | N -> N
            | E _ -> N
            | C a ->
              neg_constr_arr a
          in
          {
            aval = AA.change_environment man v.aval env false ;
            exprs = Varm.add x not_y_t v.exprs;
          }
        with
        | Not_found ->
          {
            aval = AA.change_environment man v.aval env false ;
            exprs = Varm.add x N v.exprs;
          }
      end
    | _, _ -> failwith "IntEnv: Unsupported primitive"

  let intcomp v c y z =
    let env = AA.env v.aval in
    let mk_sub y z =
      let y_t = convert_to_env y v env in
      let z_t = convert_to_env z v env in
      AE.binop AE.Sub y_t z_t AE.Int AE.Near
    in
    let dir_t = mk_sub y z in
    let rev_t = mk_sub z y in
    let opp = function
      | AC.DISEQ -> AC.EQ
      | AC.EQ -> AC.DISEQ
      | AC.SUPEQ -> AC.SUP
      | AC.SUP -> AC.SUPEQ
      | _ -> assert false
    in
    let mk_true_false y_minus_z comp =
      (AC.make (if y_minus_z then dir_t else rev_t) comp,
       AC.make (if y_minus_z then rev_t else dir_t) (opp comp))
    in
    let open Lambda in
    let tcons_true, tcons_false =
      match c with
      | Ceq -> mk_true_false true AC.EQ
      | Cneq -> mk_true_false true AC.DISEQ
      | Clt -> mk_true_false false AC.SUP
      | Cgt -> mk_true_false true AC.SUP
      | Cle -> mk_true_false false AC.SUPEQ
      | Cge -> mk_true_false true AC.SUPEQ
    in
    match AA.sat_tcons man v.aval tcons_true,
          AA.sat_tcons man v.aval tcons_false with
    | true, true -> IEsig.Bot
    | true, false -> IEsig.True
    | false, true -> IEsig.False
    | false, false -> IEsig.Top

  let var x v y =
    (* DEBUG *)
    Format.eprintf "ApronEnv.var: %a <- %a in %a@."
      Vars.print x
      Vars.print y
      print v;
    let x_v = avar x in
    let env = env_add x_v (AA.env v.aval) in
    let v_ext = AA.change_environment man v.aval env false in
    let x_t = (AE.var env (avar y)) in
    let aval = AA.assign_texpr_array man v_ext [|x_v|] [|x_t|] None in
    { aval; exprs = Varm.add x (E x_t) v.exprs }

  let update strong x v y =
    let x_v = avar x in
    let env = AA.env v.aval in
    assert (Apron.Environment.mem_var env x_v);
    let x_t = (AE.var env (avar y)) in
    if strong
    then
      { aval = AA.assign_texpr_array man v.aval [|x_v|] [|x_t|] None;
        exprs = Varm.add x (E x_t) v.exprs }
    else
      (* Weak update: Assign to a fresh variable, then merge the new variable
         with the existing values for the target variable and delete it *)
      let tmp_var = Apron.Var.of_string "#######" in
      let tmp_aval = AA.assign_texpr_array man v.aval [|tmp_var|] [|x_t|] None in
      let aval = AA.fold man tmp_aval [|x_v; tmp_var|] in
      { aval; exprs = Varm.add x N v.exprs }

  let set_top x v =
    let x_v = avar x in
    let env = AA.env v.aval in
    let aval =
      if Apron.Environment.mem_var env x_v
      then
        let projected = AA.forget_array man v.aval [|x_v|] false in
        AA.change_environment man projected env false
      else
        let env = Apron.Environment.add env [|x_v|] [||] in
        AA.change_environment man v.aval env false
    in
    let exprs = Varm.add x N v.exprs in
    { aval; exprs }

  let set_bottom x v =
    (* See comment in IEsig.mli: projection is the intended behaviour,
       not actually setting the whole abstract value to bottom. *)
    let x_v = avar x in
    let env = AA.env v.aval in
    let aval =
      if Apron.Environment.mem_var env x_v
      then
        let env = Apron.Environment.remove env [|x_v|] in
        AA.change_environment man v.aval env false
      else
        v.aval
    in
    let exprs = Varm.remove x v.exprs in
    { aval; exprs }

  let const x v cst =
    let x_v = avar x in
    let env = env_add x_v (AA.env v.aval) in
    (* debug_env "Const entry" x env; *)
    let v_ext = AA.change_environment man v.aval env false in
    let open Lambda in
    match cst with
    | Const_base bcst ->
      begin
        let open Asttypes in
        match bcst with
        | Const_int ci ->
          let expr = AE.cst env (int_coeff ci) in
          {
            aval = AA.assign_texpr man v_ext x_v expr None;
            exprs = Varm.add x (E expr) v.exprs;
          }
        | _ ->
          (* For now, all other constants are unhandled *)
          {
            aval = v_ext;
            exprs = Varm.add x N v.exprs
          }
      end
    | Const_pointer cp ->
      (* Precise handling could be done with a finite set domain (e.g. Cps),
         but I'm too lazy to do it here, so it just becomes a constant *)
      let expr = AE.cst env (int_coeff cp) in
      {
        aval = AA.assign_texpr man v_ext x_v expr None;
        exprs = Varm.add x (E expr) v.exprs;
      }
    | _ ->
      (* Unhandled constants; still adding x to the environment (if it is not
         what you want, check the constant before calling this function) *)
      {
        aval = v_ext;
        exprs = Varm.add x N v.exprs
      }

  let forall_carr f a =
    let l = AC.array_length a in
    let res = ref true in
    for i = 0 to l - 1 do
      res := !res && f (AC.array_get a i)
    done;
    !res

  let constr x v cstr =
    if var_is_bottom x v then v
    else
      let x_v = avar x in
      let env = AA.env v.aval in
      (* debug_env "Constr entry" x env; *)
      let x_t = find_term_by_xid x v.exprs in
      let generic_case i =
        (* Generic case : force x to have value cp *)
        let x_expr =
          try AE.var env x_v
          with exn ->
            Format.eprintf "Error: missing var %a(%a) in env %a@."
              Vars.print x
              Apron.Var.print x_v
              env_print env
            ;
            raise exn
        in
        let c_expr = AE.cst env (int_coeff i) in
        let s_expr = AE.binop AE.Sub x_expr c_expr AE.Int AE.Near in
        let t_cons = AC.make s_expr AC.EQ in
        let a_cons = AC.array_make env 1 in
        AC.array_set a_cons 0 t_cons;
        {v with aval = AA.meet_tcons_array man v.aval a_cons}
      in
      match cstr with
      | Ccp cp ->
        begin
          match x_t with
          | N
          | E _ -> generic_case cp
          | C a ->
            (* Recall that because of isout, we are storing negations of the
               constraints *)
            begin
              match cp with
              | 0 -> (* false *)
                {v with aval = AA.meet_tcons_array man v.aval a}
              | 1 -> (* true *)
                begin
                  match neg_constr_arr a with
                  | N ->
                    (* Negation cannot be represented exactly;
                       fallback to default case *)
                    generic_case cp
                  | C neg_a ->
                    {v with aval = AA.meet_tcons_array man v.aval neg_a}
                  | E _ -> assert false
                end
              | _ -> (* Probably an error, default the the generic case *)
                generic_case cp
            end
        end
      | Cbool false ->
        (* Same as case 0 above *)
        begin
          match x_t with
          | N | E _ -> generic_case 0
          | C a ->
            match neg_constr_arr a with
            | E _ -> assert false
            | N ->
              {v with aval = AA.meet_tcons_array man v.aval a}
            | C a' ->
              if forall_carr (fun cons -> AA.sat_tcons man v.aval cons) a'
              then
                (* The negation of contraint a is always satisfied, so
                   we know that this branch is bottom *)
                {v with aval = AA.bottom man (AA.env v.aval)}
              else
                {v with aval = AA.meet_tcons_array man v.aval a}
        end
      | Cbool true ->
        (* Same as case 1 above, except that Cbool true can return true for values
           different from 1 *)
        begin
          match x_t with
          | N | E _ -> v
          | C a ->
            begin
              match neg_constr_arr a with
              | N ->
                (* Negation cannot be represented exactly;
                   fallback to default case for 1 *)
                (* In this particular case, we know that x (or x's negation)
                   is a boolean expression, so no need to consider the cases
                   where x may be a block *)
                generic_case 1
              | C neg_a ->
                if forall_carr (fun cons -> AA.sat_tcons man v.aval cons) a
                then
                  (* The contraint a is always satisfied, so
                     we know that this branch is bottom *)
                  {v with aval = AA.bottom man (AA.env v.aval)}
                else
                  {v with aval = AA.meet_tcons_array man v.aval neg_a}
              | E _ -> assert false
            end
        end
      | Ctag _ | Ctype _ | Cstring _ | Cnotstring _ -> v (* Not handled *)

  let intv_constr x v (l,h) =
    (* TODO: actual implementation *)
    v

  let primitive x v p args =
    if is_bottom v then v else primitive x v p args

  let const x v cst =
    if is_bottom v then v else const x v cst

  let constr x v cstr =
    if is_bottom v then v else constr x v cstr

  let print_var v ppf x =
    try
      let expr = Varm.find x v.exprs in
      Format.fprintf ppf "Var %a:@ %a@.Constraints:@ %a@."
        Vars.print x
        print_expr expr
        AA.print v.aval
    with
    | Not_found -> Format.fprintf ppf "<Unknown>"

  let renaming l v =
    let old_l, new_l = List.split l in
    let old_var = Array.of_list (List.map avar old_l) in
    let new_var = Array.of_list (List.map avar new_l) in
    let new_env = Apron.Environment.make new_var [||] in
    let aval =
      AA.change_environment
        man
        (AA.rename_array man v.aval old_var new_var)
        new_env
        false
    in
    let exprs =
      Varm.fold
        (fun k v acc ->
           try
             let k' = List.assoc k l in
             Varm.add k' (convert_expr l false new_env v) acc
           with
           | Not_found -> acc)
        v.exprs
        Varm.empty
    in
    { aval; exprs }

  let partial_renaming l v =
    let old_l, new_l = List.split l in
    let old_var = Array.of_list (List.map avar old_l) in
    let new_var = Array.of_list (List.map avar new_l) in
    let aval =
      AA.rename_array man v.aval old_var new_var
    in
    let env = AA.env aval in
    let exprs =
      List.fold_left
        (fun acc (i,j) ->
           if Varm.mem i acc
           then
             Varm.add j
               (convert_expr l true env (Varm.find i acc))
               acc
           else acc)
        v.exprs
        l
    in
    { aval; exprs }

  let proj l v =
    let var_array = Array.of_list (List.map avar l) in
    let aval = AA.forget_array man v.aval var_array false in
    let exprs = Varm.filter (fun var _ -> not (List.mem var l)) v.exprs in
    { aval; exprs }
end
