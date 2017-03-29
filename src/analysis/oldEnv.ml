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

open Common_types
open Lambda

module E
    (H : Hgraph_types.CloneOrderedHashedType)
    (Stack: Stack_types.Stack)
  : AnalEnv.S
  with type t = Envs.t
   and type hedge = H.t
   and module Stack = Stack
  =
  struct

    type t = Envs.t
    type hedge = H.t

    module Stack = Stack

    open Envs
    open Access

    let bottom = bottom
    let is_bottom = is_bottom
    let empty = empty

    let meet _ _ = failwith "oldEnv meet non implemented"
    let join = join
    let widening = widening
    let leq = Manipulation.is_leq

    let intop2_of_prim o =
      let open Int_interv in
      match o with
      | XPaddint -> add
      | XPsubint -> sub
      | XPmulint -> mul
      | XPdivint -> div
      | XPmodint -> modulo
      | XPandint -> band
      | XPorint -> bor
      | XPxorint -> bxor
      | XPlslint -> blsl
      | XPlsrint -> blsr
      | XPasrint -> basr
      | _ -> assert false


    let rev_comp = function
      | Ceq -> Cneq | Cneq -> Ceq | Clt -> Cge | Cgt -> Cle | Cle -> Cgt | Cge -> Clt

    let may_rev_comp c cp =
      if cp = 0 then rev_comp c else c

    let builtin_match_failure = Strings.singleton "Match_failure"
    let builtin_assert_failure = Strings.singleton "Assert_failure"
    let builtin_failure = Strings.singleton "Failure"
    let builtin_not_found = Strings.singleton "Not_found"
    let builtin_out_of_memory = Strings.singleton "Out_of_memory"
    let builtin_stack_overflow = Strings.singleton "Stack_overflow"
    let builtin_sys_error = Strings.singleton "Sys_error"
    let builtin_end_of_file = Strings.singleton "End_of_file"
    let builtin_division_by_zero = Strings.singleton "Division_by_zero"
    let builtin_sys_blocked_io = Strings.singleton "Sys_blocked_io"
    let builtin_undefined_recursive_module = Strings.singleton "Undefined_recursive_module"

    let builtin_of_id i =
      match XId.stamp i with
      | 16 -> builtin_match_failure
      | 17 -> builtin_assert_failure
      | 18 -> builtin_failure
      | 19 -> builtin_not_found
      | 20 -> builtin_out_of_memory
      | 21 -> builtin_stack_overflow
      | 22 -> builtin_sys_error
      | 23 -> builtin_end_of_file
      | 24 -> builtin_division_by_zero
      | 25 -> builtin_sys_blocked_io
      | 26 -> builtin_undefined_recursive_module
      | _ -> assert false



    let warn ~env msg = Format.fprintf ppf "%s@." msg; Env env



    let primitive ~hedge ~s ~x ~(env:Envs.t) p l =
      let act d = Exprs.set d (Prim (p,l)) in
      (* let sa d e = set_env x (act d) e in *)
      let e = !!env in
      let set v = set_env x v e
      (* and get x = get_env x env *)
      and getis x = get_idents x e
      (* and getd x = get_data x env *)
      (* and setd i x = set_data i x e *)
      (* and vunit = Cp.singleton 0 *)
      in
      let sa v e = set_env x ( act v ) e in
      let sda loc v e = set_data loc (act v) e in
      let mkl xid  = Locations.of_xid xid in
      let seti = set_ident
      in
      let unit env = sa (Cps.singleton 0) env in
      let dsaw msg =
        let env = !! ( set ( act Data.top ) ) in
        warn ~env msg
      in
      begin
        match p, l with
        | XPbuiltin, [i] -> sa ( builtin_of_id i ) e
        (* Operations on heap blocks *)
        | XPfield i, [b] ->
          on_xid (fun locb db e ->
              set_data locb (Blocks.restrict ~has_field:i db) e
              >? set_idents x (Blocks.get_field i db)
            ) b e
        | XPsetfield ( i, _ ), [b;v] ->
          let locv = getis v in
          on_xid (fun locb db e ->
              set_data locb (Blocks.sets_field i locv db) e )
            b e
          >? unit

        | XPfloatfield i, [b] -> dsaw "TODO: floatfield"
        | XPsetfloatfield i, [b;v] -> dsaw "TODO: setfloatfield"
        | XPduprecord (trepr,i), [r] -> dsaw "TODO: duprecord"

        (* Boolean not *)
        | XPnot, [i] ->
          let loc = mkl x in
          on_xid (fun _ di e ->
              sda loc (Bools.notb di) e) i e
          >! seti x loc
        (* Integer operations *)
        | XPnegint, [i] ->
          let loc = mkl x in
          on_xid (fun _ di e ->
              sda loc ( Int.op1 Int_interv.uminus di) e)
            i e
          >! seti x loc
        | XPaddint, [a;b]
        | XPsubint, [a;b]
        | XPmulint, [a;b]
        | XPdivint, [a;b]
        | XPmodint, [a;b]
        | XPandint, [a;b]
        | XPorint, [a;b]
        | XPxorint, [a;b]
        | XPlslint, [a;b]
        | XPlsrint, [a;b]
        | XPasrint, [a;b] ->
          let loc = mkl x in
          on_xid (fun _ da e ->
              on_xid (fun _ db e ->
                  sda loc ( Int.op2 ( intop2_of_prim p) da db) e
                ) a e ) b e
          >! seti x loc
        | XPintcomp c, [a;b] ->
          let loc = mkl x in
          on_xid (fun loca da e ->
              on_xid (fun locb db e ->
                  let res, a', b' = Int.comp c da db in
                  sda loc res e
                  >! set_data loca a'
                  >! set_data locb b' )
                b e ) a e
          >! seti x loc
        | XPoffsetint i, [a] ->
          let loc = mkl x in
          on_xid (fun _ da e ->
              sda loc (Int.op1 (Int_interv.addcst i) da) e
            ) a e
          >! seti x loc
        | XPoffsetref i, [a] ->
          let x' = XId.dual x in
          on_xid (fun loca da e ->
              let da = Blocks.restrict ~tag:0 ~size:1 da in
              Blocks.on_field (fun _ d e ->
                  let d = act (Int.op1 (Int_interv.addcst i) d) in
                  let nloc = Locations.of_xid x' in
                  set_data nloc d e
                  >! set_data loca (Blocks.singleton 0 [|Locations.Locs.singleton nloc|])
                ) 0 da e
            ) a e
            (*
(* Float operations *)
| XPintoffloat | XPfloatofint
| XPnegfloat | XPabsfloat
| XPaddfloat | XPsubfloat | XPmulfloat | XPdivfloat
| XPfloatcomp of comparison
(* String operations *)
| XPstringlength | XPstringrefu | XPstringsetu | XPstringrefs | XPstringsets *)

        (* Array operations *)
        | XParraylength _, [a] ->
          on_xid (fun _ a e ->
              sa ( Int.of_interv (Arrays.size a) ) e) a e
        | XParrayrefu _, [a;_] ->
          on_xid (fun _ a e ->
              set_idents x (Arrays.get a) e
            ) a e
        | XParraysetu _, [ai;_;i] ->
          on_xid (fun loca da e ->
              on_xid (fun loci _ e ->
                  set_data loca (Arrays.add_field da loci) e
                ) i e
            ) ai e
          >? unit
        (* Test if the argument is a block or an immediate integer *)
        | XPisint, [a] ->
          on_xid (fun loca da e ->
              sa ( Int.is_int e da ) e
            ) a e
        (* Test if the (integer) argument is outside an interval *)
        | XPisout, [m;i] ->
          on_xid (fun locm dm e ->
              on_xid (fun loci di e ->
                  let res, dm, di = Int.is_out dm di in
                  sa res e
                  >! set_data locm dm
                  >! set_data loci di
                ) i e
            ) m e

(*
(* Bitvect operations *)
| XPbittest
(* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
| XPbintofint of boxed_integer
| XPintofbint of boxed_integer
| XPcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
| XPnegbint of boxed_integer
| XPaddbint of boxed_integer
| XPsubbint of boxed_integer
| XPmulbint of boxed_integer
| XPdivbint of boxed_integer
| XPmodbint of boxed_integer
| XPandbint of boxed_integer
| XPorbint of boxed_integer
| XPxorbint of boxed_integer
| XPlslbint of boxed_integer
| XPlsrbint of boxed_integer
| XPasrbint of boxed_integer
| XPbintcomp of boxed_integer * comparison
(* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
| XPbigarrayref of bool * int * bigarray_kind * bigarray_layout
| XPbigarrayset of bool * int * bigarray_kind * bigarray_layout
(* size of the nth dimension of a big array *)
| XPbigarraydim of int
(* load/set 16,32,64 bits from a string: (unsafe)*)
| XPstring_load_16 of bool
| XPstring_load_32 of bool
| XPstring_load_64 of bool
| XPstring_set_16 of bool
| XPstring_set_32 of bool
| XPstring_set_64 of bool
(* load/set 16,32,64 bits from a
(char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
| XPbigstring_load_16 of bool
| XPbigstring_load_32 of bool
| XPbigstring_load_64 of bool
| XPbigstring_set_16 of bool
| XPbigstring_set_32 of bool
| XPbigstring_set_64 of bool *)
        (* Compile time constants *)
        | XPctconst c, [] ->
          sa
            ( (* let open Lambda in *)
              match c with
              | Big_endian -> Bools.of_bool Sys.big_endian
              | Word_size -> Int.singleton Sys.word_size
              | Ostype_unix -> Bools.of_bool Sys.unix
              | Ostype_win32 -> Bools.of_bool Sys.win32
              | Ostype_cygwin -> Bools.of_bool Sys.cygwin
            ) e
(*
(* byte swap *)
| XPbswap16
| XPbbswap of boxed_integer
*)
        (* Function handlers *)
        | XPfunfield i, [f] ->
          (* at this point, f is unique *)
          on_xid (fun _ d e ->
              set_idents x (Funs.field i d) e
            ) f e
        | XPgetfun fid, [] ->
          on_xid (fun loc d e ->
              if Funs.has_fid fid d
              then sa (Funs.fid fid d) e
              else Envs.bottom
            ) fun_xid e
          >? rm_env fun_xid
        | XPgetarg, [] ->
          set_idents x (getis arg_xid) e
          >! rm_env arg_xid
        (* Lastly, if everything fails, it means there's still work to get done !*)
        | prim, _ ->
          let str = Format.asprintf "TODO: primitives %a !@."
              Print_xlambda.primitive prim in
          dsaw str
      end

    let var ~hedge ~s ~x ~env i =
      let e = !!env in
      set_idents x (get_idents i e) e

    let rec constant env = function
      | Const_base c ->
        let open Asttypes in
        env,
        begin
          match c with
          | Const_int i -> Int.singleton i
          | Const_char c -> Int.singleton (Char.code c)
          | Const_string (s,_) -> Strings.singleton s
          | Const_float s -> Floats.singleton s
          | Const_int32 i -> Otherints.( singleton I32 i )
          | Const_int64 i -> Otherints.( singleton I64 i )
          | Const_nativeint i -> Otherints.( singleton IN i )
        end
      | Const_pointer i -> env, Cps.singleton i
      | Const_block (t,l) ->
        let (env, ids) =
          List.fold_left
            (fun (env,ids) c ->
               let env,d = constant env c in
               let env,i = reg_data d !!env in
               env,i::ids )
            (env,[]) l
        in
        let a = Array.of_list (List.rev_map Locations.Locs.singleton ids) in
        env, ( Blocks.singleton t a )
      | Const_float_array l ->
        let ids,env =
          List.fold_left
            (fun (ids,env) f ->
               let env,id = reg_data (Floats.singleton f) !!env in
               ((Locations.Locs.singleton id)::ids, env)
            ) ([], env) l in
        env, Arrays.singleton ids ( Int_interv.cst (List.length l)) Pfloatarray
      | Const_immstring s -> env, Strings.singleton s
    (* Data.singleton_string s *)

    let const ~hedge ~s ~x ~env c =
      let env,loc =
          let env, d = constant env c in
          let loc = Locations.of_xid x in
          let env = set_data loc (Exprs.set d (Const c)) !!env in
          env, loc
      in
      set_idents x (Locations.Locs.singleton loc) !!env



    let hfold f d env =
      let open Data in
      let es = d.expr in
      if Hinfos.is_empty es (* should only happen inside constants *)
      then !> env
      else Hinfos.fold (fun expr e -> Envs.join ( f expr env ) e ) es Envs.bottom

    let rec constraint_env_bool_var b xid (env:Envs.t') =
      (* Format.fprintf ppf "Constraining@ %a@." XId.print xid; *)
      on_xid (constraint_env_bool_folder b) xid env

    and constraint_env_bool_folder b loc d (env:Envs.t') =
      let general g f =
        if g env d
        then 
          constraint_env_bool_exprs b d env
          >! set_data loc (f d)
        else Envs.bottom
      in
      if b
      then general Ifcond.can_be_true Ifcond.set_true
      else general Ifcond.can_be_false Ifcond.set_false

    and constraint_env_bool_exprs bool d (env:Envs.t') =
      hfold (constraint_env_bool bool) d env

    and constraint_env_bool bool expr (env:Envs.t') =
      let cp = if bool then 1 else 0 in
      match expr with
      | Var x -> constraint_env_bool_var bool x env
      | Prim ( p, l ) ->
        begin
          match p, l with
          | XPintcomp c, [x;y]  ->
            let c = may_rev_comp c cp in
            on_xid (fun locx dx e ->
                on_xid (fun locy dy e ->
                    let dx,dy = Int.make_comp c dx dy in
                    if Data.is_bottom dx || Data.is_bottom dy
                    then Envs.bottom
                    else
                      set_data locx dx e
                      >! set_data locy dy
                  ) y e
              ) x env
          | XPsetfield _, _::_::[]
          | XPsetfloatfield _, _::_::[]
            when not bool -> Env env
          | XPfield i, [b] -> Env env (* we cannot tell anything in case the block is mutable *)
          | XPnot, [x] ->
            constraint_env_bool_var (not bool) x env
          | XPisint, [x] when bool ->
            on_xid (fun loc d e ->
                set_data loc (Int.restrict_intcp d) e
              ) x env
          | XPisint, [x] ->
            on_xid (fun loc d e ->
                set_data loc (Int.restrict_not_intcp d) e)
              x env
          | XPisout, [m;i] ->
            on_xid (fun locm dm e ->
                on_xid (fun loci di e ->
                    let dm, di = Int.make_is_out bool dm di in
                    set_data locm dm e
                    >! set_data loci di )
                  i e )
              m env
          | XPbittest, [x] when bool -> warn ~env "TODO: bittest"
          | XPbittest, [x] -> warn ~env "TODO: bittest"
          | XPctconst Lambda.Word_size, [] -> Envs.bottom
          | XPctconst _, [] -> Env env (* to correct ? *)
          | _, _ -> Env env
        end
      | _ -> Env env


    let rec constraint_env_cp_var cp xid env =
      on_xid (constraint_env_cp_folder cp) xid env

    and constraint_env_cp_folder cp loc d env =
      if Cps.has cp d
      then
        if Cps.is_one d
        then Env env
        else
          hfold (constraint_env_cp cp) d env
          >? set_data loc (Cps.restrict ~v:cp d)
      else Envs.bottom

    and constraint_env_cp cp expr env =
      assert(cp >= 0);
      match expr with
      | Var x -> constraint_env_cp_var cp x env
      | Prim ( p, l ) ->
        begin
          match p, l with
          | XPsetfield _, _::_::[]
          | XPsetfloatfield _, _::_::[]
            when cp = 0 -> Env env
          | XPfield i, [b] -> Env env (* cannot say a thing because of mutables *)
          | XPnot, [x] when cp < 2 ->
            constraint_env_cp_var (1-cp) x env
          | _, _ -> Envs.bottom
        end
      | _ -> Env env

    let rec constraint_env_tag_var tag xid env =
      on_xid (constraint_env_tag_folder tag) xid env

    (* constraint_env_tag_id (get_ident id env) tag env *)

    and constraint_env_tag_folder tag loc d env =
      if Blocks.has_tag tag d
      then if Blocks.is_one_tag d
        then Env env
        else 
          constraint_env_tag_exprs tag d env
          >? set_data loc (Blocks.restrict ~tag d)
      else Envs.bottom

    and constraint_env_tag_exprs tag d env =
      hfold (constraint_env_tag tag) d env

    and constraint_env_tag tag expr env =
      match expr with
      | Var x -> constraint_env_tag_var tag x env
      | Const _
      | App_prep ( _, _ )
      | Return _| Retexn _
      | Lazyforce _
      | Ccall (_, _)
      | Send (_, _) -> Env env
      | App | App_return | App_exn -> assert false
      | Alloc (_, p, l ) ->
        begin
          match p with
          | XPmakeblock (t, _) when t = tag -> Env env
          | _ -> Envs.bottom
        end
      | Prim ( p, l ) ->
        begin
          match p with
          | XPfield f -> Env env
          (* cannot say a thing, as usual *)
          | XPduprecord _ when tag = 0 -> Env env
          | _ -> Envs.bottom
        end
      | Constraint _ -> assert false

    let constr ~hedge ~s ~x ~env = function
      | Ccp cp  -> constraint_env_cp_var cp x !!env
      | Ctag tag -> constraint_env_tag_var tag x !!env
      | Cbool b -> constraint_env_bool_var b x !!env
      | Cstring _ -> failwith "TODO: string match in oldEnv"
      | Cnotstring _ -> failwith "TODO: notstring match in oldEnv"


    let allocations ~hedge ~s ~env l =
      let env, l' = List.fold_left
          (fun (env,l) (xid,allocid,a,args) ->
             let loc = Locations.of_xid xid in
             let env = set_idents xid (Locations.Locs.singleton loc) !!env in
             (env, (loc,allocid,a,args)::l))
          (env,[]) l in
      List.fold_left
        (fun env (loc,allocid,alloc,args) ->
           let e = !!env in
           let l = List.map (fun xid -> get_idents xid e) args in
           set_data loc
             (
               Exprs.set
                 begin
                   match alloc with
                   | XPfun fid -> Funs.mk fid l
                   | XPmakearray k ->
                     Arrays.singleton l ( Int_interv.cst (List.length l)) k
                   | XPmakeblock ( tag, _) ->
                     let a = Array.of_list l in
                     Blocks.singleton tag a
                 end (Alloc (allocid,alloc,args))
             ) e
        ) env l'

    let ccall ~hedge ~s ~x ~e ~env pd l =
      let open Primitive in
      assert ( pd.prim_arity = List.length l );
      Def_c_fun.get_envs pd l x env
    let lazy_force ~hedge ~s ~x ~e ~env l =
      print_endline "Unsupported Lazyforce";
      set_env x ( Exprs.set Data.top (Lazyforce l) ) !!env, Envs.bottom
    let send  ~hedge ~s ~x ~e ~env o m =
      print_endline "Unsupported object method";
      set_env x ( Exprs.set Data.top (Send (o,m)) ) !!env, Envs.bottom

    let app_prep ~hedge ~s ~x ~env ~f ~arg =
      let e = !!env in
      let env =
        set_idents fun_xid ( get_idents f e ) e
        >! set_idents arg_xid ( get_idents arg e )
      in
      Format.printf "app_pre fun_xid %a@.arg_xid %a@."
        (fun ppf id -> Print_data.print ppf id env) fun_xid
        (fun ppf id -> Print_data.print ppf id env) arg_xid;
      env

    let app_return ~hedge ~s ~x ~env =
      let e = !!env in
      set_idents x ( get_idents ret_xid e) e

    let app_exn ~hedge ~s ~x ~env =
      let e = !!env in
      set_idents x ( get_idents exn_xid e ) e

    let app ~hedge ~s ~x ~e ~env =
      let en = !!env in
      let l =
        fold_xid (fun _ d l ->
            Funs.extract_ids d l
          ) fun_xid [] en in
      List.iter (fun f -> Format.printf "outfunction %a@." Common_types.F.print f) l;
      l

    let return ~hedge ~s ~x ~env id =
      let e = !!env in
      Format.printf "ret %a@."
        (fun ppf id -> Print_data.print ppf id env) id;
      set_idents ret_xid (get_idents id e) e
    let retexn ~hedge ~s ~x ~env id =
      let e = !!env in
      set_idents exn_xid ( get_idents id e) e

  end
