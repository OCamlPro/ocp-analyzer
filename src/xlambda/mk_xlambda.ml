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
open Xlambda

module Idm = Map.Make ( Id )
module Ids = Set.Make ( Id )

let zero_of k =
  let open Asttypes in
  Const_base (
    match k with
    | Pnativeint -> Const_nativeint 0n
    | Pint32 -> Const_int32 0l
    | Pint64 -> Const_int64 0L
  )

let zeroint =  Const_base ( Asttypes.Const_int 0 )

let prim_translate = function
  (* Operations on heap blocks *)
  | Pfield i -> XPfield i
  | Psetfield ( i, b) -> XPsetfield ( i, b)
  | Pfloatfield i -> XPfloatfield i
  | Psetfloatfield i -> XPsetfloatfield i
  | Pduprecord ( t, i ) -> XPduprecord ( t, i )
  (* Boolean operations *)
  | Pnot -> XPnot
  (* Integer operations *)
  | Pnegint -> XPnegint
  | Paddint -> XPaddint
  | Psubint -> XPsubint
  | Pmulint -> XPmulint
  | Pdivint -> XPdivint
  | Pmodint -> XPmodint
  | Pandint -> XPandint
  | Porint -> XPorint
  | Pxorint -> XPxorint
  | Plslint -> XPlslint
  | Plsrint -> XPlsrint
  | Pasrint -> XPasrint
  | Pintcomp c -> XPintcomp c
  | Poffsetint i -> XPoffsetint i
  (* Float operations *)
  | Pintoffloat -> XPintoffloat
  | Pfloatofint -> XPfloatofint
  | Pnegfloat -> XPnegfloat
  | Pabsfloat -> XPabsfloat
  | Paddfloat -> XPaddfloat
  | Psubfloat -> XPsubfloat
  | Pmulfloat -> XPmulfloat
  | Pdivfloat -> XPdivfloat
  | Pfloatcomp c -> XPfloatcomp c
  (* String operations *)
  | Pstringlength -> XPstringlength
  | Pstringrefu -> XPstringrefu
  | Pstringsetu -> XPstringsetu
  (* Array operations *)
  | Parraylength k -> XParraylength k
  | Parrayrefu k -> XParrayrefu k
  | Parraysetu k -> XParraysetu k
  (* Test if the argument is a block or an immediate integer *)
  | Pisint -> XPisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout -> XPisout
  (* Bitvect operations *)
  | Pbittest -> XPbittest
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint k -> XPbintofint k
  | Pintofbint k -> XPintofbint k
  | Pcvtbint ( ksource, kdest ) -> XPcvtbint ( ksource, kdest )
  | Pnegbint k -> XPnegbint k
  | Paddbint k -> XPaddbint k
  | Psubbint k -> XPsubbint k
  | Pmulbint k -> XPmulbint k
  | Pdivbint k -> XPdivbint k
  | Pmodbint k -> XPmodbint k
  | Pandbint k -> XPandbint k
  | Porbint k -> XPorbint k
  | Pxorbint k -> XPxorbint k
  | Plslbint k -> XPlslbint k
  | Plsrbint k -> XPlsrbint k
  | Pasrbint k -> XPasrbint k
  | Pbintcomp ( k, c ) -> XPbintcomp ( k, c )
  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref ( b, i, k, l ) -> XPbigarrayref ( b, i, k, l )
  | Pbigarrayset ( b, i, k, l ) -> XPbigarrayset ( b, i, k, l )
  (* size of the nth dimension of a big array *)
  | Pbigarraydim i -> XPbigarraydim i
  (* load/set 16,32,64 bits from a string: (unsafe)*)
  | Pstring_load_16 b -> XPstring_load_16 b
  | Pstring_load_32 b -> XPstring_load_32 b
  | Pstring_load_64 b -> XPstring_load_64 b
  | Pstring_set_16 b -> XPstring_set_16 b
  | Pstring_set_32 b -> XPstring_set_32 b
  | Pstring_set_64 b -> XPstring_set_64 b
  (* load/set 16,32,64 bits from a
     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
  | Pbigstring_load_16 b -> XPbigstring_load_16 b
  | Pbigstring_load_32 b -> XPbigstring_load_32 b
  | Pbigstring_load_64 b -> XPbigstring_load_64 b
  | Pbigstring_set_16 b -> XPbigstring_set_16 b
  | Pbigstring_set_32 b -> XPbigstring_set_32 b
  | Pbigstring_set_64 b -> XPbigstring_set_64 b
  (* Compile time constants *)
  | Pctconst c -> XPctconst c
  (* byte swap *)
  | Pbswap16 -> XPbswap16
  | Pbbswap k -> XPbbswap k
  | Pint_as_pointer -> XPint_as_pointer
  | Pidentity | Pignore | Prevapply _ | Pdirapply _ | Pgetglobal _ | Psetglobal _
  | Pmakeblock _ | Plazyforce | Praise _ | Psequand | Psequor | Pmakearray _
  | Pstringrefs | Pstringsets | Parrayrefs _ | Pccall _ | Parraysets _
  | Poffsetref _ | Ploc _
    -> assert false

let alloc_translate = function
  | Pmakeblock ( i, m) -> XPmakeblock ( i, m)
  | Pmakearray k -> XPmakearray k
  | _ -> assert false


let rec var_name_of_lambda = function
  | Lvar v -> ( match Id.name v with Some n -> n | None -> "_var_" )
  | Llet _ | Lletrec _ -> "_var_"
  | Lassign _ | Lifused _ -> assert false
  | Levent (lam,_) -> var_name_of_lambda lam
  | Lsequence _ -> "_seq_"
  | Lconst _ -> "_const_"
  | Lapply _ -> "_app_"
  | Lfunction _ -> "_funct_"
  | Lprim _ -> "_primitive_"
  | Lswitch _ -> "_match_"
  | Lstringswitch _ -> "_match_string_"
  | Lstaticcatch _ -> "_scatch_"
  | Ltrywith _ -> "_trywith_"
  | Lifthenelse _ -> "_if_"
  | Lsend _ -> "_send_"
  | Lstaticraise _ | Lwhile _ | Lfor _ -> "()"
  
let constant_has_to_be_pushed_up c =
  let open Asttypes in
  match c with
  | Const_base (Const_string _) -> true
  | Const_base
      ( Const_int _ 
      | Const_char _  
      | Const_float _  
      | Const_int32 _  
      | Const_int64 _  
      | Const_nativeint _ ) -> false
  | Const_pointer _ -> false
  | Const_block (_,_) | Const_float_array _ | Const_immstring _ -> true


let cp i = Lconst ( Const_pointer i )

let lvars = List.map (fun v -> Lvar v)

type trans_info =
  {
    push_up: ((Id.t * Location.t option) * xcontrol) list;
    nfv: Ids.t;
    fv: Id.t Idm.t;
    floc: Location.t option;
  }

let add_nf i info =
  {info with nfv = Ids.add i info.nfv}

let lambda_to_xlambda ~modname ~funs code =

  let mk ?(name="$$") () = Id.create ~name () in

  let xid i : xid = modname,i in

  let xlet ?(k = Strict) ~id xe_lam xe_in xe_floc xe_loc =
    Xlet { xe_kind = k; xe_id = xid id; xe_lam; xe_in; xe_floc; xe_loc; }
  in

  let register_function ~important ~debug_info tlam arg fv =
    let i = F.create_function ~name:modname ~important ~debug_info in
    let idf = xid ( mk ~name:"func_fv" ()) in
    let targ = xid arg in
    let tlam, _ =
      ( tlam, 0 )
      |> Idm.fold (fun _ id (tlam,n) ->
          xlet ~k:Alias ~id ( Xprim ( XPfunfield n, [idf] ) ) tlam None None, succ n
        ) fv 
    in
    Hashtbl.add funs i (
      ( Xlet
          {
            xe_kind = Alias;
            xe_id = idf;
            xe_lam = (Xprim ( XPgetfun i, [] ));
            xe_in = 
              Xlet { xe_kind = Alias; xe_id = targ; 
                     xe_lam = (Xprim ( XPgetarg, [] ));
                     xe_in = tlam;
                     xe_floc = None; xe_loc = None;
                   };
            xe_floc = None;
            xe_loc = None;
          }
      ) );
    i
  in

  let lraise_glob x l =
    Lprim (
      Praise Raise_notrace,
      [Lprim 
         ( Pmakeblock (0,Asttypes.Immutable),
           (Lprim (Pgetglobal x, []))::l )]
    )
  in
  let ldiv_by_zero =
    lraise_glob
      Ident.({ name = "Division_by_zero"; stamp = 23; flags = 0; })
      []
  in
  let linvalid_arg =
    lraise_glob
      Ident.({ name = "Invalid_argument"; stamp = 18; flags = 0; })
      [ Lconst (Const_base (Asttypes.Const_string ("index out of bounds",None))) ]
  in
  let lout_of_bounds = linvalid_arg in

  let rec xlambda ?location info = function
    (* nfv and fv are respectively non-free and free variables *)
    (* nfv is a set *)
    (* fv is a map from the variable name in the lambda to the local variable name in the xlambda
       because we want no function to have free variable, there will be a constant binding in
       the function declaration.
    *)
    | Lvar v ->
      let fv,v = check info v in
      ( info.push_up, fv , Xend (xid v) )
    | Lconst _
    | Lapply _
    | Lfunction _
    | Lprim _
    | Lswitch _
    | Lstringswitch _
    | Lstaticraise _
    | Lstaticcatch _
    | Ltrywith _
    | Lifthenelse _
    | Lwhile _
    | Lfor _
    | Lsend _
      as lam ->
      let id = mk ~name:(var_name_of_lambda lam) () in
      xcontrol (add_nf id info) [(id, location), Lvar id] lam
    | Llet ( k, id , e, cont ) ->
      xcontrol (add_nf id info) [(id, None), cont] e
    | Lletrec (l, continuation) -> xrec_main info [] l continuation
    | Lsequence ( a, b ) ->
      let id = mk ~name:"()" () in
      xcontrol (add_nf id info) [(id, None), b] a
    | Levent (lam,ev) ->
      begin
        match ev.lev_kind with
        | Lev_function ->
          xlambda {info with floc = Some ev.lev_loc} lam
        | Lev_before | Lev_after _ ->
          xlambda ~location:ev.lev_loc info lam
      end
    | Lassign _
    | Lifused _ -> assert false

  and xrec_main info stack l continuation =
    let vars, promoted, expelled =
      List.fold_left
        (fun (vars,promoted, expelled) (id, lam) -> promote_rec vars promoted expelled id lam )
        ([],[],[]) l
    in
    if expelled = []
    then
      let nfv =
        List.fold_left
          (fun nfv (i,_) -> Ids.add i nfv)
          info.nfv promoted
      in
      let push_up, fv, xr_decls = mk_xletrec {info with nfv} [] promoted in
      let info = {info with push_up; fv; nfv} in
      let push_up, fv, xr_in =
        if stack = []
        then xlambda info continuation
        else xcontrol info stack continuation
      in
      ( push_up, fv, ( Xrec { xr_decls; xr_in } ) )
    else
      let push_up, stack, cont =
        List.fold_left
          (fun (push_up,stack,cont) (i,lam) ->
             match lam with
             | Lconst c -> (add_constant (i, None) c push_up), stack, cont
             | _ ->  (push_up,((i,None),cont)::stack,lam))
          ( info.push_up, stack, Lletrec ( promoted, continuation ) ) expelled
      in
      xcontrol {info with push_up} stack cont


  and xcontrol info stack = function
    (* The stack argument keeps the list of let-bindings to put on top of the currently analysed expression *)
    | Lvar v ->
      let fv,v = check info v in
      mk_xlet {info with fv} stack ( Xvar (xid v) )
    | Lconst c ->
      if constant_has_to_be_pushed_up c
      then handle_constant info c stack
      else mk_xlet info stack (Xconst c)
    | Lapply ( Lvar f, [Lvar x], _ ) ->
      let fv, f = check info f in
      let fv, x = check {info with fv} x in
      mk_xlet {info with fv} stack ( Xapply ( xid f, xid x ) )
    | Lapply ( Lvar _ as f, [x], loc ) ->
      let ix = mk ~name:"_arg_" () in
      xcontrol (add_nf ix info)
        ( ((ix, None), Lapply ( f, [Lvar ix], loc ))::stack)
        x
    | Lapply ( f, ( _::[] as arg), loc ) ->
      let idf = mk ~name:"_fun_" () in
      xcontrol (add_nf idf info)
        ( ((idf, None), Lapply ( Lvar idf, arg, loc ) )::stack)
        f
    | Lapply ( f, arg::args, loc ) ->
      xcontrol info stack
        ( Lapply ( Lapply ( f, [arg], loc ), args, loc ))
    | Lfunction ( _, [arg], body ) ->
      let push_up, fv, f, l = fun_create info arg body in
      mk_xlet {info with push_up; fv} stack ( Xalloc ( f, List.map xid l ) )
    | Lfunction ( k, arg::args, body ) ->
      let body2 = Lfunction (k,args,body) in
      let push_up, fv, f, l = fun_create info arg body2 in
      mk_xlet {info with push_up; fv} stack ( Xalloc ( f, List.map xid l ) )
    | Llet ( k, id, e, cont ) ->
      xcontrol (add_nf id info) (((id, None),cont)::stack) e
    | Lletrec ( l, continuation ) -> xrec_main info stack l continuation
    | Lprim ( Psequand, [a;b] ) -> (* Psequand and Psequor are handled here as they can directly be changed into a if clause *)
      xcontrol info stack ( Lifthenelse ( a, b, cp 0 ) )
    | Lprim ( Psequor, [a;b] ) ->
      xcontrol info stack ( Lifthenelse ( a, cp 1, b ) )
    | Lprim ( p, l ) ->
      extract_and_apply info stack
        (fun l -> Lprim ( p, l ) )
        ( prim_handle info stack p )
        l
    | Lswitch ( Lvar v, s ) ->
      let fv, v = check info v in
      let f push_up fv =
        List.fold_left (fun (push_up,fv,l) (i,lam) ->
            let push_up, fv, tlam = xlambda {info with push_up; fv} lam in
            (push_up, fv, (i,tlam)::l) ) (info.push_up,fv,[]) in
      let push_up, fv, x_consts = f info.push_up fv s.sw_consts in
      let push_up, fv, x_blocks = f push_up fv s.sw_blocks in
      let push_up, fv, x_failaction =
        match s.sw_failaction with
        | Some lam ->
          let push_up, fv, tlam = xlambda {info with push_up; fv} lam in
          push_up, fv, Some tlam
        | None -> push_up, fv, None
      in
      mk_xlet {info with push_up; fv} stack
        ( Xswitch
            ( xid v,
              {
                x_numconsts = s.sw_numconsts;
                x_numblocks = s.sw_numblocks;
                x_consts; x_blocks; x_failaction;
              }
            )
        )
    | Lswitch ( lam, s ) ->
      let i = mk ~name:"_tomatch_" () in
      xcontrol info ( ((i, None), Lswitch ( Lvar i, s))::stack ) lam
    | Lstringswitch (Lvar v, l, lamfail) ->
      let fv,v = check info v in
      let push_up,fv,l' = List.fold_left (fun (push_up,fv,l') (s,lam) ->
          let push_up, fv, xlam = xlambda {info with push_up; fv;} lam in
          (push_up, fv, (s,xlam)::l') )
          (info.push_up, fv, []) l
      in
      let push_up, fv, xlamfail =
        match lamfail with
        | None -> push_up, fv, None
        | Some lam ->
          let push_up, fv, xlam = xlambda {info with push_up; fv} lam in
          push_up, fv, Some xlam
      in
      mk_xlet {info with push_up; fv;} stack
        ( Xstringswitch (xid v, l', xlamfail) )
    | Lstringswitch (lam, l, lamfail) ->
      let i = mk ~name:"_stringtomatch_" () in
      xcontrol info ( ((i, None), Lstringswitch ( Lvar i, l,lamfail))::stack ) lam      
    | Lstaticraise (i, l) ->
      extract_and_apply info stack
        (fun l -> Lstaticraise ( i, l ) )
        (fun l ->
           let fv, l = lcheck info l in
           mk_xlet {info with fv} stack ( Xstaticraise ( i, List.map xid l ))
        )
        l
    | Lstaticcatch ( lam, args, lam2 ) ->
      let push_up, fv, tlam = xlambda info lam in
      let nfv =
        List.fold_left
          (fun nfv v -> Ids.add v nfv)
          info.nfv (snd args) in
      let push_up, fv, tlam2 = xlambda {info with nfv} lam2 in
      mk_xlet {info with push_up; nfv; fv} stack
        ( Xstaticcatch ( tlam, (fst args, List.map xid (snd args)), tlam2 ) )
    | Ltrywith ( lam, i, lam2 ) ->
      let push_up, fv, tlam = xlambda info lam in
      let nfv = Ids.add i info.nfv in
      let push_up, fv, tlam2 = xlambda {info with push_up; nfv; fv} lam2 in
      mk_xlet {info with push_up; nfv; fv} stack ( Xtrywith ( tlam, xid i, tlam2 ) )
    | Lifthenelse ( Lvar v, t, e ) ->
      let fv, v = check info v in
      let push_up, fv, t = xlambda {info with fv} t in
      let push_up, fv, e = xlambda {info with push_up; fv} e in
      mk_xlet {info with push_up; fv} stack ( Xifthenelse ( xid v, t, e) )
    | Lifthenelse ( c, t, e ) ->
      let i = mk ~name:"_ifcond_"() in
      xcontrol (add_nf i info)
        (( (i, None), Lifthenelse ( Lvar i, t, e ) )
         ::stack )
        c
    | Lsequence ( a, b ) ->
      let i = mk ~name:"()" () in (* we don't need to make it a nfv as it won't be used *)
      xcontrol (add_nf i info) (((i, None),b)::stack) a
    | Lwhile ( c, b ) ->
      let push_up, fv, c = xlambda info c in
      let push_up, fv, b = xlambda {info with push_up; fv} b in
      mk_xlet {info with push_up; fv} stack ( Xwhile ( c, b ) )
    | Lfor ( i, Lvar s, Lvar e, d, b ) ->
      let fv, s = check info s in
      let fv, e = check {info with fv} e in
      let info = add_nf i {info with fv} in
      let push_up, fv, b = xlambda info b in
      mk_xlet {info with push_up; fv} stack
        ( Xfor ( xid i, xid s, xid e, d, b ) )
    | Lfor ( i, s, e, d, b ) ->
      let is = mk ~name:"_start_" () in
      let ie = mk ~name:"_stop_" () in
      xcontrol (add_nf is (add_nf ie info))
        (((is, None),e)
         ::((ie, None), Lfor ( i, Lvar is, Lvar ie, d, b ) )
         ::stack)
        s
    | Lsend ( k, Lvar o, Lvar m, [], _ ) ->
      let fv, o = check info o in
      let fv, m = check {info with fv} m in
      mk_xlet {info with fv} stack
        ( Xsend ( k, xid o, xid m ) )
    | Lsend ( k, ( Lvar _ as o ), m, [], loc ) ->
      let im = mk ~name:"_meth_" () in
      xcontrol info
        ( ((im, None), Lsend ( k, o, Lvar im, [], loc))
          ::stack )
        m
    | Lsend ( k, o, (Lvar _ as m), [], loc ) ->
      let io = mk ~name:"_obj_" () in
      xcontrol info
        ( ((io, None), Lsend ( k, Lvar io, m, [], loc))
          ::stack )
        o
    | Lsend ( k, o,  m, [], loc ) ->
      let im = mk ~name:"_meth_" () in
      let io = mk ~name:"_obj_" () in
      xcontrol info
        ( ((io, None), m)
          ::( (im, None), Lsend ( k, Lvar io, Lvar im, [], loc ) )
          ::stack )
        o
    | Lsend ( k, o, m, args, loc ) ->
      xcontrol info stack
        ( Lapply
            ( Lsend ( k, o, m, [], loc )
            , args, loc )
        )
    | Levent (lam, ev) ->
      begin
        match ev.lev_kind with
        | Lev_function ->
          xcontrol {info with floc = Some ev.lev_loc} stack lam
        | Lev_before | Lev_after _ ->
          begin
            match stack with
            | [] -> assert false
            | ((id, None), cont) :: tl ->
              xcontrol info (((id, Some ev.lev_loc), cont)::tl) lam
            | _ -> xcontrol info stack lam
          end
      end
    | Lfunction (_,[],_)
    | Lapply (_,[],_) -> assert false (* Not possible *)
    | Lassign (_,_)
    | Lifused (_,_) -> assert false (* Not supported *)


  and mk_xlet info stack tc =
    match stack with
    | [ ( id, loc_opt ), cont ] ->
      let push_up, fv, cont = xlambda (add_nf id info) cont in
      push_up, fv, xlet ~id tc cont info.floc loc_opt
    | ((id, loc_opt), cont) :: stack ->
      let push_up, fv, lam = xcontrol (add_nf id info) stack cont in
      push_up, fv, xlet ~id tc lam info.floc loc_opt
    | [] -> assert false

  and prim_handle info stack p l =
    match p, l with (* Special handling for some primitives *)
    | Pdivint, [a;b]
    | Pmodint, [a;b] ->
      let lb = Lvar b in
      xcontrol info stack
        ( Lifthenelse (
             Lprim ( Pintcomp Cneq, [lb; Lconst zeroint] ),
             Lprim ( p, [Lvar a; lb; lb]),
             ldiv_by_zero )
        )
    | Pstringrefs, [a;b] ->
      let la = Lvar a in
      let lb = Lvar b in
      xcontrol info stack
        ( Lifthenelse (
             Lprim ( Pintcomp Clt , [lb; Lprim ( Pstringlength, [la])] ),
             Lprim ( Pstringrefu, [la; lb]),
             linvalid_arg )
        )
    | Pstringsets, [a;b;c] ->
      let la = Lvar a in
      let lb = Lvar b in
      xcontrol info stack
        ( Lifthenelse (
             Lprim ( Pintcomp Clt , [lb; Lprim ( Pstringlength, [la])] ),
             Lprim ( Pstringsetu, [la; lb; Lvar c]),
             linvalid_arg )
        )
    | Parrayrefs k, [a;b] ->
      let la = Lvar a in
      let lb = Lvar b in
      xcontrol info stack
        ( Lifthenelse (
             Lprim ( Pintcomp Clt , [lb; Lprim ( Parraylength k, [la])] ),
             Lprim ( Parrayrefu k, [la; lb]),
             lout_of_bounds )
        )
    | Parraysets k, [a;b;c] ->
      let la = Lvar a in
      let lb = Lvar b in
      xcontrol info stack
        ( Lifthenelse (
             Lprim ( Pintcomp Clt , [lb; Lprim ( Parraylength k, [la])] ),
             Lprim ( Parraysetu k, [la; lb; Lvar c]),
             lout_of_bounds )
        )
    | Pdivbint k, [a;b]
    | Pmodbint k, [a;b] ->
      let lb = Lvar b in
      xcontrol info stack
        ( Lifthenelse (
             Lprim ( Pintcomp Cneq, [lb; Lconst (zero_of k)] ),
             Lprim ( p, [Lvar a; lb; lb]),
             (* the presence of a third argument only indicates that the div_by_zero check has been added to the lambda *)
             ldiv_by_zero )
        )
    | Poffsetref i, [a] ->
      let la = Lvar a in
      xcontrol info stack @@
      Lprim (Psetfield (0,true), [la; Lprim (Poffsetint i, [Lprim (Pfield 0, [la])])])
    | _,_ ->
      begin
        let fv, l = lcheck info l in
        let info = {info with fv} in
        let tl = mk_xlet info stack in
        match p, l with
        | Pidentity, [a] -> tl ( Xvar (xid a) )
        | Pignore, [_] -> tl ( Xconst ( Const_pointer 0 ) ) (* const propagation ? *)
        | Prevapply loc, x::f::tl
        | Pdirapply loc, f::x::tl ->
          xcontrol info stack
            ( Lapply ( Lvar f, lvars (x::tl), loc ) )
        | Pgetglobal i, [] ->
          if builtin i
          then tl ( Xprim ( XPbuiltin, [xid i] ) )
          else
            let fv, i = check info i in
            mk_xlet {info with fv} stack ( Xvar ( xid i ) )
        | Psetglobal ig, [ir] ->
          let push_up, fv, cont =
            xcontrol info stack
              ( Lconst ( Const_pointer 0))
          in
          push_up, fv, Xlet {
            xe_id = ("",ig);
            xe_lam = Xvar (xid ir);
            xe_kind = Alias;
            xe_in = cont;
            xe_floc = info.floc;
            xe_loc = None;
          }
        | Plazyforce, [a] ->
          tl ( Xlazyforce ( xid a ) )
        | Praise k, [e] ->
          tl ( Xraise (k,xid e) )
        | Pccall c, _ -> tl ( Xccall ( c, List.map xid l ) )
        | Pdivint, [a;b;_] -> (* yup, that's a hack *)
          tl ( Xprim ( XPdivint, [xid a;xid b]))
        | Pmodint, [a;b;_] ->
          tl ( Xprim ( XPmodint, [xid a;xid b]))
        | Pdivbint k, [a;b;_] -> (* yup, that's a hack *)
          tl ( Xprim ( XPdivbint k, [xid a;xid b]))
        | Pmodbint k, [a;b;_] ->
          tl ( Xprim ( XPmodbint k, [xid a;xid b]))

        (* Generic handlers for primitives *)
        | Pmakearray _, _ | Pmakeblock _, _ ->
          tl ( Xalloc ( alloc_translate p, List.map xid l) )
        | _, _ ->
          tl ( Xprim ( prim_translate p, List.map xid l ) )
      end

  and promote_rec vars promoted expelled i lam =
    (* Making sure only mandatory variables are in a recursive let rec *)
    let p_l vars promoted expelled =
      List.fold_left
        (fun (v,p,e) (id,lam) -> promote_rec v p e id lam )
        ( vars, promoted, expelled )
    in
    match lam with
    | Lfunction ( _, _::[], _ ) ->
      ( vars, ( i, lam ) :: promoted, expelled )
    | Lfunction ( k, x::tl, body ) ->
      ( x :: vars, 
        ( i, Lfunction ( k, [x],
                         Lfunction ( k, tl, body ) )
        ) :: promoted,
        expelled )
    | Lprim ( Pmakeblock _ as p, l )
    | Lprim ( Pmakearray _ as p, l ) ->
      let ( lams, l ) = extract_lams [] [] l in
      let vars, promoted, expelled =
        p_l vars promoted expelled lams in
      ( vars, ( i, Lprim ( p, l ) ) :: promoted, expelled )
    | Llet ( k, id, e, cont ) ->
      let ( vars, promoted, expelled ) =
        promote_rec vars promoted expelled id e in
      promote_rec vars promoted expelled i cont
    | Lletrec ( l, cont ) ->
      let ( vars, promoted, expelled ) =
        p_l vars promoted expelled l in
      promote_rec vars promoted expelled i cont
    | _ -> ( vars, promoted, ( i, lam ) :: expelled )

  and mk_xletrec info res l =
    match l with
    | [] -> info.push_up, info.fv, res
    | (i,lam) :: tl ->
      let push_up, fv, x =
        begin
          match lam with
          | Lprim ( p, l ) ->
            let fv, l = lcheck info ( get_vars l) in
            let p = alloc_translate p in
            info.push_up, fv, ( xid i, p, List.map xid l )
          | Lfunction ( _, [arg], body ) ->
            let push_up, fv, f, l = fun_create info arg body in
            push_up, fv, ( xid i, f, List.map xid l)
          | _ -> assert false
        end in
      mk_xletrec {info with push_up; fv} (x::res) tl

  and extract_vars l =
    let b, l =
      List.fold_left
        ( fun (b,l) lam ->
           match lam with
           | Lvar v -> (b,v::l)
           | _ ->
             let i = mk ~name:(var_name_of_lambda lam) () in
             (false,i::l)
        ) (true,[]) l
    in
    ( b, (List.rev l) )

  and extract_lams res l = function
    | [] -> res, List.rev l
    | ( Lvar _ as lam ) :: tl -> extract_lams res (lam::l) tl
    | lam :: tl ->
      let i = mk ~name:(var_name_of_lambda lam) () in
      extract_lams ((i,lam)::res) ((Lvar i)::l) tl

  and extract_and_apply info stack mkl mkt l =
    let ok, lv = extract_vars l in
    if ok
    then mkt lv
    else
      let push_up, stack, nfv, cont, _ =
        List.fold_left
          (fun (push_up, stack,nfv,cont,lv) lam ->
             match lam,lv with
             | Lvar v, x::tl ->
               assert (x = v);
               ( push_up, stack, nfv, cont, tl )
             | Lconst c, x::tl ->
               (add_constant (x, None) c push_up, stack, nfv, cont, tl)
             | _, i::tl ->
               ( push_up, ((i,None),cont)::stack,
                 ( Ids.add i nfv ),
                 lam, tl )
             | _,[] -> assert false
          )
          ( info.push_up, stack, info.nfv, ( mkl ( lvars lv ) ), lv ) l
      in
      xcontrol {info with push_up; nfv} stack cont

  and check info v =
    (* verify the freeness of a variable, returns a fresh variable
       (making sure no same variable is defined in two different places) *)
    if Ids.mem v info.nfv
    then info.fv,v
    else
      try
        info.fv, Idm.find v info.fv
      with
        Not_found ->
        let i = match Id.name v with
          | None -> mk ()
          | Some name -> mk ~name ()
        in
        Idm.add v i info.fv, i

  and lcheck info l =
    let fv, l =
      List.fold_left
        (fun (fv,l) i ->
           let fv,i = check {info with fv} i in
           (fv,i::l)
        ) (info.fv,[]) l
    in fv, List.rev l

  and get_vars = List.map (function Lvar v -> v | _ -> assert false )

  and fun_create info arg body =
    let nfv = Ids.add arg info.nfv in
    let nfv2 = Ids.singleton arg in
    let fv2 = Idm.empty in
    let push_up, fv2, lam = xlambda {info with nfv = nfv2; fv = fv2} body in
    let important, debug_info =
      match body with
      | Lfunction (_,_,_) -> false, None
      | Levent (_,{ lev_loc; lev_kind = Lev_function; _ }) ->
        true, Some lev_loc
      | _ -> true, None
    in
    let i = register_function ~important ~debug_info lam arg fv2 in
    let fv, l =
      Idm.fold
        (fun i _ (fv,l) ->
           let fv,i = check {info with nfv; fv} i in
           (fv,i::l)
        )
        fv2 (info.fv,[])
    in
    let l = List.rev l in
    ( push_up, fv, XPfun i, l )

  and add_constant id_loc c push_up =
    match c with
    | Const_base _
    | Const_pointer _
    | Const_immstring _  ->
      (id_loc,Xconst c)::push_up
    | Const_block (tag,l) ->
      let ll,push_up = List.fold_right
          (fun c (ll,push_up) ->
             let id = mk ~name:"__in_constant_block__" () in
             let push_up = add_constant (id, None) c push_up in
             (xid id)::ll, push_up
          )
          l ([],push_up)
      in
      ( id_loc, Xalloc (XPmakeblock (tag,Asttypes.Immutable), ll) ) :: push_up
    | Const_float_array l ->
      let ll,push_up = List.fold_right
          (fun s (ll,push_up) ->
             let id = mk ~name:"__in_constant_floatarray__" () in
             let push_up = add_constant (id, None) (Const_base (Asttypes.Const_float s)) push_up in
             (xid id)::ll, push_up
          )
          l ([],push_up)
      in
      ( id_loc, Xalloc ((XPmakearray Pfloatarray), ll) ) :: push_up

  and handle_constant info c = function
    | [ ( id, loc ), cont ] ->
      let push_up = add_constant (id, loc) c info.push_up in
      let nfv = Ids.remove id info.nfv in
      (* let fv, id = check info id in *)
      xlambda {info with push_up; nfv} cont
    | ((id,loc),cont) :: stack ->
      (* let fv, id = check info id in *)
      let push_up = add_constant (id,loc) c info.push_up in
      let nfv = Ids.remove id info.nfv in
      xcontrol {info with push_up; nfv} stack cont
    | [] -> assert false
  in

  let info =
    { push_up = [];
      nfv = Ids.empty;
      fv = Idm.empty;
      floc = None;
    }
  in
  let push_up, fv, lam =  xlambda info code in
  let fv, lam = List.fold_left (fun (fv,lam) ((id,loc),v) ->
      (* Format.eprintf "Pushed up: %a = %a@." Ident.print id Print_xlambda.xcontrol v; *)
      try Idm.remove id fv, xlet ~id:(Idm.find id fv) v lam None loc
      with Not_found ->
        fv, xlet ~id v lam None loc
    ) (fv,lam) push_up
  in
  let lam =
    Idm.fold (fun gid id lam ->
        Format.fprintf Format.std_formatter "Free variable in %s : %a -> %a@." modname Ident.print gid Ident.print id;
        (* assert ( gid.Ident.stamp = 0 ); *)
        xlet ~k:Alias ~id ( Xvar ( "",gid ) ) lam None None
      )
      fv lam
  in
  lam
