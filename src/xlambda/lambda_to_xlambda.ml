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

module Imap = Map.Make ( Id )
module Iset = Set.Make ( Id )

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
  | Pmakeblock ( i, m) -> XPmakeblock ( i, m)
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
  | Poffsetref i -> XPoffsetref i
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
  | Pstringrefs -> XPstringrefs
  | Pstringsets -> XPstringsets
  (* Array operations *)
  | Pmakearray k -> XPmakearray k
  | Parraylength k -> XParraylength k
  | Parrayrefu k -> XParrayrefu k
  | Parraysetu k -> XParraysetu k
  | Parrayrefs k -> XParrayrefs k
  | Parraysets k -> XParraysets k
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
  | _ -> assert false

let lambda_to_xlambda last_id code =

  let funs : ( F.t, xlambda) Hashtbl.t
    = Hashtbl.create 100
  in
  let register_function body =
    let f = F.create () in
    Hashtbl.add funs f body;
    f
  in
  let ir = ref last_id in
  let xlet ?(kind = Strict) id body cont =
    Xlet {
      xe_id = id;
      xe_lam = body;
      xe_kind = kind;
      xe_in = cont;
    }
  in
  let mk () =
    incr ir;
    Ident.({ name = ""; stamp = !ir; flags = 0 })
  in
  let remk i =
    incr ir;
    Ident.( { i with stamp = !ir; } )
  in

  let rec map_idents f = function
    | Xlet l ->
      let xe_lam = map_idents_control f l.xe_lam in
      let xe_in = map_idents f l.xe_in in
      Xlet { l with xe_lam; xe_in; }
    | Xrec r ->
      let xr_decls = List.rev_map
          (fun (i,p,ids) -> ( i, p, List.map f ids))
          r.xr_decls in
      let xr_in = map_idents f r.xr_in in
      Xrec { xr_decls; xr_in; }
    | Xend i -> Xend ( f i)

  and map_idents_control f = function
    | Xvar v -> Xvar ( f v )
    | Xapply ( i1, i2 ) -> Xapply ( f i1, f i2)
    | Xprim ( p, l ) -> Xprim ( p, List.map f l)
    | Xswitch ( i, s) ->
      let smap (a,lam) = ( a, map_idents f lam ) in
      let omap o = match o with
        | None -> o
        | Some lam -> Some ( map_idents f lam )
      in
      Xswitch
        ( f i,
          { s with
            x_consts = List.rev_map smap s.x_consts;
            x_blocks = List.rev_map smap s.x_blocks;
            x_failaction = omap s.x_failaction;
          } )
    | Xstaticraise ( i, ids) -> Xstaticraise ( i, List.map f ids )
    | Xstaticcatch ( lam, ( i, l), lam2 ) ->
      Xstaticcatch ( map_idents f lam,
                     ( i, List.map f l),
                     map_idents f lam2 )
    | Xraise i -> Xraise ( f i)
    | Xtrywith ( lam, i, lam2 ) ->
      Xtrywith ( map_idents f lam,
                 f i,
                 map_idents f lam2 )
    | Xifthenelse ( i, lam, lam2 ) ->
      Xifthenelse ( f i, map_idents f lam, map_idents f lam2 )
    | Xwhile ( c, b) ->
      Xwhile ( map_idents f c, map_idents f b)
    | Xfor ( i, ib, ie, d, b ) ->
      Xfor ( f i, f ib, f ie, d, map_idents f b )
    | c -> c
  in

  let free_vars nfv =
    let check nfv fv i =
      if Iset.mem i nfv
      then fv
      else Iset.add i fv
    in
    let rec aux nfv fv = function
      | Xlet { xe_id; xe_lam; xe_in; _ } ->
        let fv = auxc nfv fv xe_lam in
        aux fv (Iset.add xe_id nfv) xe_in
      | Xrec { xr_decls; xr_in } ->
        let fv, nfv = List.fold_left
            (fun (fv, nfv) (v,p,l) ->
               let fv = Iset.remove v fv
               and nfv = Iset.add v nfv in
               ( List.fold_left (check nfv) fv l, nfv)
            ) (fv, nfv) xr_decls in
        aux nfv fv xr_in
      | Xend i -> check nfv fv i
    and auxc nfv fv = function
      | Xvar v -> check nfv fv v
      | Xconst _ -> fv
      | Xapply ( f, x ) -> check nfv (check nfv fv f) x
      | Xprim ( _, l) -> List.fold_left ( check nfv) fv l
      | Xswitch ( i, s) ->
        let fv = check nfv fv i in
        let fold = List.fold_left
            (fun fv (_,lam) -> aux nfv fv lam) in
        let fv = fold (fold fv s.x_consts) s.x_blocks in
        begin
          match s.x_failaction with
          | None -> fv
          | Some l -> aux nfv fv l
        end
      | Xstaticraise ( _, l ) -> List.fold_left (check nfv) fv l
      | Xstaticcatch ( lt, ( _, l), lc) ->
        let fv = aux nfv fv lt in
        let nfv = List.fold_left
            (fun nfv v -> Iset.add v nfv) nfv l in
        aux nfv fv lc
      | Xraise i -> check nfv fv i
      | Xtrywith ( lt, i, lw ) ->
        let fv = aux nfv fv lt in
        let nfv = Iset.add i nfv in
        aux nfv fv lw
      | Xifthenelse ( i, t, e) ->
        let fv = check nfv fv i in
        let fv = aux nfv fv t in
        aux nfv fv e
      | Xwhile ( c, b) ->
        let fv = aux nfv fv c in
        aux nfv fv b
      | Xfor ( i, b, e, _, body) ->
        let nfv = Iset.add i nfv in
        let fv = check nfv fv b in
        let fv = check nfv fv e in
        aux nfv fv body
      | Xlazyforce id -> check nfv fv id
      | Xccall ( _, l ) -> List.fold_left (check nfv) fv l
      | Xsend ( _, o, m) -> check nfv ( check nfv fv o ) m
    in aux nfv Iset.empty
  in

  (* adding lets *)
  (* puts an identifier on every expression *)

  let rec add_let = function
    | Llet ( k, i, e, cont ) -> Llet ( k, i, add_in_let e, add_let cont )
    | Llexrec ( decls, cont ) ->
      Llexrec ( List.rev_map (fun (i,lam) -> (i, add_in_let lam) ) decls, add_let cont )
    | Lvar _ as lam -> lam
    | lam ->
      let i = mk () in
      Llet ( Strict, i, add_in_let lam, Lvar i )
  and add_in_let = function
    | Llet _
    | Llexrec _ as lam -> add_let lam
    | Lvar _
    | Lconst _ as lam -> lam
    | Lapply ( lam, args, loc ) -> Lapply ( add_let lam, List.map add_let args, loc )
    | Lfunction ( k, args, body ) -> Lfunction ( k, args, add_let body )
    | Lprim ( Psequand, [a;b] ) -> Lifthenelse ( add_let a, add_let b, add_let ( Lconst ( Const_pointer 0 )) )
    | Lprim ( Psequor, [a;b] ) -> Lifthenelse ( add_let a, add_let ( Lconst ( Const_pointer 0 )), add_let b )
    | Lprim ( p, l ) -> Lprim ( p, List.map add_let l )
    | Lswitch ( i, s ) ->
      let aux = List.rev_map (fun (i,lam) -> (i, add_let lam) ) in
      let sw_consts = aux s.sw_consts in
      let sw_blocks = aux s.sw_blocks in
      let sw_failaction =
        match s.sw_failaction with
          None -> s.sw_failaction
        | Some l -> Some ( add_let l )
      in
      Lswitch ( add_let i, { s with sw_consts; sw_blocks; sw_failaction; } )
    | Lstaticraise ( i, l) -> Lstaticraise ( i, List.map add_let l )
    | Lstaticcatch ( lt, c, lw ) -> Lstaticcatch ( add_let lt, c, add_let lw )
    | Ltrywith ( lt, c, lw ) -> Ltrywith ( add_let lt, c, add_let lw )
    | Lifthenelse ( i, t, e ) -> Lifthenelse ( add_let i, add_let t, add_let e )
    | Lsequence ( a, b ) ->
      let i = mk () in
      Llet ( Strict, i, a, add_in_let b)
    | Lwhile ( c, b ) -> Lwhile ( add_let c, add_let b )
    | Lfor ( i, start, stop, d, body ) ->
      Lfor ( i, add_let start, add_let stop, d, add_let body )
    | Lassign _ -> assert false
    | Lsend ( k, obj, meth, args, loc ) -> Lsend ( k, add_let obj, add_let meth, List.map add_let args, loc )
    | Levent ( l, _ ) -> add_in_let l
    | Lifused _ -> assert false

  in

  (* end adding lets *)

  (* organizing lets *)
  (* assumes every expression is either a let, a lexrec, or a var *)

  let llet k i lam cont = Llet ( k, i, lam, cont ) in

  let rec get_nonvars to_add vars = function
    | [] -> to_add, vars
    | ( Lvar _ ) as v :: tl -> get_nonvars to_add ( v :: vars ) tl
    | ( Llet ( k, i, e, cont )) :: tl -> get_nonvars ( ( k, i, e ) :: to_add ) ( cont :: vars ) tl
    | _ -> assert false
  in

  let rec to_add_to_lets cont = function
    | [] -> cont
    | ( k, i, e ) :: tl -> llet k i e ( to_add_to_lets cont tl )
  in

  let rec organize_list k i lam cont mklam l =
    let to_add, vars = get_nonvars [] [] l in
    match to_add with
    | [] -> llet k i lam ( organize_let cont )
    | _ ->
      organize_let
        ( to_add_to_lets
            ( llet k i ( mklam ( List.rev vars ) ) cont )
            to_add
        )

  and organize_in_let k i cont lam =
    let ll = llet k i in
    match lam with
    | Llet ( k2, i2, e2, cont2) ->
      organize_in_let k2 i2 ( ll cont2 cont ) e2
    | Llexrec ( defs, cont2) ->
      organize_let ( Llexrec ( defs, ll cont2 cont ) )
    | Lapply ( Llet ( ka, ia, ea, ca ), ( [Lvar _] as arg ), l) ->
      organize_let ( llet ka ia ea ( ll ( Lapply ( ca, arg, l )) cont ) )
    | Lapply ( lam, [Llet ( ka,ia,ea,ca)], l)->
      organize_let ( llet ka ia ea ( ll ( Lapply ( lam, [ca], l ) ) cont ) )
    | Lprim ( p, l ) ->
      organize_list k i lam cont (fun l -> Lprim ( p, l) ) l
    | Lswitch ( Lvar _ as arg, s ) ->
      ll ( Lswitch ( arg, organize_let_switch s)) ( organize_let cont )
    | Lswitch ( Llet ( ka, ia, ea, ca), s ) ->
      organize_let ( llet ka ia ea ( ll ( Lswitch ( ca, s ) ) cont ) )
    | Lstaticraise ( is, l ) ->
      organize_list k i lam cont (fun l -> Lstaticraise ( is, l ) ) l
    | Lstaticcatch ( lt, c, lw ) -> ll ( Lstaticcatch ( organize_let lt, c, organize_let lw )) ( organize_let cont )
    | Ltrywith ( lt, c, lw ) -> ll ( Ltrywith ( organize_let lt, c, organize_let lw )) ( organize_let cont )
    | Lifthenelse ( Llet ( kc, ic, ec, cc ), lt, le ) ->
      organize_let ( llet kc ic ec ( ll ( Lifthenelse ( cc, lt, le ) ) cont ) )
    | Lifthenelse ( Lvar _ as c, lt, le ) -> ll ( Lifthenelse ( c, organize_let lt, organize_let le ) ) ( organize_let cont )
    | Lwhile ( c, b ) -> ll ( Lwhile ( organize_let c, organize_let b ) ) ( organize_let cont )
    | Lfor ( x, ( Lvar _ as start ) , ( Lvar _ as  stop ), d, body ) ->
      ll ( Lfor ( x, start, stop, d, organize_let body ) ) ( Lvar i )
    | Lfor ( x, Llet ( ks, is, es, cs ), stop, d, body ) ->
      organize_in_let ks is ( ll ( Lfor ( x, cs, stop, d, body ) ) cont ) es
    | Lfor ( x, ( Lvar _ as start ), Llet ( ks, is, es, cs), d, body ) ->
      organize_in_let ks is ( ll ( Lfor ( x, start, cs, d, body ) ) cont ) es
    | Lsend ( _, Lvar _, Lvar _, [], _ ) -> ll lam ( organize_let cont )
    | Lsend ( mk, Llet ( ko, io, eo, co ), ( Lvar _ as meth ), [], loc ) ->
      organize_in_let ko io ( ll ( Lsend ( mk, co, meth, [], loc ) ) cont ) eo
    | Lsend ( mk, obj, Llet ( km, im, em, cm ), [], loc ) ->
      organize_in_let km im ( ll ( Lsend ( mk, obj, cm, [], loc ) ) cont ) em
    | Lsend ( mk, obj, meth, args, loc ) ->
      organize_in_let k i cont ( Lapply ( Lsend ( mk, obj, meth, [], loc ), args, loc ) )
    | Lassign _
    | Levent _
    | Lifused _ -> assert false
    | expr -> ll expr ( organize_let cont )
  and organize_let = function
    | Llet ( k, i, lam, cont ) ->
      organize_in_let k i cont lam
    | Llexrec ( decls, cont) ->
      let decls, to_out =
        List.fold_left (fun (decls, to_out) (id, lam) -> promote_rec decls to_out id lam ) ([],[]) decls
      in
      List.fold_left (fun cont (id,lam) -> Llet ( Strict, id, lam, cont ) ) ( Llexrec ( decls, organize_let cont )) to_out
    | Lvar _  as lam -> lam
    | _ -> assert false
  and organize_let_switch s =
    let aux = List.rev_map (fun (id, lam) -> (id, organize_let lam ) ) in
    let sw_blocks = aux s.sw_blocks in
    let sw_consts = aux s.sw_consts in
    let sw_failaction =
      match s.sw_failaction with
      |      None -> s.sw_failaction
      | Some lam -> Some ( organize_let lam )
    in
    { s with sw_blocks; sw_consts; sw_failaction; }

  and promote_rec promoted expelled i lam =
    match lam with
    | Lprim ( Praise, _) -> ( promoted, ( i, lam ) :: expelled )
    | Lprim _
    | Lfunction _
    | Lsend _  -> ( ( i, lam ) :: promoted, expelled )
    | Llet ( k, id, e, cont ) ->
      let ( promoted, expelled ) = promote_rec promoted expelled id e in
      promote_rec promoted expelled i cont
    | Llexrec ( l, cont ) ->
      let ( promoted, expelled ) =
        List.fold_left (fun (p,e) (id,lam) -> promote_rec p e id lam ) ( promoted, expelled ) l in
      promote_rec promoted expelled i cont
    | _ -> ( promoted, ( i, lam ) :: expelled )
  in

  (* end organizing lets *)

  let normalize lambda = organize_let ( add_let lambda ) in

  let extract_var = function
    | Lvar v -> v
    | _ -> assert false
  in
  let extract_vars = List.map extract_var in

  let rec xlambda = function
    | Llet ( k, i, e, cont ) ->
      Xlet { xe_kind = k; xe_id = i; xe_lam = xcontrol e; xe_in = xlambda cont; }
    | Llexrec ( decls, cont ) ->
      Xrec { xr_decls = List.rev_map
                 (fun (id, lam ) ->
                    match lam with
                    | Lprim (p,l) ->
                      ( id, prim_translate p, extract_vars l )
                    | _ -> assert false
                 ) decls;
             xr_in = xlambda cont; }
    | Lvar v -> Xend v
    | _ -> assert false
  and xcontrol = function
    | Lvar v -> Xvar v
    | Lconst c -> Xconst c
    | Lapply ( Lvar f, [Lvar x], _ ) -> Xapply ( f, x )
    | Lfunction ( _, [arg], body ) ->
      let body = xlambda body in
      let fv = free_vars ( Iset.singleton arg ) body in
      let fvl = Iset.fold (fun v acc -> (v,remk v)::acc) fv [] in
      let body =
        map_idents
          ( fun i -> try List.assoc i fvl with Not_found -> i)
          body
      in
      let body,_ = List.fold_left
          (fun (cont,idx) (_,i) ->
             ( xlet
                 ~kind:Alias
                 i
                 ( Xprim ( XPfunfield idx, [](*, [f_ident]*) ))
                 (* TODO: f_ident (I'm not sure we need it) *)
                 cont
             ), succ idx) (body,0) fvl
      in
      let idf = register_function body in
      Xprim ( XPfun idf, fst ( List.split fvl ) )
    | Lprim ( Praise, [Lvar e] ) -> Xraise e
    | Lprim ( Plazyforce, [Lvar e] ) -> Xlazyforce e
    | Lprim ( Pccall p, l ) -> Xccall ( p, extract_vars l )
    | Lprim ( Pidentity, [Lvar x] ) -> Xvar x
    | Lprim ( Pignore, [Lvar _] ) -> Xconst (Const_pointer 0)
    | Lprim ( p, l ) -> Xprim ( prim_translate p, extract_vars l )
    | Lswitch ( Lvar x, s ) ->
      let aux = List.rev_map (fun (a,b) -> (a, xlambda b) ) in
      Xswitch ( x,
                {
                  x_numconsts = s.sw_numconsts;
                  x_consts = aux s.sw_consts;
                  x_numblocks = s.sw_numblocks;
                  x_blocks = aux s.sw_blocks;
                  x_failaction =
                    (
                      match s.sw_failaction with
                      | None -> None
                      | Some l -> Some ( xlambda l )
                    );
                } )
    | Lstaticraise ( i, l ) -> Xstaticraise ( i, extract_vars l )
    | Lstaticcatch ( lt, i, lw ) -> Xstaticcatch ( xlambda lt, i, xlambda lw )
    | Ltrywith ( lt, i, lw ) -> Xtrywith ( xlambda lt, i, xlambda lw )
    | Lifthenelse ( Lvar v, t, e ) -> Xifthenelse ( v, xlambda t, xlambda e )
    | Lwhile ( c, b ) -> Xwhile ( xlambda c, xlambda b )
    | Lfor ( i, Lvar start, Lvar stop, d, body ) -> Xfor ( i, start, stop, d, xlambda body )
    | Lsend ( k, Lvar obj, Lvar meth, [], _ ) -> Xsend (  k, obj, meth )
    | _ -> assert false
  in

  let code = xlambda ( normalize code ) in
  !ir, funs, code
