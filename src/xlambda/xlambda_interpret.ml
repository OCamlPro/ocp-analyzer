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

open Ident
open Lambda
open Asttypes
open Xlambda
open Common_types

module Idm = Map.Make ( struct type t = xid let compare = compare end )

type v =
  | Int of int
  | Intn of nativeint
  | Int32 of int32
  | Int64 of int64
  | Float of float
  | Floata of float array
  | String of string
  | Cp of int
  | Block of int * xid ref list
  | Fun of F.t * xid list
  | Unknown

type env = v Idm.t
type fun_table = ( F.t, xlambda ) Hashtbl.t

let (env_empty:env) = Idm.empty

exception Staticraise of int * v list
exception Exn of v

let id_fun = "", { stamp = max_int; name = "#switch_f"; flags = 0}
let id_arg = "", { stamp = pred ( pred max_int); name = "#arg_f"; flags = 0}

let get_env env i =
  try Idm.find i env with
    Not_found ->
    XId.output stdout i;
    assert false
let set_env env i v = Idm.add i v env

let assign_list e l1 l2 =
  List.fold_left2 set_env e l1 l2

let val_to_bool v = match v with
  | Cp 0 | Int 0 -> false
  | Cp _ | Int _ -> true
  | _ -> assert false

let val_to_int i = match i with
  | Int i
  | Cp i -> i
  | _ -> assert false

let of_bool b = Cp ( if b then 1 else 0)
let of_int i = Int i

let val_unit = Cp 0
let match_failure = Exn val_unit (* This is indeed false *) (* TODO *)

let rec xlambda (funs:fun_table) (env:env) = function
  | Xend i -> get_env env i, env
  | Xlet { xe_id; xe_lam; xe_in; _ }-> 
    let v = xcontrol funs env xe_lam in
    let env = set_env env xe_id v in
    xlambda funs env xe_in
  | Xrec { xr_decls; xr_in; }  ->
    let env =
      List.fold_left
        (fun env (i,p,l) ->
           set_env env i ( call_alloc env funs p l )
        )
        env xr_decls in
    xlambda funs env xr_in

and structured_constant = function
  | Const_base c ->
    begin
      match c with
      | Const_int i -> Int i
      | Const_char c -> Int (Char.code c)
      | Const_string (s,_) -> String s
      | Const_float f -> Float ( float_of_string f)
      | Const_int32 i -> Int32 i
      | Const_int64 i -> Int64 i
      | Const_nativeint i -> Intn i
    end
  | Const_pointer i -> Cp i
  | Const_block (i, l) -> failwith "TODO: const_block"
  (* Block ( i, List.map structured_constant l) *)
  | Const_float_array l -> failwith "TODO: float_array const"
  | Const_immstring s -> String s


and xcontrol funs env = function
  | Xvar i -> get_env env i
  | Xconst sc -> structured_constant sc
  | Xapply ( f, x) -> call_fun funs (get_env env f) (get_env env x)
  | Xprim ( p, l) -> call_prim env funs p l
  | Xalloc ( p, l) -> call_alloc env funs p l
  | Xswitch ( i, s) ->
    let switch_handle i l =
      let b =
        try List.assoc i l with
          Not_found ->
          ( match s.x_failaction with
            | Some b -> b
            | None -> raise match_failure)
      in
      fst ( xlambda funs env b )
    in
    begin
      match get_env env i with
      | Int i | Cp i -> switch_handle i s.x_consts
      | Block (i,l) -> switch_handle i s.x_blocks
      | _ -> assert false
    end
  | Xstringswitch (i,l,o) ->
    begin
    match get_env env i with
    | String s ->
      let xlam = try List.assoc s l
        with Not_found -> begin match o with
            | Some xlam -> xlam
            | None -> raise match_failure
          end
      in
      fst (xlambda funs env xlam)
    | _ -> assert false
    end
  | Xstaticraise ( i, l) -> raise ( Staticraise ( i, List.map (get_env env) l))
  | Xstaticcatch ( lam, (i,l), lam2) ->
    begin
      try fst ( xlambda funs env lam ) with
      | Staticraise ( i2, l2) when i2 = i ->
        let env = assign_list env l l2 in
        fst ( xlambda funs env lam2 )
    end
  | Xraise (_,i) -> raise ( Exn ( get_env env i))
  | Xtrywith ( lam, i, lam2) ->
    begin
      try fst (xlambda funs env lam) with
        Exn v ->
        let env = set_env env i v in
        fst ( xlambda funs env lam2 )
    end
  | Xifthenelse ( i, l1, l2) ->
    fst (
      if val_to_bool ( get_env env i)
      then xlambda funs env l1
      else xlambda funs env l2
    )
  | Xwhile ( cond, body) ->
    while ( val_to_bool ( fst ( xlambda funs env cond )))
    do ignore ( xlambda funs env body) done;
    val_unit
  | Xfor ( id, start, stop, direction, body) ->
    let start = val_to_int ( get_env env start) in
    let stop = val_to_int ( get_env env stop) in
    let f i =
      let env = set_env env id (Int i) in
      ignore ( xlambda funs env body)
    in
    begin
      if direction = Asttypes.Upto
      then for i =  start to stop do f i done
      else for i =  start downto stop do f i done
    end;
    val_unit
  | Xlazyforce _ -> Unknown
  | Xccall _ -> Unknown
  | Xsend _ -> Unknown


and call_fun funs f x =
  match f with
  | Fun ( i, l ) ->
    begin
      let body = Hashtbl.find funs i in
      let e =
        env_empty
        |> Idm.add id_fun f
        |> Idm.add id_arg x
      in
      fst ( xlambda funs e body )
    end
  | _ -> assert false

and call_alloc env funs p l =
  match p, l with
  | XPmakeblock (i,_), _ ->  ( Block ( i, List.map ref l))
  | XPfun i, _ -> Fun ( i, (* List.map g *) l )
  | _,_ -> failwith "TODO: allocators"

and call_prim env funs p l =
  let g i = get_env env i in
  match p, l with
  (* Blocks *)
  | XPfield i, [b]
  | XPfloatfield i, [b] ->
    begin
      match g b with
      | Block ( _, l) ->
        g !( List.nth l i )
      | _ -> assert false
    end
  | XPsetfield ( i, _), [b;v]
  | XPsetfloatfield i, [b;v] ->
    begin
      match g b with
      | Block (_,l) ->
        (List.nth l i) := v; val_unit
      | _ -> assert false
    end
  | XPduprecord _, [i] ->
    begin
      match g i with
      |  Block (i,l) ->
        Block (i, List.map (fun i -> ref !i) l )
      | _ -> assert false
    end
  (* Booleans *)
  | XPnot, [b] -> of_bool ( not (val_to_bool (g b)))
  (* Ints *)
  | XPnegint, [x] -> of_int ( ~- (val_to_int (g x)))
  | XPaddint, [ x; y] -> of_int ( ( val_to_int (g x)) + ( val_to_int (g y)))
  | XPsubint, [ x; y] -> of_int ( ( val_to_int (g x)) - ( val_to_int (g y)))
  | XPmulint, [ x; y] -> of_int ( ( val_to_int (g x)) * ( val_to_int (g y)))
  | XPdivint, [ x; y] -> of_int ( ( val_to_int (g x)) / ( val_to_int (g y)))
  | XPmodint, [ x; y] -> of_int ( ( val_to_int (g x)) mod ( val_to_int (g y)))
  | XPandint, [ x; y] -> of_int ( ( val_to_int (g x)) land ( val_to_int (g y)))
  | XPorint, [ x; y] -> of_int ( ( val_to_int (g x)) lor ( val_to_int (g y)))
  | XPxorint, [ x; y] -> of_int ( ( val_to_int (g x)) lxor ( val_to_int (g y)))
  | XPlslint, [ x; y] -> of_int ( ( val_to_int (g x)) lsl ( val_to_int (g y)))
  | XPlsrint, [ x; y] -> of_int ( ( val_to_int (g x)) lsr ( val_to_int (g y)))
  | XPasrint, [ x; y] -> of_int ( ( val_to_int (g x)) asr ( val_to_int (g y)))
  | XPintcomp c, [ x; y] -> of_bool ( comparison c ( val_to_int (g x))  ( val_to_int (g y)))
  | XPoffsetint _, _ -> failwith "TODO: ask Pierre"
  | XPoffsetref _, _ -> failwith "TODO: ask Pierre"
  (* Functions *)
  | XPfunfield i, [] ->
    begin
      match g id_fun with
      | Fun ( _, l) -> g ( List.nth l i )
      | _ -> assert false
    end
  | _, _ -> failwith "TODO: primitives"

and comparison = function
  | Ceq -> (=)
  | Cneq -> (<>)
  | Clt -> (<)
  | Cgt -> (>)
  | Cle -> (<=)
  | Cge -> (>=)



