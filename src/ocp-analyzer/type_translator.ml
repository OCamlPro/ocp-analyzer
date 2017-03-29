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

open Type_description
open Types

let option_map f = function
  | None -> None
  | Some x -> Some (f x)

let rec convert_path = function
  | Path.Pident id -> Base (id.Ident.name)
  | Path.Pdot (p, s, pos) -> Dot (convert_path p, s, pos)
  | Path.Papply (p1, p2) -> Apply (convert_path p1, convert_path p2)

let rec transl_type_expr env te =
  match te.desc with
  | Tvar so ->
     Tyvar so
  | Tarrow (_, te1, te2, _) ->
     Tyarrow (transl_type_expr env te1, transl_type_expr env te2)
  | Ttuple l ->
     Tytuple (List.map (transl_type_expr env) l)
  | Tconstr (p, l, _) ->
     Tyconstr (constr_of_path env p, List.map (transl_type_expr env) l)
  | _ -> Tyother

and constr_of_path env p =
  let td = Env.find_type p env in
  let tyname = convert_path p in
  let tydecl = from_type_declaration env td in
  { tyname; tydecl }

and from_type_declaration env td =
  let transl_param = function
    | Tvar so -> so
    | _ -> None
  in
  let typarams = List.map transl_param td.type_params in
  let tyarity = td.type_arity in
  let tykind = transl_kind env td.type_kind in
  let tymanifest = option_map (transl_type_expr env) td.type_manifest in
  { typarams; tyarity; tykind; tymanifest }

and transl_kind env = function
  | Type_abstract -> Tyabstract
  | Type_record (l, _) -> Tyrecord (List.map (transl_label env) l)
  | Type_variant l -> Tyvariant (List.map (transl_constr env) l)
  | Type_open -> Tyopen

and transl_label env ld =
  let lid = ld.ld_id in
  let lmut = match ld.ld_mutable with Immutable -> false | Mutable -> true in
  let ltyp = transl_type_expr env ld.ld_type in
  { lid; lmut; ltyp }

and transl_constr env cd =
  let cid = cd.cd_id.Ident.name in
  let cargs = List.map (transl_type_expr env) cd.cd_args in
  let cres = option_map (transl_type_expr env) cd.cd_res in
  { cid; cargs; cres }

let is_int_type tc =
  tc.tyname = convert_path (Predef.path_int)

let is_int32_type tc =
  tc.tyname = convert_path (Predef.path_int32)

let is_int64_type tc =
  tc.tyname = convert_path (Predef.path_int64)

let is_nativeint_type tc =
  tc.tyname = convert_path (Predef.path_nativeint)

let is_float_type tc =
  tc.tyname = convert_path (Predef.path_float)

let is_char_type tc =
  tc.tyname = convert_path (Predef.path_char)

