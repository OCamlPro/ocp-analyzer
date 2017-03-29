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

open Envs

module P =
struct
  type t = string
  let hash ( d : t) = Hashtbl.hash d
  let equal (a:t) (b:t) = a = b
  let print = Format.pp_print_string
end

module HP = Utils.Htbl.Make ( P )

type fun_applyer =
    Common_types.xid list ->
    Common_types.xid ->
    Envs.t ->
    Envs.t * Envs.t

let defs : fun_applyer HP.t = HP.create 1024

let default = fun _ i e -> ( e >! Access.set_env i Data.top, Envs.bottom)

let get_envs d =
  try HP.find defs d.Primitive.prim_name with
    _ -> default

let add_def = HP.add defs
