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
open Locations
open Envs

let ignore_bottom f e = e >? f

(* basic env management *)

let loc_of_xid xid = Locations.of_xid xid

let fold_to_on f env loc d e = Envs.join (f loc d env) e
let on_first_acc = Envs.bottom

let fold_loc f loc acc env =
  Locm.fold_key f loc env.values acc

let on_loc f loc e env =
  fold_loc (fold_to_on f env) loc e env

let fold_locs f locs acc (env:Envs.t') =
  Locs.fold f locs acc

let fold__locs f locs acc (env:Envs.t') =
  fold_locs (fun loc acc -> fold_loc f loc acc env) locs acc env

let on__locs f locs env =
  fold__locs (fold_to_on f env) locs on_first_acc env

let fold_xid f xid acc env =
  let locs = XIdm.find xid env.entries in
  fold__locs f locs acc env

let on_xid f xid env =
  fold_xid (fold_to_on f env) xid on_first_acc env

let set_data loc d env =
  Env { env with values = Locm.add loc d env.values }

let set_env xid d env =
  let loc = loc_of_xid xid in
  !> {
    entries = XIdm.add xid (Locs.singleton loc) env.entries;
    values = Locm.add loc d env.values;
  }


let get_idents xid env = XIdm.find xid env.entries
let set_idents xid locs env = !> { env with entries = XIdm.add xid locs env.entries }
let set_ident xid loc env = set_idents xid (Locs.singleton loc) env

let reg_data d env =
  let loc = loc_of_xid (XId.create ()) in
  set_data loc d env, loc

let rm_env xid env =
  !> { env with entries = XIdm.remove xid env.entries }

