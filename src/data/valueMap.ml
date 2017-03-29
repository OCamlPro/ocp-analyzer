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

open Locations

type 'a locm = 'a Locm.t

type t = Data.t locm

let empty = Locm.empty

let merger_rec f _ x y =
  match x, y with
  | None, a| a, None -> a
  | Some a, Some b -> Some (f a b)

let join_or_widen union e1 e2 = Locm.merge (merger_rec union) e1 e2

module VMAccess = struct

  let get_data = Locm.find
  let set_data = Locm.add

  let get_datas is e =
    Locs.fold (fun loc acc -> Data.union acc @@ get_data loc e) is Data.bottom
  
end
