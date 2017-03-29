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

open Map_utils

module Make (ProgId: Utils.Set.OrderedType) =
struct
  module XIdm = Utils.Map.Make(ProgId)

  type t = unit XIdm.t

  let bottom = XIdm.empty

  let is_bottom =
    XIdm.is_empty

  let var_is_bottom v m =
    not (XIdm.mem v m)

  let meet = XIdm.merge (map_merge_meet (fun () () -> ()) (fun () -> false))

  let join = XIdm.merge (map_merge_join (fun () () -> ()))

  let widening = join

  let leq =
    let module M =
      MapLeq (struct
        module M = XIdm type v = unit let vleq () () = true
      end)
    in
    M.comp

  let primitive i m _p _args =
    XIdm.add i () m

  let var i m j =
    if XIdm.mem j m then XIdm.add i () m else m

  let const i m _cst =
    XIdm.add i () m

  let top i m =
    XIdm.add i () m

  let set_bottom i m =
    XIdm.remove i m

  let constr i m _cstr =
    if XIdm.mem i m then m else bottom

  let print ppf e =
    XIdm.print (fun ppf () -> Format.fprintf ppf "<Top>") ppf e

  let print_var m ppf x =
    Format.fprintf ppf "%s" (if XIdm.mem x m then "<Top>" else "<Bot>")

end
