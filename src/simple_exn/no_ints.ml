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

module Make (Vars: IEsig.Vars) =
struct
  type t = unit

  let bottom = ()

  let empty = ()

  let is_bottom _ = false

  let var_is_bottom _ _ = false

  let join _ _ = ()

  let widening _ _ = ()

  let leq _ _ = true

  let primitive _ _ _ _ = ()

  let intcomp _ _ _ _ = IEsig.Top

  let var _ _ _ = ()

  let update _ _ _ _ = ()

  let set_top _ _ = ()

  let set_bottom _ _ = ()

  let const _ _ _ = ()

  let constr _ _ _ = ()

  let intv_constr _ _ _ = ()

  let print ppf () =
    Format.fprintf ppf "(No integers)"

  let print_var () ppf _ =
    Format.fprintf ppf "(No integers)"

  let renaming _ _ = ()

  let partial_renaming _ _ = ()

  let proj _ _ = ()

end
