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

open Data

(* expressions *)

let set d e = { d with expr = Hinfos.singleton e; }
let sets d l = { d with expr = List.fold_left (fun es e -> Hinfos.add e es) Hinfos.empty l; }
let add d e = { d with expr = Hinfos.add e d.expr; }
