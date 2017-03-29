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

open Utils

module Ints = Set.Make
    (struct
      type t = int
      let compare (x:t) y = Pervasives.compare x y
      let print = Format.pp_print_int
    end)

type t = Ints.t
let join = Ints.union
let meet = Ints.inter
let widening = join
let leq = Ints.subset
let equal = Ints.equal
let bottom = Ints.empty
let is_bottom = Ints.is_empty
let print = Ints.print_sep (fun ppf -> Format.fprintf ppf "; ")
