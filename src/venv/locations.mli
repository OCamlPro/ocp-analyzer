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

module XIdm : Utils.Map.S with type key = XId.t

type atpl

val of_xid : XId.t -> atpl
val print_atpl : Format.formatter -> atpl -> unit

module Locs : Utils.Set.S with type elt = atpl
module Locm : sig
  include Utils.Map.S with type key = atpl
  val fold_key : ( key -> 'a -> 'b -> 'b) -> key -> 'a t -> 'b -> 'b
  val fold_by_loc : (key -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module Loc : sig
  type t = atpl
  val compare : t -> t -> int
  val print : Format.formatter -> t -> unit
end
