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


module type OrderedType = Set.OrderedType

module type S =
sig

  type t
  (** Type similar to standard sets, below are the standard operations on it *)

  type keys
  (** keys is an internal type that should not be used directly by the user *)

  type +'a key_chain
  (** defines the arrows necessary for nested sets *)


  val empty: t
  val is_empty: t -> bool

  val mem: ( t -> bool ) key_chain
  val add: ( t -> t ) key_chain
  val singleton: t key_chain
  val remove: ( t -> t ) key_chain
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val iter: (unit key_chain) -> t -> unit
  val fold: (( 'a -> 'a ) key_chain) -> t -> 'a -> 'a
  val for_all: (bool key_chain) -> t -> bool
  val exists: (bool key_chain) -> t -> bool
  val filter: (bool key_chain) -> t -> t
  val partition: (bool key_chain) -> t -> t * t
  val cardinal: t -> int
  val split: ( t -> t * bool * t ) key_chain

  val fold_args : (keys -> 'a) -> 'a key_chain
  (** fold_args and unfold_args allow to manipulate the internal representation key
      and to iterate through the sets without knowing the actual number of arguments.
      fold_args turns a non-currified function into a currified one.
  *)

  val unfold_args : 'a key_chain -> keys -> 'a
  (** as fold_args, allow to turn a currified function into a non-currified one *)

end

(** Similar to Set.Make but compatible with signature S above *)
module Make : functor ( Ord : OrderedType ) ->
  S with type t = Set.Make(Ord).t
     and type 'a key_chain = Ord.t -> 'a

(** Adds a key on top of the set, everything stays currified *)
module Nest : functor (Ord : OrderedType) -> functor (S : S) ->
  S with type t = S.t Map.Make(Ord).t
     and type 'a key_chain = Ord.t -> 'a S.key_chain

(** Base module for printable types *)
module type PrintableType =
sig
  type t
  val print : Format.formatter -> t -> unit
end

(** Printable key type *)
module type OrderedPrintableType =
sig
  type t
  include OrderedType with type t := t
  include PrintableType with type t := t
end

(** Printable map type *)
module type SPrintable =
sig
  type t
  include S with type t := t
  include PrintableType with type t := t
end

(** Make a printable Set *)
module MakePrint :
  functor (Ord : OrderedPrintableType) ->
    SPrintable
  with type t = Make(Ord).t
   and type 'a key_chain = 'a Make(Ord).key_chain
   and type keys = Make(Ord).keys

(** Nest a printable Set *)
module NestPrint :
  functor (Ord : OrderedPrintableType) ->
  functor (S : SPrintable) ->
    SPrintable
  with type t = Nest(Ord)(S).t
   and type 'a key_chain = 'a Nest(Ord)(S).key_chain
