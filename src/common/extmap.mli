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


module type Type = sig type t end
module type OrderedType = Map.OrderedType
module type BottomedType = Lattice.BottomedType

module type S =
sig

  type 'a t
  (** type similar to standard maps, below are the standard operations on it *)

  type keys
  (** keys is an internal type that should not be used directly by the user *)

  type +'a key_chain
  (** defines the arrows necessary for nested maps *)

  val empty: 'a t
  val is_empty: 'a t -> bool
  val mem:  ( 'a t -> bool ) key_chain
  val add: ( 'a -> 'a t -> 'a t ) key_chain
  val singleton: ( 'a -> 'a t ) key_chain
  val remove: ( 'a t -> 'a t ) key_chain
  val merge:
    (( 'a option -> 'b option -> 'c option ) key_chain) -> 'a t ->  'b t -> 'c t
  val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter: (( 'a -> unit ) key_chain) -> 'a t -> unit
  val fold: (( 'a -> 'b -> 'b ) key_chain) -> 'a t -> 'b -> 'b
  val for_all: (( 'a -> bool ) key_chain) -> 'a t -> bool
  val exists: (( 'a -> bool ) key_chain) -> 'a t -> bool
  val filter: (( 'a -> bool ) key_chain) -> 'a t -> 'a t
  val partition: (( 'a -> bool ) key_chain) -> 'a t -> 'a t * 'a t
  val cardinal: 'a t -> int
  val split: ( 'a t -> 'a t * 'a option * 'a t ) key_chain
  val find: ( 'a t -> 'a ) key_chain
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: (( 'a -> 'b ) key_chain) -> 'a t -> 'b t


  val fold_args : (keys -> 'a) -> 'a key_chain
  (** fold_args and unfold_args allow to manipulate the internal representation key
      and to iterate through the maps without knowing the actual number of arguments.
      fold_args turns a non-currified function into a currified one.
  *)

  val unfold_args : 'a key_chain -> keys -> 'a
  (** as fold_args, allow to turn a currified function into a non-currified one *)


end

(** Module type SBound is the same as S except it has a elt type
    and the map points to elt instead of being polymorphic.
*)
module type SBound =
sig

  type t
  type elt
  type keys
  type +'a key_chain

  val empty: t
  val is_empty: t -> bool
  val mem:  ( t -> bool ) key_chain
  val add: ( elt -> t -> t ) key_chain
  val singleton: ( elt -> t ) key_chain
  val remove: ( t -> t ) key_chain
  val merge:
    (( elt option -> elt option -> elt option ) key_chain) -> t ->  t -> t
  val compare: (elt -> elt -> int) -> t -> t -> int
  val equal: (elt -> elt -> bool) -> t -> t -> bool
  val iter: (( elt -> unit ) key_chain) -> t -> unit
  val fold: (( elt -> 'a -> 'a ) key_chain) -> t -> 'a -> 'a
  val for_all: (( elt -> bool ) key_chain) -> t -> bool
  val exists: (( elt -> bool ) key_chain) -> t -> bool
  val filter: (( elt -> bool ) key_chain) -> t -> t
  val partition: (( elt -> bool ) key_chain) -> t -> t * t
  val cardinal: t -> int
  val split: ( t -> t * elt option * t ) key_chain
  (* val find_unsafe: ( t -> elt ) key_chain *)
  val find: ( t -> elt ) key_chain
  val map: (elt -> elt) -> t -> t
  val mapi: (( elt -> elt ) key_chain) -> t -> t

  val fold_args : (keys -> 'a) -> 'a key_chain
  val unfold_args : 'a key_chain -> keys -> 'a
end

(** Make is identical to Map.Make *)
module Make :
  functor (Ord : OrderedType) ->
    S with type 'a t = 'a Map.Make(Ord).t
       and type 'a key_chain = Ord.t -> 'a

(** Nest takes a key type and a previously created map,
    it then returns a new map with an additional key.
*)
module Nest :
  functor (Ord : OrderedType) ->
  functor (M : S) ->
    S with type 'a t = 'a M.t Map.Make(Ord).t
       and type 'a key_chain = Ord.t -> 'a M.key_chain

(** Bind takes a polymorphic map module and turns it into
    a monomorphic one *)
module Bind :
  functor (M : S) ->
  functor (T : Type) ->
    SBound with type t = T.t M.t
            and type 'a key_chain = 'a M.key_chain
            and type elt = T.t
            and type keys = M.keys

(** SafeFind makes find return a default value instead of raising Not_found.
    Works only on monomorphic maps.
*)
module SafeFind :
  functor (M : SBound) ->
  functor (O : BottomedType with type t = M.elt) ->
    SBound with type t = M.t
            and type 'a key_chain = 'a M.key_chain
            and type elt = M.elt
            and type keys = M.keys


(** Printers for maps *)

module type PrintableType =
sig
  type t
  val print : Format.formatter -> t -> unit
end
(** Simple printable type *)

module type PrintablePolyType =
sig
  type 'a t
  val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit  
end
(** Polymorphic printer *)

module type OrderedPrintableType =
sig
  type t
  include OrderedType with type t := t
  include PrintableType with type t := t
end
(** type with a print and a compare *)

module type SPrintable =
sig
  type 'a t
  include S with type 'a t := 'a t
  include PrintablePolyType with type 'a t := 'a t
end
(** polymorphic printable map *)

module type SBoundPrintable =
sig
  type t
  include SBound with type t := t
  include PrintableType with type t := t
end
(** monomorphic printable map *)

module MakePrint :
  functor (Ord : OrderedPrintableType) ->
    SPrintable
  with type 'a t = 'a Make(Ord).t
   and type 'a key_chain = 'a Make(Ord).key_chain
   and type keys = Make(Ord).keys
(** Create a printable simple map *)

module NestPrint :
  functor (Ord : OrderedPrintableType) ->
  functor (M : SPrintable) ->
    SPrintable
  with type 'a t = 'a Nest(Ord)(M).t
   and type 'a key_chain = 'a Nest(Ord)(M).key_chain
   and type keys = Nest(Ord)(M).keys
(** Nester for printable maps *)

module BindPrint :
  functor (M : SPrintable) ->
  functor (B : PrintableType) ->
    SBoundPrintable
  with type t = B.t M.t
   and type 'a key_chain = 'a M.key_chain
   and type keys = M.keys
   and type elt = B.t
(** Polymorphic to monomorphic printer *)
