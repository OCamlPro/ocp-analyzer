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

module type Domain = sig
  type env (* the global type of environments *)
  type t (* the type of the lattice itself (example: int_interv)*)
  type key (* the type of accessors (example: XId.t)*)
  type sel (* the type of selectors under the domain (example: Pfield) *)
  type values (* the type of values under a key *)
  
  val join : env -> t -> t -> env
  val meet : env -> t -> t -> env
  val widening : env -> t -> t -> env
  val leq : env -> t -> t -> bool

  val is_bottom : env -> t -> bool
  val bottom : t
end

type 'a dom = (module Domain with type key = 'a)

module DomainRegister =
struct
  open Utils
  module type S = sig
    type key
    type t

    val add : key -> key dom -> t -> t
    val fold_key : (key dom -> 'acc -> 'acc) -> key -> t -> 'acc -> 'acc
  end

  module type Arg = Map.OrderedType

  module Make (A : Arg) : S with type key = A.t = struct
    type key = A.t
    module M = Map.Make (A)
    module DS = Set.Make (struct type t = key dom let compare = Pervasives.compare end)
    type t = DS.t M.t

    let add k d m =
      M.add k
        (try DS.add d (M.find k m)
         with Not_found -> DS.singleton d)
        m
    let fold_key f k m acc =
      DS.fold f acc (try M.find k m with Not_found -> DS.empty)
  end
end

module type Environment = sig
  include LatticeMap.Lattice

  type 'a key
  val get_key : 'a key -> t -> 'a
  val set_key : 'a key -> 'a -> t -> t
  val join_key : 'a key -> 'a -> t -> t
  val meet_key : 'a key -> 'a -> t -> t
  val widening_key : 'a key -> 'a -> t -> t
  val is_bottom_key : 'a key -> t -> bool
  
end


module type DomainMaker = functor (E : Environment) -> Domain
