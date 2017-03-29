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

(* implementation of complete lattices *)

module type BottomedType =
sig
  type t
  val bottom : t
  val is_bottom : t -> bool
end

module type ToppedType =
sig
  type t
  val top : t
  val is_top : t -> bool
end
  

module type Ordered =
sig

  type t
  val leq : t -> t -> bool
  val equal : t -> t -> bool

end

module type SemiLattice =
sig
  type t

  include BottomedType with type t := t
  include Ordered with type t := t

  val join : t -> t -> t
end

module type Lattice =
sig
  type t
    
  include SemiLattice with type t := t
  (* include ToppedType with type t := t *)

  val meet : t -> t -> t
end

module type Widenable =
sig
  type t

  val widening : t -> t -> t
end

module type WidenableLattice =
sig

  type t

  include Lattice with type t := t
  include Widenable with type t := t
  
end

module type WidenableSemiLattice =
sig
  type t
  include SemiLattice with type t := t
  include Widenable with type t := t
end

module MakeFiniteWidening ( L : SemiLattice ) : WidenableSemiLattice =
struct
  include L
  let widening = L.join
end
