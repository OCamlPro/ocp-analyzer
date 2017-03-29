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

module type Selector = Key

module type SymbolicAccessPath =
sig
  (* The simple selectors *)
  module S : Selector

  (* The bases *)
  module Sls : Set with type elt = S.t list
  module Bident : Key
  val base_to_ident : Sls.t -> Bident.t
  (* raise assert false if argument not in base *)
  val base_of_ident : Bident.t -> Sls.t

  (* The numerical domain and its usage *)

  module N : Numerical
  
  type t =
    {
      path : in_t list;
      coefs : N.Var.t list;
    }
  and in_t =
    | Sel of S.t
    | Expr of Bident.t * N.Var.t

end

module type SymbolicAliasPairs =
sig
  type t
  include Lattice.WidenableSemiLattice with type t := t
  module N : Numerical
  module SAP : SymbolicAccessPath with module N = N
end
