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

type compres = | Bot | True | False | Top

module type Vars =
sig
  include Utils.Set.OrderedType

  val to_string : t -> string
end

module type IE = functor (Vars: Vars) ->
sig
  type t

  val bottom: t

  val empty: t

  val is_bottom: t -> bool

  val var_is_bottom: Vars.t -> t -> bool

  (* val meet: t -> t -> t *)

  val join: t -> t -> t

  val widening: t -> t -> t

  val leq: t -> t -> bool

  val primitive: Vars.t -> t -> primitive -> Vars.t list -> t

  val intcomp: t -> Lambda.comparison -> Vars.t -> Vars.t -> compres

  (* Arguments: var x t y corresponds to let x = y in ... *)
  val var: Vars.t -> t -> Vars.t -> t

  (* First argument is true for strong update (destructive assignment), false for
     weak update (join) *)
  (* Argument order is the same as var, but while var expects the new variable
     to be absent from the previous state, update expects it to be present *)
  val update: bool -> Vars.t -> t -> Vars.t -> t

  (* Sets the given variable to top. If it wasn't already present, it is added. *)
  val set_top: Vars.t -> t -> t

  (* Sets the given variable to bottom, indicating that the variable cannot hold
     any regular value. In relational implementations, this should be interpreted
     as a projection (remove the variable) rather than a global bottom. *)
  val set_bottom: Vars.t -> t -> t

  val const: Vars.t -> t -> Lambda.structured_constant -> t

  val constr: Vars.t -> t -> constr -> t

  (* Interval constraint: intv_constr x abs (l,h) adds the constraints
     l <= x and x <= h to abs *)
  val intv_constr: Vars.t -> t -> int * int -> t

  val print: Format.formatter -> t -> unit

  val print_var: t -> Format.formatter -> Vars.t -> unit

  (* Renaming variables on the left side to variables on the right side *)
  (* Variables not present on the left side should be removed *)
  val renaming: (Vars.t * Vars.t) list -> t -> t

  (* Same as renaming, except that variables not present on the left side
     are left unchanged *)
  val partial_renaming: (Vars.t * Vars.t) list -> t -> t

  (* Remove the given variables from the environment.
     The exact meaning is that the given variables no longer need to
     be tracked, but the implementation is free to choose whether to actually
     remove the variables or not. *)
  val proj: Vars.t list -> t -> t
end
