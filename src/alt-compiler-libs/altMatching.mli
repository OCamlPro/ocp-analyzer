(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Compilation of pattern-matching *)

open Typedtree
open AltLambda

module type S =
sig

  type ty
  
  val for_function:
    Location.t -> int ref option -> ty lambda -> (pattern * ty lambda) list ->
    partial -> ty lambda
  val for_trywith:
    ty lambda -> (pattern * ty lambda) list -> ty lambda
  val for_let:
    Location.t -> ty lambda -> pattern -> ty lambda -> ty lambda
  val for_multiple_match:
    Location.t -> ty lambda list -> (pattern * ty lambda) list -> partial ->
    ty lambda

  val for_tupled_function:
    Location.t -> Ident.t list -> (pattern list * ty lambda) list ->
    partial -> ty lambda

  exception Cannot_flatten

  val flatten_pattern: int -> pattern -> pattern list

  val make_test_sequence:
    ty lambda option -> ty primitive -> ty primitive -> ty lambda ->
    (Asttypes.constant * ty lambda) list -> ty lambda

  val inline_lazy_force : ty lambda -> Location.t -> ty lambda
end

module Make : functor ( Tr : AltTypeTransducer.T ) -> S with type ty = Tr.t
