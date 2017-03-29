(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open AltLambda

module type S = sig

  type ty

  val oo_prim: string -> ty lambda

  val share: structured_constant -> ty lambda
  val meth: ty lambda -> string -> ty lambda * ty lambda list

  val reset_labels: unit -> unit
  val transl_label_init: ty lambda -> ty lambda
  val transl_store_label_init:
    Ident.t -> int -> ('a -> ty lambda) -> 'a -> int * ty lambda

  val method_ids: Lambda.IdentSet.t ref (* reset when starting a new wrapper *)

  val oo_wrap: Env.t -> bool -> ('a -> ty lambda) -> 'a -> ty lambda
  val oo_add_class: Ident.t -> Env.t * bool
end

module Make : functor (Tr : AltTypeTransducer.T) ->
  S with type ty = Tr.t
