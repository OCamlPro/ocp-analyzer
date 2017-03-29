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

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Typedtree
open AltLambda

module type S =
sig

  type ty

  val transl_implementation: string -> structure * module_coercion -> ty lambda
  val transl_store_phrases: string -> structure -> int * ty lambda
  val transl_store_implementation:
    string -> structure * module_coercion -> int * ty lambda
  val transl_toplevel_definition: structure -> ty lambda
  val transl_package:
    Ident.t option list -> Ident.t -> module_coercion -> ty lambda
  val transl_store_package:
    Ident.t option list -> Ident.t -> module_coercion -> int * ty lambda

  val toplevel_name: Ident.t -> string
  val nat_toplevel_name: Ident.t -> Ident.t * int

  val primitive_declarations: Primitive.description list ref

  type error =
      Circular_dependency of Ident.t

  exception Error of Location.t * error

  val report_error: Format.formatter -> error -> unit

end

module Make : functor (Tr : AltTypeTransducer.T) ->
  functor (Translobj : AltTranslobj.S with type ty = Tr.t) ->
  functor (Translcore : AltTranslcore.S with type ty = Tr.t) ->
  functor (Translclass : AltTranslclass.S with type ty = Tr.t) ->
  S with type ty = Tr.t
