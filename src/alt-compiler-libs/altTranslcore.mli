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
   for the core language *)

open Asttypes
open Typedtree
open AltLambda

module type S =
sig

  type ty
  
  val name_pattern: string -> (pattern * 'a) list -> Ident.t

  val transl_exp: expression -> ty lambda
  val transl_apply: ty lambda -> (label * expression option * optional) list
    -> Location.t -> ty -> ty lambda
  val transl_let:
    rec_flag -> (pattern * expression) list -> ty lambda -> ty lambda
  val transl_primitive: Location.t -> Primitive.description -> ty lambda
  val transl_exception:
    Ident.t -> Path.t option -> exception_declaration -> ty lambda

  val check_recursive_lambda: Ident.t list -> ty lambda -> bool

  type error =
      Illegal_letrec_pat
    | Illegal_letrec_expr
    | Free_super_var
    | Unknown_builtin_primitive of string

  exception Error of Location.t * error

  open Format

  val report_error: formatter -> error -> unit

  (* Forward declaration -- to be filled in by Translmod.transl_module *)
  val transl_module :
    (module_coercion -> Path.t option -> module_expr -> ty lambda) ref
  val transl_object :
    (Ident.t -> string list -> class_expr -> ty lambda) ref
end


module Make :
  functor ( Tr : AltTypeTransducer.T) ->
  functor (Matching : AltMatching.S with type ty = Tr.t) ->
  functor (Translobj : AltTranslobj.S with type ty = Tr.t) ->
    S with type ty = Tr.t
