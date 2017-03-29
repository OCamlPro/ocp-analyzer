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
open Xlambda_to_hgraph

type v = Vertex.t

module type Entry =
sig
  val inv : v
  val outv : v
  val exnv : v
  val g : hg
  val funs : ( F.t, fun_desc ) Hashtbl.t
  val mk_vertex : unit -> v
  val mk_hedge : unit -> Hedge.t
end

module Stack : Stack_types.Stack with type elt = F.t

module M :
  functor
    (A : functor (Stack:Stack_types.Stack) ->
        OCamlEnv.S with type hedge = Hedge.t
                    and module Stack = Stack) ->
  functor ( E : Entry ) ->
  sig
    include Fixpoint_types.Manager
      with
        module T := T
       and module H = G
       and type function_id = F.t
       and module Function_id = F
       and module Stack = Stack
       and type hedge_attribute = hattr
       and type vertex_attribute = vattr
       and type graph_attribute = gattr
       and type abstract = A(Stack).t
       and type global = A(Stack).global
  end

val get_counter : unit -> int
