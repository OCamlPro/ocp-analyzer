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
open Utils

module Vertex :
sig
  include Hgraph_types.CloneOrderedHashedType
  val mk : ?modulename : string -> unit -> t
end
module Hedge :
sig
  include Hgraph_types.CloneOrderedHashedType
  val mk : unit -> t
end

module T :  Hgraph_types.T
with type vertex = Vertex.t and type hedge = Hedge.t
and module Vertex = Vertex and module Hedge = Hedge

module G :
  ( Hgraph_types.Hgraph
    with
      type T.vertex = T.vertex
    and type T.hedge = T.hedge
    and module T := T
    and type VertexSet.t = Set.Make(T.Vertex).t
    and type VertexSet.elt = T.vertex
    and type HedgeSet.t = Set.Make(T.Hedge).t
    and type HedgeSet.elt = Set.Make(T.Hedge).elt
    and module VertexSet = Set.Make(T.Vertex)
    and module HedgeSet = Set.Make(T.Hedge)
  )

open G

type vattr = Normal | Exception
type hattr = ( xid list * hinfo ) list
type gattr = unit
type hg = ( vattr, hattr, gattr ) G.graph

type fun_desc =
  {
    f_graph : hg;
    f_in : Vertex.t array;
    f_out : Vertex.t array;
    f_vertex : VertexSet.t;
    f_hedge : HedgeSet.t;
    (* f_arg : xid; *)
    (* f_return : xid; *)
    (* f_exn : xid; *)
  }

type mod_desc

 val mk_graph :
   modulename : string ->
   ( F.t, Xlambda.xlambda ) Hashtbl.t ->
   Xlambda.xlambda ->
   ( hg * ( F.t, fun_desc ) Hashtbl.t *
       Vertex.t * Vertex.t * Vertex.t *
       xid * xid )
(* the graph, the in, out and exn vectors, the functions, the exn and return xids *)


val init :
  modulename : string ->
  ( F.t, Xlambda.xlambda ) Hashtbl.t ->
  hg * ( F.t, fun_desc ) Hashtbl.t

(*
   takes the last xid number and the fun hashtbl
   returns the graph, the fun descriptors and a exn_id
 *)

val mk_subgraph :
  g : hg ->
  modulename : string ->
  exn_id : xid ->
  Xlambda.xlambda ->
  mod_desc

(* returns a inv, a outv, a exnv and a return xid *)

val vattr_merge : vattr -> vattr -> vattr

val merge_graphs :
  g : hg ->
  mod_desc array ->
  ( Vertex.t * Vertex.t * Vertex.t * xid )
