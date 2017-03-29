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

open Hgraph_types
open Utils

module MakeT ( V : CloneOrderedHashedType ) ( H : CloneOrderedHashedType )
  : T with type vertex = V.t
       and type hedge = H.t
       and module Vertex = V
       and module Hedge = H

module Make(T:T) : Hgraph
  with module T := T
   and type VertexSet.elt = T.vertex
   and type VertexSet.t = Set.Make(T.Vertex).t
   and module VertexSet = Set.Make(T.Vertex)
   and type VertexMap.key = T.vertex
   and type 'a VertexMap.t = 'a Map.Make(T.Vertex).t
   and module VertexMap = Map.Make(T.Vertex)
   and type VertexTbl.key = T.vertex
   and module VertexTbl = Hashtbl.Make(T.Vertex)
   and type HedgeSet.elt = T.hedge
   and module HedgeSet = Set.Make(T.Hedge)
   and type HedgeMap.key = T.hedge
   and module HedgeMap = Map.Make(T.Hedge)
   and type HedgeTbl.key = T.hedge
   and module HedgeTbl = Hashtbl.Make(T.Hedge)
