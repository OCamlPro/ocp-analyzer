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
open BaseTypes
open BaseGraph

module D : UKey = MakeUKey (struct end)

(* Will be usefull for finding of common ancestor,
   should be replaced by a binomial heap *)
module Datem = Map.Make (D)

module Eq = BaseEquivalence
module G = BaseGraph

type 'date env_internal =
  {
    date : 'date;
    graph : G.graph;
    parent : env_commit;
    diff : Diff.t;
  }

and env_commit = D.t env_internal
and env = unit env_internal

let branch (parent:env_commit) =
  {
    parent with
    parent;
    diff = Diff.empty;
    date = ();
  }
let commit (v:env) = { v with date = D.make (); }

let rec bottom_commit =
  {
    date = D.make ();
    graph = G.bottom;
    parent = bottom_commit;
    diff = Diff.empty;
  }

let bottom = branch bottom_commit

(* base operation to note modifications *)
let add_diff path (e:env) = { e with diff = Diff.add path e.diff; }


(* Joining *)
(* Common ancestor finding *)

let rec find_common_ancestor_commit d2e diff =
  (* Todo : use a binomial heap for d2e *)
  let maxd,maxe = Datem.max_binding d2e in
  let d2e = Datem.remove maxd d2e in
  if Datem.is_empty d2e
  then maxe, diff
  else
    find_common_ancestor_commit
      (Datem.add maxe.parent.date maxe.parent d2e)
      (Diff.join diff maxe.diff)

let find_common_ancestor (el:env list) =
  let parents,diff =
    List.fold_left
      (fun (parents,diff) e ->
         Datem.add e.parent.date e.parent parents,
         Diff.join e.diff diff)
      (Datem.empty,Diff.empty) el
  in
  find_common_ancestor_commit parents diff

module NeqNs = CurrySet.Nest (Eq.Node) (CurrySet.Make(G.Node))

let apply_on_path_class f p a g diff =
  let a, neclass = Eq.make_automaton p a in
  let rg = failwith "TODO" in
  let re = failwith "TODO" in
  let rec descend neq n g visited diff r_curr_path =
    if NeqNs.mem neq n visited
    then g,visited,diff
    else
      let visited = NeqNs.add neq n visited in
      let g,diff =
        if Eq.Ns.mem neq neclass
        then f n g, Diff.add (List.rev r_curr_path) diff
        else g,diff
      in
      let g,visited,diff = failwith "TODO"
        (* Eq.Node.fold_on_node *)
        (*   (fun e neq' (g,visited,diff) -> *)
        (*      let n' = G.get_child n e g in *)
        (*      descend neq' n' g visited diff (e::r_curr_path) ) *)
        (*   neq a (g,visited,diff) *)
      in
      g,visited,diff
  in
  descend (Eq.get_root a) (G.get_root g) g NeqNs.empty diff []
           





(* Environment joining *)

let join le =
  let parent, diff = find_common_ancestor le in

  let ae = Array.of_list le in
  let ag = Array.map (fun e -> e.graph) ae in

  let graph_acc = G.Join.init ag parent.graph in
    
  let route k graph_acc e =
    let graph_acc,graph =
      G.Join.route ag k graph_acc e.graph
    in
    graph_acc, { e with graph;}
  in

  let stop graph_acc e =
    let graph = G.Join.stop ag graph_acc e.graph in
    { e with graph;}
  in

  let env =
    Diff.fold
      ~route ~stop diff
      graph_acc
      (branch parent)
  in
  commit { env with diff; }

