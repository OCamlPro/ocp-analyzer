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

module Node = MakeUKey (struct end)

module Nm = CurryMap.Make (Node)
module Ns = CurrySet.Make (Node)
module Ne = Puf.MakeF (Puf.Make (Node))

module NEm = CurryMap.Nest (Node) (Em)
module EmN = CurryMap.Bind (Em) (Node)

module ENs = CurrySet.Nest (Edge) (CurrySet.Make (Node))
module NENs = CurrySet.Nest (Node) (ENs)

module NmEmN =
  CurryMap.SafeFind
    (CurryMap.Bind (Nm) (EmN))
    (struct type t = EmN.t let bottom = EmN.empty end)


type eq =
  {
    root : Node.t;
    edges : Node.t NEm.t;
    back_edges : NENs.t;
    eqs : Ne.t;
  }

let bottom =
  {
    root = Node.make ();
    edges = NEm.empty;
    back_edges = NENs.empty;
    eqs = Ne.empty;
  }


(* Non-semantic operations
*)

let add_edge n e n' a =
  try
    let oldn' = NEm.find n e a.edges in
    { a with
      edges = NEm.add n e n' a.edges;
      back_edges = a.back_edges |> NENs.remove oldn' e n |> NENs.add n' e n;
    }
  with Not_found ->
    { a with
      edges = NEm.add n e n' a.edges;
      back_edges = NENs.add n' e n a.back_edges;
    }

(* n' MUST be successor for n under e *)
let remove_edge n e n' a =
  { a with
    edges = NEm.remove n e a.edges;
    back_edges = NENs.remove n' e n a.back_edges;
  }
(* The same without specifying n' *)
let remove_edge_unknown n e a =
  remove_edge n e (NEm.find n e a.edges) a

(* set two nodes to be equivalents
   after this call, n' will be out and everything about it will be in n *)
let (* rec *) link_nodes n n' a =
  { a with eqs = Ne.union n n' a.eqs; }
  (* if Node.equal n n' *)
  (* then a *)
  (* else *)
  (*   begin *)
  (*     (\* Collecting back_edges from n' *\) *)
  (*     let a = ENs.fold (fun e parent a -> *)
  (*         add_edge parent e n a) (Nm.find n' a.back_edges) a *)
  (*     in *)
  (*     (\* grouping nedges under n and n' *\) *)
  (*     let ne = NmEmN.find r a.edges *)
  (*     and ne' = NmEmN.find r' a.edges in *)
  (*     let a, to_merge = NmEmN.fold *)
  (*         (fun e next (a,to_merge) -> *)
  (*            try a, (Em.find e ne, next)::to_merge *)
  (*            with Not_fonud -> add_edge n e next a, to_merge ) *)
  (*         ne' (a,[]) *)
  (*     in *)
  (*     (\* we now have some new nodes to merge *\) *)
  (*     List.fold_left (fun a (n,n') -> link_nodes n n' a) a to_merge *)
  (*   end *)

(* descending, assuming the path exists *)
let unsafe_edge_descend n e a = (* NEm.find (repr n a) e a.edges *) failwith "TODO: unsafe edge descend"
let unsafe_path_descend path a =
  List.fold_left (fun n e -> unsafe_edge_descend n e a) a.root path

let safe_edge_descend n e a = failwith "TODO: safe edge descend"

(* create a path from a node *)
let path_create n path a =
  List.fold_left
    (fun (n,a) e ->
       let n' = Node.make () in
       n',{ a with edges = NEm.add n e n' a.edges } )
    (n,a) path

(* descending in a path, creating nodes if necessary *)
let safe_path path a =
  let rec descend n = function
    | [] -> (n,a)
    | e::tl as path ->
      try
        let n' = unsafe_edge_descend n e a in
        descend n' tl
      with Not_found -> path_create n path a
  in
  descend a.root path

(* folding over every node equivalent to path *)
let fold_path f path a acc =
  let rec descender n acc = function
    | [] -> f n acc
    | e::tl ->
      try let n' = unsafe_edge_descend n e a in
        Ne.fold (fun n' acc -> descender n' acc tl) n' a.eqs acc
      with Not_found -> acc
  in
  descender a.root acc path

(* remove edges that are under a node *)
let remove_node n a =
  try Em.fold (fun e n' a -> remove_edge n e n' a) (Nm.find n a.edges) a
  with Not_found -> a

(* semantic operations *)

(* semantical linking of paths *)
let link p p' a =
  let n, a = safe_path p a in
  let n', a = safe_path p a in
  link_nodes n n' a

(* unlinking of a path from its equivalence class *)
let unlink p a =
  let rec aux n = function
    | [] -> assert false
    (* Seriously? You are trying to unlink the whole graph,
       might as well take an empty automaton directly. *)
    | [e] -> remove_edge_unknown n e a
    | e::tl -> aux (unsafe_edge_descend n e a) tl
  in
  try aux a.root p
  with Not_found -> a

let make_automaton p a =
  (* will make a fresh automaton containing only the paths equivalent to p
     and the exit node set right,
  *)
  let rec go_up n a' =
    (* we first check if n was already added to a' *)
    if Nm.mem n a'.back_edges
    then a'
    else
      let parents = Nm.find n a.back_edges in
      ENs.fold (fun e parent a' ->
          add_edge parent e n a'
          |> go_up parent)
        parents a'
  in
  fold_path
    (fun n (a',ns) -> go_up n a', Ns.add n ns )
    p a
    ({ bottom with root = a.root; },Ns.empty)

let get_root a = a.root
let fold_on_node f n a acc =
  EmN.fold f (try Nm.find n a.edges with Not_found -> EmN.empty) acc
 
module Join =
struct
  type t = eq
  type acc = Node.t Im.t * Node.t
  type edge = Edge.t

  let init aeq eq =
    let meqn =
      snd @@
      Array.fold_left
        (fun (i,meqn) eq -> succ i, Im.add i (get_root eq) meqn)
        (0,Im.empty) aeq
    in
    meqn, get_root eq

  (* IL FAUT RETENIR L'ARC AU DESSUS DU PROCHAIN NŒUD ! *)
  
  let route aeq e (meqn,curr_node) eq =
    let meqn =
      try Im.mapi (fun i n -> unsafe_edge_descend n e aeq.(i)) meqn
      with Not_found -> assert false
    in
    let curr_node = safe_edge_descend curr_node e eq in
    (meqn,curr_node), eq

  let stop aeq (meqn, curr_node) eq =
    let eq = fold_on_node
        (fun e n eq -> remove_edge curr_node e n eq)
        curr_node eq eq
    in
    failwith "todo"
    
end
