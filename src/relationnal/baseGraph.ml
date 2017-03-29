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
module NEm = CurryMap.Nest (Node) (CurryMap.Make (Edge))
module NENs = CurrySet.Nest (Node) (CurrySet.Nest (Edge) (Ns))
module NEs = CurrySet.Nest (Node) (Es)


module NmT = CurryMap.SafeFind (CurryMap.Bind(Nm) (Terminal)) (Terminal)

type graph =
  {
    edges : Node.t NEm.t;
    back_edges : NENs.t;
    root : Node.t;
    terminals : NmT.t;
  }

let bottom =
  {
    edges = NEm.empty;
    back_edges = NENs.empty;
    root = Node.make ();
    terminals = NmT.empty;
  }

(* Non-semantic operations *)

let wrong_path () = failwith "wrong path"

let unsafe_edge_descend n k { edges; _ } = NEm.find n k edges

let get_path path g =
  try List.fold_left (fun n k -> unsafe_edge_descend n k g) g.root path
  with Not_found -> wrong_path ()

(* there is no garbage collection system as of right now *)
let add_edge n k n' g =
  { g with
    edges = NEm.add n k n' g.edges;
    back_edges = NENs.add n' k n @@
      try NENs.remove (NEm.find n k g.edges) k n g.back_edges
      with Not_found -> g.back_edges;
  }

let remove_edge n k n' g =
  { g with
    edges = NEm.remove n k g.edges;
    back_edges = NENs.remove n' k n g.back_edges;
  }

(* n must have no children nor terminals,
   its parents will be redirected to n'
   and it will be removed *)
let insert_node n n' g =
  Em.fold (fun k s g ->
      Ns.fold
        (fun parent g -> add_edge parent k n' g)
        s g )
    (Nm.find n g.back_edges) g

(* cut everything under a node and remove its terminals *)
let cut_at_node n g =
  Em.fold
    (fun k n' g -> remove_edge n k n' g)
    (try Nm.find n g.edges with Not_found -> Em.empty)
    { g with terminals = NmT.remove n g.terminals; }

(* Module tool to remember for each env the current node in its graph *)
module ImN =
struct
  include CurryMap.Bind (Im) (Node)
  let compare = Im.compare Node.compare
  let print ppf m = Im.print Node.print ppf m
end

(* Used to detect potential cycles *)
module ImNm = Map.Make (ImN)


(* Atomic descender semantically true
   (unfolds cycles and creates non-existent nodes) *)
let safe_edge_descend n k g =
  try
    let next_node = NEm.find n k g.edges in
    if Ns.equal (NEm.find next_node k g.back_edges) (Ns.singleton n)
    then next_node, g
    else
      let fresh_node = Node.make () in
      let under_next =
        try Nm.find next_node g.edges
        with Not_found -> Em.empty
      in
      fresh_node,
      { g with
        terminals =
          Nm.add fresh_node
            (NmT.find next_node g.terminals)
            g.terminals;
      }
      |> add_edge n k fresh_node
      |> Em.fold (add_edge fresh_node) under_next
  with Not_found -> let fresh_node = Node.make () in
    fresh_node, add_edge n k fresh_node g

(* Base function for simple semantic accessors *)
let at_path f path graph =
  let n, g =
    List.fold_left
      (fun (n,g) k -> safe_edge_descend n k g) (graph.root,graph) path
  in
  f n g


(* semantic function, add a tree under a path
   back edges must be directly under its parents
   former values under that path will be removed
 *)
let add_tree path tree graph =
  let rec aux names graph curr_node = function
    | Back (k, i) ->
      let n = Im.find i names in
      names, add_edge curr_node k n graph
    | Node (l,o,term) ->
      let names =
        match o with
        | None -> names
        | Some i -> Im.add i curr_node names
      in
      let graph =
        { graph with
          terminals =
            (match term with
             | None -> NmT.remove curr_node graph.terminals
             | Some term -> NmT.add curr_node term graph.terminals);
        }
      in
      List.fold_left (fun (names,graph) (k,t) ->
          let n = Node.make () in
          let graph = add_edge curr_node k n graph in
          aux names graph n t)
        (names,graph) l
  in
  at_path (fun n graph ->
      let graph = cut_at_node n graph in
      snd @@ aux Im.empty graph n tree
    ) path graph


(* tree under path will be copied under path'@[k], modifying path' *)
let copy path k path' graph =
  let n = get_path path graph in
  at_path (fun n' g -> add_edge n' k n g) path' graph

(* cut the tree starting from path and remove the terminal *)
let cut path graph =
  at_path cut_at_node path graph

let get_root g = g.root

let fold_on_node f n g acc =
  Em.fold f (try Nm.find n g.edges with Not_found -> Em.empty) acc

let get_edges_under n g edges =
  try Em.fold
        (fun k _ edges -> Es.add k edges)
        (Nm.find n g.edges)
        edges
  with Not_found -> edges

let get_terminal path graph =
  NmT.find (get_path path graph) graph.terminals

let get_child = unsafe_edge_descend

module Join =
struct
  type t = graph
  type acc = ImN.t * Node.t
  type helper = graph array

  let init ag g =
    let mgn =
      snd @@
      Array.fold_left
        (fun (i,mgn) g -> succ i, ImN.add i (get_root g) mgn)
        (0,ImN.empty) ag
    in
    mgn, get_root g


  let route ag k (mgn,curr_node) g =
    (* We are descending in the graphs
       knowing that this part is semantically common *)
    let mgn =
      try ImN.mapi (fun i n -> unsafe_edge_descend n k ag.(i)) mgn
      with Not_found -> assert false
    in

    let curr_node, g = safe_edge_descend curr_node k g in
    (mgn,curr_node), g

  let stop ag (mgn,curr_node) g =
    (* We are now at the divergent point
       what's under curr_node in e.graph doesn't count anymore,
       we want to replace it by what's under respective ln in le.graph *)

    (* First we'll cut out everything under curr_node then we'll descend
       through the different graphs until there is nothing left. *)
    let g = cut_at_node curr_node g in

    (* Now, we need a descending function
       that will add a fusion of the graphs to g *)
    let rec add_join mgn curr_node visited g =
      try insert_node curr_node (ImNm.find mgn visited) g
      with Not_found ->

        let edges =
          ImN.fold
            (fun i n edges -> get_edges_under n ag.(i) edges)
            mgn Es.empty
        in

        let visited = ImNm.add mgn curr_node visited in

        let g =
          { g with
            terminals =
              Nm.add curr_node
                (
                  ImN.fold
                    (fun i n t ->
                       Terminal.join t @@
                       NmT.find n ag.(i).terminals)
                    mgn Terminal.bottom
                )
                g.terminals;
          }
        in

        Es.fold (fun k graph ->
            let newn = Node.make () in
            let graph = add_edge curr_node k newn graph in
            let mgn =
              ImN.fold (fun i n mgn ->
                  try ImN.add i (unsafe_edge_descend n k ag.(i)) mgn
                  with Not_found -> mgn)
                mgn ImN.empty
            in
            add_join mgn newn visited graph
          ) edges g
    in

    add_join mgn curr_node ImNm.empty g

end
