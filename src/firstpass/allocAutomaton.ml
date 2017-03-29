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

module Node = Utils.MakeUKey (struct end)
module Edge = BaseTypes.Edge

module Nm = CurryMap.Make (Node)
module Em = CurryMap.Make (Edge)
module Ns = CurrySet.Make (Node)

module NEm = CurryMap.Nest (Node) (Em)
module ENs = CurrySet.Nest (Edges) (Ns)
module NENs = CurrySet.Nest (Node) (ENs)

type t =
  {
    edges : Node.t NEm.t;
    rev_edges = NENs.t;
  }

let root = Node.make ()
let empty = { edges = NEm.empty; }

let n_rm_edge n e n' a =
  { a with
    edges = NEm.remove n e a.edges;
    rev_edges = NENs.remove n' e n a.rev_edges;
  }

let n_add_edge_unsafe n e n' a =
  { a with
    edges = NEm.add n e n' a.edges;
    rev_edges = NENs.add n' e n a.rev_edges;
  }

let rec n_fusion n n' a =
  if n = n'
  then a
  else
    begin
      let nr = try Nm.find n a.rev_edges with Not_found -> ENs.empty
      and n'r = try Nm.find n Nm.find n' a.rev_edges with Not_found -> ENs.empty in
      let rev_edges =
        a.rev_edges
        |> Nm.remove n'
        |> Nm.add n (ENs.union nr n'r)
      in
      let edges = ENs.fold (fun e n'' edges -> NEm.add n'' e n edges) n'r a.edges in
      try
        let n_ = Nm.find n a.edges in
        begin
          try
            let n'_ = Nm.find n' a.edges in
            (* both nodes have successors *)
            let to_merge = ref [] in
            let nn_ = Em.merge (fun _ a b ->
                match a,b with
                | None, x | x, None -> x
                | Some n2, Some n'2 ->
                  to_merge := (n2,n'2)::to_merge; a)
                n_ n'_
            in
            let a =
              { a with
                edges =
                  edges
                  |> Nm.remove n'
                  |> Nm.add n nn_;
                rev_edges;
              }
            in
            List.fold_left (fun a (n2,n'2) -> n_fusion n2 n'2 a) !to_merge
          with Not_found -> (* n' has no successors *)
            { a with edges; rev_edges; }
        end
      with Not_found -> (* n has no successors *)
        try
          let n'_ = Nm.find n' a.edges in
          let edges = edges |> Nm.remove n' |> Nm.add n n'_ in
          { a with rev_edges; edges; }
        with Not_found -> (* no successors at all *)
          { a with edges; rev_edges; }
    end

let n_follow_edge_unsafe n e a = NEm.find n e a.edges

let n_follow_edge n e a =
  try a, n_follow_edge_unsafe n e a
  with Not_found ->
    let n' = Node.make () in
    let a = n_add_edge_unsafe n e n' a in
    a, n'

let rec n_add_path_unsafe n p a =
  match p with
  | [] -> a, n
  | hd :: tl ->
    let n' = Node.make () in
    let a = n_add_edge_unsafe n hd n' a in
    n_add_path_unsafe n' tl a

let rec n_follow_path_adding n p a =
  match p with
  | [] -> a, n
  | hd :: tl ->
    try n_follow_path_adding (NEm.find a hd a.edges) tl a
    with Not_found -> n_add_path_unsafe n p a
  
let link p p' a =
  let a,n = n_follow_path_adding root p a in
  let a,n' = n_follow_path_adding root p' a in
  n_fusion n n' a

open BaseTypes

let link_vars x y a = link [Varname x] [Varname y] a

let link_field_with_test test x i y a =
let x_ = n_follow_edge_unsafe root (Varname x) a in
  let xts = Em.fold (fun e n ns ->
      match e with
      | Allocation (XPmakeblock (t,m)) when test t m ->
        n::ns
      | _ -> ns) (Nm.find x_ a.edges) []
  in
  let a,y_ = n_follow_edge root (Varname y) a in
  List.fold_left (fun a n ->
      let a,n_ = n_follow_edge n (Field i) a in
      n_fusion y_ n_ a) a xts
  

let link_field ?(mut:false) x i y a =
  if mut
  then link_field_with_test (fun _ m -> m) x i y a
  else  link_field_with_test (fun _ _ -> true) x i y a
  
let link_tag_field ?(mut:false) x t i y a =
  if mut
  then link_field_with_test (fun t' m -> t = t' && m) x i y a
  else link_field_with_test (fun t' _ -> t = t') x i y a

let duprecord x trr size block a =
  match trr with
  | Types.Record_regular | Record_float ->
    begin
      
    end
  (* | Record_inlined i -> failwith "4.01" *)
  (* | Record_extension -> failwith "4.01" *)
