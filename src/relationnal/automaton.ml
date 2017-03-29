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

module type S =
sig

  type t
  
  val empty : t
  (* The empty automaton *)
  val link : path -> path -> t -> t
  (* The two paths now point to the same state *)
  val rm : path -> t -> t
  (* remove a path from its equivalence class *)

  val find : path -> t -> path list
  (* should be changed : find all equivalent paths *)

  val join : t -> t -> t
  val leq : t -> t -> bool
  val meet : t -> t -> t
  (* lattice operations *)

  module Join : DiffFolder
    with type t = t
     and type helper = unit

  module Meet : DiffFolder
    with type t = t
     and type helper = unit

  module Leq : DiffFolder
    with type t = t
     and type helper = unit


end

module Make (D : UKey) : S
=
struct

  type letter = edge

  module Node = MakeUKey (struct end)

  module Nm = CurryMap.Make (Node)
  module Ns = CurrySet.Make (Node)
  module Ne = Puf.Make (Node)

  module NEm = CurryMap.Nest (Node) (Em)
  
  type t =
    {
      root : Node.t
      edges : Node.t NEm.t;
      equiv : Ne.t;
    }
  
  let bottom =
    {
      root = Node.make ();
      edges = NEm.empty;
      equiv = Ne.empty;
    }

  let rec link_nodes n n' ( { entry; edges; equiv; } as a)=
    let r = Ne.find n equiv in
    let r' = Ne.find n' equiv in
    if r = r'
    then a
    else
      begin
        let equiv = Ne.union r r' equiv in
        let repr = Ne.find r equiv in
        try
          let re = Nm.find r edges in
          begin
            try
              let re' = Nm.find r' edges in
              (* both nodes have successors *)
              let to_merge = ref [] in
              let repre = Am.merge (fun _ a b ->
                  match a,b with
                  | None, x | x, None -> x
                  | Some n2, Some n'2 ->
                    to_merge := (n2,n'2) :: to_merge; a)
                  re re'
              in
              List.fold_left (fun a (n2,n'2) -> link_nodes n2 n'2 a)
                { entry; equiv;
                  edges =
                    edges
                    |> Nm.remove r'
                    |> Nm.remove r
                    |> Nm.add repr repre; }
                !to_merge
            with Not_found ->
              (* only r has successors *)
              if r = repr
              then { a with equiv; }
              else { entry; edges = edges |> Nm.add repr re |> Nm.remove r; equiv; }
          end
        with Not_found ->
          try
            let re' = Nm.find r' edges in
            (* only r' has successors *)
            if r' = repr
            then { a with equiv; }
            else { entry; edges = Nm.remove r' (Nm.add repr re' edges); equiv; }
          with Not_found -> (* no successors *) { a with equiv; }
      end


  let safe_generic find create add = ();
    fun x m ->
      try find x m, m
      with Not_found ->
        let v = create () in
        v, add x v m
          
  let safe_entry = safe_generic Em.find mkn Em.add

  let tu v () = v
  
  let safe_node = safe_generic Nm.find (tu Am.empty) Nm.add
  
  let safe_edge n l edges =
    try
      let e = Nm.find n edges in
      try Am.find l e, edges
      with Not_found ->
        let n' = mkn () in
        n', Nm.add n (Am.add l n' e) edges
    with Not_found ->
      let n' = mkn () in
      n', Nm.add n (Am.singleton l n') edges

  
  let safe_path (x,l) { entry; edges; equiv; } =
    let n,entry = safe_entry x entry in
    let n, edges, equiv =
      List.fold_left (fun (n, edges, equiv) letter ->
          let n, equiv = Ne.find n equiv in
          let n, edges = safe_edge n l edges in
          (n, edges, equiv)
        )
        (n,edges,equiv) l
    in
    n, { entry; edges; equiv; }
        
  let link p p' a =
    let n, a = safe_path p a in
    let n', a = safe_path p' a in
    link_nodes n n' a

  let rm (x,l) ( { entry; edges; equiv; } as a ) =
    try
      let n = Nm.find x entry in
      let n,equiv = Ne.find n equiv in
      let rec aux n equiv = function
        | [] -> { a with entry = Em.remove x entry; }
        | [letter] ->
          {
            a with equiv;
            edges = Nm.add n (Am.remove letter (Nm.find n edges)) edges;
          }
        | letter::tl ->
          let n = Am.find letter (Nm.find n edges) in
          let n, equiv = Ne.find n equiv in
          aux n equiv tl
      in
      aux n equiv l
    with Not_found -> a

  let find p a = assert false

  let join a a' = failwith "todo"
    
end
