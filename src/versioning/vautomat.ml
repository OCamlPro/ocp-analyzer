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

module type Alphabet = Versioning.Key

module type S =
sig

  type t
  type commit
  type entry
  type letter

  type path = entry * letter list
  
  val bottom : t
  (* The empty automaton *)
  val is_bottom : t -> bool
  (* emptyness test *)
    
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

end

module Make (E : Versioning.Key) ( A : Alphabet ) (D : UKey) (Node : Ukey)
  : S with type letter = A.t
       and type entry = E.t
=
struct

  type letter = A.t
  type entry = E.t

  module Em = Map.Make (E)
  module Es = Set.Make (E)
  module Am = Map.Make (A)

  type node = Node.t

  module Nm = Map.Make (Node)
  module Ns = Set.Make (Node)
  module Ne = Puf.Make (Node) (Nm) (Ns)

  type path = entry * letter list

  type commit =
    {
      date : D.t;
      parent : commit;
      ndiff : Ns.t;
      ediff : Es.t;
      entry : node Em.t;
      edges : node Am.t Nm.t;
      equiv : Ne.t;
    }

  type t =
    {
      parent : commit;
      ndiff : Ns.t;
      ediff : Es.t;
      entry : node Em.t;
      edges : node Am.t Nm.t;
      equiv : Ne.t;
    }

  let rec bottom_commit =
    {
      date = D.make ();
      parent = bottom_commit;
      ndiff = Ns.empty;
      ediff = Es.empty;
      entry = Em.empty;
      edges = Nm.empty;
      equiv = Ne.empty;
    }

  let commit ({parent; ndiff; ediff; entry; edges; equiv;}:t) =
    {
      date = D.make ();
      parent; ndiff; ediff; entry; edges; equiv;
    }

  let branch ({entry;edges;equiv;_} as parent:commit) =
    {
      parrent;
      ndiff = Ns.empty; ediff = Es.empty;
      entry; edges; equiv;
    }
  
  let bottom = branch bottom_commit
  let is_bottom ({ entry; _}:t) = Em.is_empty entry (* TODO :fix *)
  
  let rec link_nodes n n' ( { edges; equiv; ndiff; } as a:t ) =
    let r = Ne.find n equiv in
    let r' = Ne.find n' equiv in
    if r = r'
    then a
    else
      begin
        let ndiff = Ns.add n @@ Ns.add n' ndiff in
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
                { a with equiv; ndiff;
                  edges =
                    edges
                    |> Nm.remove r'
                    |> Nm.remove r
                    |> Nm.add repr repre; }
                !to_merge
            with Not_found ->
              (* only r has successors *)
              if r = repr
              then { a with equiv; ndiff; }
              else
                { a with
                  ndiff; equiv;
                  edges = edges |> Nm.add repr re |> Nm.remove r;
                }
          end
        with Not_found ->
          try
            let re' = Nm.find r' edges in
            (* only r' has successors *)
            if r' = repr
            then { a with equiv; ndiff; }
            else
              { a with
                equiv; ndiff;
                edges = Nm.remove r' (Nm.add repr re' edges);
              }
          with Not_found -> (* no successors *) { a with equiv; ndiff; }
      end

  let safe_path (x,l) a =
    let n, a =
      try Em.find x a.entry, a
      with Not_found ->
        let n = Node.make () in
        n,
        { a with
          entry = Em.add x a.entry;
          ndiff = Ns.add n a.ndiff;
          ediff = Es.add x a.ediff;
        }
    in
    List.fold_left (fun (n,a) letter ->
        let n = Ne.find n a.equiv in
        try Nm.find n a.edges |> Am.find letter, a
        with Not_found ->
          let n' = Node.make () in
          n', { a with
                edges =
                  Nm.add n (try Am.add letter n' @@ Nm.find n a.edges
                            with Not_found -> Am.singleton letter n')
                    a.edges;
                ndiff = Ns.add n' a.ndiff;
              } )
      (n,a) l

  let link p p' a =
    let n, a = safe_path p a in
    let n', a = safe_path p' a in
    link_nodes n n' a

  let rm (x,l) a =
    try
      let n = Em.find x a.entry in
      let n = Ne.find n a.equiv in
      let ediff = Es.add x a.ediff in
      let rec aux n equiv = function
        | [] ->
          { a with
            entry = Em.remove x a.entry;
            ediff;
          }
        | [letter] ->
          let letters = Nm.find n a.edges in
          let n' = Am.find letter letters in
          let letters = Am.remove letter letters in
          {
            a with
            equiv; ediff;
            edges = Nm.add n letters a.edges;
            ndiff = Ns.add n' a.ndiff;
          }
        | letter::tl ->
          let n = Nm.find n edges |> Am.find letter in
          let n = Ne.find n equiv in
          aux n equiv tl
      in aux n a.equiv l
    with Not_found -> a

  let rec find_common_ancestor a b ndiff ediff =
    let c = Date.compare a.date b.date in
    if c = 0 then a, ndiff, ediff
    else if c < 0
    then find_common_ancestor a b.parent
        (Ns.union b.ndiff ndiff) (Es.union b.ediff ediff)
    else find_common_ancestor a.parent b
        (Ns.union a.ndiff ndiff) (Es.union a.ediff ediff)
  
  let join a b =
    let old, ndiff, ediff =
      find_common_ancestor a.parent b.parent
        (Ns.union a.ndiff b.ndiff) (Es.union a.ediff b.ediff)
    in
    
  
end
