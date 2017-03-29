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

module type S = sig

  type t
  type key
  type elt

  include LatticeMap.LatticeMap
    with type t := t
     and type key := key
     and type elt := elt
  
end

module Make (K : Key) (E : Lattice) (D : UKey) =
struct

  type key = K.t
  type elt = E.t

  module L = E
  
  module M = Map.Make (K)
  module Diff = Set.Make (K)
  

  type commit =
    {
      date : D.t;
      content : elt M.t;
      parent : commit;
      diff : Diff.t;
    }

  type t =
    {
      content : elt M.t;
      parent : commit;
      diff : Diff.t;
    }

  let branch (commit:commit)  =
    {
      content = commit.content;
      parent = commit;
      diff = Diff.empty;
    }

  let commit ({content; parent; diff;}:t) =
    {
      date = D.make ();
      content; parent; diff;
    }

  
  let rec bottom_commit =
    {
      date = D.make ();
      content = M.empty;
      parent = bottom_commit;
      diff = Diff.empty;
    }
  let bottom = branch bottom_commit
  let is_bottom ({ content; _}:t) = M.for_all (fun _ x -> E.is_bottom x) content

  let get_content k c = try M.find k content with Not_found -> E.bottom
  
  let get k ({ content; _ }:t) =  get_content k content
  let set k v ({ content; diff; }  as (m:t)) =
    {
      m with
      content = M.add k v content;
      diff = Diff.add k diff;
    }

  let rec is_ancestor a b =
    let c = D.compare a.date b.date in
    c = 0 || c < 0 && is_ancestor a b.parent
  
  let rec find_common_ancestor_commit a b diff =
    let c = D.compare a.date b.date in
    if c = 0
    then a, diff
    else if c < 0
    then find_common_ancestor_commit a b.parent @@ Diff.union diff b.diff
    else find_common_ancestor_commit a.parent b @@ Diff.union diff a.diff

  let find_common_ancestor a b = find_common_ancestor_commit a.parent b.parent @@ Diff.union a.diff b.diff
  
  let union_template f a b =
    let parent, diff = find_common_ancestor a b in
    let date = D.make () in
    let content = Diff.fold
        (fun k content ->
           M.add k (f (get_content k a.content) (get_content k b.content)) content)
        diff parent.content in
    {
      date; parent; content;
      history =
        Diff.fold (fun k diff ->
            if E.equal (M.find k content) (get_content k parent.content)
            then remove k diff
            else diff
          ) diff diff;
    }

    let join = union_template E.join
    let meet = union_template E.join
    let widening = union_template E.widening
    
    let leq a b =
      let old,diff = find_common_ancestor a b in
      let leqdiff k = E.leq (get_content k a.content) (get_content k b.content) in
      Dm.forall leqdiff diff

end
