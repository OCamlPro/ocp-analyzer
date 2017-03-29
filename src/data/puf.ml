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

(* Persistant union-find *)

module type S =
sig
  type t
  type key
  type set

  val empty : t
  val add : key -> t -> t
  val find : key -> t -> key * t
  val union : key -> key -> t -> t
  val fold : ( key -> 'a -> 'a) -> key -> t -> 'a -> 'a * t
  val fold_classes : (set -> 'a -> 'a) -> t -> 'a -> 'a
  val join : t -> t -> t
end

module type OrderedType =
sig
  type t
  include Map.OrderedType with type t := t
  include Set.OrderedType with type t := t
end

module Make
    ( Ord : OrderedType )
    ( M : Map.S with type key = Ord.t )
    ( S : Set.S with type elt = Ord.t )
  : S with type key = Ord.t
       and type set = S.t =
struct

  type key = Ord.t
  type set = S.t

  type node =
    {
      parent : key;
      rank : int;
    }
  type t =
    { 
      forest: node M.t;
      back: S.t M.t;
    }

  let empty = { forest = M.empty; back = M.empty; }

  let mk_node k = { parent = k; rank = 0; }

  let add_unsafe k uf =
    {
      forest = M.add k (mk_node k) uf.forest;
      back = M.add k (S.singleton k) uf.back;
    }

  let add k uf =
    if M.mem k uf.forest
    then uf
    else add_unsafe k uf

  let rec find_with_rank_unsafe k uf =
    let { parent; rank } as n = M.find k uf.forest in
    if Ord.compare parent k = 0
    then n, uf
    else
      let rnode, uf = find_with_rank_unsafe parent uf in
      rnode,
      { uf with
        forest = M.add k rnode uf.forest;
      }

  let find_with_rank k uf =
    try find_with_rank_unsafe k uf
    with Not_found -> mk_node k, add_unsafe k uf

  let find k uf =
    let n, uf = find_with_rank k uf in
    n.parent, uf
    
  let uback n1p n2p ufb =
    M.add n1p (S.union (M.find n1p ufb) (M.find n2p ufb)) ufb

  let union k1 k2 uf =
    let n1, uf = find_with_rank k1 uf in
    let n2, uf = find_with_rank k2 uf in
    if Ord.compare n1.parent n2.parent = 0
    then uf
    else
      let c = compare n1.rank n2.rank in
      if c > 0
      then
        {
          forest = M.add n2.parent n1 uf.forest;
          back = uback n1.parent n2.parent uf.back;
        }
      else if c < 0
      then
        {
          forest = M.add n1.parent n2 uf.forest;
          back = uback n2.parent n1.parent uf.back;
        }
      else
        let n1 = { n1 with rank = n1.rank + 1; } in
        {
          forest = M.add n1.parent n1 (M.add n2.parent n1 uf.forest);
          back = uback n1.parent n2.parent uf.back;
        }

  let fold f k uf acc =
    let k, uf = find k uf in
    S.fold f (M.find k uf.back) acc, uf

  let join uf uf' =
    M.fold (fun k p uf' ->
        union k p.parent uf'
      ) uf.forest uf'

  let fold_classes f uf acc =
    M.fold
      (fun k n acc ->
         if k = n.parent
         then f (M.find k uf.back) acc
         else acc)
      uf.forest acc

end

module type SF =
sig
  type t
  type key
  type set

  val empty : t
  val add : key -> t -> t
  val find : key -> t -> key
  val union : key -> key -> t -> t
  val fold : ( key -> 'a -> 'a) -> key -> t -> 'a -> 'a
  val fold_classes : (set -> 'a -> 'a) -> t -> 'a -> 'a
  val join : t -> t -> t
end

(* Todo: union and join could be enhanced *)
module MakeF (E : S) : SF
  with type key = E.key
   and type set = E.set
= struct
  type t = E.t ref
  type key = E.key

  type set = E.set

  let empty = ref E.empty
  let add k uf = ref (E.add k !uf)
  let find x u =
    let (x',u') = E.find x !u in
    u := u';
    x'

  let union x y u = ref (E.union x y !u)

  let fold f x u acc =
    let acc, u' = E.fold f x !u acc in
    u := u';
    acc

  let fold_classes f uf acc = E.fold_classes f !uf acc

  let join u u' = ref @@ E.join !u !u'
end
