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

module Set =
struct
  module type S =
    sig
      type elt'
      include Set.S

      type folder = { f : 'acc. (elt -> 'acc -> 'acc) -> 'acc -> 'acc; }

      val fold2 :
        ( folder -> elt' -> 'a -> 'a ) ->
        t -> 'a -> 'a
    end

  module Make (O1 : Map.OrderedType) (O2 : Set.OrderedType) :
    S with type elt =  (O1.t * O2.t)
       and type elt' = O1.t
  =
  struct

    type elt' = O1.t
    type elt = (O1.t * O2.t)
    module M = Map.Make(O1)
    module S = Set.Make(O2)
    type t = S.t M.t

    let empty = M.empty
    let is_empty s = M.for_all (fun _ a -> S.is_empty a) s

    let mem (x1,x2) s = M.mem x1 s && S.mem x2 (M.find x1 s)

    let add (e1,e2) s =
      try M.add e1 (S.add e2 (M.find e1 s)) s
      with Not_found -> M.add e1 (S.singleton e2) s

    let singleton (e1,e2) = M.singleton e1 (S.singleton e2)

    let remove (e1,e2) s =
      try M.add e1 (S.remove e2 (M.find e1 s)) s
      with Not_found -> s

    let union s s' =
      M.merge
        (fun _ a b ->
           match a, b with
           | a, None | None, a -> a
           | Some a, Some b -> Some (S.union a b))
        s s'
    let inter s s' =
      M.merge
        (fun _ a b ->
           match a, b with
           | _, None | None, _ -> None
           | Some a, Some b -> Some (S.inter a b))
        s s'

    let diff s s' =
      M.merge
        (fun _ a b ->
           match a, b with
           | Some a, Some b -> Some (S.diff a b)
           | _, _ -> a)
        s s'

    let compare = M.compare S.compare
    let equal = M.equal S.equal


    let subset s s' = (* behaves differently ! *)
      try M.for_all (fun x1 x -> S.is_empty x || not (S.is_empty (M.find x1 s'))) s
      with Not_found -> false

    let descender d d' f s = d (fun x1 a -> d' (fun x2 -> f (x1,x2)) a ) s

    let iter f s = descender M.iter S.iter f s
    let fold f s acc = descender M.fold S.fold f s acc
    let for_all f s = descender M.for_all S.for_all f s
    let exists f s = descender M.exists S.exists f s

    let filter f s = fold (fun x acc -> if f x then add x acc else acc) s empty

    let partition f s =
      fold (fun x (yes,no) ->
          if f x
          then add x yes, no
          else yes, add x no )
        s (empty, empty)

    let cardinal s = M.fold (fun _ x acc -> S.cardinal x + acc ) s 0

    let elements s = fold (fun e acc -> e::acc) s []

    let getb f f' s = let x1,a = f s in (x1, f' a)


    let min_elt s = getb M.min_binding S.min_elt s
    let max_elt s = getb M.max_binding S.max_elt s
    let choose s = getb M.choose S.choose s

    let split (x1,x2) s =
      let (s1,ox2,s2) = M.split x1 s in
      match ox2 with
      | None -> s1,false,s2
      | Some x2s ->
        let (s21,b,s22) = S.split x2 x2s in
        (M.add x1 s21 s1, b, M.add x1 s22 s2)

    let find (x1,x2) s = (x1, S.find x2 (M.find x1 s))

    let print pp s = assert false
    let print_sep f pp s = assert false

    type folder = { f : 'acc. (elt -> 'acc -> 'acc) -> 'acc -> 'acc; }

    let fold2 f s acc =
      M.fold
        (fun x x's acc ->
           let g =
             { f =
                 fun f' acc' ->
                   S.fold (fun x' acc' -> f' ((x,x'):elt) acc') x's acc' 
             } in
           f g x acc)
        s acc
  end
end

module Map (O1 : Map.OrderedType) (O2 : Map.OrderedType) :
  Map.S with type key = (O1.t * O2.t) =
struct
  module M1 = Map.Make (O1)
  module M2 = Map.Make (O2)

  type key = (O1.t * O2.t)
  type 'a t = 'a M2.t M1.t

  let empty = M1.empty
  let is_empty = M1.is_empty

  let mem (x1,x2) m =
    M1.mem x1 m && M2.mem x2(M1.find x1 m)

  let add (x1,x2) x m =
    M1.add x1
      (M2.add x2 x
         (try M1.find x1 m
          with Not_found -> M2.empty)
      ) m

  let singleton (x1,x2) x =
    M1.singleton x1 (M2.singleton x2 x) 

  let remove (x1,x2) m =
    try M1.add x1 (M2.remove x2 (M1.find x1 m)) m
    with Not_found -> m

  let merge f m1 m2 =
    M1.merge
      (fun x1 o1 o2 ->
         let a1 = match o1 with Some a -> a | None -> M2.empty in
         let a2 = match o2 with Some a -> a | None -> M2.empty in
         let a3 =
           M2.merge
             (fun x2 x y -> f (x1,x2) x y)
             a1 a2
         in
         if M2.is_empty a3
         then None
         else Some a3
      ) m1 m2

  let compare c m1 m2 = M1.compare (M2.compare c) m1 m2
  let equal e m1 m2 = M1.equal (M2.equal e) m1 m2

  let app_two_steps s1 s2 f m =
    s1 (fun x1 a -> s2 (fun x2 x -> f (x1,x2) x) a) m

  let iter f m = app_two_steps M1.iter M2.iter f m
  let fold f m acc = app_two_steps M1.fold M2.fold f m acc
  let for_all f m = app_two_steps M1.for_all M2.for_all f m
  let exists f m = app_two_steps M1.exists M2.exists f m
  let filter f m = app_two_steps M1.mapi M2.filter f m
  let partition f m =
    fold
      (fun k x (yes,no) ->
         if f k x
         then add k x yes, no
         else yes, add k x no )
      m (empty,empty)

  let cardinal m = M1.fold (fun _ a acc -> acc + M2.cardinal a) m 0
  let bindings m = fold (fun k x l -> (k,x)::l) m []
  let getb f1 f2 m =
    let (x1,a) = f1 m in
    let (x2,x) = f2 a in
    ((x1,x2),x)
  let min_binding m = getb M1.min_binding M2.min_binding m
  let max_binding m = getb M1.max_binding M2.max_binding m
  let choose m = getb M1.choose M2.choose m

  let split (x1,x2) m =
    let m1,o,m2 = M1.split x1 m in
    match o with
    | None -> m1,None,m2
    | Some m2' ->
      let m21,o,m22 = M2.split x2 m2' in
      M1.add x1 m21 m1, o, M1.add x1 m22 m2

  let find (x1,x2) m = M2.find x2 (M1.find x1 m)

  let map f m = M1.map (fun a -> M2.map f a) m

  let mapi f m = M1.mapi (fun x1 a -> M2.mapi (fun x2 x -> f (x1,x2) x) a) m

  let print f pp m = assert false
    (* M1.iter *)
    (*   (fun x1 aidm -> *)
    (*      M2.iter *)
    (*        (fun aid x -> *)
    (*           Format.fprintf pp "@[%a@ ->@ %a@]" *)
    (*           print_atpl (AIdo.of_aid aid, x1) *)
    (*           f x *)
    (*        ) aidm *)
    (*   ) m *)
  let print_sep fsep f pp s = assert false
    (* M1.print_sep fsep (M2.print_sep fsep f) pp s *)

end
