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
open Common_types

module XIdm = Map.Make (XId)


type ap = XId.t
(* allocation point are represented by the corresponding xid *)

module AId : Id = struct
  type t = int
  let compare (a:t) (b:t) = Pervasives.compare a b
  let hash (a:t) = a
  let equal (a:t) b = a=b
  let name _ = None
  let to_string = string_of_int
  let output fd i = output_string fd (to_string i)
  let print = Format.pp_print_int

  let init = ref (-1)
  let create ?name () =
    incr init;
    !init
end

module AIdo = struct
  type t = AId.t option
  let compare (a:t) (b:t) =
    match a,b with
    | Some a, Some b -> AId.compare a b
    | None,None -> 0
    | Some _, None -> -1
    | None, Some _ -> 1
  let equal a b =
    match a, b with
    | Some a, Some b -> AId.equal a b
    | None, None -> true
    | _,_ -> false
  let print ppf = function
    | Some a -> AId.print ppf a
    | None -> Format.pp_print_char ppf 'N'
  let create ?name () = Some (AId.create ())
  let any : t = None
  let either ~any ~f = function
    | Some x -> f x
    | None -> any
  let of_aid x : t= Some x
  let to_aid : t -> AId.t = function
    | Some x -> x
    | None -> assert false
  let subset (a:t) (b:t) =
    match a,b with
    | None, None | Some _, None -> true
    | None, Some _ -> false
    | Some a, Some b -> AId.equal a b
end

type atpl = AIdo.t * ap
(* the allocation tuple *)

let of_xid xid = (AIdo.create (), xid)
let print_atpl pp (aido,xid) = Format.fprintf pp "(%a,%a)" AIdo.print aido XId.print xid

module AIdm = Map.Make(AId)

module Locs : Set.S
  with type elt = atpl
= struct

  type elt = atpl
  type t = AIdo.t XIdm.t
      
  let empty = XIdm.empty
              
  let is_empty = XIdm.is_empty

  let mem (aido,xid) s =
    try
      match XIdm.find xid s, aido with
      | None,_ -> true
      | Some a, Some b when AId.equal a b -> true
      | _, _ -> false
    with Not_found -> false

  let add (aido,xid) s =
    try
      let aido2 = XIdm.find xid s in
      if AIdo.equal aido aido2
      then s
      else XIdm.add xid AIdo.any s
    with Not_found -> XIdm.add xid aido s

  let singleton (aido,xid) = XIdm.singleton xid aido

  let remove (aido,xid) s =
    try
      let aido2 = XIdm.find xid s in
      if AIdo.equal aido aido2 || AIdo.equal aido AIdo.any
      then XIdm.remove xid s
      else s
    with Not_found -> s

  let union s1 s2 =
    XIdm.merge
      (fun _ o1 o2 ->
         match o1,o2 with
         | a,None | None,a -> a
         | Some a, Some b ->
           if AIdo.equal a b
           then o1
           else Some AIdo.any
      ) s1 s2

  let inter s1 s2 =
    XIdm.merge
      (fun _ o1 o2 ->
         match o1,o2 with
         | _,None | None,_ -> None
         | Some a, Some b when AIdo.equal a b -> o1
         | Some a, _ when AIdo.equal a AIdo.any -> o2
         | _, Some a when AIdo.equal a AIdo.any -> o1
         | _, _ -> None
      ) s1 s2

  let diff s1 s2 =
    XIdm.merge
      (fun _ o1 o2 ->
         match o1,o2 with
         | Some a1, Some a2
           when
             AIdo.equal a1 a2 ||
             AIdo.equal AIdo.any a2
           -> None
         | _, _ -> o1
      ) s1 s2

  let compare = XIdm.compare AIdo.compare
  let equal s1 s2 = XIdm.equal AIdo.equal s1 s2

  let subset s1 s2 =
    try
      XIdm.for_all (fun xid aido ->
          let aido2 = XIdm.find xid s2 in
          AIdo.subset aido aido2
        ) s1
    with Not_found -> false

  let decurry f xid aido = f (aido,xid)

  let iter f s = XIdm.iter (decurry f) s
  let fold f s acc = XIdm.fold (decurry f) s acc
  let for_all f s = XIdm.for_all (decurry f) s
  let exists f s = XIdm.exists (decurry f) s
  let filter f s = XIdm.filter (decurry f) s
  let partition f s = XIdm.partition (decurry f) s

  let cardinal s = XIdm.cardinal s
  let elements s = fold (fun e acc -> e::acc) s []

  let inv_tuple (a,b) = b,a
  let min_elt s = inv_tuple ( XIdm.min_binding s)
  let max_elt s = inv_tuple ( XIdm.max_binding s)
  let choose s = inv_tuple (XIdm.choose s)

  let split (aido,xid) s =
    let (s1,aidoo,s2) = XIdm.split xid s in
    match aidoo with
    | None -> s1,false,s2
    | Some aido2 ->
      begin
        let c = AIdo.compare aido aido2 in
        if c < 0
        then s1, false, XIdm.add xid aido2 s2
        else if c = 0
        then s1, true, s2
        else XIdm.add xid aido2 s1, false, s2
      end

  let find ((aido,xid) as x) s =
    if mem x s
    then (XIdm.find xid s, xid)
    else raise Not_found

  let print pp s = XIdm.iter (fun xid aido -> print_atpl pp (aido,xid)) s
  let print_sep f pp s =
    if XIdm.is_empty s
    then ()
    else
      begin
        let (xid,aido) = XIdm.min_binding s in
        print_atpl pp (aido,xid);
        XIdm.iter
          (fun xid aido -> f pp;print_atpl pp (aido,xid))
          (XIdm.remove xid s)
      end

  let of_list l = List.fold_left (fun s e -> add e s) empty l
      
end

module Locm : sig
  include Map.S with type key = atpl
  val fold_key : ( key -> 'a -> 'b -> 'b) -> key -> 'a t -> 'b -> 'b
  val fold_by_loc : (key -> 'b -> 'b) -> 'a t -> 'b -> 'b
end = struct

  type key = atpl
  type 'a t = 'a AIdm.t XIdm.t

  let empty = XIdm.empty
  let is_empty = XIdm.is_empty

  let mem (aido,xid) m =
    XIdm.mem xid m &&
    AIdo.either
      ~any:true
      ~f:(fun aid -> AIdm.mem aid (XIdm.find xid m)) aido

  let add (aido,xid) x m =
    let aid = AIdo.to_aid aido in
    XIdm.add
      xid
      (AIdm.add aid x
         (try XIdm.find xid m
          with Not_found -> AIdm.empty)
      ) m

  let singleton (aido,xid) x =
    let aid = AIdo.to_aid aido in
    XIdm.singleton xid (AIdm.singleton aid x) 

  let remove (aido,xid) m =
    AIdo.either
      ~any:m
      ~f:(fun aid -> try
             let a = XIdm.find xid m in
             XIdm.add xid (AIdm.remove aid a) m
           with Not_found -> m )
      aido

  let merge f m1 m2 =
    XIdm.merge
      (fun xid o1 o2 ->
         let a1 = match o1 with Some a -> a | None -> AIdm.empty in
         let a2 = match o2 with Some a -> a | None -> AIdm.empty in
         let a3 =
           AIdm.merge
             (fun aid x y -> f (AIdo.of_aid aid,xid) x y)
             a1 a2
         in
         if AIdm.is_empty a3
         then None
         else Some a3
      ) m1 m2

  let compare c m1 m2 = XIdm.compare (AIdm.compare c) m1 m2
  let equal e m1 m2 = XIdm.equal (AIdm.equal e) m1 m2

  let app_two_steps s1 s2 f m =
    s1 (fun xid a -> s2 (fun aid x -> f (AIdo.of_aid aid,xid) x) a) m

  let iter f m = app_two_steps XIdm.iter AIdm.iter f m
  let fold f m acc = app_two_steps XIdm.fold AIdm.fold f m acc
  let for_all f m = app_two_steps XIdm.for_all AIdm.for_all f m
  let exists f m = app_two_steps XIdm.exists AIdm.exists f m
  let filter f m = app_two_steps XIdm.mapi AIdm.filter f m
  let partition f m =
    fold
      (fun k x (yes,no) ->
         if f k x
         then add k x yes, no
         else yes, add k x no )
      m (empty,empty)

  let cardinal m = XIdm.fold (fun _ a acc -> acc + AIdm.cardinal a) m 0
  let bindings m = fold (fun k x l -> (k,x)::l) m []
  let getb f1 f2 m =
    let (xid,a) = f1 m in
    let (aid,x) = f2 a in
    ((AIdo.of_aid aid,xid),x)
  let min_binding m = getb XIdm.min_binding AIdm.min_binding m
  let max_binding m = getb XIdm.max_binding AIdm.max_binding m
  let choose m = getb XIdm.choose AIdm.choose m

  let split (aido,xid) m =
    let m1,o,m2 = XIdm.split xid m in
    match o with
    | None -> m1,None,m2
    | Some a ->
      AIdo.either
        ~any:(m1,None,XIdm.add xid a m2)
        ~f:(fun aid ->
            let a1,o,a2 = AIdm.split aid a in
            XIdm.add xid a1 m1, o, XIdm.add xid a2 m2)
        aido

  let find (aido,xid) m = AIdm.find (AIdo.to_aid aido) (XIdm.find xid m)

  let map f m = XIdm.map (fun a -> AIdm.map f a) m

  let mapi f m = XIdm.mapi (fun xid a -> AIdm.mapi (fun aid x -> f (AIdo.of_aid aid,xid) x) a) m

  let fold_key f (aido,xid) m acc =
    let id x = x in
    AIdo.either
      ~any:(try
              AIdm.fold
                (fun aid x acc -> f ((AIdo.of_aid aid),xid) x acc)
                (XIdm.find xid m)
            with Not_found -> id )
      ~f:(fun aid ->
          try f ((AIdo.of_aid aid),xid) (AIdm.find aid (XIdm.find xid m))
          with Not_found -> id )
      aido acc

  let print f pp m =
    XIdm.iter
      (fun xid aidm ->
         AIdm.iter
           (fun aid x ->
              Format.fprintf pp "@[%a@ ->@ %a@]"
              print_atpl (AIdo.of_aid aid, xid)
              f x
           ) aidm
      ) m
  let print_sep fsep f pp s =
    XIdm.print_sep fsep (AIdm.print_sep fsep f) pp s

  let fold_by_loc f m acc =
    XIdm.fold (fun xid _ acc ->
        f (AIdo.any,xid) acc
      ) m acc

end


module Loc = struct
  type t = atpl
  let compare (a,t) (a',t') =
    let c = AIdo.compare a a' in
    if c = 0
    then XId.compare t t'
    else c
  let print = print_atpl
end
