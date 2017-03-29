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

module type Lattice = Lattice.WidenableLattice

module type LatticeMap = sig

  type t
  type key
    
  include Lattice with type t := t

  module L : Lattice

  type elt = L.t
  
  val get : key -> t -> elt
  val set : key -> elt -> t -> t
  
end

module type LatticeSelMap = LatticeMap with type key = DataType.Sel.t

module MakeMap (M : Map.S) (La : Lattice ) : LatticeMap
  with module L := La
   and type key = M.key
=
struct
  
  type key = M.key
  type t = La.t M.t
  type elt = La.t
               
  module L = La

  let join m m' =
    M.merge (fun _ x x' ->
        match x,x' with
        | None, a | a, None -> a
        | Some x, Some x' ->
          Some (L.join x x') )
      m m'

  let meet m m' =
    M.merge (fun _ x x' ->
        match x,x' with
        | None, _ | _, None -> None
        | Some x, Some x' ->
          let r = L.meet x x' in
          if L.is_bottom r
          then None else Some r)
      m m'

  let widening m m' =
    M.merge (fun _ x x' ->
        match x,x' with
        | None, a | a, None -> a
        | Some x, Some x' ->
          Some (L.widening x x') )
      m m'

  let leq m m' =
    try M.for_all (fun k x -> L.leq x (M.find k m')) m
    with Not_found -> false

  let equal m m' =
    try let _ = M.merge (fun _ a b ->
        match a, b with
        | None, _| _, None -> raise Not_found
        | Some x, Some y -> if L.equal x y then None else raise Not_found )
        m m'
      in true
    with Not_found -> false

  let is_bottom m = M.for_all (fun _ x -> L.is_bottom x) m
  let bottom = M.empty

  let get k m = try M.find k m with Not_found -> L.bottom
  let set = M.add

end

module Intm =
  Map.Make (
  struct
    type t = int
    let compare = compare
    let print = Format.pp_print_int
  end)

module type LSMaps = sig
  include Lattice

  type 'a key

  val get : 'a key -> t -> 'a
  val set : 'a key -> 'a -> t -> t
  
end

module type Bottomised = sig
  include LSMaps
  val empty : t
end

module Bottomise (L : LSMaps) :
  Bottomised with type 'a key = 'a L.key
= struct
  type t = L.t option
  type 'a key = 'a L.key

  let join x y =
    match x,y with
    | None, a | a, None -> a
    | Some x, Some y -> Some (L.join x y)

  let widening x y =
    match x,y with
    | None, a | a, None -> a
    | Some x, Some y -> Some (L.widening x y)
    
  let meet x y =
    match x,y with
    | None, _ | _, None -> None
    | Some x, Some y -> Some (L.meet x y)

  let leq x y =
    match x,y with
    | None, _ -> true
    | _, None -> false
    | Some x, Some y -> L.leq x y

  let equal x y =
    match x,y with
    | None, None -> true
    | Some x, Some y -> L.equal x y
    | _, _ -> false
  
  let bottom = None
  let empty = Some L.bottom
  let is_bottom = function None -> true | Some _ -> false

  let get k = function
    | None -> failwith "attempting to access bottom environment"
    | Some x -> L.get k x

  let set k v = function
    | None -> failwith "attempting to access bottom environment"
    | Some x -> Some ( L.set k v x )


end
