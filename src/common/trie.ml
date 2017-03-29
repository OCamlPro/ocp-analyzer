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

module type Ord = Map.Ord

module type S =
sig
  type t
  type path
  
  val empty : t
  val is_empty : t -> bool
  val singleton : path -> t
  
  val add : path -> t -> t
  val mem : path -> t -> bool
  val remove : path -> t -> t

  val fold : (path -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    
end

module Make (O : Ord) : S with type path = O.t list =
struct

  type path = O.t list

  module Om = Map.Make (O)

  type t =
    {
      mem : bool;
      succ : t Om.t
    }

  let empty = { mem = false; succ = Om.empty; }
  let is_empty { mem; succ; } = not mem && is_empty succ

  let rec singleton = function
    | [] -> { mem = true; succ = Om.empty; }
    | h::t -> { mem = false; succ = Om.singleton h (singleton t); }
  
  let rec add l t =
    match l with
    | [] -> { mem = true; succ = t.succ }
    | hd::tl ->
      try let t' = Om.find hd t.succ in
        { t with succ = Om.add hd (add tl t') t.succ }
      with Not_found -> { t with succ = Om.add hd (singleton tl) t.succ; }

  let rec mem_unsafe l t =
    match l with
    | [] -> t.mem
    | hd::tl -> mem tl @@ Om.find hd t.succ

  let mem l t = try mem_unsafe l t with Not_found -> false

  let rec remove_unsafe l t =
    match l with
    | [] -> { t with mem = false; }
    | hd::tl ->
      let 
      let succ = Om.add hd (remove_unsafe tl @@ Om.find hd t.succ) t.succ;
      { t with
        succ = Om.add hd (remove_unsafe tl @@ Om.find hd t.succ) t.succ;
      }
  
end
