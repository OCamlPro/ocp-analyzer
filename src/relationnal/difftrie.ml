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

module type S =
sig
  type key
  type t
  type path = key list
  
  val empty : t
 
  val add : path -> t -> t
  val join : t -> t -> t
    
  val fold :
    route:( key -> 'descender -> 'common -> 'descender * 'common ) ->
    stop:( 'descender -> 'common -> 'common ) ->
    t -> 'descender -> 'common ->
    'common
    
end

module Make ( K : Key ) : S with type key = K.t =
struct

  type key = K.t
  type path = key list

  module Km = Map.Make ( K )
  
  type t =
    | T of t Km.t
    | Stop

  let empty = T Km.empty

  let rec singleton = function
    | [] -> Stop
    | hd::tl -> T (Km.singleton hd @@ singleton tl)
  
  let rec add path t =
    match path, t with
    | [], _ | _, Stop -> Stop
    | hd::tl, T m ->
      if Km.mem hd m
      then T ( Km.add hd ( add tl @@ Km.find hd m) m )
      else T ( Km.add hd (singleton tl) m)

  let rec join t t' =
    match t, t' with
    | Stop, _ | _, Stop -> Stop
    | T m, T m' ->
      T ( Km.merge (fun _ o o' ->
          match o, o' with
          | None, o | o, None -> o
          | Some t, Some t' ->
            Some (join t t') )
          m m' )

  let rec fold ~route ~stop t descender common =
    match t with
    | Stop -> stop descender common
    | T m -> Km.fold (fun k t common ->
        let descender, common = route k descender common in
        fold ~route ~stop t descender common )
        m common
  
end

module type Folder =
sig
  type t
  type acc
  type edge
  val init : t array -> t -> acc
  val route : t array -> edge -> acc -> t -> acc * t
  val stop : t array -> acc -> t -> t
end
  
