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

module type S =
sig
  type key
  type t
  type path = key list
  
  val empty : t
 
  val add : path -> t -> t
  val join : t -> t -> t

  val add_prefix : key -> t -> t
  (* adding a prefix to all words SHOULD NOT BE USED UNKNOWINGLY *)
  
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
    | T of bool * t Km.t

  let empty = T Km.empty

  let rec singleton = function
    | [] -> T (true, Km.empty)
    | hd::tl -> T (false, Km.singleton hd @@ singleton tl)
  
  let rec add path t =
    match path, t with
    | [], T(b,m) -> if b then t else T(true,m)
    | hd::tl, T (b,m) ->
      if Km.mem hd m
      then T ( b, Km.add hd @@ add tl @@ Km.find hd m)
      else T ( b, Km.add hd (singleton tl) m)

  let rec join t t' =
    match t, t' with
    | T (b,m), T (b',m') ->
      T ( b||b',
          Km.merge (fun _ o o' ->
              match o, o' with
              | None, o | o, None -> o
              | Some t, Some t' ->
                Some (join t t') )
            m m' )

  let rec fold ~route ~stop t descender common =
    match t with
    | T (b,m) -> Km.fold (fun k t common ->
        let descender, common = route k descender common in
        fold ~route ~stop t descender common )
        m (if b then stop descender common else common)

  let add_prefix k t = T (false, Km.singleton k t)
end
