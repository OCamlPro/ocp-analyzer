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

open Common_types
open Utils
open Positions

module Sel = struct
  type t = Name of XId.t | Adress of Pos.t
  let compare x y = match x,y with
    | Name _, Adress _ -> -1
    | Adress _, Name _ -> 1
    | Name n, Name n' -> XId.compare n n'
    | Adress a, Adress a' -> Pos.compare a a'

  let print ppf = function
    | Name t -> Format.fprintf ppf "$%a" XId.print t
    | Adress a -> Format.fprintf ppf "*%a" Pos.print a
end

module Selm = Map.Make (Sel)
module Sels = Set.Make (Sel)

(* type iset = Int_interv.t *)
(* type cpset = Cps.t *)
(* type bset = Blocks.t *)
(* type aset = Arrays.t *)
(* type cset = Constants.t *)

(* type _ t = *)
(*   | Int : iset t *)
(*   | Cp : cpset t *)
(*   | Block : bset t *)
(*   | IBlock : bset t *)
(*   | Array : aset t *)
(*   | Custom :  int -> cset t *)
(*     (\* integers, *)
(*        const pointers, *)
(*        mutable blocks, *)
(*        immutable blocks, *)
(*        arrays, *)
(*        other data structures *\) *)

(* type dbox = Data : 'a t * 'a -> dbox *)
