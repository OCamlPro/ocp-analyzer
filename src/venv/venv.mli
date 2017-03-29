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

(* type 'a dt = 'a DataType.t *)
open DataType
open LatticeMap

module SInt : LatticeMap with module L := Int_interv and type key = Sel.t
module SCp : LatticeMap with module L := Cps and type key = Sel.t
module SBlock : LatticeMap with module L := Blocks and type key = Sel.t
module SArray : LatticeMap with module L := Arrays and type key = Sel.t
module SConstant : LatticeMap with module L := Constants and type key = Sel.t
module SFun : LatticeMap with module L := Funs and type key = Sel.t

type 'a mkey =
  | Int : SInt.t mkey
  | Cp : SCp.t mkey
  | Block : SBlock.t mkey
  | IBlock : SBlock.t mkey
  | Array : SArray.t mkey
  | Custom :  int -> SConstant.t mkey
  | Fun : SFun.t mkey

include (Bottomised with type 'a key = 'a mkey)
(* type t *)

(* (\* base values *\) *)
(* val is_bottom : t -> bool *)

(* val bottom : t *)
(* val empty : t *)

(* (\* versionning procedures *\) *)
(* (\* val fork : t -> t *\) *)

(* val merge : t -> t -> t *)
(* val widening : t -> t -> t *)

(* (\* access procedures *\) *)

(* val get_data : Sel.t -> 'a dt -> t -> 'a *)
(* val set_data : Sel.t -> dbox -> t -> t *)
(* val join_data : Sel.t -> dbox -> t -> t *)
(* val meet_data : Sel.t -> dbox -> t -> t *)
