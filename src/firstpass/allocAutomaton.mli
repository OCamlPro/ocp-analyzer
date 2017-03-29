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

module Edge = BaseTypes.Edge

type t

val empty : t
  
val link : Edge.t list -> Edge.t list -> t -> t
val link_vars : xid -> xid -> t -> t
(* x <- y *)
val link_field : ?mut:bool -> xid -> int -> xid -> t -> t
(* x.i <- y *)
val link_tag_field : ?mut:bool -> xid -> int -> int -> xid -> t -> t
(* match x with | C_i.j <- y *)

val duprecord : xid -> Types.record_representation ->  int -> xid -> t -> t
