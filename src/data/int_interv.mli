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

type t

val bottom : t
val top : t

val is_bottom : t -> bool
val is_top : t -> bool

val leq : t -> t -> bool
val join : t -> t -> t
val join_list : t list -> t

val widening : t -> t -> t
val print : Format.formatter -> t -> unit

val mem : int -> t -> bool
val cardinal : t -> int option
val unique : t -> bool

val meet : t -> t -> t
val cst : int -> t
val at_least : int -> t
val at_most : int -> t
val addcst : int -> t -> t

val uminus : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val modulo : t -> t -> t
val band : t -> t -> t
val bor : t -> t -> t
val bxor : t -> t -> t
val blsl : t -> t -> t
val blsr : t -> t -> t
val basr : t -> t -> t

type compres = bool option

val comp : Lambda.comparison -> t -> t -> compres
val make_comp : Lambda.comparison -> t -> t -> t * t
val leqcst : t -> int -> t
val geqcst : t -> int -> t

val lower : t -> int option
val higher : t -> int option
