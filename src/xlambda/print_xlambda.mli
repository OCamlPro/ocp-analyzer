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


val xlambda : Format.formatter -> Xlambda.xlambda -> unit

val xcontrol : Format.formatter -> Xlambda.xcontrol -> unit

val primitive : Format.formatter -> Common_types.primitive -> unit

val allocator : Format.formatter -> Common_types.allocator -> unit
