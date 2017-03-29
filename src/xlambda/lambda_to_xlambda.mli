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

val lambda_to_xlambda : int -> Lambda.lambda -> int * ( Common_types.F.t, Xlambda.xlambda ) Hashtbl.t * Xlambda.xlambda
(* takes as input : the last id stamp used and some lambda code *)
