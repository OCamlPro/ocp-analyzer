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

val lambda_to_xlambda :
  modname : string ->
  funs: ( Common_types.F.t, Xlambda.xlambda ) Hashtbl.t ->
  Lambda.lambda ->
  Xlambda.xlambda

