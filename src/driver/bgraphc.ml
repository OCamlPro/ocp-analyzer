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


let handle_file ppf sourcefile =
  let outputprefix = Filename.chop_extension sourcefile in

  let funs : ( F.t, Xlambda.xlambda ) Hashtbl.t =
    Hashtbl.create 1024 in

  let lambda, modulename = Mk_lambda.mk_lambda ppf sourcefile in

  let xlambda =
    Mk_xlambda.lambda_to_xlambda
      ~modname:modulename ~funs
      lambda
  in

  let (g,funtbl,vin,vout,vexn,exn_id,return_id) =
    Xlambda_to_hgraph.mk_graph ~modulename funs xlambda
  in

  Cmb.export g funtbl vin vout vexn outputprefix

let arg_parser =
  let open Arg in
  [
    ( "-open",
      String Mk_lambda.open_module,
      "Add an implicitly opened module" )
  ]

let () =

  let ppf = Format.std_formatter in

  Arg.parse arg_parser (handle_file ppf)
    "please specify your .ml or .cmt files, order matters";

