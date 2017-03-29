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

exception Bad_file
(* exception No_implementation *)

let () =
  Clflags.nopervasives := true;
  Clflags.no_std_include := true


let ml_file ppf sourcefile outputprefix =
  Location.input_name := sourcefile;
  Compmisc.init_path true;
  let modulename =
    String.capitalize(Filename.basename(Misc.chop_extension_if_any sourcefile)) in
  (* check_unit_name ppf sourcefile modulename; *)
  Env.set_unit_name modulename;
  let inputfile = Pparse.preprocess sourcefile in
  let env = Compmisc.initial_env() in
  Compilenv.reset ?packname:!Clflags.for_package modulename;
  let lam =
    Pparse.file ppf ~tool_name:"ocp-analyser" inputfile Parse.implementation Config.ast_impl_magic_number
    |> Typemod.type_implementation sourcefile outputprefix modulename env
    |> Translmod.transl_implementation modulename
  in
  Pparse.remove_preprocessed inputfile;
  ( lam, modulename )

let cmt_file ppf sourcefile outputprefix =
  let open Cmt_format in
  let cmtf = read_cmt sourcefile in
  let modulename = cmtf.cmt_modname in
  let cmtf =
    try
      Cmt_specifics.restore_cmt_env cmtf
    with e ->
      Printf.eprintf "Error while restoring env for file %s\n%!" sourcefile;
      raise e
  in
  match cmtf.cmt_annots with
  | Implementation s ->
    ( Translmod.transl_implementation
        modulename
        (s,Typedtree.Tcoerce_none) (* Should be changed someday *)
    , modulename )
  | _ -> failwith (Printf.sprintf "Bad cmt file: %s" sourcefile)

let mli_file ppf sourcefile outputprefix =
  Optcompile.interface ppf sourcefile outputprefix

let cmti_file _ _ _ = failwith ".cmti not implemented, please directly compile .mli"

let open_module s =
  Compenv.implicit_modules := 
    s :: !Compenv.implicit_modules

let close_module s =
  let rec aux res = function
    | [] -> ()
    | h :: t when h = s -> Compenv.implicit_modules := List.rev_append res t
    | h :: t -> aux (h::res) t
  in
  aux [] !Compenv.implicit_modules

let mk_lambda ppf sourcefile =
  let outputprefix = Filename.chop_extension sourcefile in
  let c = Filename.check_suffix sourcefile in
  if c ".cmt"
  then cmt_file ppf sourcefile outputprefix
  else if c ".ml"
  then ml_file ppf sourcefile outputprefix
  else raise Bad_file


let mk_lambdas ppf ( files : string array ) =
  Array.map ( mk_lambda ppf ) files
