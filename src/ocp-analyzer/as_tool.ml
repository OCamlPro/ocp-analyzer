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

open WatcherClientAPI
open WatcherProtocol

let rec print_list oc = function
  | [] -> ()
  | str :: tail -> Printf.fprintf oc "%s\n" str; print_list oc tail

let get_cmts target =
  let req = Get_all_cmts (WatcherServerUtils.mk_absolute target) in
  let (_, res) = Sync.send_request req in
  match res with
  | No_result
  | Single _
  | Report _
  | File_info _
  | Context (_,_,_) -> assert false
  | List l -> Printf.printf "Got cmts:\n%a%!" print_list l; l
  | Server_error err ->
    Format.eprintf "Error looking up %s:@.%a@."
      target WatcherProtocol.print_error err;
    exit 2

let get_cmis target =
  let req = Get_dependencies (WatcherServerUtils.mk_absolute target) in
  let (_, res) = Sync.send_request req in
  match res with
  | No_result
  | Single _
  | Report _
  | File_info _
  | Context (_,_,_) -> assert false
  | List l -> Printf.printf "Dependencies for %s:\n%a%!" target print_list l; l
  | Server_error err ->
    Format.eprintf "Error looking up %s:@.%a@."
      target WatcherProtocol.print_error err;
    exit 2

let analyse target =
  let cmts = List.rev (get_cmts target) in
  let cmts =
    List.filter
      (fun file ->
         if Filename.check_suffix file ".cmti"
         then begin Printf.printf "Dropping cmti file %s\n%!" file; false end
         else true)
      cmts
  in
  let funs = Hashtbl.create 1024 in
  let mk_lambda cmt =
    let deps = get_cmis cmt in
    Config.load_path := List.map Filename.dirname deps;
    Mk_lambda.mk_lambda Format.err_formatter cmt
  in
  let lambdas = List.map mk_lambda cmts in
  let xlambdas =
    List.map
      (fun (lam, modname) ->
         (Mk_xlambda.lambda_to_xlambda ~modname ~funs lam, modname))
      lambdas
  in
  let out = target ^ ".xlml" in
  let oc = open_out out in
  let ppf = Format.formatter_of_out_channel oc in
  let dump_xlambda (xlam, modname) =
    Format.fprintf ppf "@[<2>module %s@ %a@]@."
      modname
      Print_xlambda.xlambda xlam
  in
  Hashtbl.iter
    (fun f xlam ->
       Format.fprintf ppf "@[<2>function %a@ %a@]@."
         Common_types.F.print f
         Print_xlambda.xlambda xlam)
    funs;
  List.iter dump_xlambda xlambdas;
  close_out oc;
  let graphs =
    List.map
      (fun (xlam, modulename) ->
         Xlambda_to_hgraph.mk_graph ~modulename funs xlam)
      xlambdas
  in
  let locs = ref [] in
  let rec iter_xlambda =
    let open Xlambda in
    function
    | Xlet x ->
      locs := (x.xe_id, (x.xe_floc, x.xe_loc)) :: !locs;
      iter_xlambda x.xe_in;
      iter_xcontrol x.xe_lam
    | Xrec x -> iter_xlambda x.xr_in
    | Xend _ -> ()
  and iter_xcontrol =
    let open Xlambda in
    function
    | Xvar _
    | Xconst _
    | Xapply (_,_)
    | Xprim (_,_)
    | Xalloc (_,_)
    | Xstaticraise (_,_)
    | Xraise _
    | Xlazyforce _
    | Xccall (_,_)
    | Xsend (_,_,_)
      -> ()
    | Xswitch (_,s) ->
      begin
        List.iter (fun (_,l) -> iter_xlambda l) s.x_consts;
        List.iter (fun (_,l) -> iter_xlambda l) s.x_blocks;
        match s.x_failaction with
        | None -> ()
        | Some l -> iter_xlambda l
      end
    | Xstaticcatch (l1,_,l2)
    | Xtrywith (l1,_,l2)
    | Xifthenelse (_,l1,l2)
    | Xwhile (l1,l2)
      ->
      iter_xlambda l1;
      iter_xlambda l2
    | Xfor (_,_,_,_,l) -> iter_xlambda l
  in
  List.iter (fun (l,_) -> iter_xlambda l) xlambdas;
  let print_xid ppf xid =
    let module XId = Common_types.XId in
    try
      match List.assoc xid !locs with
      | _, Some loc ->
        Format.fprintf ppf "%a(%a)" XId.print xid Location.print_loc loc
      | Some loc, None ->
        Format.fprintf ppf "%a(in %a)" XId.print xid Location.print_loc loc
      | None, None ->
        Format.fprintf ppf "%a" XId.print xid
    with
    | Not_found -> Format.fprintf ppf "%a" XId.print xid
  in
  (* Hack: the logic for combining graphs is currently hidden in Cmb.export
     and Cmb.import *)
  let mk_cmb (g,ftbl,vin,vout,vexn,_,_) =
    let file = Filename.temp_file "gen_graph" "" in
    Cmb.export g ftbl vin vout vexn file;
    (* Cmb.export adds .cmb extension, Cmb.import expects it... *)
    file ^ ".cmb"
  in
  let cmbs = List.map mk_cmb graphs in
  let (g,funs,inv,exnv,outv) = Cmb.import_list cmbs in
  List.iter Sys.remove cmbs;

  (* Copied from bgrapha.ml *)
  let module E =
  struct
    let inv = inv
    let outv = outv
    let exnv = exnv
    let g = g
    let funs = funs
    let mk_vertex = Xlambda_to_hgraph.Vertex.mk ~modulename:""
    let mk_hedge = Xlambda_to_hgraph.Hedge.mk
  end
  in
  let module IE =
    ApronEnv.Make( struct
      type t = Polka.strict Polka.t
      let man = Polka.manager_alloc_strict ()
    end)
  in
  let module Env = Aenv.Env(Xlambda_analysis.Stack)(IE) in
  let module Manager =
    Xlambda_analysis.M
      ( OCamlEnv.Make ( Aenv.Maker (IE) ) ( Xlambda_to_hgraph.Hedge ) )
      ( E )
  in
  let module F = Fixpoint.Fixpoint ( Xlambda_to_hgraph.T ) ( Manager ) in
  print_endline "starting the analysis";
  let result, assotiation_map =
    F.kleene_fixpoint g ( Manager.H.VertexSet.singleton inv ) in
  let exnv_output = Manager.H.VertexSet.elements
      (Manager.H.VertexMap.find exnv assotiation_map) in
  let exn_env =
    Manager.join_list exnv
      (List.map (fun v -> (Xlambda_to_hgraph.G.vertex_attrib result v).Fixpoint.v_abstract) exnv_output) in
  print_endline "analysis done";
  let dot_output = target ^ ".dot" in
  let oc = open_out dot_output in
  let ppf = Format.formatter_of_out_channel oc in
  let print_attrvertex ppf vertex attr =
    Format.fprintf ppf "%a %a"
      Xlambda_to_hgraph.T.Vertex.print vertex
      Env.print_env attr.Fixpoint.v_abstract
  in
  let module PH = Print_hgraph.Result (Xlambda_to_hgraph.T) (Manager) in
  let print_attrhedge = PH.print_attrhedge in
  Manager.H.print_dot ~print_attrvertex ~print_attrhedge ppf result;
  close_out oc;
  if Manager.is_bottom exnv exn_env
  then ()
  else
    begin
      print_endline "I found something:";
      Env.print
        ~print_xid
        Format.std_formatter
        Common_types.exn_xid
        exn_env;
      exit 1
    end

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    analyse Sys.argv.(i);
  done
