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

let ppf = Common_types.ppf

open My_main_args

let () =
  Arg.parse arg_parser (handle_file ppf)
    "Make sure that all your files are in the right order";
  if not !only_compile
  then
    begin
      let t = get_targets () in
      let (g,funs,inv,exnv,outv) = Cmb.import_list t in
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
      let module Manager = Xlambda_analysis.M (OCamlEnv.Make(OldEnv.E)(Xlambda_to_hgraph.Hedge)) ( E ) in
      begin
        match !dot_total_bigraph with
        | None -> ()
        | Some file ->
          let oc = open_out ( file ^ ".dot" ) in
          let ppf = Format.formatter_of_out_channel oc in
          let open Print_hgraph.Base in
          Xlambda_to_hgraph.G.print_dot
            ~print_attrvertex
            ~print_attrhedge
            ppf g;
          close_out oc
      end;
      let module F = Fixpoint.Fixpoint ( Xlambda_to_hgraph.T ) ( Manager ) in
      print_endline "starting the analysis";
      let result, assotiation_map =
        F.kleene_fixpoint g ( Manager.H.VertexSet.singleton inv ) in
      let outv_output = Manager.H.VertexSet.elements
          (Manager.H.VertexMap.find outv assotiation_map) in
      let outv_env =
        Manager.join_list outv
          (List.map (fun v -> (Xlambda_to_hgraph.G.vertex_attrib result v).Fixpoint.v_abstract) outv_output) in
      let exnv_output = Manager.H.VertexSet.elements
          (Manager.H.VertexMap.find exnv assotiation_map) in
      let exn_env =
        Manager.join_list exnv
          (List.map (fun v -> (Xlambda_to_hgraph.G.vertex_attrib result v).Fixpoint.v_abstract) exnv_output) in
      if !count_apply
      then Format.fprintf ppf "Pass count: %d@." (Xlambda_analysis.get_counter ());
      begin match !dot_file with
        | None -> ()
        | Some file ->
          let oc = open_out (file ^ ".dot") in
          let ppf = Format.formatter_of_out_channel oc in
          let module P = Print_hgraph.Result (Xlambda_to_hgraph.T) (Manager) in
          let open P in
          let show_hedge, show_vertex =
            if !show_unreachable
            then (fun _ _-> true), (fun _ _ -> true)
            else show_hedge, show_vertex in
          Manager.H.print_dot
            ~print_attrvertex
            ~print_attrhedge
            ~vertex_subgraph
            ~hedge_subgraph
            ~show_hedge
            ~show_vertex
            ppf result;
          close_out oc
      end;
      Format.fprintf ppf "=====@.Main exit env:@.%a@."
        Print_data.print_env outv_env;
      if Envs.is_bottom exn_env
      then ()
      else
        begin
          Format.fprintf ppf "====@.I found something:@.%a@."
            Print_data.print_env exn_env;
          Print_data.print
            ppf
            Common_types.exn_xid
            exn_env;
          exit 1
        end

    end
  else
    begin
      match !dot_total_bigraph, !dot_file with
      | None, None -> ()
      | _, _ ->
        prerr_endline
          "Can't export to dot file without going through merging and analysis";
        exit 12
    end
