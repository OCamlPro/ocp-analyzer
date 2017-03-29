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

(* Abbreviations *)
(* module WA = WatcherClientAPI *)
(* module WP = WatcherProtocol *)
(* module WU = WatcherServerUtils *)
(* Need module aliases for the following abbreviations *)
(* module XH = Xlambda_to_hgraph *)
(* module Fix = Fixpoint *)

let rec print_list oc = function
  | [] -> ()
  | str :: tail -> Printf.fprintf oc "%s\n" str; print_list oc tail

(* let get_cmts target =
  let req = WP.Get_all_cmts (WU.mk_absolute target) in
  let (_, res) = WA.Sync.send_request req in
  match res with
  | WP.No_result
  | WP.Single _
  | WP.Report _
  | WP.File_info _
  | WP.Context (_,_,_) -> assert false
  | WP.List l -> Printf.printf "Got cmts:\n%a%!" print_list l; l
  | WP.Server_error err ->
    Format.eprintf "Error looking up %s:@.%a@."
      target WP.print_error err;
    exit 2 *)

(* let get_cmis target =
  let req = WP.Get_dependencies (WU.mk_absolute target) in
  let (_, res) = WA.Sync.send_request req in
  match res with
  | WP.No_result
  | WP.Single _
  | WP.Report _
  | WP.File_info _
  | WP.Context (_,_,_) -> assert false
  | WP.List l -> Printf.printf "Dependencies for %s:\n%a%!" target print_list l; l
  | WP.Server_error err ->
    Format.eprintf "Error looking up %s:@.%a@."
      target WP.print_error err;
    exit 2 *)

let analyse ?(target="analyzer_output") int_domain cmts =
  let funs = Hashtbl.create 1024 in
  let mk_lambda cmt =
    (* let deps = get_cmis cmt in *)
    (* Config.load_path := List.map Filename.dirname deps; *)
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
  let raise_locs = ref [] in
  let rec iter_xlambda =
    let open Xlambda in
    function
    | Xlet x ->
       begin
         locs := (x.xe_id, (x.xe_floc, x.xe_loc)) :: !locs;
         iter_xlambda x.xe_in;
         iter_xcontrol x.xe_lam;
         match x.xe_lam with
         | Xraise (_,y) -> raise_locs := (y, x.xe_loc) :: !raise_locs
         | _ -> ()
       end
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
    | Xstringswitch (_,ll,lopt) ->
      begin
        List.iter (fun (_,l) -> iter_xlambda l) ll;
        match lopt with
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
  Hashtbl.iter (fun _ xlam -> iter_xlambda xlam) funs;
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
  let module IE = (val int_domain : IEsig.IE) in
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
  let fvertexstyle v =
    match (Xlambda_to_hgraph.G.vertex_attrib result v).Fixpoint.v_orig with
    | Xlambda_to_hgraph.Normal -> "shape=box,fontsize=12"
    | Xlambda_to_hgraph.Exception -> "shape=box,fontsize=12,color=red"
  in
  Manager.H.print_dot ~fvertexstyle ~print_attrvertex ~print_attrhedge ppf result;
  close_out oc;

  let module HS = Xlambda_to_hgraph.G.HedgeSet in
  let module VS = Xlambda_to_hgraph.G.VertexSet in

  (* Find, from a given node on an exception path, which hedges started the
     transition from normal to exception, i.e. raised the exception *)
  let rec find_raise_points_rec visited exnout acc =
    let visited = VS.add exnout visited in
    if Manager.is_bottom exnout (Xlambda_to_hgraph.G.vertex_attrib result exnout).Fixpoint.v_abstract
    then acc
    else
      begin
        let hedges = Xlambda_to_hgraph.G.vertex_pred result exnout in
        let raise_hedges, rec_vertices =
          HS.fold
            (fun h (rp, rv) ->
              let inputs = Xlambda_to_hgraph.G.hedge_pred result h in
              VS.fold
                (fun v (rp, rv) ->
                  match (Xlambda_to_hgraph.G.vertex_attrib result v).Fixpoint.v_orig with
                  | Xlambda_to_hgraph.Normal -> HS.add h rp, rv
                  | Xlambda_to_hgraph.Exception ->
                    rp, if VS.mem v visited then rv else VS.add v rv)
                inputs
                (rp, rv))
            hedges
            (HS.empty, VS.empty)
        in
        VS.fold
          (find_raise_points_rec visited)
          rec_vertices
          (HS.union raise_hedges acc)
      end
  in
  let find_raise_points orig_exnv =
    let exnout = VS.choose (Manager.H.VertexMap.find orig_exnv assotiation_map) in
    find_raise_points_rec VS.empty exnout HS.empty
  in

  let print_stack ppf stack =
    let st = Manager.Stack.to_stack_expr stack in
    let open Stack_types in
    let rec aux ppf = function
      | Start -> Format.fprintf ppf "<Start>"
      | Star Any -> Format.fprintf ppf "<Unknown stack frames>"
      | Any -> Format.fprintf ppf "<Unknown function>"
      | Elt f ->
         Format.fprintf ppf "@[<h>%a@]"
           Common_types.F.print_function_info f
      | Concat l -> aux_concat ppf l
      | Disjunct l -> Format.fprintf ppf "@[<v 1>( %a)@]" aux_disjunct l
      | Star st -> Format.fprintf ppf "@[<v 2>( %a)*@]" aux st
    and aux_concat ppf = function
      | [] -> ()
      | [st] -> aux ppf st
      | st :: tl ->
         begin
         (* TODO: check that the order is the one expected *)
           aux_concat ppf tl;
           Format.fprintf ppf "@\n%a" aux st
         end
    and aux_disjunct ppf = function
      | [] -> ()
      | [st] -> Format.fprintf ppf "@[<v 1>%a@]" aux st
      | st :: tl ->
         Format.fprintf ppf "@[<v 1>%a@]@\n| %a"
           aux st
           aux_disjunct tl
    in
    aux ppf st
  in

  let print_loc_opt ppf = function
    | Some loc -> Format.fprintf ppf "Location:@\n%a@\n" Location.print_loc loc
    | None -> Format.fprintf ppf "Unknown location@\n"
  in

  let print_raise_point rp =
    let attr = Xlambda_to_hgraph.G.hedge_attrib result rp in
    let stack = attr.Fixpoint.h_stack in
    let instrs = attr.Fixpoint.h_orig in
    (* Find the exact raise transition. A normal raise is translated as Var,
       other potentially raising constructs are kept specific.
       The raising transition is the last such transition. *)
    let find_raise attr =
      let rec aux = function
        | [] -> None
        | (xidl,
           (Common_types.Var _
           | Common_types.Lazyforce _
           | Common_types.Ccall (_, _)
           | Common_types.Send (_,_)
           | Common_types.App
           )
             as h) :: _ ->
          Some h
        | _ :: tl -> aux tl
      in
      aux (List.rev attr)
    in
    match find_raise instrs with
    | None -> Format.printf "Invalid raise point found@\n"
    | Some (_, Common_types.Var x) ->
       let locopt =
         try List.assoc x !raise_locs
         with Not_found -> None
       in
       Format.printf "Uncaught exception from raise@\n\
%a\
Backtrace:@\n%a@\n\
Exception xid: %a@\n@\n"
         print_loc_opt locopt
         print_stack stack
         print_xid x
    | Some (_,Common_types.App) ->
      (* Artifact of graph construction, ignore *)
      ()
    | Some (xidl,h) ->
       let msg =
         match h with
         | Common_types.Lazyforce _ -> "lazy evaluation"
         | Common_types.Ccall (_,_) -> "external call"
         | Common_types.Send (_,_) -> "method call"
         | _ -> assert false
       in
       let locopt =
         match xidl with
         | xid :: _ ->
            begin
              try snd (List.assoc xid !locs)
              with Not_found -> None
            end
         | [] -> None
       in
       Format.printf "Uncaught exception from %s@\n\
%a\
Backtrace:@\n%a@\n@\n"
         msg
         print_loc_opt locopt
         print_stack stack
  in

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
      print_newline ();
      let raise_points = find_raise_points exnv in
      HS.iter print_raise_point raise_points;
      (* No need to do exit 1; we expect to find exceptions in many programs *)
      (* exit 1 *)
    end

(* let analyse_watcher int_domain target =
  Printf.printf "--> Starting analysis of %s\n%!" target;
  let cmts = List.rev (get_cmts target) in
  let cmts =
    List.filter
      (fun file ->
         if Filename.check_suffix file ".cmti"
         then begin Printf.printf "Dropping cmti file %s\n%!" file; false end
         else true)
      cmts
  in
  analyse ~target int_domain cmts;
  Printf.printf "<-- Analysis of %s completed\n%!" target *)

(* let use_watcher = ref true *)

let integers = ref ""

let add_include dir =
  Config.load_path := !Config.load_path @ [dir]

let args =
  [
    (* "-nowatcher", Arg.Clear use_watcher, *)
    (* " Use the command-line arguments only to build the program to analyse"; *)
    "-I", Arg.String add_include,
    "<dir> Add the given directory for looking up cmi files";
    "-int-domain", Arg.Set_string integers,
    " Specify the integer domain to use\n Must be one of \
     cps, apron.box, apron.lineq, apron.polka";
  ]

let file_list = ref []

let anon file =
  file_list := file :: !file_list

let usage =
  let exe = Filename.basename (Sys.executable_name) in
  "SecurOCaml exception analyser\n\
   Usage:\n" ^
  exe ^ "<options> file1.cmt ... fileN.cmt\n\
  Analyse the program made by combining all the given cmt files\n\
  (Must include all libraries, in the right order)"

let _main =
  Config.load_path := [ "" ];
  Clflags.debug := true;
  Arg.parse args anon usage;
  file_list := List.rev !file_list;
  let int_domain =
    match !integers with
    | ""
    | "cps" ->
      (module No_ints.Make : IEsig.IE)
    | "apron.box" ->
      (module
        ApronEnv.Make( struct
          type t = Box.t
          let man = Box.manager_alloc ()
        end)
        : IEsig.IE
      )
    (* | "apron.lineq" -> *)
    (*   (module *)
    (*     ApronEnv.Make( struct *)
    (*       type t = Polka.equalities Polka.t *)
    (*       let man = Polka.manager_alloc_equalities () *)
    (*     end) *)
    (*     : IEsig.IE *)
    (*   ) *)
    (* | "apron.polka" -> *)
    (*   (module *)
    (*     ApronEnv.Make( struct *)
    (*       type t = Polka.strict Polka.t *)
    (*       let man = Polka.manager_alloc_strict () *)
    (*     end) *)
    (*     : IEsig.IE *)
    (*   ) *)
    | s ->
      Printf.eprintf "Invalid integer domain %s" s;
      Arg.usage args usage;
      exit 2
  in
  (* if !use_watcher *)
  (* then *)
  (*   List.iter (analyse_watcher int_domain) !file_list *)
  (* else *)
  analyse int_domain !file_list
