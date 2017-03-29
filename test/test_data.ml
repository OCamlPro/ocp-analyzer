open Format
open Locations
open Data
open Envs
open Utils
open Common_types

module Vertex = struct
  type t = string
  let compare (i:string) j = Pervasives.compare i j
  let hash (i:string) = Hashtbl.hash i
  let equal (i:string) j = i = j

  let print ppf s = Format.pp_print_string ppf s

  let c = ref 0
  let clone v = Printf.sprintf "%s_%i" v (incr c; !c)
end

type hedge_attr =
  | Set_int of xid * int
  | Set_closure of xid * F.t * xid array
  | Prepare_call of xid * xid
  | Call
  | Access_closure of xid * F.t * int
  | Access_param of xid
  | Add_int of xid * xid * xid
  | Ignore

let hedge_attr_to_string = function
  | Set_int _ -> "Set_int"
  | Set_closure _ -> "Set_closure"
  | Prepare_call _ -> "Prepare_call"
  | Call -> "Call"
  | Access_closure _ -> "Access_closure"
  | Access_param _ -> "Access_param"
  | Add_int _ -> "Add_int"
  | Ignore -> "Ignore"

module Hedge = struct
  type t = string
  let compare (i:t) j = Pervasives.compare i j
  let hash (i:t) = Hashtbl.hash i
  let equal (i:t) j = i = j

  let print ppf s = Format.pp_print_string ppf s
  let c = ref 0
  let clone v = Printf.sprintf "%s_%i" v (incr c; !c)
end

module T = struct

  type vertex = Vertex.t
  type hedge = Hedge.t
  module Vertex = Vertex
  module Hedge = Hedge

  let print_vertex = Vertex.print
  let print_hedge = Hedge.print

end

module H = Hgraph.Make(T)

module Env = struct

  type prepared_call =
    { closure : Locations.Locs.t;
      param : Locations.Locs.t }

  type t =
    { env : Envs.t';
      prepared_call : prepared_call option }

  let get_env_values = function
    | Env { values; _ } -> values
    | Bottom -> assert false

  (* let fold_loc loc f env = *)
  (*   Locm.fold_key (fun loc d e -> *)
  (*       let e2 = f loc d env in *)
  (*       Envs.join e e2 ) *)
  (*     loc (get_env_values env) Envs.bottom *)

  (* let fold_id id f env = *)
  (*   let values = get_env_values env in *)
  (*   let locs = get_idents id env in *)
  (*   Locs.fold *)
  (*     (fun loc e -> *)
  (*        Locm.fold_key (fun loc d e -> *)
  (*            let e2 = f loc d env in *)
  (*            Envs.join e e2 ) loc values e) *)
  (*     locs Envs.bottom *)

  (* let get_union ids env = *)
  (*   Locs.fold (fun id v -> union (get_data id env) v) ids bottom *)

  let get_union_fids locs { env; _ } =
    Access.fold_locs (fun _ -> Funs.extract_ids) locs [] !!env

  let any_int =
    { Data.bottom with int = Int_interv.top }

  let func_val func_id closure =
    { Data.bottom with f = Fm.singleton func_id
                      (Array.map Locs.singleton closure) }

  let func_val' func_id closure =
    { Data.bottom with f = Fm.singleton func_id closure }

  let env_v env = { env; prepared_call = None }

  let set_env id data env =
    {env with env = Access.set_env id data !!env.env}

  let set_data loc data env =
    {env with
     env =
       Access.on_loc
         (fun loc _ env -> Access.set_data loc data env)
         loc
         !!env.env;
    }

  let get_idents xid env = Access.get_idents xid !!env.env

  let set_constraint xid constr { env; prepared_call } =
    let env =
      Access.fold_xid
        (fun loc d e ->
           let v = Manipulation.intersect_noncommut d constr in
           Envs.join (Access.set_data loc v env) e )
        xid
        Envs.bottom
        !!env
    in
    { env; prepared_call }

  let set_int env dst cst =
    set_env dst (Int.singleton cst) env

  let add_int env ~src1 ~src2 ~dst =
    env
    |> set_constraint src1 any_int
    |> set_constraint src2 any_int
    |> set_env dst any_int

  let set_closure env dst func_id vars =
    set_env dst (func_val func_id vars) env

  let set_closure' env dst func_id vars =
    set_env dst (func_val' func_id vars) env

  let prepare_call env closure param =
    { env with prepared_call = Some {closure = Locs.singleton closure;
                                     param = Locs.singleton param} }

  let prepare_call' env closure param =
    { env with prepared_call = Some { closure; param } }

  let bottom = env_v Envs.bottom
  let empty = env_v Envs.empty
  let join e1 e2 =
    let prepared_call =
      match e1.prepared_call, e2.prepared_call with
      | None, None -> None
      | Some p, None
      | None, Some p -> Some p
      | Some p1, Some p2 ->
        Some { closure = Locs.union p1.closure p2.closure;
               param = Locs.union p1.param p2.param } in
    { env = (Envs.join e1.env e2.env);
      prepared_call }

  let is_bottom_env e = Envs.is_bottom e.env
  let is_leq e1 e2 = Manipulation.is_leq e1.env e2.env

  let call abs =
    match abs.env with
    | Bottom -> [|bottom|], []
    | Env _ ->
      match abs.prepared_call with
      | None -> failwith "malformed call sequence"
      | Some { closure; param } ->
        let functions = get_union_fids closure abs in
        [|bottom|], functions

  let merge_tabs a b = match a,b with
    | [||], x | x, [||] -> x
    | _,_ -> Array.init (Array.length a) (fun i -> Locs.union a.(i) b.(i))

  let enforce_closure abs func_id =
    match abs.prepared_call with
    | None -> failwith "malformed call sequence"
    | Some { closure; param } ->
      let env, c =
        Access.fold_locs
          (fun loc { f } ( (env,tab) as acc) ->
             try 
               let used_closure = Fm.find func_id f in
               let closure_val = func_val' func_id used_closure in
               let env2 = Access.set_data loc closure_val abs.env in
               (Envs.join env2 env, merge_tabs tab used_closure)
             with Not_found -> acc
          ) closure ( Envs.bottom, [||]) !!abs.env in
      { abs with env }, c
      (* let f = (get_union closure abs.env).f in *)
      (* if (Fm.is_empty f) *)
      (* then ( *)
      (*   let pp = Format.std_formatter in *)
      (*   Format.pp_print_newline pp (); *)
      (*   Locs.print pp closure; *)
      (*   Format.pp_print_newline pp (); *)
      (*   Print_data.print_until_done pp closure Locs.empty abs.env; *)
      (*        () *)
      (*      ); *)
      (* let used_closure = Fm.find func_id f in (\* RAISE NOT_FOUND on top AIdo *\) *)
      (* let closure_val = func_val' func_id used_closure in *)
      (* let abs = Locs.fold (fun id acc -> set_data id closure_val acc) closure abs in *)
      (* abs, used_closure *)

  let access_closure abs dst func_id pos =
    match abs.env with
    | Bottom -> bottom
    | Env _ ->
      match abs.prepared_call with
      | None -> failwith "malformed call sequence"
      | Some { closure; param } as prepared_call ->
        let abs, used_closure = enforce_closure abs func_id in
        { env = Access.set_idents dst used_closure.(pos) abs.env;
          prepared_call }

  let access_param abs dst =
    match abs.env with
    | Bottom -> bottom
    | Env _ ->
      match abs.prepared_call with
      | None -> failwith "malformed call sequence"
      | Some { closure; param } as prepared_call ->
        { env = Access.set_idents dst param abs.env;
          prepared_call }

end

type h = (unit,hedge_attr,unit) H.graph

let bigraphtbl : ( string, h * Vertex.t * h * H.subgraph ) Hashtbl.t = Hashtbl.create 16
let get_bigraph s = Hashtbl.find bigraphtbl s
let register_bigraph s q = Hashtbl.add bigraphtbl s q

let xid s = XId.create ~name:s ()


let vset_list l = List.fold_right H.VertexSet.add l H.VertexSet.empty
let hset_list l = List.fold_right H.HedgeSet.add l H.HedgeSet.empty

