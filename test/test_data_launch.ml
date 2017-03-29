open Format
open Locations
open Data
open Utils
open Common_types
open Test_data

let g,v0,g_func,func_subgraph =
  assert (Array.length Sys.argv > 1);
  get_bigraph Sys.argv.(1)

module Manager = struct
  module H = H
  type abstract = Env.t

  type vertex_attribute = unit
  type hedge_attribute = hedge_attr
  type graph_attribute = unit

  let apply hedge attr tabs =
    let abs = tabs.(0) in
    let geti x = Env.get_idents x abs in
    Printf.eprintf "%s %s\n%!" hedge (hedge_attr_to_string attr);
    match attr with
    | Set_int (id,cst) ->
      [|Env.set_int abs id cst|], [] (* id <- cst *)
    | Set_closure (fun_id, funct, id_arr ) ->
      [|Env.set_closure' abs fun_id funct (Array.map geti id_arr)|], [] (* f <- closure{a} *)
    | Prepare_call (fun_id, clos_id) ->
      [|Env.prepare_call' abs (geti fun_id) (geti clos_id)|], [] (* prepare call f b *)
    | Call ->
      Env.call abs
    | Access_closure(id, funct, n) ->
      [|Env.access_closure abs id funct n|], [] (* a <- closure.(0) *)
    | Access_param(id) ->
      [|Env.access_param abs id|], [] (* b <- param *)
    | Add_int (id1,id2,id3) ->
      [|Env.add_int abs id1 id2 id3|], [] (* c <- a + b *)
    | Ignore -> [|abs|], []

  let abstract_init i =
    if i = "v0"
    then Env.empty
    else Env.bottom

  let bottom _ = Env.bottom
  let is_bottom _ = Env.is_bottom_env
  let join_list _ l =
    List.fold_left Env.join Env.bottom l

  let widening _ a1 a2 = Env.join a1 a2
  let narrowing = None

  let is_leq _ = Env.is_leq

  type function_id = F.t
  module Function_id = F

  let find_function id =
    (* assert(F.equal id func); *)
    g_func, func_subgraph

  module Function_id' = struct
    include Function_id
    let is_important _ = true
    let n = 3
  end

  module Stack = Abstract_stack.Leveled ( Function_id' )

end

let print_env ppf attr =
  let open Format in
  let open Env in
  match attr.env with
  | Envs.Bottom -> fprintf ppf "Bottom"
  | Envs.Env env ->
    fprintf ppf "{@[ ";
    pp_print_string ppf "No env printer available";
    (* Idm.iter (fun (_,id) _ -> fprintf ppf "%a " Id.print id) env; *)
    fprintf ppf "@]}"

let print_attrvertex ppf vertex attr =
  Format.fprintf ppf "%s %a" vertex print_env attr.Fixpoint.v_abstract

let ouput_dot g =
  H.print_dot
    ~print_attrvertex
    (* ~print_attrhedge *)
    Format.std_formatter g

module FP = Fixpoint.Fixpoint(T)(Manager)
(* let err_graph = ref None *)
let r, map =
  try FP.kleene_fixpoint (* ~err_graph *) g (Manager.H.VertexSet.singleton v0)
  with e ->
    (* (match !err_graph with *)
    (*  | None -> assert false *)
    (*  | Some g -> *)
    (*    let print_attrvertex ppf vertex attr = Format.pp_print_string ppf vertex in *)
    (*    H.print_dot *)
    (*      ~print_attrvertex *)
    (*      (\* ~print_attrhedge *\) *)
    (*      Format.std_formatter g); *)
    raise e

let () = ouput_dot r

