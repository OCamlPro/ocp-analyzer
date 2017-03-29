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

open Hgraph_types
open Fixpoint_types

let rec map_filter f l =
  match l with
  | [] -> []
  | t::q -> match f t with
    | None -> map_filter f q
    | Some t' -> t' :: (map_filter f q)

let lift_option_array a =
  try Some (Array.map (function
      | None -> raise Not_found
      | Some v -> v) a)
  with Not_found -> None

module Couple(L:OrderedHashedType)(R:OrderedHashedType) :
  OrderedHashedType with type t = L.t * R.t =
struct
  type t = (L.t * R.t)

  let hash (l,r) = Hashtbl.hash (L.hash l, R.hash r)

  let compare (l1,r1) (l2,r2) =
    let c = L.compare l1 l2 in
    if c <> 0 then c
    else R.compare r1 r2

  let equal (l1,r1) (l2,r2) =
    L.equal l1 l2 && R.equal r1 r2

  let print ppf (l,r) =
    Format.fprintf ppf "(@[<2>%a,@ %a@])" L.print l R.print r
end

module StackGraph (T:T) (H : Hgraph with module T := T) (Stack:Stack_types.Stack) = struct
  type stack = Stack.t

  module HedgeAndStack = Couple(T.Hedge)(Stack)
  module VertexAndStack = Couple(T.Vertex)(Stack)

  module HaS_Set = Set.Make(HedgeAndStack)
  module HaS_Map = Map.Make(HedgeAndStack)
  module VaS_Set = Set.Make(VertexAndStack)
  module VaS_Map = Map.Make(VertexAndStack)

  module StackMap = Map.Make(Stack)

  type 'a hedge_attrib = {
    orig_attrib: 'a;
    orig_hedge: T.hedge;
    stack : stack;
  }

  type 'a vertex_attrib = {
    orig_attrib: 'a;
    orig_vertex: T.vertex;
    stack : stack;
  }

  type ('v,'h,'e) g = {
    mutable orig_graph : ('v, 'h, 'e) H.graph StackMap.t;
    mutable call_sites : H.HedgeSet.t StackMap.t;
    graph : ('v vertex_attrib, 'h hedge_attrib, unit) H.graph;
    mutable hedges: T.hedge HaS_Map.t;
    mutable vertices: T.vertex VaS_Map.t;
  }

  let init_graph graph = {
    orig_graph = StackMap.singleton Stack.empty graph;
    call_sites = StackMap.empty;
    graph = H.create ();
    hedges= HaS_Map.empty;
    vertices= VaS_Map.empty;
  }

  let exists_call_site g stack hedge =
    try H.HedgeSet.mem hedge (StackMap.find stack g.call_sites)
    with Not_found -> false

  let add_call_site g stack hedge =
    let set =
      try StackMap.find stack g.call_sites
      with Not_found -> H.HedgeSet.empty in
    let set = H.HedgeSet.add hedge set in
    StackMap.add stack set g.call_sites

  let original_vertex g vertex =
    assert(H.contains_vertex g.graph vertex);
    let attrib = H.vertex_attrib g.graph vertex in
    attrib.orig_vertex

  let find_hedge g stack orig_hedge =
    try Some (HaS_Map.find (orig_hedge, stack) g.hedges) with
    | Not_found -> None

  let find_vertex g stack orig_vertex =
    try Some (VaS_Map.find (orig_vertex, stack) g.vertices) with
    | Not_found -> None

  let get_orig_graph g stack =
    assert(StackMap.mem stack g.orig_graph);
    StackMap.find stack g.orig_graph

  let make_vertex g stack orig_vertex =
    let orig_graph = get_orig_graph g stack in
    assert(H.contains_vertex orig_graph orig_vertex);
    let vertex = T.Vertex.clone orig_vertex in
    let orig_attrib = H.vertex_attrib orig_graph orig_vertex in
    let attrib = {
      orig_vertex;
      orig_attrib;
      stack } in
    H.add_vertex g.graph vertex attrib;
    g.vertices <- VaS_Map.add (orig_vertex, stack) vertex g.vertices;
    vertex

  let get_vertex g stack orig_vertex =
    match find_vertex g stack orig_vertex with
    | None -> make_vertex g stack orig_vertex
    | Some v -> v

  let make_hedge ?(get_vertex=get_vertex) g stack orig_hedge =
    let orig_graph = get_orig_graph g stack in
    assert(H.contains_hedge orig_graph orig_hedge);
    let hedge = T.Hedge.clone orig_hedge in
    let orig_attrib = H.hedge_attrib orig_graph orig_hedge in
    let pred = Array.map (get_vertex g stack) (H.hedge_pred' orig_graph orig_hedge) in
    let succ = Array.map (get_vertex g stack) (H.hedge_succ' orig_graph orig_hedge) in
    let attrib = {
      orig_hedge;
      orig_attrib;
      stack } in
    H.add_hedge g.graph hedge attrib ~pred ~succ;
    g.hedges <- HaS_Map.add (orig_hedge, stack) hedge g.hedges;
    hedge

  let get_hedge g stack orig_hedge =
    match find_hedge g stack orig_hedge with
    | None -> make_hedge g stack orig_hedge
    | Some v -> v

  let hiset_map_hedge f set =
    H.HedgeIntSet.fold (fun (i,h) set -> H.HedgeIntSet.add (i,f h) set)
      set H.HedgeIntSet.empty

  (* get all successors of a vertex that correspond to successor of
     the original vertex *)
  let import_vertex_succ' g vertex =
    assert(H.contains_vertex g.graph vertex);
    let attrib = H.vertex_attrib g.graph vertex in
    let orig_graph = get_orig_graph g attrib.stack in
    assert(H.contains_vertex orig_graph attrib.orig_vertex);
    let orig_succ = H.vertex_succ' orig_graph attrib.orig_vertex in
    hiset_map_hedge (get_hedge g attrib.stack) orig_succ

  let import_vertex_pred' g vertex =
    assert(H.contains_vertex g.graph vertex);
    let attrib = H.vertex_attrib g.graph vertex in
    let orig_graph = get_orig_graph g attrib.stack in
    assert(H.contains_vertex orig_graph attrib.orig_vertex);
    let orig_pred = H.vertex_pred' orig_graph attrib.orig_vertex in
    hiset_map_hedge (get_hedge g attrib.stack) orig_pred

  let vertex_succ' g vertex : H.HedgeIntSet.t =
    ignore(import_vertex_succ' g vertex:H.HedgeIntSet.t);
    H.vertex_succ' g.graph vertex

  let vertex_pred' g vertex : H.HedgeIntSet.t =
    ignore(import_vertex_pred' g vertex:H.HedgeIntSet.t);
    H.vertex_pred' g.graph vertex

  let hset_of_hiset s = H.HedgeIntSet.fold (fun (_,v) set -> H.HedgeSet.add v set) s H.HedgeSet.empty

  let vertex_succ g v = hset_of_hiset (vertex_succ' g v)
  (* let vertex_pred g v = hset_of_hiset (vertex_pred' g v) *)

  let hedge_succ' g hedge =
    assert(H.contains_hedge g.graph hedge);
    (* edges have fixed successor in the graph: there is no need to
       import from the original graph *)
    H.hedge_succ' g.graph hedge

  let hedge_pred' g hedge =
    assert(H.contains_hedge g.graph hedge);
    H.hedge_pred' g.graph hedge

  let hset_of_array a = Array.fold_right H.VertexSet.add a H.VertexSet.empty

  (* let hedge_pred g h = hset_of_array (hedge_pred' g h) *)
  let hedge_succ g h = hset_of_array (hedge_succ' g h)

  let hedge_attrib g hedge =
    assert(H.contains_hedge g.graph hedge);
    (H.hedge_attrib g.graph hedge).orig_attrib

  let hedge_stack g hedge =
    assert(H.contains_hedge g.graph hedge);
    (H.hedge_attrib g.graph hedge).stack

  type ('a,'b,'c) function_info =
    { stack_elt : Stack.elt;
      f_graph : ('a,'b,'c) H.graph;
      subgraph : H.subgraph }

  let import_function_call g hedge function_info =
    assert(H.contains_hedge g.graph hedge);
    let attrib = H.hedge_attrib g.graph hedge in
    let stack = Stack.push attrib.stack function_info.stack_elt in

    let link_function g hedge stack ({subgraph;_} as function_info) =
      g.orig_graph <- StackMap.add stack function_info.f_graph g.orig_graph;

      if Array.length (hedge_pred' g hedge) <> Array.length subgraph.H.sg_input
      then raise (Invalid_argument "link_function: input and sg_input of different length");
      if Array.length (hedge_succ' g hedge) <> Array.length subgraph.H.sg_output
      then raise (Invalid_argument "link_function: output and sg_output of different length");

      Array.iter (fun sg_in ->
          if H.VertexSet.mem sg_in subgraph.H.sg_vertex
          then raise (Invalid_argument "link_function: sg_input and sg_vertex are not disjoint"))
        subgraph.H.sg_input;
      Array.iter (fun sg_out ->
          if H.VertexSet.mem sg_out subgraph.H.sg_vertex
          then raise (Invalid_argument "link_function: sg_output and sg_vertex are not disjoint"))
        subgraph.H.sg_output;

      let get_vertex' =

        let add_map_vertex callee_vertices call_site_vertices map =
          List.fold_left2
            (fun map callee_vertex call_site_vertex ->
               H.VertexMap.add callee_vertex call_site_vertex map)
            map
            (Array.to_list callee_vertices)
            (Array.to_list call_site_vertices) in
        let map_vertex =
          add_map_vertex
            function_info.subgraph.H.sg_input
            (hedge_pred' g hedge)
            H.VertexMap.empty in
        let map_vertex =
          add_map_vertex
            function_info.subgraph.H.sg_output
            (hedge_succ' g hedge)
            map_vertex in

        let get_vertex' g stack orig_vertex =
          try H.VertexMap.find orig_vertex map_vertex
          with Not_found -> get_vertex g stack orig_vertex
        in
        get_vertex'
      in

      let input_hedges' =
        Array.fold_left (fun set vertex ->
            H.HedgeSet.union set (H.vertex_succ function_info.f_graph vertex))
              H.HedgeSet.empty function_info.subgraph.H.sg_input in

      let output_hedges' =
        Array.fold_left (fun set vertex ->
            H.HedgeSet.union set (H.vertex_pred function_info.f_graph vertex))
              H.HedgeSet.empty function_info.subgraph.H.sg_output in

      let import_hedges set =
        H.HedgeSet.fold (fun orig_hedge set ->
            H.HedgeSet.add
              (make_hedge ~get_vertex:get_vertex' g stack orig_hedge)
              set)
          set H.HedgeSet.empty
      in

      let input_hedges = import_hedges input_hedges' in
      let _output_hedges = import_hedges output_hedges' in

      input_hedges
    in

    (* TODO: potentially make configurable to be able to not
       distinguish different hedges (less precise => probably faster) *)
    let new_call_site = not (exists_call_site g stack hedge) in
    if new_call_site
    then begin
      g.call_sites <- add_call_site g stack hedge;
      let input_hedges = link_function g hedge stack function_info in
      Some input_hedges
    end
    else None

end

module WorkQueue(S:Set.S) = struct
  type elt = S.elt
  type t = {
    queue : elt Queue.t;
    mutable set : S.t;
  }

  let push t v =
    if not (S.mem v t.set)
    then begin
      Queue.push v t.queue;
      t.set <- S.add v t.set
    end

  let push_set t s = S.iter (push t) s

  let pop t =
    if Queue.is_empty t.queue
    then None
    else
      let v = Queue.pop t.queue in
      t.set <- S.remove v t.set;
      Some v

  let create () =
    { queue = Queue.create (); set = S.empty }

end

type ('a,'b,'c) vertex_result_attribute =
  { v_orig : 'a;
    v_abstract : 'b;
    v_stack : 'c }

type ('a,'b,'c) hedge_result_attribute =
  { h_orig : 'a;
    h_abstract : 'b;
    h_stack : 'c }

module Fixpoint (T:T) (M:Manager with module T := T) = struct
  module SG = StackGraph(T) (M.H) (M.Stack)

  module Vwq = WorkQueue(M.H.VertexSet)
  module Hwq = WorkQueue(M.H.HedgeSet)

  type input_graph = (M.vertex_attribute,
                      M.hedge_attribute,
                      M.graph_attribute) M.H.graph

  type output_vattr = (M.vertex_attribute,
                       M.abstract,
                       M.Stack.t)
    vertex_result_attribute

  type output_hattr = (M.hedge_attribute,
                       M.abstract array option,
                       M.Stack.t)
    hedge_result_attribute

  type output_graph = (output_vattr, output_hattr, M.global) M.H.graph

  type state =
    { graph : (M.vertex_attribute, M.hedge_attribute, M.graph_attribute) SG.g;
      vertex_values : M.abstract M.H.VertexMap.t;
      hedge_values : M.abstract array M.H.HedgeMap.t;
      global_value : M.global;
    }

  let init_state graph =
    { graph = SG.init_graph graph;
      vertex_values = M.H.VertexMap.empty;
      hedge_values = M.H.HedgeMap.empty;
      global_value = M.global_init ();
    }

  let update_hedge state hedge =
    let pred = SG.hedge_pred' state.graph hedge in
    let f v =
      let abstract =
        try M.H.VertexMap.find v state.vertex_values with
        | Not_found -> M.bottom v in
      if M.is_bottom v abstract
      then None
      else Some abstract
    in
    match lift_option_array (Array.map f pred) with
    | None ->
      M.H.VertexSet.empty, M.H.HedgeSet.empty, state
    | Some abstract ->
      let attrib = SG.hedge_attrib state.graph hedge in
      let stack = SG.hedge_stack state.graph hedge in
      let abstract, global_value, functions =
        M.apply hedge attrib stack abstract state.global_value in
      let state =
        { state with
          hedge_values = M.H.HedgeMap.add hedge abstract state.hedge_values;
          global_value;
        } in
      let functions = List.map (fun function_id ->
          let f_graph, subgraph = M.find_function function_id in
          { SG.stack_elt = function_id; f_graph; subgraph }) functions in
      let hedges_to_update =
        List.fold_left
          (fun need_update fun_info ->
             match SG.import_function_call state.graph hedge fun_info with
             | None -> need_update
             | Some to_update ->
               M.H.HedgeSet.union to_update need_update)
          M.H.HedgeSet.empty functions in
      let vertices_to_update = SG.hedge_succ state.graph hedge in
      vertices_to_update, hedges_to_update, state


  let update_vertex start_vertices narrowing_phase state vertex =
    let pred = M.H.HedgeIntSet.elements (SG.vertex_pred' state.graph vertex) in

    let hedge_opt state (i,h) =
      try Some (M.H.HedgeMap.find h state.hedge_values).(i) with
      | Not_found -> None
    in

    (* Union of the the values that can reach the state *)
    let abstract = match pred with
      | [] -> M.abstract_init (SG.original_vertex state.graph vertex)
      | _ ->
        match map_filter (hedge_opt state) pred with
        | [] -> M.bottom vertex
        | [a] -> a
        | l -> M.join_list vertex l
    in

    let previous_value =
      try Some (M.H.VertexMap.find vertex state.vertex_values) with
      | Not_found -> None in

    let propagate, abstract =
      match previous_value with
      | None -> true, abstract
      | Some previous_value ->
        (* if this is not the first value for this state, accelerate *)
        let abstract, propagate =
          (* if we are not in the narrowing phase: widen *)
          match narrowing_phase with
          | None ->
            let widen =
              (* To ensure that any infinite sequence goes throug an
                 infinite number of widenings, we only need to widen
                 in loops and only one time per loop. Since each loop
                 has at least one vertex with 2 predecessor or a
                 vertex of the loop is in the start vertices, this
                 restriction is correct. *)
              match pred with
              | [_] -> M.H.VertexSet.mem vertex start_vertices
              | _ -> true
            in
            let abstract =
              if widen
              then begin
                (* Format.printf "widen %a@." T.Vertex.print vertex; *)
                M.widening vertex previous_value abstract
              end
              else M.join_list vertex [previous_value; abstract]
            in
            let propagate = not (M.is_leq vertex abstract previous_value) in
            abstract, propagate
          | Some narrow ->
            let abstract = narrow vertex previous_value abstract in
            let propagate = not (M.is_leq vertex previous_value abstract) in
            abstract, propagate
        in
        propagate, abstract
    in

    let to_update =
      if propagate
      then SG.vertex_succ state.graph vertex
      else M.H.HedgeSet.empty
    in

    to_update, { state with vertex_values = M.H.VertexMap.add vertex abstract state.vertex_values }

  let loop narrowing_phase state start_vertices =
    let vwq = Vwq.create () in
    let hwq = Hwq.create () in
    Vwq.push_set vwq start_vertices;

    let rec aux state =
      match Vwq.pop vwq with
      | None ->
        begin match Hwq.pop hwq with
          | None -> state
          | Some h ->
            let vertices_to_update, hedges_to_update, state =
              update_hedge state h in
            Vwq.push_set vwq vertices_to_update;
            Hwq.push_set hwq hedges_to_update;
            aux state
        end
      | Some v ->
        let to_update, state = update_vertex start_vertices narrowing_phase state v in
        Hwq.push_set hwq to_update;
        aux state
    in
    aux state

  let kleene_fixpoint' graph start_vertices =
    let state = init_state graph in
    let start_vertices =
      M.H.VertexSet.fold (fun v set ->
          M.H.VertexSet.add
            (SG.make_vertex state.graph M.Stack.empty v)
            set) start_vertices
        M.H.VertexSet.empty in
    let state = loop None state start_vertices in
    match M.narrowing with
    | None -> state
    | Some _ ->
      let vertices_set =
        List.fold_left
          (fun set vertex -> M.H.VertexSet.add vertex set)
          M.H.VertexSet.empty
          (M.H.list_vertex state.graph.SG.graph) in
      loop M.narrowing state vertices_set

  let kleene_fixpoint (graph:input_graph) start_vertices =
    assert(M.H.correct graph);
    let state = kleene_fixpoint' graph start_vertices in
    let empty_vertex_map =
      List.fold_left (fun map orig_vertex ->
          M.H.VertexMap.add orig_vertex M.H.VertexSet.empty map)
        M.H.VertexMap.empty
        (M.H.list_vertex graph) in
    let vertex_map = ref empty_vertex_map in
    let map_vertex new_vertex attrib =
      let orig_vertex = attrib.SG.orig_vertex in
      let set =
        try M.H.VertexMap.find orig_vertex !vertex_map
        with Not_found -> M.H.VertexSet.empty in
      vertex_map :=
        M.H.VertexMap.add
          orig_vertex
          (M.H.VertexSet.add new_vertex set)
          !vertex_map;
      let v_abstract =
        try M.H.VertexMap.find new_vertex state.vertex_values
        with Not_found -> M.bottom new_vertex in
      { v_orig = attrib.SG.orig_attrib;
        v_abstract;
        v_stack = attrib.SG.stack }
    in
    let map_hedge new_hedge (attrib:'a SG.hedge_attrib) =
      let h_abstract =
        try Some (Array.copy (M.H.HedgeMap.find new_hedge state.hedge_values))
        with Not_found -> None in
      { h_orig = attrib.SG.orig_attrib;
        h_abstract;
        h_stack  = attrib.SG.stack }
    in
    assert(M.H.correct state.graph.SG.graph);
    let graph =
      M.H.copy state.graph.SG.graph
        map_vertex
        map_hedge
        (fun _ -> state.global_value)
    in
    assert(M.H.correct graph);
    graph, !vertex_map

end
