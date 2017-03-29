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

module Base = struct

  let print_hedge_attribute pp hattr =
    let open Common_types in
    let aux ppf = function
      | Prim (prim, _) ->
        Print_xlambda.primitive ppf prim
      | Alloc (_, prim, _) ->
        Print_xlambda.allocator ppf prim
      | App_prep _ ->
        Format.fprintf ppf "App_prep"
      | App ->
        Format.fprintf ppf "App"
      | App_return ->
        Format.fprintf ppf "App_return"
      | App_exn ->
        Format.fprintf ppf "App_exn"
      | Return _ ->
        Format.fprintf ppf "Return"
      | Retexn _ ->
        Format.fprintf ppf "Retexn"
      | EndLet (x::tl) ->
        let rec pp_list ppf = function
          | [] -> ()
          | x::tl -> Format.fprintf ppf ";%a%a" XId.print x pp_list tl
        in
        Format.fprintf ppf "EndLet [%a%a]" XId.print x pp_list tl
      | EndLet _ ->
        Format.fprintf ppf "EndLet"
      | Var v ->
        XId.print ppf v
      | Const _ ->
        Format.fprintf ppf "Const"
      | Constraint _ ->
        Format.fprintf ppf "Constraint"
      | Lazyforce _ ->
        Format.fprintf ppf "Lazyforce"
      | Ccall _ ->
        Format.fprintf ppf "Ccall"
      | Send _ ->
        Format.fprintf ppf "Send"
    in
    let rec print_list = function
      | [] -> ()
      | [[i], hinfo] -> Format.fprintf pp "@[%a <- %a@]" XId.print i aux hinfo
      | [[x;e], hinfo] ->
        Format.fprintf pp "@[%a(%a) <- %a@]"
          XId.print x
          XId.print e
          aux hinfo
      | ([i], hinfo) :: t ->
        begin
          Format.fprintf pp "@[%a <- %a@]@." XId.print i aux hinfo;
          print_list t
        end
      | ([x;e], hinfo) :: t ->
        begin
          Format.fprintf pp "@[%a(%a) <- %a@]@."
            XId.print x
            XId.print e
            aux hinfo;
          print_list t
        end
      | ([],hinfo):: t ->
        begin
          Format.fprintf pp "@[_ <- %a@]@." aux hinfo;
          print_list t
        end
      | (_::_::_::_,_):: _ -> assert false
    in
    print_list hattr

  let print_attrhedge ppf h attr =
    Format.fprintf ppf "%a: %a"
      Xlambda_to_hgraph.T.Hedge.print h
      print_hedge_attribute attr

  let print_attrvertex ppf v attr =
    Xlambda_to_hgraph.T.Vertex.print ppf v

(* more correct, but less readable... app nodes helps give structure *)

(* let show_hedge _h attr = *)
(*   match attr.Fixpoint.h_abstract with *)
(*   | None -> false *)
(*   | Some arr -> *)
(*     not (List.exists Envs.is_bottom (Array.to_list arr)) *)
end

module Result
    (T: Hgraph_types.T)
    (M: Fixpoint_types.Manager with module T := T) = struct

  let show_vertex v attr =
    not (M.is_bottom v attr.Fixpoint.v_abstract)

  let show_hedge _h attr =
    match attr.Fixpoint.h_abstract with
    | None -> false
    | Some arr -> true

  let print_attrhedge ppf h attr =
    Format.fprintf ppf "%a: %a"
      T.Hedge.print h
      Base.print_hedge_attribute attr.Fixpoint.h_orig

  let print_attrvertex ppf v attr =
    Format.fprintf ppf "%a, %a"
      T.Vertex.print v
      M.Stack.print attr.Fixpoint.v_stack

  let vertex_subgraph _v attr =
    if M.Stack.equal
        M.Stack.empty
        attr.Fixpoint.v_stack
    then None
    else Some (Format.asprintf "cluster_%a" M.Stack.print attr.Fixpoint.v_stack)

  let hedge_subgraph _h attr =
    if M.Stack.equal
        M.Stack.empty
        attr.Fixpoint.h_stack
    then None
    else Some (Format.asprintf "cluster_%a" M.Stack.print attr.Fixpoint.h_stack)

end
