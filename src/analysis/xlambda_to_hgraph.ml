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

module type E =
sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val mk : unit -> t
  val print : Format.formatter -> t -> unit
  val clone : t -> t
end

module Vertex =
struct
  type t = string * int
  let compare = compare
  let equal = (=)
  let hash x = Hashtbl.hash x

  let c = ref (-1)

  let print ppf (s,i) =
    Format.pp_print_string ppf s;
    Format.pp_print_string ppf "_";
    Format.pp_print_int ppf i

  let mk ?(modulename="no_module") () =
    incr c;
    modulename, !c

  let clone (s,_) = mk ~modulename:("cloned_"^s) ()
end

module Hedge : E =
struct
  type t = int
  let compare (x:int) y = compare x y
  let equal (x:int) y = x = y
  let hash (x:int) = Hashtbl.hash x

  let c = ref (-1)

  let print ppf i = Format.fprintf ppf "v%i" i
  let mk () = incr c; !c

  let clone _ = mk ()
end

module T =
struct
  type vertex = Vertex.t
  type hedge = Hedge.t
  module Vertex = Vertex
  module Hedge = Hedge

  let print_vertex = Vertex.print
  let print_hedge = Hedge.print
end

module G = Hgraph.Make (T)
open G

type vattr = Normal | Exception
type hattr = ( xid list * hinfo ) list
type gattr = unit
type hg = ( vattr, hattr, gattr ) G.graph

type fun_desc =
  {
    f_graph : hg;
    f_in : Vertex.t array;
    f_out : Vertex.t array;
    f_vertex : VertexSet.t;
    f_hedge : HedgeSet.t;
  }

type mod_desc =
  {
    m_in : Vertex.t;
    m_out : Vertex.t;
    m_exn : Vertex.t;
    m_return : xid;
  }

let ctrue = Constraint (Cbool true)
let cfalse = Constraint (Cbool false)

module Is = Set.Make ( struct type t = int let compare (a:int) b = compare a b end )

open Xlambda

module Scope :
sig
  type 'a t
  type token
  val empty : 'a t
  val add : 'a -> 'a t -> 'a t
  val set_token : 'a t -> 'a t * token
  val retrieve : token -> 'a t -> 'a list
end = struct
  type token = int
  type 'a elt = Tok of token | Elt of 'a
  type 'a t = ('a elt) list
  let last_token = ref 0
  let empty = []
  let add x l = (Elt x) :: l
  let set_token l =
    incr last_token;
    ((Tok !last_token) :: l, !last_token)
  let retrieve tok l =
    let rec aux acc = function
      | [] -> raise (Invalid_argument "Scope.retrieve")
      | (Elt x) :: tail -> aux (x::acc) tail
      | (Tok t) :: tail -> if tok = t then acc else aux acc tail
    in
    aux [] l
end

type accu =
  { inv_acc : (Vertex.t * hattr) option;
    scope : xid Scope.t;
  }

let mk_inv_acc inv = Some (inv, [])

let nv ?(modulename="") attr g =
  let v = Vertex.mk ~modulename () in
  add_vertex g v attr;
  v

let singleh g id v ~inv ~outv =
  add_hedge g ( Hedge.mk ()) [[id],v] ~pred:[|inv|] ~succ:[|outv|]

(* The None case means that the path is unreachable (after a staticraise,
   for example), so there is no need to create the edges
   Attributes are stored with the last one at the head of the list, then
   reversed on creation to present the attributes in the right order. *)
let simpleh id v ~inv_acc =
  match inv_acc with
  | None -> None
  | Some (inv, attrs) ->
    Some (inv, ([id],v)::attrs)

let simpleh_list l ~inv_acc =
  match inv_acc with
  | None -> None
  | Some (inv, attrs) ->
    Some (inv, List.rev_append l attrs)

let close_acc g ~inv_acc ~outv =
  match inv_acc with
  | None -> ()
  | Some (inv,attrs) ->
    if Vertex.equal inv outv then
      if attrs = [] then ()
      else assert false (* non-empty edge looping on itself; technically could
                           be allowed, but it seems better to forbid it. *)
    else
      add_hedge g ( Hedge.mk ()) (List.rev attrs) ~pred:[|inv|] ~succ:[|outv|]

let endlet token ~accu =
  let ids = Scope.retrieve token accu.scope in
  match accu.inv_acc with
  | None -> None
  | Some (inv, ([], EndLet l) :: tl) ->
    Some (inv, ([], EndLet (ids @ l)) :: tl)
  | Some (inv, l) ->
    Some (inv, ([], EndLet ids) :: l)

let statics : ( int, Vertex.t * xid list * Scope.token ) Hashtbl.t =
  Hashtbl.create 32

type exn_struct =
  { exn_id: xid;
    exnv: Vertex.t;
    exn_tok: Scope.token;
  }

let xlambda ~(g:hg) ~mk_xid ~modulename ~outv ~ret_id ~exn ~scope ~inv code =
  let nv = nv ~modulename in
  let nvn = nv Normal and nve = nv Exception in

  let mk_exn_hedge ~g ~inv_acc ~exn ~scope hattr =
    let inv_acc =
      simpleh exn.exn_id hattr ~inv_acc
    in
    let accu = { inv_acc; scope } in
    let inv_acc = endlet exn.exn_tok ~accu in
    close_acc g ~inv_acc ~outv:exn.exnv
  in

  let exn_prim g ~accu ~outv ~exn ~id prim =
    let inv = nvn g in
    close_acc g ~inv_acc:accu.inv_acc ~outv:inv;
    add_hedge g (Hedge.mk ()) [ [id;exn.exn_id], prim]
      ~pred:[|inv|] ~succ:[|outv;exn.exnv|];
    Some (outv,[])
  in

  let rec xlambda ~g ~outv ~ret_id ~accu ~exn code =
    match code with
    | Xlet d -> xlet g accu outv exn ret_id d
    | Xrec d -> xrec g accu outv exn ret_id d
    | Xend id -> simpleh ret_id ( Var id) ~inv_acc:accu.inv_acc

  and xlet g accu outv exn ret_id d =
    let in_out = nvn g in
    let in_out_acc =
      xcontrol g accu in_out exn d.xe_id ret_id d.xe_lam
    in
    let (scope, tok) = Scope.set_token accu.scope in
    let scope = Scope.add d.xe_id scope in
    let accu = { inv_acc = in_out_acc; scope } in
    let outv_acc =
      xlambda ~g ~outv ~ret_id ~accu ~exn d.xe_in
    in
    endlet tok ~accu:{accu with inv_acc = outv_acc}

  and xrec g accu outv exn ret_id d =
    (* at this point, there are only primitives *)
    let in_out_acc =
      simpleh_list
       (List.rev_map
          (fun ( id, p, args ) ->
             [id], Alloc (AllocationId.create ~name:modulename (), p, args ) )
          d.xr_decls )
       ~inv_acc:accu.inv_acc
    in
    let scope, tok = Scope.set_token accu.scope in
    let scope =
      List.fold_left (fun acc (id, _, _) -> Scope.add id acc) scope d.xr_decls
    in
    let accu = { inv_acc = in_out_acc; scope } in
    let outv_acc =
      xlambda ~g ~outv ~ret_id ~accu ~exn d.xr_in
    in
    endlet tok ~accu:{accu with inv_acc = outv_acc}

  and xcontrol g accu outv exn id ret_id c =
    match c with
    | Xvar i -> simpleh id ( Var i) ~inv_acc:accu.inv_acc

    | Xconst c -> simpleh id ( Const c) ~inv_acc:accu.inv_acc

    | Xapply ( f, x) ->
      let retv = nvn g in
      let appout = nvn g and appexn = nve g in
      let inv_acc = simpleh id ( App_prep ( f, x ) ) ~inv_acc:accu.inv_acc in
      close_acc g ~inv_acc ~outv:retv;
      add_hedge g ( Hedge.mk ()) [ [id;exn.exn_id], App ]
        ~pred:[|retv|] ~succ:[| appout; appexn |];
      mk_exn_hedge
        ~g
        ~inv_acc:(mk_inv_acc appexn)
        ~exn
        ~scope:accu.scope
        App_exn;
      simpleh id ( App_return ) ~inv_acc:(Some (appout, []))

    | Xprim ( p, args) ->
      simpleh id ( Prim ( p, args ) ) ~inv_acc:accu.inv_acc

    | Xalloc ( p, args) ->
      simpleh
        id
        ( Alloc ( AllocationId.create ~name:modulename (), p, args ) )
        ~inv_acc:accu.inv_acc

    | Xswitch ( si_id, s) ->
      let startv = nvn g in
      close_acc g ~inv_acc:accu.inv_acc ~outv:startv;
      let inv_acc = Some (startv, []) in
      let switch_handle is_cp (i,lam) =
        let inc_acc =
          simpleh si_id (Constraint ( if is_cp then Ccp i else Ctag i)) ~inv_acc
        in
        let accu = {accu with inv_acc = inc_acc} in
        let outv_acc =
          xlambda ~g ~outv ~ret_id:id ~accu ~exn lam
        in
        close_acc g ~inv_acc:outv_acc ~outv
      in
      let () = List.iter ( switch_handle true) s.x_consts
      and () = List.iter ( switch_handle false) s.x_blocks
      in
      begin
        match s.x_failaction with
          None -> ()
        | Some lam ->
          let get_not_used n l =
            let rec aux n res =
              if n = 0
              then res
              else
                let n = pred n in
                aux n ( Is.add n res)
            in
            List.fold_left ( fun s (i,_) -> Is.remove i s) (aux n Is.empty) l
          in
          let cps = get_not_used s.x_numconsts s.x_consts
          and bs = get_not_used s.x_numblocks s.x_blocks in
          let inf = nvn g in
          Is.iter
            (fun cp ->
               singleh g si_id (Constraint (Ccp cp)) ~inv:startv ~outv:inf)
            cps;
          Is.iter
            (fun tag ->
               singleh g si_id (Constraint (Ctag tag)) ~inv:startv ~outv:inf)
            bs;
          let accu = {accu with inv_acc = Some (inf,[])} in
          let out_acc =
            xlambda
              ~g ~outv ~ret_id:id ~accu ~exn lam
          in
          close_acc g ~inv_acc:out_acc ~outv
      end;
      Some (outv, [])

    | Xstringswitch ( s_id, l, o) ->
      let startv = nv Normal g in
      close_acc g ~inv_acc:accu.inv_acc ~outv:startv;
      let inv_acc = Some (startv, []) in
      let () = List.iter (fun (s,xlam) ->
          let inc_acc = simpleh s_id (Constraint (Cstring s)) ~inv_acc in
          let accu = {accu with inv_acc = inc_acc} in
          let outv_acc =
            xlambda ~g ~outv ~ret_id:id ~accu ~exn xlam
          in
          close_acc g ~inv_acc:outv_acc ~outv) l
      in
      begin
        match o with
        | None -> ()
        | Some xlam ->
          let inc_acc =
            simpleh s_id
              (Constraint (Cnotstring (List.rev_map (fun (s,_) -> s) l)))
              ~inv_acc
          in
          let accu = {accu with inv_acc = inc_acc} in
          let outv_acc =
            xlambda ~g ~outv ~ret_id:id ~accu ~exn xlam
          in
          close_acc g ~inv_acc:outv_acc ~outv
      end;
      Some (outv, [])

    | Xstaticraise ( i, args) ->
      let ( catchv, cargs, ctok) = Hashtbl.find statics i in
      let assigns = List.map2 (fun carg arg -> ( [carg], Var arg)) cargs args in
      let accu =
        {accu with inv_acc = simpleh_list assigns ~inv_acc:accu.inv_acc}
      in
      let inv_acc = endlet ctok ~accu in
      close_acc g ~inv_acc ~outv:catchv;
      None

    | Xstaticcatch ( ltry, ( i, args), lwith) ->
      let catchv = nvn g in
      let try_scope, try_tok = Scope.set_token accu.scope in
      Hashtbl.add statics i (catchv,args,try_tok);
      let try_accu = {accu with scope = try_scope} in
      let try_acc =
        xlambda ~g ~outv ~ret_id:id ~accu:try_accu ~exn ltry
      in
      let with_scope, with_tok = Scope.set_token accu.scope in
      let with_scope =
        List.fold_left (fun scp xid -> Scope.add xid scp) with_scope args
      in
      let with_accu =
        { inv_acc = Some (catchv,[]);
          scope = with_scope;
        }
      in
      let with_acc =
        xlambda
          ~g ~outv ~ret_id:id ~accu:with_accu ~exn lwith
      in
      let with_acc =
        endlet with_tok ~accu:{with_accu with inv_acc = with_acc}
      in
      close_acc g ~inv_acc:try_acc ~outv;
      close_acc g ~inv_acc:with_acc ~outv;
      Some (outv, [])

    | Xraise (_,i) -> (* TODO: different kind of raise *)
      mk_exn_hedge
        ~g
        ~inv_acc:accu.inv_acc
        ~exn
        ~scope:accu.scope
        (Var i);
      None

    | Xtrywith ( ltry, exni, lwith)  ->
      let exnv2 = nve g in
      let try_scope, try_tok = Scope.set_token accu.scope in
      let exn2 =
        { exn_id = exni;
          exnv = exnv2;
          exn_tok = try_tok;
        }
      in
      let try_accu = {accu with scope = try_scope} in
      let try_acc =
        xlambda ~g ~outv ~ret_id:id ~accu:try_accu ~exn:exn2 ltry
      in
      let with_scope, with_tok = Scope.set_token accu.scope in
      let with_accu =
        { inv_acc = Some (exnv2,[]);
          scope = with_scope;
        }
      in
      let with_acc =
        xlambda
          ~g ~outv ~ret_id:id ~accu:with_accu ~exn lwith
      in
      let with_acc =
        endlet with_tok ~accu:{with_accu with inv_acc = with_acc}
      in
      close_acc g ~inv_acc:try_acc ~outv;
      close_acc g ~inv_acc:with_acc ~outv;
      Some (outv, [])

    | Xifthenelse ( i, t, e) ->
      let inv = nvn g in
      close_acc g ~inv_acc:accu.inv_acc ~outv:inv;
      let then_accu = {accu with inv_acc = Some (inv,[[i],ctrue])} in
      let else_accu = {accu with inv_acc = Some (inv,[[i],cfalse])} in
      let then_acc =
        xlambda
          ~g ~ret_id:id ~accu:then_accu ~outv ~exn t
      in
      let else_acc =
        xlambda
          ~g ~ret_id:id ~accu:else_accu ~outv ~exn e
      in
      close_acc g ~inv_acc:then_acc ~outv;
      close_acc g ~inv_acc:else_acc ~outv;
      Some (outv,[])

    | Xwhile ( lcond, lbody) ->
      let outc = nvn g in
      let inv = nvn g in
      let test_id = mk_xid "$test" in
      let body_id = mk_xid "$body" in
      close_acc g ~inv_acc:accu.inv_acc ~outv:inv;
      let init_acc = Some (inv, []) in
      let init_accu = {accu with inv_acc = init_acc} in
      let body_acc = simpleh test_id ctrue ~inv_acc:(Some (outc,[])) in
      let body_accu = {accu with inv_acc = body_acc} in
      let cond_acc =
        xlambda
          ~g ~outv:outc ~ret_id:test_id ~accu:init_accu ~exn lcond
      in
      let body_acc =
        xlambda
          ~g ~outv:inv ~ret_id:body_id ~accu:body_accu ~exn lbody
      in
      close_acc g ~inv_acc:cond_acc ~outv:outc;
      close_acc g ~inv_acc:body_acc ~outv:inv;
      simpleh test_id cfalse ~inv_acc:(Some (outc, []))

    | Xfor ( i, start, stop, dir, lbody) ->
      let test_id = mk_xid "$test" in
      let body_id = mk_xid "$body" in
      let initv = nvn g in
      let testv = nvn g in
      let outb = nvn g in
      let o = Asttypes.( match dir with Upto -> 1 | Downto -> -1 ) in
      let init_acc = simpleh i ( Var start) ~inv_acc:accu.inv_acc in
      close_acc g ~inv_acc:init_acc ~outv:initv;
      let test_prim =
        let comp =
          Asttypes.(match dir with Upto -> Lambda.Cle | Downto -> Lambda.Cge)
        in
        Prim (XPintcomp comp, [i;stop] )
      in
      singleh g test_id test_prim ~inv:initv ~outv:testv;
      let true_acc = Some (testv, [[test_id], ctrue]) in
      let true_accu = {accu with inv_acc = true_acc} in
      let body_acc =
        xlambda
          ~g ~outv:outb ~ret_id:body_id ~accu:true_accu ~exn lbody
      in
      let offset_acc =
        simpleh i ( Prim ( XPoffsetint o, [i])) ~inv_acc:body_acc
      in
      close_acc g ~inv_acc:offset_acc ~outv:initv;
      Some (testv, [[test_id], cfalse])

    | Xlazyforce i ->
      exn_prim g ~accu ~outv ~exn ~id (Lazyforce i)
    | Xccall ( p, l ) ->
      exn_prim g ~accu ~outv ~exn ~id (Ccall (p,l))
    | Xsend ( _, o, m ) ->
      exn_prim g ~accu ~outv ~exn ~id (Send(o,m))
  in

  let accu = { inv_acc = Some (inv, []); scope } in

  let acc =
    xlambda ~g ~outv ~ret_id ~accu ~exn code
  in
  close_acc g ~inv_acc:acc ~outv


let init ~modulename funs =
  let nv = nv ~modulename in
  let nvn = nv Normal and nve = nv Exception in
  let mk_xid name = ( modulename, Id.create ~name () ) in
  let g = create () in
  let nf = Hashtbl.length funs in

  let fun_descs = Hashtbl.create nf in

  Hashtbl.iter
    begin
      fun i flam ->
        let g = create () in
        let f_graph = g in
        let f_in = [| nvn g |]
        and f_out = [| nvn g; nve g |] in
        let f_return = mk_xid "#$return" and f_exn = mk_xid "#$exn" in
        let outv = nvn g and exnv = nve g in
        add_hedge g (Hedge.mk () ) [ [], Return f_return ]
          ~pred:[|outv|] ~succ:[|f_out.(0)|];
        add_hedge g (Hedge.mk () ) [ [], Retexn f_exn ]
          ~pred:[|exnv|] ~succ:[|f_out.(1)|];
        let scope, exn_tok = Scope.set_token (Scope.empty) in
        let exn =
          { exn_id = f_exn;
            exnv;
            exn_tok;
          }
        in
        xlambda ~g
          ~mk_xid
          ~modulename
          ~inv:f_in.(0)
          ~outv ~exn
          ~scope
          ~ret_id:f_return
          flam;
        let f_vertex =
              VertexSet.remove f_in.(0) (
                VertexSet.remove f_out.(0) (
                  VertexSet.remove f_out.(1) (
                    ( List.fold_left
                        (fun vs v -> VertexSet.add v vs )
                        VertexSet.empty
                        ( list_vertex f_graph )
                    )))) in
        Array.iter (fun v -> assert(not (VertexSet.mem v f_vertex))) f_in;
        Hashtbl.add fun_descs i
          {
            f_graph; f_in; f_out;
            f_vertex;
            f_hedge = List.fold_left
                (fun hs h -> HedgeSet.add h hs )
                HedgeSet.empty
                ( list_hedge f_graph );
          }
    end
    funs;
  ( g, fun_descs )

let mk_subgraph ~g ~modulename ~exn_id main =
  let nv = nv ~modulename in
  let nvn = nv Normal and nve = nv Exception in
  let inv = nvn g and outv = nvn g and exnv = nve g in
  let mk_xid name = ( modulename, Id.create ~name () ) in
  let ret_id = mk_xid "$ret" in
  let scope, exn_tok = Scope.set_token (Scope.empty) in
  let exn =
    { exn_id;
      exnv;
      exn_tok;
    }
  in
  xlambda ~g ~mk_xid ~modulename ~inv ~outv ~exn ~scope ~ret_id main;
  { m_in = inv; m_out = outv; m_exn = exnv; m_return = ret_id; }

let mk_graph ~modulename funs tlam =
  let nv = nv ~modulename in
  let nvn = nv Normal and nve = nv Exception in
  let mk_xid name = ( modulename, Id.create ~name () ) in
  let ( g, fun_descs ) = init ~modulename funs in
  let inv = nvn g and outv = nvn g and exnv = nve g in
  let ret_id = mk_xid "$ret" in
  let scope, exn_tok = Scope.set_token (Scope.empty) in
  let exn =
    { exn_id = exn_xid;
      exnv;
      exn_tok;
    }
  in
  xlambda ~g ~mk_xid ~modulename
    ~inv ~outv ~exn ~scope
    ~ret_id tlam;
  ( g, fun_descs, inv, outv, exnv, exn_xid, ret_id )

let vattr_merge a1 a2 =
  match a1, a2 with
  | Normal, Normal
  | Exception, Exception -> a1
  | _, _ -> assert false

let merge_graphs ~g subs =
  let mv = vertex_merge g vattr_merge in
  let l = Array.length subs in
  let first = subs.(0)
  and last = subs.(pred l) in
  for i = 1 to l - 1 do
    mv subs.(pred i).m_out subs.(i).m_in;
    mv subs.(0).m_exn subs.(i).m_exn
  done;
  ( first.m_in,
    last.m_out,
    first.m_exn,
    last.m_return )
