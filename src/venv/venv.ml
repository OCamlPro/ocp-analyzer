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

(* open Utils *)

(* type 'a dt = 'a DataType.t *)
open DataType
open LatticeMap

(* type 'a t = (module S with type t = 'a) *)



module SInt = MakeMap (Selm) (Int_interv)
module SCp = MakeMap (Selm) (Cps)
module SBlock = MakeMap (Selm) (Blocks)
module SArray = MakeMap (Selm) (Arrays)
module SConstant = MakeMap (Selm) (Constants)
module ISConstant = MakeMap (Intm) (SConstant)
module SFun = MakeMap (Selm) (Funs)

type 'a mkey =
  | Int : SInt.t mkey
  | Cp : SCp.t mkey
  | Block : SBlock.t mkey
  | IBlock : SBlock.t mkey
  | Array : SArray.t mkey
  | Custom :  int -> SConstant.t mkey
  | Fun : SFun.t mkey

module EnvStruct : LatticeMap.LSMaps with type 'a key = 'a mkey = struct

  type 'a key = 'a mkey

  type t =
    {
      ints : SInt.t;
      cps : SCp.t;
      blocks : SBlock.t;
      arrays : SArray.t;
      simples : ISConstant.t;
      funs : SFun.t;
    }

  let join x y =
    {
      ints = SInt.join x.ints y.ints;
      cps = SCp.join x.cps y.cps;
      blocks = SBlock.join x.blocks y.blocks;
      arrays = SArray.join x.arrays y.arrays;
      simples = ISConstant.join x.simples y.simples;
      funs = SFun.join x.funs y.funs;
    }

  let widening x y =
    {
      ints = SInt.widening x.ints y.ints;
      cps = SCp.widening x.cps y.cps;
      blocks = SBlock.widening x.blocks y.blocks;
      arrays = SArray.widening x.arrays y.arrays;
      simples = ISConstant.widening x.simples y.simples;
      funs = SFun.widening x.funs y.funs;
    }
  let meet x y =
    {
      ints = SInt.meet x.ints y.ints;
      cps = SCp.meet x.cps y.cps;
      blocks = SBlock.meet x.blocks y.blocks;
      arrays = SArray.meet x.arrays y.arrays;
      simples = ISConstant.meet x.simples y.simples;
      funs = SFun.meet x.funs y.funs;
    }
    
  let bottom =
    {
      ints = SInt.bottom;
      cps = SCp.bottom;
      blocks = SBlock.bottom;
      arrays = SArray.bottom;
      simples = ISConstant.bottom;
      funs = SFun.bottom
    }

  let is_bottom { ints; cps; blocks; arrays; simples; funs; } =
    SInt.is_bottom ints &&
    SCp.is_bottom cps &&
    SBlock.is_bottom blocks &&
    SArray.is_bottom arrays &&
    ISConstant.is_bottom simples &&
    SFun.is_bottom funs
  
  let leq x y =
    SInt.leq x.ints y.ints &&
    SCp.leq x.cps y.cps &&
    SBlock.leq x.blocks y.blocks &&
    SArray.leq x.arrays y.arrays &&
    ISConstant.leq x.simples y.simples &&
    SFun.leq x.funs y.funs

  let equal x y =
    SInt.equal x.ints y.ints &&
    SCp.equal x.cps y.cps &&
    SBlock.equal x.blocks y.blocks &&
    SArray.equal x.arrays y.arrays &&
    ISConstant.equal x.simples y.simples &&
    SFun.equal x.funs y.funs
    
  let get : type a. a key -> t -> a =
    fun k { ints; cps; blocks; arrays; simples; funs; } ->
      match k with
      | Int -> ints
      | Cp -> cps
      | Block -> blocks
      | IBlock -> blocks
      | Array -> arrays
      | Custom i -> ISConstant.get i simples
      | Fun -> funs

  let set : type a. a key -> a -> t -> t =
    fun k x ({ ints; cps; blocks; arrays; simples; funs; } as r)->
      match k with
      | Int -> { r with ints = x; }
      | Cp -> { r with cps = x; }
      | Block -> { r with blocks = x; }
      | IBlock -> { r with blocks = x; }
      | Array -> { r with arrays = x; }
      | Custom i -> { r with simples = ISConstant.set i x simples; }
      | Fun -> { r with funs = x; }

end

include Bottomise ( EnvStruct )

(* module VenvCount : sig *)
(*   type t *)
(*   val compare : t -> t -> t *)
(*   val make : unit -> t *)
(* end = *)
(* struct *)
(*   type t = int *)
(*   let compare (x:int) y = Pervasives.compare *)
(*   let make = *)
(*     let c = ref max_int in *)
(*     fun () -> incr c; !c  *)
(* end *)

(* type t = emap option *)

(* let is_bottom (x:t) = x = None *)

(* let bottom = None *)

(* let empty_emap = *)
(*   { *)
(*     ints = Selm.empty; *)
(*     cps = Selm.empty; *)
(*     blocks = Selm.empty; *)
(*     arrays = Selm.empty; *)
(*     simples = Intm.empty; *)
(*   } *)

(* let empty = Some empty_emap *)

(* let fork l = l *)

(* (\* let common_point l l' = *\) *)
(* (\*   let rec aux r r' l l' = *\) *)
(* (\*     match l, l' with *\) *)
(* (\*     | [], [] -> (r,r',[]) *\) *)
(* (\*     | (_,e)::t, [] -> aux (e::r) r' t l' *\) *)
(* (\*     | [], (_,e')::t' -> aux r (e'::r') l t' *\) *)
(* (\*     | (i,e)::t, (i',e')::t' -> *\) *)
(* (\*       let c = VenvCount.compare i i' in *\) *)
(* (\*       if c = 0 *\) *)
(* (\*       then (e::r,e'::r',t) *\) *)
(* (\*       else if c < 0 *\) *)
(* (\*       then aux r (e'::r') l t' *\) *)
(* (\*       else aux (e::r) e' t l' *\) *)

(* let merge_selm f x y = Selm.merge *)
(*     (fun _ a b -> *)
(*        match a,b with *)
(*        | None, x | x, None -> x *)
(*        | Some a, Some b -> *)
(*          Some ( f a b ) *)
(*     ) x y *)

(* let merge l l' = *)
(*   match l, l' with *)
(*   | None, a | a, None -> a *)
(*   | Some e, Some e' -> *)
(*     Some *)
(*       { *)
(*         ints = merge_selm Int_interv.meet e.ints e'.ints; *)
(*         cps = merge_selm Cps.meet e.cps e'.cps; *)
(*         blocks = Selm.empty; *)
(*         arrays = Selm.empty; *)
(*         simples = Intm.empty; *)
(*       } *)

(* let widening l l' = failwith "TODO: widening" *)

(* let (!!) : t -> 'a = function *)
(*   | None -> failwith "Accessing bottom environment" *)
(*   | Some e -> e *)

(* let (!>) e : t = Some e *)

(* let get_data : type a. Sel.t -> a dt -> t -> a = *)
(*   fun s t env -> *)
(*     let e = !! env in *)
(*     match t with *)
(*     | Int -> Selm.find s e.ints *)
(*     | Cp -> Selm.find s e.cps *)
(*     | Block -> Selm.find s e.blocks *)
(*     | IBlock -> Selm.find s e.blocks *)
(*     | Array -> Selm.find s e.arrays *)
(*     | Custom i -> Selm.find s (Intm.find i e.simples) *)

(* let set_data s (Data (t,x)) env = *)
(*   let e = !! env in *)
(*   !> ( *)
(*     match t with *)
(*     | Int -> { e with ints = Selm.add s x e.ints; } *)
(*     | Cp -> { e with cps = Selm.add s x e.cps; } *)
(*     | Block -> { e with blocks = Selm.add s x e.blocks; } *)
(*     | IBlock -> { e with blocks = Selm.add s x e.blocks; } *)
(*     | Array -> { e with arrays = Selm.add s x e.arrays; } *)
(*     | Custom i -> *)
(*       { e with *)
(*         simples = Intm.add i *)
(*             (try Selm.add s x (Intm.find i e.simples) *)
(*              with Not_found -> Selm.singleton s x) *)
(*             e.simples; *)
(*       } *)
(*   ) *)

(* let join_data s (Data (t,x)) env = *)
(*   let x' = get_data s t env in *)
(*   let d = *)
(*     match t with *)
(*     | Int -> Data (t, Int_interv.join x x') *)
(*     | Cp -> Data (t, Cps.join x x') *)
(*     | Block -> Data (t, Blocks.join x x') *)
(*     | IBlock -> Data (t, Blocks.join x x') *)
(*     | Array -> Data (t, Arrays.join x x') *)
(*     | Custom _ -> Data (t, Constants.join x x') *)
(*   in *)
(*   set_data s d env *)

(* let meet_data s (Data (t,x)) env = *)
(*   let x' = get_data s t env in *)
(*   let d = *)
(*     match t with *)
(*     | Int -> Data (t, Int_interv.meet x x') *)
(*     | Cp -> Data (t, Cps.meet x x') *)
(*     | Block -> Data (t, Blocks.meet x x') *)
(*     | IBlock -> Data (t, Blocks.meet x x') *)
(*     | Array -> Data (t, Arrays.meet x x') *)
(*     | Custom _ -> Data (t, Constants.meet x x') *)
(*   in *)
(*   set_data s d env *)

