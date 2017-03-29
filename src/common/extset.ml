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


module type OrderedType = Set.OrderedType

module type S =
sig
  type t
  type keys
  type +'a key_chain

  val empty: t
  val is_empty: t -> bool

  val mem: ( t -> bool ) key_chain
  val add: ( t -> t ) key_chain
  val singleton: t key_chain
  val remove: ( t -> t ) key_chain
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val iter: (unit key_chain) -> t -> unit
  val fold: (( 'a -> 'a ) key_chain) -> t -> 'a -> 'a
  val for_all: (bool key_chain) -> t -> bool
  val exists: (bool key_chain) -> t -> bool
  val filter: (bool key_chain) -> t -> t
  val partition: (bool key_chain) -> t -> t * t
  val cardinal: t -> int
  val split: ( t -> t * bool * t ) key_chain

  val fold_args : (keys -> 'a) -> 'a key_chain
  val unfold_args : 'a key_chain -> keys -> 'a

end

module Make ( Ord : OrderedType )
  : S with type t = Set.Make(Ord).t
       and type 'a key_chain = Ord.t -> 'a
=
struct

  module S = Set.Make(Ord)

  include S

  type keys = Ord.t
  type 'a key_chain = Ord.t -> 'a

  let fold_args f = f
  let unfold_args f = f

end

module Nest (Ord : OrderedType) (S : S) :
  S with type t = S.t Map.Make(Ord).t
     and type 'a key_chain = Ord.t -> 'a S.key_chain
=
struct

  module MS = Map.Make(Ord)

  type t = S.t MS.t

  type keys = Ord.t * S.keys
  type 'a key_chain = Ord.t -> 'a S.key_chain

  let fold_args (f: keys -> 'a) : 'a key_chain =
    fun k -> S.fold_args (fun rest -> f (k,rest))
  let unfold_args f (k,r) = S.unfold_args (f k) r

  let empty = MS.empty
  let is_empty m = MS.for_all (fun _ s -> S.is_empty s) m

  let mem =
    fold_args
      (fun (k,rest) m ->
         try MS.find k m |> S.unfold_args S.mem rest
         with Not_found -> false)

  let add =
    fold_args
      (fun (k,rest) m ->
         try let m' = MS.find k m in
           MS.add k ( S.unfold_args S.add rest m' ) m
         with Not_found -> MS.add k ( S.unfold_args S.singleton rest ) m)

  let singleton =
    fold_args (fun (k,rest) -> MS.singleton k @@ S.unfold_args S.singleton rest)

  let remove =
    fold_args
      (fun (k,rest) m ->
         try let s = MS.find k m in
           MS.add k (S.unfold_args S.remove rest s) m
         with Not_found -> m)

  let union m m' =
    MS.merge (fun _ o o' ->
        match o,o' with
        | None, x | x, None -> x
        | Some s, Some s' -> Some (S.union s s')
      ) m m'

  let inter m m' =
    MS.merge (fun _ o o' ->
        match o,o' with
        | None, _ | _, None -> None
        | Some s, Some s' -> Some (S.inter s s')
      ) m m'

  let diff m m' =
    MS.merge (fun _ o o' ->
        match o,o' with
        | None,_ | _, None -> o
        | Some s, Some s' -> Some (S.diff s s')
      ) m m'

  let compare m m' = MS.compare S.compare m m'
  let equal m m' = MS.equal S.equal m m'
  let subset m m' =
    try MS.for_all (fun k s -> S.subset s @@ MS.find k m') m
    with Not_found -> false

  let iter f m = MS.iter (fun k s -> S.iter (f k) s) m
  let fold f m acc = MS.fold (fun k s acc -> S.fold (f k) s acc) m acc
  let for_all f m = MS.for_all (fun k s -> S.for_all (f k) s) m
  let exists f m = MS.exists (fun k s -> S.exists (f k) s) m

  let filter f m =
    MS.fold (fun k s res -> MS.add k (S.filter (f k) s) res) m MS.empty
  let partition f m =
    MS.fold (fun k s (res,res') ->
        let s,s' = S.partition (f k) s in
        MS.add k s res, MS.add k s' res')
      m (MS.empty, MS.empty)

  let cardinal m = MS.fold (fun _ s res -> res + (S.cardinal s)) m 0

  let split =
    fold_args (fun (k,rest) m ->
        let minf,o,msup = MS.split k m in
        match o with
        | None -> minf,false,msup
        | Some s ->
          let sinf,b,ssup = S.unfold_args S.split rest s in
          MS.add k sinf minf, b, MS.add k ssup msup
      )


end


(* printing options *)
module type PrintableType =
sig
  type t
  val print : Format.formatter -> t -> unit
end

module type OrderedPrintableType =
sig
  type t
  include OrderedType with type t := t
  include PrintableType with type t := t
end

module type SPrintable =
sig
  type t
  include S with type t := t
  include PrintableType with type t := t
end

module MakePrint (Ord : OrderedPrintableType) :
  SPrintable
  with type t = Make(Ord).t
   and type 'a key_chain = 'a Make(Ord).key_chain
   and type keys = Make(Ord).keys
=
struct

  include Make(Ord)

  let print ppf s =
    Format.fprintf ppf "{@ @[<hv 2>%a@]}"
      (fun ppf s ->
         iter (fun v ->
             Format.fprintf ppf "%a@ "
               Ord.print v)
           s) s
end

module NestPrint (Ord : OrderedPrintableType) (S : SPrintable) :
  SPrintable
  with type t = Nest(Ord)(S).t
   and type 'a key_chain = 'a Nest(Ord)(S).key_chain
=
struct
  include Nest(Ord)(S)

  module M = Map.Make(Ord)

  let print ppf s =
    Format.fprintf ppf "{@ @[<hv 2>%a@]}"
      (fun ppf s ->
         M.iter (fun k s ->
             Format.fprintf ppf "@[%a@ ->@ %a@]"
               Ord.print k
               S.print s
           ) s
      ) s
end
