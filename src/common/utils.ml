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


module type Empty = sig end

module type BaseId = sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val name : t -> string option
  val to_string : t -> string
  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
end

module type Id = sig
  include BaseId
  val create : ?name:string -> unit -> t
end

(* functor to declare a new identifier kind *)
module MakeId(E:Empty) : Id = struct
  type t = int * string
  let empty_string = ""
  let create = let r = ref 0 in
    fun  ?(name=empty_string) () -> incr r; !r, name
  let equal (t1,_) (t2,_) = (t1:int) = t2
  let compare (t1,_) (t2,_) = t1 - t2
  let hash (t,_) = t
  let name (_,name) =
    if name == empty_string
    then None
    else Some name
  let to_string (t,name) =
    if name == empty_string
    then string_of_int t
    else Printf.sprintf "%s_%i" name t
  let output fd t = output_string fd (to_string t)
  let print ppf v = Format.pp_print_string ppf (to_string v)
end

module Set =
struct

  module type OrderedType = 
  sig
    include Set.OrderedType
    val print : Format.formatter -> t -> unit
  end

  module type S =
  sig
    include Set.S
    val print : Format.formatter -> t -> unit
    val print_sep :
      ( Format.formatter -> unit) ->
      Format.formatter ->
      t ->
      unit
  end

  module Make ( Ord : OrderedType ) :
    S with type elt = Ord.t
       and type t = Set.Make(Ord).t
  =
  struct
    include Set.Make ( Ord )
    let print pp = iter (Ord.print pp)
    let print_sep f pp s =
      if is_empty s
      then ()
      else (
        let e = choose s in
        Ord.print pp e;
        iter (fun e -> f pp; Ord.print pp e) (remove e s)
      )
  end

end



module Map =
struct

  module type OrderedType = 
  sig
    include Map.OrderedType
    val print : Format.formatter -> t -> unit
  end

  module type S =
  sig
    include Map.S
    val print :
      ( Format.formatter -> 'a -> unit ) ->
      Format.formatter ->
      'a t ->
      unit
    val print_sep :
      ( Format.formatter -> unit) ->
      ( Format.formatter -> 'a -> unit ) ->
      Format.formatter ->
      'a t ->
      unit
  end

  module Make ( Ord : OrderedType ) :
    S with type key = Ord.t
       and type 'a t = 'a Map.Make(Ord).t
  =
  struct
    include Map.Make ( Ord )
    let print f pp = iter (fun k e -> Format.fprintf pp "@[%a -> %a@]@ " Ord.print k f e)
    let print_sep fsep f pp s =
      if is_empty s
      then ()
      else (
        let (k,e) = choose s in
        Ord.print pp k; f pp e;
        iter
          (fun k e -> fsep pp; Ord.print pp k; f pp e)
          (remove k s)
      )
  end

end


module Htbl =
struct

  module type HashedType = 
  sig
    include Hashtbl.HashedType
    val print : Format.formatter -> t -> unit
  end

  module type S =
  sig
    include Hashtbl.S
    val print :
      ( Format.formatter -> 'a -> unit ) ->
      Format.formatter ->
      'a t ->
      unit
    val print_sep :
      ( Format.formatter -> unit) ->
      ( Format.formatter -> 'a -> unit ) ->
      Format.formatter ->
      'a t ->
      unit
  end

  module Make ( H : HashedType ) :
    S with type key = H.t
       and type 'a t = 'a Hashtbl.Make(H).t
  =
  struct
    include Hashtbl.Make ( H )
    let print f pp = iter (fun k e -> Format.fprintf pp "@[%a -> %a@]@ " H.print k f e)
    let print_sep fsep f pp s =
      let first = ref true in
      iter
        (fun k e ->
           if !first
           then first := false
           else fsep pp;
           H.print pp k;
           f pp e)
        s
  end

end

module type Key =
sig
  type t
  include Hashtbl.HashedType with type t := t
  include Map.OrderedType with type t := t
  include Set.OrderedType with type t := t
end

module type UKey =
sig
  type t
  include Key with type t := t
  val make : unit -> t
end

module MakeUKey (E:Empty) : UKey =
struct
  type t = int
  let hash (x:t) = x
  let print = Format.pp_print_int
  let compare (x:t) y = Pervasives.compare x y
  let equal (x:t) y = x = y

  let key_counter = ref max_int
  let make () = incr key_counter; !key_counter
end
