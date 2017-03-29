open Pervasives

(* external ( + ) : int -> int -> int = "%addint" *)
(* external ( - ) : int -> int -> int = "%subint" *)
(* external ( = ) : 'a -> 'a -> bool = "%equal" *)
(* external ( > ) : 'a -> 'a -> bool = "%greaterthan" *)
(* external ( >= ) : 'a -> 'a -> bool = "%greaterequal" *)

(* type 'a ref = { mutable contents : 'a } *)
(* external ref : 'a -> 'a ref = "%makemutable" *)
(* external ( ! ) : 'a ref -> 'a = "%field0" *)
(* external ( := ) : 'a ref -> 'a -> unit = "%setfield0" *)
(* external incr : int ref -> unit = "%incr" *)
(* external decr : int ref -> unit = "%decr" *)

(* With function *)
(* let mccarthy n = *)
(*   let stack = ref 1 in *)
(*   let cur = ref n in *)
(*   while !stack > 0 do *)
(*     if !cur > 100 *)
(*     then begin *)
(*       cur := !cur - 10; *)
(*       decr stack *)
(*     end *)
(*     else begin *)
(*       cur := !cur + 11; *)
(*       incr stack *)
(*     end *)
(*   done; *)
(*   !cur *)

(* let () = *)
(*   let x = mccarthy 1 in *)
(*   assert (x = 91) *)

(* Simplified *)
let () =
  let stack = ref 1 in
  let cur = ref 1 in
  while !stack > 0 do
    if !cur > 100
    then begin
      cur := !cur - 10;
      decr stack
    end
    else begin
      cur := !cur + 11;
      incr stack
    end
  done;
  assert (!cur = 91)

