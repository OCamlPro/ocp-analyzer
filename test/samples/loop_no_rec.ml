external ( = ) : 'a -> 'a -> bool = "%equal"
external ( + ) : int -> int -> int = "%addint"
external raise : exn -> 'a = "%raise"

type 'a t = F of ('a t -> 'a -> 'a)
let f (F g) h x = g (F g) h (h x)
let iter h x = f (F f) h x

exception Myexn

let _ = iter (fun x -> if x = 33 then raise Myexn else x+1) 0
