open Pervasives

let prout = 550
let test w = w + 112 + prout

let rec f x =
  if x = 1
  then x
  else 2 + f x

let rec g x = 
  match x with
  | Some y -> y
  | None -> h (Some 3)
and h x =
  match x with
  | Some y -> y
  | None -> g (Some 4)

let c = 42

let rec l42 = c :: l42

let rec l = 0::l

let rec x = let y = (c+c) in y :: x;;
