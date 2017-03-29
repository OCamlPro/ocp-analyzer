external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( = ) : 'a -> 'a -> bool = "%equal"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"


let rec mccarthy n =
  if n > 100 then n - 10
  else mccarthy (mccarthy (n + 11))

let () =
  let x = mccarthy 1 in
  assert (x = 91)
