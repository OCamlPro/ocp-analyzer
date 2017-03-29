external raise : exn -> 'a = "%raise"

let invalid_arg s = raise(Invalid_argument s)

let bool_of_string = function
  | "true" -> true
  | "false" -> false
  | _ -> invalid_arg "bool_of_string"
