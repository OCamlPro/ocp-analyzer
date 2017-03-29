
module type T = sig

  type t

  val map_type : Env.t -> Types.type_expr -> t

  val no_type : t
end

module Id : T with type t = ( Env.t * Types.type_expr ) option =
struct

  type t = ( Env.t * Types.type_expr ) option

  let map_type e x : t = Some (e,x)

  let no_type = None
  
end
