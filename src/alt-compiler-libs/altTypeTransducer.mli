
module type T = sig

  type t

  val map_type : Env.t -> Types.type_expr -> t

  val no_type : t
    
end

module Id : T with type t = ( Env.t * Types.type_expr ) option
  
