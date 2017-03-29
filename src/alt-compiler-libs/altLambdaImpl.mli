module Tr : AltTypeTransducer.T with type t = (Env.t * Types.type_expr ) option

module Matching : AltMatching.S with type ty = Tr.t
module Translobj : AltTranslobj.S with type ty = Tr.t
module Translcore : AltTranslcore.S with type ty = Tr.t
module Translclass : AltTranslclass.S with type ty = Tr.t
module Translmod : AltTranslmod.S with type ty = Tr.t
    
