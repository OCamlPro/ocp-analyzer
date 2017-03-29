module Tr = AltTypeTransducer.Id

module Matching = AltMatching.Make (Tr)
module Translobj = AltTranslobj.Make (Tr)
module Translcore = AltTranslcore.Make (Tr) (Matching) (Translobj)
module Translclass = AltTranslclass.Make (Tr) (Matching) (Translobj) (Translcore)
module Translmod = AltTranslmod.Make (Tr)  (Translobj) (Translcore) (Translclass)
    
