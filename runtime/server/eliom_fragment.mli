
type +'a t

val create : closure_id:int64 -> instance_id:int64 -> _ t

exception Creation_invalid_context of int64
