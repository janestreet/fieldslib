module T : sig
  type t = private { a : int } with fields
end = struct
  type t = { a : int } with fields
end
let _ = T.Fields.map
