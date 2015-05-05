module T = struct
  type t = { a : int }
end
type t = T.t = private { a : int } with fields
let _ = (Fields.a :> (_, _) Fieldslib.Field.t)
