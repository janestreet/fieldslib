module T = struct
  type t = { a : int }
end
type t = T.t = private { a : int } with fields
let _ = Fieldslib.Field.setter Fields.a
