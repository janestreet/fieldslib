module Simple = struct
  type t = {x:int;w:int} with fields
  let _ = x
  let _ = w
end

module Rec = struct
  type a = {
    something1 : b;
  }
  and b = A of a
  with fields

  let _ = something1
end

module Multiple_names = struct
  type a = {
    a : int;
  }
  and b = {
    b : int;
  }
  with fields
  TEST = b { b = 1 } = 1
  TEST = a { a = 1 } = 1
  let _ = Fields_of_a.a
  let _ = Fields_of_b.b
  let _ = (Fields_of_a.a : (_, _) Fieldslib.Field.t :> (_, _) Fieldslib.Field.readonly_t)
end

module Private : sig
  type t = private { a : int; mutable b : int }
  with fields
end = struct
  type u = { a : int; mutable b : int }
  type t = u = private { a : int; mutable b : int }
  with fields
  (* let _ = Fieldslib.Field.setter Fields.a *)
end
(* let _ = Fieldslib.Field.setter Private.Fields.a *)
let _ = Private.Fields.fold
let _ = Private.Fields.a
let _ = Fieldslib.Field.name Private.Fields.a
let _ = Fieldslib.Field.get Private.Fields.a
let _ = Private.Fields.map_poly
  { Fieldslib.Field.f = (fun f -> let _ = Fieldslib.Field.get f in ())}

module Warnings : sig
  (* could generate an unused warning but for crazy reasons, only
     when the type is private *)
  type t = private { foo : int } with fields
  val foo : string
end = struct
  type t = { foo : int } with fields
  let foo = "a"
end
