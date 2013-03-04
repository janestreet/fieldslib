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
end

module Private : sig
  type t = private { a : int; mutable b : int }
  with fields
  (* exporting the type u wouldn't work for now *)
end = struct
  type t = { a : int; mutable b : int }
  with fields
  module U = struct
    type u = t = private { a : int; mutable b : int }
    with fields
  end
end
let _ = Private.Fields.fold
