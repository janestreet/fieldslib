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

module Private : sig
  type t = private { a : int; mutable b : int }
  with fields
end = struct
  type u = { a : int; mutable b : int }
  type t = u = private { a : int; mutable b : int }
  with fields
end
