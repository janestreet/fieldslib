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
