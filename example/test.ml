
type ('a,'b) t = {
  dir : 'a * 'b;
  quantity : ('a , 'b) t;
  price : int * 'a;
  mutable cancelled : bool;
} with fields

type foo = {
  a : [`Bar | `Baz of string];
  b : int;
} with fields

module Private_in_mli = struct
  type ('a,'b) t = {
    dir : 'a * 'b;
    quantity : ('a , 'b) t;
    price : int * 'a;
    mutable cancelled : bool;
  } with fields
end

module Private_in_ml = struct
  type ('a,'b) t = ('a,'b) Private_in_mli.t = private {
    dir : 'a * 'b;
    quantity : ('a , 'b) t;
    price : int * 'a;
    mutable cancelled : bool;
  } with fields
end
