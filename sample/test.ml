
type ('a,'b) t = {
  dir : 'a * 'b;
  quantity : ('a , 'b) t;
  price : int * 'a;
  mutable cancelled : bool;
(*   symbol : string;   *)
} with fields

module Private = struct
  type ('a,'b) t = private {
    dir : 'a * 'b;
    quantity : ('a , 'b) t;
    price : int * 'a;
    mutable cancelled : bool;
  (*   symbol : string;   *)
  } with fields
end
