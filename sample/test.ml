open Fieldslib
open Printf
open StdLabels

type ('a,'b) t = {
  dir : 'a * 'b;
  quantity : ('a , 'b) t;
  price : int * 'a;
  mutable cancelled : bool;
(*   symbol : string;   *)
} with fields
