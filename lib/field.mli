(** OCaml record field. *)

(* ['record] is the type of the record.  ['field] is the type of the
   values stored in the record field with name [name]. *)
type ('record, 'field) t = {
  name : string;
  setter : ('record -> 'field -> unit) option;
  getter : ('record -> 'field);
  fset   : ('record -> 'field -> 'record);
}

val name : (_, _) t -> string
val get  : ('r, 'a) t -> 'r -> 'a
val fset : ('r, 'a) t -> 'r -> 'a -> 'r
val setter : ('r, 'a) t -> ('r -> 'a -> unit) option

type ('record,'result) user =
    {f : 'field. ('record,'field) t -> 'result}
