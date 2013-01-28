type ('record, 'field) t = {
  name : string;
  setter : ('record -> 'field -> unit) option;
  getter : ('record -> 'field);
  fset   : ('record -> 'field -> 'record);
}

let name f = f.name

let get f r = f.getter r

let fset (f : ('r, 'f) t) (r : 'r) (v : 'f) = f.fset r v

let setter f = f.setter

type ('record,'result) user =
  {f : 'field. ('record,'field) t -> 'result}
