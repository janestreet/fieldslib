This library defines a syntax extension for OCaml using Camlp4 that
can be used to define first class values representing record fields,
and additional routines, to get and set record fields, iterate and
fold over all fields of a record and create new record values.

Basic Usage
===========

If you define a type as follows:

    type t = {
      dir : [ `Buy | `Sell ];
      quantity : int;
      price : float;
      mutable cancelled : bool;
    } with fields

then the following code will be generated:

    (* getters *)
    val cancelled : t -> bool            
    val price     : t -> float           
    val quantity  : t -> int             
    val dir       : t -> [ `Buy | `Sell ]

    (* setters *)
    val set_cancelled : t -> bool -> unit

    (* higher order fields and functions over all fields *)
    module Fields : sig
      val cancelled : (t, bool            ) Field.t
      val price     : (t, float           ) Field.t
      val quantity  : (t, int             ) Field.t
      val dir       : (t, [ `Buy | `Sell ]) Field.t

      val fold :
        init:'a
        -> dir      :('a -> (t, [ `Buy | `Sell ]) Field.t -> 'b)
        -> quantity :('b -> (t, int             ) Field.t -> 'c)
        -> price    :('c -> (t, float           ) Field.t -> 'd)
        -> cancelled:('d -> (t, bool            ) Field.t -> 'e)
        -> 'e

      val iter :
        dir         :((t, [ `Buy | `Sell ]) Field.t -> unit)
        -> quantity :((t, int             ) Field.t -> unit)
        -> price    :((t, float           ) Field.t -> unit)
        -> cancelled:((t, bool            ) Field.t -> unit)
        -> unit

      val map :
        dir         :((t, [ `Buy | `Sell ]) Field.t -> [ `Buy | `Sell ])
        -> quantity :((t, int             ) Field.t -> int)
        -> price    :((t, float           ) Field.t -> float)
        -> cancelled:((t, bool            ) Field.t -> bool)
        -> t

      val for_all :
        dir         :((t, [ `Buy | `Sell ]) Field.t -> bool)
        -> quantity :((t, int             ) Field.t -> bool)
        -> price    :((t, float           ) Field.t -> bool)
        -> cancelled:((t, bool            ) Field.t -> bool)
        -> bool

      val exists :
        dir         :((t, [ `Buy | `Sell ]) Field.t -> bool)
        -> quantity :((t, int             ) Field.t -> bool)
        -> price    :((t, float           ) Field.t -> bool)
        -> cancelled:((t, bool            ) Field.t -> bool)
        -> bool

      (* DEPRECATED *)
      val make_creator :
           dir      :((t, [ `Buy | `Sell ]) Field.t -> 'a -> ('b -> [ `Buy | `Sell ]) * 'c)
        -> quantity :((t, int)   Field.t -> 'c -> ('b -> int) * 'd)
        -> price    :((t, float) Field.t -> 'd -> ('b -> float) * 'e)
        -> cancelled:((t, bool)  Field.t -> 'e -> ('b -> bool) * 'f)
        -> 'a
        -> ('b -> t) * 'f 


    end

Here's the type of a Field as defined in Fieldslib.

    module Field : sig
        type ('record, 'field) t = {
          name : string;
          setter : ('record -> 'field -> unit) option;
          fset   : ('record -> 'field -> 'record);
          getter : 'record -> 'field;
          rank   : int;
        }
        val name : (_, _) t -> string
        val get : ('record, 'field) t -> 'record -> 'field
        val setter : ('record, 'field) t -> ('record -> 'field -> unit) option
    end

Functions over all fields
=========================

These are useful to check exhaustiveness wrt. record fields.  For
example if you are writing a custom equality operator to ignore small
price differences:

    let ( = ) a b : bool =
      let use op = fun field ->
        op (Field.get field a) (Field.get field b)
      in
      let price_equal p1 p2 = abs_float (p1 -. p2) < 0.001 in
      Fields.for_all
        ~dir:(use (=)) ~quantity:(use (=))
        ~price:(use price_equal) ~cancelled:(use (=))
    ;;

A type error would occur if you were to add a new field and not change
the definition of ( = ):

    type t = {
      dir : [ `Buy | `Sell ];
      quantity : int;
      price : float;
      mutable cancelled : bool;
      symbol : string;
    } with fields

    ...

    Error: This expression has type
            symbol:((t, string) Field.t -> bool) -> bool
           but an expression was expected of type
            bool


Or similarly you could use fold to create to_string function:

    let to_string t =
      let conv to_s = fun acc f ->
        (sprintf "%s: %s" (Field.name f) (to_s (Field.get f t))) :: acc
      in
      let fs =
        Fields.fold ~init:[]
          ~dir:(conv (function `Buy -> "Buy" | `Sell -> "Sell"))
          ~quantity:(conv string_of_int)
          ~price:(conv string_of_float)
          ~cancelled:(conv string_of_bool)
      in
      String.concat fs ~sep:", "
    ;;

Further functions exist for boolean logic over fields (for_all,
exists), map a set of functions that return the field values into a
record (map) and to iterate over a elements of a record (iter).
