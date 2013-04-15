(* Generated code should depend on the environment in scope as little as
   possible.  E.g. rather than [foo = []] do [match foo with [] ->], to eliminate the
   use of [=].  It is especially important to not use polymorphic comparisons, since we
   are moving more and more to code that doesn't have them in scope. *)

module List = ListLabels
open Printf
open Camlp4.PreCast
open Pa_type_conv

(** Utility functions to construct and deconstruct camlp4 asts *)
module Create = struct
  let record _loc bindings =
    let bindings =
      List.map bindings ~f:(fun (n, v) ->
        <:rec_binding< $lid:n$ = $v$ >> ) in
    <:expr< { $list:bindings$ } >>
  ;;

  let lambda _loc patterns body =
    List.fold_right patterns ~init:body ~f:(fun pattern acc ->
      <:expr< fun [ $pattern$ -> $acc$ ] >> )
  ;;

  let lambda_sig _loc arg_tys body_ty =
    List.fold_right arg_tys ~init:body_ty ~f:(fun arg_ty acc ->
      <:ctyp< $arg_ty$ -> $acc$ >> )
  ;;
end
module Inspect = struct
  let field = function
    | <:ctyp< $lid:name$ : mutable $field_ty$ >> -> (name, `Mutable, field_ty)
    | <:ctyp< $lid:name$ : $field_ty$ >>         -> (name, `Immutable, field_ty)
    | _                                          -> assert false
  ;;

  let get_field_name fld =
    let name, _, _ = field fld in
    name

  let field_names ty = List.map ( Ast.list_of_ctyp ty [] ) ~f:get_field_name
  let fields ty = List.map (Ast.list_of_ctyp ty []) ~f:field
end

let generate_at_least_once rec_ ~f ~combine typedefs =
  if not rec_ then
    failwith "nonrec is not compatible with the `fields' preprocessor";
  let rec aux = function
    | Ast.TyDcl (_loc, ty_name, tps, rhs, _) -> f _loc ~ty_name ~tps ~rhs
    | Ast.TyAnd (_loc, td1, td2) -> (
      match aux td1, aux td2 with
      | `Ok str1, `Ok str2 -> `Ok (combine _loc str1 str2)
      | `Ok str1, `Error _ -> `Ok str1
      | `Error _, `Ok str2 -> `Ok str2
      | `Error _, `Error _ ->
        `Error "'with fields' can only be applied on type definitions in which at \
                least one type definition is a record"
    )
    | Ast.TyNil _loc ->
      `Error "'with fields': unexpected TyNil without a TyAnd somewhere around!!"
    | _ -> assert false in

  match aux typedefs with
  | `Ok res -> res
  | `Error s -> failwith s

let raise_unsupported () =
  `Error "Unsupported use of fields (you can only use it on records)."

let perm _loc private_ =
  match private_ with
  | true -> <:ctyp< [< `Read ] >>
  | false -> <:ctyp< [< `Read | `Set_and_create ] >>

let field_t _loc private_ =
  match private_ with
  | false -> <:ctyp< Fieldslib.Field.t >>
  | true -> <:ctyp< Fieldslib.Field.readonly_t >>

module Gen_sig = struct
  let apply_type _loc ~ty_name ~tps =
    List.fold_left tps
      ~init:<:ctyp< $lid:ty_name$ >>
      ~f:(fun acc tp -> <:ctyp< $acc$ $tp$ >>)

  let label_arg _loc name ty = Ast.TyLab (_loc, name, ty)

  let field_arg _loc ~private_ ~record f = fun (name, _m, ty) ->
    label_arg _loc name (
      f ~field: <:ctyp< $field_t _loc private_$ $record$ $ty$ >> ~ty)
  ;;

  let create_fun ~ty_name ~tps _loc ty =
    let record = apply_type _loc ~ty_name ~tps in
    let fields = Inspect.fields ty in
    let f = field_arg _loc ~private_:false ~record (fun ~field ~ty ->
      let create_f = <:ctyp< 'input__ -> ( $ty$ ) >> in
      <:ctyp< $field$ -> 'compile_acc__ -> ($create_f$ * 'compile_acc__) >>
    )  in
    let types   = List.map fields ~f in
    let create_record_f = <:ctyp< 'input__ -> ($record$) >> in
    let t = Create.lambda_sig _loc
      (types @ [ <:ctyp< 'compile_acc__ >> ]) (<:ctyp< ( $create_record_f$ * 'compile_acc__ ) >>)
    in
    <:sig_item< value make_creator : $t$ >>
  ;;


  let fold_fun ~private_ ~ty_name ~tps _loc ty =
    let record  = apply_type _loc ~ty_name ~tps in
    let fields  = Inspect.fields ty in
    let f = field_arg _loc ~private_ ~record (fun ~field ~ty:_ ->
      <:ctyp< 'acc__ -> $field$ -> 'acc__ >>) in
    let types   = List.map fields ~f in
    let init_ty = label_arg _loc "init" <:ctyp< 'acc__ >> in
    let t       = Create.lambda_sig _loc
      (init_ty :: types) <:ctyp< 'acc__ >> in
    <:sig_item< value fold : $t$ >>
  ;;

  let simple_create_fun ~ty_name ~tps _loc ty =
    let record  = apply_type _loc ~ty_name ~tps in
    let fields  = Inspect.fields ty in
    let f (name,_,ty)  = label_arg _loc name ty in
    let types   = List.map fields ~f in
    let t       = Create.lambda_sig _loc
      types record in
    <:sig_item< value create : $t$ >>
  ;;



  let bool_fun fun_name ~private_ ~ty_name ~tps _loc ty =
    let record    = apply_type _loc ~ty_name ~tps in
    let fields    = Inspect.fields ty in
    let f = field_arg _loc ~private_ ~record (fun ~field ~ty:_ ->
      <:ctyp< $field$ -> bool >> ) in
    let types   = List.map fields ~f in
    let t       = Create.lambda_sig _loc types <:ctyp< bool >> in
    <:sig_item< value $lid:fun_name$ : $t$ >>
  ;;

  let iter_fun ~private_ ~ty_name ~tps _loc ty =
    let record    = apply_type _loc ~ty_name ~tps in
    let fields    = Inspect.fields ty in
    let f = field_arg _loc ~private_ ~record (fun ~field ~ty:_ ->
      <:ctyp< $field$ -> unit >>) in
    let types  = List.map fields ~f in
    let t     = Create.lambda_sig _loc types <:ctyp< unit >> in
    <:sig_item< value iter : $t$ >>
  ;;

  let direct_iter_fun ~private_ ~ty_name ~tps _loc ty =
    let record    = apply_type _loc ~ty_name ~tps in
    let fields    = Inspect.fields ty in
    let f = field_arg _loc ~private_ ~record (fun ~field ~ty:field_ty ->
      <:ctyp< $field$ -> $record$ -> $field_ty$ -> unit >>) in
    let types  = List.map fields ~f in
    let t     = Create.lambda_sig _loc (record :: types) <:ctyp< unit >> in
    <:sig_item< value iter : $t$ >>
  ;;

  let direct_fold_fun ~private_ ~ty_name ~tps _loc ty =
    let record  = apply_type _loc ~ty_name ~tps in
    let fields  = Inspect.fields ty in
    let f = field_arg _loc ~private_ ~record (fun ~field ~ty:field_ty ->
      <:ctyp< 'acc__ -> $field$ -> $record$ -> $field_ty$ -> 'acc__ >>) in
    let types   = List.map fields ~f in
    let init_ty = label_arg _loc "init" <:ctyp< 'acc__ >> in
    let t       = Create.lambda_sig _loc
      (record :: init_ty :: types) <:ctyp< 'acc__ >> in
    <:sig_item< value fold : $t$ >>
  ;;

  let to_list_fun ~private_ ~ty_name ~tps _loc ty =
    let record    = apply_type _loc ~ty_name ~tps in
    let fields    = Inspect.fields ty in
    let f = field_arg _loc ~private_ ~record (fun ~field ~ty:_ ->
        <:ctyp< $field$ -> 'elem__ >>)
    in
    let types = List.map fields ~f in
    let t     = Create.lambda_sig _loc types <:ctyp< list 'elem__ >> in
    <:sig_item< value to_list : $t$ >>
  ;;

  let map_fun ~ty_name ~tps _loc ty =
    let record    = apply_type _loc ~ty_name ~tps in
    let fields    = Inspect.fields ty in
    let f = field_arg _loc ~private_:false ~record (fun ~field ~ty ->
      <:ctyp< $field$ -> $ty$ >>) in
    let types = List.map fields ~f in
    let t     = Create.lambda_sig _loc (types) record in
    <:sig_item< value map : $t$ >>
  ;;


 let map_poly ~private_ ~ty_name ~tps _loc _ =
   let record    = apply_type _loc ~ty_name ~tps in
   let tps_names =
     List.map
       ~f:(function
         | <:ctyp< '$a$ >> -> a
         | _ -> assert false)
       tps
   in
    let fresh_variable =
      let rec loop i =
        let ret = sprintf "x%i" i in
        if List.mem ret ~set:tps_names then
          loop (i+1)
        else
          ret
      in
      <:ctyp<'$lid:loop 0$>>
    in
    let perm = perm _loc private_ in
    let t =
      <:ctyp< Fieldslib.Field.user $perm$ $record$ $fresh_variable$ -> list $fresh_variable$  >>
    in
    <:sig_item< value map_poly : $t$ >>
 ;;



  let record ~private_ ~ty_name ~tps _loc ty =
    let fields = Inspect.fields ty in
    let record_ty = apply_type _loc ~ty_name ~tps in
    let conv_field (res_getset, res_fields) (name, m, ty) =
      let getter = <:sig_item< value $lid:name$ : $record_ty$ -> $ty$ >> in
      let field  =
        <:sig_item< value $lid:name$ : $field_t _loc private_$ $record_ty$ $ty$ >>
      in
      match m, private_ with
      | `Immutable, _
      | `Mutable, true ->
          ( <:sig_item< $getter$ ; $res_getset$ >> ,
            <:sig_item< $field$ ; $res_fields$ >>
          )
      | `Mutable, false ->
          let setter    =
            <:sig_item< value $lid:"set_" ^ name$ : $record_ty$ -> $ty$ -> unit >> in
          ( <:sig_item< $getter$ ; $setter$ ;  $res_getset$ >> ,
            <:sig_item< $field$ ; $res_fields$ >>
          )
    in
    let getters_and_setters, fields =
      List.fold_left fields ~init:(<:sig_item<>>, <:sig_item<>>) ~f:conv_field
    in
    let create_fun = create_fun ~ty_name ~tps _loc ty in
    let simple_create_fun = simple_create_fun ~ty_name ~tps _loc ty in
    let fields_module = if ty_name = "t" then "Fields" else "Fields_of_" ^ ty_name in
    let iter        = iter_fun ~private_ ~ty_name ~tps _loc ty in
    let fold        = fold_fun ~private_ ~ty_name ~tps _loc ty in
    let map         = map_fun ~ty_name ~tps _loc ty in
    let map_poly    = map_poly ~private_ ~ty_name ~tps _loc ty in
    let and_f       = bool_fun "for_all" ~private_ ~ty_name ~tps _loc ty in
    let or_f        = bool_fun "exists" ~private_ ~ty_name ~tps _loc ty in
    let to_list     = to_list_fun ~private_ ~ty_name ~tps _loc ty in
    let direct_iter = direct_iter_fun ~private_ ~ty_name ~tps _loc ty in
    let direct_fold = direct_fold_fun ~private_ ~ty_name ~tps _loc ty in
    <:sig_item< $getters_and_setters$ ;
        module $uid:fields_module$ : sig
          value names : list string ;
          $fields$ ;
          $fold$ ;
          $ if private_
            (* The ['perm] phantom type prohibits first-class fields from mutating or
               creating private records, so we can expose them (and fold, etc.).

               However, we still can't expose functions that explicitly create private
               records.
            *)
            then <:sig_item< >>
            else <:sig_item< $create_fun$ ; $simple_create_fun$ ; $map$ ; >>
          $ ;
          $iter$ ; $and_f$ ; $or_f$ ; $to_list$ ; $map_poly$ ;
          module Direct : sig
            $direct_iter$ ;
            $direct_fold$ ;
          end ;
        end
    >>
  ;;

  let fields_of_ty_sig _loc ~ty_name ~tps ~rhs =
    match rhs with
    | <:ctyp@loc< $_$ == private { $flds$ } >>
    | <:ctyp@loc< private { $flds$ } >> ->
      `Ok (record ~ty_name ~private_:true ~tps loc flds)
    | <:ctyp@loc< $_$ == { $flds$ } >>
    | <:ctyp@loc< { $flds$ } >> ->
      `Ok (record ~ty_name ~private_:false ~tps loc flds)
    | _ -> raise_unsupported ()

  let generate rec_ typedefs =
    generate_at_least_once
      rec_
      ~f:fields_of_ty_sig
      ~combine:(fun _loc item1 item2 -> <:sig_item< $item1$; $item2$; >>)
      typedefs

end

module Gen_struct = struct
  let fields ~private_ _loc ty =
    let fields = Inspect.fields ty in
    let rec_id =
      match fields with
      | [_] -> Ast.ExNil _loc
      | _ -> Ast.ExId (_loc, Ast.IdLid (_loc, "_r__"))
    in
    let conv_field (res_getset, res_fields) (name, m, field_ty) =
      let getter = <:str_item< value $lid:name$ _r__ = _r__.$lid:name$ >> in
      let setter, setter_field =
        match m, private_ with
        | `Mutable, true ->
          <:str_item< >>,
          <:expr< Some (fun _ _ -> failwith "invalid call to a setter of a private type") >>
        | `Mutable, false ->
            let setter =
              <:str_item<
                value $lid:"set_" ^ name$ _r__ v__ = _r__.$lid:name$ := v__
              >>
            in
            let setter_field = <:expr< Some $lid:"set_" ^ name$ >> in
            setter, setter_field
        | `Immutable, _ -> <:str_item< >>, <:expr< None >>
      in
      let field  =
        let e =
          Ast.ExRec (_loc, Ast.RbEq (_loc,
              Ast.IdLid (_loc, name),
              Ast.ExId (_loc, Ast.IdLid (_loc, "v__"))),
            rec_id)
        in
        let fset =
          match private_ with
          | true ->
            <:expr< fun _ _ -> failwith "Invalid call to an fsetter of a private type" >>
          | false -> <:expr< fun _r__ v__ -> $e$ >>
        in
        let perm = perm _loc private_ in
        <:str_item<
          value $lid:name$ : Fieldslib.Field.t_with_perm $perm$ _ $field_ty$ =
              Fieldslib.Field.Field { Fieldslib.Field.For_generated_code.
                force_variance = (fun (_ : $perm$) -> ());
                name = $str:name$;
                getter = $lid:name$;
                setter = $setter_field$;
                fset = $fset$;
              }
        >>
      in
      ( <:str_item< $getter$ ; $setter$ ; $res_getset$ >>,
        <:str_item< $field$ ; $res_fields$ >> )
    in
    List.fold_left fields ~init:(<:str_item<>>, <:str_item<>>) ~f:conv_field
  ;;

  let label_arg ?label _loc name =
    let l =
      match label with
      | None    -> name
      | Some n  -> n in
    Ast.PaLab (_loc, l, <:patt< $lid:name$ >> )
  ;;

  let label_arg_fun _loc name =
    label_arg ~label:name _loc (name ^ "_fun__")
  ;;

  let creation_fun _loc _record_name ty =
    let names = Inspect.field_names ty  in
    let f =
      (* create creation function *)
      let body_record = Create.record _loc
        (List.map names ~f:(fun n -> (n, <:expr< $lid:n$ >> ))) in
      let body =
        List.fold_right names
          ~init: <:expr< ( $body_record$ ) >>
          ~f: (fun field_name acc ->
          <:expr<
            let $lid:field_name$ =
              $lid:field_name ^ "_gen__"$ acc__
            in
            $acc$
          >> )
      in
      Create.lambda _loc [ <:patt< acc__ >> ] body
    in
    let patterns = List.map names ~f:(label_arg_fun _loc) in
    let body0 = <:expr< ($f$, compile_acc__) >> in
    let body  =
      List.fold_right names
        ~init: body0
        ~f: (fun field_name acc ->
          <:expr<
            let ($lid:field_name ^ "_gen__"$, compile_acc__) =
              $lid:field_name ^ "_fun__"$ $lid:field_name$ compile_acc__
            in
            $acc$
          >>)
    in
    let f    = Create.lambda _loc (patterns @ [ <:patt< compile_acc__ >> ]) body in
    <:str_item<
      value make_creator = $f$;
    >>
  ;;


  let simple_creation_fun _loc _record_name ty =
    let names = Inspect.field_names ty  in
    let f =
      Create.record _loc
        (List.map names ~f:(fun n -> (n, <:expr< $lid:n$ >> )))
    in
    let patterns = List.map names ~f:(fun x -> label_arg _loc x ) in
    let f    = Create.lambda _loc patterns f  in
    <:str_item<
      value create = $f$;
    >>
  ;;

  let fold_fun ~record_name:_ _loc ty =
    let names     = Inspect.field_names ty in
    let field_fold acc_expr field_name =
      <:expr< $lid:field_name ^ "_fun__" $ $acc_expr$ $lid:field_name$ >> in
    let body =
      List.fold_left names ~init:<:expr< init__ >> ~f:field_fold in
    let patterns = List.map names ~f:(label_arg_fun _loc) in
    let init     = label_arg ~label:"init" _loc "init__" in
    let lambda = Create.lambda _loc
      ( init :: patterns ) body in
    <:str_item<
      value fold = $lambda$;
    >>
  ;;

  let and_fun ~record_name:_ _loc ty =
    let names     = Inspect.field_names ty in
    let field_fold acc_expr field_name =
      <:expr< $acc_expr$ && $lid:field_name ^ "_fun__"$ $lid:field_name$ >> in
    let body =
      List.fold_left names ~init:<:expr< True >> ~f:field_fold in
    let patterns = List.map names ~f:(label_arg_fun _loc) in
    let lambda = Create.lambda _loc patterns body in
    <:str_item<
      value for_all = $lambda$;
    >>
  ;;

  let or_fun ~record_name:_ _loc ty =
    let names     = Inspect.field_names ty in
    let field_fold acc_expr field_name =
      <:expr< $acc_expr$ || $lid:field_name ^ "_fun__"$ $lid:field_name$ >> in
    let body =
      List.fold_left names ~init:<:expr< False >> ~f:field_fold in
    let patterns = List.map names ~f:(label_arg_fun _loc) in
    let lambda = Create.lambda _loc patterns body in
    <:str_item<
      value exists = $lambda$;
    >>
  ;;

  let iter_fun ~record_name:_ _loc ty =
    let names    = Inspect.field_names ty in
    let iter_field field_name =
      <:expr< $lid:field_name ^ "_fun__" $ $lid:field_name$ >> in
    let body    =
      List.fold_left (List.tl names)
        ~init:(iter_field (List.hd names))
        ~f:(fun acc n -> <:expr< ( $acc$ ; $iter_field n$ ) >>) in
    let patterns = List.map names ~f:(label_arg_fun _loc) in
    let lambda     = Create.lambda _loc
        (patterns) body in
    <:str_item<
      value iter = $lambda$;
    >>
  ;;

  let direct_iter_fun ~record_name:_ _loc ty =
    let names    = Inspect.field_names ty in
    let iter_field field_name =
      <:expr< $lid:field_name ^ "_fun__" $
        $lid:field_name$ record__ record__.$lid:field_name$
      >>
    in
    let body    =
      List.fold_left (List.tl names)
        ~init:(iter_field (List.hd names))
        ~f:(fun acc n -> <:expr< ( $acc$ ; $iter_field n$ ) >>) in
    let patterns = List.map names ~f:(label_arg_fun _loc) in
    let lambda     = Create.lambda _loc ( <:patt< record__ >> :: patterns) body in
    <:str_item<
      value iter = $lambda$;
    >>
  ;;

  let direct_fold_fun ~record_name:_ _loc ty =
    let names     = Inspect.field_names ty in
    let field_fold acc_expr field_name =
      <:expr< $lid:field_name ^ "_fun__" $
        $acc_expr$ $lid:field_name$ record__ record__.$lid:field_name$
      >>
    in
    let body =
      List.fold_left names ~init:<:expr< init__ >> ~f:field_fold in
    let patterns = List.map names ~f:(label_arg_fun _loc) in
    let init     = label_arg ~label:"init" _loc "init__" in
    let lambda = Create.lambda _loc
      ( <:patt< record__ >> :: init :: patterns ) body in
    <:str_item<
      value fold = $lambda$;
    >>
  ;;

  let map_fun ~record_name:_ _loc ty =
    let names    = Inspect.field_names ty in
    let patterns = List.map names ~f:(label_arg_fun _loc) in
    let body = Create.record _loc (List.map names ~f:(fun field_name ->
      let e = <:expr< $lid:field_name ^ "_fun__"$ $lid:field_name$ >> in
      (field_name, e ))) in
    let f    = Create.lambda _loc patterns body in
    <:str_item<
      value map = $f$;
    >>
  ;;

  let to_list_fun ~record_name:_ _loc ty =
    let names    = Inspect.field_names ty in
    let patterns = List.map names ~f:(label_arg_fun _loc) in
    let body    =
      List.fold_right names ~init:<:expr< [ ] >> ~f:(fun field_name tail ->
        <:expr< [ $lid:field_name ^ "_fun__" $ $lid:field_name$  :: $tail$ ] >> )
    in
    let f        = Create.lambda _loc patterns body in
    <:str_item<
      value to_list = $f$;
    >>
  ;;

  let map_poly ~record_name:_ _loc ty =
    let names    = Inspect.field_names ty in
    let fold name acc =
      <:expr< [ (record__.Fieldslib.Field.f $lid:name$) :: $acc$ ] >>
    in
    let body =
      List.fold_right
        names
        ~init:<:expr<[]>>
        ~f:fold
    in
    <:str_item<
      value map_poly record__ = $body$;
    >>
  ;;

  let record ~private_ ~record_name _loc ty =
    let getter_and_setters, fields = fields ~private_ _loc ty in
    let create   = creation_fun _loc record_name ty in
    let simple_create   = simple_creation_fun _loc record_name ty in
    let names =
      List.fold_right (Inspect.field_names ty) ~init:<:expr< [ ] >> ~f:(fun head tail ->
        <:expr< [ $str:head$ :: $tail$ ] >>)
    in
    let fields_module = if record_name = "t" then "Fields" else "Fields_of_" ^ record_name in
    let iter        = iter_fun ~record_name _loc ty in
    let fold        = fold_fun ~record_name _loc ty in
    let map         = map_fun ~record_name _loc ty in
    let map_poly    = map_poly ~record_name _loc ty in
    let andf        = and_fun ~record_name _loc ty in
    let orf         = or_fun ~record_name _loc ty in
    let direct_iter = direct_iter_fun ~record_name _loc ty in
    let direct_fold = direct_fold_fun ~record_name _loc ty in
    let to_list     = to_list_fun ~record_name _loc ty in
    <:str_item<
      $getter_and_setters$ ;
      module $uid:fields_module$ = struct
        value names = $names$ ;
        $fields$;
        $ if private_
          then <:str_item< >>
          else <:str_item< $create$ ; $simple_create$; $map$; >>
        $ ;
        $iter$ ; $fold$ ; $map_poly$ ;
        $andf$ ; $orf$ ; $to_list$ ;
        module Direct = struct
          $direct_iter$ ;
          $direct_fold$ ;
        end ;
      end
    >>
  ;;

  let fields_of_ty _loc ~ty_name:record_name ~tps:_ ~rhs =
    match rhs with
    | <:ctyp@loc< $_$ == private { $flds$ } >>
    | <:ctyp@loc< private { $flds$ } >> ->
      `Ok (record ~record_name ~private_:true loc flds)
    | <:ctyp@loc< $_$ == { $flds$ } >>
    | <:ctyp@loc< { $flds$ } >> ->
      `Ok (record ~record_name ~private_:false loc flds)
    | _ -> raise_unsupported ()

  let generate rec_ typedefs =
    generate_at_least_once
      rec_
      ~f:fields_of_ty
      ~combine:(fun _loc item1 item2 -> <:str_item< $item1$; $item2$; >>)
      typedefs
end

let () = add_generator "fields" Gen_struct.generate
let () = add_sig_generator "fields" Gen_sig.generate
