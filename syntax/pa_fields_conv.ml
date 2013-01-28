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

  let string_of_ctyp ctyp =
    let module PP = Camlp4.Printers.OCaml.Make (Syntax) in
    let conv_ctyp = (new PP.printer ())#ctyp in
    let buffer    = Buffer.create 32 in
    Format.bprintf buffer "%a%!" conv_ctyp ctyp;
    let s = Buffer.contents buffer in
    s
  ;;

  let get_field_name fld =
    let name, _, _ = field fld in
    name

  let field_names ty = List.map ( Ast.list_of_ctyp ty [] ) ~f:get_field_name
  let fields ty = List.map (Ast.list_of_ctyp ty []) ~f:field
end

let raise_unsupported () =
  failwith "Unsupported use of fields (you can only use it on records)."

module Gen_sig = struct
  let apply_type _loc ~ty_name ~tps =
    List.fold_left tps
      ~init:<:ctyp< $lid:ty_name$ >>
      ~f:(fun acc tp -> <:ctyp< $acc$ $tp$ >>)

  let label_arg _loc name ty = Ast.TyLab (_loc, name, ty)

  let field_arg _loc ~record f = fun (name, _m, ty) ->
    label_arg _loc name (
      f ~field: <:ctyp< Fieldslib.Field.t $record$ $ty$ >> ~ty)
  ;;

  let create_fun ~ty_name ~tps _loc ty =
    let record = apply_type _loc ~ty_name ~tps in
    let fields = Inspect.fields ty in
    let f = field_arg _loc ~record (fun ~field ~ty ->
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


  let fold_fun ~ty_name ~tps _loc ty =
    let record  = apply_type _loc ~ty_name ~tps in
    let fields  = Inspect.fields ty in
    let f = field_arg _loc ~record (fun ~field ~ty:_ ->
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



  let bool_fun fun_name ~ty_name ~tps _loc ty =
    let record    = apply_type _loc ~ty_name ~tps in
    let fields    = Inspect.fields ty in
    let f = field_arg _loc ~record (fun ~field ~ty:_ ->
      <:ctyp< $field$ -> bool >> ) in
    let types   = List.map fields ~f in
    let t       = Create.lambda_sig _loc types <:ctyp< bool >> in
    <:sig_item< value $lid:fun_name$ : $t$ >>
  ;;

  let iter_fun ~ty_name ~tps _loc ty =
    let record    = apply_type _loc ~ty_name ~tps in
    let fields    = Inspect.fields ty in
    let f = field_arg _loc ~record (fun ~field ~ty:_ ->
      <:ctyp< $field$ -> unit >>) in
    let types  = List.map fields ~f in
    let t     = Create.lambda_sig _loc types <:ctyp< unit >> in
    <:sig_item< value iter : $t$ >>
  ;;

  let direct_iter_fun ~ty_name ~tps _loc ty =
    let record    = apply_type _loc ~ty_name ~tps in
    let fields    = Inspect.fields ty in
    let f = field_arg _loc ~record (fun ~field ~ty:field_ty ->
      <:ctyp< $field$ -> $record$ -> $field_ty$ -> unit >>) in
    let types  = List.map fields ~f in
    let t     = Create.lambda_sig _loc (record :: types) <:ctyp< unit >> in
    <:sig_item< value iter : $t$ >>
  ;;

  let direct_fold_fun ~ty_name ~tps _loc ty =
    let record  = apply_type _loc ~ty_name ~tps in
    let fields  = Inspect.fields ty in
    let f = field_arg _loc ~record (fun ~field ~ty:field_ty ->
      <:ctyp< 'acc__ -> $field$ -> $record$ -> $field_ty$ -> 'acc__ >>) in
    let types   = List.map fields ~f in
    let init_ty = label_arg _loc "init" <:ctyp< 'acc__ >> in
    let t       = Create.lambda_sig _loc
      (record :: init_ty :: types) <:ctyp< 'acc__ >> in
    <:sig_item< value fold : $t$ >>
  ;;

  let to_list_fun ~ty_name ~tps _loc ty =
    let record    = apply_type _loc ~ty_name ~tps in
    let fields    = Inspect.fields ty in
    let f = field_arg _loc ~record (fun ~field ~ty:_ ->
        <:ctyp< $field$ -> 'elem__ >>)
    in
    let types = List.map fields ~f in
    let t     = Create.lambda_sig _loc types <:ctyp< list 'elem__ >> in
    <:sig_item< value to_list : $t$ >>
  ;;

  let map_fun ~ty_name ~tps _loc ty =
    let record    = apply_type _loc ~ty_name ~tps in
    let fields    = Inspect.fields ty in
    let f = field_arg _loc ~record (fun ~field ~ty ->
      <:ctyp< $field$ -> $ty$ >>) in
    let types = List.map fields ~f in
    let t     = Create.lambda_sig _loc (types) record in
    <:sig_item< value map : $t$ >>
  ;;


 let map_poly ~ty_name ~tps _loc _ =
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
    let t =
      <:ctyp< Fieldslib.Field.user $record$ $fresh_variable$ -> list $fresh_variable$  >>
    in
    <:sig_item< value map_poly : $t$ >>
 ;;



  let record ~ty_name ~tps _loc ty =
    let fields = Inspect.fields ty in
    let record_ty = apply_type _loc ~ty_name ~tps in
    let conv_field (res_getset, res_fields) (name, m, ty) =
      let getter = <:sig_item< value $lid:name$ : $record_ty$ -> $ty$ >> in
      let field  =
        <:sig_item< value $lid:name$ : Fieldslib.Field.t $record_ty$ $ty$ >>
      in
      match m with
      | `Immutable ->
          ( <:sig_item< $getter$ ; $res_getset$ >> ,
            <:sig_item< $field$ ; $res_fields$ >>
          )
      | `Mutable ->
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
    if ty_name = "t" then
      let iter        = iter_fun ~ty_name ~tps _loc ty in
      let fold        = fold_fun ~ty_name ~tps _loc ty in
      let map         = map_fun ~ty_name ~tps _loc ty in
      let map_poly         = map_poly ~ty_name ~tps _loc ty in
      let and_f       = bool_fun "for_all" ~ty_name ~tps _loc ty in
      let or_f        = bool_fun "exists" ~ty_name ~tps _loc ty in
      let to_list     = to_list_fun ~ty_name ~tps _loc ty in
      let direct_iter = direct_iter_fun ~ty_name ~tps _loc ty in
      let direct_fold = direct_fold_fun ~ty_name ~tps _loc ty in
      <:sig_item< $getters_and_setters$ ;
          module Fields : sig
            value names : list string ;
            $fields$ ;
            $fold$ ;
            $create_fun$ ; $simple_create_fun$ ; $iter$ ; $map$ ; $map_poly$ ; $and_f$ ; $or_f$ ; $to_list$ ;
            module Direct : sig
              $direct_iter$ ;
              $direct_fold$ ;
            end ;
          end
      >>
    else
      <:sig_item< $getters_and_setters$ ;
          module Fields : sig
            $fields$
          end
      >>
  ;;

  let fields_of_ty_sig _loc ~ty_name ~tps ~rhs =
    let unsupported = (fun _ _ -> raise_unsupported ()) in
    Gen.switch_tp_def
      ~alias:unsupported
      ~sum:unsupported
      ~variants:unsupported
      ~mani:unsupported
      ~nil:(fun _ -> raise_unsupported ())
      ~record:(record ~ty_name ~tps)
      rhs

  let generate = function
    | Ast.TyDcl (_loc, ty_name, tps, rhs, _) -> fields_of_ty_sig _loc ~ty_name ~tps ~rhs
    | Ast.TyAnd (_loc, _, _) as tds    ->
        ignore (_loc, tds);
        failwith "Not supported"
    | _                             -> assert false
end

module Gen_struct = struct
  let fields _loc ty =
    let fields = Inspect.fields ty in
    let rec_id =
      match fields with
      | [_] -> Ast.ExNil _loc
      | _ -> Ast.ExId (_loc, Ast.IdLid (_loc, "_r__"))
    in
    let conv_field (res_getset, res_fields) (name, m, field_ty) =
      let getter = <:str_item< value $lid:name$ _r__ = _r__.$lid:name$ >> in
      let setter, setter_field =
        match m with
        | `Mutable ->
            let setter =
              <:str_item<
                value $lid:"set_" ^ name$ _r__ v__ = _r__.$lid:name$ := v__
              >>
            in
            let setter_field = <:expr< Some $lid:"set_" ^ name$ >> in
            setter, setter_field
        | `Immutable -> <:str_item< >>, <:expr< None >>
      in
      let field  =
        let e =
          Ast.ExRec (_loc, Ast.RbEq (_loc,
              Ast.IdLid (_loc, name),
              Ast.ExId (_loc, Ast.IdLid (_loc, "v__"))),
            rec_id)
        in
        let fset = <:expr< fun _r__ v__ -> $e$ >> in
        <:str_item<
          value $lid:name$ =
            ( { Fieldslib.Field.
              name    = $str:name$;
              getter  = $lid:name$;
              setter  = $setter_field$;
              fset    = $fset$;
            } : Fieldslib.Field.t _ $field_ty$ )
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
    <:str_item< value make_creator = $f$ >>
  ;;


  let simple_creation_fun _loc _record_name ty =
    let names = Inspect.field_names ty  in
    let f =
      Create.record _loc
        (List.map names ~f:(fun n -> (n, <:expr< $lid:n$ >> )))
    in
    let patterns = List.map names ~f:(fun x -> label_arg _loc x ) in
    let f    = Create.lambda _loc patterns f  in
    <:str_item< value create = $f$ >>
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
    <:str_item< value fold = $lambda$ >>
  ;;

  let and_fun ~record_name:_ _loc ty =
    let names     = Inspect.field_names ty in
    let field_fold acc_expr field_name =
      <:expr< $acc_expr$ && $lid:field_name ^ "_fun__"$ $lid:field_name$ >> in
    let body =
      List.fold_left names ~init:<:expr< True >> ~f:field_fold in
    let patterns = List.map names ~f:(label_arg_fun _loc) in
    let lambda = Create.lambda _loc patterns body in
    <:str_item< value for_all = $lambda$ >>
  ;;

  let or_fun ~record_name:_ _loc ty =
    let names     = Inspect.field_names ty in
    let field_fold acc_expr field_name =
      <:expr< $acc_expr$ || $lid:field_name ^ "_fun__"$ $lid:field_name$ >> in
    let body =
      List.fold_left names ~init:<:expr< False >> ~f:field_fold in
    let patterns = List.map names ~f:(label_arg_fun _loc) in
    let lambda = Create.lambda _loc patterns body in
    <:str_item< value exists = $lambda$ >>
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
    <:str_item< value iter = $lambda$ >>
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
    <:str_item< value iter = $lambda$ >>
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
    <:str_item< value fold = $lambda$ >>
  ;;

  let map_fun ~record_name:_ _loc ty =
    let names    = Inspect.field_names ty in
    let patterns = List.map names ~f:(label_arg_fun _loc) in
    let body = Create.record _loc (List.map names ~f:(fun field_name ->
      let e = <:expr< $lid:field_name ^ "_fun__"$ $lid:field_name$ >> in
      (field_name, e ))) in
    let f    = Create.lambda _loc patterns body in
    <:str_item< value map = $f$ >>
  ;;

  let to_list_fun ~record_name:_ _loc ty =
    let names    = Inspect.field_names ty in
    let patterns = List.map names ~f:(label_arg_fun _loc) in
    let body    =
      List.fold_right names ~init:<:expr< [ ] >> ~f:(fun field_name tail ->
        <:expr< [ $lid:field_name ^ "_fun__" $ $lid:field_name$  :: $tail$ ] >> )
    in
    let f        = Create.lambda _loc patterns body in
    <:str_item< value to_list = $f$ >>
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
    <:str_item< value map_poly record__ = $body$ >>
  ;;

  let record ~record_name _loc ty =
    let getter_and_setters, fields = fields _loc ty in
    let create   = creation_fun _loc record_name ty in
    let simple_create   = simple_creation_fun _loc record_name ty in
    let names =
      List.fold_right (Inspect.field_names ty) ~init:<:expr< [ ] >> ~f:(fun head tail ->
        <:expr< [ $str:head$ :: $tail$ ] >>)
    in
    if record_name = "t" then
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
        module Fields = struct
          value names = $names$ ;
          $fields$ ;
          $create$ ; $simple_create$ ; $iter$ ; $fold$ ; $map$ ; $map_poly$ ; $andf$ ; $orf$ ; $to_list$ ;
          module Direct = struct
            $direct_iter$ ;
            $direct_fold$ ;
          end ;
        end
      >>
    else
      <:str_item<
        $getter_and_setters$ ;
        module Fields = struct
          $fields$ ;
        end
      >>
  ;;

  let mani ~record_name ty =
    match ty with
    | <:ctyp@loc< { $x$ } >> ->
      record ~record_name loc x
    | _ -> failwith "the right hand side of the manifest must be a record"

  let fields_of_ty _loc ~record_name ~tps:_ ~rhs =
    let unsupported = (fun _ _ -> raise_unsupported ()) in
    Gen.switch_tp_def
      ~alias:    unsupported
      ~sum:      unsupported
      ~variants: unsupported
      ~mani:     (fun (_:Loc.t) _tp1 tp2 -> mani ~record_name tp2)
      ~nil:      (fun _ -> raise_unsupported ())
      ~record:   (record ~record_name)
      rhs

  let generate = function
    | Ast.TyDcl (_loc, name, tps, rhs, _) -> fields_of_ty _loc ~record_name:name ~tps ~rhs
    | Ast.TyAnd (_loc, _, _) as tds ->
        ignore (_loc, tds);
        failwith "Not supported"
    | _                             -> assert false
end

let () = add_generator "fields" Gen_struct.generate
let () = add_sig_generator "fields" Gen_sig.generate
