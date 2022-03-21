open Ast
open Core

type venv = Env.entry Symbol.table
type tenv = Env.ty Symbol.table
type expty = { exp : Translate.exp; ty : Env.ty }

type error =
  | Wrong_num_arguments
  | Expr_type_clash of Types.ty * Types.ty
  (* | Duplicate_record_field *)
  | Unbound_value of string
  | Unbound_type of string
  | Unexpected_break
  | Not_a_function of string
  | Readonly of string
  | Nonexistent_field of string
  | Not_an_array
  | Not_a_record
  | Not_an_integer
  | Not_comparable
  | Not_equality
  | Multiple_bindings of string

exception Error of error * int

let with_loop_marker venv =
  Symbol.add venv ~symbol:(Symbol.symbol ":loop")
    ~data:(Env.VarEntry { ty = Types.Unit; readonly = true })

let raise_unbound_value sym pos =
  raise (Error (Unbound_value (Symbol.name sym), pos))

let raise_unbound_type sym pos =
  raise (Error (Unbound_type (Symbol.name sym), pos))

let find_or_raise_unbound_type tenv sym pos =
  match Symbol.find tenv ~symbol:sym with
  | Some ty -> ty
  | None -> raise (Error (Unbound_type (Symbol.name sym), pos))

let expect_type_assignable ~dst ~src ~pos =
  if not (Types.is_assignable ~dst ~src) then
    raise (Error (Expr_type_clash (dst, src), pos))

let expect_type_equal x y pos =
  if not (Types.equal x y) then raise (Error (Expr_type_clash (x, y), pos))

let rec trans_exp venv tenv exp =
  match exp with
  | VarExp var -> trans_var venv tenv var
  | NilExp -> { exp = (); ty = Types.Nil }
  | IntExp _ -> { exp = (); ty = Types.Int }
  | StringExp _ -> { exp = (); ty = Types.String }
  | CallExp { func; args; pos } ->
      let ty =
        match Symbol.find venv ~symbol:func with
        (* Match each argument type *)
        | Some (Env.FunEntry { formals; result }) ->
            if List.length args <> List.length formals then
              raise (Error (Wrong_num_arguments, 1));
            let arg_types =
              List.map args ~f:(fun arg -> (trans_exp venv tenv arg).ty)
            in
            List.iter (List.zip_exn formals arg_types)
              ~f:(fun (formal, arg_type) ->
                expect_type_assignable ~dst:formal ~src:arg_type ~pos);
            result
        | _ -> raise_unbound_value func 3
      in
      { exp = (); ty }
  | OpExp { left; oper; right; pos } ->
      let { ty = tyl; _ } = trans_exp venv tenv left in
      let { ty = tyr; _ } = trans_exp venv tenv right in
      (match oper with
      | EqOp | NeqOp ->
          if not (Types.can_test_equality tyl) then
            raise (Error (Not_equality, pos))
      | LeOp | LtOp | GeOp | GtOp ->
          if not (Types.comparable tyl) then raise (Error (Not_comparable, pos))
      | PlusOp | MinusOp | TimesOp | DivideOp | AndOp | OrOp ->
          if not (Types.is_int tyl) then raise (Error (Not_an_integer, pos)));
      expect_type_assignable ~dst:tyl ~src:tyr ~pos;
      { exp = (); ty = Types.Int }
  | RecordExp { fields; typ; pos } -> (
      match Symbol.find tenv ~symbol:typ with
      | Some (Types.Record (tyfields, _) as actual_ty)
      | Some
          (Types.Name (_, { contents = Some (Types.Record (tyfields, _)) }) as
          actual_ty) ->
          if List.length tyfields <> List.length fields then failwith "";
          let fields' =
            List.sort fields ~compare:(fun (s1, _, _) (s2, _, _) ->
                String.compare (Symbol.name s1) (Symbol.name s2))
          in
          let tyfields' =
            List.sort tyfields ~compare:(fun (s1, _) (s2, _) ->
                String.compare (Symbol.name s1) (Symbol.name s2))
          in
          List.iter (List.zip_exn fields' tyfields')
            ~f:(fun ((_, exp1, _), (_, ty2)) ->
              let { ty; _ } = trans_exp venv tenv exp1 in
              expect_type_assignable ~dst:ty2 ~src:ty ~pos);
          { exp = (); ty = actual_ty }
      | _ -> raise_unbound_type typ pos)
  | SeqExp exps ->
      List.fold exps ~init:{ exp = (); ty = Types.Unit } ~f:(fun _ (exp, _) ->
          trans_exp venv tenv exp)
  | AssignExp { var; exp; pos } ->
      let { ty = dst; _ } = trans_var ~fail_readonly:true venv tenv var in
      let { ty = src; _ } = trans_exp venv tenv exp in
      expect_type_assignable ~dst ~src ~pos;
      { exp = (); ty = Types.Unit }
  | IfExp { test; then'; else'; pos } -> (
      match trans_exp venv tenv test with
      | { exp = _; ty = Types.Int } ->
          let { ty = then_ty; _ } = trans_exp venv tenv then' in
          let result_ty =
            match else' with
            | Some else' ->
                let { ty = else_ty; _ } = trans_exp venv tenv else' in
                expect_type_equal then_ty else_ty pos;
                then_ty
            | None ->
                expect_type_equal Types.Unit then_ty pos;
                Types.Unit
          in
          { exp = (); ty = result_ty }
      | { ty; _ } -> raise (Error (Expr_type_clash (Types.Int, ty), pos)))
  | WhileExp { test; body; pos } -> (
      match trans_exp venv tenv test with
      | { exp = _; ty = Types.Int } -> (
          let venv' = with_loop_marker venv in
          match trans_exp venv' tenv body with
          | { ty = Types.Unit; _ } -> { exp = (); ty = Types.Unit }
          | { ty; _ } -> raise (Error (Expr_type_clash (Types.Unit, ty), pos)))
      | { ty; _ } -> raise (Error (Expr_type_clash (Types.Int, ty), pos)))
  | ForExp { var; escape = _; lo; hi; body; pos } ->
      (* assert lo int *)
      let { exp = _exp; _ } =
        expect_trans_exp_type venv tenv lo Types.Int pos
      in
      (* assert hi int *)
      let { exp = _exp; _ } =
        expect_trans_exp_type venv tenv hi Types.Int pos
      in
      let venv' =
        venv
        |> Symbol.add ~symbol:var
             ~data:(Env.VarEntry { ty = Types.Int; readonly = true })
        |> with_loop_marker
      in
      (* assert body *)
      let { ty; _ } = trans_exp venv' tenv body in
      expect_type_equal Types.Unit ty pos;
      { exp = (); ty = Types.Unit }
  | BreakExp pos -> (
      match Symbol.find venv ~symbol:(Symbol.symbol ":loop") with
      | Some _ -> { exp = (); ty = Types.Unit }
      | None -> raise (Error (Unexpected_break, pos)))
  | LetExp { decs; body; _ } ->
      let venv', tenv' =
        List.fold decs ~init:(venv, tenv) ~f:(fun (venv, tenv) dec ->
            trans_dec venv tenv dec)
      in
      let { ty; _ } = trans_exp venv' tenv' body in
      { exp = (); ty }
  | ArrayExp { typ; size; init; pos } -> (
      let ty = find_or_raise_unbound_type tenv typ pos in
      match Types.maybe_extract_array ty with
      | Some (inner_ty, _) ->
          let { ty = size_ty; _ } = trans_exp venv tenv size in
          expect_type_assignable ~dst:Types.Int ~src:size_ty ~pos;
          let { ty = init_ty; _ } = trans_exp venv tenv init in
          expect_type_assignable ~dst:inner_ty ~src:init_ty ~pos;
          { exp = (); ty }
      | None -> raise (Error (Not_an_array, pos)))

and trans_var ?(fail_readonly = false) venv tenv var =
  match var with
  | SimpleVar (symbol, pos) ->
      let ty =
        match Symbol.find venv ~symbol with
        | Some (Env.VarEntry { ty; readonly }) ->
            if fail_readonly && readonly then
              raise (Error (Readonly (Symbol.name symbol), pos));
            ty
        | Some (Env.FunEntry _) -> failwith "function can't be in this position"
        | None -> raise (Error (Unbound_value (Symbol.name symbol), 0))
      in
      { exp = (); ty }
  | FieldVar (var, symbol, pos) -> (
      let { exp = _; ty } = trans_var venv tenv var in
      match Types.maybe_extract_record ty with
      | Some (fields, _) -> (
          match
            List.find fields ~f:(fun (field_symbol, _) ->
                String.equal (Symbol.name field_symbol) (Symbol.name symbol))
          with
          | Some (_, ty) -> { exp = (); ty }
          | None -> raise (Error (Nonexistent_field (Symbol.name symbol), pos)))
      | _ -> raise (Error (Not_a_record, pos)))
  | SubscriptVar (var, exp, pos) -> (
      let { exp = _; ty } = trans_var venv tenv var in
      match Types.maybe_extract_array ty with
      | Some (arr_ty, _unique) ->
          let { exp = _; ty = expty } = trans_exp venv tenv exp in
          if not (Types.is_assignable ~dst:Types.Int ~src:expty) then
            raise (Error (Expr_type_clash (Types.Int, expty), pos));
          { exp = (); ty = arr_ty }
      | _ -> raise (Error (Not_an_array, pos)))

and trans_dec venv tenv = function
  | FunctionDec fundecs ->
      let venv', _ =
        List.fold fundecs
          ~init:(venv, Set.empty (module String))
          ~f:(fun (acc, s) { params; pos; result; name; body = _body } ->
            let sname = Symbol.name name in
            if Set.mem s sname then raise (Error (Multiple_bindings sname, pos));
            let formals =
              List.map params ~f:(fun { typ; _ } ->
                  let ty = find_or_raise_unbound_type tenv typ pos in
                  ty)
            in
            let result =
              match result with
              | Some (sym, pos) -> find_or_raise_unbound_type tenv sym pos
              | None -> Types.Unit
            in
            ( Symbol.add acc ~symbol:name
                ~data:(Env.FunEntry { formals; result }),
              Set.add s sname ))
      in
      (* type check function bodies *)
      List.iter fundecs ~f:(fun { body; params; pos; result; _ } ->
          let venv'' =
            List.fold params ~init:venv' ~f:(fun acc { typ; name; _ } ->
                Symbol.add acc ~symbol:name
                  ~data:
                    (Env.VarEntry
                       {
                         ty = find_or_raise_unbound_type tenv typ pos;
                         readonly = false;
                       }))
          in
          let result =
            match result with
            | Some (sym, pos) -> find_or_raise_unbound_type tenv sym pos
            | None -> Types.Unit
          in
          let { ty; _ } = trans_exp venv'' tenv body in
          expect_type_assignable ~dst:result ~src:ty ~pos);
      (venv', tenv)
  | VarDec { name; typ; init; pos = _pos; _ } -> (
      let { ty = init_ty; _ } = trans_exp venv tenv init in
      match typ with
      | Some (sym, pos) ->
          let ty = find_or_raise_unbound_type tenv sym pos in
          expect_type_assignable ~dst:ty ~src:init_ty ~pos;
          let venv =
            Symbol.add venv ~symbol:name
              ~data:(VarEntry { ty; readonly = false })
          in
          (venv, tenv)
      | None ->
          let venv =
            Symbol.add venv ~symbol:name
              ~data:(VarEntry { ty = init_ty; readonly = false })
          in
          (venv, tenv))
  | TypeDec type_infos ->
      let tenv', l, _ =
        List.fold type_infos
          ~init:(tenv, [], Set.empty (module String))
          ~f:(fun (acc_tenv, acc_l, s) { name; ty = ast_ty; pos } ->
            let sname = Symbol.name name in
            if Set.mem s sname then raise (Error (Multiple_bindings sname, pos));
            let ty_ref = ref None in
            let name_ty = Types.Name (name, ty_ref) in
            ( Symbol.add acc_tenv ~symbol:name ~data:name_ty,
              (ast_ty, ty_ref) :: acc_l,
              Set.add s sname ))
      in

      List.iter l ~f:(fun (ast_ty, ty_ref) ->
          ty_ref := Some (trans_ty venv tenv' ast_ty));
      (venv, tenv')

and trans_ty _venv tenv = function
  | NameTy (sym, pos) -> find_or_raise_unbound_type tenv sym pos
  | ArrayTy (sym, pos) ->
      let ty = find_or_raise_unbound_type tenv sym pos in
      Types.Array (ty, ref ())
  | RecordTy fields ->
      let fields' =
        List.map fields ~f:(fun { name; typ; pos; _ } ->
            let ty = find_or_raise_unbound_type tenv typ pos in
            (name, ty))
      in
      Types.Record (fields', ref ())

and expect_trans_exp_type venv tenv exp ty pos =
  let res = trans_exp venv tenv exp in
  expect_type_equal ty res.ty pos;
  res