open Ast
open Core

type venv = Env.entry Symbol.table
type tenv = Env.ty Symbol.table
type expty = { exp : Translate.exp; ty : Env.ty }

type error =
  | Wrong_num_arguments
  | Expr_type_clash
  (* | Duplicate_record_field *)
  | Unbound_value of string
  | Unbound_type of string

exception Error of error * int

let raise_unbound_value sym pos =
  raise (Error (Unbound_value (Symbol.name sym), pos))

let find_or_raise_unbound_type tenv sym pos =
  match Symbol.find tenv ~symbol:sym with
  | Some ty -> ty
  | None -> raise (Error (Unbound_type (Symbol.name sym), pos))

let rec trans_exp venv tenv exp =
  match exp with
  | VarExp var -> trans_var venv tenv var
  | NilExp -> { exp = (); ty = Types.Nil }
  | IntExp _ -> { exp = (); ty = Types.Int }
  | StringExp _ -> { exp = (); ty = Types.String }
  | CallExp { func; args; _ } ->
      let ty =
        match Symbol.find venv ~symbol:func with
        (* Match each argument type *)
        | Some (Env.FunEntry { formals; result }) ->
            if List.length args <> List.length formals then
              raise (Error (Wrong_num_arguments, 1));
            let arg_types =
              List.map args ~f:(fun arg -> (trans_exp venv tenv arg).ty)
            in
            if not (List.equal Types.equal arg_types formals) then
              raise (Error (Expr_type_clash, 2))
            else result
        | _ -> raise_unbound_value func 3
      in
      { exp = (); ty }
  | OpExp { left; oper; right; pos } -> (
      let { ty = tyl; _ } = trans_exp venv tenv left in
      let { ty = tyr; _ } = trans_exp venv tenv right in
      match oper with
      | PlusOp | MinusOp | TimesOp | DivideOp | EqOp | NeqOp | LtOp | LeOp
      | GtOp | GeOp | AndOp | OrOp -> (
          match (tyl, tyr) with
          | Types.Int, Types.Int -> { exp = (); ty = Types.Int }
          | _ -> raise (Error (Expr_type_clash, pos))))
  | RecordExp { fields; typ; pos } -> (
      match Symbol.find venv ~symbol:typ with
      | Some (Env.VarEntry { ty = Types.Record (tyfields, uniq) }) ->
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
              if not (Types.equal ty ty2) then
                raise (Error (Expr_type_clash, pos)));
          { exp = (); ty = Types.Record (tyfields, uniq) }
      | _ -> raise_unbound_value typ pos)
  | SeqExp exps ->
      if List.is_empty exps then { exp = (); ty = Types.Unit }
      else
        let exp, _ = List.last_exn exps in
        trans_exp venv tenv exp
  | AssignExp _ -> { exp = (); ty = Types.Unit }
  | IfExp { test; then'; else'; pos } -> (
      match trans_exp venv tenv test with
      | { exp = _; ty = Types.Int } -> (
          let { ty = then_ty; _ } = trans_exp venv tenv then' in
          match else' with
          | Some else' ->
              let { ty = else_ty; _ } = trans_exp venv tenv else' in
              if Types.equal then_ty else_ty then { exp = (); ty = then_ty }
              else raise (Error (Expr_type_clash, pos))
          | None -> (
              match then_ty with
              | Types.Unit -> { exp = (); ty = Types.Unit }
              | _ -> raise (Error (Expr_type_clash, pos))))
      | _ -> raise (Error (Expr_type_clash, pos)))
  | WhileExp { test; body; pos } -> (
      match trans_exp venv tenv test with
      | { exp = _; ty = Types.Int } -> (
          match trans_exp venv tenv body with
          | { ty = Types.Unit; _ } -> { exp = (); ty = Types.Unit }
          | _ -> raise (Error (Expr_type_clash, pos)))
      | _ -> raise (Error (Expr_type_clash, pos)))
  | ForExp { var; escape = _; lo; hi; body; pos } -> (
      (* assert lo int *)
      (match trans_exp venv tenv lo with
      | { exp = _; ty = Types.Int } -> ()
      | _ -> raise (Error (Expr_type_clash, pos)));
      (* assert hi int *)
      (match trans_exp venv tenv hi with
      | { exp = _; ty = Types.Int } -> ()
      | _ -> raise (Error (Expr_type_clash, pos)));
      let venv' =
        Symbol.add venv ~symbol:var ~data:(Env.VarEntry { ty = Types.Int })
      in
      (* assert body *)
      match trans_exp venv' tenv body with
      | { exp = _; ty = Types.Unit } -> { exp = (); ty = Types.Unit }
      | _ -> raise (Error (Expr_type_clash, pos)))
  | BreakExp _ -> { exp = (); ty = Types.Unit }
  | ArrayExp { typ; size; init; pos } -> (
      match Symbol.find tenv ~symbol:typ with
      | Some (Types.Array (inner_ty, _) as ty) ->
          (let { ty = size_ty; _ } = trans_exp venv tenv size in
           if not (Types.equal size_ty Types.Int) then
             raise (Error (Expr_type_clash, pos)));
          let { ty = init_ty; _ } = trans_exp venv tenv init in
          if Types.equal init_ty inner_ty then { exp = (); ty }
          else raise (Error (Expr_type_clash, pos))
      | _ -> failwith "")
  | _ -> failwith "unsupported exp type"

and trans_var venv tenv var =
  match var with
  | SimpleVar (symbol, _pos) ->
      let ty =
        match Symbol.find venv ~symbol with
        | Some (Env.VarEntry { ty }) -> ty
        | Some (Env.FunEntry _) -> failwith "function can't be in this position"
        | None -> raise (Error (Unbound_value (Symbol.name symbol), 0))
      in
      { exp = (); ty }
  | FieldVar (var, symbol, _pos) -> (
      let { exp = _; ty } = trans_var venv tenv var in
      match ty with
      | Types.Record (fields, _) -> (
          match
            List.find fields ~f:(fun (field_symbol, _) ->
                String.equal (Symbol.name field_symbol) (Symbol.name symbol))
          with
          | Some (_, ty) -> { exp = (); ty }
          | None -> failwith "field doesn't exist on type")
      | _ -> failwith "can't access field of a non-record type")
  | SubscriptVar (var, exp, _pos) -> (
      let { exp = _; ty } = trans_var venv tenv var in
      match ty with
      | Types.Array (ty, _unique) -> (
          let { exp = _; ty = expty } = trans_exp venv tenv exp in
          match expty with
          | Types.Int -> { exp = (); ty }
          | _ -> failwith "can't subscript with a non-integer value")
      | _ -> failwith "can't subscript a non-array type")

and trans_dec venv tenv = function
  | FunctionDec fundecs ->
      let venv =
        List.fold fundecs ~init:venv
          ~f:(fun acc { params; pos; result; name; _ } ->
            let formals =
              List.map params ~f:(fun { name; _ } ->
                  find_or_raise_unbound_type tenv name pos)
            in
            let result =
              match result with
              | Some (sym, pos) -> find_or_raise_unbound_type tenv sym pos
              | None -> Types.Unit
            in
            Symbol.add acc ~symbol:name ~data:(Env.FunEntry { formals; result }))
      in
      (venv, tenv)
  | VarDec { name; escape = _escape; typ; init; pos = _pos } -> (
      let { ty = init_ty; _ } = trans_exp venv tenv init in
      match typ with
      | Some (sym, pos) ->
          let ty = find_or_raise_unbound_type tenv sym pos in
          if Types.equal init_ty ty then
            let venv =
              Symbol.add venv ~symbol:name ~data:(VarEntry { ty = init_ty })
            in
            (venv, tenv)
          else raise (Error (Expr_type_clash, pos))
      | None ->
          let venv =
            Symbol.add venv ~symbol:name ~data:(VarEntry { ty = init_ty })
          in
          (venv, tenv))
  | TypeDec type_infos ->
      let tenv' =
        List.fold type_infos ~init:tenv ~f:(fun acc { name; ty; _ } ->
            let ty' = trans_ty venv tenv ty in
            Symbol.add acc ~symbol:name ~data:ty')
      in
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
