(* Compiler Construction - Minimal Lambda Language
 *
 * The type checker throws an exception if it encounter
 * a type error. This file defines all the types supported
 * by the language, along with an implementation of the
 * Hindley-Milner algorithm which infers the most general
 * polymorphic type for each function in the program.
 *
 * Hindley-Milner is based on unification: if the type of
 * an expression is known to be of a certain kind, or if
 * the types of expressions are known to be identical, they
 * are unified using the method defined below.
 *)

open Ast

exception Error of Ast.loc * string

module IdentMap = Map.Make(String)


(* Enumeration of types *)
type ty
  (* Basic integer type *)
  = TyInt
  (* Basic boolean type *)
  | TyBool
  (* Unit type for functions with no return *)
  | TyUnit
  (* Function type *)
  | TyArr of ty array * ty
  (* Type variable *)
  | TyVar of var_ty ref
  (* Forall qualified type - must be substituted *)
  | TyAbs of int
 and var_ty
  = Unbound of int
  | Bound of ty

let type_to_string ty
  = match ty with
    | TyInt -> "int"
    | TyBool -> "bool"
    | TyUnit -> "unit"
    | TyArr _ -> "array"
    | TyAbs _ -> "abs"
    | TyVar _ -> "var"

(* Scope to find types in. *)
type lambda_capture = int * Typed_ast.expr * ty
type type_scope
  = GlobalScope of (int * ty) IdentMap.t
  | LabellingScope of int
  | GroupScope of (int * ty) IdentMap.t
  | FuncScope of (int * ty) IdentMap.t * ty
  | BindScope of string * int * ty
  | WhileScope of (string option) * int
  | LambdaScope of (int * ty) IdentMap.t * (lambda_capture IdentMap.t) ref

(* Occurs check *)
let rec occurs loc r ty
  = match ty with
    | TyInt -> ()
    | TyBool -> ()
    | TyUnit -> ()
    | TyArr(params, ret) ->
      occurs loc r ret;
      Array.iter (occurs loc r) params
    | TyAbs _ ->
      failwith "should have been instantiated"
    | TyVar ({ contents = Bound ty }) ->
      occurs loc r ty
    | TyVar r' ->
      if r = r' then raise(Error(loc, "recursive type"))

(* Implementation of unification *)
let rec unify loc a b
  = if a = b then () else match a, b with
    | TyVar ({ contents = Bound ty }), ty'
    | ty, TyVar ({ contents = Bound ty' }) ->
      unify loc ty ty'
    | TyVar ({ contents = Unbound _ } as r), ty
    | ty, TyVar ({ contents = Unbound _ } as r) ->
      occurs loc r ty;
      r := Bound ty
    | TyArr(pa, ra), TyArr(pb, rb) ->
      let len_a = Array.length pa in
      let len_b = Array.length pb in
      if len_a != len_b then
        raise(Error(loc, "mismatched function types"));
      for i = 0 to len_a - 1 do
        unify loc pa.(i) pb.(i);
      done;
      unify loc ra rb
    | _, _ ->
      raise(Error(loc, "mismatched types " ^ (type_to_string a) ^ " and " ^ (type_to_string b)))

(* Helper to generate a type variable. *)
let ty_idx = ref 0
let new_ty_var () =
  let idx = !ty_idx in
  ty_idx := idx + 1;
  TyVar (ref (Unbound idx))

(* Generalises a type *)
let rec generalise ty
  = match ty with
  | TyInt -> ty
  | TyBool -> ty
  | TyUnit -> ty
  | TyArr(params, ret) ->
    TyArr(Array.map generalise params, generalise ret)
  | TyAbs _ ->
    failwith "should have been instantiated"
  | TyVar ({ contents = Bound ty }) ->
    generalise ty
  | TyVar ({ contents = Unbound id }) ->
    TyAbs id

(* Instantiates a type *)
let instantiate ty =
  let abs_context = Hashtbl.create 5 in
  let rec loop ty = match ty with
    | TyInt -> ty
    | TyBool -> ty
    | TyUnit -> ty
    | TyArr(params, ret) ->
      TyArr(Array.map loop params, loop ret)
    | TyAbs id ->
      begin
        try Hashtbl.find abs_context id
        with Not_found ->
          let ty = new_ty_var () in
          Hashtbl.add abs_context id ty;
          ty
      end
    | TyVar _ ->
      failwith "should have been generalised"
  in loop ty

(* Gets the id of the variable by name *)
let rec get_id name scope fallback_id
  = match scope with
  | GlobalScope map :: _ when IdentMap.mem name map ->
    (* Type schemes are instantiated here. *)
    let id, _ = IdentMap.find name map in id
  | GroupScope map :: _ when IdentMap.mem name map ->
    (* Polymorphic recursion is not allowed, no generalisation here. *)
    let id, _ = IdentMap.find name map in id
  | FuncScope(map, _) :: _ when IdentMap.mem name map ->
    let id, _ = IdentMap.find name map in id
  | BindScope(name', bound_id, _) :: _ when name = name' ->
    bound_id
  | LambdaScope(map, captures) :: rest ->
    (* In a lambda scope, see what needs to be captured. Arguments are *)
    (* handled as expected, while captures are cached. If a captured name *)
    (* is to be foud, the outside scope is searched, but an env reference *)
    (* is returned in its place, unless the name is a global. *)
    if IdentMap.mem name map then
      let id, _ = IdentMap.find name map in id
    else if IdentMap.mem name !captures then
      let id, _, _ = IdentMap.find name !captures in id
    else get_id name rest fallback_id
  | _ :: rest ->
    get_id name rest fallback_id
  | [] ->
    fallback_id

let label_rf = ref 0
(* Gets the next unique labelling id *)
let next_label_id _(*ignored scope arg to prevent optimisation*)
  = let label = !label_rf in
  label_rf := label + 1;
  (label)

(* Gets the id of the while loop by optional name*)
let rec get_while_id o_name scope
  = match scope with
  | WhileScope(Some(name), bound_id) :: rest -> begin
    match o_name with
      | Some(o_n) when o_n = name -> bound_id
      | _ -> get_while_id o_name rest
    end
  | WhileScope(None, bound_id) :: rest -> begin
    match o_name with
      | Some(_) -> get_while_id o_name rest
      | None -> bound_id
    end
  | _ :: rest ->
    get_while_id o_name rest
  | [] ->
    failwith "couldn't find while loop"

(* Checks the type of an expression *)
let rec check_expr scope expr
  = match expr with
  | IdentExpr(loc, name) ->
    let rec find_name ss
      = match ss with
      | GlobalScope map :: _ when IdentMap.mem name map ->
        (* Type schemes are instantiated here. *)
        let id, ty = IdentMap.find name map in
        Typed_ast.FuncExpr(loc, id), instantiate ty
      | GroupScope map :: _ when IdentMap.mem name map ->
        (* Polymorphic recursion is not allowed, no generalisation here. *)
        let id, ty = IdentMap.find name map in
        Typed_ast.FuncExpr(loc, id), ty
      | FuncScope(map, _) :: _ when IdentMap.mem name map ->
        let id, ty = IdentMap.find name map in
        Typed_ast.ArgExpr(loc, id), ty
      | BindScope(name', id, ty) :: _ when name = name' ->
        Typed_ast.BoundExpr(loc, id), ty
      | LambdaScope(map, captures) :: rest ->
        (* In a lambda scope, see what needs to be captured. Arguments are *)
        (* handled as expected, while captures are cached. If a captured name *)
        (* is to be foud, the outside scope is searched, but an env reference *)
        (* is returned in its place, unless the name is a global. *)
        if IdentMap.mem name map then
          let id, ty = IdentMap.find name map in
          Typed_ast.ArgExpr(loc, id), ty
        else if IdentMap.mem name !captures then
          let id, _, ty = IdentMap.find name !captures in
          Typed_ast.EnvExpr(loc, id), ty
        else begin
          let expr, ty = find_name rest in
          match expr with
          | Typed_ast.FuncExpr(_, _) -> expr, ty
          | _ ->
            let id = IdentMap.cardinal !captures in
            captures := IdentMap.add name (id, expr, ty) !captures;
            Typed_ast.EnvExpr(loc, id), ty
        end
      | _ :: rest ->
        find_name rest
      | [] ->
        raise(Error(loc, "unbound variable " ^ name))
    in find_name scope
  | IntExpr(loc, i) ->
    Typed_ast.IntExpr(loc, i), TyInt
  | BoolExpr(loc, i) ->
    Typed_ast.BoolExpr(loc, i), TyBool
  | AddExpr(loc, lhs, rhs) ->
    let lhs', ty_lhs = check_expr scope lhs in
    unify loc ty_lhs TyInt;
    let rhs', ty_rhs = check_expr scope rhs in
    unify loc ty_rhs TyInt;
    Typed_ast.AddExpr(loc, lhs', rhs'), TyInt
  | EqualsExpr(loc, lhs, rhs) ->
    let lhs', ty_lhs = check_expr scope lhs in
    let rhs', ty_rhs = check_expr scope rhs in
    unify loc ty_lhs ty_rhs;
    if (ty_lhs == TyInt || ty_lhs == TyBool || ty_rhs == TyInt || ty_rhs == TyBool) then
      Typed_ast.EqualsExpr(loc, lhs', rhs'), TyBool
    else
      raise(Error(loc,"mismatched dual types eq "^type_to_string ty_lhs ^ " and " ^ type_to_string ty_rhs))
  | NequalsExpr(loc, lhs, rhs) ->
    let lhs', ty_lhs = check_expr scope lhs in
    let rhs', ty_rhs = check_expr scope rhs in
    unify loc ty_lhs ty_rhs;
    if (ty_lhs == TyInt || ty_lhs == TyBool || ty_rhs == TyInt || ty_rhs == TyBool) then
      Typed_ast.NequalsExpr(loc, lhs', rhs'), TyBool
    else
      raise(Error(loc,"mismatched dual types neq "^type_to_string ty_lhs ^ " and " ^ type_to_string ty_rhs))
  | Or2Expr(loc, lhs, rhs) ->
    let lhs', ty_lhs = check_expr scope lhs in
    let rhs', ty_rhs = check_expr scope rhs in
    unify loc ty_lhs ty_rhs;
    if (ty_lhs == TyInt || ty_lhs == TyBool || ty_rhs == TyInt || ty_rhs == TyBool) then
      Typed_ast.Or2Expr(loc, lhs', rhs'), TyInt
    else
      raise(Error(loc,"mismatched dual types or "^type_to_string ty_lhs ^ " and " ^ type_to_string ty_rhs))
  | And2Expr(loc, lhs, rhs) ->
    let lhs', ty_lhs = check_expr scope lhs in
    let rhs', ty_rhs = check_expr scope rhs in
    unify loc ty_lhs ty_rhs;
    if (ty_lhs == TyInt || ty_lhs == TyBool || ty_rhs == TyInt || ty_rhs == TyBool) then
      Typed_ast.And2Expr(loc, lhs', rhs'), TyInt
    else
      raise(Error(loc,"mismatched dual types and "^type_to_string ty_lhs ^ " and " ^ type_to_string ty_rhs))
  | SubtractExpr(loc, lhs, rhs) ->
    let lhs', ty_lhs = check_expr scope lhs in
    unify loc ty_lhs TyInt;
    let rhs', ty_rhs = check_expr scope rhs in
    unify loc ty_rhs TyInt;
    Typed_ast.SubtractExpr(loc, lhs', rhs'), TyInt
  | LambdaExpr(loc, params, body) ->
    let args, ty_args = List.fold_left
      (fun (map, ty_args) param ->
        let id = IdentMap.cardinal map in
        let ty_arg = new_ty_var () in
        IdentMap.add param (id, ty_arg) map, ty_arg :: ty_args
      ) (IdentMap.empty, []) params
    in
    let captures = ref IdentMap.empty in
    let lambda_scope = LambdaScope(args, captures) in
    let body, ty_body = check_expr (lambda_scope :: scope) body in
    let lambda_ty = TyArr(Array.of_list (List.rev ty_args), ty_body) in
    let capture_list = Array.init (IdentMap.cardinal !captures)
      (fun i ->
        let _, (_, capture, _) = List.find
          (fun (_, (id, _, _)) -> id == i)
          (IdentMap.bindings !captures)
        in
        capture
      )
    in
    Typed_ast.LambdaExpr(loc, List.length params, capture_list, body), lambda_ty
  | CallExpr(loc, callee, args) ->
    (* When checking the type of a call, a dummy type is created: *)
    (* (ty_arg0, ty_arg1, ...) -> ty_return *)
    (* The type is then unified with the calle's type - during unification *)
    (* ty_return is unified with the function's return type, yielding the *)
    (* type of the call expression. *)
    let callee', ty_callee = check_expr scope callee in
    let args' = List.map (check_expr scope) args in
    let arg_tys = List.map snd args' in
    let ret_ty = new_ty_var () in
    let ty_func = TyArr(Array.of_list arg_tys, ret_ty) in
    unify loc ty_func ty_callee;
    Typed_ast.CallExpr(loc, callee', Array.of_list (List.map fst args')), ret_ty

(* Checks the type of a statement. *)
let rec check_statements ret_ty acc scope stats
  = let rec iter (nb, acc) scope stats = match stats with
    | ReturnStmt(loc, e) :: rest ->
      let e', ty = check_expr scope e in
      unify loc ty ret_ty;
      let node = Typed_ast.ReturnStmt(loc, e') in
      iter (nb, node :: acc) scope rest
    | ExprStmt(loc, e) :: rest ->
      (* It is a funky design choice to unify everything with unit. *)
      let e', ty = check_expr scope e in
      unify loc ty TyUnit;
      let node = Typed_ast.ExprStmt(loc, e') in
      iter (nb, node :: acc) scope rest
    | BindStmt(loc, name, e) :: rest ->
      let e', ty = check_expr scope e in
      let x = (get_id name scope nb) in
      if x == nb then begin
        let scope' = BindScope(name, x, ty) :: scope in
        let node = Typed_ast.BindStmt(loc, x, e') in
        iter (nb + 1, node :: acc) scope' rest
      end else begin
        let node = Typed_ast.AssignStmt(loc, x, e') in
        iter (nb, node::acc) scope rest
      end
    | IfStmt(loc, e1, e2, Some(e3)) :: rest ->
      let body1, ty = check_expr scope e1 in
      unify loc ty TyBool;
      let nb2, body2 = check_statements ret_ty (0, []) scope e2 in
      let nb3, body3 = check_statements ret_ty (0, []) scope e3 in
      let id = next_label_id scope in
      let node = Typed_ast.IfStmt(loc,id,body1,body2,Some(body3)) in
      iter (nb+nb2+nb3, node::acc) scope rest
    | IfStmt(loc, e1, e2, None) :: rest ->
      let body1, ty = check_expr scope e1 in
      unify loc ty TyBool;
      let nb2, body2 = check_statements ret_ty (0, []) scope e2 in
      let id = next_label_id scope in
      let node = Typed_ast.IfStmt(loc,id,body1,body2,None) in
      iter (nb+nb2, node::acc) scope rest
    | WhileStmt(loc, e1, e2, name) :: rest ->
      let body1, ty = check_expr scope e1 in
      unify loc ty TyBool;
      let id = next_label_id scope in
      let scope'' = WhileScope(name, id) :: scope in
      let nb2, body2 = check_statements ret_ty (0, []) scope'' e2 in
      let node = Typed_ast.WhileStmt(loc,body1,body2,id) in
      iter (nb+nb2+1, node::acc) scope rest
    | BreakStmt(loc,name) :: rest ->
      let id = get_while_id name scope in
      let node = Typed_ast.BreakStmt(loc,id) in
      iter (nb, node::acc) scope rest
    | ContinueStmt(loc,name) :: rest ->
      let id = get_while_id name scope in
      let node = Typed_ast.ContinueStmt(loc,id) in
      iter (nb, node::acc) scope rest
    | [] ->
      (nb, acc)
  in iter acc scope stats

(* Finds the free variables in an expression. *)
let rec find_refs_expr bound acc expr
  = match expr with
  | IdentExpr(loc, name) ->
    if List.mem name bound then acc else (loc, name) :: acc
  | IntExpr(_, _) ->
    acc
  | BoolExpr(_, _) ->
    acc
  | AddExpr(_, lhs, rhs) ->
    find_refs_expr bound (find_refs_expr bound acc rhs) lhs
  | EqualsExpr(_, lhs, rhs) ->
    find_refs_expr bound (find_refs_expr bound acc rhs) lhs
  | NequalsExpr(_, lhs, rhs) ->
    find_refs_expr bound (find_refs_expr bound acc rhs) lhs
  | Or2Expr(_, lhs, rhs) ->
    find_refs_expr bound (find_refs_expr bound acc rhs) lhs
  | And2Expr(_, lhs, rhs) ->
    find_refs_expr bound (find_refs_expr bound acc rhs) lhs
  | SubtractExpr(_, lhs, rhs) ->
    find_refs_expr bound (find_refs_expr bound acc rhs) lhs
  | LambdaExpr(_, params, body) ->
    find_refs_expr (List.append params bound) acc body
  | CallExpr(_, callee, args) ->
    List.fold_left (find_refs_expr bound) (find_refs_expr bound acc callee) args

(* Finds the free variables in a function body. *)
let rec find_refs_stat bound stats
  = let _, acc = List.fold_left
      (fun (bound, acc) stat ->
        match stat with
        | ReturnStmt(_, e) ->
          (bound, find_refs_expr bound acc e)
        | ExprStmt(_, e) ->
          (bound, find_refs_expr bound acc e)
        | BindStmt(_, name, e) ->
          (* The expression can refer to previous instances of 'name'. *)
          (name :: bound, find_refs_expr bound acc e)
        | IfStmt(_, e1, e2, Some(e3)) ->
          let acc1 = find_refs_expr bound acc e1 in
          let acc2 = find_refs_stat bound e2 in
          let acc3 = find_refs_stat bound e3 in
          (bound, acc1 @ acc2 @ acc3)
        | IfStmt(_, e1, e2, None) ->
          let acc1 = find_refs_expr bound acc e1 in
          let acc2 = find_refs_stat bound e2 in
          (bound, acc1 @ acc2)
        | WhileStmt(_, e1, e2, _) ->
          let acc1 = find_refs_expr bound acc e1 in
          let acc2 = find_refs_stat bound e2 in
          (bound, acc1 @ acc2)
        | BreakStmt(_, _) -> (bound,acc)
        | ContinueStmt(_, _) -> (bound,acc)
      ) (bound, []) stats
    in acc

(* Helper structure for the very hacky and imperative SCC implementation. *)
type dfs_info =
  { index: int
  ; mutable link: int
  ; mutable on_stack: bool
  }

let check prog =
  (* For each toplevel definition, collect the list of references. *)
  let num_funcs = Array.length prog in
  let name_table = Hashtbl.create num_funcs in
  let references = prog |> Array.mapi
    (fun i func ->
      if Hashtbl.mem name_table func.name then
        raise (Error(func.loc, "duplicate name"));
      Hashtbl.add name_table func.name i;
      match func.body with
      | None ->
        []
      | Some body ->
        find_refs_stat func.params body
    )
  in

  (* Build a directed graph of function-to-function references. *)
  let graph = Array.init num_funcs
    (fun i ->
      let refs = references.(i) in
      Array.of_list (refs |> List.map
        (fun (loc, ref_name) ->
          try Hashtbl.find name_table ref_name
          with Not_found -> raise (Error(loc, "undefined function"))
        ))
    )
  in

  (* Using the directed graph of references, find the strongly connected *)
  (* components. Inside a component, i.e a recursive or mutually recursive *)
  (* context, the type is not polymorphic. This is due to the fact that the *)
  (* problem of inferring polymorphically recursive types is undecidable *)
  let scc_info = Array.init num_funcs (fun _ ->
    { index = -1
    ; link = -1
    ; on_stack = false
    })
  in
  let dfs_stack = ref [] in
  let index = ref 0 in
  let sccs_rev = ref [] in

  let rec scc_dfs node_from =
    let idx = !index in
    index := idx + 1;
    scc_info.(node_from) <- { index = idx; link = idx; on_stack = true };
    dfs_stack := node_from :: !dfs_stack;

    let update_link node_to =
      let link = min scc_info.(node_from).link scc_info.(node_to).link in
      scc_info.(node_from).link <- link
    in

    graph.(node_from) |> Array.iter (fun node_to ->
      let { index = to_index; on_stack = to_on_stack; _ } = scc_info.(node_to) in
      if to_index < 0 then begin
        scc_dfs node_to;
        update_link node_to
      end else if to_on_stack then begin
        update_link node_to
      end
    );

    if scc_info.(node_from).index = scc_info.(node_from).link then begin
      let rec build_component acc stack = match stack with
        | node :: rest when node != node_from ->
          scc_info.(node).on_stack <- false;
          build_component (node :: acc) rest
        | node :: rest ->
          scc_info.(node).on_stack <- false;
          (node :: acc, rest)
        | [] ->
          (acc, [])
      in
      let (scc, stack) = build_component [] !dfs_stack in
      dfs_stack := stack;
      sccs_rev := Array.of_list scc :: !sccs_rev;
    end
  in

  for i = 0 to num_funcs - 1 do
    if scc_info.(i).index < 0 then scc_dfs i;
  done;

  let sccs = Array.of_list (List.rev !sccs_rev) in

  (* Typecheck each method group. Types are polymorphic only outside of SCCs. *)
  let typed_prog, _ = Array.fold_left
    (fun (typed_prog, root_scope) group ->
      (* Create type vars for each function *)
      let group_map = Array.fold_left
        (fun scope id ->
          let { name; _ } = prog.(id) in
          IdentMap.add name (id, new_ty_var ()) scope
        ) IdentMap.empty group
      in
      let group_scope = GroupScope group_map :: GlobalScope root_scope :: [LabellingScope 0]
      in
      (* Typecheck individual methods *)
      let types = Array.map
        (fun id ->
          let func = prog.(id) in
          (* Set up argument / return types. *)
          let args = List.fold_left
            (fun map name ->
              IdentMap.add name (IdentMap.cardinal map, new_ty_var ()) map
            ) IdentMap.empty func.params
          in
          let ret = new_ty_var () in
          let scope = FuncScope(args, ret) :: group_scope in
          (* Recursively check the function. *)
          let new_body, num_locals = (match func.body with
          | None ->
            None, 0
          | Some body ->
            let nb, body = check_statements ret (0, []) scope body in
            Some (List.rev body), nb
          )
          in
          let new_func =
            { Typed_ast.id
            ; name = func.name
            ; num_params = IdentMap.cardinal args
            ; num_locals
            ; body = new_body
            ; loc = func.loc
            }
          in
          let arg_types = Array.init (List.length func.params)
            (fun i ->
              let _, ty = IdentMap.find (List.nth func.params i) args
              in ty
            )
          in
          (* Construct a function type. *)
          (TyArr(arg_types, ret), new_func)
        ) group
      in

      (* Unify the types with their tvars. *)
      let funcs = Array.mapi
        (fun i id ->
          let ty, func = types.(i) in
          let _, fn_ty = IdentMap.find (prog.(id).name) group_map in
          unify func.Typed_ast.loc fn_ty ty;
          func
        ) group
      in

      (* Generalise the types. *)
      let new_root_scope = Array.fold_left
        (fun map id ->
          let { name; _ } = prog.(id) in
          let _, fn_ty = IdentMap.find name group_map in
          let func_ty = generalise fn_ty in
          IdentMap.add name (id, func_ty) map
        ) root_scope group
      in
      (funcs :: typed_prog, new_root_scope)
    ) ([], IdentMap.empty) sccs
  in
  Array.of_list (List.rev typed_prog)

