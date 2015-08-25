(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

type t = {
  offsets : Closure_offsets.result;
  closures : Flambda.function_declarations Closure_id.Map.t;
  constant_sets_of_closures : Set_of_closures_id.Set.t;
}

type ('a, 'b) declaration_position =
  | Current_unit of 'a
  | Imported_unit of 'b
  | Not_declared

module Env : sig
  type t

  val empty : t

  val add_fresh_ident : t -> Variable.t -> Ident.t * t
  val ident_for_var_exn : t -> Variable.t -> Ident.t
end = struct
  type t =
    { const_subst : Clambda.ulambda Variable.Map.t;
      subst : Clambda.ulambda Variable.Map.t;
      var : Ident.t Variable.Map.t;
      toplevel : bool;
    }

  let empty =
    { const_subst = Variable.Map.empty;
      subst = Variable.Map.empty;
      var = Variable.Map.empty;
      toplevel = false;
    }

  let add_sb t id subst =
    { t with subst = Variable.Map.add id subst t.subst }

  let find_sb t id =
    try Variable.Map.find id t.subst with
    | Not_found -> Variable.Map.find id t.const_subst

  let ident_for_var_exn t id = Variable.Map.find id t.var

  let add_fresh_ident t var =
    let id = Variable.unique_ident var in
    id, { t with var = Variable.Map.add var id t.var }
end

let to_clambda_const (const : Flambda.constant_defining_value_block_field)
      : Clambda.uconstant =
  match const with
  | Symbol s ->
    let lbl = Linkage_name.to_string (Symbol.label s) in
    (* The constant should contains details about the variable to
       allow cmmgen to unbox *)
    Uconst_ref (lbl, None)
  | Const (Int i) -> Uconst_int i
  | Const (Char c) -> Uconst_int (Char.code c)
  | Const (Const_pointer i) -> Uconst_ptr i

(* The offset table associate a function label to its offset
   inside a closure *)
let fun_offset_table = P.offsets.code_pointer_offsets
let fv_offset_table = P.offsets.free_variable_offsets

(* offsets of functions and free variables in closures comming from
   a linked module *)
let extern_fun_offset_table = (Compilenv.approx_env ()).ex_offset_fun
let extern_fv_offset_table = (Compilenv.approx_env ()).ex_offset_fv
let ex_closures = (Compilenv.approx_env ()).ex_functions_off
(* let ex_functions = *)
(*   (Compilenv.approx_env ()).Flambdaexport_types.ex_functions *)
let ex_constant_closures = (Compilenv.approx_env ()).ex_constant_closures

let get_fun_offset off =
  try
    if Closure_id.in_compilation_unit (Compilenv.current_unit ()) off
    then Closure_id.Map.find off fun_offset_table
    else Closure_id.Map.find off extern_fun_offset_table
  with Not_found ->
    Misc.fatal_error
      (Format.asprintf "missing offset %a" Closure_id.print off)

let get_fv_offset off =
  if Var_within_closure.in_compilation_unit (Compilenv.current_unit ()) off
  then
    if not (Var_within_closure.Map.mem off fv_offset_table)
    then Misc.fatal_error (Format.asprintf "env field offset not found: %a\n%!"
                             Var_within_closure.print off)
    else Var_within_closure.Map.find off fv_offset_table
  else Var_within_closure.Map.find off extern_fv_offset_table

let function_declaration_position cf =
  try Current_unit (Closure_id.Map.find cf P.closures) with
  | Not_found ->
    try Imported_unit (Closure_id.Map.find cf ex_closures) with
    | Not_found ->
      Not_declared

let is_function_constant cf =
  match function_declaration_position cf with
  | Current_unit { set_of_closures_id } ->
    Set_of_closures_id.Set.mem set_of_closures_id P.constant_sets_of_closures
  | Imported_unit { set_of_closures_id } ->
    Set_of_closures_id.Set.mem set_of_closures_id ex_constant_closures
  | Not_declared ->
    Misc.fatal_error (Format.asprintf "missing closure %a"
                        Closure_id.print cf)

let subst_var env var : Clambda.ulambda =
  try find_sb var env
  with Not_found ->
    try Uvar (ident_for_var_exn var env)
    with Not_found ->
      Misc.fatal_errorf "Clambdagen.to_clambda: unbound variable %a@.%s@."
        Variable.print var
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack 400000))

let subst_vars env vars = List.map (subst_var env) vars

let build_uoffset ulam offset : Clambda.ulambda =
  if offset = 0 then ulam
  else Uoffset (ulam, offset)

let rec to_clambda t env (flam : Flambda.t) : Clambda.ulambda =
(*
Format.eprintf "Clambdagen.to_clambda: %a\n"
Flambda.print flam;
*)
  match flam with
  | Var var -> subst_var env var
  | Let (_, var, def, body) ->
    let id, env_body = Env.add_fresh_ident env var in
    Ulet (id, to_clambda_named t env def, to_clambda t env_body body)
  | Let_rec (defs, body) ->
    let env, defs =
      List.fold_right (fun (var, def) (env, defs) ->
          let id, env = Env.add_fresh_ident env var in
          env, (id, def) :: defs)
        defs (env, [])
    in
    let defs =
      List.map (fun (id, def) -> id, to_clambda_named t env def) defs
    in
    Uletrec (defs, to_clambda t env body)
  | Apply { func; args; kind = Direct direct_func; dbg = dbg } ->
    to_clambda_direct_apply t func args direct_func dbg env
  | Apply { func; args; kind = Indirect; dbg = dbg } ->
    (* CR mshinwell for mshinwell: improve this comment *)
    (* The closure parameter of the function is added by cmmgen, but
       it already appears in the list of parameters of the clambda
       function for generic calls. Notice that for direct calls it is
       added here. *)
    Ugeneric_apply (subst_var env func, subst_vars env args, dbg)
  | Switch (arg, sw) ->
    let aux () : Clambda.ulambda =
      let const_index, const_actions =
        to_clambda_switch t env sw.consts sw.numconsts sw.failaction
      in
      let block_index, block_actions =
        to_clambda_switch t env sw.blocks sw.numblocks sw.failaction
      in
      Uswitch (subst_var env arg,
        { us_index_consts = const_index;
          us_actions_consts = const_actions;
          us_index_blocks = block_index;
          us_actions_blocks = block_actions;
        })
    in
    let simple_expr (flam : Flambda.t) =
      match flam with
      | Var _ -> true
      | _ -> false
    in
    (* Check that the [failaction] may be duplicated.  If this is not the
       case, share it through a static raise / static catch. *)
    (* CR pchambart for pchambart: This is overly simplified. We should verify
       that this does not generates too bad code. If it the case, handle some
       let cases.
    *)
    begin match sw.failaction with
    | None -> aux ()
    | Some (Static_raise (_, args)) when List.for_all simple_expr args ->
      aux ()
    | Some failaction ->
      let exn = Static_exception.create () in
      let sw =
        { sw with
          failaction = Some (Flambda.Static_raise (exn, []));
        }
      in
      let expr : Flambda.t =
        Static_catch (exn, [], Switch (arg, sw), failaction)
      in
      to_clambda env expr
    end
  | String_switch (arg, sw, def) ->
    let arg = subst_var env arg in
    let sw = List.map (fun (s, e) -> s, to_clambda t env e) sw in
    let def = Misc.may_map (to_clambda t env) def in
    Ustringswitch (arg, sw, def)
  | Static_raise (static_exn, args) ->
    Ustaticfail (Static_exception.to_int static_exn,
      List.map (to_clambda t env) args)
  | Static_catch (static_exn, vars, body, handler) ->
    let env_handler, ids =
      List.fold_right (fun var (env, ids) ->
          let id, env = Env.add_fresh_ident env var in
          env, id :: ids)
        vars (env, [])
    in
    Ucatch (Static_exception.to_int static_exn, ids,
      to_clambda t env body, to_clambda t env_handler handler)
  | Try_with (body, var, handler) ->
    let id, env_handler = Env.add_fresh_ident env var in
    Utrywith (to_clambda t env body, id, to_clambda t env_handler handler)
  | If_then_else (arg, ifso, ifnot) ->
    Uifthenelse (subst_var env arg, to_clambda t env ifso,
      to_clambda t env ifnot)
  | While (cond, body) ->
    Uwhile (to_clambda t env cond, to_clambda t env body)
  | For { bound_var; from_value; to_value; direction; body } ->
    let id, env_body = Env.add_fresh_ident env bound_var in
    Ufor (id, subst_var env from_value, subst_var env to_value,
      direction, to_clambda t env_body body)
  | Assign { being_assigned; new_value } ->
    let id =
      try Env.ident_for_var_exn env being_assigned
      with Not_found ->
        Misc.fatal_errorf "Unbound variable %a in [Assign]: %a"
          Variable.print being_assigned
          Flambda.print flam
    in
    Uassign (id, subst_var env new_value)
  | Send { kind; meth; obj; args; dbg } ->
    Usend (kind, subst_var env meth, subst_var env obj,
      subst_vars env args, dbg)
  | Proved_unreachable ->
    (* CR pchambart: shouldn't be executable, maybe build something else e.g.
       Uprim(Praise, [Uconst (Uconst_pointer 0, None)], Debuginfo.none) *)
    Uunreachable

and to_clambda_named t env (named : Flambda.named) : Clambda.ulambda =
  match named with
  | Symbol sym ->
    let lbl = Linkage_name.to_string (Symbol.label sym) in
    (* CR pchambart: The constant should contains details about the variable to
       allow cmmgen to unbox.
       mshinwell: What are we going to do here? *)
    Uconst (Uconst_ref (lbl, None))
  | Const (Const_pointer n) -> Uconst (Uconst_ptr n)
  | Const (Int n) -> Uconst (Uconst_int n)
  | Const (Char c) -> Uconst (Uconst_int (Char.code c))
  | Allocated_const _ ->
    Misc.fatal_errorf "[Allocated_const] should have been lifted to a \
        [Let_symbol] construction before [Clambdagen]: %a"
      Flambda.print_named named
  | Set_of_closures set_of_closures ->
    to_clambda_set_of_closures t env set_of_closures
  | Project_closure { set_of_closures; closure_id } ->
    let ulam = subst_var env set_of_closures in
    let offset = get_fun_offset t closure_id in
    (* CR mshinwell for pchambart: I don't understand how this comment
       relates to this code.  Can you explain? *)
    (* compilation of let rec in cmmgen assumes
       that a closure is not offseted (Cmmgen.expr_size) *)
    build_uoffset ulam offset
  | Move_within_set_of_closures { closure; start_from; move_to } ->
    let ulam = subst_var env closure in
    let relative_offset =
      (get_fun_offset t move_to) - (get_fun_offset t start_from)
    in
    build_uoffset ulam relative_offset
  | Project_var { closure; var; closure_id } ->
    let ulam = subst_var env closure in
    let fun_offset = get_fun_offset t closure_id in
    Format.eprintf "Clambdagen: Project_var: %a\n" Flambda.print_named named;
    let var_offset = get_fv_offset t var in
    let pos = var_offset - fun_offset in
    Uprim (Pfield pos, [ulam], Debuginfo.none)
  | Prim (Pgetglobalfield (id, index), _, dbg) ->
    let ident = Ident.create_persistent (Compilenv.symbol_for_global id) in
    Uprim (Pfield index, [Clambda.Uprim (Pgetglobal ident, [], dbg)], dbg)
  | Prim (Psetglobalfield index, [arg], dbg) ->
    let ident = Ident.create_persistent (Compilenv.make_symbol None) in
    Uprim (Psetfield (index, false), [
        Clambda.Uprim (Pgetglobal ident, [], dbg);
        subst_var env arg;
      ], dbg)
  | Prim (p, args, dbg) -> Uprim (p, subst_vars env args, dbg)
  | Expr expr -> to_clambda t env expr

and to_clambda_switch env cases keys default =
  let num_keys =
    if Ext_types.Int.Set.cardinal num_keys = 0 then 0
    else Ext_types.Int.Set.max_elt num_keys + 1
  in
  let index = Array.make num_keys 0 in
  let store = Flambda.Switch_storer.mk_store () in
  begin match default with
  | Some def when List.length cases < num_keys -> ignore (store.act_store def)
  | _ -> ()
  end;
  List.iter (fun (key, lam) -> index.(key) <- store.Switch.act_store lam)
    cases;
  let actions = Array.map (to_clambda t env) (store.Switch.act_get ()) in
  match actions with
  | [| |] -> [| |], [| |]  (* May happen when [default] is [None]. *)
  | _ -> index, actions

and to_clambda_direct_apply func args direct_func dbg env : Clambda.ulambda =
  let closed = is_function_constant direct_func in
  let label = Compilenv.function_label direct_func in
  let uargs =
    let uargs = subst_vars env args in
    if closed then uargs else uargs @ [subst_var env func] in

  (* If the function is closed, the function expression is always a
     variable, so it is ok to drop it. Note that it means that
     some Let can be dead. The un-anf pass should get rid of it *)
  Clambda.Udirect_apply(label, uargs, dbg)

and to_clambda_set_of_closures env
    (({ function_decls = functs; free_vars = fv } : Flambda.set_of_closures)
      as set_of_closures) =
  (* Make the susbtitutions for variables bound by the closure:
     the variables bounds are the functions inside the closure and
     the free variables of the functions.

     For instance the closure for a code like

       let rec fun_a x =
         if x <= 0 then 0 else fun_b (x-1) v1
       and fun_b x y =
         if x <= 0 then 0 else v1 + v2 + y + fun_a (x-1)

     will be represented in memory as:

       [ closure header; fun_a;
         1; infix header; fun caml_curry_2;
         2; fun_b; v1; v2 ]

     fun_a and fun_b will take an additional parameter 'env' to
     access their closure.  It will be shifted such that in the body
     of a function the env parameter points to its code
     pointer. i.e. in fun_b it will be shifted by 3 words.

     Hence accessing to v1 in the body of fun_a is accessing to the
     6th field of 'env' and in the body of fun_b it is the 1st
     field.
  *)

(*
Format.eprintf "Clambdagen.to_clambda_set_of_closures: %a\n"
Flambda.print_set_of_closures set_of_closures;
*)

  let funct = Variable.Map.bindings functs.funs in
  let env_var = Ident.create "env" in

  let fv_ulam =
    Variable.Map.bindings
      (Variable.Map.map (subst_var env) fv)
  in

  let to_clambda_function (id, (func : Flambda.function_declaration))
    : Clambda.ufunction =
    let cf = Closure_id.wrap id in
    (* adds variables from the closure to the substitution environment *)
    let fun_offset = Closure_id.Map.find cf fun_offset_table in

    (* inside the body of the function, we cannot access variables
       declared outside, so take a clean substitution table. *)
    let env = { empty_env with const_subst = env.const_subst } in

    let env =
      (* Add to the substitution the value of the free variables *)

      let add_env_variable id _ env =
        let var_offset =
          try
            Var_within_closure.Map.find
              (Var_within_closure.wrap id) fv_offset_table
          with Not_found ->
            Misc.fatal_errorf "Clambda.to_clambda_set_of_closures: offset for \
                free variable %a is unknown.  Set of closures: %a"
              Variable.print id
              Flambda.print_set_of_closures set_of_closures
        in
        let pos = var_offset - fun_offset in
        add_sb id (Uprim(Pfield pos, [Clambda.Uvar env_var], Debuginfo.none)) env
      in

      let env = Variable.Map.fold add_env_variable fv env in

      (* Add to the substitution the value of the functions defined in
         the current closure:
         this can be retrieved by shifting the environment. *)
      let add_offset_subst pos env (id,_) =
        let offset = Closure_id.Map.find (Closure_id.wrap id) fun_offset_table in
        let exp : Clambda.ulambda = Uoffset(Uvar env_var, offset - pos) in
        add_sb id exp env in
      List.fold_left (add_offset_subst fun_offset) env funct
    in

    let env_body, params =
      List.fold_right (fun var (env, params) ->
          let id, env = add_fresh_ident var env in
          env, id :: params) func.params (env, []) in

    { Clambda.
      label = Compilenv.function_label cf;
      arity = Flambda_utils.function_arity func;
      params = params @ [env_var];
      body = to_clambda env_body func.body;
      dbg = func.dbg;
    }
  in

  let ufunct = List.map to_clambda_function funct in

  Clambda.Uclosure (ufunct, List.map snd fv_ulam)

and to_clambda_closed_set_of_closures env symbol
    ({ function_decls = functs } : Flambda.set_of_closures)
  : Clambda.ustructured_constant =

  let funct = Variable.Map.bindings functs.funs in

  (* the label used for constant closures *)
  let closure_lbl = Linkage_name.to_string (Symbol.label symbol) in

  let to_clambda_function (id, (func : Flambda.function_declaration))
    : Clambda.ufunction =

    (* inside the body of the function, we cannot access variables
       declared outside, so take a clean substitution table. *)
    let env = { empty_env with const_subst = env.const_subst } in

    let env_body, params =
      List.fold_right (fun var (env, params) ->
          let id, env = add_fresh_ident var env in
          env, id :: params) func.params (env, []) in

    { Clambda.
      label = Compilenv.function_label (Closure_id.wrap id);
      arity = Flambda_utils.function_arity func;
      params;
      body = to_clambda env_body func.body;
      dbg = func.dbg;
    }
  in

  let ufunct = List.map to_clambda_function funct in

  Uconst_closure (ufunct, closure_lbl, [])

let to_clambda_initialize_symbol env symbol fields : Clambda.ulambda =
  let fields = List.mapi (fun p expr -> p, to_clambda env expr) fields in
  let to_clambda_field (p, field) : Clambda.ulambda =
    (* This [Psetfield] can affect a pointer, but since we are
       initializing a toplevel symbol, it is safe not to use
       [caml_modify]. *)
    let symbol_string = Linkage_name.to_string (Symbol.label symbol) in
    Uprim (
      Psetfield (p, false),
      [Clambda.Uconst (Uconst_ref (symbol_string, None)); field],
      Debuginfo.none
    )
  in
  match fields with
  | [] -> Uconst (Uconst_ptr 0)
  | h :: t ->
    List.fold_left (fun acc (p, field) ->
        Clambda.Usequence (to_clambda_field (p, field), acc))
      (to_clambda_field h) t

let rec to_clambda_program (program : Flambda.program) : Clambda.ulambda =
  match program with
  | Let_symbol (_, _, program)
  | Let_rec_symbol (_, program)
  | Import_symbol (_, program) -> to_clambda_program program
  | End _ -> Uconst (Uconst_ptr 0)
  | Initialize_symbol (symbol, _tag, fields, program) ->
    Usequence (to_clambda_initialize_symbol empty_env symbol fields,
      to_clambda_program program)
  | Effect (expr, program) ->
    Usequence (to_clambda empty_env expr, to_clambda_program program)

let to_clambda_allocated_constant (const : Allocated_const.t)
      : Clambda.ustructured_constant =
  match const with
  | Float f -> Uconst_float f
  | Int32 i -> Uconst_int32 i
  | Int64 i -> Uconst_int64 i
  | Nativeint i -> Uconst_nativeint i
  | Immstring s | String s -> Uconst_string s
  | Float_array a -> Uconst_float_array a

let add_structured_constant
    symbol (c:Flambda.constant_defining_value)
    structured_constants =
  match c with
  | Allocated_const c ->
    Symbol.Map.add symbol (to_clambda_allocated_constant c)
      structured_constants
  | Block (tag, fields) ->
    let fields = List.map to_clambda_const fields in
    Symbol.Map.add symbol (Clambda.Uconst_block (Tag.to_int tag, fields))
      structured_constants
  | Set_of_closures _
  | Project_closure _ -> structured_constants

let add_constant_sets_of_closures
    to_clambda_closed_set_of_closures
    symbol (c:Flambda.constant_defining_value)
    constant_sets_of_closures =
  match c with
  | Allocated_const _ -> constant_sets_of_closures
  | Block _ -> constant_sets_of_closures
  | Set_of_closures set_of_closures ->
    let to_clambda_set_of_closures =
      to_clambda_closed_set_of_closures empty_env symbol set_of_closures
    in
    Symbol.Map.add symbol to_clambda_set_of_closures
      constant_sets_of_closures
  | Project_closure _ -> constant_sets_of_closures

let to_clambdaert ~program ~exported =
  let t =
    { offsets = Closure_offsets.compute program;
      closures = Flambda_utils.make_closure_map program;
      constant_sets_of_closures =
        Flambda_utils.all_lifted_constant_sets_of_closures program;
    }
  in
  let constants =
    Symbol.Map.of_list (Flambda_utils.constant_symbol_declarations program)
  in
  let structured_constants =
    Symbol.Map.fold add_structured_constant
      constants Symbol.Map.empty
  in
  let constant_sets_of_closures =
    Symbol.Map.fold
      (add_constant_sets_of_closures M.to_clambda_closed_set_of_closures)
      constants Symbol.Map.empty
  in
  let initialize_symbols = Flambda_utils.initialize_symbols program in
  let preallocated_blocks =
    List.map (fun (symbol, tag, fields) ->
        { Clambda.symbol = Linkage_name.to_string (Symbol.label symbol);
          tag = Tag.to_int tag;
          size = List.length fields })
      initialize_symbols
  in
  let expr = to_clambda_program t program in
  (* CR mshinwell for pchambart: add offsets to export info *)
  expr,
  preallocated_blocks,
  Symbol.Map.disjoint_union
    structured_constants
    constant_sets_of_closures,
  exported
