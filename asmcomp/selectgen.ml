(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

open Misc
open Cmm
open Reg
open Mach

type environment = (Ident.t, Reg.t array) Tbl.t

(* Infer the type of the result of an operation *)

let oper_result_type = function
    Capply(ty, _) -> ty
  | Cextcall(s, ty, alloc, _) -> ty
  | Cload (c, _) ->
      begin match c with
      | Word_val -> typ_val
      | Single | Double | Double_u -> typ_float
      | _ -> typ_int
      end
  | Calloc -> typ_val
  | Cstore (c, _) -> typ_void
  | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi |
    Cand | Cor | Cxor | Clsl | Clsr | Casr |
    Ccmpi _ | Ccmpa _ | Ccmpf _ -> typ_int
  | Caddv -> typ_val
  | Cadda -> typ_addr
  | Cnegf | Cabsf | Caddf | Csubf | Cmulf | Cdivf -> typ_float
  | Cfloatofint -> typ_float
  | Cintoffloat -> typ_int
  | Craise _ -> typ_void
  | Ccheckbound _ -> typ_void

(* Infer the size in bytes of the result of an expression whose evaluation
   may be deferred (cf. [emit_parts]). *)
 
exception Size_expr

let size_expr_exn (env:environment) exp =
  let rec size localenv = function
      Cconst_int _ | Cconst_natint _
    | Cconst_blockheader _ -> Arch.size_int
    | Cconst_symbol _ | Cconst_pointer _ | Cconst_natpointer _ ->
        Arch.size_addr
    | Cconst_float _ -> Arch.size_float
    | Cvar id ->
        begin try
          Tbl.find id localenv
        with Not_found ->
        try
          let regs = Tbl.find id env in
          size_machtype (Array.map (fun r -> r.typ) regs)
        with Not_found ->
          fatal_error("Selection.size_expr: unbound var " ^
                      Ident.unique_name id)
        end
    | Ctuple el ->
        List.fold_right (fun e sz -> size localenv e + sz) el 0
    | Cop(op, args) ->
        size_machtype(oper_result_type op)
    | Clet(id, arg, body) ->
        size (Tbl.add id (size localenv arg) localenv) body
    | Csequence(e1, e2) ->
        size localenv e2
    | Cifthenelse (_cond, ifso, ifnot) ->
        let size_ifso = size localenv ifso in
        let size_ifnot = size localenv ifnot in
        let ok =
          size_ifso = size_ifnot
            || size_ifso = 0
            || size_ifnot = 0
        in
        if not ok then begin
          fatal_errorf "Selection.size_expr: Cifthenelse size mismatch \
              (ifso %d, ifnot %d)"
            size_ifso size_ifnot
        end;
        max size_ifso size_ifnot
    | _ ->
        raise Size_expr
  in
  size Tbl.empty exp

let size_expr env exp =
  try size_expr_exn env exp
  with Size_expr ->
    fatal_errorf "Selection.size_expr: cannot measure %a"
      Printcmm.expression exp

(* Swap the two arguments of an integer comparison *)

let swap_intcomp = function
    Isigned cmp -> Isigned(swap_comparison cmp)
  | Iunsigned cmp -> Iunsigned(swap_comparison cmp)

(* Naming of registers *)

let all_regs_anonymous rv =
  try
    for i = 0 to Array.length rv - 1 do
      if not (Reg.anonymous rv.(i)) then raise Exit
    done;
    true
  with Exit ->
    false

let name_regs id rv =
  if Array.length rv = 1 then
    rv.(0).raw_name <- Raw_name.create_from_ident id
  else
    for i = 0 to Array.length rv - 1 do
      rv.(i).raw_name <- Raw_name.create_from_ident id;
      rv.(i).part <- Some i
    done

(* "Join" two instruction sequences, making sure they return their results
   in the same registers. *)

let join opt_r1 seq1 opt_r2 seq2 =
  match (opt_r1, opt_r2) with
    (None, _) -> opt_r2
  | (_, None) -> opt_r1
  | (Some r1, Some r2) ->
      let l1 = Array.length r1 in
      assert (l1 = Array.length r2);
      let r = Array.make l1 Reg.dummy in
      for i = 0 to l1-1 do
        if Reg.anonymous r1.(i)
          && Cmm.ge_component r1.(i).typ r2.(i).typ
        then begin
          r.(i) <- r1.(i);
          seq2#insert_move r2.(i) r1.(i)
        end else if Reg.anonymous r2.(i)
          && Cmm.ge_component r2.(i).typ r1.(i).typ
        then begin
          r.(i) <- r2.(i);
          seq1#insert_move r1.(i) r2.(i)
        end else begin
          let typ = Cmm.lub_component r1.(i).typ r2.(i).typ in
          r.(i) <- Reg.create typ;
          seq1#insert_move r1.(i) r.(i);
          seq2#insert_move r2.(i) r.(i)
        end
      done;
      Some r

(* Same, for N branches *)

let join_array rs =
  let some_res = ref None in
  for i = 0 to Array.length rs - 1 do
    let (r, s) = rs.(i) in
    if r <> None then some_res := r
  done;
  match !some_res with
    None -> None
  | Some template ->
      let size_res = Array.length template in
      let res = Array.make size_res Reg.dummy in
      for i = 0 to size_res - 1 do
        res.(i) <- Reg.create template.(i).typ
      done;
      for i = 0 to Array.length rs - 1 do
        let (r, s) = rs.(i) in
        match r with
          None -> ()
        | Some r -> s#insert_moves r res
      done;
      Some res

(* Extract debug info contained in a C-- operation *)
let debuginfo_op = function
  | Capply(_, dbg) -> dbg
  | Cextcall(_, _, _, dbg) -> dbg
  | Craise (_, dbg) -> dbg
  | Ccheckbound dbg -> dbg
  | _ -> Debuginfo.none

(* Registers for catch constructs *)
let catch_regs = ref []

(* Name of function being compiled *)
let current_function_name = ref ""

(* Environment parameter for the function being compiled, if any. *)
let current_function_env_param = ref None

module Effect = struct
  type t =
    | None
    | Raise
    | Arbitrary

  let join t1 t2 =
    match t1, t2 with
    | None, t2 -> t2
    | t1, None -> t1
    | Raise, Raise -> Raise
    | Arbitrary, _ | _, Arbitrary -> Arbitrary

  let pure = function
    | None -> true
    | Raise | Arbitrary -> false
end

module Coeffect = struct
  type t =
    | None
    | Read_mutable

  let join t1 t2 =
    match t1, t2 with
    | None, None -> None
    | None, Read_mutable | Read_mutable, None
    | Read_mutable, Read_mutable -> Read_mutable

  let copure = function
    | None -> true
    | Read_mutable -> false
end

module Effect_and_coeffect : sig
  type t

  val none : t
  val arbitrary : t

  val effect : t -> Effect.t
  val coeffect : t -> Coeffect.t

  val pure_and_copure : t -> bool

  val effect_only : Effect.t -> t
  val coeffect_only : Coeffect.t -> t

  val join : t -> t -> t
  val join_list_map : 'a list -> ('a -> t) -> t
end = struct
  type t = Effect.t * Coeffect.t

  let none = Effect.None, Coeffect.None
  let arbitrary = Effect.Arbitrary, Coeffect.Read_mutable

  let effect (e, _ce) = e
  let coeffect (_e, ce) = ce

  let pure_and_copure (e, ce) = Effect.pure e && Coeffect.copure ce

  let effect_only e = e, Coeffect.None
  let coeffect_only ce = Effect.None, ce

  let join (e1, ce1) (e2, ce2) =
    Effect.join e1 e2, Coeffect.join ce1 ce2

  let join_list_map xs f =
    match xs with
    | [] -> none
    | x::xs -> List.fold_left (fun acc x -> join acc (f x)) (f x) xs
end

(* The default instruction selection class *)

class virtual selector_generic = object (self)

(* Analyses the effects and coeffects of an expression.  This is used across
   a whole list of expressions with a view to determining which expressions
   may have their evaluation deferred.  The result of this function, modulo
   target-specific judgements if the [effects_of] method is overridden, is a
   property of the Cmm language rather than anything particular about the
   instruction selection algorithm in this file.

   In the case of e.g. an OCaml function call, the arguments whose evaluation
   cannot be deferred (cf. [emit_parts], below) are computed in right-to-left
   order first with their results going into temporaries, then the block is
   allocated, then the remaining arguments are evaluated before being
   combined with the temporaries. *)
method effects_of exp =
  let module EC = Effect_and_coeffect in
  match exp with
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
  | Cconst_pointer _ | Cconst_natpointer _ | Cconst_blockheader _
  | Cvar _ -> EC.none
  | Ctuple el -> EC.join_list_map el self#effects_of
  | Clet (_id, arg, body) ->
    EC.join (self#effects_of arg) (self#effects_of body)
  | Csequence (e1, e2) ->
    EC.join (self#effects_of e1) (self#effects_of e2)
  | Cifthenelse (cond, ifso, ifnot) ->
    EC.join (self#effects_of cond)
      (EC.join (self#effects_of ifso) (self#effects_of ifnot))
  | Cop (op, args) ->
    let from_op =
      match op with
      | Capply _ | Cextcall _ -> EC.arbitrary
      | Calloc -> EC.none
      | Cstore _ -> EC.effect_only Effect.Arbitrary
      | Craise _ | Ccheckbound _ -> EC.effect_only Effect.Raise
      | Cload (_, Asttypes.Immutable) -> EC.none
      | Cload (_, Asttypes.Mutable) ->
        (* Loads from the current function's closure are a common case.
           Such loads are always from immutable blocks, even though for the
           moment there is insufficient information propagated from the
           middle-end for them to be marked [Immutable]. *)
        let is_from_closure =
          match !current_function_env_param with
          | None -> false
          | Some env_param ->
            match args with
            | [Cop (Cadda, [Cvar ident; Cconst_int _])] ->
              Ident.same ident env_param
            | _ -> false
        in
        if is_from_closure then EC.none
        else EC.coeffect_only Coeffect.Read_mutable
      | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi | Cand | Cor | Cxor
      | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _ | Cnegf | Cabsf
      | Caddf | Csubf | Cmulf | Cdivf | Cfloatofint | Cintoffloat | Ccmpf _ ->
        EC.none
    in
    EC.join from_op (EC.join_list_map args self#effects_of)
  | Cassign _ | Cswitch _ | Cloop _ | Ccatch _ | Cexit _ | Ctrywith _ ->
    EC.arbitrary

(* A syntactic criterion used in addition to judgements about (co)effects as
   to whether the evaluation of a given expression may be deferred by
   [emit_parts].  This criterion is a property of the instruction selection
   algorithm in this file rather than a property of the Cmm language.

   The criterion is used to enforce one particular restriction at the
   moment: [Calloc] instructions may not be deferred.  This is to ensure
   that it is not possible to interperse some expression that might trigger
   a GC between the [Ialloc] instruction that creates the block and the
   instructions that fill it up.
*)
method private cannot_defer = function
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
  | Cconst_pointer _ | Cconst_natpointer _ | Cconst_blockheader _
  | Cvar _ -> false
  | Ctuple el -> List.exists self#cannot_defer el
  | Clet (_id, arg, body) -> self#cannot_defer arg || self#cannot_defer body
  | Csequence (e1, e2) -> self#cannot_defer e1 || self#cannot_defer e2
  | Cifthenelse (cond, e1, e2) ->
    self#cannot_defer cond || self#cannot_defer e1 || self#cannot_defer e2
  | Cop (op, args) ->
    begin match op with
    | Calloc -> true
    | _ -> List.exists self#cannot_defer args
    end
  | Cassign _ | Cswitch _ | Cloop _ | Ccatch _ | Cexit _ | Ctrywith _ ->
    true

(* Says whether an integer constant is a suitable immediate argument *)

method virtual is_immediate : int -> bool

(* Selection of addressing modes *)

method virtual select_addressing :
  Cmm.memory_chunk -> Cmm.expression -> Arch.addressing_mode * Cmm.expression

(* Default instruction selection for stores (of words) *)

method select_store is_assign addr arg =
  (Istore(Word_val, addr, is_assign), arg)

(* call marking methods, documented in selectgen.mli *)

method mark_call =
  Proc.contains_calls := true

method mark_tailcall = ()

method mark_c_tailcall = ()

method mark_instr = function
  | Iop (Icall_ind | Icall_imm _ | Iextcall _) ->
      self#mark_call
  | Iop (Itailcall_ind | Itailcall_imm _) ->
      self#mark_tailcall
  | Iop (Ialloc _) ->
      self#mark_call (* caml_alloc*, caml_garbage_collection *)
  | Iop (Iintop Icheckbound | Iintop_imm(Icheckbound, _)) ->
      self#mark_c_tailcall (* caml_ml_array_bound_error *)
  | Iraise raise_kind ->
    begin match raise_kind with
      | Lambda.Raise_notrace -> ()
      | Lambda.Raise_regular | Lambda.Raise_reraise ->
        if !Clflags.debug then (* PR#6239 *)
        (* caml_stash_backtrace; we #mark_call rather than
           #mark_c_tailcall to get a good stack backtrace *)
          self#mark_call
    end
  | Itrywith _ ->
    self#mark_call
  | _ -> ()

(* Default instruction selection for operators *)

method select_operation op args =
  match (op, args) with
    (Capply(ty, dbg), Cconst_symbol s :: rem) -> (Icall_imm s, rem)
  | (Capply(ty, dbg), _) -> (Icall_ind, args)
  | (Cextcall(s, ty, alloc, dbg), _) -> (Iextcall(s, alloc), args)
  | (Cload (chunk, _mut), [arg]) ->
      let (addr, eloc) = self#select_addressing chunk arg in
      (Iload(chunk, addr), [eloc])
  | (Cstore (chunk, init), [arg1; arg2]) ->
      let (addr, eloc) = self#select_addressing chunk arg1 in
      let is_assign =
        match init with
        | Lambda.Initialization -> false
        | Lambda.Assignment -> true
      in
      if chunk = Word_int || chunk = Word_val then begin
        let (op, newarg2) = self#select_store is_assign addr arg2 in
        (op, [newarg2; eloc])
      end else begin
        (Istore(chunk, addr, is_assign), [arg2; eloc])
        (* Inversion addr/datum in Istore *)
      end
  | (Calloc, _) -> (Ialloc 0, args)
  | (Caddi, _) -> self#select_arith_comm Iadd args
  | (Csubi, _) -> self#select_arith Isub args
  | (Cmuli, _) -> self#select_arith_comm Imul args
  | (Cmulhi, _) -> self#select_arith_comm Imulh args
  | (Cdivi, _) -> (Iintop Idiv, args)
  | (Cmodi, _) -> (Iintop Imod, args)
  | (Cand, _) -> self#select_arith_comm Iand args
  | (Cor, _) -> self#select_arith_comm Ior args
  | (Cxor, _) -> self#select_arith_comm Ixor args
  | (Clsl, _) -> self#select_shift Ilsl args
  | (Clsr, _) -> self#select_shift Ilsr args
  | (Casr, _) -> self#select_shift Iasr args
  | (Ccmpi comp, _) -> self#select_arith_comp (Isigned comp) args
  | (Caddv, _) -> self#select_arith_comm Iadd args
  | (Cadda, _) -> self#select_arith_comm Iadd args
  | (Ccmpa comp, _) -> self#select_arith_comp (Iunsigned comp) args
  | (Cnegf, _) -> (Inegf, args)
  | (Cabsf, _) -> (Iabsf, args)
  | (Caddf, _) -> (Iaddf, args)
  | (Csubf, _) -> (Isubf, args)
  | (Cmulf, _) -> (Imulf, args)
  | (Cdivf, _) -> (Idivf, args)
  | (Cfloatofint, _) -> (Ifloatofint, args)
  | (Cintoffloat, _) -> (Iintoffloat, args)
  | (Ccheckbound _, _) -> self#select_arith Icheckbound args
  | _ -> fatal_error "Selection.select_oper"

method private select_arith_comm op = function
    [arg; Cconst_int n] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | [arg; Cconst_pointer n] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | [Cconst_int n; arg] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | [Cconst_pointer n; arg] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

method private select_arith op = function
    [arg; Cconst_int n] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | [arg; Cconst_pointer n] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

method private select_shift op = function
    [arg; Cconst_int n] when n >= 0 && n < Arch.size_int * 8 ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

method private select_arith_comp cmp = function
    [arg; Cconst_int n] when self#is_immediate n ->
      (Iintop_imm(Icomp cmp, n), [arg])
  | [arg; Cconst_pointer n] when self#is_immediate n ->
      (Iintop_imm(Icomp cmp, n), [arg])
  | [Cconst_int n; arg] when self#is_immediate n ->
      (Iintop_imm(Icomp(swap_intcomp cmp), n), [arg])
  | [Cconst_pointer n; arg] when self#is_immediate n ->
      (Iintop_imm(Icomp(swap_intcomp cmp), n), [arg])
  | args ->
      (Iintop(Icomp cmp), args)

(* Instruction selection for conditionals *)

method select_condition = function
    Cop(Ccmpi cmp, [arg1; Cconst_int n]) when self#is_immediate n ->
      (Iinttest_imm(Isigned cmp, n), arg1)
  | Cop(Ccmpi cmp, [Cconst_int n; arg2]) when self#is_immediate n ->
      (Iinttest_imm(Isigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpi cmp, [arg1; Cconst_pointer n]) when self#is_immediate n ->
      (Iinttest_imm(Isigned cmp, n), arg1)
  | Cop(Ccmpi cmp, [Cconst_pointer n; arg2]) when self#is_immediate n ->
      (Iinttest_imm(Isigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpi cmp, args) ->
      (Iinttest(Isigned cmp), Ctuple args)
  | Cop(Ccmpa cmp, [arg1; Cconst_pointer n]) when self#is_immediate n ->
      (Iinttest_imm(Iunsigned cmp, n), arg1)
  | Cop(Ccmpa cmp, [arg1; Cconst_int n]) when self#is_immediate n ->
      (Iinttest_imm(Iunsigned cmp, n), arg1)
  | Cop(Ccmpa cmp, [Cconst_pointer n; arg2]) when self#is_immediate n ->
      (Iinttest_imm(Iunsigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpa cmp, [Cconst_int n; arg2]) when self#is_immediate n ->
      (Iinttest_imm(Iunsigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpa cmp, args) ->
      (Iinttest(Iunsigned cmp), Ctuple args)
  | Cop(Ccmpf cmp, args) ->
      (Ifloattest(cmp, false), Ctuple args)
  | Cop(Cand, [arg; Cconst_int 1]) ->
      (Ioddtest, arg)
  | arg ->
      (Itruetest, arg)

(* Return an array of fresh registers of the given type.
   Normally implemented as Reg.createv, but some
   ports (e.g. Arm) can override this definition to store float values
   in pairs of integer registers. *)

method regs_for tys = Reg.createv tys

(* Buffering of instruction sequences *)

val mutable instr_seq = dummy_instr

method insert_debug desc dbg arg res =
  instr_seq <- instr_cons_debug desc arg res dbg instr_seq

method insert desc arg res =
  instr_seq <- instr_cons desc arg res instr_seq

method extract =
  let rec extract res i =
    if i == dummy_instr
    then res
    else extract {i with next = res} i.next in
  extract (end_instr()) instr_seq

(* Insert a sequence of moves from one pseudoreg set to another. *)

method insert_move src dst =
  if src.stamp <> dst.stamp then
    self#insert (Iop Imove) [|src|] [|dst|]

method insert_moves src dst =
  for i = 0 to min (Array.length src) (Array.length dst) - 1 do
    self#insert_move src.(i) dst.(i)
  done

(* Adjust the types of destination pseudoregs for a [Cassign] assignment.
   The type inferred at [let] binding might be [Int] while we assign
   something of type [Val] (PR#6501). *)

method adjust_type src dst =
  let ts = src.typ and td = dst.typ in
  if ts <> td then
    match ts, td with
    | Val, Int -> dst.typ <- Val
    | Int, Val -> ()
    | _, _ -> fatal_error("Selection.adjust_type: bad assignment to "
                                                           ^ Reg.name dst)

method adjust_types src dst =
  for i = 0 to min (Array.length src) (Array.length dst) - 1 do
    self#adjust_type src.(i) dst.(i)
  done

(* Insert moves and stack offsets for function arguments and results *)

method insert_move_args arg loc stacksize =
  if stacksize <> 0 then self#insert (Iop(Istackoffset stacksize)) [||] [||];
  self#insert_moves arg loc

method insert_move_results loc res stacksize =
  if stacksize <> 0 then self#insert(Iop(Istackoffset(-stacksize))) [||] [||];
  self#insert_moves loc res

(* Add an Iop opcode. Can be overridden by processor description
   to insert moves before and after the operation, i.e. for two-address
   instructions, or instructions using dedicated registers. *)

method insert_op_debug op dbg rs rd =
  self#insert_debug (Iop op) dbg rs rd;
  rd

method insert_op op rs rd =
  self#insert_op_debug op Debuginfo.none rs rd

(* Add the instructions for the given expression
   at the end of the self sequence *)

method emit_expr env exp =
  match exp with
    Cconst_int n ->
      let r = self#regs_for typ_int in
      Some(self#insert_op (Iconst_int(Nativeint.of_int n)) [||] r)
  | Cconst_natint n ->
      let r = self#regs_for typ_int in
      Some(self#insert_op (Iconst_int n) [||] r)
  | Cconst_blockheader n ->
      let r = self#regs_for typ_int in
      Some(self#insert_op (Iconst_blockheader n) [||] r)
  | Cconst_float n ->
      let r = self#regs_for typ_float in
      Some(self#insert_op (Iconst_float (Int64.bits_of_float n)) [||] r)
  | Cconst_symbol n ->
      let r = self#regs_for typ_val in
      Some(self#insert_op (Iconst_symbol n) [||] r)
  | Cconst_pointer n ->
      let r = self#regs_for typ_val in  (* integer as Caml value *)
      Some(self#insert_op (Iconst_int(Nativeint.of_int n)) [||] r)
  | Cconst_natpointer n ->
      let r = self#regs_for typ_val in  (* integer as Caml value *)
      Some(self#insert_op (Iconst_int n) [||] r)
  | Cvar v ->
      begin try
        Some(Tbl.find v env)
      with Not_found ->
        fatal_error("Selection.emit_expr: unbound var " ^ Ident.unique_name v)
      end
  | Clet(v, e1, e2) ->
      begin match self#emit_expr env e1 with
        None -> None
      | Some r1 -> self#emit_expr (self#bind_let env v r1) e2
      end
  | Cassign(v, e1) ->
      let rv =
        try
          Tbl.find v env
        with Not_found ->
          fatal_error ("Selection.emit_expr: unbound var " ^ Ident.name v) in
      begin match self#emit_expr env e1 with
        None -> None
      | Some r1 -> self#adjust_types r1 rv; self#insert_moves r1 rv; Some [||]
      end
  | Ctuple [] ->
      Some [||]
  | Ctuple exp_list ->
      begin match self#emit_parts_list env exp_list with
        None -> None
      | Some(simple_list, ext_env) ->
          self#emit_tuple ext_env simple_list
      end
  | Cop(Craise (k, dbg), [arg]) ->
      begin match self#emit_expr env arg with
        None -> None
      | Some r1 ->
          let rd = [|Proc.loc_exn_bucket|] in
          self#insert (Iop Imove) r1 rd;
          self#insert_debug (Iraise k) dbg rd [||];
          None
      end
  | Cop(Ccmpf comp, args) ->
      self#emit_expr env (Cifthenelse(exp, Cconst_int 1, Cconst_int 0))
  | Cop(op, args) ->
      begin match self#emit_parts_list env args with
        None -> None
      | Some(simple_args, env) ->
          let ty = oper_result_type op in
          let (new_op, new_args) = self#select_operation op simple_args in
          let dbg = debuginfo_op op in
          match new_op with
            Icall_ind ->
              begin match self#emit_tuple env new_args with
              | None -> None
              | Some r1 ->
                  let rarg = Array.sub r1 1 (Array.length r1 - 1) in
                  let rd = self#regs_for ty in
                  let (loc_arg, stack_ofs) = Proc.loc_arguments rarg in
                  let loc_res = Proc.loc_results rd in
                  self#insert_move_args rarg loc_arg stack_ofs;
                  self#insert_debug (Iop Icall_ind) dbg
                              (Array.append [|r1.(0)|] loc_arg) loc_res;
                  self#insert_move_results loc_res rd stack_ofs;
                  Some rd
              end
          | Icall_imm lbl ->
              begin match self#emit_tuple env new_args with
              | None -> None
              | Some r1 ->
                  let rd = self#regs_for ty in
                  let (loc_arg, stack_ofs) = Proc.loc_arguments r1 in
                  let loc_res = Proc.loc_results rd in
                  self#insert_move_args r1 loc_arg stack_ofs;
                  self#insert_debug (Iop(Icall_imm lbl)) dbg loc_arg loc_res;
                  self#insert_move_results loc_res rd stack_ofs;
                  Some rd
              end
          | Iextcall(lbl, alloc) ->
              begin match self#emit_extcall_args env new_args with
              | None -> None
              | Some (loc_arg, stack_ofs) ->
                  let rd = self#regs_for ty in
                  let loc_res = self#insert_op_debug (Iextcall(lbl, alloc)) dbg
                                        loc_arg (Proc.loc_external_results rd)
                  in
                  self#insert_move_results loc_res rd stack_ofs;
                  Some rd
              end
          | Ialloc _ ->
              let rd = self#regs_for typ_val in
              let size = size_expr env (Ctuple new_args) in
              self#insert (Iop(Ialloc size)) [||] rd;
              self#emit_stores env new_args rd;
              Some rd
          | op ->
              begin match self#emit_tuple env new_args with
              | None -> None
              | Some r1 ->
                  let rd = self#regs_for ty in
                  Some (self#insert_op_debug op dbg r1 rd)
              end
      end
  | Csequence(e1, e2) ->
      begin match self#emit_expr env e1 with
        None -> None
      | Some r1 -> self#emit_expr env e2
      end
  | Cifthenelse(econd, eif, eelse) ->
      let (cond, earg) = self#select_condition econd in
      begin match self#emit_expr env earg with
        None -> None
      | Some rarg ->
          let (rif, sif) = self#emit_sequence env eif in
          let (relse, selse) = self#emit_sequence env eelse in
          let r = join rif sif relse selse in
          self#insert (Iifthenelse(cond, sif#extract, selse#extract))
                      rarg [||];
          r
      end
  | Cswitch(esel, index, ecases) ->
      begin match self#emit_expr env esel with
        None -> None
      | Some rsel ->
          let rscases = Array.map (self#emit_sequence env) ecases in
          let r = join_array rscases in
          self#insert (Iswitch(index,
                               Array.map (fun (r, s) -> s#extract) rscases))
                      rsel [||];
          r
      end
  | Cloop(ebody) ->
      let (rarg, sbody) = self#emit_sequence env ebody in
      self#insert (Iloop(sbody#extract)) [||] [||];
      Some [||]
  | Ccatch(nfail, ids, e1, e2) ->
      let rs =
        List.map
          (fun id ->
            let r = self#regs_for typ_val in name_regs id r; r)
          ids in
      catch_regs := (nfail, Array.concat rs) :: !catch_regs ;
      let (r1, s1) = self#emit_sequence env e1 in
      catch_regs := List.tl !catch_regs ;
      let new_env =
        List.fold_left
        (fun env (id,r) -> Tbl.add id r env)
        env (List.combine ids rs) in
      let (r2, s2) = self#emit_sequence new_env e2 in
      let r = join r1 s1 r2 s2 in
      self#insert (Icatch(nfail, s1#extract, s2#extract)) [||] [||];
      r
  | Cexit (nfail,args) ->
      begin match self#emit_parts_list env args with
        None -> None
      | Some (simple_list, ext_env) ->
          begin match self#emit_tuple ext_env simple_list with
          | None -> None
          | Some src ->
              let dest =
                try List.assoc nfail !catch_regs
                with Not_found ->
                  Misc.fatal_error
                    ("Selectgen.emit_expr, on exit("^string_of_int nfail^")") in
              self#insert_moves src dest ;
              self#insert (Iexit nfail) [||] [||];
              None
          end
      end
  | Ctrywith(e1, v, e2) ->
      let (r1, s1) = self#emit_sequence env e1 in
      let rv = self#regs_for typ_val in
      let (r2, s2) = self#emit_sequence (Tbl.add v rv env) e2 in
      let r = join r1 s1 r2 s2 in
      self#insert
        (Itrywith(s1#extract,
                  instr_cons (Iop Imove) [|Proc.loc_exn_bucket|] rv
                             (s2#extract)))
        [||] [||];
      r

method private emit_sequence env exp =
  let s = {< instr_seq = dummy_instr >} in
  let r = s#emit_expr env exp in
  (r, s)

method private bind_let env v r1 =
  if all_regs_anonymous r1 then begin
    name_regs v r1;
    Tbl.add v r1 env
  end else begin
    let rv = Reg.createv_like r1 in
    name_regs v rv;
    self#insert_moves r1 rv;
    Tbl.add v rv env
  end

(* The following two functions, [emit_parts] and [emit_parts_list], force
   right-to-left evaluation order as required by the Flambda [Un_anf] pass
   (and to be consistent with the bytecode compiler). *)

method private emit_parts (env:environment) ~effects_after exp =
  let module EC = Effect_and_coeffect in
  let may_defer_evaluation =
    let ec = self#effects_of exp in
    match EC.effect ec with
    | Effect.Arbitrary (*| Effect.Raise*) ->
      (* Preserve the ordering of effectful expressions by evaluating them
         early (in the correct order) and assigning their results to
         temporaries.  We can avoid this in just one case: if we know that
         every [exp'] in the original expression list (cf. [emit_parts_list])
         to be evaluated after [exp] cannot possibly affect the result of
         [exp] or depend on the result of [exp], then [exp] may be deferred.
         (Checking purity here is not enough: we need to check copurity too
         to avoid e.g. moving mutable reads earlier than the raising of
         an exception.) *)
      false
    | Effect.Raise ->
      EC.pure_and_copure effects_after
    | Effect.None ->
      match EC.coeffect ec with
      | Coeffect.None ->
        (* Pure expressions may be moved. *)
        true
      | Coeffect.Read_mutable ->
        (* Read-mutable expressions may only be deferred if evaluation of
           every [exp'] (for [exp'] as in the comment above) has no effects
           "worse" (in the sense of the ordering in [Effect.t]) than raising
           an exception. *)
        match EC.effect effects_after with
        | Effect.None | Effect.Raise -> true
        | Effect.Arbitrary -> false
  in
  (* Even though some expressions may look like they can be deferred from
     the (co)effect analysis, it may be forbidden to move them.  (See
     [cannot_defer], above.) *)
  if may_defer_evaluation && not (self#cannot_defer exp) then
    Some (exp, env)
  else begin
    match self#emit_expr env exp with
      None -> None
    | Some r ->
        if Array.length r = 0 then
          Some (Ctuple [], env)
        else begin
          (* The normal case *)
          let id = Ident.create "bind" in
          if all_regs_anonymous r then
            (* r is an anonymous, unshared register; use it directly *)
            Some (Cvar id, Tbl.add id r env)
          else begin
            (* Introduce a fresh temp to hold the result *)
            let tmp = Reg.createv_like r in
            self#insert_moves r tmp;
            Some (Cvar id, Tbl.add id tmp env)
          end
        end
  end

method private emit_parts_list (env:environment) exp_list =
  let module EC = Effect_and_coeffect in
  let exp_list_right_to_left, _effect =
    (* Annotate each expression with the (co)effects that happen after it
       when the original expression list is evaluated from right to left.
       The resulting expression list has the rightmost expression first. *)
    List.fold_left (fun (exp_list, effects_after) exp ->
        let exp_effect = self#effects_of exp in
        (exp, effects_after)::exp_list, EC.join exp_effect effects_after)
      ([], EC.none)
      exp_list
  in
  List.fold_left (fun results_and_env (exp, effects_after) ->
      match results_and_env with
      | None -> None
      | Some (result, env) ->
          match self#emit_parts env exp ~effects_after with
          | None -> None
          | Some (exp_result, env) -> Some (exp_result :: result, env))
    (Some ([], env))
    exp_list_right_to_left

method private emit_tuple_not_flattened env exp_list =
  let rec emit_list = function
  | [] -> Some []
  | exp :: rem ->
      (* Again, force right-to-left evaluation *)
      match emit_list rem with
      | None -> None
      | Some loc_rem ->
        match self#emit_expr env exp with
        | None -> None
        | Some loc_exp -> Some (loc_exp :: loc_rem)
  in
  emit_list exp_list

method private emit_tuple env exp_list =
  match self#emit_tuple_not_flattened env exp_list with
  | None -> None
  | Some regs -> Some (Array.concat regs)

method emit_extcall_args env args =
  match self#emit_tuple_not_flattened env args with
  | None -> None
  | Some args ->
    let arg_hard_regs, stack_ofs =
      Proc.loc_external_arguments (Array.of_list args)
    in
    (* Flattening [args] and [arg_hard_regs] causes parts of values split
      across multiple registers to line up correctly, by virtue of the
      semantics of [split_int64_for_32bit_target] in cmmgen.ml, and the
      required semantics of [loc_external_arguments] (see proc.mli). *)
    let args = Array.concat args in
    let arg_hard_regs = Array.concat (Array.to_list arg_hard_regs) in
    self#insert_move_args args arg_hard_regs stack_ofs;
    Some (arg_hard_regs, stack_ofs)

method emit_stores env data regs_addr =
  let a =
    ref (Arch.offset_addressing Arch.identity_addressing (-Arch.size_int)) in
  List.iter
    (fun e ->
      let (op, arg) = self#select_store false !a e in
      match self#emit_expr env arg with
        None -> assert false
      | Some regs ->
          match op with
            Istore(_, _, _) ->
              for i = 0 to Array.length regs - 1 do
                let r = regs.(i) in
                let kind = if r.typ = Float then Double_u else Word_val in
                self#insert (Iop(Istore(kind, !a, false)))
                            (Array.append [|r|] regs_addr) [||];
                a := Arch.offset_addressing !a (size_component r.typ)
              done
          | _ ->
              self#insert (Iop op) (Array.append regs regs_addr) [||];
              a := Arch.offset_addressing !a (size_expr env e))
    data

(* Same, but in tail position *)

method private emit_return env exp =
  match self#emit_expr env exp with
    None -> ()
  | Some r ->
      let loc = Proc.loc_results r in
      self#insert_moves r loc;
      self#insert Ireturn loc [||]

method emit_tail env exp =
  match exp with
    Clet(v, e1, e2) ->
      begin match self#emit_expr env e1 with
        None -> ()
      | Some r1 -> self#emit_tail (self#bind_let env v r1) e2
      end
  | Cop(Capply(ty, dbg) as op, args) ->
      begin match self#emit_parts_list env args with
        None -> ()
      | Some(simple_args, env) ->
          let (new_op, new_args) = self#select_operation op simple_args in
          match new_op with
            Icall_ind ->
              begin match self#emit_tuple env new_args with
              | None -> ()
              | Some r1 ->
                  let rarg = Array.sub r1 1 (Array.length r1 - 1) in
                  let (loc_arg, stack_ofs) = Proc.loc_arguments rarg in
                  if stack_ofs = 0 then begin
                    self#insert_moves rarg loc_arg;
                    self#insert (Iop Itailcall_ind)
                                (Array.append [|r1.(0)|] loc_arg) [||]
                  end else begin
                    let rd = self#regs_for ty in
                    let loc_res = Proc.loc_results rd in
                    self#insert_move_args rarg loc_arg stack_ofs;
                    self#insert_debug (Iop Icall_ind) dbg
                                (Array.append [|r1.(0)|] loc_arg) loc_res;
                    self#insert(Iop(Istackoffset(-stack_ofs))) [||] [||];
                    self#insert Ireturn loc_res [||]
                  end
              end
          | Icall_imm lbl ->
              begin match self#emit_tuple env new_args with
              | None -> ()
              | Some r1 ->
                  let (loc_arg, stack_ofs) = Proc.loc_arguments r1 in
                  if stack_ofs = 0 then begin
                    self#insert_moves r1 loc_arg;
                    self#insert (Iop(Itailcall_imm lbl)) loc_arg [||]
                  end else if lbl = !current_function_name then begin
                    let loc_arg' = Proc.loc_parameters r1 in
                    self#insert_moves r1 loc_arg';
                    self#insert (Iop(Itailcall_imm lbl)) loc_arg' [||]
                  end else begin
                    let rd = self#regs_for ty in
                    let loc_res = Proc.loc_results rd in
                    self#insert_move_args r1 loc_arg stack_ofs;
                    self#insert_debug (Iop(Icall_imm lbl)) dbg loc_arg loc_res;
                    self#insert(Iop(Istackoffset(-stack_ofs))) [||] [||];
                    self#insert Ireturn loc_res [||]
                  end
              end
          | _ -> fatal_error "Selection.emit_tail"
      end
  | Csequence(e1, e2) ->
      begin match self#emit_expr env e1 with
        None -> ()
      | Some r1 -> self#emit_tail env e2
      end
  | Cifthenelse(econd, eif, eelse) ->
      let (cond, earg) = self#select_condition econd in
      begin match self#emit_expr env earg with
        None -> ()
      | Some rarg ->
          self#insert (Iifthenelse(cond, self#emit_tail_sequence env eif,
                                         self#emit_tail_sequence env eelse))
                      rarg [||]
      end
  | Cswitch(esel, index, ecases) ->
      begin match self#emit_expr env esel with
        None -> ()
      | Some rsel ->
          self#insert
            (Iswitch(index, Array.map (self#emit_tail_sequence env) ecases))
            rsel [||]
      end
  | Ccatch(nfail, ids, e1, e2) ->
       let rs =
        List.map
          (fun id ->
            let r = self#regs_for typ_val in
            name_regs id r  ;
            r)
          ids in
      catch_regs := (nfail, Array.concat rs) :: !catch_regs ;
      let s1 = self#emit_tail_sequence env e1 in
      catch_regs := List.tl !catch_regs ;
      let new_env =
        List.fold_left
        (fun env (id,r) -> Tbl.add id r env)
        env (List.combine ids rs) in
      let s2 = self#emit_tail_sequence new_env e2 in
      self#insert (Icatch(nfail, s1, s2)) [||] [||]
  | Ctrywith(e1, v, e2) ->
      let (opt_r1, s1) = self#emit_sequence env e1 in
      let rv = self#regs_for typ_val in
      let s2 = self#emit_tail_sequence (Tbl.add v rv env) e2 in
      self#insert
        (Itrywith(s1#extract,
                  instr_cons (Iop Imove) [|Proc.loc_exn_bucket|] rv s2))
        [||] [||];
      begin match opt_r1 with
        None -> ()
      | Some r1 ->
          let loc = Proc.loc_results r1 in
          self#insert_moves r1 loc;
          self#insert Ireturn loc [||]
      end
  | _ ->
      self#emit_return env exp

method private emit_tail_sequence env exp =
  let s = {< instr_seq = dummy_instr >} in
  s#emit_tail env exp;
  s#extract

(* Sequentialization of a function definition *)

method emit_fundecl f =
  Proc.contains_calls := false;
  current_function_name := f.Cmm.fun_name;
  current_function_env_param := f.Cmm.fun_env;
  let rargs =
    List.map
      (fun (id, ty) -> let r = self#regs_for ty in name_regs id r; r)
      f.Cmm.fun_args in
  let rarg = Array.concat rargs in
  let loc_arg = Proc.loc_parameters rarg in
  let env =
    List.fold_right2
      (fun (id, ty) r env -> Tbl.add id r env)
      f.Cmm.fun_args rargs Tbl.empty in
  self#insert_moves loc_arg rarg;
  self#emit_tail env f.Cmm.fun_body;
  let body = self#extract in
  instr_iter (fun instr -> self#mark_instr instr.Mach.desc) body;
  { fun_name = f.Cmm.fun_name;
    fun_args = loc_arg;
    fun_body = body;
    fun_fast = f.Cmm.fun_fast;
    fun_dbg  = f.Cmm.fun_dbg }

end

(* Tail call criterion (estimated).  Assumes:
- all arguments are of type "int" (always the case for OCaml function calls)
- one extra argument representing the closure environment (conservative).
*)

let is_tail_call nargs =
  assert (Reg.dummy.typ = Int);
  let args = Array.make (nargs + 1) Reg.dummy in
  let (loc_arg, stack_ofs) = Proc.loc_arguments args in
  stack_ofs = 0

let _ =
  Simplif.is_tail_native_heuristic := is_tail_call

let reset () =
  catch_regs := [];
  current_function_name := ""
