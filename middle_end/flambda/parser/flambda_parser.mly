%{
open Fexpr

let make_loc (startpos, endpos) = Lambda.of_raw_location {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let make_tag ~loc:_ = function
  | s, None -> int_of_string s
  | _, Some _ ->
    failwith "No modifier allowed for tags"

let make_tagged_immediate ~loc:_ = function
  | s, Some 't' -> s
  | _, _ ->
    failwith "Tagged immediates must have modifier 't'"

let make_const_int (i, m) : Fexpr.const =
  match m with
  | None -> Naked_nativeint (Int64.of_string i)
  | Some 'u' -> Naked_immediate i
  | Some 't' -> Tagged_immediate i
  | Some 'l' -> Naked_int32 (Int32.of_string i)
  | Some 'L' -> Naked_int64 (Int64.of_string i)
  | Some c -> failwith (Printf.sprintf "Unknown int modifier %c" c)

let make_const_float (i, m) =
  match m with
  | None -> Naked_float (float_of_string i)
  | Some c -> failwith (Printf.sprintf "Unknown float modifier %c" c)

let partition_map (f : 'a -> [ `Left of 'b | `Right of 'c ]) (l : 'a list)
    : 'b list * 'c list =
  let next (acc_l, acc_r) x =
    match f x with
    | `Left b -> b :: acc_l, acc_r
    | `Right c -> acc_l, c :: acc_r
  in
  let rev_l, rev_r = List.fold_left next ([], []) l in
  List.rev rev_l, List.rev rev_r

let make_segment
      (bindings : [ `Code of code_binding
                  | `Closure of static_closure_binding ] list)
      (closure_elements : closure_element list option) =
  let code_bindings, closure_bindings =
    partition_map (function
    | `Code code_binding -> `Left code_binding
    | `Closure closure_binding -> `Right closure_binding
    ) bindings
  in
  { code_bindings; closure_bindings; closure_elements }
%}

/* Tokens */

%token AND   [@symbol "and"]
%token AT    [@symbol "@"]
%token APPLY [@symbol "apply"]
%token BLOCK [@symbol "Block"]
%token CCALL  [@symbol "ccall"]
%token CLOSURE  [@symbol "closure"]
%token CODE  [@symbol "code"]
%token COLON  [@symbol ":"]
%token COMMA  [@symbol ","]
%token CONT  [@symbol "cont"]
%token DOT   [@symbol "."]
%token EQUAL [@symbol "="]
%token EXN   [@symbol "exn"]
%token <string * char option> FLOAT
%token HCF   [@symbol "HCF"]
%token IN    [@symbol "in"]
%token IS_INT  [@symbol "is_int"]
%token <string * char option> INT
%token LANGLE [@symbol "<"]
%token LBRACE [@symbol "{"]
%token LBRACKET [@symbol "["]
%token LET    [@symbol "let"]
%token LETK   [@symbol "letk"]
%token <string> LIDENT
%token LPAREN [@symbol "("]
%token MINUS    [@symbol "-"]
%token MINUSDOT [@symbol "-."]
%token MINUSGREATER [@symbol "->"]
%token NEWER_VERSION_OF [@symbol "newer_version_of"]
%token OPAQUE [@symbol "opaque_identity"]
%token PLUS     [@symbol "+"]
%token PLUSDOT  [@symbol "+."]
%token PROJECT_VAR [@symbol "Project_var"]
%token RANGLE [@symbol ">"]
%token RBRACE [@symbol "}"]
%token RBRACKET [@symbol "]"]
%token REC    [@symbol "rec"]
%token RPAREN [@symbol ")"]
%token SEMICOLON [@symbol ";"]
%token SEGMENT [@symbol "segment"]
%token STUB   [@symbol "stub"]
%token STAR   [@symbol "*"]
%token SWITCH [@symbol "switch"]
%token SYMBOL [@symbol "symbol"]
%token TAG    [@symbol "tag"]
%token <string> UIDENT
%token UNDERSCORE [@symbol "_"]
%token UNREACHABLE [@symbol "Unreachable"]
%token WITH [@symbol "with"]
%token EOF

%start flambda_unit
%type <Fexpr.flambda_unit> flambda_unit
%type <Fexpr.static_structure> static_structure
%type <Fexpr.expr> expr
(* %type <Fexpr.name> name *)
%type <Fexpr.named> named
%type <Fexpr.of_kind_value> of_kind_value
%%

flambda_unit:
  | return_cont = continuation
    exception_cont = option(exn_continuation)
    body = expr
    EOF
    { { return_cont; exception_cont; body } }
;

exn_continuation:
  | STAR cont = continuation { cont }
;

let_symbol:
  | LET; bindings = symbol_bindings; IN; body = expr; { { bindings; body } }
;

symbol_bindings:
  | SYMBOL s = static_structure { Simple s }
  | segs = segments { Segments segs }
;

segments:
  | seg = single_segment { [ seg ] }
  | SEGMENT; seg_and_segs = segment_and_more_segments
    { let bindings, elts, segs = seg_and_segs in
      let seg = make_segment bindings elts in
      seg :: segs }
;

single_segment:
  | bindings = separated_nonempty_list(AND, code_or_closure_binding);
    closure_elements = with_closure_elements_opt;  
    { make_segment bindings closure_elements }
;

(* 
 * This odd arrangement is needed to avoid a LALR(1) ambiguity.  We have
 * bindings separated by AND and segments separated by AND SEGMENT.  So if we
 * try to do the obvious thing, namely something more like:
 * 
 *   segments: segment | segment AND segments
 *   segment: SEGMENT segment_body
 *   segment_body: binding | binding AND segment_body 
 *
 * we end up with a shift/reduce conflict between (shift):
 *
 *   segment_body: binding . AND segment_body
 *
 * and (reduce):
 *
 *   segment: SEGMENT segment_body .
 *   segments: segment . AND segments
 *
 * Only the presence of SEGMENT after AND disambiguates, and we have only the
 * one token of lookahead.
 *
 * So the solution is to avoid committing to anything upon seeing an AND by
 * having one nonterminal cover both the remainder of the current segment and
 * all the following segments.  That way, AND can be followed by *either* the
 * rest of the segment *or* the beginning of some more segments.
 *
 * (I've glossed over the optional "with" clause in each segment, which just
 * makes everything painful.)
 *)
segment_and_more_segments:
  | binding = code_or_closure_binding; AND; more = segment_and_more_segments
    { let bindings, elts, segs = more in binding :: bindings, elts, segs }
  | elts = with_closure_elements_opt; SEGMENT; more = segment_and_more_segments
    { let more_bindings, more_elts, segs = more in
      (* Careful! [elts] goes with the *previous* segment, not the one we're
       * building now. *)
      let seg = make_segment more_bindings more_elts in
      [], elts, seg :: segs }
  | elts = with_closure_elements_opt; { [], elts, [] }
;

code_or_closure_binding:
  | code_binding = code_binding { `Code code_binding }
  | closure_binding = static_closure_binding { `Closure closure_binding }
;

code_binding:
  | CODE;
    recursive = recursive;
    id = code_id;
    closure_id = option(AT; cid = closure_id { cid });
    newer_version_of = option(NEWER_VERSION_OF; id = code_id { id });
    params = kinded_args;
    closure_var = variable;
    vars_within_closures = kinded_vars_within_closures;
    MINUSGREATER; ret_cont = continuation;
    exn_cont = option(exn_continuation);
    ret_arity = return_arity;
    EQUAL; expr = expr;
    { { id; closure_id; newer_version_of; params; closure_var;
        vars_within_closures; ret_cont; ret_arity; exn_cont; recursive; expr; }
       : code_binding }
;

static_closure_binding:
  | SYMBOL; symbol = symbol; EQUAL; code_id = closure; { { symbol; code_id } }
;

recursive:
  | { Nonrecursive }
  | REC { Recursive }
;

tag_to_size:
  | tag = INT COLON size = INT {
  let (tag, _) = tag in
  let (size, _) = size in
  int_of_string tag, int_of_string size
}

tags_to_sizes:
  | { [] }
  | tag_to_size = tag_to_size { [ tag_to_size ] }
  | tag_to_size = tag_to_size COMMA tags_to_sizes = tags_to_sizes { tag_to_size :: tags_to_sizes }

switch_sort:
  | TAG LBRACKET tags_to_sizes = tags_to_sizes RBRACKET { Tag { tags_to_sizes } }
  | { Int }
  | IS_INT { Is_int }
;

unop:
  | OPAQUE { Opaque_identity }
  | PROJECT_VAR; var = var_within_closure; { Project_var var }

infix_binop:
  | PLUS { Plus }
  | PLUSDOT { Plusdot }
  | MINUS { Minus }
  | MINUSDOT { Minusdot }
;

binop:
  | a = simple DOT LPAREN f = simple RPAREN
    { Binop (Block_load (Block Value, Immutable), a, f) }

named:
  | s = simple { Simple s }
  | u = unop a = simple { Prim (Unop (u, a)) }
  | a1 = simple b = infix_binop a2 = simple { Prim (Infix_binop (b, a1, a2)) }
  | b = binop { Prim b }
  | BLOCK t = tag LPAREN elts = simple* RPAREN { Prim (Block (t, Immutable, elts)) }
  | c = closure { Closure { code_id = c } }
;

switch_case:
  | i = tag MINUSGREATER c = continuation { i,c }
;

switch:
  | option(SEMICOLON) { [] }
  | c = switch_case { [c] }
  | h = switch_case SEMICOLON t = switch { h :: t }
;
kind:
  | { None }
;
kinds:
  | { [] }
  | k = kind COMMA t = kinds { k :: t }
;
return_arity:
  | { None }
  | COLON k = kinds { Some k }
;

expr:
  | HCF { Invalid Halt_and_catch_fire }
  | UNREACHABLE { Invalid Treat_as_unreachable }
  | CONT c = continuation s = simple_args { Apply_cont (c, None, s) }
  | SWITCH sort = switch_sort scrutinee = simple LBRACE cases = switch RBRACE
    { Switch {scrutinee; sort; cases} }
  | LET l = let_ { Let l }
  | defining_expr = named SEMICOLON body = expr
      { Let { bindings = [ { var = None; kind = None; defining_expr; } ];
              closure_elements = None; 
              body } }
  | LETK recursive = recursive handler = continuation_handler t = andk body = expr
     { let handlers = handler :: t in
       Let_cont { recursive; body; handlers } }
  | ls = let_symbol; { Let_symbol ls }
  | CCALL LBRACKET func = csymbol RBRACKET args = simple_args ra = return_arity
    MINUSGREATER r = continuation e = exn_continuation
     { Apply {
          func = Symbol func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind = C_call {
              alloc = true; (* TODO noalloc *)
              (* param_arity =; *)
              return_arity = ra;
            };
       }}
  | APPLY func = name args = simple_args MINUSGREATER
    r = continuation e = exn_continuation
     { Apply {
          func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind = Function Indirect_unknown_arity;
       }}
;

let_:
  | bindings = separated_nonempty_list(AND, let_binding);
    closure_elements = with_closure_elements_opt;
    IN body = expr;
    { { bindings; closure_elements; body } }
;

let_binding:
  | v = kinded_variable_opt EQUAL defining_expr = named
      { let (var, kind) = v in { var; kind; defining_expr } }
;

with_closure_elements_opt:
  | { None }
  | WITH LBRACE;
    elements = separated_list(SEMICOLON, closure_element);
    RBRACE;
    { Some elements }
;

closure_element:
  | var = var_within_closure; EQUAL; value = simple; { { var; value; } }
;

closure:
  | CLOSURE; code_id = code_id; { code_id }
;

exn_and_stub:
  | { false, false }
  | STUB { false, true }
  | EXN { true, false }
  | STUB EXN { true, true }
  | EXN STUB { true, true }
;

continuation_handler:
  | exn_and_stub = exn_and_stub name = continuation
    params = kinded_args LBRACE handler = expr RBRACE
    { let is_exn_handler, stub = exn_and_stub in
      { name; params; stub; is_exn_handler; handler } }
;

andk:
  | AND h = continuation_handler t = andk { h :: t }
  | { [] }

kinded_args:
  | LPAREN v = kinded_variable* RPAREN { v }
  | { [] }

kinded_vars_within_closures:
  | LANGLE v = kinded_var_within_closure* RANGLE { v }
  | { [] }

static_structure:
  | s = symbol EQUAL BLOCK t = tag LPAREN elts = of_kind_value* RPAREN
    { ( s, None, Block (t, Immutable, elts) ) }
;

tag:
  tag = INT { make_tag ~loc:(make_loc ($startpos, $endpos)) tag }
;

of_kind_value:
  | s = symbol { Symbol s }
  | v = variable { Dynamically_computed v }
  | i = INT { Tagged_immediate ( make_tagged_immediate ~loc:($startpos, $endpos) i ) }
;

kinded_variable:
  | param = variable { { param; kind = None } }
;

kinded_variable_opt:
  | v = variable_opt { v, None }
;

kinded_var_within_closure:
  | var = var_within_closure { { var; kind = None } }
;

simple_args:
  | { [] }
  | LPAREN s = simple* RPAREN { s }
;

const:
  | c = INT { make_const_int c }
  | c = FLOAT { make_const_float c }
;

name:
  | s = symbol { (Symbol s:name) }
  | v = variable { (Var v:name) }
;

simple:
  | s = symbol { Symbol s }
  | v = variable { Var v }
  | c = const { Const c }
;

code_id:
  | v = variable { v }
;
 
closure_id:
  | v = variable { v }
;

symbol:
  | e = UIDENT { e, make_loc ($startpos, $endpos) }
;

csymbol:
  | s = LIDENT { s, make_loc ($startpos, $endpos) }
;

variable:
  | e = LIDENT { e, make_loc ($startpos, $endpos) }
;

variable_opt:
  | UNDERSCORE { None }
  | e = LIDENT { Some (e, make_loc ($startpos, $endpos)) }
;

continuation:
  | e = LIDENT { e, make_loc ($startpos, $endpos) }
;

var_within_closure:
  | e = LIDENT { e, make_loc ($startpos, $endpos) }
;
%%
