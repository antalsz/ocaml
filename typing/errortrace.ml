open Types
open Format

type position = First | Second

let swap_position = function
  | First -> Second
  | Second -> First

let print_pos ppf = function
  | First -> fprintf ppf "first"
  | Second -> fprintf ppf "second"

type desc = { t: type_expr; expanded: type_expr option }
type 'a diff = { got: 'a; expected: 'a}

let short t = { t; expanded = None }
let map_diff f r =
  (* ordering is often meaningful when dealing with type_expr *)
  let got = f r.got in
  let expected = f r.expected in
  { got; expected}

let flatten_desc f x = match x.expanded with
  | None -> f x.t x.t
  | Some expanded -> f x.t expanded

let swap_diff x = { got = x.expected; expected = x.got }

type 'a escape_kind =
  | Constructor of Path.t
  | Univ of type_expr
  (* The type_expr argument of [Univ] is always a [Tunivar _],
     we keep a [type_expr] to track renaming in {!Printtyp} *)
  | Self
  | Module_type of Path.t
  | Equation of 'a
  | Constraint

type 'a escape =
  { kind : 'a escape_kind;
    context : type_expr option }

let explain trace f =
  let rec explain = function
    | [] -> None
    | [h] -> f ~prev:None h
    | h :: (prev :: _ as rem) ->
      match f ~prev:(Some prev) h with
      | Some _ as m -> m
      | None -> explain rem in
  explain (List.rev trace)

type no_info = |

type rec_occur = Rec_occur of type_expr * type_expr

type printing_status =
  | Discard
  | Keep
  | Optional_refinement
  (** An [Optional_refinement] printing status is attributed to trace
      elements that are focusing on a new subpart of a structural type.
      Since the whole type should have been printed earlier in the trace,
      we only print those elements if they are the last printed element
      of a trace, and there is no explicit explanation for the
      type error.
  *)

type type_printers = {
  mark_loops : type_expr -> unit;
  type_expr  : Format.formatter -> type_expr -> unit;
  path       : Format.formatter -> Path.t -> unit
}

let explain_no_info _ = function | (_ : no_info) -> .

let explain_rec_occur type_printers = function
  | Rec_occur(x,y) ->
    type_printers.mark_loops y;
    Some(dprintf "@,@[<hov>The type variable %a occurs inside@ %a@]"
           type_printers.type_expr x type_printers.type_expr y)

module type Trace_info_types = sig
  type variant_info
  type obj_info
  type elt_info
end

module type Trace_core = sig
  (* Distinct for every trace *)
  include Trace_info_types

  (* Uniform for every trace *)

  type variant =
    | Incompatible_types_for of string
    | Vinfo of variant_info

  type obj =
    | Missing_field of position * string
    | Abstract_row of position
    | Oinfo of obj_info

  type 'a elt =
    | Diff of 'a diff
    | Variant of variant
    | Obj of obj
    | Escape of 'a escape
    | Incompatible_fields of { name:string; diff: type_expr diff }
    | Info of elt_info

  type t = desc elt list

  val diff : type_expr -> type_expr -> desc elt

  val flatten: (type_expr -> type_expr -> 'a) -> t -> 'a elt list

  val map : (desc -> desc) -> desc elt list -> desc elt list

  val incompatible_fields : string -> type_expr -> type_expr -> desc elt
end

module type Trace = sig
  include Trace_core

  (* Also distinct for every type *)

  val incompatibility_phrase : string

  val constraint_escape_status : printing_status
  val drop_from_trace : (Types.type_expr * 'a) elt -> bool
  val explain_contextless_escaped_field_mismatch : bool

  val explain_variant_info :
    type_printers -> variant_info -> (Format.formatter -> unit) option
  val explain_obj_info :
    type_printers -> obj_info -> (Format.formatter -> unit) option
  val explain_elt_info :
    type_printers -> elt_info -> (Format.formatter -> unit) option
end

module Make_trace_core (Info : Trace_info_types)
  : Trace_core with type variant_info := Info.variant_info
                and type obj_info := Info.obj_info
                and type elt_info := Info.elt_info
= struct
  type variant =
    | Incompatible_types_for of string
    | Vinfo of Info.variant_info

  type obj =
    | Missing_field of position * string
    | Abstract_row of position
    | Oinfo of Info.obj_info

  type 'a elt =
    | Diff of 'a diff
    | Variant of variant
    | Obj of obj
    | Escape of 'a escape
    | Incompatible_fields of { name:string; diff: type_expr diff }
    | Info of Info.elt_info
  type t = desc elt list

  let diff got expected = Diff (map_diff short { got; expected })

  let map_elt f = function
    | Diff x -> Diff (map_diff f x)
    | Escape { kind = Equation x; context} -> Escape { kind = Equation (f x); context }
    | Escape { kind = (Univ _ | Self | Constructor _ | Module_type _ | Constraint); _ }
    | Variant _ | Obj _
    | Incompatible_fields _
    | Info _ as x -> x

  let map f t = List.map (map_elt f) t

  (* Convert desc to type_expr * type_expr *)
  let flatten f = map (flatten_desc f)

  let incompatible_fields name got expected =
    Incompatible_fields { name; diff={got; expected} }
end

module Unification = struct
  type fixed_row_case =
    | Cannot_be_closed
    | Cannot_add_tags of string list

  module Info = struct
    type variant_info =
      | No_intersection
      | No_tags of position * (Asttypes.label * row_field) list
      | Fixed_row of position * fixed_row_case * fixed_explanation

    type obj_info = Self_cannot_be_closed

    type elt_info = rec_occur
  end

  include Info
  include Make_trace_core (Info)

  (* Extra non-printing functionality *)

  (* Permute the expected and actual values *)
  let swap_elt = function
    | Diff x -> Diff (swap_diff x)
    | Incompatible_fields { name; diff } ->
      Incompatible_fields { name; diff = swap_diff diff}
    | Obj (Missing_field(pos,s)) -> Obj (Missing_field(swap_position pos,s))
    | Obj (Abstract_row pos) -> Obj (Abstract_row (swap_position pos))
    | Variant (Vinfo (Fixed_row(pos,k,f))) ->
      Variant (Vinfo (Fixed_row(swap_position pos,k,f)))
    | Variant (Vinfo (No_tags(pos,f))) ->
      Variant (Vinfo (No_tags(swap_position pos,f)))
    | x -> x

  let swap e = List.map swap_elt e

  (* Printing configuration *)

  let incompatibility_phrase = "is not compatible with type"

  let constraint_escape_status = Discard
  let drop_from_trace _ = false
  let explain_contextless_escaped_field_mismatch = true

  let print_tag ppf = fprintf ppf "`%s"

  let print_tags =
    let comma ppf () = fprintf ppf ",@ " in
    pp_print_list ~pp_sep:comma print_tag

  let explain_fixed_row type_printers pos expl = match expl with
    | Types.Fixed_private ->
      dprintf "The %a variant type is private" print_pos pos
    | Types.Univar x ->
      dprintf "The %a variant type is bound to the universal type variable %a"
        print_pos pos type_printers.type_expr x
    | Types.Reified p ->
      dprintf "The %a variant type is bound to %a" print_pos pos type_printers.path p
    | Types.Rigid -> ignore

  let explain_fixed_row_case ppf = function
    | Cannot_be_closed -> fprintf ppf "it cannot be closed"
    | Cannot_add_tags tags -> fprintf ppf "it may not allow the tag(s) %a" print_tags tags

  let explain_variant_info type_printers = function
    | No_intersection ->
      Some(dprintf "@,These two variant types have no intersection")
    | No_tags(pos,fields) -> Some(
      dprintf
        "@,@[The %a variant type does not allow tag(s)@ @[<hov>%a@]@]"
        print_pos pos
        print_tags (List.map fst fields)
    )
    | Fixed_row (pos, k, (Univar _ | Reified _ | Fixed_private as e)) ->
      Some (
        dprintf "@,@[%t,@ %a@]" (explain_fixed_row type_printers pos e)
          explain_fixed_row_case k
      )
    | Fixed_row (_,_, Rigid) ->
      (* this case never happens *)
      None

  let explain_obj_info _type_printers = function
    | Self_cannot_be_closed ->
      Some (dprintf "@,Self type cannot be unified with a closed object type")

  let explain_elt_info = explain_rec_occur
end

module Equality = struct
  module Info = struct
    type variant_info =
      | Openness of position
      | Missing of position * Asttypes.label

    type obj_info = no_info

    type elt_info = no_info
  end

  include Info
  include Make_trace_core (Info)

  let incompatibility_phrase = "is not equal to type"

  let constraint_escape_status = Discard
  let drop_from_trace = function
    | Diff {got = ({desc = Tpoly _}, _); expected = ({desc = Tpoly _}, _)} -> true
    | _ -> false
  let explain_contextless_escaped_field_mismatch = true

  let explain_variant_info _type_printers = function
    | Openness ord ->
        Some(dprintf "@,The %a is open and the %a is not"
               print_pos ord
               print_pos (swap_position ord))
    | Missing (ord, l) ->
        Some(dprintf "@,The %a declaration has no tag `%s" print_pos ord l)

  let explain_obj_info = explain_no_info

  let explain_elt_info = explain_no_info
end

module Moregen = struct
  module Info = struct
    type variant_info =
      | Openness
      | Missing of position * Asttypes.label

    type obj_info = no_info

    type elt_info = rec_occur
  end

  include Info
  include Make_trace_core (Info)

  let incompatibility_phrase = "is not compatible with type"

  let constraint_escape_status = Keep
  let drop_from_trace _ = false
  let explain_contextless_escaped_field_mismatch = false

  let explain_variant_info _type_printers = function
    | Missing (pos, f) ->
        Some(dprintf "@,@[The %a object type has no method %s@]" print_pos pos f)
    | Openness ->
        Some (dprintf "@,@[The second object is open and the first is not@]")

  let explain_obj_info = explain_no_info

  let explain_elt_info = explain_rec_occur
end

module Subtype = struct
  type 'a elt =
    | Diff of 'a diff

  type t = desc elt list

  let diff got expected = Diff (map_diff short {got;expected})

  let map_elt f = function
    | Diff x -> Diff (map_diff f x)

  let map f t = List.map (map_elt f) t

  let flatten f t = map (flatten_desc f) t
end

