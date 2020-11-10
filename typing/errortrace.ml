open Types

type position = First | Second
let swap_position = function
  | First -> Second
  | Second -> First

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

module type Trace = sig
  type variant_info
  type obj_info
  type elt_info

  type variant =
    | Incompatible_types_for of string
    | Info of variant_info

  type obj =
    | Missing_field of position * string
    | Abstract_row of position
    | Info of obj_info

  type 'a elt =
    | Diff of 'a diff
    | Variant of variant
    | Obj of obj
    | Escape of 'a escape
    | Incompatible_fields of { name:string; diff: type_expr diff }
    | Note of string
    | Info of elt_info

  type t = desc elt list

  val diff : type_expr -> type_expr -> desc elt

  val debug_note : __LOC__:string -> string -> 'a elt

  (** [flatten f trace] flattens all elements of type {!desc} in
      [trace] to either [f x.t expanded] if [x.expanded=Some expanded]
      or [f x.t x.t] otherwise *)
  val flatten: (type_expr -> type_expr -> 'a) -> t -> 'a elt list

  (* CR aspectorzabusky: Could be polymorphic (['a -> 'b]) *)
  val map : (desc -> desc) -> desc elt list -> desc elt list

  val incompatible_fields : string -> type_expr -> type_expr -> desc elt
end

module type TraceParams = sig
  type variant_info
  type obj_info
  type elt_info
end

module Make_Trace (Params : TraceParams)
  : Trace with type variant_info := Params.variant_info
           and type obj_info := Params.obj_info
           and type elt_info := Params.elt_info =
struct
  type variant =
    | Incompatible_types_for of string
    | Info of Params.variant_info

  type obj =
    | Missing_field of position * string
    | Abstract_row of position
    | Info of Params.obj_info

  type 'a elt =
    | Diff of 'a diff
    | Variant of variant
    | Obj of obj
    | Escape of 'a escape
    | Incompatible_fields of { name:string; diff: type_expr diff }
    | Note of string
    | Info of Params.elt_info
  type t = desc elt list

  let diff got expected = Diff (map_diff short { got; expected })

  let debug_note ~__LOC__:loc msg = Note (Format.sprintf "[DEBUG] %s: %s" loc msg)

  let map_elt f = function
    | Diff x -> Diff (map_diff f x)
    | Escape { kind = Equation x; context} -> Escape { kind = Equation (f x); context }
    | Escape { kind = (Univ _ | Self | Constructor _ | Module_type _ | Constraint); _ }
    | Variant _ | Obj _
    | Incompatible_fields _
    | Note _
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

  type variant_info =
    | No_intersection
    | No_tags of position * (Asttypes.label * row_field) list
    | Fixed_row of position * fixed_row_case * fixed_explanation

  type self_cannot_be_closed = Self_cannot_be_closed

  type nonrec rec_occur = rec_occur = Rec_occur of type_expr * type_expr

  include Make_Trace(struct
      type nonrec variant_info = variant_info
      type obj_info = self_cannot_be_closed
      type elt_info = rec_occur
    end)

  (* Permute the expected and actual values *)
  let swap_elt = function
    | Diff x -> Diff (swap_diff x)
    | Incompatible_fields { name; diff } ->
      Incompatible_fields { name; diff = swap_diff diff}
    | Obj (Missing_field(pos,s)) -> Obj (Missing_field(swap_position pos,s))
    | Obj (Abstract_row pos) -> Obj (Abstract_row (swap_position pos))
    | Variant (Info (Fixed_row(pos,k,f))) ->
      Variant (Info (Fixed_row(swap_position pos,k,f)))
    | Variant (Info (No_tags(pos,f))) -> Variant (Info (No_tags(swap_position pos,f)))
    | x -> x

  let swap e = List.map swap_elt e
end

module Equality = struct
  type variant_info =
    | Openness of position
    | Missing of position * Asttypes.label

  include Make_Trace(struct
      type nonrec variant_info = variant_info
      type obj_info = no_info
      type elt_info = no_info
    end)
end

module Moregen = struct
  type variant_info =
    | Openness
    | Missing of position * Asttypes.label

  type nonrec rec_occur = rec_occur = Rec_occur of type_expr * type_expr

  include Make_Trace(struct
      type nonrec variant_info = variant_info
      type obj_info = no_info
      type elt_info = rec_occur
    end)
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
