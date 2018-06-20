(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Closure_stack = Flambda.Closure_stack

let log
  : (Closure_stack.t * Inlining_stats_types.Decision.t) list ref
  = ref []

let record_decision decision ~closure_stack =
  if !Clflags.inlining_report then begin
    match closure_stack with
    | []
    | Closure_stack.Closure _ :: _
    | Closure_stack.Inlined :: _
    | Closure_stack.Specialised _ :: _ ->
      Misc.fatal_errorf "record_decision: missing Call node"
    | Closure_stack.Call _ :: _ ->
      log := (closure_stack, decision) :: !log
  end

module Inlining_report = struct

  module Place = struct
    type kind =
      | Closure
      | Call

    type t = Debuginfo.t * Closure_id.t * kind

    let compare ((d1, cl1, k1) : t) ((d2, cl2, k2) : t) =
      let c = Debuginfo.compare d1 d2 in
      if c <> 0 then c else
      let c = Closure_id.compare cl1 cl2 in
      if c <> 0 then c else
        match k1, k2 with
        | Closure, Closure -> 0
        | Call, Call -> 0
        | Closure, Call -> 1
        | Call, Closure -> -1
  end

  module Place_map = Map.Make(Place)

  type decision =
    | Decision of Inlining_stats_types.Decision.t
    | Reference of Closure_stack.t

  type t = node Place_map.t

  and node =
    | Closure of t
    | Call of call

  and call =
    { decision: decision;
      inlined: t option;
      specialised: t option; }

  let empty_call path =
    let path = match path with
      | None -> []
      | Some p -> p
    in
    { decision = Reference path;
      inlined = None;
      specialised = None; }

  (* Prevented or unchanged decisions may be overridden by a later look at the
     same call. Other decisions may also be "overridden" because calls are not
     uniquely identified. *)
  let add_call_decision call (decision : Inlining_stats_types.Decision.t) =
    match call.decision, decision with
    | Reference _, _ -> { call with decision = Decision decision }
    | Decision _, Prevented _ -> call
    | Decision (Prevented _), _ -> { call with decision = Decision decision }
    | Decision (Specialised _), _ -> call
    | Decision _, Specialised _ -> { call with decision = Decision decision }
    | Decision (Inlined _), _ -> call
    | Decision _, Inlined _ -> { call with decision = Decision decision }
    | Decision Unchanged _, Unchanged _ -> call

  let add_decision t (stack, decision) =
    let rec loop t : Closure_stack.t -> _ = function
      | Closure(cl, dbg) :: rest ->
          let key : Place.t = (dbg, cl, Closure) in
          let v =
            try
              match Place_map.find key t with
              | Closure v -> v
              | Call _ -> assert false
            with Not_found -> Place_map.empty
          in
          let v = loop v rest in
          Place_map.add key (Closure v) t
      | Call(cl, dbg, path) :: rest ->
          let key : Place.t = (dbg, cl, Call) in
          let v =
            try
              match Place_map.find key t with
              | Call v -> v
              | Closure _ -> assert false
            with Not_found -> empty_call path
          in
          let v =
            match rest with
            | [] -> add_call_decision v decision
            | Inlined :: rest ->
                let inlined =
                  match v.inlined with
                  | None -> Place_map.empty
                  | Some inlined -> inlined
                in
                let inlined = loop inlined rest in
                { v with inlined = Some inlined }
            | Specialised _ :: rest ->
                let specialised =
                  match v.specialised with
                  | None -> Place_map.empty
                  | Some specialised -> specialised
                in
                let specialised = loop specialised rest in
                { v with specialised = Some specialised }
            | Call _ :: _ -> assert false
            | Closure _ :: _ -> assert false
          in
          Place_map.add key (Call v) t
      | [] -> assert false
      | Inlined :: _ -> assert false
      | Specialised _ :: _ -> assert false
    in
    loop t (List.rev stack)

  let build log =
    List.fold_left add_decision Place_map.empty log

  let print_stars ppf n =
    let s = String.make n '*' in
    Format.fprintf ppf "%s" s

  let print_reference ppf reference =
    Format.fprintf ppf "The decision for this site was taken at:@;  %a"
      Closure_stack.print reference

  let rec print ~depth ppf t =
    Place_map.iter (fun (dbg, cl, _) v ->
       match v with
       | Closure t ->
         Format.fprintf ppf "@[<h>%a Definition of %a%s@]@."
           print_stars (depth + 1)
           Closure_id.print cl
           (Debuginfo.to_string dbg);
         print ppf ~depth:(depth + 1) t;
         if depth = 0 then Format.pp_print_newline ppf ()
       | Call c ->
         match c.decision with
         | Reference reference ->
           begin
           Format.pp_open_vbox ppf (depth + 2);
           Format.fprintf ppf "@[<h>%a Application of %a%s@]@;@;@[%a@]"
             print_stars (depth + 1)
             Closure_id.print cl
             (Debuginfo.to_string dbg)
             print_reference reference;
           Format.pp_close_box ppf ();
           Format.pp_print_newline ppf ();
           Format.pp_print_newline ppf ();
           begin
             match c.specialised with
             | None -> ()
             | Some specialised ->
               print ppf ~depth:(depth + 1) specialised
           end;
           begin
             match c.inlined with
             | None -> ()
             | Some inlined ->
               print ppf ~depth:(depth + 1) inlined
           end;
             end
         | Decision decision ->
           Format.pp_open_vbox ppf (depth + 2);
           Format.fprintf ppf "@[<h>%a Application of %a%s@]@;@;@[%a@]"
             print_stars (depth + 1)
             Closure_id.print cl
             (Debuginfo.to_string dbg)
             Inlining_stats_types.Decision.summary decision;
           Format.pp_close_box ppf ();
           Format.pp_print_newline ppf ();
           Format.pp_print_newline ppf ();
           Inlining_stats_types.Decision.calculation ~depth:(depth + 1)
             ppf decision;
           begin
             match c.specialised with
             | None -> ()
             | Some specialised ->
               print ppf ~depth:(depth + 1) specialised
           end;
           begin
             match c.inlined with
             | None -> ()
             | Some inlined ->
               print ppf ~depth:(depth + 1) inlined
           end;
           if depth = 0 then Format.pp_print_newline ppf ())
      t

  let print ppf t = print ~depth:0 ppf t

end

let really_save_then_forget_decisions ~output_prefix =
  let report = Inlining_report.build !log in
  let out_channel = open_out (output_prefix ^ ".inlining.org") in
  let ppf = Format.formatter_of_out_channel out_channel in
  Inlining_report.print ppf report;
  close_out out_channel

let save_then_forget_decisions ~output_prefix =
  if !Clflags.inlining_report then begin
    really_save_then_forget_decisions ~output_prefix
  end
