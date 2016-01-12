(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

module A = Simple_value_approx
module E = Inline_and_simplify_aux.Env
module R = Inline_and_simplify_aux.Result
module U = Flambda_utils
module W = Inlining_cost.Whether_sufficient_benefit
module T = Inlining_cost.Threshold
module S = Inlining_stats_types
module D = S.Decision

type ('a, 'b) inlining_result =
  | Changed of (Flambda.t * R.t) * 'a
  | Original of 'b

type 'b good_idea =
  | Try_it
  | Dont_try_it of 'b

let inline env r ~lhs_of_application
    ~(function_decls : Flambda.function_declarations)
    ~closure_id_being_applied ~(function_decl : Flambda.function_declaration)
    ~value_set_of_closures ~only_use_of_function ~original ~recursive
    ~(args : Variable.t list) ~size_from_approximation ~simplify
    ~always_inline ~(inline_requested : Lambda.inline_attribute) =
  let toplevel = E.at_toplevel env in
  let branch_depth = E.branch_depth env in
  let try_inlining =
    if only_use_of_function || always_inline then
      Try_it
    else if not (E.unrolling_allowed env function_decls.set_of_closures_origin)
         && (Lazy.force recursive) then
      Dont_try_it S.Not_inlined.Unrolling_depth_exceeded
    else if not (toplevel && branch_depth = 0)
         && A.all_not_useful (E.find_list_exn env args) then
      (* When all of the arguments to the function being inlined are unknown, then
         we cannot materially simplify the function.  As such, we know what the
         benefit of inlining it would be: just removing the call.  In this case
         we may be able to prove the function cannot be inlined without traversing
         its body.
         Note that if the function is sufficiently small, we still have to call
         [simplify], because the body needs freshening before substitution.
      *)
      (* CR-someday mshinwell: (from GPR#8): pchambart writes:

          We may need to think a bit about that. I can't see a lot of meaningful
          examples right now, but there are some cases where some optimisation can
          happen even if we don't know anything about the shape of the arguments.

          For instance

          let f x y = x

          let g x =
            let y = (x,x) in
            f x y
          let f x y =
            if x = y then ... else ...

          let g x = f x x
      *)
      match size_from_approximation with
      | Some body_size ->
        let wsb =
          let benefit = Inlining_cost.Benefit.zero in
          let benefit = Inlining_cost.Benefit.remove_call benefit in
          let benefit =
            Variable.Set.fold (fun v acc ->
                try
                  let t =
                    Var_within_closure.Map.find (Var_within_closure.wrap v)
                      value_set_of_closures.A.bound_vars
                  in
                  match t.A.var with
                  | Some v ->
                    if (E.mem env v) then Inlining_cost.Benefit.remove_prim acc
                    else acc
                  | None -> acc
                with Not_found -> acc)
              function_decl.free_variables benefit
          in
          W.create_estimate
            ~original_size:Inlining_cost.direct_call_size
            ~new_size:body_size
            ~toplevel:(E.at_toplevel env)
            ~branch_depth:(E.branch_depth env)
            ~lifting:function_decl.Flambda.is_a_functor
            ~round:(E.round env)
            ~benefit
        in
        if (not (W.evaluate wsb)) then begin
          Dont_try_it
            (S.Not_inlined.Without_subfunctions wsb)
        end else Try_it
      | None ->
        (* The function is definitely too large to inline given that we don't
           have any approximations for its arguments.  Further, the body
           should already have been simplified (inside its declaration), so
           we also expect no gain from the code below that permits inlining
           inside the body. *)
        Dont_try_it S.Not_inlined.Unspecialised
    else begin
      (* There are useful approximations, so we should simplify. *)
      Try_it
    end
  in
  match try_inlining with
  | Dont_try_it decision -> Original decision
  | Try_it -> begin
    let body, r_inlined =
      (* First we construct the code that would result from copying the body of
         the function, without doing any further inlining upon it, to the call
         site. *)
      Inlining_transforms.inline_by_copying_function_body ~env
        ~r:(R.reset_benefit r) ~function_decls ~lhs_of_application
        ~closure_id_being_applied ~inline_requested ~function_decl
        ~args ~simplify
    in
    let num_direct_applications_seen =
      (R.num_direct_applications r_inlined) - (R.num_direct_applications r)
    in
    assert (num_direct_applications_seen >= 0);
    let keep_inlined_version decision =
      (* Inlining the body of the function was sufficiently beneficial that we
         will keep it, replacing the call site.  We continue by allowing
         further inlining within the inlined copy of the body. *)
      let r_inlined =
        (* The meaning of requesting inlining is that the user ensure
           that the function has a benefit of at least its size. It is not
           added to the benefit exposed by the inlining because the user should
           have taken that into account before annotating the function. *)
        if always_inline then
          R.map_benefit r_inlined
            (Inlining_cost.Benefit.max ~round:(E.round env)
               Inlining_cost.Benefit.(requested_inline ~size_of:body zero))
        else r_inlined
      in
      let r =
        R.map_benefit r_inlined (Inlining_cost.Benefit.(+) (R.benefit r))
      in
      let env = E.note_entering_inlined env in
      let env =
        (* We decrement the unrolling count even if the function is not
           recursive to avoid having to check whether or not it is recursive *)
        E.inside_unrolled_function env function_decls.set_of_closures_origin
      in
      let env =
        if E.inlining_level env = 0
           (* If the function was considered for inlining without considering
              its sub-functions, and it is not below another inlining choice,
              then we are certain that this code will be kept. *)
        then env
        else E.inlining_level_up env
      in
      Changed((simplify env r body), decision)
    in
    if always_inline then
      keep_inlined_version S.Inlined.Unconditionally
    else if only_use_of_function then
      keep_inlined_version S.Inlined.Decl_local_to_application
    else begin
      let wsb =
        W.create ~original body
          ~toplevel:(E.at_toplevel env)
          ~branch_depth:(E.branch_depth env)
          ~lifting:function_decl.Flambda.is_a_functor
          ~round:(E.round env)
          ~benefit:(R.benefit r_inlined)
      in
      if W.evaluate wsb then
        keep_inlined_version
          (S.Inlined.Without_subfunctions wsb)
      else if num_direct_applications_seen < 1 then begin
      (* Inlining the body of the function did not appear sufficiently
         beneficial; however, it may become so if we inline within the body
         first.  We try that next, unless it is known that there are were
         no direct applications in the simplified body computed above, meaning
         no opportunities for inlining. *)
        Original (S.Not_inlined.Without_subfunctions wsb)
      end else begin
        let env = E.inlining_level_up env in
        let env = E.note_entering_inlined env in
        let env =
          (* We decrement the unrolling count even if the function is recursive
             to avoid having to check whether or not it is recursive *)
          E.inside_unrolled_function env function_decls.set_of_closures_origin
        in
        let body, r_inlined = simplify env r_inlined body in
        let wsb_with_subfunctions =
          W.create ~original body
            ~toplevel:(E.at_toplevel env)
            ~branch_depth:(E.branch_depth env)
            ~lifting:function_decl.Flambda.is_a_functor
            ~round:(E.round env)
            ~benefit:(R.benefit r_inlined)
        in
        if W.evaluate wsb_with_subfunctions then begin
          let res =
            (body, R.map_benefit r_inlined
                     (Inlining_cost.Benefit.(+) (R.benefit r)))
          in
          let decision =
            S.Inlined.With_subfunctions(wsb, wsb_with_subfunctions)
          in
          Changed(res, decision)
        end
        else begin
          (* r_inlined contains an approximation that may be invalid for the
             untransformed expression: it may reference functions that only
             exists if the body of the function is in fact inlined.
             If the function approximation contained an approximation that
             does not depend on the actual values of its arguments, it
             could be returned instead of [A.value_unknown]. *)
          let decision =
            S.Not_inlined.With_subfunctions(wsb, wsb_with_subfunctions)
          in
          Original decision
        end
      end
    end
  end

let specialise env r ~lhs_of_application
      ~(function_decls : Flambda.function_declarations)
      ~(function_decl : Flambda.function_declaration)
      ~closure_id_being_applied
      ~(value_set_of_closures : Simple_value_approx.value_set_of_closures)
      ~args ~args_approxs ~dbg ~simplify ~original ~recursive
      ~(inline_requested : Lambda.inline_attribute) =
  let try_specialising =
    (* Try specialising if the function is:
       - recursive
       - closed
       - has useful approximations for some invariant parameters *)
    let invariant_params = value_set_of_closures.invariant_params in
    let bound_vars = value_set_of_closures.bound_vars in
    if !Clflags.classic_inlining then
      Dont_try_it S.Not_specialised.Classic_mode
    else if not (Var_within_closure.Map.is_empty bound_vars) then
      Dont_try_it S.Not_specialised.Not_closed
    else if not (Lazy.force recursive) then
      Dont_try_it S.Not_specialised.Not_recursive
    else if Variable.Map.is_empty (Lazy.force invariant_params) then
      Dont_try_it S.Not_specialised.No_invariant_parameters
    else if List.for_all2
              (fun id approx ->
                 not ((A.useful approx)
                      && Variable.Map.mem id (Lazy.force invariant_params)))
              function_decl.params args_approxs then
      Dont_try_it S.Not_specialised.No_useful_approximations
    else Try_it
  in
  match try_specialising with
  | Dont_try_it decision -> Original decision
  | Try_it -> begin
      let copied_function_declaration =
        Inlining_transforms.inline_by_copying_function_declaration ~env
          ~r:(R.reset_benefit r) ~lhs_of_application
          ~function_decls ~closure_id_being_applied ~function_decl
          ~args ~args_approxs
          ~invariant_params:value_set_of_closures.invariant_params
          ~specialised_args:value_set_of_closures.specialised_args ~dbg
          ~simplify ~inline_requested
      in
      match copied_function_declaration with
      | Some (expr, r_inlined) ->
        let wsb =
          W.create ~original expr
            ~toplevel:false
            ~branch_depth:(E.branch_depth env)
            ~lifting:false
            ~round:(E.round env)
            ~benefit:(R.benefit r_inlined)
        in
        let env =
          (* CR-someday lwhite: could avoid calculating this if stats is turned
             off *)
          let closure_ids =
            Closure_id.Set.of_list (
              List.map Closure_id.wrap
                (Variable.Set.elements (Variable.Map.keys function_decls.funs)))
          in
          E.note_entering_specialised env ~closure_ids
        in
        if W.evaluate wsb then begin
          let r =
            R.map_benefit r_inlined (Inlining_cost.Benefit.(+) (R.benefit r))
          in
          let closure_env =
            let env =
              if E.inlining_level env = 0
               (* If the function was considered for specialising without considering
                  its sub-functions, and it is not below another inlining choice,
                  then we are certain that this code will be kept. *)
              then env
              else E.inlining_level_up env
            in
              E.set_never_inline_outside_closures env
          in
          let application_env = E.set_never_inline_inside_closures env in
          let expr, r = simplify closure_env r expr in
          let res = simplify application_env r expr in
          let decision = S.Specialised.Without_subfunctions wsb in
          Changed(res, decision)
        end else begin
          let closure_env =
            let env = E.inlining_level_up env in
            E.set_never_inline_outside_closures env
          in
          let expr, r_inlined = simplify closure_env r_inlined expr in
          let wsb_with_subfunctions =
            W.create ~original expr
              ~toplevel:false
              ~branch_depth:(E.branch_depth env)
              ~lifting:false
              ~round:(E.round env)
              ~benefit:(R.benefit r_inlined)
          in
          if W.evaluate wsb_with_subfunctions then begin
             let r =
               R.map_benefit r_inlined
                        (Inlining_cost.Benefit.(+) (R.benefit r))
             in
             let application_env = E.set_never_inline_inside_closures env in
             let res = simplify application_env r expr in
             let decision =
               S.Specialised.With_subfunctions(wsb, wsb_with_subfunctions)
             in
             Changed(res, decision)
          end else begin
            let decision = S.Not_specialised.Not_beneficial(wsb, wsb_with_subfunctions) in
            Original decision
          end
        end
      | None ->
        let decision = S.Not_specialised.No_useful_approximations in
        Original decision
    end

let for_call_site ~env ~r ~(function_decls : Flambda.function_declarations)
      ~lhs_of_application ~closure_id_being_applied
      ~(function_decl : Flambda.function_declaration)
      ~(value_set_of_closures : Simple_value_approx.value_set_of_closures)
      ~args ~args_approxs ~dbg ~simplify ~inline_requested =
  if List.length args <> List.length args_approxs then begin
    Misc.fatal_error "Inlining_decision.for_call_site: inconsistent lengths \
        of [args] and [args_approxs]"
  end;
  let original =
    Flambda.Apply {
      func = lhs_of_application;
      args;
      kind = Direct closure_id_being_applied;
      dbg;
      inline = inline_requested;
    }
  in
  let original_r =
    R.set_approx (R.seen_direct_application r) (A.value_unknown Other)
  in
  if function_decl.stub then
    let body, r =
      Inlining_transforms.inline_by_copying_function_body ~env ~r
        ~function_decls ~lhs_of_application ~closure_id_being_applied
        ~inline_requested ~function_decl ~args ~simplify
    in
    simplify env r body
  else if E.never_inline env then
    (* This case only occurs when examining the body of a stub function
       but not in the context of inlining said function.  As such, there
       is nothing to do here (and no decision to report). *)
    original, original_r
  else begin
    let env = E.unset_never_inline_inside_closures env in
    let env =
      E.note_entering_call env
        ~closure_id:closure_id_being_applied ~debuginfo:dbg
    in
    let max_level =
      Clflags.Int_arg_helper.get ~key:(E.round env) !Clflags.max_inlining_depth
    in
    let inline_annotation =
      (* Merge call site annotation and function annotation.
         The call site annotation takes precedence *)
      match (inline_requested : Lambda.inline_attribute) with
      | Default_inline -> function_decl.inline
      | Always_inline | Never_inline -> inline_requested
    in
    let always_inline =
      match (inline_annotation : Lambda.inline_attribute) with
      | Always_inline -> true
      | Never_inline | Default_inline -> false
    in
    let num_params = List.length function_decl.params in
    let only_use_of_function = false in
    let raw_inlining_threshold = R.inlining_threshold r in
    let max_inlining_threshold =
      if E.at_toplevel env then
        Inline_and_simplify_aux.initial_inlining_toplevel_threshold
          ~round:(E.round env)
      else
        Inline_and_simplify_aux.initial_inlining_threshold ~round:(E.round env)
    in
    let unthrottled_inlining_threshold =
      match raw_inlining_threshold with
      | None -> max_inlining_threshold
      | Some inlining_threshold -> inlining_threshold
    in
    let inlining_threshold =
      T.min unthrottled_inlining_threshold max_inlining_threshold
    in
    let inlining_threshold_diff =
      T.sub unthrottled_inlining_threshold inlining_threshold
    in
    let fun_var =
      U.find_declaration_variable closure_id_being_applied function_decls
    in
    let recursive_functions =
      lazy
        (Find_recursive_functions.in_function_declarations function_decls
           ~backend:(E.backend env))
    in
    let recursive =
      lazy (Variable.Set.mem fun_var (Lazy.force recursive_functions))
    in
    let fun_cost : Inlining_cost.Threshold.t =
      match (inline_annotation : Lambda.inline_attribute) with
      | Never_inline -> Never_inline
      | Always_inline | Default_inline ->
        if always_inline
          || (only_use_of_function && not (Lazy.force recursive))
        then
          inlining_threshold
        else begin
          Inlining_cost.can_try_inlining function_decl.body inlining_threshold
            ~number_of_arguments:num_params
            (* CR mshinwell: for the moment, this is None, since the
               Inlining_cost code isn't checking sizes up to the max inlining
               threshold---this seems to take too long. *)
            ~size_from_approximation:None
        end
    in
    let simpl =
      if not always_inline
         && E.inside_set_of_closures_declaration
              function_decls.set_of_closures_origin env then
        Original (D.Prevented Self_call)
      else if fun_cost = T.Never_inline then
        (* CR pchambart: should we also accept unconditionnal inline ?  It is
           some kind of user defined stub, but if we restrict to stub we are
           certain that no abusive use of [@@inline] can blow things up *)
        let reason : Inlining_stats_types.Prevented.t =
          match inlining_threshold with
          | Never_inline ->
            Function_prevented_from_inlining
          | Can_inline_if_no_larger_than threshold ->
            Function_obviously_too_large threshold
        in
        Original (D.Prevented reason)
      else if !Clflags.classic_inlining && not always_inline then
        Original (D.Prevented Classic_heuristic)
      else if E.inlining_level env >= max_level then
        Original (D.Prevented Level_exceeded)
      else begin
        let remaining_inlining_threshold = fun_cost in
        let r =
          R.set_inlining_threshold r (Some remaining_inlining_threshold)
        in
        let specialise_result =
          specialise env r ~lhs_of_application ~function_decls ~recursive
            ~closure_id_being_applied ~function_decl ~value_set_of_closures
            ~args ~args_approxs ~dbg ~simplify ~original ~inline_requested
        in
        match specialise_result with
        | Changed(res, spec_reason) -> Changed(res, D.Specialised spec_reason)
        | Original spec_reason ->
          (* If we didn't specialise then try inlining *)
          let size_from_approximation =
            match
              Variable.Map.find fun_var (Lazy.force value_set_of_closures.size)
            with
            | size -> size
            | exception Not_found ->
                Misc.fatal_errorf "Approximation does not give a size for the \
                  function having fun_var %a.  value_set_of_closures: %a"
                  Variable.print fun_var
                  A.print_value_set_of_closures value_set_of_closures
          in
          let inline_result =
            inline env r ~function_decls ~lhs_of_application
              ~closure_id_being_applied ~function_decl ~value_set_of_closures
              ~only_use_of_function ~original ~recursive
              ~inline_requested ~always_inline ~args ~size_from_approximation
              ~simplify
          in
          match inline_result with
          | Changed(res, inl_reason) ->
              Changed(res, D.Inlined(spec_reason, inl_reason))
          | Original inl_reason ->
              Original (D.Unchanged(spec_reason, inl_reason))
      end
    in
    let res, decision =
      match simpl with
      | Original decision -> (original, original_r), decision
      | Changed((expr, r), decision) ->
        let res =
          if E.inlining_level env = 0
          then expr, R.set_inlining_threshold r raw_inlining_threshold
          else expr, R.add_inlining_threshold r inlining_threshold_diff
        in
        res, decision
    in
    E.record_decision env decision;
    res
  end


(* We do not inline inside stubs, which are always inlined at their call site.
   Inlining inside the declaration of a stub could result in more code than
   expected being inlined. *)
(* CR mshinwell for pchambart: maybe we need an example here *)
let should_inline_inside_declaration (decl : Flambda.function_declaration) =
  not decl.stub
