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

(* Operations on module types *)

open Asttypes
open Path
open Types

type aliasable =
  | Aliasable
  | Aliasable_with_constraints
  | Not_aliasable

let rec scrape env mty =
  match mty with
    Mty_ident p ->
      begin try
        scrape env (Env.find_modtype_expansion p env)
      with Not_found ->
        mty
      end
  | _ -> mty

let freshen mty =
  Subst.modtype Subst.identity mty

let rec strengthen_modtype  ~aliasable env mty p =
  match scrape env mty with
  | Mty_signature sg ->
      Mty_signature(strengthen_sig ~aliasable env sg p)
  | Mty_functor(param, arg, res)
    when !Clflags.applicative_functors && Ident.name param <> "*" ->
      Mty_functor(param, arg,
        strengthen_modtype ~aliasable:Not_aliasable
          env res (Papply(p, Pident param)))
  | mty ->
      mty

and strengthen_sig ~aliasable env sg p =
  match sg with
    [] -> []
  | (Sig_value _ as sigelt) :: rem ->
      sigelt :: strengthen_sig ~aliasable env rem p
  | Sig_type(id, {type_kind=Type_abstract}, _) ::
    (Sig_type(id', {type_private=Private}, _) :: _ as rem)
    when Ident.name id = Ident.name id' ^ "#row" ->
      strengthen_sig ~aliasable env rem p
  | Sig_type(id, decl, rs) :: rem ->
      let newdecl =
        match decl.type_manifest, decl.type_private, decl.type_kind with
          Some _, Public, _ -> decl
        | Some _, Private, (Type_record _ | Type_variant _) -> decl
        | _ ->
            let manif =
              Some(Btype.newgenty(Tconstr(Pdot(p, Ident.name id),
                                          decl.type_params, ref Mnil))) in
            if decl.type_kind = Type_abstract then
              { decl with type_private = Public; type_manifest = manif }
            else
              { decl with type_manifest = manif }
      in
      Sig_type(id, newdecl, rs) :: strengthen_sig ~aliasable env rem p
  | (Sig_typext _ as sigelt) :: rem ->
      sigelt :: strengthen_sig ~aliasable env rem p
  | Sig_module(id, pres, md, rs) :: rem ->
      let str =
        strengthen_module_declaration
          ~aliasable env md (Pdot(p, Ident.name id))
      in
      Sig_module(id, pres, str, rs)
      :: strengthen_sig ~aliasable
        (Env.add_module_declaration ~check:false id pres md env) rem p
      (* Need to add the module in case it defines manifest module types *)
  | Sig_modtype(id, decl) :: rem ->
      let newdecl =
        match decl.mtd_type with
          None ->
            {decl with mtd_type = Some(Mty_ident(Pdot(p,Ident.name id)))}
        | Some _ ->
            decl
      in
      Sig_modtype(id, newdecl) ::
      strengthen_sig ~aliasable (Env.add_modtype id decl env) rem p
      (* Need to add the module type in case it is manifest *)
  | (Sig_class _ as sigelt) :: rem ->
      sigelt :: strengthen_sig ~aliasable env rem p
  | (Sig_class_type _ as sigelt) :: rem ->
      sigelt :: strengthen_sig ~aliasable env rem p

and strengthen_module_declaration ~aliasable env md p =
  match md.md_type, aliasable with
  | Mty_alias _, _ -> md
  | mty, Aliasable_with_constraints ->
    let alias = Ma_tconstraint(Ma_path p, mty) in
    {md with md_type = Mty_alias alias}
  | _, Aliasable ->
    {md with md_type = Mty_alias(Ma_path p)}
  | mty, _ -> {md with md_type = strengthen_modtype ~aliasable env mty p}

let aliasable_of_module_alias env alias =
  let rec loop env = function
    | Ma_path path ->
        if Env.is_aliasable path env then Aliasable
        else Not_aliasable
    | Ma_dot(ma, _) -> loop env ma
    | Ma_tconstraint(ma, _) -> begin
        match loop env ma with
        | Aliasable | Aliasable_with_constraints ->
            Aliasable_with_constraints
        | Not_aliasable ->
            Not_aliasable
      end
  in
  loop env alias

let strengthen env md alias =
  let aliasable = aliasable_of_module_alias env alias in
  let path = path_of_module_alias alias in
  strengthen_modtype ~aliasable env md path

let strengthen_decl env md alias =
  let aliasable = aliasable_of_module_alias env alias in
  let path = path_of_module_alias alias in
  strengthen_module_declaration ~aliasable env md path

let () = Env.strengthen := strengthen

let rec make_aliases_absent pres mty =
  match mty with
  | Mty_alias _ -> Mta_absent, mty
  | Mty_signature sg ->
      pres, Mty_signature(make_aliases_absent_sig sg)
  | Mty_functor(param, arg, res) ->
      let _, res = make_aliases_absent Mta_present res in
      pres, Mty_functor(param, arg, res)
  | mty ->
      pres, mty

and make_aliases_absent_sig sg =
  match sg with
    [] -> []
  | Sig_module(id, pres, md, rs) :: rem ->
      let pres, md_type = make_aliases_absent pres md.md_type in
      let md = { md with md_type } in
      Sig_module(id, pres, md, rs) :: make_aliases_absent_sig rem
  | sigelt :: rem ->
      sigelt :: make_aliases_absent_sig rem

let scrape_for_type_of env pres mty =
  let rec loop env path mty =
    match mty, path with
    | Mty_alias alias, _ -> begin
        try
          let mty = Env.find_module_alias alias env in
          loop env (Some alias) mty
        with Not_found -> mty
      end
    | mty, Some alias ->
        strengthen env mty alias
    | _ -> mty
  in
  make_aliases_absent pres (loop env None mty)

(* In nondep_supertype, env is only used for the type it assigns to id.
   Hence there is no need to keep env up-to-date by adding the bindings
   traversed. *)

type variance = Co | Contra | Strict

let nondep_supertype env mid mty =

  let rec nondep_mty env va mty =
    match mty with
      Mty_ident p ->
        if Path.isfree mid p then
          nondep_mty env va (Env.find_modtype_expansion p env)
        else mty
    | Mty_alias alias -> begin
        match nondep_module_alias env alias with
        | alias -> Mty_alias alias
        | exception Not_found ->
            let mty = Env.find_module_alias alias env in
            let mty = nondep_mty env va (strengthen env mty alias) in
            let path = path_of_module_alias alias in
            let path =
              if Path.isfree mid path then
                Env.normalize_module_path None env path
              else path
            in
            if Path.isfree mid path then begin
              match va with
              | Co -> mty
              | _ -> raise Not_found
            end else begin
              Mty_alias (Ma_tconstraint(Ma_path path, mty))
            end
      end
    | Mty_signature sg ->
        Mty_signature(nondep_sig env va sg)
    | Mty_functor(param, arg, res) ->
        let var_inv =
          match va with Co -> Contra | Contra -> Co | Strict -> Strict in
        Mty_functor(param, Misc.may_map (nondep_mty env var_inv) arg,
                    nondep_mty
                      (Env.add_module ~arg:true param Mta_present
                         (Btype.default_mty arg) env) va res)

  and nondep_sig env va = function
    [] -> []
  | item :: rem ->
      let rem' = nondep_sig env va rem in
      match item with
        Sig_value(id, d) ->
          Sig_value(id,
                    {d with val_type = Ctype.nondep_type env mid d.val_type})
          :: rem'
      | Sig_type(id, d, rs) ->
          Sig_type(id, Ctype.nondep_type_decl env mid (va = Co) d, rs)
          :: rem'
      | Sig_typext(id, ext, es) ->
          Sig_typext(id, Ctype.nondep_extension_constructor env mid ext, es)
          :: rem'
      | Sig_module(id, pres, md, rs) ->
          let md = {md with md_type=nondep_mty env va md.md_type} in
          Sig_module(id, pres, md, rs) :: rem'
      | Sig_modtype(id, d) ->
          begin try
            Sig_modtype(id, nondep_modtype_decl env d) :: rem'
          with Not_found ->
            match va with
              Co -> Sig_modtype(id, {mtd_type=None; mtd_loc=Location.none;
                                     mtd_attributes=[]}) :: rem'
            | _  -> raise Not_found
          end
      | Sig_class(id, d, rs) ->
          Sig_class(id, Ctype.nondep_class_declaration env mid d, rs)
          :: rem'
      | Sig_class_type(id, d, rs) ->
          Sig_class_type(id, Ctype.nondep_cltype_declaration env mid d, rs)
          :: rem'

  and nondep_modtype_decl env mtd =
    {mtd with mtd_type = Misc.may_map (nondep_mty env Strict) mtd.mtd_type}

  and nondep_module_alias env = function
    | Ma_path p ->
        if Path.isfree mid p then raise Not_found
        else Ma_path p
    | Ma_dot(ma, s) -> Ma_dot(nondep_module_alias env ma, s)
    | Ma_tconstraint(ma, mty) ->
        Ma_tconstraint(nondep_module_alias env ma, nondep_mty env Strict mty)

  in
    nondep_mty env Co mty

let enrich_typedecl env p id decl =
  match decl.type_manifest with
    Some _ -> decl
  | None ->
      try
        let orig_decl = Env.find_type p env in
        if decl.type_arity <> orig_decl.type_arity then
          decl
        else
          let orig_ty =
            Ctype.reify_univars
              (Btype.newgenty(Tconstr(p, orig_decl.type_params, ref Mnil)))
          in
          let new_ty =
            Ctype.reify_univars
              (Btype.newgenty(Tconstr(Pident id, decl.type_params, ref Mnil)))
          in
          let env = Env.add_type ~check:false id decl env in
          Ctype.mcomp env orig_ty new_ty;
          let orig_ty =
            Btype.newgenty(Tconstr(p, decl.type_params, ref Mnil))
          in
          {decl with type_manifest = Some orig_ty}
      with Not_found | Ctype.Unify _ ->
        (* - Not_found: type which was not present in the signature, so we don't
           have anything to do.
           - Unify: the current declaration is not compatible with the one we
           got from the signature. We should just fail now, but then, we could
           also have failed if the arities of the two decls were different,
           which we didn't. *)
        decl

let rec enrich_modtype env p mty =
  match mty with
    Mty_signature sg ->
      Mty_signature(List.map (enrich_item env p) sg)
  | _ ->
      mty

and enrich_item env p = function
    Sig_type(id, decl, rs) ->
      Sig_type(id,
                enrich_typedecl env (Pdot(p, Ident.name id)) id decl, rs)
  | Sig_module(id, pres, md, rs) ->
      Sig_module(id, pres,
                  {md with
                   md_type = enrich_modtype env
                       (Pdot(p, Ident.name id)) md.md_type},
                 rs)
  | item -> item

let rec type_paths env p mty =
  match scrape env mty with
    Mty_ident _ -> []
  | Mty_alias _ -> []
  | Mty_signature sg -> type_paths_sig env p sg
  | Mty_functor _ -> []

and type_paths_sig env p sg =
  match sg with
    [] -> []
  | Sig_type(id, _decl, _) :: rem ->
      Pdot(p, Ident.name id) :: type_paths_sig env p rem
  | Sig_module(id, pres, md, _) :: rem ->
      type_paths env (Pdot(p, Ident.name id)) md.md_type @
      type_paths_sig (Env.add_module_declaration ~check:false id pres md env)
        p rem
  | Sig_modtype(id, decl) :: rem ->
      type_paths_sig (Env.add_modtype id decl env) p rem
  | (Sig_value _ | Sig_typext _ | Sig_class _ | Sig_class_type _) :: rem ->
      type_paths_sig env p rem


let rec no_code_needed_mod env pres mty =
  match pres with
  | Mta_absent -> true
  | Mta_present -> begin
      match scrape env mty with
        Mty_ident _ -> false
      | Mty_signature sg -> no_code_needed_sig env sg
      | Mty_functor _ -> false
      | Mty_alias _ -> false
    end

and no_code_needed_sig env sg =
  match sg with
    [] -> true
  | Sig_module(id, pres, md, _) :: rem ->
      no_code_needed_mod env pres md.md_type &&
      no_code_needed_sig
        (Env.add_module_declaration ~check:false id pres md env) rem
  | (Sig_type _ | Sig_modtype _ | Sig_class_type _) :: rem ->
      no_code_needed_sig env rem
  | (Sig_value _ | Sig_typext _ | Sig_class _) :: _ ->
      false

let no_code_needed env mty = no_code_needed_mod env Mta_present mty

(* Check whether a module type may return types *)

let rec contains_type env = function
    Mty_ident path ->
      begin try match (Env.find_modtype path env).mtd_type with
      | None -> raise Exit (* PR#6427 *)
      | Some mty -> contains_type env mty
      with Not_found -> raise Exit
      end
  | Mty_signature sg ->
      contains_type_sig env sg
  | Mty_functor (_, _, body) ->
      contains_type env body
  | Mty_alias _ ->
      ()

and contains_type_sig env = List.iter (contains_type_item env)

and contains_type_item env = function
    Sig_type (_,({type_manifest = None} |
                 {type_kind = Type_abstract; type_private = Private}),_)
  | Sig_modtype _
  | Sig_typext (_, {ext_args = Cstr_record _}, _) ->
      (* We consider that extension constructors with an inlined
         record create a type (the inlined record), even though
         it would be technically safe to ignore that considering
         the current constraints which guarantee that this type
         is kept local to expressions.  *)
      raise Exit
  | Sig_module (_, _, {md_type = mty}, _) ->
      contains_type env mty
  | Sig_value _
  | Sig_type _
  | Sig_typext _
  | Sig_class _
  | Sig_class_type _ ->
      ()

let contains_type env mty =
  try contains_type env mty; false with Exit -> true


(* Remove module aliases from a signature *)

let rec get_prefixes = function
  | Pident _ -> Path.Set.empty
  | Pdot (p, _)
  | Papply (p, _) -> Path.Set.add p (get_prefixes p)

let rec get_arg_paths = function
  | Pident _ -> Path.Set.empty
  | Pdot (p, _) -> get_arg_paths p
  | Papply (p1, p2) ->
      Path.Set.add p2
        (Path.Set.union (get_prefixes p2)
           (Path.Set.union (get_arg_paths p1) (get_arg_paths p2)))

let rec rollback_path subst p =
  try Pident (Path.Map.find p subst)
  with Not_found ->
    match p with
      Pident _ | Papply _ -> p
    | Pdot (p1, s) ->
        let p1' = rollback_path subst p1 in
        if Path.same p1 p1' then p else rollback_path subst (Pdot (p1', s))

let rec collect_ids subst bindings p =
    begin match rollback_path subst p with
      Pident id ->
        let ids =
          try collect_ids subst bindings (Ident.find_same id bindings)
          with Not_found -> Ident.Set.empty
        in
        Ident.Set.add id ids
    | _ -> Ident.Set.empty
    end

let collect_arg_paths mty =
  let open Btype in
  let paths = ref Path.Set.empty
  and subst = ref Path.Map.empty
  and bindings = ref Ident.empty in
  (* let rt = Ident.create "Root" in
     and prefix = ref (Path.Pident rt) in *)
  let it_path p = paths := Path.Set.union (get_arg_paths p) !paths
  and it_signature_item it si =
    type_iterators.it_signature_item it si;
    match si with
    | Sig_module (id, _, {md_type=Mty_alias alias}, _) ->
        let path = path_of_module_alias alias in
        bindings := Ident.add id path !bindings
    | Sig_module (id, _, {md_type=Mty_signature sg}, _) ->
        List.iter
          (function Sig_module (id', _, _, _) ->
              subst :=
                Path.Map.add (Pdot (Pident id, Ident.name id')) id' !subst
            | _ -> ())
          sg
    | _ -> ()
  in
  let it = {type_iterators with it_path; it_signature_item} in
  it.it_module_type it mty;
  it.it_module_type unmark_iterators mty;
  Path.Set.fold (fun p -> Ident.Set.union (collect_ids !subst !bindings p))
    !paths Ident.Set.empty

let rec remove_aliases_mty env excl pres mty =
  match mty with
    Mty_signature sg ->
      Mta_present, Mty_signature (remove_aliases_sig env excl sg)
  | Mty_alias _ ->
      let mty' = Env.scrape_alias_and_ident env mty in
      if mty' = mty then pres, mty else
      remove_aliases_mty env excl pres mty'
  | mty ->
      Mta_present, mty

and remove_aliases_sig env excl sg =
  match sg with
    [] -> []
  | Sig_module(id, pres, md, rs) :: rem  ->
      let pres, mty =
        match md.md_type with
          Mty_alias _ when Ident.Set.mem id excl ->
            pres, md.md_type
        | mty ->
            remove_aliases_mty env excl pres mty
      in
      Sig_module(id, pres, {md with md_type = mty} , rs) ::
      remove_aliases_sig (Env.add_module id pres mty env) excl rem
  | Sig_modtype(id, mtd) :: rem ->
      Sig_modtype(id, mtd) ::
      remove_aliases_sig (Env.add_modtype id mtd env) excl rem
  | it :: rem ->
      it :: remove_aliases_sig env excl rem

let scrape_for_type_of ~remove_aliases env mty =
  let _, mty =
    if remove_aliases then begin
      let excl = collect_arg_paths mty in
      remove_aliases_mty env excl Mta_present mty
    end else begin
      scrape_for_type_of env Mta_present mty
    end
  in
  mty

(* Lower non-generalizable type variables *)

let lower_nongen nglev mty =
  let open Btype in
  let it_type_expr it ty =
    let ty = repr ty in
    match ty with
      {desc=Tvar _; level} ->
        if level < generic_level && level > nglev then set_level ty nglev
    | _ ->
        type_iterators.it_type_expr it ty
  in
  let it = {type_iterators with it_type_expr} in
  it.it_module_type it mty;
  it.it_module_type unmark_iterators mty
