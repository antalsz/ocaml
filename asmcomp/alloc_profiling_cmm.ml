(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2015, Jane Street Group, LLC                             *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

let size_in_words_of_trie_node ~num_instrumented_alloc_points
      ~num_direct_call_points =
  2*num_instrumented_alloc_points + 2*num_direct_call_points

let code_to_allocate_trie_node ~num_allocation_points ~call_types =
  let size = size_in_words_of_trie_node ~num_allocation_points ~call_types in
  let node = Ident.create "node" in
  let header = Cmmgen.black_block_header Obj.abstract_tag size in
  let open Cmm in
  Clet (node,
    Cop (Cextcall ("calloc", [| Int |], false, Debuginfo.none),
      [Cconst_int (1 + size);  (* "1 + " for a header *)
       Cconst_int Sys.word_size;
      ]),
    Csequence (
      Cop (Cstore Word, [node, Cconst_natint header]);
      Cop (Caddi, [node, Cconst_int Arch.size_addr])))

let function_prologue ~num_allocation_points ~call_types =
  let node_hole = Ident.create "node_hole" in
  let node = Ident.create "node" in
  let new_node = Ident.create "new_node" in
  let open Cmm in
  Clet (node_hole, Cop (Calloc_profiling_node_hole, []),
    Clet (node, Cop (Cload Word, [Cvar node_hole]),
      Cifthenelse (Cop (Ccmpi Cne, [Cvar node, Cconst_int 0]),
        Cvar node,
        Clet (new_node,
          code_to_allocate_trie_node ~num_allocation_points ~call_types,
          Csequence (
            Cop (Cstore Word, [Cvar node_hole; Cvar new_node]),
            Cvar new_node)))))

let use_override_profinfo =
  Cmm.Cconst_symbol "caml_allocation_profiling_use_override_profinfo"

let override_profinfo =
  Cmm.Cconst_symbol "caml_allocation_profiling_override_profinfo"

let profinfo_counter =
  Cmm.Cconst_symbol "caml_allocation_profiling_profinfo"

let code_for_allocation_point ~value's_header ~alloc_point_number ~node =
  let pc = Ident.create "pc" in
  let existing_profinfo = Ident.create "existing_profinfo" in
  let new_profinfo = Ident.create "new_profinfo" in
  let new_profinfo' = Ident.create "new_profinfo'" in
  let new_profinfo'' = Ident.create "new_profinfo''" in
  let profinfo = Ident.create "profinfo" in
  let offset_into_node =
    ((2 * Arch.size_addr) * alloc_point_number)
  in
  let open Cmm in
  let address_of_profinfo =
    Cop (Cadda, [
      Cvar node;
      Cconst_int offset_into_node;
    ])
  in
  let address_of_pc =
    Cop (Cadda, [
      Cvar node;
      Cconst_int (offset_into_node + Arch.size_addr);
    ])
  in
  let do_not_use_override_profinfo =
    (* Determine whether values should be annotated with a user-specified
       profinfo. *)
    Cop (Ccmpi Ceq, [
      Cop (Cload Word, [use_override_profinfo]);
      Cconst_int 0;
    ])
  in
  (* CR mshinwell: ensure these match the C code *)
  let profinfo_shift = Cconst_int 42 in
  let max_profinfo = Cconst_int 0x3f_ffff in
  let generate_new_profinfo =
    (* When a new profinfo value is required, we obtain the current
       program counter, and store it together with a fresh profinfo value
       into the current trie node. *)
    Clet (pc, Cop (Cor, [Cprogram_counter; Cconst_int 1]),
      Clet (new_profinfo,
        Clet (new_profinfo', Cop (Cload Word, [profinfo_counter]),
          Cifthenelse (
            Cop (Ccmpi Cgt, [Cvar new_profinfo'; max_profinfo]),
            Cconst_int 0,  (* profiling counter overflow *)
            Clet (new_profinfo'',
              Cop (Caddi, [Cvar new_profinfo'; Cconst_int 1]),
              Csequence (
                Cop (Cstore Word,
                  [profinfo_counter; Cvar new_profinfo'']),
                Cvar new_profinfo'
              )))),
        Csequence (
          Csequence (
            Cop (Cstore Word, [address_of_pc; Cvar pc]),
            Cop (Cstore Word, [address_of_profinfo; Cvar new_profinfo])),
          Cop (Clsl, [Cvar new_profinfo; profinfo_shift]))))
  in
  (* Check if we have already allocated a profinfo value for this allocation
     point with the current backtrace.  If so, use that value; if not,
     allocate a new one. *)
  Clet (existing_profinfo, Cop (Cload Word, [address_of_profinfo]),
    Clet (profinfo,
      Cifthenelse (
        Cop (Ccmpa Ceq, [Cvar existing_profinfo; Cconst_pointer 0]),
        Cvar existing_profinfo,
        Cifthenelse (do_not_use_override_profinfo,
          generate_new_profinfo,
          Cop (Cload Word, [override_profinfo]))),
      (* [profinfo] is already shifted by [PROFINFO_SHIFT]. *)
      Cop (Cor, [Cvar profinfo; Cconst_natint value's_header])))

let code_for_direct_call ~node ~num_instrumented_alloc_points ~callee
      ~direct_call_point_index =
  let offset_in_trie_node_in_words =
    num_instrumented_alloc_points*2 + direct_call_point_index*2
  in
  let place_within_node =
    Cop (Caddi, [
      node;
      Cconst_int (offset_in_trie_node_in_words * Arch.size_addr);
    ]
  in
  let callee_addr =
    Cop (Cor, [Cconst_symbol callee; Cconst_int 3])
  in
  let node_hole_ptr =
    Cop (Caddi, [place_within_node; Cconst_int Arch.size_addr])
  in
  Csequence (
    Cop (Cstore Word, [place_within_node; callee_addr]),
    Cop (Calloc_profiling_load_node_hole_ptr, node_hole_ptr))
