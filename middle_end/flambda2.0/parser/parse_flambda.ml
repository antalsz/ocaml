module Lex = Flambda_lex
module Parser = Flambda_parser

type error =
  | Lexing_error of Lex.error * Location.t
  | Parsing_error of string * Location.t

let make_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let parse_fexpr filename =
  let ic = open_in filename in
  try
    let pos = { Lexing.pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
    let lb = Lexing.from_channel ic in
    let lb = { lb with lex_start_p = pos; lex_curr_p = pos } in
    let supplier = Parser.MenhirInterpreter.lexer_lexbuf_to_supplier Lex.token lb in
    let start = Parser.Incremental.flambda_unit pos in
    let unit =
      try Parser.MenhirInterpreter.loop_handle
            (fun ans -> Ok ans)
            (function
              | HandlingError error_state ->
                let s =
                  Parser.MenhirInterpreter.current_state_number error_state
                in
                let msg = Flambda_parser_messages.message s in
                let loc =
                  make_loc (Parser.MenhirInterpreter.positions error_state)
                in
                Error (Parsing_error (msg, loc))
              | _ ->
                assert false (* the manual promises that HandlingError is the
                only possible constructor *))
            supplier start
      with
      | Lex.Error (error, loc) ->
        Error (Lexing_error (error, make_loc loc))
    in
    close_in ic;
    unit
  with
  | e ->
    let x = Printexc.get_raw_backtrace () in
    close_in ic;
    Printexc.raise_with_backtrace e x

let parse (* ~backend *) filename =
  parse_fexpr filename
  |> Result.map (Fexpr_to_flambda.conv (* ~backend *)) 
