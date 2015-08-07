type t = A of string * int | B of int

let rec f x =
  if x = 0 then f 47
  else if x < 42 then A ("foo", x + 3)
  else B (x * 2)

let () =
  begin match f 9 with
  | A (s, i) -> print_string s; print_int i; print_newline ()
  | B x -> print_int x; print_newline ()
  end;
  begin match f 50 with
  | A (s, i) -> print_string s; print_int i; print_newline ()
  | B x -> print_int x; print_newline ()
  end;
  begin match f 0 with
  | A (s, i) -> print_string s; print_int i; print_newline ()
  | B x -> print_int x; print_newline ()
  end;
  Allocation_profiling.debug ()
