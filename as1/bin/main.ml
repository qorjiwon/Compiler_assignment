(* CSE322 Compiler Assignment 1*)

open As1

let runlex filename = 
  try
	  let file = Stdlib.open_in filename in
 	  let lexbuf = Lexing.from_channel file in
 	  let _ = Errormsg.reset filename in
  	  while true do
        let _ = Funlex.initial lexbuf in ()
      done
	with Errormsg.Error -> print_endline "Lexing failed."
     | Funlex.Eof -> print_endline "Lexing done."

let () = runlex "data/test.fun"
