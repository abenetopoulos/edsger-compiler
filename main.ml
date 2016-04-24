let () =
    let cin,fname =
        if Array.length Sys.argv > 1
            then open_in Sys.argv.(1), Sys.argv.(1)
            else stdin, "stdin"
    in
    let lexbuf = Lexing.from_channel cin in
    try
        let _ = Parser.prog (Lexer.read [fname]) lexbuf in ()
    with
    | Failure msg         -> print_endline ("Failure --- " ^ msg)
    | Parsing.Parse_error -> print_endline ("Parse error")
    | End_of_file         -> print_endline "Parse error: unexpected end of string"
