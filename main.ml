open Lexer
open Format
open Semantic
open Ast
open Codegen
open Llvm
open Llvm_analysis

let () =
    let cin,fname =
        if Array.length Sys.argv > 1
            then open_in Sys.argv.(1), Sys.argv.(1)
            else stdin, "stdin"
    in
    let lexbuf = Lexing.from_channel cin in
    (try
        Parser.prog (Lexer.read (fname :: [])) lexbuf;
        (*print_ast !astTree;*) (*NOTE: enable this at your own risk!(see comment in ast.ml)*)
        check_ast !astTree;
        let llm = code_gen !astTree in
        (match (verify_module llm) with
         | None -> ()
         | Some e ->
            Printf.printf "Invalid module: %s\n" e
        );
        let outName = fname ^ ".out" in
        print_module outName llm;
        exit 0
     with
        | Failure msg         -> print_endline ("Failure --- " ^ msg); exit 1
        | Parsing.Parse_error -> print_endline ("Parse error"); exit 1
        | End_of_file         -> print_endline "Parse error: unexpected end of string"; exit 1)
