(*TODO:
    - write dump_assembly
    - write usage string
    - make command line arg parsing better
    - check each sys call's return value
*)


open Lexer
open Format
open Semantic
open Ast
open Codegen
open Llvm
open Llvm_analysis

let get_args () =
    let readStd : bool ref = ref false in
    let writeLLStd : bool ref = ref false in
    let writeAsmStd : bool ref = ref false in
    let opt : bool ref = ref false in
    if Array.length Sys.argv > 1 then
    begin
        let name = Array.fold_left (fun acc a ->
            if (a = "-i") then (readStd := true; writeAsmStd := true; acc)
            else if (a = "-f") then (readStd := true; writeLLStd := true; acc)
            else if (a = "-O") then (opt := true; acc)
            else (
                let ext = String.sub a ((String.length a) - 4) 4 in
                if (ext = ".eds") then 
                    a ^ acc
                else
                    acc
            )) "" Sys.argv
        in
        !readStd, !writeLLStd, !writeAsmStd, !opt, name
    end
    else
        (Printf.printf "<ADD USAGE STRING>\n"; exit 1)

let dump_assembly fName = 
    () (*TODO: implement simple printing*)

let () =
    let readFromStd, writeLLToStd, writeAsmToStd, optimize, name = get_args () in
    let cin,fname =
        if not readFromStd then(
            Printf.printf "Filename: %s\n" name;
            open_in name, name
        )
        else stdin, "stdin"
    in
    let lexbuf = Lexing.from_channel cin in
    try
        Parser.prog (Lexer.read (fname :: [])) lexbuf;
        check_ast !astTree;
        let llm = code_gen !astTree in
        (match (verify_module llm) with
         | None -> Printf.printf "Module is correct\n"
         | Some e ->
            Printf.printf "Invalid module: %s\n" e
        );

        Printf.printf "will find name\n";
        let baseName = 
            if (name = "") then ".temp"
            else
                let dotI = String.index fname '.' in
                String.sub fname 0 dotI
        in
        let outName = baseName ^ ".ll" in
        Printf.printf "Will write ir to: %s\n" outName;
        if (writeLLToStd) then
            dump_module llm
        else
            ()
        ;
        print_module outName llm;

        let optString = 
            if optimize then "-O=2"
            else ""
        in
        let llcCommand = Printf.sprintf "llc-3.5 %s -filetype=asm %s" optString outName in
        Sys.command llcCommand;

        let asmName = baseName ^ ".s" in
        if (writeAsmToStd) then
            dump_assembly asmName
        else
            ()
        ;

        let binName = 
            if (baseName = ".temp") then "a.out"
            else baseName
        in
        let libName = "~/Developer/univ/compiler/lib/linux/lib.a" in
        let clangCommand = Printf.sprintf "clang-3.5 %s %s -o %s" asmName libName binName in
        Sys.command clangCommand;
        
        exit 0
     with
        | Failure msg         -> print_endline ("Failure --- " ^ msg); exit 1
        | Parsing.Parse_error -> print_endline ("Parse error"); exit 1
        | End_of_file         -> print_endline "Parse error: unexpected end of string"; exit 1
        | _ as exc            -> Printf.printf "\x1b[31mException\x1b[0m: %s\n" (Printexc.to_string exc); exit 1
