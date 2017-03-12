(*TODO:
    - make command line arg parsing better
*)

open Lexer
open Format
open Semantic
open Ast
open Codegen
open Llvm
open Llvm_analysis

let readFromStd : bool ref = ref false
let writeLLToStd : bool ref = ref false
let writeAsmToStd : bool ref = ref false
let optimize : bool ref = ref false

let get_args () =
    if Array.length Sys.argv > 1 then
    begin
        let name = Array.fold_left (fun acc a ->
            if (a = "-i") then (readFromStd := true; writeAsmToStd := true; acc)
            else if (a = "-f") then (readFromStd := true; writeLLToStd := true; acc)
            else if (a = "-O") then (optimize := true; acc)
            else (
                let ext = String.sub a ((String.length a) - 4) 4 in
                if (ext = ".eds") then 
                    a ^ acc
                else
                    acc
            )) "" Sys.argv
        in
        name
    end
    else
        let usageString = "edsgerc: A compiler for the Edsger language written in OCaml\n\n" ^
                           "Usage: edsgerc [options] [input_file]\n" ^
                           "Options:\n" ^
                           "  -O\tOptimize generated code (equivalent to -O2 in gcc)\n" ^
                           "  -i\tRead program from console, output assembly to console\n" ^
                           "  -f\tRead program from console, output LLVM ir to console\n\n" ^
                           "If neither -i nor -f are used, the user should provide a .eds file containing the source code to be compiled\n"
        in
        (Printf.eprintf "%s" usageString; exit 1)

let dump_assembly fName = 
    let asmChannel = open_in fName in
    try
        while true; do
            Printf.printf "%s\n" (input_line asmChannel)
        done
    with
    | End_of_file -> close_in asmChannel

let () =
    let name = get_args () in
    let doNotCompile = false in
    let cin,fname =
        if not !readFromStd then(
            (*Printf.printf "Filename: %s\n" name;*)
            open_in name, name
        )
        else stdin, "stdin"
    in
    let lexbuf = Lexing.from_channel cin in
    try
        Parser.prog (Lexer.read (fname :: [])) lexbuf;
        check_ast !astTree;
        (*print_ast !astTree;*)
        let llm = code_gen !astTree in
        (match (verify_module llm) with
         | None -> () (*Printf.printf "DEBUG: Module is correct\n"*)
         | Some e ->
            (*Printf.printf "DEBUG: Invalid module: %s\n" e;*)
            exit 1;
        );

        (*Printf.printf "DEBUG: will find name\n";*)
        let baseName = 
            if (name = "") then ".temp"
            else
                let dotI = String.index fname '.' in
                String.sub fname 0 dotI
        in
        let outName = baseName ^ ".ll" in
        (*Printf.printf "DEBUG: Will write ir to: %s\n" outName;*)
        if (!writeLLToStd) then
            dump_module llm
        else
            ()
        ;
        print_module outName llm;

        let optString = 
            if !optimize then "-O=2"
            else ""
        in
        let llcCommand = Printf.sprintf "llc-3.5 %s -filetype=asm %s" optString outName in
        (*let llcCommand = Printf.sprintf "/Volumes/Files/Developer/bin/llc %s -filetype=asm %s" optString outName in*)
        if (Sys.command llcCommand <> 0) then begin
            (*Printf.printf "DEBUG: llc could not compile our program\n";*)
            exit 1
        end
        else
            (*Printf.printf "DEBUG: llc compiled our program\n"*)
            ()
        ;

        let asmName = baseName ^ ".s" in
        if (!writeAsmToStd) then
            dump_assembly asmName
        else
            ()
        ;

        if (doNotCompile = false) then begin 
            let binName = 
                if (baseName = ".temp") then "a.out"
                else baseName
            in
            let libName = "./stdlib/lib.a" in
            let libCheckerName = "./libchecker.a" in
            (*let libName = "" in*)
            let clangCommand = Printf.sprintf "clang -g %s %s %s -o %s" asmName libCheckerName libName binName in
            (*let clangCommand = Printf.sprintf "clang -g %s %s -o %s" asmName libName binName in*)
            if (Sys.command clangCommand <> 0) then begin
                (*Printf.printf "DEBUG: Clang could not compile to binary\n";*)
                exit 1
            end
            else
                ()
                (*Printf.printf "DEBUG: Clang compiled our program\n"*)
            ;
        end
        else
            ()
        ;
        
        if (!writeLLToStd || !writeAsmToStd) then
            ignore (Sys.command "rm .temp.ll .temp.s")
        else (
            (*let delCommand = Printf.sprintf "rm %s %s" outName asmName in
            ignore (Sys.command delCommand)*)
        )
        ;

        exit 0
     with
        | Failure msg         -> print_endline ("Failure --- " ^ msg); exit 1
        | Parsing.Parse_error -> print_endline ("Parse error"); exit 1
        | End_of_file         -> print_endline "Parse error: unexpected end of string"; exit 1
        | _ as exc            -> Printf.printf "\x1b[31mException\x1b[0m: %s\n" (Printexc.to_string exc); exit 1
