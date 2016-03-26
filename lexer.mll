{

open Lexing
open Printf    

exception SyntaxError of string

let next_line lexbuf = 
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'f']
let const_char = ''' ('\\' ['n' 't' 'r' '0' '\\' '''] | ['a'-'z' 'A'-'Z' '0'-'9'] | '\\' 'x' hexdigit hexdigit) ''' 
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let int_in = digit+
let float_in = digit+ frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']*

let unary_op = ['&' '*' '+' '-' '!']
let binary_op = ['*' '/' '%' '+' '-' '<' '>' ] |"<=" | ">=" | "==" | "!=" | "&&" | "||" | ","
let unary_ass = "++" | "--"
let binary_ass = '=' | "*=" | "/=" | "%=" | "+=" | "-="
let separator = [';' '(' ')' '[' ']' '{' '}']

rule read = 
    parse
    | white {read lexbuf}
    | newline {next_line lexbuf; read lexbuf}
    | const_char {printf "Constant char %s\n" (Lexing.lexeme lexbuf); read lexbuf}
    | int_in {printf "Integer %d (%s)\n" (int_of_string (Lexing.lexeme lexbuf)) (Lexing.lexeme lexbuf); read lexbuf}
    | float_in {printf "Float %f (%s)\n" (float_of_string (Lexing.lexeme lexbuf)) (Lexing.lexeme lexbuf); read lexbuf}
    | unary_op {printf "Unary op \'%s\'" (Lexing.lexeme lexbuf); read lexbuf}
    | id {printf "Identifier %s\n" (Lexing.lexeme lexbuf); read lexbuf}
    | eof {raise End_of_file}
    | _ {printf "Invalid token %s\n" (Lexing.lexeme lexbuf); read lexbuf}

{

let main () =
    let cin =
        if Array.length Sys.argv > 1
        then open_in Sys.argv.(1)
        else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    try read lexbuf
    with End_of_file -> ()

let _ = Printexc.print main ()
}
