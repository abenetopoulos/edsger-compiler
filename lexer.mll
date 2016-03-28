(*TODO(achilles): we need to create a type to hold the name of the file we are currently
 * reading from, to be able to provide better error messages*)
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
let exp = ['e' 'E'] ['-' '+']? digit+
let int_in = digit+
let float_in = digit+ '.' digit+ exp?

let white = [' ' '\t']+
let newline = '\r' | '\n'
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let keyword = "bool" | "break" | "byref" | "char" | "continue" | "delete" | "double" | "else" | "for" | "false" | "if" | "int" | "new" | "NULL" | "return" | "true" | "void"

let op = ['=' '>' '<' '+' '-' '*' '/' '%' '&' '!' '?' ':' ','] | "==" | "!=" | '>' | '<' | ">=" | "<=" | "&&" | "||" | "++" | "--" | "+=" | "-=" | "*=" | "/=" | "%="
let separator = [';' '(' ')' '[' ']' '{' '}']

rule read incFiles= 
    parse
    | white {read incFiles lexbuf}
    | newline {next_line lexbuf; read incFiles lexbuf}
    | "//" [^ '\n']* {printf "Single line comment\n"; read incFiles lexbuf}
    | "/*" {read_comment lexbuf.lex_curr_p.pos_lnum incFiles lexbuf; read incFiles lexbuf}
    | const_char {printf "Constant char %s\n" (Lexing.lexeme lexbuf); read incFiles lexbuf}
    | int_in {printf "Integer %d (%s)\n" (int_of_string (Lexing.lexeme lexbuf)) (Lexing.lexeme lexbuf); read incFiles lexbuf}
    | float_in {printf "Float %f (%s)\n" (float_of_string (Lexing.lexeme lexbuf)) (Lexing.lexeme lexbuf); read incFiles lexbuf}
    | '"' {let str = read_string (Buffer.create 17) lexbuf in printf "String %S\n" str; read incFiles lexbuf}
    | "#include" {start_include incFiles lexbuf; read incFiles lexbuf}
    | keyword {printf "Keyword %s\n" (Lexing.lexeme lexbuf); read incFiles lexbuf}
    | id {printf "Identifier %s\n" (Lexing.lexeme lexbuf); read incFiles lexbuf}
    | op {printf "Operator %s\n" (Lexing.lexeme lexbuf); read incFiles lexbuf}
    | separator {printf "Separator %s\n" (Lexing.lexeme lexbuf); read incFiles lexbuf}
    | eof {raise End_of_file}
    | _ {printf "Invalid token %s on line %d\n" (Lexing.lexeme lexbuf) lexbuf.lex_curr_p.pos_lnum; read incFiles lexbuf}
and start_include incFiles=
    parse
    | '"' {let fileName = read_string (Buffer.create 17) lexbuf in 
           if (not (List.exists (fun x -> x = fileName) incFiles)) then
                let inF = open_in fileName in
                let lbuf = Lexing.from_channel inF in
                try read (fileName :: incFiles) lbuf 
                with End_of_file -> ()
           else begin
               printf "Cyclical \"#include\"s found. Exiting\n";
               exit 1
           end
          }
    | [' ']* {start_include incFiles lexbuf}
and read_string buf =
    parse
    | '"'       { (Buffer.contents buf)}
    | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
    | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
    | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
    | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
    | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | [^ '"' '\\']+
        { Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_string buf lexbuf
        }
    | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (SyntaxError ("String is not terminated")) }
and read_comment startLine incFiles=
    parse
    | "*/"      {printf "Comment\n"; read incFiles lexbuf}
    | _         {read_comment startLine incFiles lexbuf}
    | eof 
        { let errString = sprintf "Comment starting on line %d is not terminated" startLine in
          raise (SyntaxError errString)}

{
    let main () =
        let cin =
            if Array.length Sys.argv > 1
            then open_in Sys.argv.(1)
            else stdin
        in
        let lexbuf = Lexing.from_channel cin in
        try read [] lexbuf
        with End_of_file -> ()

    let _ = Printexc.print main ()
}
