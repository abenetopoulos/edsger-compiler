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

rule read = 
    parse
    | white {read lexbuf}
    | newline {next_line lexbuf; read lexbuf}
    | "//" [^ '\n']* {printf "Single line comment\n"; read lexbuf}
    | "/*" {read_comment lexbuf.lex_curr_p.pos_lnum lexbuf; read lexbuf}
    | const_char {printf "Constant char %s\n" (Lexing.lexeme lexbuf); read lexbuf}
    | int_in {printf "Integer %d (%s)\n" (int_of_string (Lexing.lexeme lexbuf)) (Lexing.lexeme lexbuf); read lexbuf}
    | float_in {printf "Float %f (%s)\n" (float_of_string (Lexing.lexeme lexbuf)) (Lexing.lexeme lexbuf); read lexbuf}
    | '"' {read_string (Buffer.create 17) lexbuf}
    | "#include" { (); read lexbuf}
    | keyword {printf "Keyword %s\n" (Lexing.lexeme lexbuf); read lexbuf}
    | id {printf "Identifier %s\n" (Lexing.lexeme lexbuf); read lexbuf}
    | op {printf "Operator %s\n" (Lexing.lexeme lexbuf); read lexbuf}
    | separator {printf "Separator %s\n" (Lexing.lexeme lexbuf); read lexbuf}
    | eof {raise End_of_file}
    | _ {printf "Invalid token %s on line %d\n" (Lexing.lexeme lexbuf) lexbuf.lex_curr_p.pos_lnum; read lexbuf}
and read_string buf =
    parse
    | '"'       { printf "String \"%S\"\n" (Buffer.contents buf);  read lexbuf }
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
and read_comment startLine=
    parse
    | "*/"      {printf "Comment\n"; read lexbuf}
    | _         {read_comment startLine lexbuf}
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
        try read lexbuf
        with End_of_file -> ()

    let _ = Printexc.print main ()
}
