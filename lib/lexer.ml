type t = {
  content: string;
  len: int;
  mutable position: int;
  mutable ch: char option
}

let peek_char lexer = 
  if lexer.position >= lexer.len then None
  else Some (String.unsafe_get lexer.content lexer.position)

let read_char lexer =
  let ch = peek_char lexer in 
lexer.ch <- ch;
lexer.position <- lexer.position + 1

let from_string content =
  let lexer = {
    content;
    len = String.length content;
    position = 0;
    ch = None;
  } in 
 read_char lexer;
  lexer

let is_whitespace c = 
  match c with
  | '\t' 
  | '\n' 
  | '\x0C' 
  | '\r' 
  | ' ' -> true 
  | _ -> false

let rec skip_whitespace lexer = 
  match lexer.ch with
  Some c when is_whitespace c -> 
   read_char lexer;
    skip_whitespace lexer
    | _ -> ()

let rec read_ident lexer start = 
  let start =
    match start with
    | Some start -> start
    | None -> lexer.position - 1 in 

  match peek_char lexer with
  | Some 'a'..'z' 
  | Some 'A'..'Z'
  | Some '0'..'9'
  | Some '_' -> 
   read_char lexer;
    read_ident lexer (Some start)
    | _ -> (
      let value = String.sub lexer.content start (lexer.position - start) in

      match value with 
      | "const" -> Token.Const
      | "mut" -> Token.Mut
      | value -> Token.Ident value)

let next_token lexer = 
  skip_whitespace lexer;
  let token = match lexer.ch with 
  | None -> Token.Invalid
  | Some 'a'..'z' 
  | Some 'A'..'Z'
  | Some '_' -> read_ident lexer None
  | _ -> Token.Invalid 
in
  read_char lexer;
  token