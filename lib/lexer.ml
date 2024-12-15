type t = {
  content: string;
  len: int;
  mutable position: int;
  mutable ch: char option
}

let read_next lexer =
  let ch = if lexer.position >= lexer.len then None
  else Some (String.unsafe_get lexer.content lexer.position)
in 
lexer.ch <- ch;
lexer.position <- lexer.position + 1

let from_string content =
  let lexer = {
    content;
    len = String.length content;
    position = 0;
    ch = None;
  } in 
  read_next lexer;
  lexer