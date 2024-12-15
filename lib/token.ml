type t = EOF | Invalid | Const | Mut | Ident of string

let to_string t = 
  match t with 
  | EOF -> "EOF"
  | Invalid -> "Invalid"
  | Const -> "Const"
  | Mut -> "Mut"
  | Ident s -> "Ident " ^ s;