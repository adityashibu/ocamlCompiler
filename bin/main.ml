let lexer = Compiler.Lexer.from_string("const a = 5;");;

let rec print_token_until_invalid lexer = 
  let token = Compiler.Lexer.next_token lexer in
  print_endline ("Token: \"" ^ Compiler.Token.to_string token ^ "\"");
  match token with
  | Invalid -> print_endline "Invalid Input"
  | EOF -> ()
  | _ -> print_token_until_invalid lexer;;

print_token_until_invalid lexer