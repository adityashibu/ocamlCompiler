type t 

val peek_char : t -> char option
val read_char : t -> unit
val from_string : string -> t
val is_whitespace : char -> bool
val skip_whitespace : t -> unit
val read_ident : t -> int option -> Token.t
val next_token : t -> Token.t
