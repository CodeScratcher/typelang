type token = 
  | Number of string
  | String of string
  | Ident of string
  | LParen
  | RParen
  | EOF

type tokenState = EmptyState | NumberState of string | StringState of string | IdentState of string

let tokenize string state = 
  match (string, state) with
    | ("", EmptyState) -> Either.Left (EOF)
    | (_, EmptyState) ->  
      (let char = string.[0] in 
        match char with
          | 'A'..'Z' | 'a'..'z' | '!' | '#'..'&' | '*'..'/' | ':'..'@' -> Either.Left (tokenize (sub string 1 (len string - 1)))
          | _ -> Either.Right "Invalid token")
    | (_, _)  -> Either.Right "Not implemented"