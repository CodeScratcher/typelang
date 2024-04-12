type token = 
  | Number of string
  | String of string
  | Ident of string
  | LParen
  | RParen
  | EOF

type tokenState = EmptyState | NumberState of string | StringState of string | IdentState of string

let next string = String.sub string 1 (String.length string - 1)

(* Behold: The tokenizer monstrosity. 5 mutually recursive functions representing a state machine*)

let[@tail_mod_cons] rec tokenize string state = 
  match (string, state) with
    | ("", EmptyState) -> [Result.Ok EOF]
    | ("", IdentState x) -> [Result.Ok (Ident x)]
    | ("", NumberState x) -> [Result.Ok (Number x)]
    | ("", StringState x) -> [Result.Ok (String x)]
    | (_, EmptyState) ->  tokenizeEmptyState string
    | (_, IdentState x) -> tokenizeIdentState string x
    | (_, NumberState x) -> tokenizeNumberState string x
    | (_, StringState x) -> tokenizeStringState string x

and[@tail_mod_cons] tokenizeEmptyState string = 
  let char = string.[0] in 
    match char with
      | 'A'..'Z' | 'a'..'z' | '!' | '#'..'&' | '*'..'/' | ':'..'@' | '{'..'~' -> tokenize (next string) (IdentState (String.make 1 char))
      | '0'..'9' -> tokenize (next string) (NumberState (String.make 1 char))
      | '"' -> tokenize (next string) (StringState "") 
      | '(' -> Result.Ok LParen :: tokenize (next string) EmptyState
      | ')' -> Result.Ok RParen :: tokenize (next string) EmptyState
      | '\x00'..' ' -> tokenize (next string) EmptyState
      | _ -> [Result.Error "Invalid token"]

and[@tail_mod_cons] tokenizeIdentState string cur = 
  let char = string.[0] in
    match char with
      | 'A'..'Z' | 'a'..'z' | '!' | '#'..'&' | '*'..'/' | ':'..'@' | '{'..'~' | '0'..'9' -> tokenize (next string) (IdentState (cur ^ (String.make 1 char)))
      | _ -> Result.Ok (Ident cur) :: tokenize string EmptyState

and[@tail_mod_cons] tokenizeNumberState string cur = 
  let char = string.[0] in
    match char with 
      | '0'..'9' | '.' | 'f' -> tokenize (next string) (NumberState (cur ^ (String.make 1 char)))
      | _ -> Result.Ok (Number cur) :: tokenize string EmptyState

and[@tail_mod_cons] tokenizeStringState string cur = 
  let char = string.[0] in
    match char with 
      | '"' -> Result.Ok (Number cur) :: tokenize string EmptyState
      | _ -> tokenize (next string) (StringState (cur ^ (String.make 1 char)))