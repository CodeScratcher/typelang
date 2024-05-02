open Lexer 
open Ast

let int_parse x err = 
  match int_of_string_opt x with
    | Option.Some x -> AtomExpr (IntAtom x)
    | Option.None -> ParseError err

let parseAtom token = match token with 
  | Number x -> int_parse x "Wrong type"
  | Ident x -> (AtomExpr (IdentAtom x))
  | String x -> (AtomExpr (StringAtom x))
  | _ -> ParseError "Wrong token"

let rec parseList tokens = match tokens with
  | [] -> ([ParseError "Unexpected end of file, expecting right parenthesis"], [])
  | head :: tail -> (match head with 
    | RParen -> ([], tail)
    | LParen -> let parsedList = parseList tail in 
      let parsedTail = parseList (snd parsedList) in 
        (ListExpr (fst parsedList) :: fst parsedTail, snd parsedTail)
    | Number _ | Ident _ | String _ -> let parsedTail = parseList tail in (parseAtom head :: fst parsedTail, snd parsedTail)
    | EOF -> ([ParseError "Unexpected end of file, expecting right parenthesis"], [])
    | TokenError x -> ([ParseError x], []))
  
let rec parse tokens = match tokens with
  | [] -> []
  | head :: tail -> (match head with
    | RParen -> [ParseError "Unopened right parenthesis"]
    | LParen -> let parsedList = parseList tail in ListExpr (fst parsedList) :: parse (snd parsedList)
    | Number _ | Ident _ | String _ -> parseAtom head :: parse tail
    | TokenError x -> [ParseError x]
    | EOF -> [])

let parseFromString str = parse (tokenize str EmptyState)