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
    | Number x -> let parsedTail = parseList tail in (int_parse x "Wrong type" :: fst parsedTail, snd parsedTail)
    | Ident x -> let parsedTail = parseList tail in (AtomExpr (IdentAtom x) :: fst parsedTail, snd parsedTail)
    | String x -> let parsedTail = parseList tail in (AtomExpr (StringAtom x) :: fst parsedTail, snd parsedTail)
    | EOF -> ([ParseError "Unexpected end of file, expecting right parenthesis"], []))
  
let rec parse tokens = match tokens with
  | [] -> []
  | head :: tail -> (match head with
    | RParen -> [ParseError "Unopened right parenthesis"]
    | LParen -> let parsedList = parseList tail in ListExpr (fst parsedList) :: parse (snd parsedList)
    | Number x -> int_parse x "Wrong type" :: parse tail
    | Ident x -> AtomExpr (IdentAtom x) :: parse tail
    | String x -> AtomExpr (StringAtom x) :: parse tail
    | EOF -> [])

