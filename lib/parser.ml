open Lexer 
open Ast

let compose f g x = f (g x)

let int_parse x err = 
  match int_of_string_opt x with
    | Option.Some x -> Result.Ok (IntAtom x)
    | Option.None -> Result.Error err

let rec parse tokens = match List.hd tokens with
  | Number x -> int_parse x "Integer parsing error" :: parse (List.tl tokens)
  | _ -> [Result.Error "Unimplemented"]