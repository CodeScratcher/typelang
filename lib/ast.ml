type atom =
  | IntAtom of int
  | StringAtom of string
  | IdentAtom of string
  | FloatAtom of float

let printAtom atom = match atom with
  | IntAtom x -> String.cat "Int: " (string_of_int x)
  | StringAtom x -> String.cat "String: " x 
  | IdentAtom x -> String.cat "Ident: " x
  | FloatAtom x -> String.cat "Float: " (string_of_float x)

type sexpr = 
  | AtomExpr of atom
  | ListExpr of sexpr list
  | ParseError of string

let rec printSexpr sexpr = match sexpr with
  | AtomExpr x -> String.cat "Atom: " (printAtom x)
  | ListExpr x -> String.concat " " ("List: (" :: (List.append (List.map printSexpr x) [")"]))
  | ParseError x -> String.cat "Error: " x