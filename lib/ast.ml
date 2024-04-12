type atom =
  | IntAtom of int
  | StringAtom of string
  | IdentAtom of string
  | FloatAtom of float

type sexpr = 
  | AtomExpr of atom
  | ListExpr of sexpr list