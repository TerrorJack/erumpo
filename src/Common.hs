module Common where

import qualified Data.Map as Map

data Dec =
    DefineDec Pat Exp
    | ImportDec String

type Env = Map.Map String Val

data Exp =
    ConstExp Val
    | VarExp String
    | LambdaExp Pat Exp
    | LetrecExp [(Pat,Exp)] Exp
    | IfExp Exp Exp Exp
    | CaseExp Exp [(Pat,Exp)]
    | AppExp Exp Exp
    | ADTExp String [Exp]
    | UnaryOpExp String Exp
    | BinaryOpExp String Exp Exp

data Val =
    UnitVal
    | BoolVal Bool
    | IntVal Integer
    | FloatVal Float
    | CharVal Char
    | ClosureVal Pat Exp Env
    | ADTVal String [Val]

data Pat =
    NilPat
    | ConstPat Val
    | VarPat String
    | ADTPat String [Pat]
