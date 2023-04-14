
module Exp where

newtype Var = Var { getVar :: String }
  deriving (Show)

data ComplexExp                         --  ComplexExp ::= "(" ComplexExp ")"
  = CX Var                              --          |   Var
  | CLam Var ComplexExp                 --          |   "\" Var "->" ComplexExp
  | CApp ComplexExp ComplexExp          --          |   ComplexExp ComplexExp
  deriving (Show)

data IndexedVar = IndexedVar
  { ivName :: String
  , ivCount :: Int
  } deriving (Eq, Read, Show)

makeIndexedVar :: String -> IndexedVar
makeIndexedVar name = IndexedVar name 0

data Exp
  = X IndexedVar
  | Lam IndexedVar Exp
  | App Exp Exp
  deriving (Show)