
module Printing (showExp) where

import Exp

showVar :: Var -> String
showVar a = getVar a


showExp :: ComplexExp -> String
showExp (CX a) = showVar a
showExp (CLam a newcom) = "\\" ++ (showVar a) ++ (showExp newcom)
showExp (CApp a b) = "(" ++ (showExp a) ++ (showExp b) ++ ")"

