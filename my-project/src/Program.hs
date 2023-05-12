module Program where
import Exp
import Lab2 ( Parser, endOfInput, whiteSpace, reserved, digit, nat, apply)
import Parsing ( expr, var,  parseFirst, semiSep1, exprParser )
import Sugar ( desugarExp, desugarVar )
import Eval ( substitute )
import Control.Applicative ( Alternative(..) )
import System.IO ( stderr, hPutStrLn )
import qualified Data.Map.Strict as Map

data Definition = Definition{ 
    defHead :: Var
    , defArgs :: [Var]
    , defBody :: ComplexExp
}deriving (Show)

definition :: Parser Definition
definition = do
                head <- var
                args <- many var 
                reserved ":="
                body <- exprParser
                return $ Definition head args body

-- >>> parseFirst definition "id := \\x -> x"
-- Just (Definition {defHead = Var {getVar = "id"}, defArgs = [], defBody = CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"}))})
-- >>> parseFirst definition "id x := x"
-- Just (Definition {defHead = Var {getVar = "id"}, defArgs = [Var {getVar = "x"}], defBody = CX (Var {getVar = "x"})})
-- >>> parseFirst definition "const x y := x"
-- Just (Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})})

program :: Parser [Definition]
program = semiSep1 (whiteSpace *> definition)

-- >>> parseFirst program " id x := x ; const x y := x"
-- Nothing
-- >>> parseFirst program " id x := x ; const x y := x ;"
-- Just [Definition {defHead = Var {getVar = "id"}, defArgs = [Var {getVar = "x"}], defBody = CX (Var {getVar = "x"})},Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})}]

{-
definitionExp :: Definition -> ComplexExp
definitionExp def = let args = defArgs def
                        body = defBody def
                    in if length args == 0 then body
                        else foldr args body CLam

                        -}
-- >>> definitionExp (Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})})
-- CLam (Var {getVar = "x"}) (CLam (Var {getVar = "y"}) (CX (Var {getVar= "x"})))

type Environment = Map.Map IndexedVar Exp

programEnv :: [Definition] -> Environment
programEnv pgm = undefined

normalizeEnv :: Environment -> Exp -> Exp
normalizeEnv = undefined
