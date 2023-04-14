module Sugar where

import Exp

desugarVar :: Var -> IndexedVar
desugarVar a = makeIndexedVar (getVar a)

-- >>> desugarVar (Var "x")
-- IndexedVar {ivName = "x", ivCount = 0}

sugarVar :: IndexedVar -> Var
sugarVar (IndexedVar name cnt) = 
    case cnt of
        0 -> (Var name)
        _ -> (Var (name ++ "_" ++ show(cnt)))

desugarExp :: ComplexExp -> Exp
desugarExp (CX var) = (X (desugarVar var))
desugarExp (CLam var b) = (Lam (desugarVar var) (desugarExp b))
desugarExp (CApp a b) = (App (desugarExp a) (desugarExp b))

sugarExp :: Exp -> ComplexExp
sugarExp (X ivar) = (CX (sugarVar ivar))
sugarExp (Lam ivar b) = (CLam (sugarVar ivar) (sugarExp b))
sugarExp (App a b) = (CApp (sugarExp a) (sugarExp b))