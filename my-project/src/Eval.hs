module Eval where

import Exp
import Data.List ( union, delete, nub, sort )

vars :: Exp -> [IndexedVar]
vars (X ivar) = [ivar]
vars (Lam ivar b) =  nub (vars b)
vars (App a b) = nub (union (vars a) (vars b))

freeVars :: Exp -> [IndexedVar]
freeVars (X ivar) = [ivar]
freeVars (Lam ivar b) =  delete ivar (nub (vars b)) 
freeVars (App a b) = nub (union (vars a) (vars b))

-- >>> freeVars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "y", ivCount = 0}]

occursFree :: IndexedVar -> Exp -> Bool
occursFree ivar expr = ivar `elem` (freeVars expr)

-- >>> makeIndexedVar "x" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- False
-- >>> makeIndexedVar "y" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- True

smallestNotIn :: [Int] -> Int
smallestNotIn xs = go (sort xs) 0
  where
    go [] n = n
    go (x:xs) n
      | x < n = go xs n
      | x == n = go xs (n+1)
      | x > n = n

freshVar :: IndexedVar -> [IndexedVar] -> IndexedVar
freshVar ivar list = let mylist = filter (\elem -> ivName(elem) == ivName(ivar)) (nub list)
                    in let allints = map (\elem -> ivCount(elem)) mylist
                        in (IndexedVar (ivName ivar) (smallestNotIn allints))

-- >>> freshVar (makeIndexedVar "x") [makeIndexedVar "x"]
-- IndexedVar {ivName = "x", ivCount = 1}


renameVar :: IndexedVar -> IndexedVar -> Exp -> Exp
renameVar toReplace replacement (X ivar) = if toReplace == ivar then (X replacement) else (X ivar)
renameVar toReplace replacement (Lam ivar b) = if toReplace == ivar then (Lam ivar b) else (Lam ivar (renameVar toReplace replacement b))
renameVar toReplace replacement (App a b) = (App (renameVar toReplace replacement a) (renameVar toReplace replacement b))

-- renameVar (makeIndexedVar "x") (makeIndexedVar "z") (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))

substitute :: IndexedVar -> Exp -> Exp -> Exp
substitute toReplace replacement (X ivar) = if toReplace == ivar then replacement else (X ivar)
substitute toReplace replacement (Lam ivar b) = 
    if toReplace == ivar then (Lam ivar b)
    else if not (occursFree ivar replacement) then (Lam ivar (substitute toReplace replacement b))
        else let listaelems = nub ((freeVars b) ++ (freeVars replacement))
                 newvar = (freshVar ivar listaelems)
            in (Lam newvar (substitute toReplace replacement (renameVar ivar newvar b)))
substitute toReplace replacement (App a b) = (App (substitute toReplace replacement a) (substitute toReplace replacement b))
-- >>> substitute (makeIndexedVar "x") (X (makeIndexedVar "y")) (Lam (makeIndexedVar "z") (X (makeIndexedVar "x")))

 
pasReductie :: Exp -> Maybe Exp
pasReductie (App (Lam x m) n) = Just (substitute x n m)
pasReductie _ = Nothing


normalize :: Exp -> Exp
normalize exp = let ans = (pasReductie exp)
                in (maybe exp normalize ans)

-- >>> normalize (X (makeIndexedVar "x"))
-- X (IndexedVar {ivName = "x", ivCount = 0})

myvar = (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) ( X (IndexedVar {ivName = "y", ivCount = 0}))) (App (Lam (IndexedVar {ivName = "z", ivCount = 0}) (App ( X (IndexedVar {ivName = "z", ivCount = 0})) ( X (IndexedVar {ivName = "z", ivCount = 0})))) (Lam (IndexedVar {ivName = "w", ivCount = 0}) ( X (IndexedVar {ivName = "w", ivCount = 0})))))