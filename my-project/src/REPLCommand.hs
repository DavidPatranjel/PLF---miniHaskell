
module REPLCommand where

import Lab2
import Control.Applicative (many, (<|>))
import Parsing

data REPLCommand
  = Quit
  | Load String
  | Eval String

instance Show REPLCommand where
  show Quit = "Quit"
  show (Load a) = "Load " ++ a
  show (Eval a) = "Eval " ++ a

quit :: Parser REPLCommand
quit  = do
  (reserved ":quit") <|> (reserved ":q")
  endOfInput
  return Quit


load :: Parser REPLCommand
load = do
  (reserved ":load") <|> (reserved ":l")
  rest <- (many anychar)
  return (Load rest)


eval :: Parser REPLCommand
eval = do
    rest <- (many anychar)
    return (Eval rest)


replCommand :: Parser REPLCommand
replCommand =  quit <|> load <|> eval
