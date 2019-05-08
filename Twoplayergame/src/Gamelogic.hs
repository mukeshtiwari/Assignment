module Gamelogic where
import Datatype

{- This function would be called by each player, and it would return a Card.
   If you pass a number whose value is more than cardinality of Card then it would
   throw a run time error. One way to solve this in Haskell is, compute
   v = mod n (cardinality Card) and pass v to toEnum function.
   In more expressive languages with dependent types, e.g. Idris, Agda, or Coq,
   rather than passing Int, pass a value of finite type.
   https://coq.inria.fr/library/Coq.Logic.FinFun.html 
   Also, our game is simple, so we have just one rule.
-}

makeMove :: Int -> Card
makeMove n = toEnum n
