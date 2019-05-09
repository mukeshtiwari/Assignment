module Gamerules where
import Datatype


{- 
 Queen beats King. 
 Same cards are Draw 
-}
playGame :: Card -> Card -> Outcome
playGame King Queen = Loss
playGame Queen King = Win
playGame _ _ = Draw
