{-# LANGUAGE MultiWayIf #-}
module Main where
import Crypto.Random
import qualified Lib as Lib
import qualified Ped as Ped
import Datatype
import Pedpubparameter
import qualified Data.ByteString.Char8 as BS
import Crypto.Number.Generate
import Gamelogic
import System.IO


size :: Int
size = 2048

readCard :: String -> Card
readCard s = read  s

{-
-- Game sequence which uses Hash based mechanism. 
-- It calls function from library Lib.hs
gameMove :: Player -> IO (Card, Rand, Commitment)
gameMove p = do
  putStrLn $ show p ++ " on move:"
  str <- System.IO.getLine
  let card = readCard str
  randbyte <- getRandomBytes size 
  let commit = Lib.generateCommitment (RByte randbyte) card
  putStrLn $ "I have played my card, and my commitment is: " ++ show commit
  return (card, RByte randbyte, commit)  

--Verification function which checks the claim of users. 
--It call the function from Lib.hs
verificationPhase :: Player -> Player -> Rand -> Commitment -> IO Bool
verificationPhase p1 p2 rnd comm =  do 
  putStrLn $ show p1 ++ ": what was your card " ++ show p2 ++"?(Message to user: Try to cheat here)"
  str <- System.IO.getLine
  let card = readCard str
  return (Lib.verifyCommitment rnd card comm)
--}

-- Game sequence which uses discrete logarithm (Pedersen commitments).
-- It calls function from library Ped.hs
gameMove :: Player -> IO (Card, Rand, Commitment)
gameMove p = do
  putStrLn $ show p ++ " on move:"
  str <- System.IO.getLine
  let card = readCard str
  randnum <- generateBetween 2 (q - 1)
  let commit = Ped.generateCommitment (RInt randnum) card
  putStrLn $ "I have played my card, and my commitment is: " ++ show commit
  return (card, RInt randnum, commit)    


--Verification function which checks the claim of users.
--It call the function from library Ped.hs
verificationPhase :: Player -> Player -> Rand -> Commitment -> IO Bool
verificationPhase p1 p2 rnd comm =  do
  putStrLn $ show p1 ++ ": what was your card " ++ show p2 ++"?(Message to user: Try to cheat here)"
  str <- System.IO.getLine
  let card = readCard str
  return (Ped.verifyCommitment rnd card comm)



main :: IO ()
main = do
  -- Let's say Alice goes first for simplicity
  -- But we cand toss a coin and let that player go first
  (acard, arnd, acom) <- gameMove Alice
  (bcard, brnd, bcom) <- gameMove Bob
  putStrLn "Verification Phase"
  r1 <- verificationPhase Bob Alice arnd acom
  if | not r1 -> putStrLn "Bob => Alice: Your commitment does not match with your answer! You cheated.  I am winner"
     | r1 -> do
          r2 <- verificationPhase Alice Bob brnd bcom
          if | not r2 -> putStrLn "Alice => Bob: Your commitment does not match with your answer! You are  cheater cock ;). I am winner"
             | r2 -> do
               -- No one has cheated, so playgame and declare result
               case playGame acard bcard of
                 Win -> putStrLn "Alice => Bob: I won the game. Good luck next time."
                 Loss -> putStrLn "Bob => Alice: I won the game. Good luck next time."
                 Draw -> putStrLn "Alice <=> Bob: Match ended as draw mate. It was fun game."
    
