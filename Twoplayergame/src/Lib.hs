{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Lib where
import Datatype
import Crypto.Hash
import qualified Data.ByteString.Char8 as BS

someFunc :: IO ()
someFunc = putStrLn "someFunc"



{- This function would be called by each player, and it would return a Card. 
   It could have been written as makeMove :: IO Card, but it much better to keep 
   the  IO/randomness separate from logic.
   If you pass a number whose value is more than cardinality of Card then it would
   throw a run time error. One way to solve this in Haskell is, compute 
   v = mod n (length Card) and pass v to toEnum function. 
   In more expressive languages with dependent types, e.g. Idris, Agda, or Coq, 
   rather than passing Int, pass a value of finite type. 
   https://coq.inria.fr/library/Coq.Logic.FinFun.html -}

makeMove :: Int -> Card
makeMove n = toEnum n


type RandomByte = BS.ByteString
{- Compute the commitment hash value of (random bytestring || card value).
   Publish the commitment. The reason for passing RandomBytes is because 
   1. It give opportunity to pass random number generated from any external source, 
      e.g. We can call some api from https://www.random.org, and pass it to 
      commitment function.  -}
generateCommitment :: RandomByte -> Card -> BS.ByteString
generateCommitment rnd crd =  ret where
  ret = BS.pack . show . hashWith SHA512 . 
        BS.append rnd $ (BS.pack . show $ crd)

type Commitment = BS.ByteString
{- This function verifies the claim -}
verifyCommitment :: RandomByte -> Card -> Commitment -> Bool
verifyCommitment rnd crd commit = 
  commit == (BS.pack . show . hashWith SHA512 . 
             BS.append rnd $ (BS.pack . show $ crd))     



