module Lib where
import Datatype
import Crypto.Hash
import qualified Data.ByteString.Char8 as BS


type RandomByte = BS.ByteString
type Commitment = BS.ByteString

{- Compute the commitment hash value of (random bytestring || card value).
   Publish the commitment. The reason for passing RandomBytes is because 
   1. It give opportunity to pass random number generated from any external source, 
      e.g. We can call some api from https://www.random.org, and pass it to 
      commitment function.  -}
{- Security of hash based scheme hinges on two things:
 1. Collision Resistence of Hash function
 2. Length of Random byte 
Explain more in doc -}

generateCommitment :: RandomByte -> Card -> BS.ByteString
generateCommitment rnd crd =  ret where
  ret = BS.pack . show . hashWith SHA512 . 
        BS.append rnd $ (BS.pack . show $ crd)

{- This function verifies the claim.
   hash (rnd || card) == commitment -}
verifyCommitment :: RandomByte -> Card -> Commitment -> Bool
verifyCommitment rnd crd commit = 
  commit == (BS.pack . show . hashWith SHA512 . 
             BS.append rnd $ (BS.pack . show $ crd))     



