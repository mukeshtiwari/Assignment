module Ped where
import Datatype
import Crypto.Number.ModArithmetic
import Pedpubparameter

{- This file implements Pedersen's commitment scheme -}

type RInt = Integer {- It could have been better if it were dependent product type. {x : Integer | In x Zq} -}
type Commitment = Integer

{- Generate commitment.  g^m * h ^ r -}
generateCommitment :: RInt -> Card -> Commitment
generateCommitment rnd crd =  ret where
  ret = mod ((expFast g (toInteger . fromEnum $ crd) p) *
             (expFast h rnd p)) p  


{- This function verifies the claim. 
   Reveal randomenss and card => g^(toEnum card) * h ^ randomness == commitment
-}
verifyCommitment :: RInt -> Card -> Commitment -> Bool
verifyCommitment rnd crd commit = 
  commit == mod ((expFast g (toInteger . fromEnum $ crd) p) *  
                 (expFast h rnd p)) p 





