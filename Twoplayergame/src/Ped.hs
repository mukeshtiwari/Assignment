module Ped where
import Datatype
import Crypto.Number.ModArithmetic
import Pedpubparameter

{- This file implements Pedersen's commitment scheme -}


{- Generate commitment.  g^m * h ^ r -}
generateCommitment :: Rand -> Card -> Commitment
generateCommitment (RInt rnd) crd =  ret where
  ret = CInt (mod ((expFast g (toInteger . fromEnum $ crd) p) *
                   (expFast h rnd p)) p) 


{- This function verifies the claim. 
   Reveal randomenss and card => g^(toEnum card) * h ^ randomness == commitment
-}
verifyCommitment :: Rand -> Card -> Commitment -> Bool
verifyCommitment (RInt rnd) crd (CInt commit) = 
  commit == mod ((expFast g (toInteger . fromEnum $ crd) p) *  
                 (expFast h rnd p)) p 





