module Pedpubparameter where
import Crypto.Number.ModArithmetic
import Crypto.Number.Generate

{- Public Parameters of Pedersen Scheme -}
{- https://en.wikipedia.org/wiki/Schnorr_group -}
{- I have taken these two primes from IACR 2018 election conducted on Helios. The reason for choosing these two primes were: 
    1. I have proved them coq that they are prime.
    2. It's ongoing work on verification of Sigma protocol in Coq, and we have taken IACR election as case study, so I can share 
       my results when paper finishes. -}
{-
   Generates a Safe Prime Field (p,q,g) and a random value
   (a \in Z_q\) such that \(g^a = h\), where g and h are the bases
   to be used in the pedersen commit function. 
--}

p :: Integer
p =  16328632084933010002384055033805457329601614771185955389739167309086214800406465799038583634953752941675645562182498120750264980492381375579367675648771293800310370964745767014243638518442553823973482995267304044326777047662957480269391322789378384619428596446446984694306187644767462460965622580087564339212631775817895958409016676398975671266179637898557687317076177218843233150695157881061257053019133078545928983562221396313169622475509818442661047018436264806901023966236718367204710755935899013750306107738002364137917426595737403871114187750804346564731250609196846638183903982387884578266136503697493474682071

q :: Integer
q =  61329566248342901292543872769978950870633559608669337131139375508370458778917

r :: Integer
r = div (p - 1) q

generateGroup :: [Integer]
generateGroup = filter (\h -> expFast h r p /= 1) $ [2 .. (p - 1)]

g :: Integer
g = 2

{- 
h :: IO Integer
h = generateBetween 2 (q - 1) 
-}

h :: Integer
h = 29365765491083580594055155446471569814605493934783941691606683969649881281777


