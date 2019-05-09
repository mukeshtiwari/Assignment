# Assignment
In general, the idea for this kind of situation, where no one trusts 
each other, is using commitment schemes. In this assignment, I have 
used two methods: 

1. Hash based scheme (HMAC)
2. Discrete logarithm (Pedersen's Commitment)

In HMAC based solution (Ped.hs file), I have defined two functions. 

1. ```generateCommitment``` which generates commitment (blob) by taking a 
   random bytes and card as input and returns SHA256 hash of random 
   bytes appended with card, i.e hash (rand-byte || card).
2. ```verifityCommitment``` which verifies the commiment (blob) which
   takes random bytes, card and commitment and verifies the fact 
   that commitment is indeed equal to hash of appended random bytes and 
   card, i.e commitment == hash (rand-byte || card)
   
The security of this whole scheme hinges on length of byte and collision resistence of hash function. If the lenght of bytes used are small 
then adversary
can brute froce all the possible bits (2 ^ n where n is length of byte) and 
combine it with all possible cards to produce the matching commitment. Similarly,  if hash function is not collision resistant, then he can 
produce same commitment with different values of random bytes and card.


```
generateCommitment :: Rand -> Card -> Commitment
generateCommitment (RByte rnd) crd =  ret where
  ret = CByte (BS.pack . show . hashWith SHA256 . 
               BS.append rnd $ (BS.pack . show $ crd))


{-  hash (rnd || card) == commitment -}
verifyCommitment :: Rand -> Card -> Commitment -> Bool
verifyCommitment (RByte rnd) crd (CByte commit) = 
  commit == (BS.pack . show . hashWith SHA256 . 
             BS.append rnd $ (BS.pack . show $ crd))     

```

In principal, the high level ides of Pedersen's commitment scheme 
is same as HMAC, but the underlying implementation uses 
discrete logarithm (Ped.hs). 

```
{- Generate commitment.  g^m * h ^ r -}
generateCommitment :: Rand -> Card -> Commitment
generateCommitment (RInt rnd) crd =  ret where
  ret = CInt (mod ((expFast g (toInteger . fromEnum $ crd) p) *
                   (expFast h rnd p)) p) 


{- verify the commitment-}
verifyCommitment :: Rand -> Card -> Commitment -> Bool
verifyCommitment (RInt rnd) crd (CInt commit) = 
  commit == mod ((expFast g (toInteger . fromEnum $ crd) p) *  
                 (expFast h rnd p)) p 
```

The securit of Pedersen's commitment rely on hardness of computing discrete 
logarithm. To avoid the commonly used attacks on discrete log problem, I have
used [Schnorr group](https://en.wikipedia.org/wiki/Schnorr_group) (see the file 
Pedpubparameter.hs). I have taken the primes from IACR 2018 election. The reason
for choosing these primes is that they produce Schnorr group which avoids 
common attacks on discrete logarithm problem. Also, I have machine 
checked certificate 
about the primality of these numbers (see primeP.v and primeQ.v).

#####A bit of digression (Feel free to skip it)####
To be honest, I did not produce these certificate for this assignment, 
but it's part of ongoing 
project about formal verification of [sigma protocol](https://en.wikipedia.org/wiki/Proof_of_knowledge#Sigma_protocols) in Coq theorem prover with a fellow 
cryptographer. We are trying to verify Douglas Wikstrom's algorithm on 
[commitment-consistent-shuffling](https://eprint.iacr.org/2011/168.pdf). 
The idea is, any mix-net using his alogrithm 
produces a random permutation matrix of 0 and 1s 
for shuffling purpose. The mix-net commits to this matrix using 
vector Pedersen commitment and publishes the commitment (hiding step).
However, for binding step rather than opening the permutation, it produces 
zero-knowledge-proof that it has used the same matrix in shuffling 
which it committed to. I have used a very similar idea while designing
a protocol to verify the validity of ballots in Homomorphic-Schulze method. 
You can see my paper in the home directory of this project (This paper is 
under review, so please don't distribute it).

#####end  of digression ####


I have defined the game rules in file ```Gamerules.hs```

```
{- 
 Queen beats King. 
 Same cards are Draw 
-}
playGame :: Card -> Card -> Outcome
playGame King Queen = Loss
playGame Queen King = Win
playGame _ _ = Draw
```


```Datatypes.hs``` encapsulates different algebraic data types used in this
project. 

```
data Card = King | Queen
  deriving (Eq, Enum, Show, Read)

data Player = Alice | Bob
  deriving Show

data Outcome = Win | Loss | Draw 
  deriving Show

data Rand = RByte ByteString 
          | RInt Integer 
          deriving Show

data Commitment = 
    CByte ByteString 
  | CInt Integer
  deriving Show

{- What if we have more that two players ?. In that case,
   we can assign a value to each player. This value might come
   from database or assign them when they join the game.
   First player is Player 0
   Second player is Player 1
   Data type for multiplayer. -}

data Mplayer where
  Mplayer :: Int -> Mplayer

deriving instance Show Mplayer
{- In Mplayer, We can give explicit names to players
   by type synonyms.
   type Alice = Mplayer 0
   type Bob = MPlayer 1
-}
```

