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
can brute force all the possible bits (2 ^ n where n is length of byte) and 
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

In Pedersen's commitment scheme, the high level idea
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

##### A bit of digression (Feel free to skip it) ####
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

##### end  of digression ####


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
project.  I have assumed just two cards, two players, and three outcome of 
game. ```Rand``` abstracts randomess used in HMAC (```RByte ByteString```)
and Pedersen's commitment (```RInt Integer```). Similar abstractions are 
used for commitment (```Commitment```).

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

Finally, ```Main.hs``` combines the game logic.  ```Alice``` moves first, and 
```Bob``` second.  Each move produces three things:

1. Card 
2. Randomness
3. Commitment

After each move, a player would announce his/her commitment by publishing 
it to bulletin board (Blockchain) or sending it to his/her opponent. Once 
round finishes, every one opens his/her card and randomess for verification. If 
every player involve in the game is honest, then we proceed normally and 
declare a winner. However, if someone try to cheat, then depending on rules 
of game we take action (may be expelling and restarting the game again with
remaining players). In my two player game, if one person tries to cheat, then 
I declare other person as winner. 

```
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
```                 




A execution of game:

```

keep-learnings-MacBook-Pro:Twoplayergame keep_learning$ stack clean
keep-learnings-MacBook-Pro:Twoplayergame keep_learning$ stack build
Twoplayergame-0.1.0.0: unregistering (local file changes: ChangeLog.md README.md Twoplayergame.cabal app/Main.hs package.yaml src/Datatype.hs src/Gamerules...)
Building all executables for `Twoplayergame' once. After a successful build of all of them, only specified executables will be rebuilt.
Twoplayergame-0.1.0.0: configure (lib + exe)
Configuring Twoplayergame-0.1.0.0...
Twoplayergame-0.1.0.0: build (lib + exe)
Preprocessing library for Twoplayergame-0.1.0.0..
Building library for Twoplayergame-0.1.0.0..
keep-learnings-MacBook-Pro:Twoplayergame keep_learning$ stack exec Twoplayergame-exe
Alice on move:
King
I have played my card, and my commitment is: CInt 4759346955005880837461905990600817923550474659780717811575557916635005613181457085537979670578127038559920621947812696282391761397256463921963991895306065974797541274929987104780479170181884501541136584412678564004972409449839114542205175001446297776133187537956445424474272406622690642855390886221101909712211446516116706392844087163643461375361199672197092709275375287187959462657384245490559568127772311711860597139546688916699376240585220039834877585368890191452036854142855070890664052111578448678313146994140544856842554439114105531120099614701878182284309453445777272331842927286069187820308351200304330228427
Bob on move:
Queen
I have played my card, and my commitment is: CInt 10586573943992642187558293604812625918417139157189036659478803063425824173923155283892263624560767721766546545275902254586440422729009080868857648436113786041942618377881480285285380995148694109517297300152138819588368084823816813018446668723545240102447791374085658726630400487251127041377402889228147284185446719556206078006578046643466312134697542645365392929151936348482889822235195992706170687114590858617765085844627805866001367017182561402778110200973355274655747451906597213328183337225722220761579536992727312887569852465767387220337566075256881971394410384168837249186544084697158082771940776658622056999501
Verification Phase
Bob: what was your card Alice?(Message to user: Try to cheat here)
Queen
Bob => Alice: Your commitment does not match with your answer! You cheated.  I am winner
```

When we execute ```stack exec Twoplayergame-exe```, it immediately prompts
```Alice on move``` and waits for user(Alice) to input her move from 
standard input (```King```  or 
```Queen```. It's case sensitive, because ```Card``` data type has 
read instance, 
so ```read``` turns string into ```Card``` data type). 
Once Alice gives her input, it produces the message 
```I have played my card, and my commitment is: ``` with some 
random interger (if you are running Pedersen's commitment) or random byte 
(hash version). Next, it prompts ```Bob on move:``` and waits for his input. 
It produces the similar message like it produced for Alice. After Bob's move,
 the program enters into verification phase. In verification phase, 
 Bob asks Alice about her card by displaying the message ``` Bob: what 
 was your card Alice?(Message to user: Try to cheat here)```. 
 At this point, 
 program waits for Alice to give her card from standard input. The reason 
 for choosing the standard input is, giving Alice opportunity to cheat. She 
 can change her mind and give a different than what she committed. If she
 cheats then Bob immediately detects it, and declare himself winner. If 
 Alice is honest, then she asks Bob about his card. If Bob is cheating, 
 then she declares herself winner, otherwise game proceeds normally, and 
 declares winner based on game rule. 
 Couple of more runs. 
 
```
Bob is cheater cock :)
keep-learnings-MacBook-Pro:Twoplayergame keep_learning$ stack exec Twoplayergame-exe
Alice on move:
Queen
I have played my card, and my commitment is: CInt 16114073709323729762389214992268874781804700324826475864157218022083436299652181994417672306447690345062326044187372552481042245763908043030175370678313660453500797516975631221739016524773511355284594622912146801675947793969263423608307640645911284679386072649654665205522655498632862231632515243227288021795102652557941318310620945117904553580298012682906498274112853810255321397720315170632484279563860205182161008159953156832201710110624646889770224302038518984585329398125690046107449698327169518909250713825301097600407166315213069847022676125558957382983023139075473579254507790931026291250101825062372747374631
Bob on move:
King
I have played my card, and my commitment is: CInt 15876487508547651119387730145300210288609839998493091080347025958697189456397754377169451446889588236800327556362523627259872341560692396715556234583194529283554634706548140323498826413709209784028933898910218635453206254930200679072328850994539832466804184831469996732604272365882365905650789076952902764553218682950397198317331158031755863541818127112220305025051160163912263363941024786989285665134525670746776837256667303301592946939920128099925454345081039556392027011933946626008666530103380305844728588215961639216396708068730300842866581537548293737213092266285783214761608827860571044473100492083197502858811
Verification Phase
Bob: what was your card Alice?(Message to user: Try to cheat here)
Queen
Alice: what was your card Bob?(Message to user: Try to cheat here)
Queen
Alice => Bob: Your commitment does not match with your answer! You are  cheater cock ;). I am winner 


Everyone is honest in this game.
keep-learnings-MacBook-Pro:Twoplayergame keep_learning$ stack exec Twoplayergame-exe
Alice on move:
Queen
I have played my card, and my commitment is: CInt 15114880518482090610183682629647708451041013414336434732331055078600824570841772730326543635727513098189302720955919628728123140800240326125645961510128460435466133717100607422159134071881743552719713679374146087904516443444041949938667332931887443186890246427479933256450802813299522224707543639528248253929554251465260144962389180636932793980071710499935419696688220035237462704986352484858096135060424833136381474792325088557422920414383512225070049200471363566310840881346499432231996152424804958263312987548399272443440362956902223144503520915465740143280565056861381510078904395367745362644547542374972497484628
Bob on move:
King
I have played my card, and my commitment is: CInt 3750018543733138708429805298167668251236572445938769960444702785029273423059665880510376334552001622932101765474313828246958340588818654596981188002803156951560333318421710178516389428454420564830812145778267487434808351131891436691015457146780023283947240119308388772209793240934027561325614120930851436230049981582299600822766806484475955206416092982129106545559682695659753770318665998026162705806728142355004306343670331604779005328507609235109086782967787899589599022423103538921638906095285896846297610423889461030873730067198644785367904749697703495809083749142838066140564252392567672960814996733893754647776
Verification Phase
Bob: what was your card Alice?(Message to user: Try to cheat here)
Queen
Alice: what was your card Bob?(Message to user: Try to cheat here)
King
Alice => Bob: I won the game. Good luck next time.


Hash based verification execution
keep-learnings-MacBook-Pro:app keep_learning$ stack exec Twoplayergame-exe
Alice on move:
King
I have played my card, and my commitment is: CByte "42a2b939d7b4e96fa9e833cd21ef9641221d5fb2eb6980c12e864707f8b065dd"
Bob on move:
Queen
I have played my card, and my commitment is: CByte "9477931f6d74c33e64274c61da0ae06abeadde5c4c072efe7a244db6d255b512"
Verification Phase
Bob: what was your card Alice?(Message to user: Try to cheat here)
King
Alice: what was your card Bob?(Message to user: Try to cheat here)
Queen
Bob => Alice: I won the game. Good luck next time.
```

I believe the language perfect for this problem is Rust sprinkled with
[separation logic](http://plv.mpi-sws.org/rustbelt/), but I don't know 
Rust in much depth, so I went with Haskell. Haskell is excellent lanague, but
in my personal opinion, laziness is not promising situation for cryptographic application. Well, Rust is next language in my to-do list. Please, let me 
know if you have any question. 


Best regards,<br> 
Mukesh Tiwari
