{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Datatype where
import Data.ByteString as ByteString

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

