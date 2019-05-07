module Main where
import Lib

tossCoin :: IO Int
tossCoin = do 
  return 1 

main :: IO ()
main = do
  t <- tossCoin () -- Decide who goes first.
   
