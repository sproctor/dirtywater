module Roll where

import System.Random

import Types

rollDie :: IO Int
rollDie = getStdRandom (randomR (1, 6))

rollDice :: Int -> IO Int
rollDice 0 = return 0
rollDice nDice = do
  d <- rollDie
  rest <- rollDice (nDice - 1)
  return (d + rest)

rollDamage :: AttackType -> Int -> IO Int
rollDamage attackStyle striking = do
  let (nDice, modifier) = getStrikingDice attackStyle striking
  damage <- fmap ((+) modifier) (rollDice nDice)
  if damage < 0
    then return 0
    else return damage

getStrikingDice :: AttackType -> Int -> (Int, Int)
getStrikingDice Thrust = getThrustDice
getStrikingDice Swing = getSwingDice

getThrustDice :: Int -> (Int, Int)
getThrustDice 1 = (1, -6)
getThrustDice 2 = (1, -6)
getThrustDice 3 = (1, -5)
getThrustDice 4 = (1, -5)
getThrustDice 5 = (1, -4)
getThrustDice 6 = (1, -4)
getThrustDice 7 = (1, -3)
getThrustDice 8 = (1, -3)
getThrustDice 9 = (1, -2)
getThrustDice 10 = (1, -2)
getThrustDice n = (n `div` 10, 0)

getSwingDice :: Int -> (Int, Int)
getSwingDice 1 = (1, -5)
getSwingDice 2 = (1, -5)
getSwingDice 3 = (1, -4)
getSwingDice 4 = (1, -4)
getSwingDice 5 = (1, -3)
getSwingDice 6 = (1, -3)
getSwingDice 7 = (1, -2)
getSwingDice 8 = (1, -2)
getSwingDice 9 = (1, -1)
getSwingDice 10 = (1, 0)
getSwingDice n = (n `div` 8, 0)
