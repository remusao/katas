#! /usr/bin/env stack
-- stack --resolver lts-9.17 --install-ghc runghc --package primes

import Data.Numbers.Primes

-- We need only check the primes numbers (-1)
-- There seem to be a symmetry in the values of d + n/d so maybe we need only half?
main :: IO ()
main = do
  print . length $ takeWhile (< 100000000) primes
  print "solve"
