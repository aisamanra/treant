{-# LANGUAGE TypeFamilies #-}

module Treant where

import qualified System.Random as Random

-- * The relevant types

newtype Size = Size { fromSize :: Int } deriving (Eq, Show)

data Gen r = Gen
  { fromGen :: Size -> Random.StdGen -> (r, Random.StdGen)
  }

runGen :: Gen r -> IO r
runGen (Gen k) = do
  gen <- Random.getStdGen
  let (r, gen') = k (Size 0) gen
  Random.setStdGen gen'
  pure r

instance Functor Gen where
  fmap f (Gen k) = Gen $ \ size gen ->
    let (r, gen') = k size gen
    in (f r, gen')

instance Applicative Gen where
  pure r = Gen $ \_ gen -> (r, gen)
  Gen f <*> Gen x = Gen $ \size gen ->
    let (f', gen' ) = f size gen
        (x', gen'') = x size gen'
    in (f' x', gen'')

instance Monad Gen where
  Gen x >>= f = Gen $ \size gen ->
    let (x', gen') = x size gen
    in fromGen (f x') size gen'


class Distribution dist where
  type Item dist
  generate :: dist -> Gen (Item dist)

-- * Typical mathy distributions

data Normal r = Normal
  { normalMu    :: r
  , normalSigma :: r
  } deriving (Eq, Show)

instance Floating r => Distribution (Normal r) where
  type Item (Normal r) = r
  generate Normal {} = error "unimplemented"

data Cauchy r = Cauchy
  { cauchyX     :: r
  , cauchyGamma :: r
  } deriving (Eq, Show)

instance Floating r => Distribution (Cauchy r) where
  type Item (Cauchy r) = r
  generate Cauchy {} = error "unimplemented"

-- * Categorical distribution

newtype Categorical a = Categorical
  { categoricalItems :: [a]
  } deriving (Eq, Show)

instance Distribution (Categorical r) where
  type Item (Categorical r) = r
  generate (Categorical rs) = do
    let len = length rs
    index <- int
    pure (rs !! (index `mod` len))

-- * Weighted distribution

newtype Weighted n a = Weighted
  { weightedItems :: [(n, a)]
  } deriving (Eq, Show)

instance Integral n => Distribution (Weighted n r) where
  type Item (Weighted n r) = r
  generate (Weighted rs) = do
    let bound = sum (map fst rs)
    choice <- int
    let idx = fromIntegral choice `mod` bound
        findChoice _ [] = error "internal error"
        findChoice n ((p, x):xs)
          | n < p = x
          | otherwise = findChoice (n-p) xs
    pure (findChoice idx rs)

-- * Dirac distribution

newtype Dirac r = Dirac
  { diracConstant :: r
  } deriving (Eq, Show)

instance Distribution (Dirac r) where
  type Item (Dirac r) = r
  generate dist = pure (diracConstant dist)


-- * Some typical generator functions

random :: Random.Random r => Gen r
random = Gen $ \size gen -> Random.random gen

int :: Gen Int
int = random

list :: Gen a -> Gen [a]
list = undefined

maybe :: Gen a -> Gen (Maybe a)
maybe = undefined

d :: Int -> Gen Int
d n = generate (Categorical [1..n])

d20 :: Gen Int
d20 = d 20
