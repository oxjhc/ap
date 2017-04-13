{-# LANGUAGE DeriveGeneric         #-}

module Vault(
    Vault(..),
    Polynomial(..),
    Key
) where

import qualified Data.Set as Set
import Data.List
import GHC.Generics (Generic)

newtype Vault n = Vault [(n, n)] deriving (Eq, Show, Generic)
newtype Polynomial n = P{unPoly :: [n]}
type Key = [PrimeField]

poly :: n -> Polynomial n
poly n = P [n]

evalPoly :: Num n => Polynomial n -> n -> n
evalPoly (P cs) n = foldr (flip ((+) . (*n))) 0 cs

-- Including this in the Num instance would be slightly awkward as it would mean there would have to be Eq constraints everywhere as well.
trimPoly :: (Num n, Eq n) => Polynomial n -> Polynomial n
trimPoly = P . dropWhileEnd (== 0) . unPoly

polyX :: Num n => Polynomial n
polyX = P [0, 1]

-- I'm sure this will come in handy for debugging
instance (Show n, Num n, Eq n) => Show (Polynomial n) where
    show (P []) = "0"
    show (P ns) = intercalate " + " $ reverse $ map (uncurry showTerm) $ filter ((/=0) . snd) $ zip [0..] ns
        where showTerm :: (Show n, Num n, Eq n) => Int -> n -> String
              showTerm 0 n = show n
              showTerm 1 n = show' n ++ "x"
              showTerm d n = show' n ++ "x^" ++ show d
              show' 1 = ""
              show' (-1) = "-"
              show' n = show n

instance (Num n, Eq n) => Eq (Polynomial n) where
    a == b = unPoly (trimPoly a) == unPoly (trimPoly b)

instance Num n => Num (Polynomial n) where
    (P []) + y = y
    x + (P []) = x
    (P (x:xs)) + (P (y:ys)) = P ((x+y) : unPoly (P xs + P ys))
    
    (P xs) * y = foldr (\x ys' -> addShift (timesScalar x y) ys') (P []) xs
        where timesScalar a (P b) = P (map (*a) b)
              addShift (P []    ) (P b) = P (0:b)
              addShift (P (a:as)) (P b) = P (a: unPoly (P as + P b))
    
    negate (P x) = P (map negate x)
    -- It should be possible to divide by the signum of anything, but as the only bound on the result is that it's a Num, it's impossible to derive a sensible abs for polynomials (i.e. take the abs of the first term and scale everything else by the same amount).
    abs a = a
    signum a = 1
    fromInteger n =  if n == 0 then P [] else P [fromInteger n]

-- Finite fields aren't naturally members of Ord, but it's probably not too hard to provide an instance.
openVault :: (Fractional n, Ord n) => Vault n -> [n] -> Polynomial n
openVault (Vault ps) b = interpolate (filter (flip Set.member b' . fst) ps)
    where b' = Set.fromList b

interpolate :: Fractional n => [(n, n)] -> Polynomial n
interpolate ps = sum $ zipWith3 makeTerm ps factorPrefixes factorSuffixes
    where linearFactors = map (\(x, y) -> polyX - poly x) ps
          factorPrefixes =        scanl (*) 1 linearFactors
          factorSuffixes = tail $ scanr (*) 1 linearFactors
          makeTerm (x, y) pre suf = let otherTerms = pre * suf in otherTerms * poly (y / evalPoly otherTerms x)
