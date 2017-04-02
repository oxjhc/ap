module Number(
    PrimeField
) where

import Data.Ratio

-- It would be nice to parameterise over this, but that would require the modulus to be part of the type of the numbers, which I can't work out how to do without just using type-level unary number representations, which would be awful for efficiency.
p :: Int
p = 65521

-- Ord is only included for tree purposes. It has no algebraic significance.
newtype PrimeField = PF {unPF :: Int} deriving (Eq, Ord)

instance Num PrimeField where
    (PF a) + (PF b) = PF $ mod (a + b) p
    (PF a) - (PF b) = PF $ mod (a - b) p
    (PF a) * (PF b) = PF $ mod (a * b) p
    abs a = a -- abs doesn't make sense at all for finite fields.
    signum _ = 1
    fromInteger n = PF $ mod (fromInteger n) p

instance Fractional PrimeField where
    -- the fromRational definition doesn't entirely make sense but apparently it's compulsory.
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
    recip (PF a) = if u == 1 then PF i else error "divide by zero"
        where (u, _, i) = bézout p a

bézout :: Integral n => n -> n -> (n, n, n)
bézout x y -- = (u, a, b) where u = a*x + b*y, assuming x > y
  | y == 0    = (x, 1, 0)
  | otherwise = (u, b, a-q*b)
      where (q, r) = divMod x y
            (u, a, b) = bézout y r

instance Show PrimeField where
    show (PF n) = show $ mod (n+o) p - o
        where o = div p 2

instance Enum PrimeField where
    fromEnum = unPF
    toEnum   = PF . (flip mod p)
