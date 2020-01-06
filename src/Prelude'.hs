module Prelude' (
  {- old stuff -}
  
    ($!)
  , (!!)
  , ($)
  , (&&)
  , (||)
  , (^)
  , (^^)
  , Bool(..)
  , Bounded(..)
  , Char
  , Double
  , Either(..)
  , Enum(..)
  , Eq(..)
  , FilePath
  , Float
  , Floating(..)
  , Fractional(..)
  , Functor(..)
  , Int
  , IO
  , Integer
  , Integral(..)
  , Maybe(..)
  , Monad(..)
  , Num(..)
  , Ord(..)
  , Ordering(..)
  , Rational
  , Read(..)
  , ReadS
  , Real(..)
  , RealFloat(..)
  , RealFrac(..)
  , Show(..)
  , ShowS
  , String
  , appendFile
  , asTypeOf
  , comparing
  , const
  , curry
  , either
  , error
  , even
  , flip
  , fromIntegral
  , fst
  , gcd
  , getChar
  , getContents
  , getLine
  , id
  , lcm
  , maybe
  , not
  , odd
  , otherwise
  , print
  , putChar
  , putStr
  , putStrLn
  , read
  , readFile
  , reads
  , realToFrac
  , seq
  , showChar
  , showString
  , shows
  , snd
  , subtract
  , uncurry
  , undefined
  , until
  , writeFile

  {- new stuff -}

  ,  if'
  , (.)
  , (++)
  , dot
  , revOrd
  ) where

-- TODO : Check if IO stuff conflicts with importing System.IO
-- Control.Monad ?
-- see if : [] exist

import Prelude hiding ((.), (++))
import qualified Prelude as P

import Data.Ord (comparing)

import Control.Arrow ()
import Data.Monoid

-----------------------------------------------------------

infixr 9 .
infixr 9 `dot`

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

(.) :: Functor f => (a -> b) -> (f a -> f b)
(.) = fmap

(++) :: (Monoid m) => m -> m -> m
(++) = mappend

dot :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
dot = (.).(.)

revOrd :: Ordering -> Ordering
revOrd LT = GT
revOrd GT = LT
revOrd EQ = EQ
