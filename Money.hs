{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Money
    ( Money(..)
    ) where

import Data.Char (isDigit, ord)
import Data.Ratio
import Text.Printf (printf)

newtype Money = Money Rational deriving (Num, Ord, Eq)

instance Show Money where
  show (Money r) = reverse
                 . dropWhile (\c -> c =='0' || c == '.')
                 . reverse
                 . (printf "%.10f" :: Double -> String)
                 . fromRational
                 $ r

instance Read Money where
  readsPrec _ str = [ (Money (readRational str), "") ]

-- from GHC Utils
readRational__ :: ReadS Rational -- NB: doesn't handle leading "-"
readRational__ r = do
     (n,d,s) <- readFix r
     (k,t)   <- readExp s
     return ((n%1)*10^^(k-d), t)
 where
     readFix r_ = do
        (ds,s)  <- lexDecDigits r_
        (ds',t) <- lexDotDigits s
        return (read (ds++ds'), length ds', t)

     readExp (e:s) | e `elem` ("eE" :: String) = readExp' s
     readExp s                     = return (0,s)

     readExp' ('+':s) = readDec s
     readExp' ('-':s) = do (k,t) <- readDec s
                           return (-k,t)
     readExp' s       = readDec s

     readDec s = do
        (ds,r_) <- nonnull isDigit s
        return (foldl1 (\n d -> n * 10 + d) [ ord d - ord '0' | d <- ds ],
                r_)

     lexDecDigits = nonnull isDigit

     lexDotDigits ('.':s) = return (span isDigit s)
     lexDotDigits s       = return ("",s)

     nonnull p s = do (cs@(_:_),t) <- return (span p s)
                      return (cs,t)

readRational :: String -> Rational -- NB: *does* handle a leading "-"
readRational top_s
  = case top_s of
      '-' : xs -> - (read_me xs)
      xs       -> read_me xs
  where
    read_me s
      = case (do { (x,"") <- readRational__ s ; return x }) of
          [x] -> x
          []  -> error ("readRational: no parse:"        ++ top_s)
          _   -> error ("readRational: ambiguous parse:" ++ top_s)
