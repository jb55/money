{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Money
    ( Money(..)
    , showMoney
    ) where

import Data.Char (isDigit, ord)
import Data.Ratio
import Data.List (findIndex)

newtype Money = Money { getMoney :: Rational }
  deriving (Num, Ord, Eq, Fractional)

instance Show Money where
  show money = showMoney Nothing money

instance Read Money where
  readsPrec _ str = [ (Money (readRational str), "") ]

-- https://stackoverflow.com/a/30979717/10486
showMoney :: Maybe Int -> Money -> String
showMoney (Just n) (Money r) =
    let d = round (abs r * 10^n)
        s = show (d :: Integer)
        s' = replicate (n - length s + 1) '0' ++ s
        (h, f) = splitAt (length s' - n) s'
    in  (if r < 0 then "-" else "") ++ h ++ "." ++ f
-- The length of the repeating digits is related to the totient function of the
-- denominator. This means that the complexity of computing them is at least as
-- bad as factoring, i.e., it quickly becomes infeasible.
showMoney Nothing (Money r) =
  let (i, f) = properFraction (abs r) :: (Integer, Rational)
      si = if r < 0 then "-" ++ show i else show i
      decimals f = loop f [] ""
      loop x fs ds =
        if x == 0 then ds
        else
          case findIndex (x ==) fs of
            Just i  -> let (l, r) = splitAt i ds in l ++ "(" ++ r ++ ")"
            Nothing -> let (c, f) = properFraction (10 * x) :: (Integer, Rational)
                       in loop f (fs ++ [x]) (ds ++ show c)
  in if f == 0 then si else si ++ "." ++ decimals f


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
