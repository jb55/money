~~~~ {.haskell}
module Data.Money () where
~~~~

~~~~ {.haskell}
import Data.Default
~~~~

Data.Money
==========

~~~~ {.haskell}
data Money a = Money !Double a
~~~~

The money type allows you to perform calculations and currency
conversions in a type-safe way. The currency is a part of the type
signature, so adding USD and CAD would result in a type error.

Since `Currency` is a typeclass, you can define your own currency types
with custom exchange rates and it will work just fine.

Examples
--------

Define the Canadian/Euro currency with USD exchange rate

Specifying the exchange rate this way allows us to change the exchange
rate on the fly if needed

~~~~ {.haskell}
cadC :: CAD
cadC = CAD 1.00361
~~~~

~~~~ {.haskell}
eurC :: EUR
eurC = EUR 1.3339
~~~~

Helper function for dealing with Canadian/Euros

~~~~ {.haskell}
eur :: Double -> Money EUR
eur = money eurC
~~~~

~~~~ {.haskell}
cad :: Double -> Money CAD
cad = money cadC
~~~~

Convert 2000 CAD to EUR

~~~~ {.haskell}
ex1 :: Money EUR
ex1 = cad 2000 `to` eurC
~~~~

Convert any money type to USD

~~~~ {.haskell}
ex2 :: Money USD
ex2 = toUSD ex1
~~~~

Instances
---------

~~~~ {.haskell}
instance Eq (Money a) where
  m1 == m2 = raw m1 == raw m2
~~~~

~~~~ {.haskell}
instance (Currency a, HasSign a, Show a) => Num (Money a) where
  (+) = liftMoney (+)
  (*) = liftMoney (*)
  (-) = liftMoney (-)
  negate m = money (cur m) . negate . raw $ m
  abs m    = money (cur m) . abs . raw $ m
  signum m = money (cur m) . signum . raw $ m
  fromInteger i = undefined
~~~~

~~~~ {.haskell}
instance (Currency a, HasSign a, Show a) => Fractional (Money a) where
  (/)  = liftMoney (/)
  recip m = money (cur m) $ recip (raw m)
  fromRational i = error "nope"
~~~~

~~~~ {.haskell}
instance (HasSign a, Show a) => Show (Money a) where
  show m = let s = sign (cur m)
               c = dbl m
               c' = ' ':(show $ cur m)
               negWrap s
                 | raw m < 0 = "(" ++ s ++ ")"
                 | otherwise = s
           in case signPos s of
             ToLeft  -> negWrap $ signSymbol s ++ show c ++ c'
             ToRight -> negWrap $ show c ++ signSymbol s ++ c'
~~~~

~~~~ {.haskell}
instance Currency USD where
  xrate _ = 1
  toUSD  m = m
  fromUSD _ m = m
~~~~

~~~~ {.haskell}
instance Currency CAD where
  xrate (CAD x) = x
~~~~

~~~~ {.haskell}
instance Currency EUR where
  xrate (EUR x) = x
~~~~

~~~~ {.haskell}
instance Default Sign where
  def = toLeft "$"
~~~~

~~~~ {.haskell}
instance HasSign EUR where
  sign _ = toLeft "€"
~~~~

~~~~ {.haskell}
instance HasSign CAD where
  sign _ = def
~~~~

~~~~ {.haskell}
instance HasSign USD where
  sign _ = def
~~~~

Data
----

A simple currency sign definition, use `toLeft` and `toRight` to
construct these.

~~~~ {.haskell}
data Sign = Sign {
    signSymbol :: String
  , signPos    :: Position
} deriving (Show)
~~~~

Position used for positioning currency signs when printing

~~~~ {.haskell}
data Position = ToLeft | ToRight
              deriving (Show)
~~~~

Currencies
----------

Since the Currency typeclass uses the US dollar a reference point, we
don't need to define USD to have an `ExchangeRate` in its constructor

~~~~ {.haskell}
data USD = USD
         deriving (Show, Eq)
~~~~

Canadian Dollar

~~~~ {.haskell}
data CAD = CAD ExchangeRate
         deriving (Show, Eq)
~~~~

Euro

~~~~ {.haskell}
data EUR = EUR ExchangeRate
         deriving (Show, Eq)
~~~~

Constructors
------------

~~~~ {.haskell}
toRight :: String -> Sign
toRight = flip Sign ToLeft
~~~~

~~~~ {.haskell}
toLeft :: String -> Sign
toLeft = flip Sign ToLeft
~~~~

~~~~ {.haskell}
type ExchangeRate = Double
~~~~

~~~~ {.haskell}
money :: Currency a => a -> Double -> Money a
money = flip Money
~~~~

~~~~ {.haskell}
round' n places = round (n / fromIntegral factor) * factor
  where factor = 10 ^ (places - 1)
~~~~

~~~~ {.haskell}
dbl :: Money a -> Double
dbl m
  | c == 0    = 0.0
  | otherwise = (fromIntegral (round $ c * 100)) / 100
  where
    c = raw m
~~~~

~~~~ {.haskell}
usd :: Double -> Money USD
usd = money USD
~~~~

~~~~ {.haskell}
cur :: Money a -> a
cur (Money _ c) = c
~~~~

~~~~ {.haskell}
raw :: Money a -> Double
raw (Money i _) = i
~~~~

~~~~ {.haskell}
--  liftMoneyX :: (Currency a, Currency b, Num n) => (n -> n -> n) -> Money a -> Money b -> Money a
--  liftMoneyX f a b = let ca = raw $ toUSD a
--                         cb = raw $ toUSD b
--                     in fromUSD $ usdc $ round (fromIntegral ca `f` fromIntegral cb)
~~~~

~~~~ {.haskell}
liftMoney :: (Currency a) => (Double -> Double -> Double) -> Money a -> Money a -> Money a
liftMoney f a b = money (cur a) (raw a `f` raw b)
~~~~

~~~~ {.haskell}
class HasSign a where
  sign :: a -> Sign
~~~~

~~~~ {.haskell}
class Currency a where
  xrate   :: a -> ExchangeRate
  toUSD   :: Money a -> Money USD
  fromUSD :: a -> Money USD -> Money a
  to    :: Currency b => Money a -> b -> Money b
~~~~

~~~~ {.haskell}
  a `to` b = fromUSD b $ toUSD a
  toUSD m = toRate (cur m) USD m
  fromUSD = fromRate USD
~~~~

~~~~ {.haskell}
toRate :: (Currency a, Currency b) => a -> b -> Money a -> Money b
toRate a b m = let c    = raw m
                   rate = xrate a
               in money b $ c * rate
~~~~

~~~~ {.haskell}
fromRate :: (Currency a, Currency b) => a -> b -> Money a -> Money b
fromRate a b m = let c    = raw m
                     rate = recip $ xrate b
                 in money b $ c * rate
~~~~
