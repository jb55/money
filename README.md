Data.Money
==========

```haskell
module Data.Money ( Currency(..)
                  , leftSign
                  , rightSign
                  ) where
```

```haskell
import Data.Default
import Data.Decimal
import GHC.Word (Word8)
```

```haskell
data Money a = Money !Decimal a
```

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

```haskell
cadC :: CAD
cadC = CAD 1.0017
```

```haskell
eurC :: EUR
eurC = EUR 1.3289
```

Helper function for dealing with Canadian/Euros

```haskell
eur :: Decimal -> Money EUR
eur = money eurC
```

```haskell
cad :: Decimal -> Money CAD
cad = money cadC
```

Convert 2000 CAD to EUR

```haskell
ex1 :: Money EUR
ex1 = cad (fm 2000) `to` eurC
```

Convert any money type to USD

```haskell
ex2 :: Money USD
ex2 = toUSD ex1
```

Classes
-------

Minimal complete definition: only `xrate` needs to be defined

```haskell
class Currency a where
  xrate   :: a -> ExchangeRate
  toUSD   :: Money a -> Money USD
  fromUSD :: a -> Money USD -> Money a
  to    :: Currency b => Money a -> b -> Money b
```

```haskell
  a `to` b = fromUSD b $ toUSD a
  toUSD m = toRate (cur m) USD m
  fromUSD = fromRate USD
```

Currencies with an HasSign instance can be `Show`n

```haskell
class HasSign a where
  sign :: a -> Sign
```

Instances
---------

```haskell
instance Eq (Money a) where
  m1 == m2 = raw m1 == raw m2
```

```haskell
instance (Currency a, HasSign a, Show a) => Num (Money a) where
  (+) = liftMoney (+)
  (*) = liftMoney (*)
  (-) = liftMoney (-)
  negate m = money (cur m) . negate . raw $ m
  abs m    = money (cur m) . abs . raw $ m
  signum m = money (cur m) . signum . raw $ m
  fromInteger i = undefined
```

```haskell
--instance (Currency a, HasSign a, Show a) => Fractional (Money a) where
--  (/)  = liftMoney (/)
--  recip m = money (cur m) $ recip (raw m)
--  fromRational i = error "nope"
```

```haskell
instance (HasSign a, Show a) => Show (Money a) where
  show m = let s = sign (cur m)
               c = roundTo 2 $ raw m
               c' = ' ':(show $ cur m)
               negWrap s
                 | c < 0 = "(" ++ s ++ ")"
                 | otherwise = s
           in case signPos s of
             ToLeft  -> negWrap $ signSymbol s ++ show c ++ c'
             ToRight -> negWrap $ show c ++ signSymbol s ++ c'
```

```haskell
instance Currency USD where
  xrate _ = 1
  toUSD  m = m
  fromUSD _ m = m
```

```haskell
instance Currency CAD where
  xrate (CAD x) = x
```

```haskell
instance Currency EUR where
  xrate (EUR x) = x
```

```haskell
instance Default Sign where
  def = leftSign "$"
```

```haskell
instance HasSign EUR where
  sign _ = leftSign "€"
```

```haskell
instance HasSign CAD where
  sign _ = def
```

```haskell
instance HasSign USD where
  sign _ = def
```

Data
----

A simple currency sign definition, use `leftSign` and `rightSign` to
construct these.

```haskell
data Sign = Sign {
    signSymbol :: String
  , signPos    :: Position
} deriving (Show)
```

Position used for positioning currency signs when printing

```haskell
data Position = ToLeft | ToRight
              deriving (Show)
```

Currencies
----------

Since the Currency typeclass uses the US dollar a reference point, we
don't need to define USD to have an `ExchangeRate` in its constructor

```haskell
data USD = USD
         deriving (Show, Eq)
```

Canadian Dollar

```haskell
data CAD = CAD ExchangeRate
         deriving (Show, Eq)
```

Euro

```haskell
data EUR = EUR ExchangeRate
         deriving (Show, Eq)
```

Misc
----

```haskell
type ExchangeRate = Double
type Dollars = Int
type Cents = Int
```

```haskell
precision :: Word8
precision = 6
```

```haskell
fm :: RealFrac f => f -> Decimal
fm = realFracToDecimal precision
```

```haskell
rightSign :: String -> Sign
rightSign = flip Sign ToLeft
```

```haskell
leftSign :: String -> Sign
leftSign = flip Sign ToLeft
```

```haskell
makePrecise :: Decimal -> Decimal
makePrecise d
  | dp < precision = Decimal precision $ dm * (10^(precision - dp))
  | otherwise      = d 
  where
    dp = decimalPlaces d
    dm = decimalMantissa d
```

```haskell
money :: (Currency a) => a -> Decimal -> Money a
money c d = Money (makePrecise d) c
```

```haskell
round' :: Decimal -> Decimal
round' c = c
```

```haskell
roundMoney :: Money a -> Decimal
roundMoney m = round' $ raw m
```

```haskell
usd :: Decimal -> Money USD
usd = money USD
```

```haskell
cur :: Money a -> a
cur (Money _ c) = c
```

```haskell
raw :: Money a -> Decimal
raw (Money i _) = i
```

```haskell
--  liftMoneyX :: (Currency a, Currency b, Num n) => (n -> n -> n) -> Money a -> Money b -> Money a
--  liftMoneyX f a b = let ca = raw $ toUSD a
--                         cb = raw $ toUSD b
--                     in fromUSD $ usdc $ round (fromIntegral ca `f` fromIntegral cb)
```

```haskell
liftMoney :: (Currency a) => (Decimal -> Decimal -> Decimal) -> Money a -> Money a -> Money a
liftMoney f a b = money (cur a) (raw a `f` raw b)
```

```haskell
toRate :: (Currency a, Currency b) => a -> b -> Money a -> Money b
toRate a b m = let c    = raw m
                   rate = xrate a
               in money b $ c *. rate
```

```haskell
fromRate :: (Currency a, Currency b) => a -> b -> Money a -> Money b
fromRate a b m = let c    = raw m
                     rate = recip $ xrate b
                 in money b $ c *. rate
```
