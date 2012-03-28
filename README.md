~~~~ {.sourceCode .literate .haskell}
import Data.Default
~~~~

Data.Money
==========

~~~~ {.sourceCode .literate .haskell}
data Money a = Money !Double a
~~~~

The money type allows you to perform calculations and currency
conversions in a type-safe way. The currency is a part of the type
signature, so adding USD and CAD would result in a type error.

Since `Currency` is a typeclass, you can define your own currency types
with custom exchange rates and it will work just fine.

Instances
---------

~~~~ {.sourceCode .literate .haskell}
instance Eq (Money a) where
  m1 == m2 = raw m1 == raw m2
~~~~

~~~~ {.sourceCode .literate .haskell}
instance (Currency a, HasSign a, Show a) => Num (Money a) where
  (+) = liftMoney (+)
  (*) = liftMoney (*)
  (-) = liftMoney (-)
  negate m = money (cur m) . negate . raw $ m
  abs m    = money (cur m) . abs . raw $ m
  signum m = money (cur m) . signum . raw $ m
  fromInteger i = undefined
~~~~

~~~~ {.sourceCode .literate .haskell}
instance (Currency a, HasSign a, Show a) => Fractional (Money a) where
  (/)  = liftMoney (/)
  recip m = money (cur m) $ recip (raw m)
  fromRational i = error "nope"
~~~~

~~~~ {.sourceCode .literate .haskell}
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

~~~~ {.sourceCode .literate .haskell}
instance Currency USD where
  xrate _ = 1
  toUSD  m = m
  fromUSD _ m = m
~~~~

~~~~ {.sourceCode .literate .haskell}
instance Currency CAD where
  xrate (CAD x) = x
~~~~

~~~~ {.sourceCode .literate .haskell}
instance Currency EUR where
  xrate (EUR x) = x
~~~~

~~~~ {.sourceCode .literate .haskell}
instance Default Sign where
  def = toLeft "$"
~~~~

~~~~ {.sourceCode .literate .haskell}
instance HasSign EUR where
  sign _ = toLeft "€"
~~~~

~~~~ {.sourceCode .literate .haskell}
instance HasSign CAD where
  sign _ = def
~~~~

~~~~ {.sourceCode .literate .haskell}
instance HasSign USD where
  sign _ = def
~~~~

Data
----

A simple currency sign definition, use `toLeft` and `toRight` to
construct these.

~~~~ {.sourceCode .literate .haskell}
data Sign = Sign {
    signSymbol :: String
  , signPos    :: Position
} deriving (Show)
~~~~

Position used for positioning currency signs when printing

~~~~ {.sourceCode .literate .haskell}
data Position = ToLeft | ToRight
              deriving (Show)
~~~~

### Currencies

Since the Currency typeclass uses the US dollar a reference point, we
don't need to define USD to have an `ExchangeRate` in its constructor

~~~~ {.sourceCode .literate .haskell}
data USD = USD
         deriving (Show, Eq)
~~~~

Canadian Dollar

~~~~ {.sourceCode .literate .haskell}
data CAD = CAD ExchangeRate
         deriving (Show, Eq)
~~~~

Euro

~~~~ {.sourceCode .literate .haskell}
data EUR = EUR ExchangeRate
         deriving (Show, Eq)
~~~~

Constructors
------------

~~~~ {.sourceCode .literate .haskell}
toRight :: String -> Sign
toRight = flip Sign ToLeft
~~~~

~~~~ {.sourceCode .literate .haskell}
toLeft :: String -> Sign
toLeft = flip Sign ToLeft
~~~~

~~~~ {.sourceCode .literate .haskell}
type ExchangeRate = Double
~~~~

~~~~ {.sourceCode .literate .haskell}
money :: Currency a => a -> Double -> Money a
money = flip Money
~~~~

~~~~ {.sourceCode .literate .haskell}
round' n places = round (n / fromIntegral factor) * factor
  where factor = 10 ^ (places - 1)
~~~~

~~~~ {.sourceCode .literate .haskell}
dbl :: Money a -> Double
dbl m
  | c == 0    = 0.0
  | otherwise = (fromIntegral (round $ c * 100)) / 100
  where
    c = raw m
~~~~

~~~~ {.sourceCode .literate .haskell}
usd :: Double -> Money USD
usd = money USD
~~~~

~~~~ {.sourceCode .literate .haskell}
cur :: Money a -> a
cur (Money _ c) = c
~~~~

~~~~ {.sourceCode .literate .haskell}
raw :: Money a -> Double
raw (Money i _) = i
~~~~

~~~~ {.sourceCode .literate .haskell}
--  liftMoneyX :: (Currency a, Currency b, Num n) => (n -> n -> n) -> Money a -> Money b -> Money a
--  liftMoneyX f a b = let ca = raw $ toUSD a
--                         cb = raw $ toUSD b
--                     in fromUSD $ usdc $ round (fromIntegral ca `f` fromIntegral cb)
~~~~

~~~~ {.sourceCode .literate .haskell}
liftMoney :: (Currency a) => (Double -> Double -> Double) -> Money a -> Money a -> Money a
liftMoney f a b = money (cur a) (raw a `f` raw b)
~~~~

~~~~ {.sourceCode .literate .haskell}
class HasSign a where
  sign :: a -> Sign
~~~~

~~~~ {.sourceCode .literate .haskell}
class Currency a where
  xrate   :: a -> ExchangeRate
  toUSD   :: Money a -> Money USD
  fromUSD :: a -> Money USD -> Money a
  conv    :: Currency b => Money a -> b -> Money b
~~~~

~~~~ {.sourceCode .literate .haskell}
  a `conv` b = fromUSD b $ toUSD a
  toUSD m = toRate (cur m) USD m
  fromUSD = fromRate USD
~~~~

~~~~ {.sourceCode .literate .haskell}
toRate :: (Currency a, Currency b) => a -> b -> Money a -> Money b
toRate a b m = let c    = raw m
                   rate = xrate a
               in money b $ c * rate
~~~~

~~~~ {.sourceCode .literate .haskell}
fromRate :: (Currency a, Currency b) => a -> b -> Money a -> Money b
fromRate a b m = let c    = raw m
                     rate = recip $ xrate b
                 in money b $ c * rate
~~~~

~~~~ {.sourceCode .literate .haskell}
eur' :: ExchangeRate -> Double -> Money EUR
eur' x = money (EUR x)
~~~~

~~~~ {.sourceCode .literate .haskell}
cad' :: ExchangeRate -> Double -> Money CAD
cad' x = money (CAD x)
~~~~
