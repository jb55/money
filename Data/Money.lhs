
Data.Money
==========

> module Data.Money ( Currency(..)
>                   , Money
>                   , ExchangeRate
>                   , HasSign(..)
>                   , Sign(..)
>                   , Position(..)
>                   , leftSign
>                   , rightSign
>                   , currency
>                   , money
>                   , USD
>                   , usd
>                   ) where

> import Data.Default
> import Data.Decimal
> import GHC.Word (Word8)

> data Money a = Money !Decimal a

The money type allows you to perform calculations and currency conversions in a
type-safe way. The currency is a part of the type signature, so adding USD and
CAD would result in a type error.

Since `Currency` is a typeclass, you can define your own currency types with
custom exchange rates and it will work just fine.

Examples
--------

Define the Canadian/Euro currency with USD exchange rate

Specifying the exchange rate this way allows us to change the exchange rate on
the fly if needed

> cadC :: CAD
> cadC = CAD 0.997197874

> eurC :: EUR
> eurC = EUR 0.75001875

Helper function for dealing with Canadian/Euros

> eur :: Decimal -> Money EUR
> eur = money eurC

> cad :: Decimal -> Money CAD
> cad = money cadC

Convert 2000 CAD to EUR

> ex1 :: Money EUR
> ex1 = cad 2000 `to` eurC

Convert any money type to USD

> ex2 :: Money USD
> ex2 = toUSD ex1

Classes
-------

Minimal complete definition: only `xrate` needs to be defined

> class Currency a where
>   xrate   :: a -> ExchangeRate
>   toUSD   :: Money a -> Money USD
>   fromUSD :: a -> Money USD -> Money a
>   to    :: Currency b => Money a -> b -> Money b

>   a `to` b = fromUSD b $ toUSD a
>   toUSD m = toRate (currency m) USD m
>   fromUSD = fromRate USD

Currencies with an HasSign instance can be `Show`n

> class HasSign a where
>   sign :: a -> Sign
>   sign = leftSign "$"


Instances
---------

> instance Eq (Money a) where
>   m1 == m2 = raw m1 == raw m2

> instance (Currency a, HasSign a, Show a) => Num (Money a) where
>   (+) = liftMoney (+)
>   (*) = liftMoney (*)
>   (-) = liftMoney (-)
>   negate m = money (currency m) . negate . raw $ m
>   abs m    = money (currency m) . abs . raw $ m
>   signum m = money (currency m) . signum . raw $ m
>   fromInteger = undefined 

> --instance (Currency a, HasSign a, Show a) => Fractional (Money a) where
> --  (/)  = liftMoney (/)
> --  recip m = money (currency m) $ recip (raw m)
> --  fromRational i = error "nope"

> instance (HasSign a, Show a, Currency a) => Show (Money a) where
>   show m = let s = sign (currency m)
>                c = roundTo 2 $ raw m
>                c' = ' ':(show $ currency m)
>                showc c'
>                  | c' < 0    = show $ negate c'
>                  | otherwise = show c'
>                negWrap s
>                  | c < 0     = "(" ++ s ++ ")"
>                  | otherwise = s
>            in case signPos s of
>              ToLeft  -> negWrap $ signSymbol s ++ showc c ++ c'
>              ToRight -> negWrap $ showc c ++ signSymbol s ++ c'

> instance Currency USD where
>   xrate _ = 1
>   toUSD  m = m
>   fromUSD _ m = m

> instance Currency CAD where
>   xrate (CAD x) = x

> instance Currency EUR where
>   xrate (EUR x) = x

> instance Default Sign where
>   def = leftSign "$"

> instance HasSign EUR where
>   sign _ = leftSign "€"

> instance HasSign CAD where
>   sign _ = def

> instance HasSign USD where
>   sign _ = def

Data
----

A simple currency sign definition, use `leftSign` and `rightSign` to construct
these.

> data Sign = Sign {
>     signSymbol :: String
>   , signPos    :: Position
> } deriving (Show)

Position used for positioning currency signs when printing

> data Position = ToLeft | ToRight
>               deriving (Show)

Currencies
----------

Since the Currency typeclass uses the US dollar a reference point, we don't need
to define USD to have an `ExchangeRate` in its constructor

> data USD = USD
>          deriving (Show, Eq)

Canadian Dollar

> data CAD = CAD ExchangeRate
>          deriving (Show, Eq)

Euro

> data EUR = EUR ExchangeRate
>          deriving (Show, Eq)


Misc
----

> type ExchangeRate = Double
> type Dollars = Int
> type Cents = Int

> precision :: Word8
> precision = 6

> fm :: RealFrac f => f -> Decimal
> fm = realFracToDecimal precision

> rightSign :: String -> Sign
> rightSign = flip Sign ToLeft

> leftSign :: String -> Sign
> leftSign = flip Sign ToLeft

> makePrecise :: Decimal -> Decimal
> makePrecise d
>   | dp < precision = Decimal precision $ dm * (10^(precision - dp))
>   | otherwise      = d 
>   where
>     dp = decimalPlaces d
>     dm = decimalMantissa d

> money :: (Currency a) => a -> Decimal -> Money a
> money c d = Money (makePrecise d) c

> round' :: Decimal -> Decimal
> round' c = c

> roundMoney :: Money a -> Decimal
> roundMoney m = round' $ raw m

> usd :: Decimal -> Money USD
> usd = money USD

> currency :: Currency a => Money a -> a
> currency (Money _ c) = c

> raw :: Money a -> Decimal
> raw (Money i _) = i

> --  liftMoneyX :: (Currency a, Currency b, Num n) => (n -> n -> n) -> Money a -> Money b -> Money a
> --  liftMoneyX f a b = let ca = raw $ toUSD a
> --                         cb = raw $ toUSD b
> --                     in fromUSD $ usdc $ round (fromIntegral ca `f` fromIntegral cb)

> liftMoney :: (Currency a) => (Decimal -> Decimal -> Decimal) -> Money a -> Money a -> Money a
> liftMoney f a b = money (currency a) (raw a `f` raw b)

> fromRate :: (Currency a, Currency b) => a -> b -> Money a -> Money b
> fromRate a b m = let c    = raw m
>                      rate = xrate a
>                  in money b $ c *. rate

> toRate :: (Currency a, Currency b) => a -> b -> Money a -> Money b
> toRate a b m = let c    = raw m
>                    rate = recip $ xrate b
>                in money b $ c *. rate
