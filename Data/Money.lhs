
Data.Money
==========

This module is currently being reworked

> module Data.Money ( makePrecise
>                   , fm
>                   ) where

> import Data.Decimal


> fm :: RealFrac f => f -> Decimal
> fm = realFracToDecimal precision

> makePrecise :: Decimal -> Decimal
> makePrecise d
>   | dp < precision = Decimal precision $ dm * (10^(precision - dp))
>   | otherwise      = d 
>   where
>     dp = decimalPlaces d
>     dm = decimalMantissa d
