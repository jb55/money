
Data.Money
==========

This module is currently being reworked

> module Data.Money ( makePrecise
>                   , makePrecise_
>                   , defaultPrecision
>                   , fm
>                   ) where

> import Data.Word
> import Data.Decimal

> defaultPrecision :: Word8
> defaultPrecision = 14

> fm :: RealFrac f => f -> Decimal
> fm = realFracToDecimal defaultPrecision

> makePrecise :: Decimal -> Decimal
> makePrecise = makePrecise_ defaultPrecision

> makePrecise_ :: Word8 -> Decimal -> Decimal
> makePrecise_ precision d
>   | dp < precision = Decimal precision $ dm * (10^(precision - dp))
>   | otherwise      = d 
>   where
>     dp = decimalPlaces d
>     dm = decimalMantissa d
