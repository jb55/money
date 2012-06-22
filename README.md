Data.Money
==========

This module is currently being reworked

```haskell
module Data.Money ( makePrecise
                  , makePrecise_
                  , defaultPrecision
                  , fm
                  ) where
```

```haskell
import Data.Word
import Data.Decimal
```

```haskell
defaultPrecision :: Word8
defaultPrecision = 14
```

```haskell
fm :: RealFrac f => f -> Decimal
fm = realFracToDecimal defaultPrecision
```

```haskell
makePrecise :: Decimal -> Decimal
makePrecise = makePrecise_ defaultPrecision
```

```haskell
makePrecise_ :: Word8 -> Decimal -> Decimal
makePrecise_ precision d
  | dp < precision = Decimal precision $ dm * (10^(precision - dp))
  | otherwise      = d 
  where
    dp = decimalPlaces d
    dm = decimalMantissa d
```
