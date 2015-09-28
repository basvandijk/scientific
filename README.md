`Data.Scientific` provides a space efficient and arbitrary precision
scientific number type.

`Scientific` numbers are represented using
[scientific notation](http://en.wikipedia.org/wiki/Scientific_notation). It uses
a coefficient `c :: Integer` and a base-10 exponent `e :: Int` (do note that
since we're using an 'Int' to represent the exponent these numbers aren't truly
arbitrary precision. I intend to change this to Integer in the future). A
scientific number corresponds to the 'Fractional' number:
`fromInteger c * 10 ^^ e`.

The main application of `Scientific` is to be used as the target of parsing
arbitrary precision numbers coming from an untrusted source. The advantages over
using `Rational` for this are that:

* A `Scientific` is more efficient to construct. Rational numbers need to be
constructed using `%` which has to compute the `gcd` of the `numerator` and
`denominator`.

* `Scientific` is safe against numbers with huge exponents. For example:
`1e1000000000 :: Rational` will fill up all space and crash your
program. Scientific works as expected:

```
 > read "1e1000000000" :: Scientific
 1.0e1000000000
 ```

* Also, the space usage of converting scientific numbers with huge exponents to
Integral's (like: `Int`) or RealFloats (like: `Double` or `Float`) will always
be bounded by the target type.
