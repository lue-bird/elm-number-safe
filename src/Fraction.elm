module Fraction exposing
    ( Signed, Above0Up
    , fromInteger, fromIntegerOver
    , inverse, negate, multiplyBy
    )

{-| Arbitrary-precision `numerator / denominator`

@docs Signed, Above0Up


## create

@docs fromInteger, fromIntegerOver


## alter

@docs inverse, negate, multiplyBy

-}

import Integer
import N0able exposing (N0able)
import Natural
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign exposing (Sign)


{-| A signed fraction. Note that fraction here uses the mathematical definition as the division of 2 values.
For "part of a whole", see [`Percentage`](Percentage).
-}
type alias Signed =
    RecordWithoutConstructorFunction
        { sign : Sign, absolute : Above0Up }


{-| A positive fraction.

Don't rely on common factors of numerator and denominator being canceled,
as this might change between versions while the type won't reflect this.

-}
type alias Above0Up =
    RecordWithoutConstructorFunction
        { numerator : Natural.N1Up, denominator : Natural.N1Up }


{-| Convert from an [integer](Integer). Information about "0-ability" is preserved.

    half : N0able Signed n0Never_
    half =
        Natural.n2 |> Integer.fromNatural |> Fraction.fromInteger |> Fraction.inverse

Isn't it neat how the information about the number being not 0 is padded from operation to operation?

-}
fromInteger : N0able Integer.Signed n0PossiblyOrNever -> N0able Signed n0PossiblyOrNever
fromInteger =
    \integer ->
        integer
            |> fromIntegerOver (Natural.n1 |> Integer.fromNatural)


{-| Create from the numerator and a given signed denominator.

    fromInteger : N0able Integer.Signed n0PossiblyOrNever -> N0able Signed n0PossiblyOrNever
    fromInteger =
        fromIntegerOver (Natural.n1 |> Integer.fromNatural)

-}
fromIntegerOver :
    N0able Integer.Signed Never
    -> (N0able Integer.Signed n0PossiblyOrNever -> N0able Signed n0PossiblyOrNever)
fromIntegerOver divisor =
    \base ->
        base
            |> N0able.not0Map
                (\baseSigned ->
                    let
                        divisorSigned : Integer.Signed
                        divisorSigned =
                            divisor |> N0able.toNot0
                    in
                    { sign = baseSigned.sign |> Sign.multiplyBy divisorSigned.sign
                    , absolute =
                        { numerator = baseSigned.absolute
                        , denominator = divisorSigned.absolute
                        }
                    }
                )


{-| Swap numerator and denominator.

    fractionDivide : N0able Signed n0PossiblyOrNever -> (N0able Signed n0PossiblyOrNever -> N0able Signed n0PossiblyOrNever)
    fractionDivide divisor =
        Fraction.multiplyBy (divisor |> Fraction.inverse)

-}
inverse : N0able Signed n0PossiblyOrNever -> N0able Signed n0PossiblyOrNever
inverse =
    \fraction ->
        fraction
            |> N0able.not0Map
                (\signed ->
                    { sign = signed.sign
                    , absolute = signed.absolute |> above0UpInverse
                    }
                )


above0UpInverse : Above0Up -> Above0Up
above0UpInverse =
    \above0Up ->
        { numerator = above0Up.denominator, denominator = above0Up.numerator }


{-| Flip its sign.
-}
negate : N0able Signed n0PossiblyOrNever -> N0able Signed n0PossiblyOrNever
negate =
    \fraction ->
        fraction
            |> N0able.not0Map
                (\signed ->
                    { sign = signed.sign |> Sign.opposite, absolute = signed.absolute }
                )


{-| Multiply the fraction by a given fraction.

To scale it, use

    yourFraction
        |> Fraction.multiplyBy (yourInteger |> Fraction.fromInteger)

-}
multiplyBy : N0able Signed n0PossiblyOrNever -> (N0able Signed n0PossiblyOrNever -> N0able Signed n0PossiblyOrNever)
multiplyBy factor =
    \fraction ->
        case fraction of
            N0able.N0 possiblyOrNever ->
                N0able.N0 possiblyOrNever

            N0able.Not0 fractionSigned ->
                case factor of
                    N0able.N0 possiblyOrNever ->
                        N0able.N0 possiblyOrNever

                    N0able.Not0 factorSigned ->
                        fractionSigned
                            |> signedMultiplyBySigned factorSigned
                            |> N0able.Not0


signedMultiplyBySigned : Signed -> Signed -> Signed
signedMultiplyBySigned factorSigned =
    \fractionSigned ->
        { sign =
            fractionSigned.sign |> Sign.multiplyBy factorSigned.sign
        , absolute =
            { numerator =
                (fractionSigned.numerator |> N0able.Not0)
                    |> Natural.multiplyBy (factorSigned.numerator |> N0able.Not0)
                    |> N0able.toNot0
            , denominator =
                (fractionSigned.denominator |> N0able.Not0)
                    |> Natural.multiplyBy (factorSigned.denominator |> N0able.Not0)
                    |> N0able.toNot0
            }
        }
