module Decimal exposing
    ( Signed, SignedAbsolute(..), N1Up
    , Exception(..)
    , fromFloat
    , ceiling, floor, truncate
    , toFloat, orExceptionToFloat, exceptionToFloat, exceptionToString
    )

{-| Arbitrarily precise floating point number
with the explicit _option_ of allowing of [exceptions](Decimal#Exception) or not.

Cannot represent decimals with a period (infinitely repeating digits after the decimal point).
To represent decimals with a period without loss of precision, use a [fraction](Fraction)

@docs Signed, SignedAbsolute, N1Up
@docs Exception


## create

@docs fromFloat


## alter

@docs ceiling, floor, truncate


## transform

@docs toFloat, orExceptionToFloat, exceptionToFloat, exceptionToString

The type is rarely useful in its current state,
as the only thing you can do is convert from and to other types.

This is enough for my use-cases
but feel free to PR or open an issue if you'd like to see support
for arbitrary-precision arithmetic like addition, multiplication, ...

-}

import Emptiable exposing (Emptiable)
import Integer
import N exposing (In, N, N0, N9, n0, n1, n9)
import N0able exposing (N0able)
import Natural
import Natural1UpBase10
import Percentage exposing (PercentageAbove0ToBelow1)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign exposing (Sign(..))
import Stack exposing (Stacked)


{-| A signed floating point number.

Don't shy away from spinning your own version of this if needed, like

    type FieldException
        = DivisionByZeroResult
        | Infinity Sign

    Result FieldException (N0able Decimal.Signed Possibly)

See also [`Decimal.Exception`](Decimal#Exception)

-}
type alias Signed =
    RecordWithoutConstructorFunction
        { sign : Sign
        , absolute : SignedAbsolute
        }


{-| What comes after its [`Sign`](Sign#Sign)
-}
type SignedAbsolute
    = PercentageAbove0ToBelow1 PercentageAbove0ToBelow1
    | N1Up N1Up


{-| A positive decimal ≥ 1
-}
type alias N1Up =
    RecordWithoutConstructorFunction
        { whole : Natural.N1Up
        , afterDecimalPoint : N0able PercentageAbove0ToBelow1 Possibly
        }


{-| Non-number calculation result, see also [IEEE 754 number exception states](#Exception)

`Result Exception Decimal` for example is like an [`elm/core` `Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float)
except

  - [`Exception`](#Exception)s are an explicit case so you can easily extract a [`Decimal`](Decimal#Decimal)
  - it can have arbitrary decimal points, see [`Decimal`](Decimal#Decimal)

-}
type Exception
    = NaN
    | Infinity Sign



--


{-| Remove the [`Fraction`](#Fraction) part after the decimal point `.`
to create an [`Integer`](Integer#Integer)
-}
truncate : N0able Signed n0PossiblyOrNever_ -> N0able Integer.Signed Possibly
truncate =
    \decimal ->
        case decimal of
            N0able.N0 _ ->
                N0able.n0

            N0able.Not0 signed ->
                signed |> signedTruncate


signedTruncate : Signed -> N0able Integer.Signed Possibly
signedTruncate =
    \signed ->
        case signed.absolute of
            PercentageAbove0ToBelow1 _ ->
                N0able.n0

            N1Up atLeast1 ->
                N0able.Not0
                    { sign = signed.sign
                    , absolute = atLeast1.whole
                    }


{-| Its nearest lower [`Integer`](Integer#Integer) number
-}
floor : N0able Signed n0PossiblyOrNever_ -> N0able Integer.Signed Possibly
floor =
    \decimal ->
        case decimal of
            N0able.N0 _ ->
                N0able.n0

            N0able.Not0 signed ->
                signed |> signedFloor


signedFloor : Signed -> N0able Integer.Signed Possibly
signedFloor =
    \signed ->
        case signed.absolute of
            PercentageAbove0ToBelow1 _ ->
                N0able.n0

            N1Up atLeast1 ->
                N0able.Not0
                    { sign = signed.sign
                    , absolute =
                        case signed.sign of
                            Positive ->
                                atLeast1.whole

                            Negative ->
                                atLeast1.whole |> Natural.add Natural.n1 |> N0able.toNot0
                    }


{-| Its nearest greater [`Integer`](Integer#Integer) number
-}
ceiling : N0able Signed n0PossiblyOrNever_ -> N0able Integer.Signed Possibly
ceiling =
    \decimal ->
        case decimal of
            N0able.N0 _ ->
                N0able.n0

            N0able.Not0 signed ->
                signed |> signedCeiling


signedCeiling : Signed -> N0able Integer.Signed Possibly
signedCeiling =
    \signed ->
        case signed.absolute of
            PercentageAbove0ToBelow1 _ ->
                case signed.sign of
                    Positive ->
                        N0able.Not0
                            { sign = signed.sign, absolute = Natural.n1 |> N0able.toNot0 }

                    Negative ->
                        N0able.n0

            N1Up atLeast1 ->
                N0able.Not0
                    { sign = signed.sign
                    , absolute =
                        case signed.sign of
                            Positive ->
                                atLeast1.whole |> Natural.add Natural.n1 |> N0able.toNot0

                            Negative ->
                                atLeast1.whole
                    }


{-| Print an [`Exception`](#Exception)
-}
exceptionToString : Exception -> String
exceptionToString =
    \exception ->
        case exception of
            NaN ->
                "NaN"

            Infinity Sign.Negative ->
                "negative infinity"

            Infinity Sign.Positive ->
                "positive infinity"


{-| Convert from an [`elm/core` `Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float).
Don't be surprised to find more precise representations in [`Decimal`](#Decimal)-land

    -9999.124 |> Decimal.fromFloat
    --→ Ok with the Decimal representation of
    --→ 999.1239999999997962731868028640747070312

-}
fromFloat : Float -> Result Exception (N0able Signed Possibly)
fromFloat =
    \float_ ->
        if float_ |> Basics.isNaN then
            Err NaN

        else if float_ |> Basics.isInfinite then
            Err
                (Infinity
                    (if float_ < 0 then
                        Negative

                     else
                        Positive
                    )
                )

        else
            Ok (float_ |> fromNonExceptionFloat)


{-| **Should not be exposed**
-}
fromNonExceptionFloat : Float -> N0able Signed Possibly
fromNonExceptionFloat =
    \float_ ->
        if float_ == 0 then
            N0able.n0

        else
            -- /= 0
            let
                floatAbsolute : number_
                floatAbsolute =
                    float_ |> Basics.abs

                wholeAbsolute : Int
                wholeAbsolute =
                    floatAbsolute |> Basics.truncate
            in
            N0able.Not0
                { sign =
                    if float_ < 0 then
                        Negative

                    else
                        Positive
                , absolute =
                    case wholeAbsolute of
                        0 ->
                            floatAbsolute |> floatToAfterDecimalPoint |> PercentageAbove0ToBelow1

                        wholeAbsoluteExcept0 ->
                            { whole =
                                wholeAbsoluteExcept0 |> Natural1UpBase10.fromIntPositive |> Natural1UpBase10.toBase2
                            , fraction =
                                let
                                    floatFraction =
                                        floatAbsolute - (wholeAbsoluteExcept0 |> Basics.toFloat)
                                in
                                if floatFraction == 0 then
                                    Nothing

                                else
                                    -- floatFraction /= 0
                                    floatFraction
                                        |> Basics.abs
                                        |> floatToAfterDecimalPoint
                                        |> Just
                            }
                                |> N1Up
                }


floatToAfterDecimalPoint : Float -> PercentageAbove0ToBelow1
floatToAfterDecimalPoint =
    \float ->
        case float |> floatFractionToBase10 |> unpadLeading0Digits of
            [] ->
                { beforeEnd = [], end = n1 |> N.maxTo n9 |> N.inToNumber }

            first :: afterFirst ->
                let
                    digitsReverse : Emptiable (Stacked (N (In N0 N9))) never_
                    digitsReverse =
                        Stack.topBelow first afterFirst |> Stack.reverse
                in
                { beforeEnd = digitsReverse |> Stack.removeTop |> Stack.toList |> List.reverse
                , end = digitsReverse |> Stack.top |> N.inToOn |> N.toIn ( n1, n9 ) |> N.inToNumber
                }


floatFractionToBase10 : Float -> List (N (In N0 N9))
floatFractionToBase10 =
    \float ->
        if float == 0 then
            []

        else
            let
                floatShifted1Digit : Float
                floatShifted1Digit =
                    float * 10

                digit : Int
                digit =
                    floatShifted1Digit |> Basics.floor
            in
            (digit |> N.intToIn ( n0, n9 ) |> N.inToNumber)
                :: ((floatShifted1Digit - (digit |> Basics.toFloat))
                        |> floatFractionToBase10
                   )


unpadLeading0Digits : List (N (In N0 N9)) -> List (N (In N0 N9))
unpadLeading0Digits =
    \digits ->
        case digits of
            [] ->
                []

            digit0 :: digits1Up ->
                case N.toInt digit0 of
                    0 ->
                        unpadLeading0Digits digits1Up

                    _ ->
                        digit0 :: digits1Up


{-| NaN, infinity or [`Decimal.toFloat`](#toFloat)

Keep in mind that `Decimal -> Float` can be lossy
since `Float` is fixed in bit size while [`Decimal`](Decimal#Decimal) is not

-}
orExceptionToFloat : Result Exception (N0able Signed possiblyOrNever_) -> Float
orExceptionToFloat =
    \decimalOrException ->
        case decimalOrException of
            Err exception ->
                exception |> exceptionToFloat

            Ok decimal ->
                decimal |> toFloat


{-| infinity/NaN represented as a `Float`
-}
exceptionToFloat : Exception -> Float
exceptionToFloat =
    \exception ->
        case exception of
            NaN ->
                floatNaN

            Infinity Sign.Positive ->
                floatInfinity

            Infinity Sign.Negative ->
                -floatInfinity


floatNaN : Float
floatNaN =
    0.0 / 0.0


floatInfinity : Float
floatInfinity =
    1.0 / 0.0


{-| Convert to a `Float`

Keep in mind that `DecimalOrException -> Float` can be lossy
since `Float` is fixed in bit size while a [decimal](Decimal) is not

-}
toFloat : N0able Signed possiblyToNever_ -> Float
toFloat =
    \decimal ->
        case decimal of
            N0able.N0 _ ->
                0

            N0able.Not0 numberSigned ->
                let
                    toSigned : number -> number
                    toSigned =
                        case numberSigned.sign of
                            Negative ->
                                Basics.negate

                            Positive ->
                                Basics.abs
                in
                numberSigned.absolute |> signedAbsoluteToFloat |> toSigned


signedAbsoluteToFloat : SignedAbsolute -> Float
signedAbsoluteToFloat =
    \absolute ->
        case absolute of
            PercentageAbove0ToBelow1 percentageAbove0ToBelow1 ->
                percentageAbove0ToBelow1 |> Percentage.toFloat

            N1Up atLeast1 ->
                let
                    wholeFloat : Float
                    wholeFloat =
                        atLeast1.whole |> N0able.Not0 |> Natural.n0UpToN |> N.toFloat
                in
                case atLeast1.fraction of
                    Nothing ->
                        wholeFloat

                    Just fraction_ ->
                        wholeFloat + (fraction_ |> Percentage.toFloat)
