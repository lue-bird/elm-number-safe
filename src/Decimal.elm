module Decimal exposing
    ( Above0Up(..), N1Up
    , fromFloat
    , ceiling, floor, truncate
    , toFloat
    , FloatException(..)
    )

{-| Arbitrarily precise floating point number
which is [`Negatable`](Negatable#Negatable), [`N0able`]

The type cannot represent decimals with a period (infinitely repeating digits after the decimal point).
To represent decimals with a period without losing information, use a [rational](Rational)

@docs Above0Up, N1Up


## create

@docs fromFloat


## alter

@docs ceiling, floor, truncate


## transform

@docs toFloat

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
import Natural1UpBase10
import Negatable exposing (Negatable)
import Percentage exposing (PercentageAbove0ToBelow1)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign
import Stack exposing (Stacked)


{-| What comes after its [`Sign`](Sign#Sign). Used in types like

    N0able (Negatable Decimal.Above0Up Possibly) Possibly

Don't shy away from spinning your own version of this if needed, like

    type FieldException
        = DivisionByZeroResult
        | InfinityNegative
        | InfinityPositive

    Result FieldException (N0able (Negatable Decimal.Above0Up Possibly) Possibly)

-}
type Above0Up
    = PercentageAbove0ToBelow1 PercentageAbove0ToBelow1
    | N1Up N1Up


{-| A positive decimal ≥ 1
-}
type alias N1Up =
    RecordWithoutConstructorFunction
        { whole : Integer.N1Up
        , afterDecimalPoint : N0able PercentageAbove0ToBelow1 Possibly
        }



--


{-| Remove the [`Rational`](#Rational) part after the decimal point `.`
to create an [`Integer`](Integer#Integer)
-}
truncate : N0able (Negatable Above0Up negative) n0PossiblyOrNever_ -> N0able (Negatable Integer.N1Up negative) Possibly
truncate =
    \decimal ->
        case decimal of
            N0able.N0 _ ->
                N0able.n0

            N0able.Not0 signed ->
                signed |> negatableTruncate


negatableTruncate : Negatable Above0Up negative -> N0able (Negatable Integer.N1Up negative) Possibly
negatableTruncate =
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
floor : N0able (Negatable Above0Up negative) n0PossiblyOrNever_ -> N0able (Negatable Integer.N1Up negative) Possibly
floor =
    \decimal ->
        case decimal of
            N0able.N0 _ ->
                N0able.n0

            N0able.Not0 signed ->
                signed |> signedFloor


signedFloor : Negatable Above0Up negative -> N0able (Negatable Integer.N1Up negative) Possibly
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
                            Sign.Positive ->
                                atLeast1.whole

                            Sign.Negative _ ->
                                atLeast1.whole |> Integer.add Integer.n1 |> N0able.toNot0
                    }


{-| Its nearest greater [`Integer`](Integer#Integer) number
-}
ceiling : N0able (Negatable Above0Up negative) n0PossiblyOrNever_ -> N0able (Negatable Integer.N1Up negative) Possibly
ceiling =
    \decimal ->
        case decimal of
            N0able.N0 _ ->
                N0able.n0

            N0able.Not0 signed ->
                signed |> signedCeiling


signedCeiling : Negatable Above0Up negative -> N0able (Negatable Integer.N1Up negative) Possibly
signedCeiling =
    \signed ->
        case signed.absolute of
            PercentageAbove0ToBelow1 _ ->
                case signed.sign of
                    Sign.Positive ->
                        N0able.Not0
                            { sign = signed.sign, absolute = Integer.n1 |> N0able.toNot0 }

                    Sign.Negative _ ->
                        N0able.n0

            N1Up atLeast1 ->
                N0able.Not0
                    { sign = signed.sign
                    , absolute =
                        case signed.sign of
                            Sign.Positive ->
                                atLeast1.whole |> Integer.add Integer.n1 |> N0able.toNot0

                            Sign.Negative _ ->
                                atLeast1.whole
                    }


{-| Non-number calculation result, see also [IEEE 754 number exception states](#Exception).

Used to safely convert [`fromFLoat`](#fromFLoat)

-}
type FloatException
    = NaN
    | Infinity (Sign.Negatable Possibly)


{-| Convert from an [`elm/core` `Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float).

Don't be surprised to find more precise representations in [`Decimal`](#Decimal)-land

    -9999.124 |> Decimal.fromFloat
    --→ Ok with the Decimal representation of
    --→ 999.1239999999997962731868028640747070312

For safety, [`fromFloat`](#fromFloat) can return [IEEE 754 number exception states](#FloatException).

-}
fromFloat : Float -> Result FloatException (N0able (Negatable Above0Up Possibly) Possibly)
fromFloat =
    \float_ ->
        if float_ |> Basics.isNaN then
            Err NaN

        else if float_ |> Basics.isInfinite then
            Err
                (Infinity
                    (if float_ < 0 then
                        Sign.Negative Possibly.Possible

                     else
                        Sign.Positive
                    )
                )

        else
            Ok (float_ |> fromNonExceptionFloat)


{-| **Should not be exposed**
-}
fromNonExceptionFloat : Float -> N0able (Negatable Above0Up Possibly) Possibly
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
                        Sign.Negative Possibly.Possible

                    else
                        Sign.Positive
                , absolute =
                    case wholeAbsolute of
                        0 ->
                            floatAbsolute |> floatToAfterDecimalPoint |> PercentageAbove0ToBelow1

                        wholeAbsoluteExcept0 ->
                            { whole =
                                wholeAbsoluteExcept0 |> Natural1UpBase10.fromIntPositive |> Natural1UpBase10.toBase2
                            , rational =
                                let
                                    floatAfterDecimalPoint : Float
                                    floatAfterDecimalPoint =
                                        floatAbsolute - (wholeAbsoluteExcept0 |> Basics.toFloat)
                                in
                                if floatAfterDecimalPoint == 0 then
                                    Nothing

                                else
                                    -- floatRational /= 0
                                    floatAfterDecimalPoint
                                        |> Basics.abs
                                        |> floatToAfterDecimalPoint
                                        |> Just
                            }
                                |> N1Up
                }


floatToAfterDecimalPoint : Float -> PercentageAbove0ToBelow1
floatToAfterDecimalPoint =
    \float ->
        case float |> floatRationalToBase10 |> unpadLeading0Digits of
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


floatRationalToBase10 : Float -> List (N (In N0 N9))
floatRationalToBase10 =
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
                        |> floatRationalToBase10
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


{-| Convert to a `Float`

Keep in mind that `DecimalOrException -> Float` can be lossy
since `Float` is fixed in bit size while a [decimal](Decimal) is not

-}
toFloat : N0able (Negatable Above0Up negativePossiblyOrNever_) possiblyToNever_ -> Float
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
                            Sign.Negative _ ->
                                Basics.negate

                            Sign.Positive ->
                                Basics.abs
                in
                numberSigned.absolute |> signedAbsoluteToFloat |> toSigned


signedAbsoluteToFloat : Above0Up -> Float
signedAbsoluteToFloat =
    \absolute ->
        case absolute of
            PercentageAbove0ToBelow1 percentageAbove0ToBelow1 ->
                percentageAbove0ToBelow1 |> Percentage.toFloat

            N1Up atLeast1 ->
                let
                    wholeFloat : Float
                    wholeFloat =
                        atLeast1.whole |> N0able.Not0 |> Integer.n0UpToN |> N.toFloat
                in
                case atLeast1.rational of
                    Nothing ->
                        wholeFloat

                    Just rational_ ->
                        wholeFloat + (rational_ |> Percentage.toFloat)
