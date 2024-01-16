module Rational exposing
    ( Above0Up
    , fromInteger, fromIntegerOver
    , multiplyBy
    , simplify
    , areEqual
    , reciprocal
    )

{-| Arbitrary-precision signed rational `numerator / denominator`.

Note that "rational" doesn't mean "part of a whole". For that, see [`Percentage`](Percentage).

@docs Above0Up


## create

@docs fromInteger, fromIntegerOver


## alter

@docs inverse, multiplyBy
@docs simplify


## scan

@docs areEqual

-}

import Bit
import Integer
import N
import N0able exposing (N0able)
import Negatable exposing (Negatable)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign


{-| A positive rational.

While you can expect the operations in this module to return the simplest form
(numerator and denominator are coprime: they have no shared common factors that could be canceled),
the type doesn't reflect this fact.
I recommend using [`simplify`](#simplify) to for example display numerator and denominator
and [`areEqual`](#areEqual) for comparing.

-}
type alias Above0Up =
    RecordWithoutConstructorFunction
        { numerator : Integer.N1Up, denominator : Integer.N1Up }


{-| Convert from an [integer](Integer). Information about "0-ability" is preserved.

    half : N0able Signed n0Never_
    half =
        Integer.n2 |> Integer.fromNatural |> Rational.fromInteger |> Rational.inverse

Isn't it neat how the information about the number being not 0 is padded from operation to operation?

-}
fromInteger :
    N0able (Negatable Integer.N1Up negativePossiblyOrNever) n0PossiblyOrNever
    -> N0able (Negatable Above0Up negativePossiblyOrNever) n0PossiblyOrNever
fromInteger =
    \integer ->
        integer |> fromIntegerOver Integer.n1


{-| Create from the numerator and a given signed denominator.

    fromInteger : N0able (Negatable Integer.N1Up negative) n0 -> N0able (Negatable Above0Up negative) n0
    fromInteger =
        fromIntegerOver Integer.n1

-}
fromIntegerOver :
    N0able (Negatable Integer.N1Up negativePossiblyOrNever) Never
    ->
        (N0able (Negatable Integer.N1Up negativePossiblyOrNever) n0PossiblyOrNever
         -> N0able (Negatable Above0Up negativePossiblyOrNever) n0PossiblyOrNever
        )
fromIntegerOver divisor =
    \base ->
        base
            |> N0able.not0Map
                (\baseSigned ->
                    let
                        divisorSigned : Negatable Integer.N1Up negativePossiblyOrNever
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


{-| Swap numerator and denominator. Often called the (multiplicative) inverse and denoted by x^(−1) or 1/x.

    rationalDivide : N0able (Negatable Above0Up negative) n0 -> (N0able (Negatable Above0Up negative) n0 -> N0able (Negatable Above0Up negative) n0)
    rationalDivide divisor =
        Rational.multiplyBy (divisor |> Rational.reciprocal)

-}
reciprocal : N0able (Negatable Above0Up negativePossiblyOrNever) n0PossiblyOrNever -> N0able (Negatable Above0Up negativePossiblyOrNever) n0PossiblyOrNever
reciprocal =
    \rational ->
        rational
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


{-| Multiply the rational by a given rational.

To scale it, use

    yourRational
        |> Rational.multiplyBy (yourInteger |> Rational.fromInteger)

-}
multiplyBy :
    N0able (Negatable Above0Up negativePossiblyOrNever) n0PossiblyOrNever
    ->
        (N0able (Negatable Above0Up negativePossiblyOrNever) n0PossiblyOrNever
         -> N0able (Negatable Above0Up negativePossiblyOrNever) n0PossiblyOrNever
        )
multiplyBy factor =
    \rational ->
        case rational of
            N0able.N0 possiblyOrNever ->
                N0able.N0 possiblyOrNever

            N0able.Not0 rationalSigned ->
                case factor of
                    N0able.N0 possiblyOrNever ->
                        N0able.N0 possiblyOrNever

                    N0able.Not0 factorSigned ->
                        rationalSigned
                            |> signedMultiplyBySigned factorSigned
                            |> signedSimplify
                            |> N0able.Not0


signedMultiplyBySigned :
    Negatable Above0Up negativePossiblyOrNever
    -> (Negatable Above0Up negativePossiblyOrNever -> Negatable Above0Up negativePossiblyOrNever)
signedMultiplyBySigned factorSigned =
    \rationalSigned ->
        { sign =
            rationalSigned.sign |> Sign.multiplyBy factorSigned.sign
        , absolute =
            { numerator =
                (rationalSigned.numerator |> N0able.Not0)
                    |> Integer.multiplyBy (factorSigned.numerator |> N0able.Not0)
                    |> N0able.toNot0
            , denominator =
                (rationalSigned.denominator |> N0able.Not0)
                    |> Integer.multiplyBy (factorSigned.denominator |> N0able.Not0)
                    |> N0able.toNot0
            }
        }


{-| While you can expect the operations in this module to return the simplest form
(numerator and denominator are coprime: they have no shared common factors that could be canceled),
the type doesn't reflect this fact.

→ use [`simplify`](#simplify) to for example display numerator and denominator
and [`areEqual`](#areEqual) for comparing.

-}
simplify :
    N0able (Negatable Above0Up negativePossiblyOrNever) n0PossiblyOrNever
    -> N0able (Negatable Above0Up negativePossiblyOrNever) n0PossiblyOrNever
simplify =
    \rational ->
        rational |> N0able.not0Map (\signed -> signed |> signedSimplify)


signedSimplify : Negatable Above0Up negativePossiblyOrNever -> Negatable Above0Up negativePossiblyOrNever
signedSimplify =
    \signed ->
        { sign = signed.sign, absolute = signed.absolute |> above0UpSimplify }


above0UpSimplify : Above0Up -> Above0Up
above0UpSimplify =
    \above0Up ->
        -- TODO
        above0Up


greatestCommonFactor : ( Integer.N1Up, Integer.N1Up ) -> Integer.N1Up
greatestCommonFactor =
    \( above0Up0, above0Up1 ) ->
        ( above0Up0, above0Up1 ) |> greatestCommonFactorFromIndex N0able.n0


greatestCommonFactorFromIndex : N0able Integer.N1Up possiblyOrNever_ -> (( Integer.N1Up, Integer.N1Up ) -> Integer.N1Up)
greatestCommonFactorFromIndex index =
    -- https://en.wikipedia.org/wiki/Greatest_common_divisor#Binary_GCD_algorithm
    \( above0Up0, above0Up1 ) ->
        if above0Up0 == above0Up1 then
            above0Up0 |> multiplyBy (naturalN2ToPower index)

        else
            Debug.todo ""


naturalN2ToPower =
    Debug.todo ""


naturalIsEven : N0able Integer.N1Up n0PossiblyOrNever_ -> Bool
naturalIsEven =
    \natural ->
        case natural of
            N0able.N0 _ ->
                True

            N0able.Not0 n1Up ->
                case ( Bit.I, n1Up.bitsAfterI ) |> listFilledLast of
                    Bit.I ->
                        False

                    Bit.O ->
                        True


listFilledLast : ( a, List a ) -> a
listFilledLast =
    \listFilled ->
        case listFilled of
            ( onlyElement, [] ) ->
                onlyElement

            ( _, second :: thirdUp ) ->
                ( second, thirdUp ) |> listFilledLast


{-| While you can expect the operations in this module to return the simplest form
(numerator and denominator are coprime: they have no shared common factors that could be canceled),
the type doesn't reflect this fact.

→ use [`simplify`](#simplify) to for example display numerator and denominator
and [`areEqual`](#areEqual) for comparing.

-}
areEqual :
    ( N0able (Negatable Above0Up negativePossiblyOrNever0_) n0PossiblyOrNever0_
    , N0able (Negatable Above0Up negativePossiblyOrNever1_) n0PossiblyOrNever1_
    )
    -> Bool
areEqual =
    \rationals ->
        case rationals of
            ( N0able.N0 _, N0able.N0 _ ) ->
                True

            ( N0able.N0 _, N0able.Not0 _ ) ->
                False

            ( N0able.Not0 _, N0able.N0 _ ) ->
                False

            ( N0able.Not0 signed0, N0able.Not0 signed1 ) ->
                (signed0 |> signedSimplify) == (signed1 |> signedSimplify)
