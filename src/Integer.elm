module Integer exposing
    ( N1Up
    , n1, n2, fromN, fromInt
    , compare
    , add, addAdapt, subtract1, multiplyBy
    , toInt, n0UpToN, n1UpToN
    )

{-| Arbitrarily large whole number which is [`Negatable`](Negatable#Negatable) and [`N0able`](N0able#N0able).

@docs N1Up


## create

@docs n1, n2, fromN, fromInt


## observe

@docs compare


## alter

@docs add, addAdapt, subtract1, multiplyBy


## transform

@docs toInt, n0UpToN, n1UpToN

The type is rarely useful in its current state,
as the only thing you can do is convert from and to other types.

This is enough for my use-cases
but feel free to PR or open an issue if you'd like to see support
for arbitrary-precision arithmetic like addition, multiplication, ...

-}

import Bit exposing (Bit)
import Bitwise
import Linear
import List.Linear
import N exposing (N)
import N0able exposing (N0able)
import Negatable exposing (Negatable)
import PartialOrComplete exposing (PartialOrComplete)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign


{-| Positive whole number (integer ≥ 1) of arbitrary precision.
Either 0 directly or a positive number represented by the bit `I` followed by some
[`Bit`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/Bit)s

Why does the type look like this?

If you need a natural number representation with a specific number of bits, you can try

    ArraySized Bit (Exactly (On bitLength))

For larger numbers, where you want to allow numbers of arbitrary precision,
only our version can enforce that `==` always gives the correct answer,
since the `ArraySized` could be constructed with leading `O`s.

-}
type alias N1Up =
    RecordWithoutConstructorFunction
        { bitsAfterI : List Bit }


{-| The positive natural number 1
-}
n1 : N0able (Negatable N1Up negativeNever_) n0Never_
n1 =
    N0able.Not0 { sign = Sign.Positive, absolute = n1As1Up }


n1As1Up : N1Up
n1As1Up =
    { bitsAfterI = [] }


{-| The positive natural number 2
-}
n2 : N0able (Negatable N1Up negativeNever_) n0Never_
n2 =
    n1 |> add n1 |> Negatable.negativeAdapt never |> N0able.n0Adapt never


{-| Convert from an `Int`
-}
fromInt : Int -> N0able (Negatable N1Up Possibly) Possibly
fromInt =
    \intBroad ->
        intBroad
            |> N.intToAbsolute
            |> fromN
            |> N0able.not0Map
                (\n1Up ->
                    { sign =
                        if intBroad > 0 then
                            Sign.Positive

                        else
                            -- intBroad < 0
                            Sign.Negative Possibly.Possible
                    , absolute = n1Up
                    }
                )


compare : ( N0able (Negatable N1Up aNegative_) a0_, N0able (Negatable N1Up bNegative_) b0_ ) -> Basics.Order
compare =
    \integers ->
        case integers of
            ( N0able.N0 _, N0able.N0 _ ) ->
                Basics.EQ

            ( N0able.N0 _, N0able.Not0 _ ) ->
                Basics.LT

            ( N0able.Not0 _, N0able.N0 _ ) ->
                Basics.GT

            ( N0able.Not0 aNegatable, N0able.Not0 bNegatable ) ->
                case ( aNegatable.sign, bNegatable.sign ) |> Sign.compare of
                    Basics.LT ->
                        Basics.LT

                    Basics.GT ->
                        Basics.GT

                    Basics.EQ ->
                        ( Bit.I :: aNegatable.absolute.bitsAfterI
                        , Bit.I :: bNegatable.absolute.bitsAfterI
                        )
                            |> bitListsToEquallySized
                            |> bitsListCompare


bitsListCompare : List ( Bit, Bit ) -> Basics.Order
bitsListCompare =
    \bitsList ->
        bitsList
            |> List.Linear.foldUntilCompleteFrom ()
                Linear.Up
                (\bits () ->
                    case bits |> bitCompare of
                        Basics.LT ->
                            Basics.LT |> PartialOrComplete.Complete

                        Basics.GT ->
                            Basics.GT |> PartialOrComplete.Complete

                        Basics.EQ ->
                            () |> PartialOrComplete.Partial
                )
            |> PartialOrComplete.completeElseOnPartial (\() -> EQ)


bitCompare : ( Bit, Bit ) -> Basics.Order
bitCompare =
    \bits ->
        case bits of
            ( Bit.O, Bit.O ) ->
                Basics.EQ

            ( Bit.O, Bit.I ) ->
                Basics.LT

            ( Bit.I, Bit.O ) ->
                Basics.GT

            ( Bit.I, Bit.I ) ->
                Basics.EQ


{-|

  - `Negative` means negate
  - `Positive` means keep the current sign

-}
signPrependToNumber : Sign.Negatable negativePossiblyOrNever_ -> (number -> number)
signPrependToNumber sign =
    case sign of
        Sign.Negative _ ->
            Basics.negate

        Sign.Positive ->
            identity


{-| Convert to an `Int`

Keep in mind that this can overflow
since `Int` is fixed in bit size while an [integer](Integer) is not.

-}
toInt : N0able (Negatable N1Up negativePossiblyOrNever_) n0PossiblyOrNever_ -> Int
toInt =
    \integerNarrow ->
        case integerNarrow of
            N0able.N0 _ ->
                0

            N0able.Not0 signedValue ->
                signedValue.absolute
                    |> N0able.Not0
                    |> n0UpToN
                    |> N.toInt
                    |> signPrependToNumber signedValue.sign


{-| Convert from a [natural number of type `N`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/).
-}
fromN : N range_ -> N0able N1Up Possibly
fromN =
    \n ->
        if n |> nHasAtMost32Bits then
            let
                nBits : List Bit
                nBits =
                    List.range 0 31
                        |> List.reverse
                        |> List.map
                            (\bitIndex -> n |> nBitAt bitIndex)
                        |> bitsUnpad
            in
            case nBits of
                [] ->
                    N0able.n0

                _ :: bitsAfterI ->
                    N0able.Not0 { bitsAfterI = bitsAfterI }

        else
            N0able.Not0 { bitsAfterI = List.repeat 32 Bit.I }


{-| Remove `Bit.O` padding on the left until the first `Bit.I`.
-}
bitsUnpad : List Bit -> List Bit
bitsUnpad =
    \bits ->
        bits
            |> List.Linear.mapFoldFrom
                { foundI = False }
                Linear.Up
                (\state ->
                    if state.folded.foundI then
                        { folded = { foundI = True }, element = state.element |> Just }

                    else
                        case state.element of
                            Bit.I ->
                                { folded = { foundI = True }, element = state.element |> Just }

                            Bit.O ->
                                { folded = { foundI = False }, element = Nothing }
                )
            |> .mapped
            |> List.filterMap identity


nHasAtMost32Bits : N range_ -> Bool
nHasAtMost32Bits =
    \n ->
        (n |> N.toInt) < 2 * (2 ^ 32)


nBitAt : Int -> (N range_ -> Bit)
nBitAt index =
    \n ->
        (n |> N.toInt |> Bitwise.shiftRightBy index)
            |> N.intModBy N.n2
            |> Bit.fromN


{-| Convert the 0-able natural number to an [`N`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/)

Keep in mind that this can overflow
since `N` is fixed in bit size just like `Int` while [our natural number](Natural) is not.

If you have a positive natural number, use [`n1UpToN`](#n1UpToN)

-}
n0UpToN : N0able N1Up n0PossiblyOrNever_ -> N (N.Min (N.Up0 minX_))
n0UpToN =
    \naturalNarrow ->
        case naturalNarrow of
            N0able.N0 _ ->
                N.n0 |> N.maxToInfinity

            N0able.Not0 n1Up ->
                n1Up |> N0able.Not0 |> n1UpToN |> N.minSubtract N.n1


{-| Convert the positive natural number to an [`N`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/)

Keep in mind that this can overflow
since `N` is fixed in bit size just like `Int` while [our natural number](Natural) is not.

If you have a 0-able natural number, use [`n0UpToN`](#n0UpToN)

-}
n1UpToN : N0able N1Up Never -> N (N.Min (N.Up1 minX_))
n1UpToN =
    \n1Up ->
        (Bit.I :: (n1Up |> N0able.toNot0 |> .bitsAfterI))
            |> List.Linear.foldFrom
                { power = 0, total = 0 }
                Linear.Down
                (\bit soFar ->
                    { power = soFar.power + 1
                    , total =
                        soFar.total
                            |> (case bit of
                                    Bit.O ->
                                        identity

                                    Bit.I ->
                                        \x -> x + (2 ^ soFar.power)
                               )
                    }
                )
            |> .total
            |> N.intToAtLeast N.n1


{-| On a natural number known to be positive, we can safely calculate its predecessor.

    factorial : N0able N1Up Possibly -> N0able N1Up n0Never_
    factorial =
        \natural ->
            case natural of
                N0able.N0 _ ->
                    n1

                N0able.Not0 n1Up ->
                    let
                        naturalPositive : N0able N1Up n0Never_
                        naturalPositive =
                            N0able.Not0 n1Up
                    in
                    factorial (naturalPositive |> subtract1)
                        |> multiplyBy naturalPositive

-}
subtract1 :
    N0able (Negatable N1Up resultNotNegative) resultNotNegative
    -> N0able (Negatable N1Up resultNotNegative) Possibly
subtract1 =
    \natural1Up ->
        case natural1Up of
            N0able.N0 _ ->
                N0able.n0

            N0able.Not0 negatable ->
                case negatable.sign of
                    Sign.Positive ->
                        negatable.absolute |> n1UpSubtract1

                    Sign.Negative negativePossiblyOrNever ->
                        { sign = Sign.Negative negativePossiblyOrNever
                        , absolute = negatable.absolute |> n1UpAdd1Up n1As1Up
                        }
                            |> N0able.Not0


n1UpSubtract1 : N1Up -> N0able (Negatable N1Up negativeNever_) Possibly
n1UpSubtract1 =
    \n1Up ->
        let
            subtractedBits : PartialOrComplete (List Bit) (List Bit)
            subtractedBits =
                n1Up.bitsAfterI
                    |> List.foldr
                        (\bit soFar ->
                            case soFar of
                                PartialOrComplete.Complete soFarList ->
                                    bit :: soFarList |> PartialOrComplete.Complete

                                PartialOrComplete.Partial soFarList ->
                                    case bit of
                                        Bit.O ->
                                            Bit.I :: soFarList |> PartialOrComplete.Partial

                                        Bit.I ->
                                            Bit.O :: soFarList |> PartialOrComplete.Complete
                        )
                        ([] |> PartialOrComplete.Partial)
        in
        case subtractedBits of
            -- the first I bit won't change
            PartialOrComplete.Complete afterISubtracted ->
                N0able.Not0 { sign = Sign.Positive, absolute = { bitsAfterI = afterISubtracted } }

            -- n1Up.bitsAfterI was empty → n1Up was 1
            PartialOrComplete.Partial [] ->
                N0able.n0

            -- first I bit needs to be subtracted
            PartialOrComplete.Partial (firstAfterI :: afterFirstAfterI) ->
                N0able.Not0 { sign = Sign.Positive, absolute = { bitsAfterI = firstAfterI :: afterFirstAfterI } }


{-| Sum 2 natural numbers where what you want to add to is 0-able and what you want to add is positive.
If this is not the case, [`add`](#add) will do.

On the type-level, this is exactly like [`Stack.attachAdapt`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack#attachAdapt)

-}
addAdapt :
    N0able (Negatable N1Up result0PossiblyOrNever) result0PossiblyOrNever
    ->
        (N0able (Negatable N1Up result0PossiblyOrNever) addedN0PossiblyOrNever_
         -> N0able (Negatable N1Up result0PossiblyOrNever) result0PossiblyOrNever
        )
addAdapt toAdd =
    \natural -> toAdd |> add natural


n1UpAdd1Up : N1Up -> (N1Up -> N1Up)
n1UpAdd1Up toAdd =
    \naturalPositive ->
        let
            bitsSum : { inRange : List Bit, overflow : Bit }
            bitsSum =
                (Bit.I :: naturalPositive.bitsAfterI)
                    |> bitListAdd (Bit.I :: toAdd.bitsAfterI)
        in
        case bitsSum.overflow of
            Bit.I ->
                { bitsAfterI = bitsSum.inRange }

            Bit.O ->
                { bitsAfterI = bitsSum.inRange |> List.drop 1 }


bitListAdd :
    List Bit
    ->
        (List Bit
         -> { inRange : List Bit, overflow : Bit }
        )
bitListAdd toAdd =
    \bitList -> bitList |> bitListAddWithCarry Bit.O toAdd


bitListAddWithCarry :
    Bit
    -> List Bit
    ->
        (List Bit
         -> { inRange : List Bit, overflow : Bit }
        )
bitListAddWithCarry initialCarry toAdd =
    \bits ->
        let
            addResult : { mapped : List Bit, folded : Bit }
            addResult =
                ( bits, toAdd )
                    |> bitListsToEquallySized
                    |> List.Linear.mapFoldFrom initialCarry
                        Linear.Down
                        (\step ->
                            let
                                summed : { bitLeastSignificant : Bit, bitMostSignificant : Bit }
                                summed =
                                    ( step.element |> Tuple.first, ( step.element |> Tuple.second, step.folded ) ) |> bits3Add
                            in
                            { element = summed.bitLeastSignificant, folded = summed.bitMostSignificant }
                        )
        in
        { inRange = addResult.mapped, overflow = addResult.folded }


bits3Add : ( Bit, ( Bit, Bit ) ) -> { bitLeastSignificant : Bit, bitMostSignificant : Bit }
bits3Add =
    \( a, ( b, c ) ) ->
        case ( a, ( b, c ) ) of
            ( Bit.O, ( Bit.O, overflowSoFar ) ) ->
                { bitLeastSignificant = overflowSoFar, bitMostSignificant = Bit.O }

            ( Bit.I, ( Bit.I, overflowSoFar ) ) ->
                { bitLeastSignificant = overflowSoFar, bitMostSignificant = Bit.I }

            ( Bit.I, ( Bit.O, Bit.O ) ) ->
                { bitLeastSignificant = Bit.I, bitMostSignificant = Bit.O }

            ( Bit.O, ( Bit.I, Bit.O ) ) ->
                { bitLeastSignificant = Bit.I, bitMostSignificant = Bit.O }

            ( Bit.I, ( Bit.O, Bit.I ) ) ->
                { bitLeastSignificant = Bit.O, bitMostSignificant = Bit.I }

            ( Bit.O, ( Bit.I, Bit.I ) ) ->
                { bitLeastSignificant = Bit.O, bitMostSignificant = Bit.I }


{-| The natural number plus a given natural number.

If the number you want to add to is 0-able and the natural number to add is positive,
use [`addAdapt`](#addAdapt)

-}
add :
    N0able (Negatable N1Up resultPositivePossiblyOrNever) addedN0PossiblyOrNever_
    ->
        (N0able (Negatable N1Up resultPositivePossiblyOrNever) resultPositivePossiblyOrNever
         -> N0able (Negatable N1Up resultPositivePossiblyOrNever) resultPositivePossiblyOrNever
        )
add toAdd =
    \integer ->
        case integer of
            N0able.N0 _ ->
                toAdd

            N0able.Not0 negatable ->
                case toAdd of
                    N0able.N0 _ ->
                        negatable |> N0able.Not0

                    N0able.Not0 toAddNegatable ->
                        case ( negatable.sign, toAddNegatable.sign ) of
                            ( Sign.Positive, Sign.Positive ) ->
                                { sign = Sign.Positive
                                , absolute = negatable.absolute |> n1UpAdd1Up toAddNegatable.absolute
                                }
                                    |> N0able.Not0

                            ( Sign.Negative negativePossiblyOrNever, Sign.Negative _ ) ->
                                { sign = Sign.Negative negativePossiblyOrNever
                                , absolute = negatable.absolute |> n1UpAdd1Up toAddNegatable.absolute
                                }
                                    |> N0able.Not0

                            ( Sign.Positive, Sign.Negative negativePossiblyOrNever ) ->
                                negatable.absolute
                                    |> n1UpSubtract toAddNegatable.absolute
                                    |> N0able.n0Adapt (\_ -> negativePossiblyOrNever)

                            ( Sign.Negative negativePossiblyOrNever, Sign.Positive ) ->
                                toAddNegatable.absolute
                                    |> n1UpSubtract negatable.absolute
                                    |> N0able.n0Adapt (\_ -> negativePossiblyOrNever)


n1UpSubtract : N1Up -> (N1Up -> N0able (Negatable N1Up Possibly) Possibly)
n1UpSubtract toSubtractN1Up =
    \n1Up ->
        let
            bitsDifference : { inRange : List Bit, overflow : Bit }
            bitsDifference =
                (Bit.I :: n1Up.bitsAfterI)
                    |> bitListSubtract (Bit.I :: toSubtractN1Up.bitsAfterI)
        in
        case bitsDifference.overflow of
            Bit.I ->
                { sign = Sign.Negative Possibly.Possible
                , absolute = { bitsAfterI = bitsDifference.inRange |> bitsUnpad }
                }
                    |> N0able.Not0
                    |> subtract1
                    |> N0able.not0Map
                        (\negatableComplement1Up ->
                            { sign = negatableComplement1Up.sign
                            , absolute = { bitsAfterI = negatableComplement1Up.absolute.bitsAfterI |> List.map Bit.opposite }
                            }
                        )

            Bit.O ->
                { sign = Sign.Positive
                , absolute = { bitsAfterI = bitsDifference.inRange |> bitsUnpad }
                }
                    |> N0able.Not0


bitListsToEquallySized : ( List Bit, List Bit ) -> List ( Bit, Bit )
bitListsToEquallySized =
    \( aBitList, bBitList ) ->
        let
            lengthMaximum : Int
            lengthMaximum =
                Basics.max
                    (aBitList |> List.length)
                    (bBitList |> List.length)
        in
        List.map2 Tuple.pair
            (aBitList |> List.Linear.padToAtLeast Linear.Down lengthMaximum (\l -> List.repeat l Bit.O))
            (bBitList |> List.Linear.padToAtLeast Linear.Down lengthMaximum (\l -> List.repeat l Bit.O))


bitListSubtract : List Bit -> (List Bit -> { inRange : List Bit, overflow : Bit })
bitListSubtract toSubtract =
    \bits ->
        -- relies on the same mechanism as 2s complement: adding the inverse + 1 is the same as subtracting it
        (Bit.O :: bits)
            |> bitListAddWithCarry Bit.I (Bit.I :: (toSubtract |> List.map Bit.opposite))


{-| Multiply it by another given integer.
If both are positive, the result type will be as well.
-}
multiplyBy : N0able (Negatable N1Up negative) n0 -> (N0able (Negatable N1Up negative) n0 -> N0able (Negatable N1Up negative) n0)
multiplyBy factor =
    -- TODO: There's a faster method for large numbers: karatsuba, see https://github.com/dwayne/elm-natural/blob/1.1.1/src/Natural.elm#L956C1
    \natural ->
        case natural of
            N0able.N0 possiblyOrNever ->
                N0able.N0 possiblyOrNever

            N0able.Not0 negatable ->
                case factor of
                    N0able.N0 possiblyOrNever ->
                        N0able.N0 possiblyOrNever

                    N0able.Not0 factorNegatable ->
                        N0able.Not0
                            { sign = negatable.sign |> Sign.multiplyBy factorNegatable.sign
                            , absolute = negatable.absolute |> n1UpMultiplyByN1Up factorNegatable.absolute
                            }


n1UpMultiplyByN1Up : N1Up -> (N1Up -> N1Up)
n1UpMultiplyByN1Up factor =
    \n1Up ->
        n1Up
            |> n1UpAdd
                (n1Up |> n1UpMultiplyBy (factor |> n1UpSubtract1 |> N0able.not0Map .absolute))


n1UpMultiplyBy : N0able N1Up n0PossiblyOrNever -> (N1Up -> N0able N1Up n0PossiblyOrNever)
n1UpMultiplyBy factor =
    \n1Up ->
        factor
            |> N0able.not0Map
                (\factorN1Up ->
                    n1Up |> n1UpMultiplyByN1Up factorN1Up
                )


n1UpAdd : N0able N1Up n0PossiblyOrNever_ -> (N1Up -> N1Up)
n1UpAdd toAdd =
    \n1Up ->
        case toAdd of
            N0able.N0 _ ->
                n1Up

            N0able.Not0 toAdd1Up ->
                n1Up |> n1UpAdd1Up toAdd1Up
