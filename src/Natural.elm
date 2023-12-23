module Natural exposing
    ( N1Up
    , n1, n2, fromN
    , add, addAdapt, subtract1, multiplyBy
    , n0UpToN, n1UpToN
    )

{-| Arbitrarily-sized natural number ≥ 0 or 1.

@docs N1Up


## create

@docs n1, n2, fromN


## alter

@docs add, addAdapt, subtract1, multiplyBy


## transform

@docs n0UpToN, n1UpToN

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
import N exposing (Min, N, Up0, Up1)
import N0able exposing (N0able)
import PartialOrComplete exposing (PartialOrComplete)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


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
n1 : N0able N1Up n0Never_
n1 =
    N0able.Not0 { bitsAfterI = [] }


{-| The positive natural number 2
-}
n2 : N0able N1Up n0Never_
n2 =
    n1 |> add n1


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
n0UpToN : N0able N1Up n0PossiblyOrNever_ -> N (Min (Up0 minX_))
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
n1UpToN : N0able N1Up Never -> N (Min (Up1 minX_))
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

    factorial : N0able Natural.N1Up Possibly -> N0able Natural.N1Up n0Never_
    factorial =
        \natural ->
            case natural of
                N0able.N0 _ ->
                    Natural.n1

                N0able.Not0 n1Up ->
                    let
                        naturalPositive : N0able Natural.N1Up n0Never_
                        naturalPositive =
                            N0able.Not0 n1Up
                    in
                    factorial (naturalPositive |> Natural.subtract1)
                        |> Natural.multiplyBy naturalPositive

-}
subtract1 : N0able N1Up Never -> N0able N1Up Possibly
subtract1 =
    \natural1Up ->
        natural1Up |> N0able.toNot0 |> n1UpSubtract1


n1UpSubtract1 : N1Up -> N0able N1Up Possibly
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
                N0able.Not0 { bitsAfterI = afterISubtracted }

            -- n1Up.bitsAfterI was empty → n1Up was 1
            PartialOrComplete.Partial [] ->
                N0able.n0

            -- first I bit needs to be subtracted
            PartialOrComplete.Partial (firstAfterI :: afterFirstAfterI) ->
                N0able.Not0 { bitsAfterI = firstAfterI :: afterFirstAfterI }


{-| Sum 2 natural numbers where what you want to add to is 0-able and what you want to add is positive.
If this is not the case, [`add`](#add) will do.

On the type-level, this is exactly like [`Stack.attachAdapt`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack#attachAdapt)

-}
addAdapt : N0able N1Up n0PossiblyOrNever -> (N0able N1Up addedN0PossiblyOrNever_ -> N0able N1Up n0PossiblyOrNever)
addAdapt toAdd =
    \natural -> toAdd |> add natural


n1UpAdd : N0able N1Up n0PossiblyOrNever_ -> (N1Up -> N1Up)
n1UpAdd toAdd =
    \n1Up ->
        case toAdd of
            N0able.N0 _ ->
                n1Up

            N0able.Not0 toAdd1Up ->
                n1Up |> n1UpAdd1Up toAdd1Up


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
    \bits ->
        let
            lengthMaximum : Int
            lengthMaximum =
                Basics.max
                    (bits |> List.length)
                    (toAdd |> List.length)

            addResult : { mapped : List Bit, folded : Bit }
            addResult =
                List.map2 Tuple.pair
                    (bits |> List.Linear.padToAtLeast Linear.Down lengthMaximum (\l -> List.repeat l Bit.O))
                    (toAdd |> List.Linear.padToAtLeast Linear.Down lengthMaximum (\l -> List.repeat l Bit.O))
                    |> List.Linear.mapFoldFrom Bit.O
                        Linear.Down
                        (\step ->
                            case ( step.element, step.folded ) of
                                ( ( Bit.O, Bit.O ), overflowSoFar ) ->
                                    { element = overflowSoFar, folded = Bit.O }

                                ( ( Bit.I, Bit.I ), overflowSoFar ) ->
                                    { element = overflowSoFar, folded = Bit.I }

                                ( ( Bit.I, Bit.O ), Bit.O ) ->
                                    { element = Bit.I, folded = Bit.O }

                                ( ( Bit.O, Bit.I ), Bit.O ) ->
                                    { element = Bit.I, folded = Bit.O }

                                ( ( Bit.I, Bit.O ), Bit.I ) ->
                                    { element = Bit.O, folded = Bit.I }

                                ( ( Bit.O, Bit.I ), Bit.I ) ->
                                    { element = Bit.O, folded = Bit.I }
                        )
        in
        { inRange = addResult.mapped, overflow = addResult.folded }


{-| The natural number plus a given natural number.

If the number you want to add to is 0-able and the natural number to add is positive,
use [`addAdapt`](#addAdapt)

-}
add : N0able N1Up addedN0PossiblyOrNever_ -> (N0able N1Up n0PossiblyOrNever -> N0able N1Up n0PossiblyOrNever)
add toAdd =
    \natural ->
        case natural of
            N0able.N0 _ ->
                n1

            N0able.Not0 n1Up ->
                n1Up |> n1UpAdd toAdd |> N0able.Not0


n1UpMultiplyBy : N0able N1Up n0PossiblyOrNever -> (N1Up -> N0able N1Up n0PossiblyOrNever)
n1UpMultiplyBy factor =
    \n1Up ->
        factor
            |> N0able.not0Map
                (\factorN1Up ->
                    n1Up |> n1UpMultiplyByN1Up factorN1Up
                )


{-| Multiply the natural number by a given natural number.
-}
multiplyBy : N0able N1Up n0PossiblyOrNever -> (N0able N1Up n0PossiblyOrNever -> N0able N1Up n0PossiblyOrNever)
multiplyBy factor =
    -- TODO: There's probably a faster method
    \natural ->
        case natural of
            N0able.N0 possiblyOrNever ->
                N0able.N0 possiblyOrNever

            N0able.Not0 n1Up ->
                n1Up |> n1UpMultiplyBy factor


n1UpMultiplyByN1Up : N1Up -> (N1Up -> N1Up)
n1UpMultiplyByN1Up factor =
    \n1Up ->
        n1Up
            |> n1UpAdd
                (n1Up
                    |> n1UpMultiplyBy (factor |> N0able.Not0 |> subtract1)
                )
