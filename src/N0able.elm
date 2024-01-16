module N0able exposing
    ( N0able(..)
    , n0
    , not0Map
    , n0Adapt
    , toEmptiableNot0
    , toNot0
    )

{-| 0 or hero.

@docs N0able


## create

@docs n0


## alter

@docs not0Map


### type-level

@docs n0Adapt


## transform

@docs toEmptiableNot0


## safe internals

@docs toNot0

-}

import Emptiable exposing (Emptiable)
import Possibly exposing (Possibly(..))


{-| This type is used for both numbers that do and don't allow 0.
The second type argument specifies how much we know about it being 0-able.

The concept is exactly the same as with [`Emptiable`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable),
allowing us to have a much smaller API surface and allowing you to save conversions.
Taking an integer type as an example:


### value types

    N0able Integer.Signed freeVariable_ -- ≠ 0

    N0able Integer.Signed Possibly -- can be 0


### argument types

    N0able Integer.Signed Never -- ≠ 0

    N0able Integer.Signed freeVariable_ -- can be 0

-}
type N0able not0 n0PossiblyOrNever
    = N0 n0PossiblyOrNever
    | Not0 not0


{-| 0 that conforms to any number type like `N0able Integer.Signed Possibly` or `N0able Integer.N1Up Possibly`
-}
n0 : N0able not0_ Possibly
n0 =
    N0 Possible


{-| Intended for package authors. If the number is not 0, change it using a given function based on its current value.

    integerToNatural : N0able Integer.Signed n0 -> N0able Integer.N1Up n0
    integerToNatural =
        N0able.not0Map .absolute

-}
not0Map : (not0 -> mappedNot0) -> (N0able not0 n0PossiblyOrNever -> N0able mappedNot0 n0PossiblyOrNever)
not0Map not0Change =
    \n0able ->
        case n0able of
            N0 possiblyOrNever ->
                N0 possiblyOrNever

            Not0 not0 ->
                not0 |> not0Change |> Not0


{-| Usually used to take a `N0able ... Never` argument and make a `N0able ... freeVariable_` value out of it

    natural1UpToIntegerSigned : N0able Integer.N1Up Never -> N0able Integer.Signed freeVariable_
    natural1UpToIntegerSigned =
        N0able.n0adapt never >> Integer.fromNatural

Check out [`Emptiable.emptyAdapt`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable#emptyAdapt)
for more details and examples

-}
n0Adapt :
    (n0PossiblyOrNever -> adaptedN0PossiblyOrNever)
    -> (N0able not0 n0PossiblyOrNever -> N0able not0 adaptedN0PossiblyOrNever)
n0Adapt n0PossiblyOrNeverChange =
    \n0able ->
        case n0able of
            N0 possiblyOrNever ->
                possiblyOrNever |> n0PossiblyOrNeverChange |> N0

            Not0 not0 ->
                not0 |> Not0


{-| [`empty`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable#empty) if its [`n0`](#n0),
else [`Emptiable.filled`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable#filled)
with the wrapped non-0 number representation.

    import Stack -- lue-bird/elm-emptiness-typed

    Stack.fromList [ Integer.n1, N0able.n0 ]
        |> Stack.map N0able.toEmptiableNot0
        |> Stack.fills
    --> Stack.fromList [ Integer.n1 ]

-}
toEmptiableNot0 : N0able not0 n0PossiblyOrNever -> Emptiable (N0able not0 n0Never_) n0PossiblyOrNever
toEmptiableNot0 =
    \n0able ->
        case n0able of
            Not0 not0 ->
                not0 |> Not0 |> Emptiable.filled

            N0 possiblyOrNever ->
                possiblyOrNever |> Emptiable.Empty


{-| For package authors: Get the raw representation of the non-0 number.

    { one = () } |> N0able.Not0 |> N0able.toNot0
    --> { one = () }

Nothing spooky going on here, just [`Basics.never`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#never) doing it's job.
Read more in [allowable state](https://dark.elm.dmy.fr/packages/lue-bird/elm-allowable-state/latest/)

-}
toNot0 : N0able not0 Never -> not0
toNot0 =
    \n0able ->
        case n0able of
            Not0 not0 ->
                not0

            N0 ever ->
                never ever
