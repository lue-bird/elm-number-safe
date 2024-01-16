module Sign exposing
    ( Negatable(..)
    , compare
    , opposite, multiplyBy
    , negativeAdapt
    )

{-| Negative or positive.

@docs Negatable


## observe

@docs compare


## alter

@docs opposite, multiplyBy


### type-level

@docs negativeAdapt

-}

import Possibly exposing (Possibly)


{-| A number's sign

  - minus-signed: `Negative Possibly|Never`
  - plus-signed: `Positive`

-}
type Negatable negativePossiblyOrNever
    = Negative negativePossiblyOrNever
    | Positive


{-| Order the [signs](#Negatable) as `Negative ... < Positive`
-}
compare : ( Negatable aNegative_, Negatable bNegative_ ) -> Order
compare =
    \signs ->
        case signs of
            ( Positive, Positive ) ->
                Basics.EQ

            ( Negative _, Positive ) ->
                Basics.LT

            ( Positive, Negative _ ) ->
                Basics.GT

            ( Negative _, Negative _ ) ->
                Basics.EQ


{-| Flip the [`Sign`](#Sign) `Negative` ↔ `Positive`

    Integer.negate =
        N0able.not0Map (\s -> { s | sign = opposite s.sign })

-}
opposite : Negatable negativePossiblyOrNever_ -> Negatable Possibly
opposite =
    \sign ->
        case sign of
            Negative _ ->
                Positive

            Positive ->
                Negative Possibly.Possible


{-| The resulting of sign when multiplying numbers with given signs.
-}
multiplyBy : Negatable negativePossiblyOrNever -> (Negatable negativePossiblyOrNever -> Negatable negativePossiblyOrNever)
multiplyBy factorSign =
    \sign ->
        case sign of
            Positive ->
                factorSign

            Negative possible ->
                case factorSign of
                    Negative _ ->
                        Positive

                    Positive ->
                        Negative possible


negativeAdapt :
    (negativePossiblyOrNever -> adaptedNegative0PossiblyOrNever)
    -> (Negatable negativePossiblyOrNever -> Negatable adaptedNegative0PossiblyOrNever)
negativeAdapt n0PossiblyOrNeverChange =
    \sign ->
        case sign of
            Negative possiblyOrNever ->
                possiblyOrNever |> n0PossiblyOrNeverChange |> Negative

            Positive ->
                Positive
