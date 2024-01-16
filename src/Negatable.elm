module Negatable exposing (Negatable, absolute, negate, negativeAdapt, toNotNegative)

import N0able exposing (N0able)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign


{-| Examples

signed [integer](Integer), constructable from a [`Sign`](Sign#Sign)
and [`Integer.N1Up` bits](Natural#N1Up):

    Negatable Integer.N1Up Possible

-}
type alias Negatable absolute negativeSignPossiblyOrNever =
    RecordWithoutConstructorFunction
        { sign : Sign.Negatable negativeSignPossiblyOrNever
        , absolute : absolute
        }


{-| Flip its [`Sign`](Sign#Sign)
-}
negate :
    N0able (Negatable absolute negativeSignPossiblyOrNever_) n0PossiblyOrNever
    -> N0able (Negatable absolute Possibly) n0PossiblyOrNever
negate =
    \n0ableNegatable ->
        n0ableNegatable
            |> N0able.not0Map
                (\negatable ->
                    { sign = negatable.sign |> Sign.opposite
                    , absolute = negatable.absolute
                    }
                )


{-| Remove its [`Sign`](Sign#Sign)
-}
absolute :
    N0able (Negatable absolute negativePossiblyOrNever_) n0PossiblyOrNever
    -> N0able (Negatable absolute negativeNever_) n0PossiblyOrNever
absolute =
    \n0ableNegatable ->
        n0ableNegatable
            |> N0able.not0Map
                (\negatable -> { sign = Sign.Positive, absolute = negatable.absolute })


toNotNegative :
    N0able (Negatable absolute negativePossiblyOrNever_) n0PossiblyOrNever
    -> Maybe (N0able (Negatable absolute negativeNever_) n0PossiblyOrNever)
toNotNegative =
    \n0ableNegatable ->
        case n0ableNegatable of
            N0able.N0 possiblyOrNever ->
                N0able.N0 possiblyOrNever |> Just

            N0able.Not0 negatable ->
                case negatable.sign of
                    Sign.Negative _ ->
                        Nothing

                    Sign.Positive ->
                        { sign = Sign.Positive, absolute = negatable.absolute } |> N0able.Not0 |> Just


negativeAdapt :
    (negativePossiblyOrNever -> adaptedNegativePossiblyOrNever)
    ->
        (N0able (Negatable absolute negativePossiblyOrNever) n0PossiblyOrNever
         -> N0able (Negatable absolute adaptedNegativePossiblyOrNever) n0PossiblyOrNever
        )
negativeAdapt negativePossiblyOrNeverChange =
    \n0ableNegatable ->
        n0ableNegatable
            |> N0able.not0Map
                (\negatable ->
                    { sign = negatable.sign |> Sign.negativeAdapt negativePossiblyOrNeverChange
                    , absolute = negatable.absolute
                    }
                )
