module Percentage exposing (PercentageAbove0ToBelow1, toFloat)

{-| Number in range `(0,1)` like [`Percentage`](Percentage#Percentage) but 0 and 1 excluded.

@docs PercentageAbove0ToBelow1, toFloat

-}

import N exposing (In, N, N0, N1, N9, n0)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


{-| Only some digits after the decimal point. Can't be none.

So not "rational number with a numerator and denominator" or "number in range [0;1]" –
it means: the absolute value of a written decimal number without a period, like 0.345 or 0.001

-}
type alias PercentageAbove0ToBelow1 =
    RecordWithoutConstructorFunction
        { beforeEnd : List (N (In N0 N9))
        , end : N (In N1 N9)
        }


{-| Convert to an [`elm/core` `Float`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#Float).
-}
toFloat : PercentageAbove0ToBelow1 -> Float
toFloat =
    \percentageAbove0ToBelow1 ->
        percentageAbove0ToBelow1.beforeEnd
            ++ [ percentageAbove0ToBelow1.end |> N.inToOn |> N.minTo n0 |> N.inToNumber ]
            |> List.indexedMap
                (\decimal digit ->
                    (digit |> N.toFloat)
                        * (10 ^ -(1 + (decimal |> Basics.toFloat)))
                )
            |> List.sum
