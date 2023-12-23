module Sign exposing
    ( Sign(..)
    , opposite, multiplyBy
    )

{-| Negative or positive.

@docs Sign


## alter

@docs opposite, multiplyBy

-}


{-| A number's sign

  - minus-signed: `Negative`
  - plus-signed: `Positive`

-}
type Sign
    = Negative
    | Positive


{-| Flip the [`Sign`](#Sign) `Negative` ↔ `Positive`

    Integer.negate =
        N0able.not0Map (\s -> { s | sign = opposite s.sign })

-}
opposite : Sign -> Sign
opposite =
    \sign ->
        case sign of
            Negative ->
                Positive

            Positive ->
                Negative


{-| The resulting of sign when multiplying numbers with given signs.
-}
multiplyBy : Sign -> (Sign -> Sign)
multiplyBy factorSign =
    \sign ->
        case sign of
            Positive ->
                factorSign

            Negative ->
                case factorSign of
                    Negative ->
                        Positive

                    Positive ->
                        Negative
