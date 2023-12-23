module Integer exposing
    ( Signed
    , fromInt, fromNatural
    , absolute, negate
    , toInt
    )

{-| Arbitrarily large whole number.

@docs Signed


## create

@docs fromInt, fromNatural


## alter

@docs absolute, negate


## transform

@docs toInt

-}

import N
import N0able exposing (N0able)
import Natural
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Sign exposing (Sign)


{-| Arbitrary-precision signed [integer](Integer), constructable from a [`Sign`](Sign#Sign)
and [`Natural.N1Up` bits](Natural#N1Up)
-}
type alias Signed =
    RecordWithoutConstructorFunction
        { sign : Sign
        , absolute : Natural.N1Up
        }


{-| Flip its [`Sign`](Sign#Sign)
-}
negate : N0able Signed n0PossiblyOrNever -> N0able Signed n0PossiblyOrNever
negate =
    \integer ->
        integer
            |> N0able.not0Map
                (\signed ->
                    { signed | sign = signed.sign |> Sign.opposite }
                )


{-| Remove its [`Sign`](Sign#Sign)
-}
absolute : N0able Signed n0PossiblyOrNever -> N0able Natural.N1Up n0PossiblyOrNever
absolute =
    \integer ->
        integer |> N0able.not0Map (\signed -> signed.absolute)



{-
   add : Integer -> (Integer -> Integer)
   add toAdd =
       \integer ->
           case ( integer, toAdd ) of
               ( Integer.N0, result ) ->
                   result

               ( Integer.Signed integerSigned, Integer.N0 ) ->
                   Integer.Signed integerSigned

               ( Integer.Signed integerSigned, Integer.Signed toAddSigned ) ->
                   integerSigned |> signedAdd toAddSigned


   signedAdd : Integer.Signed -> (Integer.Signed -> Integer)
   signedAdd toAdd =
       \signed ->
           case ( signed.sign, toAdd.sign ) of
               ( Positive, Positive ) ->
                   Integer.Signed { sign = Positive, absolute = signed.absolute |> Natural.AtLeast1.add toAdd.absolute }

               ( Negative, Negative ) ->
                   Integer.Signed { sign = Negative, absolute = signed.absolute |> Natural.AtLeast1.add toAdd.absolute }

               ( Negative, Positive ) ->
                   signed.absolute |> Natural.AtLeast1.subtract toAdd.absolute |> negate

               ( Positive, Negative ) ->
                   signed.absolute |> Natural.AtLeast1.subtract toAdd.absolute
-}


{-| Convert from a [natural number](Natural)
-}
fromNatural : N0able Natural.N1Up n0PossiblyOrNever -> N0able Signed n0PossiblyOrNever
fromNatural =
    \natural ->
        natural |> N0able.not0Map (\n1Up -> { sign = Sign.Positive, absolute = n1Up })


{-| Convert from an `Int`
-}
fromInt : Int -> N0able Signed Possibly
fromInt =
    \intBroad ->
        intBroad
            |> N.intToAbsolute
            |> Natural.fromN
            |> N0able.not0Map
                (\n1Up ->
                    { sign =
                        if intBroad > 0 then
                            Sign.Positive

                        else
                            -- intBroad < 0
                            Sign.Negative
                    , absolute = n1Up
                    }
                )


{-| Convert to an `Int`

Keep in mind that this can overflow
since `Int` is fixed in bit size while an [integer](Integer) is not.

-}
toInt : N0able Signed n0PossiblyOrNever_ -> Int
toInt =
    \integerNarrow ->
        case integerNarrow of
            N0able.N0 _ ->
                0

            N0able.Not0 signedValue ->
                signedValue.absolute
                    |> N0able.Not0
                    |> Natural.n0UpToN
                    |> N.toInt
                    |> signPrependToNumber signedValue.sign


{-|

  - `Negative` means negate
  - `Positive` means keep the current sign

-}
signPrependToNumber : Sign -> (number -> number)
signPrependToNumber sign =
    case sign of
        Sign.Negative ->
            Basics.negate

        Sign.Positive ->
            identity
