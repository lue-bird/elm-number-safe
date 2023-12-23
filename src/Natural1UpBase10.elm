module Natural1UpBase10 exposing
    ( Natural1UpBase10
    , fromIntPositive
    , fromBase2, toBase2
    )

{-| Package-internal helpers for [`Natural.N1Up`](Natural#N1Up)
but represented as it's decimal digits.
**Should not be exposed**

@docs Natural1UpBase10


## int positive

@docs fromIntPositive


## base 2

@docs fromBase2, toBase2

-}

import Bit exposing (Bit)
import Linear
import List.Linear
import N exposing (Add1, In, N, N0, N1, N9, On, Up0, Up9, n0, n1, n10, n2, n9)
import N.Local exposing (N19)
import Natural
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


type alias Natural1UpBase10 =
    RecordWithoutConstructorFunction
        { first : N (In N1 N9)
        , afterFirst : List (N (In N0 N9))
        }


toBase2 : Natural1UpBase10 -> Natural.N1Up
toBase2 =
    \naturalAtLeast1Base10 ->
        let
            base2Digits : List Bit
            base2Digits =
                (naturalAtLeast1Base10.first |> N.minToOn |> N.minTo n0 |> N.minToNumber)
                    :: naturalAtLeast1Base10.afterFirst
                    |> digitsToBase2
                    |> List.reverse
        in
        base2Digits
            |> bitsUnpad
            -- not possible because the base10 number is ≥ 1
            |> Maybe.withDefault
                -- 1
                { bitsAfterI = [] }


digitsToBase2 : List (N (In N0 N9)) -> List Bit
digitsToBase2 =
    \digits ->
        let
            digitsDivisionBy2 : { divided : List (N (In N0 N9)), remainder : N (In (On N0) (On N1)) }
            digitsDivisionBy2 =
                digits |> digitsDivideBy2
        in
        (digitsDivisionBy2.remainder |> Bit.fromN)
            :: (case digitsDivisionBy2.divided |> digitsUnpad of
                    [] ->
                        []

                    dividedHead :: dividedTail ->
                        (dividedHead :: dividedTail) |> digitsToBase2
               )


digitToNumber :
    ( N (In (On newMin) (N.Up newMinMaxToMin_ N.To min))
    , N (In (On newMaxMin) (On newMax))
    )
    -> N (In (On min) (N.Up maxToNewMaxMin_ N.To newMaxMin))
    -> N (In newMin newMax)
digitToNumber ( low, high ) =
    \digit1To9 ->
        digit1To9 |> N.minTo low |> N.maxTo high |> N.inToNumber


digitToUp :
    ( N (In newMin (N.Up newMinMaxToMin_ N.To min)), N (In (On max) newMax) )
    -> (N (In min max) -> N (In newMin newMax))
digitToUp ( low, high ) =
    \digit1To9 ->
        digit1To9 |> N.inToOn |> N.minTo low |> N.maxTo high


digitsDivideBy2 :
    List (N (In N0 N9))
    ->
        { divided : List (N (In N0 N9))
        , remainder : N (In (On N0) (On N1))
        }
digitsDivideBy2 =
    \digits ->
        let
            divisionResult : { mapped : List (N (In N0 (N.N0OrAdd1 Never (N.N0OrAdd1 Never (N.N0OrAdd1 Never (N.N0OrAdd1 Never (N.N0OrAdd1 Never (N.N0OrAdd1 Never (N.N0OrAdd1 Never (N.N0OrAdd1 Never (N.N0OrAdd1 Never N0))))))))))), folded : N (In (Up0 N0) (N.Up1 maxX_)) }
            divisionResult =
                digits
                    |> List.Linear.mapFoldFrom (n0 |> N.maxTo n1)
                        Linear.Up
                        (\step ->
                            let
                                intermediate : N (N.Min (On N0))
                                intermediate =
                                    step.folded |> N.multiplyBy n10 |> N.addMin (step.element |> digitToUp ( n0, n9 ))
                            in
                            { element =
                                intermediate
                                    |> N.divideBy n2
                                    -- we can safely clamp to 0..9 because 19/2 is still below 10
                                    |> N.toIn ( n0, n9 )
                                    |> digitToNumber ( n0, n9 )
                            , folded = intermediate |> N.remainderBy n2
                            }
                        )
        in
        { remainder = divisionResult.folded
        , divided = divisionResult.mapped
        }


digitsUnpad : List (N range) -> List (N range)
digitsUnpad =
    \digits ->
        case digits of
            [] ->
                []

            first :: afterFirst ->
                case first |> N.toInt of
                    0 ->
                        afterFirst |> digitsUnpad

                    _ ->
                        first :: afterFirst


bitsUnpad : List Bit -> Maybe Natural.N1Up
bitsUnpad =
    \bits ->
        case bits of
            [] ->
                Nothing

            first :: afterFirst ->
                case first of
                    Bit.O ->
                        afterFirst |> bitsUnpad

                    Bit.I ->
                        Just { bitsAfterI = afterFirst }


add :
    Natural1UpBase10
    ->
        (Natural1UpBase10
         -> Natural1UpBase10
        )
add toAdd =
    \naturalAtLeast1Base10 ->
        let
            digitsSum :
                { inRange : List (N (In N0 N9))
                , overflow : N (In (On N0) (On N1))
                }
            digitsSum =
                (naturalAtLeast1Base10 |> toDigits)
                    |> addDigits (toAdd |> toDigits)
        in
        case digitsSum.overflow |> N.toInt of
            0 ->
                case digitsSum.inRange of
                    -- can't happen
                    [] ->
                        naturalAtLeast1Base10

                    first :: afterFirst ->
                        { first =
                            -- we can do that because both numbers start with a positive digit
                            -- and the overflow is 0
                            first |> N.toIn ( n1, n9 ) |> N.inToNumber
                        , afterFirst = afterFirst
                        }

            -- 1
            _ ->
                { first = n1 |> N.maxTo n9 |> N.inToNumber
                , afterFirst = digitsSum.inRange
                }


toDigits : Natural1UpBase10 -> List (N (In N0 N9))
toDigits =
    \naturalAtLeast1Base10 ->
        (naturalAtLeast1Base10.first |> N.inToOn |> N.minTo0 |> N.inToNumber)
            :: naturalAtLeast1Base10.afterFirst


addDigits :
    List (N (In N0 N9))
    ->
        (List (N (In N0 N9))
         ->
            { inRange : List (N (In N0 N9))
            , overflow : N (In (On N0) (On N1))
            }
        )
addDigits toAdd =
    \digits ->
        let
            lengthMaximum : Int
            lengthMaximum =
                Basics.max
                    (digits |> List.length)
                    (toAdd |> List.length)

            toAddPadded : List (N (In N0 N9))
            toAddPadded =
                toAdd
                    |> List.Linear.padToAtLeast Linear.Down
                        lengthMaximum
                        (\l -> List.repeat l (n0 |> N.maxTo n9 |> N.inToNumber))

            digitsPadded : List (N (In N0 N9))
            digitsPadded =
                digits
                    |> List.Linear.padToAtLeast Linear.Down
                        lengthMaximum
                        (\l -> List.repeat l (n0 |> N.maxTo n9 |> N.inToNumber))

            addResult : { mapped : List (N (In N0 N9)), folded : N (In (On N0) (On N1)) }
            addResult =
                List.map2 Tuple.pair digitsPadded toAddPadded
                    |> List.Linear.mapFoldFrom (n0 |> N.maxTo n1)
                        Linear.Down
                        (\step ->
                            let
                                ( digit, digitToAdd ) =
                                    step.element

                                stepSum : N (In (On N0) (On N19))
                                stepSum =
                                    step.folded
                                        |> N.add (digit |> digitToUp ( n0, n9 ))
                                        |> N.add (digitToAdd |> digitToUp ( n0, n9 ))
                            in
                            case stepSum |> N.isAtLeast n10 of
                                Ok digitSumAtLeast10 ->
                                    { element = digitSumAtLeast10 |> N.remainderBy n10 |> N.inToNumber
                                    , folded = n1 |> N.minTo n0
                                    }

                                Err digitSumAtMost9 ->
                                    { element = digitSumAtMost9 |> N.inToNumber, folded = n0 |> N.maxTo n1 }
                        )
        in
        { inRange = addResult.mapped
        , overflow = addResult.folded
        }


fromBase2 : Natural.N1Up -> Natural1UpBase10
fromBase2 =
    \digits ->
        digits.bitsAfterI
            |> List.foldl
                (\bit soFar ->
                    case bit of
                        Bit.O ->
                            soFar |> multiplyBy2

                        Bit.I ->
                            soFar |> multiplyBy2 |> add (fromDigit n1)
                )
                (fromDigit n1)


fromDigit : N (In (On (Add1 maxX_)) (N.Up maxTo9_ N.To N9)) -> Natural1UpBase10
fromDigit digit =
    { first = digit |> digitToNumber ( n1, n9 )
    , afterFirst = []
    }


multiplyBy2 : Natural1UpBase10 -> Natural1UpBase10
multiplyBy2 =
    \naturalAtLeast1Base10 ->
        naturalAtLeast1Base10 |> add naturalAtLeast1Base10


fromIntPositive : Int -> Natural1UpBase10
fromIntPositive =
    \int ->
        let
            highest10Exponent : Int
            highest10Exponent =
                logBase 10 (int |> Basics.toFloat) |> floor
        in
        { first = int |> digitFor10Exponent highest10Exponent |> N.toIn ( n1, n9 ) |> N.inToNumber
        , afterFirst =
            List.range 0 highest10Exponent
                |> List.map
                    (\n10Exponent ->
                        int |> digitFor10Exponent n10Exponent |> N.inToNumber
                    )
        }


digitFor10Exponent : Int -> (Int -> N (In (Up0 minX_) (Up9 maxX_)))
digitFor10Exponent n10Exponent =
    \int ->
        (int // (10 ^ n10Exponent))
            |> N.intModBy n10
