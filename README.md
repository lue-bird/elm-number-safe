## [`elm-numbers-safe`](https://dark.elm.dmy.fr/packages/lue-bird/elm-numbers-safe/latest/)... why?

- have types to represent percentages in the intervals `[0;1]`, `[0;1)`, `(0;1]`, `(0;1)`
- replace operations like
  ```elm
  divide : Number -> Number -> Maybe Number
  ```
  with
  ```elm
  operation : Number -> NumberNot0 -> Number
  ```
- have a common base type for 0-able, non-0, negatable and unsigned numbers so that operations can allow both
- have transparent definitions, aka have "parsed, not validated" data
    - no internal operation is "unsafe", in fact there are no real internal operations
    - users can safely create arbitrary numbers

## TODO
- `Rational` rounding, toFloat, toAtLeast
- `Integer` toAtLeast, divideBy, remainderBy
- complete `Percentage` including 1
- add more arithmetic operations
- change `Percentage` to base 2
- remove `module Decimal` (alternative: to base 2, add `toRational`)
