## [`elm-numbers-safe`](https://dark.elm.dmy.fr/packages/lue-bird/elm-numbers-safe/latest/)... why?

- have types to represent percentages in the intervals `[0;1]`, `[0;1)`, `(0;1]`, `(0;1)`
- replace all
  ```elm
  operation : Number -> Number -> Maybe Number
  ```
  like divide/remainder
  with
  ```elm
  operation : Number -> NumberNot0 -> Maybe Number
  ```
- have a common base type for `Number` and `NumberNot0` so that operations allow both
- have transparent definitions, aka have "parsed, not validated" data
    - no internal operation is "unsafe", in fact there are no real internal operations
    - users can safely create arbitrary numbers

## TODO
- complete `Percentage` including 1
- add more arithmetic operations
- change `Percentage` to base 2
- remove `module Decimal` (alternative: to base 2)
