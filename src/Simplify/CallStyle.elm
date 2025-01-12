module Simplify.CallStyle exposing (FunctionCallStyle(..), LeftOrRightDirection(..))

{-|

@docs FunctionCallStyle, LeftOrRightDirection

-}


{-| How an argument is given as input to a function:

  - `Pipe RightToLeft`: `function <| argument`
  - `Pipe LeftToRight`: `argument |> function`
  - `Application`: `function argument`

-}
type FunctionCallStyle
    = Application
    | Pipe LeftOrRightDirection


type LeftOrRightDirection
    = RightToLeft
    | LeftToRight
