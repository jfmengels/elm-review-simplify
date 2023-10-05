module Fn.Regex exposing (..)

import Elm.Syntax.ModuleName


contains : ( Elm.Syntax.ModuleName.ModuleName, String )
contains =
    ( [ "Regex" ], "contains" )


find : ( Elm.Syntax.ModuleName.ModuleName, String )
find =
    ( [ "Regex" ], "find" )


findAtMost : ( Elm.Syntax.ModuleName.ModuleName, String )
findAtMost =
    ( [ "Regex" ], "findAtMost" )


fromString : ( Elm.Syntax.ModuleName.ModuleName, String )
fromString =
    ( [ "Regex" ], "fromString" )


fromStringWith : ( Elm.Syntax.ModuleName.ModuleName, String )
fromStringWith =
    ( [ "Regex" ], "fromStringWith" )


never : ( Elm.Syntax.ModuleName.ModuleName, String )
never =
    ( [ "Regex" ], "never" )


replace : ( Elm.Syntax.ModuleName.ModuleName, String )
replace =
    ( [ "Regex" ], "replace" )


replaceAtMost : ( Elm.Syntax.ModuleName.ModuleName, String )
replaceAtMost =
    ( [ "Regex" ], "replaceAtMost" )


split : ( Elm.Syntax.ModuleName.ModuleName, String )
split =
    ( [ "Regex" ], "split" )


splitAtMost : ( Elm.Syntax.ModuleName.ModuleName, String )
splitAtMost =
    ( [ "Regex" ], "splitAtMost" )
