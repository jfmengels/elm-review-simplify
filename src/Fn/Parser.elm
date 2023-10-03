module Fn.Parser exposing (..)

import Elm.Syntax.ModuleName


andThen : ( Elm.Syntax.ModuleName.ModuleName, String )
andThen =
    ( [ "Parser" ], "andThen" )


backtrackable : ( Elm.Syntax.ModuleName.ModuleName, String )
backtrackable =
    ( [ "Parser" ], "backtrackable" )


chompIf : ( Elm.Syntax.ModuleName.ModuleName, String )
chompIf =
    ( [ "Parser" ], "chompIf" )


chompUntil : ( Elm.Syntax.ModuleName.ModuleName, String )
chompUntil =
    ( [ "Parser" ], "chompUntil" )


chompUntilEndOr : ( Elm.Syntax.ModuleName.ModuleName, String )
chompUntilEndOr =
    ( [ "Parser" ], "chompUntilEndOr" )


chompWhile : ( Elm.Syntax.ModuleName.ModuleName, String )
chompWhile =
    ( [ "Parser" ], "chompWhile" )


commit : ( Elm.Syntax.ModuleName.ModuleName, String )
commit =
    ( [ "Parser" ], "commit" )


deadEndsToString : ( Elm.Syntax.ModuleName.ModuleName, String )
deadEndsToString =
    ( [ "Parser" ], "deadEndsToString" )


end : ( Elm.Syntax.ModuleName.ModuleName, String )
end =
    ( [ "Parser" ], "end" )


float : ( Elm.Syntax.ModuleName.ModuleName, String )
float =
    ( [ "Parser" ], "float" )


getChompedString : ( Elm.Syntax.ModuleName.ModuleName, String )
getChompedString =
    ( [ "Parser" ], "getChompedString" )


getCol : ( Elm.Syntax.ModuleName.ModuleName, String )
getCol =
    ( [ "Parser" ], "getCol" )


getIndent : ( Elm.Syntax.ModuleName.ModuleName, String )
getIndent =
    ( [ "Parser" ], "getIndent" )


getOffset : ( Elm.Syntax.ModuleName.ModuleName, String )
getOffset =
    ( [ "Parser" ], "getOffset" )


getPosition : ( Elm.Syntax.ModuleName.ModuleName, String )
getPosition =
    ( [ "Parser" ], "getPosition" )


getRow : ( Elm.Syntax.ModuleName.ModuleName, String )
getRow =
    ( [ "Parser" ], "getRow" )


getSource : ( Elm.Syntax.ModuleName.ModuleName, String )
getSource =
    ( [ "Parser" ], "getSource" )


int : ( Elm.Syntax.ModuleName.ModuleName, String )
int =
    ( [ "Parser" ], "int" )


keyword : ( Elm.Syntax.ModuleName.ModuleName, String )
keyword =
    ( [ "Parser" ], "keyword" )


lazy : ( Elm.Syntax.ModuleName.ModuleName, String )
lazy =
    ( [ "Parser" ], "lazy" )


lineComment : ( Elm.Syntax.ModuleName.ModuleName, String )
lineComment =
    ( [ "Parser" ], "lineComment" )


loop : ( Elm.Syntax.ModuleName.ModuleName, String )
loop =
    ( [ "Parser" ], "loop" )


map : ( Elm.Syntax.ModuleName.ModuleName, String )
map =
    ( [ "Parser" ], "map" )


mapChompedString : ( Elm.Syntax.ModuleName.ModuleName, String )
mapChompedString =
    ( [ "Parser" ], "mapChompedString" )


multiComment : ( Elm.Syntax.ModuleName.ModuleName, String )
multiComment =
    ( [ "Parser" ], "multiComment" )


number : ( Elm.Syntax.ModuleName.ModuleName, String )
number =
    ( [ "Parser" ], "number" )


oneOf : ( Elm.Syntax.ModuleName.ModuleName, String )
oneOf =
    ( [ "Parser" ], "oneOf" )


problem : ( Elm.Syntax.ModuleName.ModuleName, String )
problem =
    ( [ "Parser" ], "problem" )


run : ( Elm.Syntax.ModuleName.ModuleName, String )
run =
    ( [ "Parser" ], "run" )


sequence : ( Elm.Syntax.ModuleName.ModuleName, String )
sequence =
    ( [ "Parser" ], "sequence" )


spaces : ( Elm.Syntax.ModuleName.ModuleName, String )
spaces =
    ( [ "Parser" ], "spaces" )


succeed : ( Elm.Syntax.ModuleName.ModuleName, String )
succeed =
    ( [ "Parser" ], "succeed" )


symbol : ( Elm.Syntax.ModuleName.ModuleName, String )
symbol =
    ( [ "Parser" ], "symbol" )


token : ( Elm.Syntax.ModuleName.ModuleName, String )
token =
    ( [ "Parser" ], "token" )


variable : ( Elm.Syntax.ModuleName.ModuleName, String )
variable =
    ( [ "Parser" ], "variable" )


withIndent : ( Elm.Syntax.ModuleName.ModuleName, String )
withIndent =
    ( [ "Parser" ], "withIndent" )