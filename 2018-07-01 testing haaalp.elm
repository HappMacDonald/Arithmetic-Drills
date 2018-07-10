import Html
import FindRecord exposing (findRecord)


type Operation
  = Plus
  | Minus
  | Times
  | DividedBy


type alias OperationNames =
  { operation : Operation
  , string : String
  , hash : String
  , symbol : String
  , label : String
  }


operationRecordBuilder : (Operation, String, String) -> OperationNames
operationRecordBuilder (oper, symbol, label) =
  { operation = oper
  , string = toString oper
  , hash = "#" ++ toString oper
  , symbol = symbol
  , label = symbol ++ " " ++ label
  }


defaultOperation : OperationNames
defaultOperation = operationRecordBuilder ( Plus, "+", "Addition" )


operationGrid : List OperationNames
operationGrid =
  (++) [defaultOperation]
  <| List.map operationRecordBuilder
    [ ( Minus, "-", "Subtraction" )
    , ( Times, "x", "Multiplication" )
    , ( DividedBy, "÷", "Division" )
    ]


operationFind : ( OperationNames -> keyContent ) -> keyContent -> OperationNames
operationFind keyGetter keyContent =
  findRecord keyGetter keyContent operationGrid |> Maybe.withDefault defaultOperation


main : Program Never Int msg
main =
  Html.beginnerProgram
    { model = 0
    , view = (\_ -> Html.text (operationFind .hash "#DividedBy" |> .operation |> toString) )
    , update = (\_ b -> b)
    }

