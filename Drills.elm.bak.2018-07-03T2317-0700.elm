module Drills exposing (main)

{-| Web app that acts as arithmetic flashcards
-}


import Html exposing
  ( Html
  , div
  , ul
  , ol
  , li
  , section
  , h1
  , text
  )
import Html.Attributes as Attr
import Navigation as Nav
import FindRecord exposing
  ( findRecord
  )


-- PRIMARY DECLARATION

main : Program Never Model Msg
main = Nav.program
  UrlChange
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }


-- HELPERS AND DEFINITIONS

{-| Types of arithmetic operator. Used as navigation, tabs along top.
-}
type Operation
  = Plus
  | Minus
  | Times
  | DividedBy


{-| A record of all facts related to a specific Operation
-}
type alias OperationNames =
  { operation : Operation
  , string : String
  , hash : String
  , symbol : String
  , label : String
  }


{-| A function that expands the compressed, unique facts about an operation
into a complete, searchable record.
-}
operationRecordBuilder : (Operation, String, String) -> OperationNames
operationRecordBuilder (oper, symbol, label) =
  { operation = oper
  , string = toString oper
  , hash = "#" ++ toString oper
  , symbol = symbol
  , label = symbol ++ " " ++ label
  }


{-| In the hypothetical case where an Operation record lookup fails,
this first record (set apart from the rest) will serve as the fallback
lookup result.
-}
defaultOperation : OperationNames
defaultOperation =
  operationRecordBuilder ( Plus, "+", "Addition" )


{-| The initial/default record is concattenated with all other
Operation records
-}
operationRecords : List OperationNames
operationRecords =
  [defaultOperation]
  ++ List.map operationRecordBuilder
    [ ( Minus, "-", "Subtraction" )
    , ( Times, "x", "Multiplication" )
    , ( DividedBy, "÷", "Division" )
    ]



{-| This function looks up Operator record based on any key/value pair.

    operationFind .symbol "x" |> .hash == "#Times"
-}
operationFind : ( OperationNames -> key ) -> key -> OperationNames
operationFind keyGetter key =
  findRecord keyGetter key operationRecords
  |> Maybe.withDefault defaultOperation


{-| Even more compressed convenience function for looking up
operation messages from navigational hashes.
-}
operationFromHash : String -> Operation
operationFromHash hashValue ->
  operationFind .hash hashValue |> .oper


{-| These values define the parts of the flashcard equation
-}
type alias Lvalue = Int


type alias Rvalue = Int


type alias Problem =
  { value : Lvalue
  , operation : Operation
  , rvalue : Rvalue
  }


-- MODEL

type alias Model =
  { tab : Operation
  , currentProblem : Problem
  } 


-- INIT

init : Nav.Location -> (Model, Cmd Msg)
init location =
  ( { tab =
        operationFromHash location.hash
    , currentProblem =
        Problem 2 Plus 2
    }
    , Cmd.none
  )


-- UPDATE

type Msg
  = UrlChange Nav.Location
  | ChangeTab Operation
--  | GiveAnswer Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UrlChange location ->
      update
        ( ChangeTab
        <| operationFromHash location.hash
        )
        model
      
    ChangeTab operation ->
      ( { model
        | tab = operation
        }
      , Cmd.none
      )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- VIEW

{-| For now, just whether or not tab is active.
-}
tabAttr : Model -> Operation -> List (Html.Attribute Msg)
tabAttr model thisOperation = 
  if model.tab == thisOperation then
    [ Attr.class "is-active"
    ]
  else
    []


{-| Creates HTML Anchor (hyperlink) element for the given operation,
complete with link to correct nav hash and text label that includes
the operation symbol and name.
-}
tabAnchor : Model -> Operation -> Html Msg
tabAnchor model operator =
  let
    record =
      operatorLookup .operator operator
  in
    Html.a
    [ Attr.href
    <| record.hash
    ]
    [ text record.label
    ]


view : Model -> Html Msg
view model =
  div
    [
    ]
    [ Html.node
      "link"
      [ Attr.rel "stylesheet"
      , Attr.href "bulma.min.css"
      ]
      [
      ]
    , section [ Attr.class "hero" ]
      [ div [ Attr.class "hero-body" ]
        [ div [ Attr.class "container" ]
          [ h1 [ Attr.class "title" ] [ text "Arithmetic Drills" ]
          ]
        ]
      ]
    , section [ Attr.class "tabs is-centered is-large is-boxed is-fullwidth" ]
      [ div [ Attr.class "container" ]
        [ ul []
          [ li (tabAttr model Plus)      [ tabAnchor model Plus ]
          , li (tabAttr model Minus)     [ tabAnchor model Minus ]
          , li (tabAttr model Times)     [ tabAnchor model Times ]
          , li (tabAttr model DividedBy) [ tabAnchor model DividedBy ]
          ]
        ]
      ]
    ]