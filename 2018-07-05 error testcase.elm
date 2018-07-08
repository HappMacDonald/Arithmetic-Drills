{-
module Drills exposing (main)

{-| Web app that acts as arithmetic flashcards
-}

import FindRecord
    exposing
        ( findRecord
        )
import Html
    exposing
        ( Html
        , div
        , h1
        , li
        , ol
        , section
--        , text
        , ul
        )
import Html.Attributes as Attr
import Navigation as Nav

-- PRIMARY DECLARATION


main : Program Never Model Msg
main =
    Nav.program
        UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- HELPERS AND DEFINITIONS


{-| Types of arithmetic operation. Used as navigation, tabs along top.
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
operationRecordBuilder : ( Operation, String, String ) -> OperationNames
operationRecordBuilder ( oper, symbol, label ) =
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
    [ defaultOperation ]
        ++ List.map operationRecordBuilder
            [ ( Minus, "-", "Subtraction" )
            , ( Times, "x", "Multiplication" )
            , ( DividedBy, "÷", "Division" )
            ]


{-| This function looks up operation record based on any key/value pair.

    operationFind .symbol "x" |> .hash == "#Times"

-}
operationFind : (OperationNames -> key) -> key -> OperationNames
operationFind keyGetter key =
    findRecord keyGetter key operationRecords
        |> Maybe.withDefault defaultOperation


{-| Even more compressed convenience function for looking up
operation messages from navigational hashes.
-}
operationFromHash : String -> Operation
operationFromHash hashValue =
    operationFind .hash hashValue |> .operation


{-| These values define the parts of the flashcard equation
-}
type alias Lvalue =
    Int


type alias Rvalue =
    Int


type alias Problem =
    {   value : Lvalue
    ,   operation : Operation
    ,   rvalue : Rvalue
    }



-- MODEL


type alias Model =
    {   currentProblem : Problem
    }



-- INIT


init : Nav.Location -> ( Model, Cmd Msg )
init location =
    (   {   currentProblem =
                Problem 2 (operationFromHash location.hash) 2
        }
    ,   Cmd.none
    )



-- UPDATE


type Msg
    =   UrlChange Nav.Location
    |   ChangeTab Operation



--  | GiveAnswer Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            update ( ChangeTab <| operationFromHash location.hash ) model

        ChangeTab operation ->
            (   { model | tab = operation }
            ,   Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


{-| For now, just whether or not tab is active.
-}
tabAttr : Model -> Operation -> List (Html.Attribute Msg)
tabAttr model thisOperation =
    [ Attr.classList [("is-active", model.tab == thisOperation)] ]

{-| Creates HTML Anchor (hyperlink) element for the given operation,
complete with link to correct nav hash and text label that includes
the operation symbol and name.
-}
tabAnchor : Model -> Operation -> Html Msg
tabAnchor model operation =
    let
        record =
            operationFind .operation operation
    in
    Html.a
        [   Attr.href <|
            record.hash
        ]
        [   Html.text record.label
        ]

-}
view : Model -> Html Msg
view model =
    div
        []
        [   section [ Attr.class "card"]
            [   div [ Attr.class "level" ]
                [   div [ Attr.class "level-item has-text-centered"]
                        [ Html.text "2"]
                ,   div [ Attr.class "level-item has-text-centered"]
                        [ Html.text "4"]
                ]
            ]
