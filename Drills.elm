module Drills exposing (main)

{-| Web app that acts as arithmetic flashcards
-}

import FindRecord
    exposing
        (   findRecord
        )
import Html
    exposing
        (   Html
        ,   div
        ,   p
        ,   h1
        ,   li
        ,   ol
        ,   section
--        , text
        ,   ul
        )
import Html.Attributes as Attr
import Html.Events exposing (onInput, onBlur)
import Html.Events.Extra exposing (onEnter)
import Dom exposing (focus)
import Task
import Random
import Navigation as Nav


-- PRIMARY DECLARATION


main : Program Never Model Msg
main =
    Nav.program
        UrlChange
        { init =
            init

        , view =
            view

        , update =
            update

        , subscriptions =
            subscriptions

        }



-- HELPERS AND DEFINITIONS


{-| Types of arithmetic operation. Used as navigation, tabs along top.
-}
type Operation
    = Plus
    | Minus
    | Times
    | DividedBy


{-| Shortcut for elm infix integer arithmetic operators like (+) and (//).
-}
type alias IntegerFunction =
    (Int -> Int -> Int)


{-| A record of all facts related to a specific Operation
-}
type alias OperationNames =
    {   operation : Operation
    ,   string : String
    ,   hash : String
    ,   symbol : String
    ,   function : IntegerFunction
    ,   label : String
    }


{-| A function that expands the compressed, unique facts about an operation
into a complete, searchable record.
-}
operationRecordBuilder : ( Operation, String, IntegerFunction, String ) -> OperationNames
operationRecordBuilder ( oper, symbol, function, label ) =
    { operation =
        oper

    , string =
        toString oper

    , hash =
        "#" ++ toString oper

    , symbol =
        symbol

    , function =
        function

    , label =
        symbol ++ " " ++ label

    }


{-| In the hypothetical case where an Operation record lookup fails,
this first record (set apart from the rest) will serve as the fallback
lookup result.
-}
defaultOperation : OperationNames
defaultOperation =
    operationRecordBuilder ( Plus, "+", (+), "Addition" )


{-| The initial/default record is concattenated with all other
Operation records
-}
operationRecords : List OperationNames
operationRecords =
    [ defaultOperation ]
        ++ List.map operationRecordBuilder
            [ ( Minus, "-", (-), "Subtraction" )
            , ( Times, "x", (*), "Multiplication" )
            , ( DividedBy, "÷", (//), "Division" )
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
    {   lValue : Lvalue
    ,   operation : Operation
    ,   rValue : Rvalue
    }


type alias Round =
    {   problem : Problem
    ,   yourAnswer : String
    }


-- MODEL


type alias Model =
    {   lastRound: Maybe Round
    ,   thisRound: Round
    ,   totalRight: Int
    ,   totalWrong: Int
    }



-- INIT


newOperands : List (Cmd Msg)
newOperands =
    [   Random.generate
        NewOperands
        (   Random.pair
            ( Random.int 0 9 )
            ( Random.int 0 9 )
        )
    ]


newProblem : Operation -> Problem
newProblem oper =
    Problem 2 oper 2


assertFocus : List (Cmd Msg)
assertFocus =
    [ Dom.focus "answer" |> Task.attempt FocusResult ]


init : Nav.Location -> ( Model, Cmd Msg )
init location =
    {   lastRound =
            Nothing
            --Just <| Round (newProblem <| operationFromHash location.hash) "3"

    ,   thisRound =
            Round (newProblem <| operationFromHash location.hash) ""

    ,   totalRight =
            0

    ,   totalWrong =
            0

    } ! ( assertFocus ++ newOperands )



-- UPDATE


type Msg
    =   Focus
    |   FocusResult (Result Dom.Error ())
    |   UrlChange Nav.Location
    |   ChangeTab Operation
    |   Typing String
    |   Answer
    |   NewOperands (Int, Int)


problemAnswer : Problem -> Int
problemAnswer problem =
    let
        function =
            operationFind .operation problem.operation |> .function
    in
        function problem.lValue problem.rValue


isCorrect : Round -> Bool
isCorrect round =
    integerInput round.yourAnswer == problemAnswer round.problem



integerInput : String -> Int
integerInput answer =
    Result.withDefault -1 (String.toInt answer)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( { thisRound, lastRound } as model ) =
    let
        problem =
            thisRound.problem

    in
        case msg of
            Focus ->
                model ! ( assertFocus )
            
            FocusResult result ->
                model ! []
{-
                case result of
                    Err (Dom.NotFound id) ->
                        -- unable to find dom 'id'
                    Ok () ->
                        -- successfully focus the dom
-}

            UrlChange location ->
                update ( ChangeTab <| operationFromHash location.hash ) model

            ChangeTab operation ->
                {   model
                |   thisRound =
                    {   thisRound
                    |   problem =
                        {   problem
                        |   operation =
                                operation

                        }

                    }

                } ! ( assertFocus ++ newOperands )
            
            Typing yourAnswer ->
                {   model
                |   thisRound =
                    {   thisRound
                    |   yourAnswer =
                            yourAnswer

                    }

                } ! []
            
            Answer ->
                let
                    totalRight =
                        if isCorrect thisRound then
                            model.totalRight + 1
                        else
                            model.totalRight

                    totalWrong =
                        if isCorrect thisRound then
                            model.totalWrong
                        else
                            model.totalWrong + 1

                in
                    {   model
                    |   lastRound =
                            Just <| Round problem thisRound.yourAnswer

                    ,   thisRound = 
                            Round (newProblem problem.operation) ""

                    ,   totalRight =
                            totalRight

                    ,   totalWrong =
                            totalWrong

                    } ! ( assertFocus ++ newOperands )
            
            NewOperands (lValue, rValue) ->
                let
--                    junk =
--                        Debug.log "NewOperands" (lValue, rValue)

                    problemOld =
                        thisRound.problem

                    problemNew =
                        case problemOld.operation of
                            Plus ->
                                Problem lValue Plus rValue

                            Minus ->
                                Problem ( lValue + rValue ) Minus rValue

                            Times ->
                                Problem lValue Times rValue

                            DividedBy ->
                                if rValue==0 then -- We can't divide by zero, dawg :P
                                    Problem ( lValue * 9 ) DividedBy 9 -- So let's replace zero rolls with 9 and call it a day.
                                else
                                    Problem ( lValue * rValue ) DividedBy rValue

                in
                    {   model
                    |   thisRound =
                        {   thisRound
                        |   problem = problemNew
                        }
                    }   ! ( assertFocus )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


{-| For now, just whether or not tab is active.
-}
tabAttr : Model -> Operation -> List (Html.Attribute Msg)
tabAttr model thisOperation =
    [ Attr.classList [("is-active", model.thisRound.problem.operation == thisOperation)] ]

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


whitespace : Html Msg
whitespace =
    Html.text " "


displayProblem : Round -> List (Html Msg)
displayProblem round =
    [   Html.text <| toString round.problem.lValue
    ,   whitespace
    ,   round.problem.operation
        |>  operationFind .operation
        |>  .symbol
        |>  Html.text
    ,   whitespace
    ,   Html.text <| toString round.problem.rValue
    ,   whitespace
    ,   Html.text "="
    ,   whitespace
    ]


sectionSuccess : Bool -> Html.Attribute msg
sectionSuccess success =
    Attr.classList
        [   ("card", True)
        ,   ("field", True)
        ,   ("is-horizontal", True)
        ,   ("is-group-centered", True)
        ,   ("statRow", True)
        ,   ("success", success)
        ,   ("failure", not success)
        ]


lastRound : Maybe Round -> List (Html Msg)
lastRound maybeRound =
    case maybeRound of
        Nothing ->
            []

        Just round ->
            [   section [ sectionSuccess <| isCorrect round ]
                [   p [ Attr.class "field-label control is-size-1 has-text-right" ]
                    [
                        Html.label
                            [ Attr.class "label" ]
                            <| displayProblem round
                    ]
                ,   p [ Attr.class "field-body no-grow control is-size-1 has-text-left" ]
                    <|
                    [   round.yourAnswer |> Html.text 
                    ]
                    ++  if isCorrect round then
                            []
                        else
                            [   Html.span
                                [ Attr.class "is-size-3 fixAlign"]
                                [   Html.text
                                <|  "  (Should have been "
                                ++  (round.problem |> problemAnswer |> toString )
                                ++  ")"
                                ]
                            ]
                ]
            ]


showStats : Model -> List (Html Msg)
showStats model =
    let
        percent =
            model.totalRight * 100 // ( model.totalRight + model.totalWrong )

        goodResults =
            percent >= 90

    in
        [   section [ Attr.class "card field is-horizontal is-group-centered success statRow" ]
            [   p   [ Attr.class "field-label control is-size-3 has-text-right" ]
                    [ Html.text "Total right so far:" ]
            ,   p   [ Attr.class "field-body no-grow control is-size-3 has-text-left" ]
                    [ model.totalRight |> toString |> Html.text ]
            ]
        ,   section [ Attr.class "card field is-horizontal is-group-centered failure statRow" ]
            [   p   [ Attr.class "field-label control is-size-3 has-text-right" ]
                    [ Html.text "Total wrong so far:" ]
            ,   p   [ Attr.class "field-body no-grow control is-size-3 has-text-left" ]
                    [ model.totalWrong |> toString |> Html.text ]
            ]
        ,   section [ sectionSuccess goodResults ]
            [   p   [ Attr.class "field-label control is-size-3 has-text-right" ]
                    [ Html.text "Percent win rate:" ]
            ,   p   [ Attr.class "field-body no-grow control is-size-3 has-text-left" ]
                    [   percent |> toString |> Html.text
                    ,   Html.text "%"
                    ]
            ]
        ]


view : Model -> Html Msg
view model =
    div
        []
        [   Html.node
            "link"
            [   Attr.rel "stylesheet"
            ,   Attr.href "bulma.min.css"
            ]
            []
        ,   Html.node
            "link"
            [   Attr.rel "stylesheet"
            ,   Attr.href "Drills.css"
            ]
            []
        ,   div [ Attr.class "container" ]
            <|
            [   section [ Attr.class "hero" ]
                [   div [ Attr.class "hero-body" ]
                    [   h1 [ Attr.class "title" ] [ Html.text "Arithmetic Drills" ]
                    ]
                ]
            ,   Html.nav [ Attr.class "tabs is-centered is-large is-boxed is-fullwidth" ]
                [   ul []
                    [   li (tabAttr model Plus) [ tabAnchor model Plus ]
                    ,   li (tabAttr model Minus) [ tabAnchor model Minus ]
                    ,   li (tabAttr model Times) [ tabAnchor model Times ]
                    ,   li (tabAttr model DividedBy) [ tabAnchor model DividedBy ]
                    ]
                ]
            ]
            ++ lastRound model.lastRound
            ++
            [   section [ Attr.class "card field is-horizontal is-group-centered" ]
                [   p [ Attr.class "field-label control is-size-1 has-text-right" ]
                    [
                        Html.label
                            [ Attr.class "label" ]
                            <| displayProblem model.thisRound
                    ]
                ,   p [ Attr.class "field-body no-grow control is-vertical-center brief" ]
                    [   Html.input
                        [   Attr.id "answer"
                        ,   Attr.class "input is-large brief"
                        ,   Attr.value model.thisRound.yourAnswer
                        ,   onBlur Focus
                        ,   onInput Typing
                        ,   onEnter Answer
                        ] []
                    ]
                ]
            ]
            ++
            if model.totalRight + model.totalWrong > 0 then
                showStats model
            else
                []
        ]
