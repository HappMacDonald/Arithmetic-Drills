module Drills exposing (main)

{-| Web app that acts as arithmetic flashcards.

Todo:
* Change answer to a number input; this will improve experience on mobile 100%

* Firefox seems to have a bug that fails to pop the dialog, according to Michael?

* Michael's bug testing also clarified that canceling debug leaves you at the "new" url, so refreshing takes you to new URL instead of keeping you at present one.
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

{-| Types of navigational tabs, which double as operational modes.
This is represented as Maybe Operation at present because the options are each operation, or "none of them" to represent randomly chosen operations for each problem. :)
-}
type alias TabType =
    Maybe Operation


{-| Shortcut for elm infix integer arithmetic operators like (+) and (//).
-}
type alias IntegerFunction =
    (Int -> Int -> Int)


{-| A record of all facts related to a specific Operation
-}
type alias OperationNames =
    {   index : Int
    ,   tab : TabType
    ,   string : String
    ,   hash : String
    ,   symbol : String
    ,   function : IntegerFunction
    ,   label : String
    }


{-| A function that expands the compressed, unique facts about an operation
into a complete, searchable record.
-}
operationRecordBuilder : ( Int, TabType, String, IntegerFunction, String ) -> OperationNames
operationRecordBuilder ( index, tab, symbol, function, label ) =
    let
        string = case tab of
            Nothing ->
                "Everything"

            Just oper ->
                toString oper
    in
        {   index =
                index
            
        ,   tab =
                tab

        ,   string =
                string

        ,   hash =
                "#" ++ string

        ,   symbol =
                symbol

        ,   function =
                function

        ,   label =
                symbol ++ " " ++ label

        }


{-| In the hypothetical case where an Operation record lookup fails,
this first record (set apart from the rest) will serve as the fallback
lookup result.
-}
defaultOperation : OperationNames
defaultOperation =
    operationRecordBuilder ( 0, Just Plus, "+", (+), "Addition" )


{-| The initial/default record is concattenated with all other
Operation records
-}
operationRecords : List OperationNames
operationRecords =
    [ defaultOperation ]
        ++ List.map operationRecordBuilder
            [   ( 1, Just Minus, "-", (-), "Subtraction" )
            ,   ( 2, Just Times, "x", (*), "Multiplication" )
            ,   ( 3, Just DividedBy, "÷", (//), "Division" )
            ,   ( 4, Nothing, "😎", max, "Everything!")
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
tabFromHash : String -> TabType
tabFromHash hashValue =
    operationFind .hash hashValue |> .tab


operationMapper : Int -> Operation
operationMapper index =
    operationFind .index index |> .tab |> Maybe.withDefault Plus


{-| These values define the parts of a flashcard equation
Lvalue, Dividend, and Rvalue are meant to be constrained to Int[-9 - 9]
-}
type alias Lvalue =
    Int


type alias Rvalue =
    Int


type alias Dividend =
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
    ,   tab: TabType
    ,   potentialTab: Maybe TabType
    ,   totalRight: Int
    ,   totalWrong: Int
    }



-- INIT


{-| Represents what we want NewProblem to return: a bunch of random values suitable for generating a new problem regardless of context.
-}
type alias DiceRoll =
    {   rValue : Rvalue
    ,   operation : Operation
    ,   lValue : Lvalue
    ,   dividend : Int
    }


newProblem : List (Cmd Msg)
newProblem =
    [   Random.generate
        NewProblem
        (   Random.map4
            DiceRoll
            ( Random.int -9 9 )
            ( Random.map operationMapper (Random.int 0 3) )
            ( Random.int -9 9 )
            ( Random.int -8 9 )
        )
    ]


defaultProblem : Problem
defaultProblem =
    Problem 2 Plus 2


assertFocus : List (Cmd Msg)
assertFocus =
    [ Dom.focus "answer" |> Task.attempt FocusResult ]


init : Nav.Location -> ( Model, Cmd Msg )
init location =
    {   lastRound =
            Nothing

    ,   thisRound =
            Round defaultProblem ""

    ,   tab =
            tabFromHash location.hash

    ,   potentialTab =
            Nothing

    ,   totalRight =
            0

    ,   totalWrong =
            0

    } ! ( assertFocus ++ newProblem )



-- UPDATE


type Msg
    =   Focus
    |   FocusResult (Result Dom.Error ())
    |   UrlChange Nav.Location
    |   ChangeTabWarning TabType
    |   ChangeTabCancel
    |   ChangeTab TabType
    |   Typing String
    |   Answer
    |   NewProblem DiceRoll


problemAnswer : Problem -> Int
problemAnswer problem =
    let
        function =
            operationFind .tab (Just problem.operation) |> .function
    in
        function problem.lValue problem.rValue


isCorrect : Round -> Bool
isCorrect round =
    integerInput round.yourAnswer == problemAnswer round.problem


integerInput : String -> Int
integerInput answer =
    answer |> String.trim |> String.toInt |> Result.withDefault -999


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

            UrlChange location ->
                let
                    msg = if (model.totalRight + model.totalWrong)>0 then
                        ChangeTabWarning <| tabFromHash location.hash
                    else
                        ChangeTab <| tabFromHash location.hash

                in
                    update msg model

            ChangeTab tab ->
                {   model
                |   tab =
                        tab

                ,   potentialTab =
                        Nothing

                ,   totalRight =
                        0

                ,   totalWrong  =
                        0

                } ! ( assertFocus ++ newProblem )

            ChangeTabWarning tab ->
                { model | potentialTab = Just tab } ! []

            ChangeTabCancel ->
                { model | potentialTab = Nothing } ! ( assertFocus )
            
            Typing yourAnswer ->
                {   model
                |   thisRound =
                    {   thisRound
                    |   yourAnswer =
                            yourAnswer

                    }

                } ! []
            
            Answer ->
                if integerInput thisRound.yourAnswer >= -99 then
                    let
                        totalRight =
                            if isCorrect thisRound then
                                model.totalRight + 1
                            else
                                model.totalRight

                        totalWrong =
                            if not <| isCorrect thisRound then
                                model.totalWrong + 1
                            else
                                model.totalWrong

                    in
                        {   model
                        |   lastRound =
                                Just <| Round problem thisRound.yourAnswer

                        ,   thisRound = 
                                Round defaultProblem ""

                        ,   totalRight =
                                totalRight

                        ,   totalWrong =
                                totalWrong

                        } ! ( assertFocus ++ newProblem )
                else
                    model ! ( assertFocus )
            
            NewProblem diceRollUncooked ->
                let
--                    junk =
--                        Debug.log "NewProblem" (lValue, oper, rValue)

                    diceRoll =
                        {   diceRollUncooked
                        |   dividend =
                                if diceRollUncooked.dividend < 1
                                then diceRollUncooked.dividend - 1
                                else diceRollUncooked.dividend
                        }
                    problemOld =
                        thisRound.problem

                    operation =
                        case model.tab of
                            Just operation ->
                                operation

                            Nothing ->
                                diceRoll.operation

                    problemNew =
                        case operation of
                            Plus ->
                                Problem diceRoll.lValue Plus diceRoll.rValue

                            Minus ->
                                Problem ( diceRoll.lValue + diceRoll.rValue ) Minus diceRoll.rValue

                            Times ->
                                Problem diceRoll.lValue Times diceRoll.rValue

                            DividedBy ->
                                Problem ( diceRoll.lValue * diceRoll.dividend ) DividedBy diceRoll.dividend

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
tabAttr : Model -> TabType -> List (Html.Attribute Msg)
tabAttr model tab =
    [ Attr.classList [("is-active", model.tab == tab)] ]

{-| Creates HTML Anchor (hyperlink) element for the given operation,
complete with link to correct nav hash and text label that includes
the operation symbol and name.
-}
tabAnchor : Model -> TabType -> Html Msg
tabAnchor model tab =
    let
        record =
            operationFind .tab tab

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
        |>  Just
        |>  operationFind .tab
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
        <|
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
        ,   Html.node "dialog"
            [ Attr.id "confirm" ]
            [   Html.div [ Attr.class "is-large" ]
                [   Html.span [ Attr.class "icon has-text-warning is-large"]
                    [ Html.i [ Attr.class "fas fa-exclamation-triangle fa-3x"] [] ]
                ,   Html.text "WARNING:"
                ,   Html.br [] []
                ,   Html.text "Changing tabs will erase all of your stats you've built up."
                ,   Html.br [] []
                ,   Html.text "Are you certain you wish to proceed?"
                ]
            ,   Html.div []
                [   Html.button [ Html.Events.onClick <| ChangeTab <| Maybe.withDefault (Just Plus) model.potentialTab ] [ Html.text "Yes, please!" ]
                ,   Html.button [ Html.Events.onClick ChangeTabCancel ] [ Html.text "Wait, what? No I'm not ready 😮" ]
                ]
            ]
        ,   div [ Attr.class "container" ]
            <|
            [   section [ Attr.class "hero" ]
                [   div [ Attr.class "hero-body" ]
                    [   h1 [ Attr.class "title" ] [ Html.text "Arithmetic Drills" ]
                    ]
                ]
            ,   Html.nav [ Attr.class "tabs is-centered is-large is-boxed is-fullwidth" ]
                [   ul []
                    [   li (tabAttr model <| Just Plus) [ tabAnchor model <| Just Plus ]
                    ,   li (tabAttr model <| Just Minus) [ tabAnchor model <| Just Minus ]
                    ,   li (tabAttr model <| Just Times) [ tabAnchor model <| Just Times ]
                    ,   li (tabAttr model <| Just DividedBy) [ tabAnchor model <| Just DividedBy ]
                    ,   li (tabAttr model Nothing) [ tabAnchor model Nothing ]
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
        ++  case model.potentialTab of
                Nothing ->
                    [   Html.node "script" [] [ Html.text {-"alert('!'); "++ -} "document.getElementById('confirm').close();" ]
                    ,   Html.text ""
                    ]
                
                Just tab ->
                    [   Html.text ""
                    ,   Html.node "script" [] [ Html.text {-"alert('?'); "++ -} "document.getElementById('confirm').showModal();" ]
                    ]
