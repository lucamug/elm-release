module Main exposing (main)

import Browser
import Chart
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Process
import Task


operator : String
operator =
    "Jason"


buddy : String
buddy =
    "Cindy"


releaseName : String
releaseName =
    "[Elm] Version 1.7.1"


alphaLayer : Float
alphaLayer =
    0.7


type alias Model =
    { releaseTransition : ReleaseTransition
    , log : List ReleaseTransition
    , codeOperator : String
    , codeBuddy : String
    , width : Int
    }


type alias Flags =
    { width : Int }


init : Flags -> ( Model, Cmd msg )
init flags =
    let
        initReleaseTransition =
            Stable Unreleased
    in
    ( { releaseTransition = initReleaseTransition
      , log = [ initReleaseTransition ]
      , codeOperator = ""
      , codeBuddy = ""
      , width = flags.width
      }
    , Cmd.none
    )


type Msg
    = MsgCodeOperator String
    | MsgCodeBuddy String
    | MsgConfirmRelease
    | MsgCancel
    | MsgOnSwitch ReleaseState
    | MsgEndMoving


updateHelper :
    { b | log : List a, releaseTransition : a }
    -> a
    -> c
    -> ( { b | log : List a, releaseTransition : a }, c )
updateHelper model newReleaseTransition cmd =
    ( { model
        | releaseTransition = newReleaseTransition
        , log = newReleaseTransition :: model.log
      }
    , cmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgEndMoving ->
            case model.releaseTransition of
                Moving _ to ->
                    updateHelper model (Stable to) Cmd.none

                _ ->
                    ( model, Cmd.none )

        MsgConfirmRelease ->
            case model.releaseTransition of
                Request from to ->
                    updateHelper model (Moving from to) (Process.sleep 1000 |> Task.perform (always MsgEndMoving))

                _ ->
                    ( model, Cmd.none )

        MsgCancel ->
            case model.releaseTransition of
                Request from _ ->
                    updateHelper model (Stable from) Cmd.none

                Moving from _ ->
                    updateHelper model (Stable from) Cmd.none

                _ ->
                    ( model, Cmd.none )

        MsgCodeOperator code ->
            ( { model | codeOperator = code }, Cmd.none )

        MsgCodeBuddy code ->
            ( { model | codeBuddy = code }, Cmd.none )

        MsgOnSwitch to ->
            case model.releaseTransition of
                Stable from ->
                    updateHelper model (Request from to) Cmd.none

                _ ->
                    ( model, Cmd.none )


type ReleaseTransition
    = Stable ReleaseState
    | Request ReleaseState ReleaseState
    | Moving ReleaseState ReleaseState


releaseTransitionToString : ReleaseTransition -> String
releaseTransitionToString releaseTransition =
    case releaseTransition of
        Stable from ->
            "State: \"" ++ releaseStateToString from ++ "\""

        Request from to ->
            "Requested to change from \"" ++ releaseStateToString from ++ "\" to \"" ++ releaseStateToString to ++ "\""

        Moving from to ->
            "Changed from \"" ++ releaseStateToString from ++ "\" to \"" ++ releaseStateToString to ++ "\""


type ReleaseState
    = Unreleased
    | Released000
    | Released001
    | Released005
    | Released010
    | Released100


releaseStateList : List ReleaseState
releaseStateList =
    [ Unreleased
    , Released000
    , Released001
    , Released005
    , Released010
    , Released100
    ]


releaseStateToString : ReleaseState -> String
releaseStateToString releaseState =
    case releaseState of
        Unreleased ->
            "Not Released"

        Released000 ->
            "Released 0%"

        Released001 ->
            "Released 1%"

        Released005 ->
            "Released 5%"

        Released010 ->
            "Released 10%"

        Released100 ->
            "Released 100%"


releaseStateToStringShort : ReleaseState -> String
releaseStateToStringShort releaseState =
    case releaseState of
        Unreleased ->
            "NR"

        Released000 ->
            "0%"

        Released001 ->
            "1%"

        Released005 ->
            "5%"

        Released010 ->
            "10%"

        Released100 ->
            "100%"


releaseStateTime : ReleaseState -> String
releaseStateTime releaseState =
    case releaseState of
        Unreleased ->
            ""

        Released000 ->
            ""

        Released001 ->
            "10:00"

        Released005 ->
            "10:30"

        Released010 ->
            "11:00"

        Released100 ->
            "12:00"


listLength : Float
listLength =
    toFloat (List.length releaseStateList)


val1 : Float
val1 =
    listLength / (listLength - 1)


indexToRatio : Int -> Float
indexToRatio index =
    (toFloat index * val1) / listLength


isBefore : ReleaseState -> ReleaseState -> Bool
isBefore releaseState1 releaseState2 =
    getIndex releaseState1 + 1 < getIndex releaseState2


getIndex : ReleaseState -> Int
getIndex releaseStateToFind =
    Tuple.second <|
        List.foldl
            (\releaseState ( found, index ) ->
                if found then
                    ( found, index )

                else if releaseStateToFind == releaseState then
                    -- Found!
                    ( True, index )

                else
                    -- Not found
                    ( False, index + 1 )
            )
            ( False, 0 )
            releaseStateList


shadow : { blur : number, color : Color, offset : ( number1, number2 ), size : number3 }
shadow =
    { offset = ( 2, 5 ), size = 2, blur = 0, color = rgba 0 0 0 0.2 }


color : Float -> Float -> Color
color ratio alpha =
    -- rgb(3, 169, 244)
    -- rgb(156, 39, 176);
    -- rgba (0.15 * toFloat ratio) (0.7 - (0.15 * toFloat ratio)) 0 alpha
    rgba255 (variation 3 156 ratio) (variation 169 39 ratio) (variation 244 176 ratio) alpha


variation : Float -> Float -> Float -> Int
variation rangeA rangeB ratio =
    if rangeA < rangeB then
        round <| rangeA + (ratio * (rangeB - rangeA))

    else
        round <| rangeA - (ratio * (rangeA - rangeB))


bigButton :
    { activable : Bool
    , active : Bool
    , attempted : Bool
    , ratio : Float
    }
    -> ReleaseState
    -> Element Msg
bigButton args releaseState =
    Input.button
        ([ width <| px 100
         , height <| px 200
         , Border.rounded 100
         , Background.color <| color args.ratio 1
         , htmlAttribute <| Html.Attributes.style "transition" "all 300ms"
         , mouseOver [ Border.shadow shadow ]
         , inFront <|
            el
                ([ width <| px 80
                 , height <| px 80
                 , moveRight 10
                 , htmlAttribute <| Html.Attributes.style "transition" "all 300ms"
                 , Border.rounded 80
                 , Background.color <| rgb 1 1 0
                 , Border.shadow shadow
                 ]
                    ++ (if args.active then
                            [ moveDown 10 ]

                        else if args.attempted then
                            [ moveDown 70
                            , alpha 0.5
                            ]

                        else
                            [ moveDown 110
                            , alpha 0.5
                            ]
                       )
                    ++ [ mouseOver [ alpha 1 ] ]
                )
            <|
                el [ centerY, centerX, Font.size 30 ] <|
                    text <|
                        releaseStateToStringShort releaseState
         ]
            ++ (if args.activable then
                    [ alpha 0.3
                    , htmlAttribute <| Html.Attributes.style "cursor" "not-allowed"
                    ]

                else if args.active then
                    [ htmlAttribute <| Html.Attributes.style "cursor" "default"
                    ]

                else
                    []
               )
        )
        { label = none
        , onPress =
            if args.activable || args.active then
                Nothing

            else
                Just <| MsgOnSwitch releaseState
        }


viewUnlock : { a | codeBuddy : String, codeOperator : String } -> Attribute Msg
viewUnlock model =
    inFront <|
        el
            [ Background.color <|
                rgba 0 0 0 0.5
            , width fill
            , height fill
            ]
        <|
            column
                [ Background.color <| rgb 1 1 1
                , centerX
                , centerY
                , width <| px 320
                , Border.rounded 10
                , Border.shadow shadow
                , paddingXY 20 30
                , spacing 50
                ]
                [ Input.newPassword [ Font.size 50, Font.center ]
                    { onChange = MsgCodeOperator
                    , text = model.codeOperator
                    , placeholder = Nothing
                    , label = Input.labelAbove [ Font.size 25 ] <| text <| "Code " ++ operator
                    , show = False
                    }
                , Input.newPassword [ Font.size 50, Font.center ]
                    { onChange = MsgCodeBuddy
                    , text = model.codeBuddy
                    , placeholder = Nothing
                    , label = Input.labelAbove [ Font.size 25 ] <| text <| "Code " ++ buddy
                    , show = False
                    }
                , Input.button
                    [ Font.size 25
                    , Font.center
                    , Background.color <| rgb 1 1 0
                    , padding 20
                    , Border.rounded 50
                    , Border.shadow shadow
                    , Border.width 1
                    , Border.color <| rgba 0 0 0 0.3
                    , width fill
                    ]
                    { onPress = Nothing
                    , label =
                        paragraph
                            []
                            [ text "Release from 10% to 100%" ]
                    }
                , Input.button
                    [ Font.size 25
                    , Font.center
                    , Background.color <| rgba 0 0 0 0.1
                    , padding 10
                    , Border.rounded 10
                    , Border.shadow shadow
                    , Border.width 1
                    , Border.color <| rgba 0 0 0 0.3
                    , width fill
                    ]
                    { onPress = Nothing
                    , label =
                        paragraph
                            []
                            [ text "Cancel" ]
                    }
                ]


viewHeader : Element msg
viewHeader =
    paragraph
        [ Font.center
        , Font.color <| rgba 0 0 0 0.3
        ]
        [ newTabLink []
            { url = "https://jira.com/jira/browse/ELM-2968"
            , label = text "ELM-2968"
            }
        , text " ◆ Tuesday March 17, 2022 ◆ "
        , text "Operator "
        , el [ Font.color <| rgb 0 0 0 ] <| text operator
        , text " ◆ Buddy "
        , el [ Font.color <| rgb 0 0 0 ] <| text buddy
        , text " ◆ "
        , newTabLink []
            { url = "https://example.com/manual"
            , label = text "Manual"
            }
        , text " ◆ "
        , newTabLink []
            { url = "https://example.com/zoom"
            , label = text "Zoom"
            }
        , text " ◆ "
        , el [ Font.color <| rgb 0 0 0 ] <| text "UI "
        , text "8f1d77814f0-527 "
        , newTabLink []
            { url = "https://example.com/1.7.0"
            , label = text "1.7.0"
            }
        , text " ("
        , newTabLink []
            { url = "https://example.com/1.6.0"
            , label = text "1.6.0"
            }
        , text ") ◆ "
        , el [ Font.color <| rgb 0 0 0 ] <| text "API "
        , text "77d02609c83-882 "
        , newTabLink []
            { url = "https://example.com/"
            , label = text "1.7.1"
            }
        , text " ("
        , newTabLink []
            { url = "https://example.com/"
            , label = text "1.7.0"
            }
        , text ") ◆ "
        , el [ Font.color <| rgb 0 0 0 ] <| text "Config "
        , newTabLink []
            { url = "https://example.com/"
            , label = text "1.7.0"
            }
        ]


viewFooter : Element msg
viewFooter =
    paragraph [ Font.center, Font.size 14, Font.color <| rgba 0 0 0 0.3 ]
        [ text "This is a proof of concept so it has not effects whatsoever" ]


viewState : Int -> ReleaseState -> Element msg
viewState _ releaseState =
    el [ width fill ] <|
        paragraph
            [ paddingXY 0 20
            , Background.color <| color (indexToRatio (getIndex releaseState)) 1
            , Font.color <| rgb 1 1 1
            , Font.size 25
            , Font.center
            , htmlAttribute <| Html.Attributes.style "transition" "all 1000ms"
            ]
            [ text <|
                releaseName
                    ++ ": "
                    ++ releaseStateToString releaseState
            ]


viewSwitches : ReleaseState -> Maybe ReleaseState -> Element Msg
viewSwitches releaseState attemptedReleaseState =
    wrappedRow [ spacing 20, centerX, paddingEach { top = 40, right = 0, bottom = 0, left = 0 } ] <|
        List.indexedMap
            (\index releaseState_ ->
                column [ alignTop, width <| px 100, spacing 10 ]
                    [ bigButton
                        { active = releaseState_ == releaseState
                        , activable = isBefore releaseState releaseState_
                        , attempted =
                            case attemptedReleaseState of
                                Just attempted ->
                                    attempted == releaseState_

                                Nothing ->
                                    False
                        , ratio = indexToRatio index
                        }
                        releaseState_
                    , el [ centerX, Font.color <| rgba 0 0 0 0.3 ] <| text <| releaseStateTime releaseState_
                    ]
            )
            releaseStateList


buttonAttrs : List (Attribute msg)
buttonAttrs =
    [ paddingXY 10 20
    , width fill
    , Font.size 25
    , Font.center
    , Border.rounded 20
    , Border.shadow shadow
    , Background.color <| rgb 0.8 0.8 0.8
    ]


viewRequest : ReleaseState -> ReleaseState -> Attribute Msg
viewRequest from to =
    inFront <|
        el
            [ Background.color <| rgba 0 0 0 alphaLayer
            , width fill
            , height fill
            , htmlAttribute <| Html.Attributes.style "transition" "all 500ms"
            ]
        <|
            el
                [ centerX
                , alignBottom
                , width (fill |> maximum 600)
                , paddingXY 20 30
                ]
            <|
                column
                    [ spacing 30
                    , width fill
                    ]
                    [ Input.button
                        (buttonAttrs
                            ++ [ Background.color <| color (indexToRatio (getIndex to)) 1
                               , Font.color <| rgb 1 1 1
                               ]
                        )
                        { onPress = Just MsgConfirmRelease
                        , label =
                            column
                                [ Font.center, centerX, spacing 10 ]
                                [ paragraph []
                                    [ el [ Font.color <| rgba 1 1 1 0.5 ] <| text <| "Change from "
                                    , text <| releaseStateToString from
                                    ]
                                , paragraph []
                                    [ el [ Font.color <| rgba 1 1 1 0.5 ] <| text "to "
                                    , text <| releaseStateToString to
                                    ]
                                ]
                        }
                    , Input.button buttonAttrs
                        { onPress = Just MsgCancel
                        , label =
                            paragraph
                                []
                                [ text "Cancel" ]
                        }
                    ]


viewMoving : ReleaseState -> ReleaseState -> Attribute Msg
viewMoving from to =
    inFront <|
        el
            [ Background.color <| rgba 0 0 0 alphaLayer
            , width fill
            , height fill
            , htmlAttribute <| Html.Attributes.style "transition" "all 500ms"
            ]
        <|
            el
                [ centerX
                , alignBottom
                , width (fill |> maximum 600)
                , paddingXY 20 40
                ]
            <|
                column
                    -- [ Background.color <| rgb 1 1 1
                    -- , Border.rounded 10
                    -- , Border.shadow shadow
                    [ spacing 40
                    , width fill
                    ]
                    [ -- Spinner from https://loading.io/css/
                      html <| Html.node "style" [] [ Html.text """.lds-spinner { color: official; display: inline-block; position: relative; width: 80px; height: 80px; }
                        .lds-spinner div { transform-origin: 40px 40px; animation: lds-spinner 1.2s linear infinite; }
                        .lds-spinner div:after { content: " "; display: block; position: absolute; top: 3px; left: 37px; width: 6px; height: 18px; border-radius: 20%; background: #fff; }
                        .lds-spinner div:nth-child(1) { transform: rotate(0deg); animation-delay: -1.1s; }
                        .lds-spinner div:nth-child(2) { transform: rotate(30deg); animation-delay: -1s; }
                        .lds-spinner div:nth-child(3) { transform: rotate(60deg); animation-delay: -0.9s; }
                        .lds-spinner div:nth-child(4) { transform: rotate(90deg); animation-delay: -0.8s; }
                        .lds-spinner div:nth-child(5) { transform: rotate(120deg); animation-delay: -0.7s; }
                        .lds-spinner div:nth-child(6) { transform: rotate(150deg); animation-delay: -0.6s; }
                        .lds-spinner div:nth-child(7) { transform: rotate(180deg); animation-delay: -0.5s; }
                        .lds-spinner div:nth-child(8) { transform: rotate(210deg); animation-delay: -0.4s; }
                        .lds-spinner div:nth-child(9) { transform: rotate(240deg); animation-delay: -0.3s; }
                        .lds-spinner div:nth-child(10) { transform: rotate(270deg); animation-delay: -0.2s; }
                        .lds-spinner div:nth-child(11) { transform: rotate(300deg); animation-delay: -0.1s; }
                        .lds-spinner div:nth-child(12) { transform: rotate(330deg); animation-delay: 0s; }
                        @keyframes lds-spinner { 0% { opacity: 1; } 100% { opacity: 0; } }""" ]
                    , el [ centerX, paddingXY 0 30 ] <| html <| Html.div [ Html.Attributes.class "lds-spinner" ] <| List.map (\_ -> Html.div [] []) (List.repeat 12 ())
                    , Input.button buttonAttrs
                        { onPress = Just MsgCancel
                        , label =
                            paragraph
                                []
                                [ text "Cancel" ]
                        }
                    ]


viewEmptyOverlay : Attribute Msg
viewEmptyOverlay =
    inFront <|
        el
            [ Background.color <| rgba 0 0 0 0
            , width fill
            , height fill
            , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
            , htmlAttribute <| Html.Attributes.style "transition" "all 500ms"
            ]
            none


viewLog : { a | log : List ReleaseTransition } -> Element msg
viewLog model =
    column
        [ width (fill |> maximum 700)
        , centerX
        , spacing 10
        ]
        [ text "History"
        , column
            [ height <| px 200
            , width fill
            , scrollbars
            , Background.color <| rgb 0.2 0.2 0.2
            , Font.color <| rgb 0.8 0.8 0.8
            , Font.family [ Font.monospace ]
            , padding 10
            , spacing 5
            ]
          <|
            List.indexedMap
                (\index log ->
                    el [] <|
                        text <|
                            (String.fromInt <| index + 1)
                                ++ " - "
                                ++ releaseTransitionToString log
                )
                (List.reverse model.log)
        ]


viewReleaseState : Model -> ReleaseState -> Maybe ReleaseState -> List (Attribute Msg) -> Html.Html Msg
viewReleaseState model releaseState attemptedReleaseState layerInFront =
    layoutWith
        { options =
            [ focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Just shadow
                }
            ]
        }
        ([ Font.size 16
         ]
            ++ layerInFront
            ++ [ inFront <| viewState model.width releaseState ]
        )
    <|
        column [ width fill, spacing 20 ]
            [ viewState model.width releaseState
            , column [ spacing 20, padding 10, width fill ]
                [ viewHeader
                , viewSwitches releaseState attemptedReleaseState
                , wrappedRow [ centerX, spacing 30, width fill, moveUp 20 ]
                    [ el [ width (fill |> minimum 320 |> maximum 1600), centerX ] <| html Chart.chart1
                    , el [ width (fill |> minimum 320 |> maximum 1600), centerX ] <| html Chart.chart2
                    ]
                , viewLog model
                , viewFooter
                ]
            ]


view : Model -> Html.Html Msg
view model =
    case model.releaseTransition of
        Stable from ->
            viewReleaseState model from Nothing [ viewEmptyOverlay ]

        Request from to ->
            viewReleaseState model from (Just to) [ viewRequest from to ]

        Moving from to ->
            viewReleaseState model from (Just to) [ viewMoving from to ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
