module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import History exposing (History)
import Html exposing (Html)



---- MODEL ----


type alias Model =
    { health : History Int
    , maxHealth : Int
    , experience : History Int
    }


init : ( Model, Cmd Msg )
init =
    let
        maxHealth =
            10
    in
    ( { maxHealth = maxHealth, health = History.new maxHealth, experience = History.new 0 }, Cmd.none )



---- UPDATE ----


type Msg
    = IncrementHealth Int
    | IncrementExperience Int
    | UndoHealthChange
    | UndoExperienceChange
    | IncrementMaxHealth Int
    | Reset
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        IncrementHealth newHealth ->
            ( { model
                | health =
                    History.append
                        (clamp 0 model.maxHealth (newHealth + History.current model.health))
                        model.health
              }
            , Cmd.none
            )

        IncrementExperience newExperience ->
            ( { model
                | experience =
                    History.append
                        (max 0 (newExperience + History.current model.experience))
                        model.experience
              }
            , Cmd.none
            )

        IncrementMaxHealth inc ->
            let
                newMaxHealth =
                    max 0 (inc + model.maxHealth)
            in
            ( { model
                | maxHealth = newMaxHealth
                , health = History.replace (min newMaxHealth (History.current model.health)) model.health
              }
            , Cmd.none
            )

        UndoHealthChange ->
            ( { model
                | health =
                    History.undo model.health
              }
            , Cmd.none
            )

        UndoExperienceChange ->
            ( { model
                | experience =
                    History.undo model.experience
              }
            , Cmd.none
            )

        Reset ->
            ( { model
                | health =
                    History.append model.maxHealth model.health
                , experience =
                    History.append 0 model.experience
              }
            , Cmd.none
            )



---- VIEW ----


gray100 : Color
gray100 =
    rgb 0.9 0.9 0.9


red500 : Color
red500 =
    rgb255 252 3 3


blue500 : Color
blue500 =
    rgb255 0 125 214


green500 : Color
green500 =
    rgb255 0 214 43


white : Color
white =
    rgb 1 1 1


incrementButton : Int -> (Int -> Msg) -> Element Msg
incrementButton x incrementMsg =
    let
        color =
            if x < 0 then
                red500

            else
                green500
    in
    Input.button
        [ Background.color color, padding 16 ]
        { onPress = Just (incrementMsg x)
        , label =
            el [ Font.color white, Font.bold ]
                ((if x > 0 then
                    "+"

                  else
                    ""
                 )
                    ++ String.fromInt x
                    |> text
                )
        }


statCard : String -> Int -> Msg -> (Int -> Msg) -> Element Msg
statCard name value undoMsg incrementMsg =
    row [ width fill, Background.color white, Border.rounded 8, clip ]
        [ column [ spacing 2 ]
            [ incrementButton -3 incrementMsg
            , incrementButton -2 incrementMsg
            , incrementButton -1 incrementMsg
            ]
        , column [ centerX ]
            [ el [ centerX, Font.size 12 ] (text (String.toUpper name))
            , el [ centerX, Font.size 60, Font.extraBold ] (text (String.fromInt value))
            , Input.button [ centerX, Font.size 14, Font.color blue500, paddingXY 8 4 ]
                { onPress = Just undoMsg, label = text "UNDO" }
            ]
        , column [ spacing 2 ]
            [ incrementButton 3 incrementMsg
            , incrementButton 2 incrementMsg
            , incrementButton 1 incrementMsg
            ]
        ]


view : Model -> Html Msg
view model =
    layout [ Background.color gray100 ]
        (el
            [ width (fill |> maximum 460), centerX, padding 16 ]
            (column [ width fill, spacing 16 ]
                [ statCard "Health" (History.current model.health) UndoHealthChange IncrementHealth
                , statCard "Experience" (History.current model.experience) UndoExperienceChange IncrementExperience
                , row [ spacing 16 ]
                    [ Input.button
                        [ Font.size 14, Background.color blue500, Font.color white, paddingXY 16 8, Border.rounded 4, Font.bold ]
                        { onPress = Just Reset, label = el [] (text "RESET") }
                    , row [ spacing 8 ]
                        [ Input.button
                            [ Font.size 14, Background.color blue500, Font.color white, padding 8, Border.rounded 4, Font.bold ]
                            { onPress = Just (IncrementMaxHealth -1), label = el [] (text "-1") }
                        , el [ centerY ] (text (String.fromInt model.maxHealth))
                        , Input.button
                            [ Font.size 14, Background.color blue500, Font.color white, padding 8, Border.rounded 4, Font.bold ]
                            { onPress = Just (IncrementMaxHealth 1), label = el [] (text "+1") }
                        ]
                    ]
                ]
            )
        )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
