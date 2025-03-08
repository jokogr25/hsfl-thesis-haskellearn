module Main exposing (..)

import Browser
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (classList, style)
import Html.Events exposing (onClick)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    Int


init : Model
init =
    1



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            case model of
                0 ->
                    model

                _ ->
                    model
                        - 1



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ card model
        ]


card : Model -> Html Msg
card model =
    div
        [ classList [ ( "card", True ) ]
        , style "width" "18rem"
        , style "margin" "auto"
        ]
        [ img [ classList [ ( "card-img-top", True ) ] ] []
        , div [ classList [ ( "card-body", True ) ] ]
            [ Html.h5
                [ classList [ ( "card-title", True ) ] ]
                [ text "Counter" ]
            , Html.h1
                [ classList [ ( "card-text", True ) ]
                ]
                [ text (String.fromInt model) ]
            , button
                [ onClick Decrement
                , classList [ ( "btn btn-primary btn-sm", True ) ]
                ]
                [ text "RUNTER" ]
            , button
                [ onClick Increment
                , classList [ ( "btn btn-secondary btn-sm", True ) ]
                ]
                [ text "RAUF" ]
            ]
        ]
