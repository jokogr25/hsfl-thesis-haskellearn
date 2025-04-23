module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (classList, lang, placeholder, style, type_)
import Html.Events exposing (onClick, onInput)
import List exposing (length)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Username =
    String


type Model
    = Landing Username
    | Start Username


init : Model
init =
    Landing ""



-- UPDATE


type Msg
    = EnteringName String
    | EnteringNameDone


update : Msg -> Model -> Model
update msg model =
    case msg of
        EnteringName name ->
            Landing name

        EnteringNameDone ->
            case model of
                Landing name ->
                    if String.length name > 2 then
                        Start name

                    else
                        model

                _ ->
                    model



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Landing _ ->
            landingPage model

        Start name ->
            div []
                [ text ("Hallo " ++ name ++ "!")
                ]


landingPage : Model -> Html Msg
landingPage model =
    div [ Html.Attributes.class "container" ]
        [ h1 []
            [ text "earn you a haskell"
            ]
        , div
            [ Html.Attributes.class "mb-3" ]
            [ Html.input
                [ onInput EnteringName
                , placeholder "Gib deinen Namen ein"
                , type_ "text"
                , Html.Attributes.class "form-control"
                ]
                []
            ]
        , button
            [ onClick EnteringNameDone
            , Html.Attributes.class "btn btn-primary"
            , Html.Attributes.disabled
                (case model of
                    Landing name ->
                        String.length name < 3

                    _ ->
                        False
                )
            ]
            [ text "Start" ]
        ]
