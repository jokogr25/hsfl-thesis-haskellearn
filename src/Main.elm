module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map
            (\key ->
                case key of
                    Enter ->
                        EnteringNameDone

                    _ ->
                        NoOp
            )
            (Browser.Events.onKeyDown keyDecoder)
        ]



-- MODEL


type alias Username =
    String


type Model
    = Landing Username
    | Start Username


init : () -> ( Model, Cmd Msg )
init _ =
    ( Landing "", Cmd.none )



-- UPDATE


type Msg
    = EnteringName String
    | EnteringNameDone
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EnteringName name ->
            ( Landing name, Cmd.none )

        EnteringNameDone ->
            case model of
                Landing name ->
                    if String.length name > 2 then
                        ( Start name, Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )



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


type Key
    = Enter
    | Other


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Key
toKey key =
    case key of
        "Enter" ->
            Enter

        _ ->
            Other
