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



-- MODEL


type Page
    = Landing
    | Start


type alias Model =
    { page : Page
    , username : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = Landing
      , username = Nothing
      }
    , Cmd.none
    )


type Msg
    = EnteringName String
    | EnteringNameDone
    | NoOp



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



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EnteringName name ->
            ( { model | username = Just name }, Cmd.none )

        EnteringNameDone ->
            case model.page of
                Landing ->
                    if checkUsername model.username then
                        ( { model
                            | page = Start
                            , username = model.username
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        Landing ->
            landingPage model

        Start ->
            div []
                [ text
                    ("Hallo " ++ Maybe.withDefault "Guest" model.username ++ "!")
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
            , Html.Attributes.classList
                [ ( "btn", True )
                , ( "btn-primary", checkUsername model.username )
                , ( "btn-secondary", not (checkUsername model.username) )
                , ( "btn-lg", True )
                , ( "btn-block", True )
                ]
            , Html.Attributes.disabled
                (case model.page of
                    Landing ->
                        not (checkUsername model.username)

                    _ ->
                        False
                )
            ]
            [ text "Start" ]
        ]


checkUsername : Maybe String -> Bool
checkUsername name =
    case name of
        Just n ->
            String.length n > 2

        Nothing ->
            False


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
