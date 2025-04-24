module Main exposing (..)

import Browser
import Browser.Events
import Course.Course as Course exposing (Course, Lecture, course1, course2)
import Html exposing (Html, a, button, div, h1, nav, text)
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


type alias LandingPageModel =
    { username : Maybe String
    }


type alias StartPageModel =
    { username : String
    , courses : List Course
    }


type alias CoursePageModel =
    { username : String
    , lectures : List Lecture
    }


type Page
    = Landing LandingPageModel
    | Start StartPageModel
    | Course CoursePageModel


type alias Model =
    { page : Page
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = Landing { username = Nothing }
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
            case model.page of
                Landing l ->
                    ( { model
                        | page = Landing { l | username = Just name }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EnteringNameDone ->
            case model.page of
                Landing l ->
                    if checkUsername l.username then
                        ( { model
                            | page =
                                case l.username of
                                    Just username ->
                                        Start { username = username, courses = [ course1, course2 ] }

                                    Nothing ->
                                        Landing { l | username = l.username }
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
    div []
        [ header
        , case model.page of
            Landing l ->
                landingPage l

            Start s ->
                startPage s

            Course c ->
                coursePage c
        , foot
        ]


landingPage : LandingPageModel -> Html Msg
landingPage l =
    div [ Html.Attributes.class "container" ]
        [ div
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
                , ( "btn-primary", checkUsername l.username )
                , ( "btn-secondary", not (checkUsername l.username) )
                , ( "btn-lg", True )
                , ( "btn-block", True )
                ]
            , Html.Attributes.disabled (not (checkUsername l.username))
            ]
            [ text "Start" ]
        ]


startPage : StartPageModel -> Html Msg
startPage s =
    div []
        [ pageHeader s.username
        , div
            [ Html.Attributes.class "album py-5 bg-light" ]
            (List.map
                (\course ->
                    div
                        [ Html.Attributes.class "card mb-4" ]
                        [ div
                            [ Html.Attributes.class "card-title text-center" ]
                            [ text course.title
                            ]
                        , div
                            [ Html.Attributes.class "card-body" ]
                            [ div
                                [ Html.Attributes.class "card-text" ]
                                [ text course.description ]
                            ]
                        ]
                )
                s.courses
            )
        ]


coursePage : CoursePageModel -> Html Msg
coursePage _ =
    div [] []


header : Html Msg
header =
    nav [ Html.Attributes.class "navbar" ]
        [ a [ Html.Attributes.class "navbar-brand" ]
            [ text "earn you a haskell"
            ]
        ]


foot : Html Msg
foot =
    div [] []


pageHeader : String -> Html Msg
pageHeader username =
    div []
        [ h1 []
            [ text ("Hallo " ++ username ++ "!")
            ]
        ]


checkUsername : Maybe String -> Bool
checkUsername name =
    case name of
        Just n ->
            String.length n > 2

        Nothing ->
            False



-- KEY DECODER


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
