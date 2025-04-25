module Main exposing (..)

import Browser
import Browser.Events
import Course.Course as Course exposing (Course, Lecture, course1, course2)
import Html exposing (Html, a, button, div, h1, h6, nav, text)
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


type alias User =
    { name : String
    }


type Error
    = UsernameIncorrect


type alias LandingPageModel =
    { username : Maybe String
    , error : Maybe Error
    }


type alias CourseOverviewPageModel =
    { courses : List Course
    }


type alias CoursePageModel =
    { lectures : List Lecture
    }


type Page
    = Landing LandingPageModel
    | CourseOverview CourseOverviewPageModel
    | Course CoursePageModel


type alias Model =
    { page : Page
    , user : Maybe User
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = Landing { username = Nothing, error = Nothing }
      , user = Nothing
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
                Landing _ ->
                    ( { model
                        | page =
                            Landing
                                { username = Just name
                                , error =
                                    if xor (checkUsername (Just name)) (String.length name == 0) then
                                        Nothing

                                    else
                                        Just UsernameIncorrect
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EnteringNameDone ->
            case model.page of
                Landing l ->
                    case l.username of
                        Just username ->
                            ( { model
                                | page =
                                    CourseOverview
                                        { courses =
                                            [ course1, course2 ]
                                        }
                                , user =
                                    Just
                                        { name = username }
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model
                                | page = Landing { l | error = Just UsernameIncorrect }
                              }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ header
        , case model.user of
            Just user ->
                appHeader user.name

            Nothing ->
                text ""
        , case model.page of
            Landing l ->
                landingPage l

            CourseOverview s ->
                courseOverview s

            Course c ->
                coursePage c
        ]


landingPage : LandingPageModel -> Html Msg
landingPage l =
    div [ Html.Attributes.class "container fixed-bottom mb-2" ]
        [ div
            [ Html.Attributes.class "alert alert-danger mt-2"
            , Html.Attributes.hidden
                (case l.error of
                    Just error ->
                        case error of
                            UsernameIncorrect ->
                                False

                    Nothing ->
                        True
                )
            ]
            [ text "Dein Name muss mindestens drei Zeichen lang sein."
            ]
        , div
            [ Html.Attributes.class "mb-1" ]
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


courseOverview : CourseOverviewPageModel -> Html Msg
courseOverview c =
    div [ Html.Attributes.class "container" ]
        [ h6
            [ Html.Attributes.class "m-1" ]
            [ text
                ("Dir stehen " ++ String.fromInt (List.length c.courses) ++ " Kurse zur Verfügung: ")
            ]
        , div
            [ Html.Attributes.class "album p-1 bg-light" ]
            (List.map
                (\course ->
                    div
                        [ Html.Attributes.class "card m-2" ]
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
                c.courses
            )
        , foot
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
    div
        [ Html.Attributes.class "footer fixed-bottom text-center" ]
        [ text "Copyright © 2025" ]


appHeader : String -> Html Msg
appHeader username =
    div [ Html.Attributes.class "m-1" ]
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
