module Main exposing (..)

import Browser
import Browser.Events
import Course.Course as Course exposing (Course, Exercise, Lecture, course1)
import Html exposing (Html, a, button, div, h3, h4, h6, nav, text)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Images.Images as Img
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


type alias CoursesOverviewPageModel =
    { courses : List Course
    }


type alias CoursePageModel =
    { course : Course
    , selectedLecture : Maybe Lecture
    , lectureState : LectureState
    , answeredExercices : List ( Exercise, Course.Answer )
    }


type Page
    = Landing LandingPageModel
    | CoursesOverview CoursesOverviewPageModel
    | Course CoursePageModel


type alias Model =
    { page : Page
    , user : Maybe User
    }


type LectureState
    = NotStarted
    | Running
    | Finished


type Msg
    = EnteringName String
    | EnteringNameDone
    | SelectCourse Course
    | SelectLecture Lecture
    | StartLecture
    | StopLecture
    | SelectAnswer Course.Exercise Course.Answer
    | NoOp


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page =
            Landing
                { username = Nothing
                , error = Nothing
                }
      , user = Nothing
      }
    , Cmd.none
    )



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
                                    CoursesOverview
                                        { courses =
                                            [ course1 ]
                                        }
                                , user =
                                    Just
                                        { name = username }
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model
                                | page =
                                    Landing
                                        { l
                                            | error = Just UsernameIncorrect
                                        }
                              }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        SelectCourse course ->
            ( { model
                | page =
                    Course
                        { course = course
                        , selectedLecture = Nothing
                        , lectureState = NotStarted
                        , answeredExercices = []
                        }
              }
            , Cmd.none
            )

        SelectLecture lecture ->
            case model.page of
                Course course ->
                    ( { model
                        | page =
                            Course
                                { course
                                    | selectedLecture = Just lecture
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        StartLecture ->
            case model.page of
                Course course ->
                    ( { model
                        | page =
                            Course
                                { course
                                    | lectureState = Running
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        StopLecture ->
            case model.page of
                Course course ->
                    ( { model
                        | page =
                            Course
                                { course
                                    | lectureState = NotStarted
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SelectAnswer exercise answer ->
            case model.page of
                Course course ->
                    ( { model
                        | page =
                            Course
                                { course
                                    | lectureState =
                                        case course.selectedLecture of
                                            Just lecture ->
                                                if List.length lecture.exercises == 1 then
                                                    Finished

                                                else
                                                    Running

                                            Nothing ->
                                                NotStarted
                                    , selectedLecture =
                                        case course.selectedLecture of
                                            Just lecture ->
                                                Just
                                                    { lecture
                                                        | exercises =
                                                            List.tail lecture.exercises
                                                                |> Maybe.withDefault []
                                                    }

                                            _ ->
                                                Nothing
                                    , answeredExercices =
                                        ( exercise, answer ) :: course.answeredExercices
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ header model.user
        , case model.page of
            Landing l ->
                landingPage l

            CoursesOverview s ->
                coursesOverview s

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
                , Html.Attributes.class "form-control form-control-lg"
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


coursesOverview : CoursesOverviewPageModel -> Html Msg
coursesOverview c =
    div [ Html.Attributes.class "container mb-2" ]
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
                        [ Html.Attributes.class "card m-2", onClick (SelectCourse course) ]
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
coursePage c =
    div [ Html.Attributes.class "container mb-2" ]
        [ h3
            []
            [ text c.course.title ]
        , case c.selectedLecture of
            Just l ->
                case c.lectureState of
                    NotStarted ->
                        lectureView l

                    Running ->
                        case List.head l.exercises of
                            Just e ->
                                runningLectureView l e

                            Nothing ->
                                div []
                                    [ text "Hier stimmt was nicht!"
                                    ]

                    Finished ->
                        div []
                            [ h4 []
                                [ text
                                    ("Du hast "
                                        ++ String.fromInt (List.length c.answeredExercices)
                                        ++ " Aufgaben abgeschlossen!"
                                    )
                                ]
                            ]

            Nothing ->
                div []
                    (List.map
                        (\lecture ->
                            div
                                [ Html.Attributes.class "card m-2"
                                , onClick (SelectLecture lecture)
                                ]
                                [ div
                                    [ Html.Attributes.class "card-title text-center" ]
                                    [ text lecture.title
                                    ]
                                , div
                                    [ Html.Attributes.class "card-body" ]
                                    [ div
                                        [ Html.Attributes.class "card-text" ]
                                        [ text lecture.description ]
                                    ]
                                ]
                        )
                        c.course.lectures
                    )
        ]


lectureView : Lecture -> Html Msg
lectureView l =
    div [ Html.Attributes.class "container" ]
        [ h4 []
            [ text l.title
            ]
        , div [ Html.Attributes.class "card text-center m-2" ]
            [ div
                [ Html.Attributes.class "card-title" ]
                [ text
                    ("Diese Lektion beinhaltet " ++ String.fromInt (List.length l.exercises) ++ " Übung(en).")
                ]
            ]
        , button
            [ onClick StartLecture
            , Html.Attributes.class "btn btn-success btn-lg m-2"
            ]
            [ text "Lektion starten"
            ]
        ]


runningLectureView : Lecture -> Exercise -> Html Msg
runningLectureView l e =
    div [ Html.Attributes.class "container" ]
        [ h4 []
            [ text l.title
            ]
        , div
            []
            [ excerciseView e
            ]
        ]


excerciseView : Exercise -> Html Msg
excerciseView exercise =
    case exercise of
        Course.SingleExpression e ->
            div
                [ Html.Attributes.class "card m-2 fixed-bottom"
                ]
                [ div
                    [ Html.Attributes.class "card-header text-center" ]
                    [ text e.title
                    ]
                , div
                    [ Html.Attributes.class "card-body" ]
                    [ div
                        [ Html.Attributes.class "card-title" ]
                        [ text
                            (case e.description of
                                Just d ->
                                    d

                                Nothing ->
                                    ""
                            )
                        ]
                    , div
                        [ Html.Attributes.class "card-content" ]
                        [ Html.code
                            []
                            [ text e.expression ]
                        ]
                    ]
                , div
                    [ Html.Attributes.class "card-footer btn-toolbar" ]
                    (List.map
                        (\answer ->
                            div
                                [ Html.Attributes.class "btn btn-dark m-1"
                                , onClick (SelectAnswer exercise answer)
                                ]
                                [ Html.code
                                    []
                                    [ text answer.code
                                    ]
                                ]
                        )
                        e.answers
                    )
                ]


header : Maybe User -> Html Msg
header user =
    nav [ Html.Attributes.class "navbar" ]
        (a
            [ Html.Attributes.class "navbar-brand" ]
            [ Img.logo
            ]
            :: (case user of
                    Just u ->
                        [ text (u.name ++ " le(a)rnt grad") ]

                    Nothing ->
                        []
               )
        )


foot : Html Msg
foot =
    div
        [ Html.Attributes.class "footer fixed-bottom text-center" ]
        [ text "Copyright © 2025" ]


appHeader : String -> Html Msg
appHeader username =
    div [ Html.Attributes.class "ml-1" ]
        [ h3 []
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
