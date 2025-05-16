module Main exposing (..)

import Browser
import Browser.Events
import Course.Course as Course exposing (Course, Exercise, Lecture, course1)
import Html exposing (Html, a, button, div, h3, h4, h5, nav, text)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Images.Images as Img
import Json.Decode as Decode
import SyntaxHighlight as Highlight



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
    | GoToCourseOverview
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

        GoToCourseOverview ->
            ( { model
                | page =
                    CoursesOverview
                        { courses =
                            [ course1 ]
                        }
              }
            , Cmd.none
            )

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
                                        course.answeredExercices ++ [ ( exercise, answer ) ]
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
        [ header model
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
            [ Html.Attributes.class "alert bg-danger-subtle"
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
                , ( "btn-lg", True )
                , ( "w-100", True )
                , ( "btn-dark", checkUsername l.username )
                , ( "disabled", not (checkUsername l.username) )
                ]
            , Html.Attributes.disabled (not (checkUsername l.username))
            ]
            [ text "Start" ]
        ]


coursesOverview : CoursesOverviewPageModel -> Html Msg
coursesOverview c =
    div [ Html.Attributes.class "m-1" ]
        [ Html.h1
            [ Html.Attributes.class "display-5 text-center" ]
            [ text "Kursübersicht"
            ]
        , Html.p
            [ Html.Attributes.class "text-center" ]
            [ text
                ("Dir stehen " ++ String.fromInt (List.length c.courses) ++ " Kurse zur Verfügung: ")
            ]
        , div
            [ Html.Attributes.class "album" ]
            [ div
                [ Html.Attributes.class "container" ]
                [ div
                    [ Html.Attributes.class
                        "row row-cols-1 row-cols-sm-2 row-cols-md-3 g-3"
                    ]
                    [ div
                        [ Html.Attributes.class "col" ]
                        (List.map
                            (\course ->
                                div
                                    [ Html.Attributes.class "card shadow-sm", onClick (SelectCourse course) ]
                                    [ div
                                        [ Html.Attributes.class "card-title text-center" ]
                                        [ text course.title
                                        ]
                                    , div
                                        [ Html.Attributes.class "card-body" ]
                                        [ div
                                            [ Html.Attributes.class "card-text" ]
                                            [ text course.description ]
                                        , div
                                            [ Html.Attributes.class
                                                "d-flex justify-content-between align-items-center"
                                            ]
                                            [ div [] []
                                            , Html.small
                                                [ Html.Attributes.class "muted" ]
                                                [ text
                                                    (String.fromInt (List.length course.lectures)
                                                        ++ " Lektionen"
                                                    )
                                                ]
                                            ]
                                        ]
                                    ]
                            )
                            c.courses
                        )
                    ]
                ]
            ]
        , foot
        ]


coursePage : CoursePageModel -> Html Msg
coursePage c =
    div []
        [ h3
            [ Html.Attributes.class "display-5 text-center" ]
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
                            (List.map
                                (\( exercise, answer ) ->
                                    finishedExerciseView exercise answer
                                )
                                c.answeredExercices
                            )

            Nothing ->
                div [ Html.Attributes.class "album" ]
                    [ div
                        [ Html.Attributes.class "container" ]
                        [ div
                            [ Html.Attributes.class
                                "row row-cols-1 row-cols-sm-2 row-cols-md-3 g-3"
                            ]
                            [ div
                                [ Html.Attributes.class "col" ]
                                (List.map
                                    (\lecture ->
                                        div
                                            [ Html.Attributes.class
                                                "card shadow-sm m-1"
                                            , onClick (SelectLecture lecture)
                                            ]
                                            [ div
                                                [ Html.Attributes.class
                                                    "card-title text-center"
                                                ]
                                                [ text lecture.title
                                                ]
                                            , div
                                                [ Html.Attributes.class
                                                    "card-body"
                                                ]
                                                [ div
                                                    [ Html.Attributes.class
                                                        "card-text"
                                                    ]
                                                    [ text lecture.description
                                                    , div
                                                        [ Html.Attributes.class "d-flex justify-content-between align-items-center" ]
                                                        [ div [] []
                                                        , Html.small
                                                            [ Html.Attributes.class "muted" ]
                                                            [ text
                                                                (String.fromInt (List.length lecture.exercises)
                                                                    ++ " Aufgaben"
                                                                )
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]
                                    )
                                    c.course.lectures
                                )
                            ]
                        ]
                    ]
        ]


lectureView : Lecture -> Html Msg
lectureView l =
    div [ Html.Attributes.class "container fixed-bottom mb-2" ]
        [ h4 []
            [ text l.title
            ]
        , div []
            [ div
                []
                [ Html.p [] [ text l.description ]
                , Html.p []
                    [ text
                        ("Diese Lektion beinhaltet "
                            ++ String.fromInt (List.length l.exercises)
                            ++ " "
                            ++ (if List.length l.exercises == 1 then
                                    "Aufgabe"

                                else
                                    "Aufgaben"
                               )
                        )
                    ]
                ]
            ]
        , button
            [ onClick StartLecture
            , Html.Attributes.class "btn btn-dark btn-lg w-100 h-100"
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
    div [ Html.Attributes.class "card m-2 fixed-bottom" ]
        (case exercise of
            Course.SingleExpression singleExpression ->
                [ div
                    [ Html.Attributes.class "card-header text-center" ]
                    [ text singleExpression.title
                    ]
                , div
                    [ Html.Attributes.class "card-body" ]
                    [ div
                        [ Html.Attributes.class "card-title" ]
                        [ text
                            (case singleExpression.description of
                                Just d ->
                                    d

                                Nothing ->
                                    ""
                            )
                        ]
                    , div
                        [ Html.Attributes.class "card-content" ]
                        (highlightedExpressionView singleExpression.expression Nothing)
                    ]
                , runningExerciseAnswerView exercise singleExpression.answers
                ]

            Course.BinaryExpression binaryExpression ->
                [ div
                    [ Html.Attributes.class "card-header text-center" ]
                    [ text binaryExpression.title
                    ]
                , div
                    [ Html.Attributes.class "card-body" ]
                    [ div
                        [ Html.Attributes.class "card-title" ]
                        [ text
                            (case binaryExpression.description of
                                Just d ->
                                    d

                                Nothing ->
                                    ""
                            )
                        ]
                    , div
                        [ Html.Attributes.class "card-content" ]
                        (highlightedExpressionView
                            (String.join
                                " "
                                [ binaryExpression.leftExpression
                                , binaryExpression.operator
                                , binaryExpression.rightExpression
                                ]
                            )
                            Nothing
                        )
                    ]
                , runningExerciseAnswerView exercise binaryExpression.answers
                ]

            Course.FunctionExpression functionExpression ->
                [ div
                    [ Html.Attributes.class "card-header text-center" ]
                    [ text functionExpression.title
                    ]
                , div
                    [ Html.Attributes.class "card-body" ]
                    [ div
                        [ Html.Attributes.class "card-title" ]
                        [ text
                            (case functionExpression.description of
                                Just d ->
                                    d

                                Nothing ->
                                    ""
                            )
                        ]
                    , div
                        [ Html.Attributes.class "card-content" ]
                        (highlightedExpressionView
                            (functionExpression.functionName
                                ++ " "
                                ++ String.join " " functionExpression.arguments
                            )
                            Nothing
                        )
                    ]
                , runningExerciseAnswerView exercise functionExpression.answers
                ]

            Course.GuardExpression guardExpression ->
                [ div
                    [ Html.Attributes.class "card-header text-center" ]
                    [ text guardExpression.title
                    ]
                , div
                    [ Html.Attributes.class "card-body" ]
                    [ div
                        [ Html.Attributes.class "card-title" ]
                        [ text
                            (case guardExpression.description of
                                Just d ->
                                    d

                                Nothing ->
                                    ""
                            )
                        ]
                    , div
                        [ Html.Attributes.class "card-content" ]
                        (highlightedExpressionView
                            (guardExpression.functionName
                                ++ " "
                                ++ String.join " " guardExpression.arguments
                                ++ guardExpression.expression
                            )
                            Nothing
                        )
                    ]
                , runningExerciseAnswerView exercise guardExpression.answers
                ]

            Course.PatternMatchingExpression patternMatchingExpression ->
                [ div
                    [ Html.Attributes.class "card-header text-center" ]
                    [ text patternMatchingExpression.title
                    ]
                , div
                    [ Html.Attributes.class "card-body" ]
                    [ div
                        [ Html.Attributes.class "card-title" ]
                        [ text
                            (case patternMatchingExpression.description of
                                Just d ->
                                    d

                                Nothing ->
                                    ""
                            )
                        ]
                    , div
                        [ Html.Attributes.class "card-content" ]
                        (highlightedExpressionView
                            (String.join "\n" patternMatchingExpression.patterns)
                            Nothing
                        )
                    ]
                , runningExerciseAnswerView exercise patternMatchingExpression.answers
                ]
        )


runningExerciseAnswerView : Course.Exercise -> List Course.Answer -> Html Msg
runningExerciseAnswerView exercise answers =
    div
        [ Html.Attributes.class "card-footer btn-toolbar" ]
        (List.map
            (\answer ->
                div
                    [ Html.Attributes.class "btn bg-white btn-outline-dark m-1"
                    , onClick (SelectAnswer exercise answer)
                    ]
                    (highlightedInlineView answer.code)
            )
            answers
        )


highlightedExpressionView : String -> Maybe Int -> List (Html Msg)
highlightedExpressionView expression line =
    [ Highlight.useTheme Highlight.gitHub
    , Highlight.elm
        expression
        |> Result.map (Highlight.toBlockHtml line)
        |> Result.withDefault
            (Html.pre []
                [ Html.code
                    []
                    [ text expression
                    ]
                ]
            )
    ]


highlightedInlineView : String -> List (Html Msg)
highlightedInlineView expression =
    [ Highlight.useTheme Highlight.gitHub
    , Highlight.elm
        expression
        |> Result.map Highlight.toInlineHtml
        |> Result.withDefault
            (Html.pre []
                [ Html.code
                    []
                    [ text expression
                    ]
                ]
            )
    ]


finishedExerciseAnswerView : List Course.Answer -> Course.Answer -> Html Msg
finishedExerciseAnswerView answers studentAnswer =
    div
        [ Html.Attributes.class
            "card-footer btn-toolbar"
        ]
        (List.map
            (\answer ->
                div
                    [ Html.Attributes.class "btn bg-white m-1 pe-none"
                    , if answer == studentAnswer then
                        Html.Attributes.classList
                            [ ( "btn-outline-success", answer.isCorrect )
                            , ( "btn-outline-danger", not answer.isCorrect )
                            ]

                      else
                        Html.Attributes.classList
                            [ ( "btn-outline-success", answer.isCorrect )
                            , ( "btn-outline-dark opacity-50", not answer.isCorrect )
                            ]
                    ]
                    (highlightedInlineView answer.code)
            )
            answers
        )


finishedExerciseView : Exercise -> Course.Answer -> Html Msg
finishedExerciseView exercise answer =
    case exercise of
        Course.SingleExpression singleExpressionModel ->
            div
                [ Html.Attributes.class "card m-2" ]
                [ div
                    [ Html.Attributes.class "card-title text-center" ]
                    [ Html.h5
                        []
                        [ text singleExpressionModel.title
                        ]
                    ]
                , div
                    [ Html.Attributes.class "card-body" ]
                    ((case singleExpressionModel.description of
                        Just d ->
                            div
                                [ Html.Attributes.class "card-text"
                                ]
                                [ text d ]

                        Nothing ->
                            text ""
                     )
                        :: highlightedExpressionView
                            singleExpressionModel.expression
                            Nothing
                    )
                , finishedExerciseAnswerView
                    singleExpressionModel.answers
                    answer
                ]

        Course.BinaryExpression binaryExpressionModel ->
            div
                [ Html.Attributes.class "card m-2" ]
                [ div
                    [ Html.Attributes.class "card-title text-center" ]
                    [ h5
                        []
                        [ text binaryExpressionModel.title
                        ]
                    ]
                , div
                    [ Html.Attributes.class "card-body" ]
                    [ case binaryExpressionModel.description of
                        Just d ->
                            div
                                [ Html.Attributes.class "card-text"
                                ]
                                [ text d ]

                        Nothing ->
                            text ""
                    , Html.code
                        []
                        [ text (binaryExpressionModel.leftExpression ++ " " ++ binaryExpressionModel.operator ++ " " ++ binaryExpressionModel.rightExpression)
                        ]
                    , finishedExerciseAnswerView
                        binaryExpressionModel.answers
                        answer
                    ]
                ]

        Course.FunctionExpression functionExpressionModel ->
            div []
                [ div
                    [ Html.Attributes.class "card m-2" ]
                    [ div
                        [ Html.Attributes.class "card-header text-center" ]
                        [ h5
                            []
                            [ text functionExpressionModel.title
                            ]
                        ]
                    , div
                        [ Html.Attributes.class "card-body" ]
                        [ case functionExpressionModel.description of
                            Just d ->
                                div
                                    [ Html.Attributes.class "card-text"
                                    ]
                                    [ text d ]

                            Nothing ->
                                text ""
                        , Html.code
                            []
                            [ text (functionExpressionModel.functionName ++ " " ++ (functionExpressionModel.arguments |> String.join " "))
                            ]
                        ]
                    , finishedExerciseAnswerView
                        functionExpressionModel.answers
                        answer
                    ]
                ]

        Course.GuardExpression guardExpressionModel ->
            div []
                [ div
                    [ Html.Attributes.class "card m-2" ]
                    [ div
                        [ Html.Attributes.class "card-header text-center" ]
                        [ h5
                            []
                            [ text guardExpressionModel.title
                            ]
                        ]
                    , div
                        [ Html.Attributes.class "card-body" ]
                        ((case guardExpressionModel.description of
                            Just d ->
                                div
                                    [ Html.Attributes.class "card-text"
                                    ]
                                    [ text d ]

                            Nothing ->
                                text ""
                         )
                            :: highlightedExpressionView
                                (String.join
                                    " "
                                    (guardExpressionModel.functionName :: (guardExpressionModel.arguments ++ [ guardExpressionModel.expression ]))
                                )
                                Nothing
                        )
                    , finishedExerciseAnswerView
                        guardExpressionModel.answers
                        answer
                    ]
                ]

        Course.PatternMatchingExpression patternExpressionModel ->
            div
                []
                [ div
                    [ Html.Attributes.class "card m-2" ]
                    [ div
                        [ Html.Attributes.class "card-header text-center" ]
                        [ h5
                            []
                            [ text patternExpressionModel.title
                            ]
                        ]
                    , div
                        [ Html.Attributes.class "card-body" ]
                        ((case patternExpressionModel.description of
                            Just d ->
                                div
                                    [ Html.Attributes.class "card-text"
                                    ]
                                    [ text d ]

                            Nothing ->
                                text ""
                         )
                            :: highlightedExpressionView
                                (String.join "\n" patternExpressionModel.patterns)
                                Nothing
                        )
                    , finishedExerciseAnswerView
                        patternExpressionModel.answers
                        answer
                    ]
                ]


header : Model -> Html Msg
header m =
    nav [ Html.Attributes.class "navbar navbar-expand-lg bg-body-tertiary" ]
        [ div
            [ Html.Attributes.class "container-fluid" ]
            (a
                [ Html.Attributes.class "navbar-brand" ]
                [ Img.logo
                ]
                :: (case m.page of
                        Landing _ ->
                            []

                        _ ->
                            [ h5 [] [ text (Maybe.withDefault "" (Maybe.map .name m.user)) ]
                            , button
                                [ Html.Attributes.class "navbar-toggler"
                                , Html.Attributes.attribute "data-bs-toggle" "collapse"
                                , Html.Attributes.attribute "data-bs-target" "#navbarNav"
                                ]
                                [ Html.span
                                    [ Html.Attributes.class "navbar-toggler-icon"
                                    ]
                                    []
                                ]
                            , div
                                [ Html.Attributes.class "collapse navbar-collapse"
                                , Html.Attributes.id "navbarNav"
                                ]
                                [ Html.ul
                                    [ Html.Attributes.class "navbar-nav" ]
                                    [ Html.li
                                        [ Html.Attributes.class "nav-item" ]
                                        [ a
                                            [ Html.Attributes.class "nav-link"
                                            , Html.Attributes.classList
                                                [ ( "nav-link", True )
                                                , ( "active"
                                                  , case m.page of
                                                        CoursesOverview _ ->
                                                            True

                                                        _ ->
                                                            False
                                                  )
                                                ]
                                            , onClick GoToCourseOverview
                                            ]
                                            [ text "Kursübersicht"
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                   )
            )
        ]


foot : Html Msg
foot =
    div
        [ Html.Attributes.class "footer fixed-bottom text-center" ]
        [ text "Copyright © 2025" ]


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
