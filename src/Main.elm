module Main exposing (..)

import Browser
import Browser.Events
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


type alias Course =
    { id : Int
    , title : String
    , description : String
    , lectures : List Lecture
    }


type alias LearningExample =
    { id : Int
    , title : String
    , expression : String
    , description : Maybe String
    }


type alias LearningContent =
    { id : Int
    , title : String
    , description : String
    , examples : List LearningExample
    }


type alias Lecture =
    { id : Int
    , title : String
    , description : String
    , learningContent : LearningContent
    , exercises : List Exercise
    , badge : Badge
    }


type Exercise
    = SingleExpression SingleExpressionModel
    | BinaryExpression BinaryExpressionModel
    | FunctionExpression FunctionExpressionModel
    | GuardExpression GuardExpressionModel
    | PatternMatchingExpression PatternMatchingExpressionModel


type alias SingleExpressionModel =
    { id : Int
    , title : String
    , description : Maybe String
    , expression : String
    , answers : List Answer
    }


type alias BinaryExpressionModel =
    { id : Int
    , title : String
    , description : Maybe String
    , answers : List Answer
    , leftExpression : String
    , rightExpression : String
    , operator : String
    }


type alias FunctionExpressionModel =
    { id : Int
    , title : String
    , description : Maybe String
    , answers : List Answer
    , functionName : String
    , arguments : List String
    }


type alias GuardExpressionModel =
    { id : Int
    , title : String
    , description : Maybe String
    , functionName : String
    , expression : String
    , arguments : List String
    , answers : List Answer
    }


type alias PatternMatchingExpressionModel =
    { id : Int
    , title : String
    , description : Maybe String
    , patterns : List String
    , answers : List Answer
    }


type alias Answer =
    { code : String
    , isCorrect : Bool
    }


type alias Badge =
    { id : String
    , name : String
    , image : Html Msg
    }


type alias User =
    { name : String
    , badges : List Badge
    }


type Error
    = UsernameIncorrect


type alias LandingPageModel =
    { username : Maybe String
    , error : Maybe Error
    }


type Page
    = Landing LandingPageModel
    | CoursesOverview (List Course)
    | CoursePage Course
    | LecturePage Lecture
    | LearningContentPage Lecture Int
    | RunningQuiz Lecture (List Exercise) (List ( Exercise, Answer ))
    | FinishedQuiz Lecture (List ( Exercise, Answer )) Int
    | WinningQuiz Lecture


type alias Model =
    { page : Page
    , user : Maybe User
    }


type Msg
    = EnteringName String
    | EnteringNameDone
    | SelectCourse Course
    | SelectLecture Lecture
    | StartLecture
    | NextExample
    | StartQuiz
    | SelectAnswer Exercise Answer
    | GoToCourseOverview
    | Next
    | Prev
    | AddBadge Badge
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

                    ArrowRight ->
                        Next

                    ArrowLeft ->
                        Prev

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
                    CoursesOverview [ course1 ]
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
                                        [ course1 ]
                                , user =
                                    Just
                                        { name = username, badges = [] }
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
                    CoursePage course
              }
            , Cmd.none
            )

        SelectLecture lecture ->
            case model.page of
                CoursePage _ ->
                    ( { model
                        | page =
                            LecturePage lecture
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        StartLecture ->
            case model.page of
                LecturePage lecture ->
                    ( { model
                        | page =
                            LearningContentPage lecture 0
                      }
                    , Cmd.none
                    )

                FinishedQuiz lecture _ _ ->
                    ( { model
                        | page =
                            RunningQuiz lecture lecture.exercises []
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SelectAnswer exercise answer ->
            case model.page of
                RunningQuiz lecture remainingExercises answeredExercises ->
                    ( { model
                        | page =
                            let
                                newAnswers =
                                    answeredExercises ++ [ ( exercise, answer ) ]

                                newRemainingExercises =
                                    List.filter
                                        (\e -> e /= exercise)
                                        remainingExercises
                            in
                            if List.length newRemainingExercises == 0 then
                                if List.all (\( _, a ) -> a.isCorrect) newAnswers then
                                    WinningQuiz lecture

                                else
                                    FinishedQuiz lecture newAnswers 0

                            else
                                RunningQuiz lecture newRemainingExercises newAnswers
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Next ->
            case model.page of
                FinishedQuiz lecture answeredExercises i ->
                    ( { model
                        | page =
                            if
                                List.length
                                    (List.filter (\( _, a ) -> not a.isCorrect) answeredExercises)
                                    - 1
                                    == i
                            then
                                FinishedQuiz lecture answeredExercises i

                            else
                                FinishedQuiz lecture answeredExercises (i + 1)
                      }
                    , Cmd.none
                    )

                LearningContentPage lecture i ->
                    ( { model
                        | page = LearningContentPage lecture (i + 1)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Prev ->
            case model.page of
                FinishedQuiz lecture answeredExercises i ->
                    ( { model
                        | page =
                            FinishedQuiz
                                lecture
                                answeredExercises
                                (max 0 (i - 1))
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        AddBadge badge ->
            case model.user of
                Just user ->
                    ( { model
                        | user =
                            Just
                                { user
                                    | badges =
                                        if List.any (\b -> b == badge) user.badges then
                                            user.badges

                                        else
                                            badge :: user.badges
                                }
                        , page = CoursesOverview [ course1 ]
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        NextExample ->
            case model.page of
                LearningContentPage lecture i ->
                    ( { model
                        | page = LearningContentPage lecture (i + 1)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        StartQuiz ->
            case model.page of
                LearningContentPage lecture _ ->
                    ( { model
                        | page =
                            RunningQuiz lecture lecture.exercises []
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
        , case model.user of
            Just user ->
                case model.page of
                    CoursesOverview s ->
                        coursesOverview s user

                    CoursePage c ->
                        coursePage c user

                    LecturePage lecture ->
                        lectureView lecture

                    RunningQuiz lecture remainingExercises _ ->
                        case List.head remainingExercises of
                            Just exercise ->
                                runningQuizView lecture exercise

                            Nothing ->
                                div [] [ text "Hier gehörst du nicht hin!" ]

                    WinningQuiz lecture ->
                        div
                            [ Html.Attributes.class "container fixed-bottom mb-2" ]
                            [ h4
                                []
                                [ text lecture.title ]
                            , Html.p [] [ text "Herzlichen Glückwunsch! Du hast die Lektion erfolgreich abgeschlossen." ]
                            , button
                                [ onClick (AddBadge lecture.badge)
                                , Html.Attributes.class "btn btn-dark btn-lg w-100 h-100"
                                ]
                                [ text "Zurück zur Kursübersicht" ]
                            ]

                    FinishedQuiz _ answeredExercises i ->
                        let
                            wrongExercises =
                                List.filter (\( _, a ) -> not a.isCorrect) answeredExercises
                        in
                        case wrongExercises of
                            [] ->
                                text "FinishedLecture: Hier stimmt was nicht!"

                            w ->
                                case get i w of
                                    Just ( exercise, answer ) ->
                                        div
                                            [ Html.Attributes.class "container mb-2 fixed-bottom" ]
                                            [ text
                                                ("Du hast "
                                                    ++ String.fromInt (List.length answeredExercises - List.length w)
                                                    ++ " von "
                                                    ++ String.fromInt
                                                        (List.length answeredExercises)
                                                    ++ " Aufgaben richtig gelöst."
                                                )
                                            , finishedExerciseView
                                                exercise
                                                answer
                                            ]

                                    Nothing ->
                                        Debug.log (String.fromInt (List.length wrongExercises))
                                            text
                                            "WARUM IST DAS HIER NULL?!"

                    LearningContentPage lecture i ->
                        div [ Html.Attributes.class "container" ]
                            [ runningLearningContentView lecture i
                            ]

                    Landing _ ->
                        text "Dieser Fall sollte nicht eintreten!"

            Nothing ->
                case model.page of
                    Landing l ->
                        landingPage l

                    _ ->
                        div [] []
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


coursesOverview : List Course -> User -> Html Msg
coursesOverview courses user =
    div [ Html.Attributes.class "m-1" ]
        [ Html.h1
            [ Html.Attributes.class "display-5 text-center" ]
            [ text "Kursübersicht"
            ]
        , Html.p
            [ Html.Attributes.class "text-center" ]
            [ text
                ("Dir stehen " ++ String.fromInt (List.length courses) ++ " Kurse zur Verfügung: ")
            ]
        , div
            [ Html.Attributes.class "album" ]
            [ div
                [ Html.Attributes.class "container" ]
                [ div
                    [ Html.Attributes.class
                        "row"
                    ]
                    (List.map
                        (\course ->
                            div
                                [ Html.Attributes.class "col" ]
                                [ div
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
                                        ]
                                    , let
                                        progress =
                                            List.length
                                                (List.filter (\l -> List.any (\b -> b == l.badge) user.badges) course.lectures)
                                                * 100
                                                // List.length course.lectures
                                      in
                                      div
                                        [ Html.Attributes.class "card-footer"
                                        , Html.Attributes.classList
                                            [ ( "d-none", progress == 0 )
                                            ]
                                        ]
                                        (if progress < 100 then
                                            [ div
                                                [ Html.Attributes.class "progress"
                                                , Html.Attributes.style "height" "2em"
                                                ]
                                                [ div
                                                    [ Html.Attributes.class "progress-bar progress-bar bg-success"
                                                    , Html.Attributes.attribute "role" "progressbar"
                                                    , Html.Attributes.attribute "aria-valuenow" (String.fromInt progress)
                                                    , Html.Attributes.attribute "aria-valuemin" "0"
                                                    , Html.Attributes.attribute "aria-valuemax" "100"
                                                    , Html.Attributes.style "width" (String.fromInt progress ++ "%")
                                                    ]
                                                    []
                                                ]
                                            ]

                                         else
                                            [ div
                                                [ Html.Attributes.class "d-flex justify-content-between align-items-center" ]
                                                [ div [] []
                                                , div
                                                    []
                                                    [ Img.bookmarkCheckSvg
                                                    ]
                                                ]
                                            ]
                                        )
                                    ]
                                ]
                        )
                        courses
                    )
                ]
            ]
        , foot
        ]


coursePage : Course -> User -> Html Msg
coursePage course user =
    div []
        [ h3
            [ Html.Attributes.class "display-5 text-center" ]
            [ text course.title
            ]
        , div
            [ Html.Attributes.class "album" ]
            [ div
                [ Html.Attributes.class "container" ]
                [ div
                    [ Html.Attributes.class
                        "row row-cols-sm-2"
                    ]
                    (List.map
                        (\lecture ->
                            div
                                [ Html.Attributes.class "col-md-4" ]
                                [ div
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
                                                    [ if List.any (\b -> b == lecture.badge) user.badges then
                                                        Img.badgeSvg

                                                      else
                                                        text ""
                                                    , text
                                                        (String.fromInt (List.length lecture.exercises)
                                                            ++ " Aufgaben"
                                                        )
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                        )
                        course.lectures
                    )
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


runningLearningContentView : Lecture -> Int -> Html Msg
runningLearningContentView lecture exampleIndex =
    case get exampleIndex lecture.learningContent.examples of
        Just example ->
            div
                [ Html.Attributes.class "container"
                ]
                [ h4
                    []
                    [ text lecture.learningContent.title
                    ]
                , div
                    []
                    [ text lecture.learningContent.description ]
                , learningExampleView example
                ]

        Nothing ->
            div
                [ Html.Attributes.class "container fixed-bottom mb-2"
                ]
                [ div
                    [ Html.Attributes.class "d-grid gap-2" ]
                    [ button
                        [ Html.Attributes.class "btn btn-lg btn-dark d-block"
                        , onClick StartQuiz
                        ]
                        [ text ("Quiz \"" ++ lecture.title ++ "\" starten") ]
                    ]
                ]


learningExampleView : LearningExample -> Html Msg
learningExampleView example =
    div
        [ Html.Attributes.class "card m-2 fixed-bottom"
        ]
        [ div
            [ Html.Attributes.class "card-header" ]
            [ text example.title ]
        , div
            [ Html.Attributes.class "card-body" ]
            [ div
                [ Html.Attributes.class "card-title" ]
                [ text (Maybe.withDefault "" example.description) ]
            , div
                [ Html.Attributes.class "card-content" ]
                (highlightedExpressionView example.expression Nothing)
            ]
        , div
            [ Html.Attributes.class "card-footer d-flex justify-content-between align-items-center"
            ]
            [ div [] []
            , button
                [ Html.Attributes.class "btn btn-lg btn-secondary"
                , onClick NextExample
                ]
                [ text "Weiter" ]
            ]
        ]


runningQuizView : Lecture -> Exercise -> Html Msg
runningQuizView l e =
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
            SingleExpression singleExpression ->
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

            BinaryExpression binaryExpression ->
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

            FunctionExpression functionExpression ->
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

            GuardExpression guardExpression ->
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

            PatternMatchingExpression patternMatchingExpression ->
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


runningExerciseAnswerView : Exercise -> List Answer -> Html Msg
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


finishedExerciseAnswerView : List Answer -> Answer -> Html Msg
finishedExerciseAnswerView answers studentAnswer =
    div
        []
        (List.map
            (\answer ->
                if answer == studentAnswer then
                    div
                        [ Html.Attributes.class "btn bg-white m-1 pe-none"
                        , Html.Attributes.classList
                            [ ( "btn-outline-success", answer.isCorrect )
                            , ( "btn-outline-danger", not answer.isCorrect )
                            ]
                        ]
                        (highlightedInlineView answer.code)

                else if answer.isCorrect then
                    div
                        [ Html.Attributes.class "btn bg-white m-1 pe-none"
                        , Html.Attributes.classList
                            [ ( "btn-outline-danger", not answer.isCorrect )
                            , ( "btn-outline-success", answer.isCorrect )
                            ]
                        ]
                        (highlightedInlineView answer.code)

                else
                    text ""
            )
            answers
        )


finishedExerciseView : Exercise -> Answer -> Html Msg
finishedExerciseView exercise answer =
    case exercise of
        SingleExpression singleExpressionModel ->
            div
                [ Html.Attributes.class "card m-2" ]
                [ div
                    [ Html.Attributes.class "card-header text-center" ]
                    [ Html.h5
                        []
                        [ text singleExpressionModel.title
                        ]
                    ]
                , div
                    [ Html.Attributes.class "card-body" ]
                    (((case singleExpressionModel.description of
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
                        ++ [ finishedExerciseAnswerView
                                singleExpressionModel.answers
                                answer
                           ]
                    )
                , finishedLectureFooter
                ]

        BinaryExpression binaryExpressionModel ->
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
                    (((case binaryExpressionModel.description of
                        Just d ->
                            div
                                [ Html.Attributes.class "card-text"
                                ]
                                [ text d ]

                        Nothing ->
                            text ""
                      )
                        :: highlightedExpressionView (binaryExpressionModel.leftExpression ++ " " ++ binaryExpressionModel.operator ++ " " ++ binaryExpressionModel.rightExpression) Nothing
                     )
                        ++ [ finishedExerciseAnswerView
                                binaryExpressionModel.answers
                                answer
                           ]
                    )
                , finishedLectureFooter
                ]

        FunctionExpression functionExpressionModel ->
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
                    , finishedLectureFooter
                    ]
                ]

        GuardExpression guardExpressionModel ->
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
                    , finishedLectureFooter
                    ]
                ]

        PatternMatchingExpression patternExpressionModel ->
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
                    , finishedLectureFooter
                    ]
                ]


finishedLectureFooter : Html Msg
finishedLectureFooter =
    div
        [ Html.Attributes.class
            "card-footer d-flex justify-content-between align-items-center"
        ]
        [ button
            [ Html.Attributes.class "btn btn-secondary", onClick Prev ]
            [ text "<" ]
        , button
            [ Html.Attributes.class "btn btn-outline-warning", onClick StartLecture ]
            [ text "Lektion neustarten" ]
        , button
            [ Html.Attributes.class "btn btn-secondary", onClick Next ]
            [ text ">" ]
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
                            [ h5
                                []
                                [ text (Maybe.withDefault "" (Maybe.map .name m.user))
                                ]
                            , Maybe.withDefault (text "")
                                (Maybe.map
                                    (\user ->
                                        if List.length user.badges == 0 then
                                            text ""

                                        else
                                            div
                                                [ Html.Attributes.class "bg-success rounded" ]
                                                [ Html.span
                                                    [ Html.Attributes.class "badge badge-pill" ]
                                                    [ text (String.fromInt (List.length user.badges))
                                                    , Img.badgeSvg
                                                    ]
                                                ]
                                    )
                                    m.user
                                )
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
    | ArrowLeft
    | ArrowRight
    | Other


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Key
toKey key =
    case key of
        "Enter" ->
            Enter

        "ArrowLeft" ->
            ArrowLeft

        "ArrowRight" ->
            ArrowRight

        _ ->
            Other


get : Int -> List a -> Maybe a
get n xs =
    List.head (List.drop n xs)



-- EXAMPLES
--


course1 : Course
course1 =
    { id = 1
    , title = "Ausdrücke"
    , description = "Lerne etwas zu Ausdrücken in Haskell"
    , lectures = [ lecture1, lecture2, lecture3, lecture4, lecture5 ]
    }


lecture2 : Lecture
lecture2 =
    { id = 2
    , title = "Typen von zweistelligen Ausdrücken"
    , description = "Diese Lektion beinhaltet Aufgaben mit zweistelligen Ausdrücken, die über einfache Operatoren miteinander verbunden sind."
    , badge =
        { id = "binaryexpression"
        , name = "Zweistellige Ausdrücke"
        , image = div [] []
        }
    , learningContent =
        { id = 1
        , title = "Zweistellige Ausdrücke"
        , description = "In Haskell gibt es viele Operatoren, die über zwei Ausdrücke gelegt werden können. Diese Operatoren sind z.B. +, -, *, /, ++, &&, || und viele mehr. In dieser Lektion wirst du sehen, wie diese Operatoren in Haskell aussehen."
        , examples = []
        }
    , exercises =
        [ BinaryExpression
            { id = 8
            , title = "Zahlenausdruck"
            , description = Just "Welchen Typ hat der folgende Ausdruck?"
            , answers =
                [ { code = "Int"
                  , isCorrect = True
                  }
                , { code = "String"
                  , isCorrect = False
                  }
                , { code = "Float"
                  , isCorrect = False
                  }
                , { code = "SomeType"
                  , isCorrect = False
                  }
                ]
            , leftExpression = "1"
            , rightExpression = "2"
            , operator = "+"
            }
        , BinaryExpression
            { id = 9
            , title = "Stringausdruck"
            , description = Just "Welchen Typ hat der folgende Ausdruck?"
            , answers =
                [ { code = "Int"
                  , isCorrect = False
                  }
                , { code = "String"
                  , isCorrect = True
                  }
                , { code = "Float"
                  , isCorrect = False
                  }
                , { code = "SomeType"
                  , isCorrect = False
                  }
                ]
            , leftExpression = "\"Hallo\""
            , rightExpression = "\"Welt\""
            , operator = "++"
            }
        , BinaryExpression
            { id = 10
            , title = "Boolausdruck"
            , description = Just "Welchen Typ hat der folgende Ausdruck?"
            , answers =
                [ { code = "Int"
                  , isCorrect = False
                  }
                , { code = "String"
                  , isCorrect = False
                  }
                , { code = "Float"
                  , isCorrect = False
                  }
                , { code = "Bool"
                  , isCorrect = True
                  }
                ]
            , leftExpression = "True"
            , rightExpression = "False"
            , operator = "&&"
            }
        , BinaryExpression
            { id = 11
            , title = "Listenausdruck"
            , description = Just "Welchen Typ hat der folgende Ausdruck?"
            , answers =
                [ { code = "[Int]"
                  , isCorrect = True
                  }
                , { code = "String"
                  , isCorrect = False
                  }
                , { code = "Float"
                  , isCorrect = False
                  }
                , { code = "SomeType"
                  , isCorrect = False
                  }
                ]
            , leftExpression = "[1, 2]"
            , rightExpression = "[3, 4]"
            , operator = "++"
            }
        , BinaryExpression
            { id = 12
            , title = "Listenausdruck"
            , description = Just "Welchen Typ hat der folgende Ausdruck?"
            , answers =
                [ { code = "[(1, String)]"
                  , isCorrect = True
                  }
                , { code = "(1, String)"
                  , isCorrect = False
                  }
                , { code = "Float"
                  , isCorrect = False
                  }
                , { code = "SomeType"
                  , isCorrect = False
                  }
                ]
            , leftExpression = "[(1, \"Joscha\"), (4, \"Test\")]"
            , rightExpression = "[(5, \"Noch\"), (2, \"Konstantin\")]"
            , operator = "++"
            }
        ]
    }


lecture3 : Lecture
lecture3 =
    { id = 3
    , title = "Funktionen"
    , description = "In dieser Lektion wird dein Wissen über Funktionen getestet."
    , badge =
        { id = ""
        , name = "Funktionen"
        , image = div [] []
        }
    , learningContent =
        { id = 1
        , title = "Funktionen in Haskell"
        , description = "Funktionen sind ein zentraler Bestandteil von Haskell. Sie ermöglichen es, Eingaben zu verarbeiten und Ausgaben zu erzeugen. Funktionen können mehrere Argumente haben und verschiedene Typen zurückgeben. In dieser Lektion lernst du, wie Funktionen in Haskell definiert und verwendet werden."
        , examples =
            [ { id = 1
              , title = "Einfache Funktion"
              , expression = "add :: Int -> Int -> Int\nadd x y = x + y"
              , description = Just "Diese Funktion nimmt zwei Ganzzahlen als Eingabe und gibt ihre Summe zurück."
              }
            ]
        }
    , exercises =
        [ FunctionExpression
            { id = 13
            , title = "Funktion"
            , description = Just "Welchen Typ hat die folgende Funtkion?"
            , functionName = "add"
            , arguments = [ "x", "y" ]
            , answers =
                [ { code = "add :: Int -> Int -> Int"
                  , isCorrect = True
                  }
                , { code = "add :: Int -> Int -> String"
                  , isCorrect = True
                  }
                , { code = "add :: Float"
                  , isCorrect = False
                  }
                , { code = "Int"
                  , isCorrect = False
                  }
                ]
            }
        ]
    }


lecture4 : Lecture
lecture4 =
    { id = 4
    , title = "Guards"
    , description = "In dieser Lektion wird dein Wissen über Guards getestet."
    , badge =
        { id = "guards"
        , name = "Guards"
        , image = div [] []
        }
    , learningContent =
        { id = 1
        , title = "Guards in Haskell"
        , description = "Guards sind eine Möglichkeit, Bedingungen in Haskell übersichtlich und lesbar zu gestalten. Sie ermöglichen es, verschiedene Fälle einer Funktion durch Bedingungen zu unterscheiden. Guards werden mit einem senkrechten Strich (|) eingeleitet und können mehrere Bedingungen enthalten, die nacheinander geprüft werden."
        , examples =
            [ { id = 1
              , title = "Einfacher Guard-Ausdruck"
              , expression = "absolute :: Int -> Int\nabsolute x\n    | x >= 0 = x\n    | otherwise = -x"
              , description = Just "Diese Funktion berechnet den absoluten Wert einer Zahl. Wenn die Zahl größer oder gleich 0 ist, wird sie direkt zurückgegeben. Andernfalls wird ihr negatives Pendant zurückgegeben."
              }
            ]
        }
    , exercises =
        [ GuardExpression
            { id = 14
            , title = "Guard-Ausdruck"
            , description = Just "Welchen Typ hat der folgende Guard-Ausdruck?"
            , functionName = "guardFunction"
            , arguments = [ "x" ]
            , expression =
                "\n\t | x > 0 = \"größer 0\""
                    ++ "\n\t | x < 0 = \"kleiner 0\""
                    ++ "\n\t | otherwise = \"gleich 0\""
            , answers =
                [ { code = "guardFunction :: Int -> String"
                  , isCorrect = True
                  }
                , { code = "guardFunction :: String"
                  , isCorrect = False
                  }
                , { code = "guardFunction :: Float"
                  , isCorrect = False
                  }
                , { code = "guardFunction :: SomeType"
                  , isCorrect = False
                  }
                ]
            }
        ]
    }


lecture5 : Lecture
lecture5 =
    { id = 5
    , title = "Pattern Matching"
    , description = "In dieser Lektion wird dein Wissen über Pattern Matching getestet."
    , badge =
        { id = "pattern-matching"
        , name = "Pattern Matching"
        , image = div [] []
        }
    , learningContent =
        { id = 1
        , title = "Pattern Matching in Haskell"
        , description = "Pattern Matching ist eine leistungsstarke Funktion in Haskell, die es ermöglicht, Werte anhand ihrer Struktur zu zerlegen und zu analysieren. Es wird häufig in Funktionen verwendet, um verschiedene Fälle zu behandeln."
        , examples =
            [ { id = 1
              , title = "Einfaches Pattern Matching"
              , expression = "f :: Int -> String\nf 0 = \"Null\"\nf 1 = \"Eins\"\nf _ = \"Andere Zahl\""
              , description = Just "In diesem Beispiel wird die Funktion `f` definiert, die eine Ganzzahl als Eingabe nimmt und einen String zurückgibt. Für die Eingabe `0` gibt sie \"Null\" zurück, für `1` \"Eins\" und für alle anderen Werte \"Andere Zahl\"."
              }
            ]
        }
    , exercises =
        [ PatternMatchingExpression
            { id = 15
            , title = "Pattern Matching"
            , description = Just "Welchen Typ hat der folgende Pattern Matching Ausdruck?"
            , patterns = [ "f _ 0 = 0", "f 1 _ = 1", "f x y = x + y" ]
            , answers =
                [ { code = "f :: Int -> Int -> Int"
                  , isCorrect = True
                  }
                , { code = "f :: Int -> String -> Int"
                  , isCorrect = False
                  }
                , { code = "f :: Int -> Int -> String"
                  , isCorrect = False
                  }
                , { code = "f :: Int -> Int -> Float"
                  , isCorrect = False
                  }
                ]
            }
        ]
    }


lecture1 : Lecture
lecture1 =
    { id = 1
    , title = "Typen von einfachen Ausdrücken"
    , description = "In dieser Lektion wird dein Wissen über Typen von einfachen Ausdrücken getestet."
    , badge =
        { id = "singleexpression"
        , name = "Single Expression"
        , image = div [] []
        }
    , learningContent =
        { id = 1
        , title = "Einfache Ausdrücke bzw. Typen"
        , description = "In Haskell ist alles ein Ausdruck. Um Wikipedia zu zitieren: \"Ein Ausdruck ist in vielen Programmiersprachen ein Konstrukt, das gemäß einer gegebenen Semantik in Bezug auf einen Kontext ausgewertet werden kann, also einen Wert liefert. \". Einfache Ausdrücke sind Ausdrücke, die nur einen Wert haben. Das bedeutet, dass sie keine Funktionen oder Operatoren enthalten. Einfache Ausdrücke sind z.B. Zahlen, Strings, Listen und Tupel. Im folgenden wirst du sehen, wie einfache Ausdrück in Haskell aussehen können. Das \"x\" ist in jedem Beispiel eine Konstante, hinter der ein Wert steckt. Der Typ des Wertes wird durch den Typ des Ausdrucks bestimmt. Das bedeutet, dass der Typ des Ausdrucks immer dem Typ der Variable entspricht. Wenn du also den Ausdruck \"x = 1\" hast, dann ist der Typ von \"x\" immer \"Int\". Wenn du den Ausdruck \"x = 1.0\" hast, dann ist der Typ von \"x\" immer \"Float\"."
        , examples =
            [ { id = 1
              , title = "Ganzzahliger Ausdruck"
              , expression = "x :: Int\nx = 1"
              , description = Nothing
              }
            , { id = 2
              , title = "Gleitkommazahl-Ausdruck"
              , expression = "x :: Float\nx = 1.0"
              , description = Nothing
              }
            , { id = 3
              , title = "String-Ausdruck"
              , expression = "x :: String\nx = \"Hallo\""
              , description = Nothing
              }
            , { id = 4
              , title = "Gleitkommazahl-Ausdruck"
              , expression = "x :: Float\nx = 1.0"
              , description = Nothing
              }
            , { id = 5
              , title = "Listen-Ausdruck"
              , expression = "x :: [String]\nx = [\"Hallo\", \"Welt\"]"
              , description = Nothing
              }
            , { id = 6
              , title = "Boolescher Ausdruck"
              , expression = "x :: Bool\nx = True"
              , description = Just "Ein boolescher Ausdruck kann entweder 'True' oder 'False' sein. In diesem Beispiel ist der Wert 'True'."
              }
            ]
        }
    , exercises =
        [ exercise1
        , exercise2
        , exercise3
        , exercise4
        , exercise5
        , exercise6
        , exercise7
        ]
    }


exercise1 : Exercise
exercise1 =
    SingleExpression
        { id = 1
        , title = "Zahlenausdruck"
        , description = Just "Welchen Typ hat der folgende Ausdruck?"
        , expression = "1"
        , answers =
            [ { code = "Int"
              , isCorrect = True
              }
            , { code = "String"
              , isCorrect = False
              }
            , { code = "Float"
              , isCorrect = False
              }
            , { code = "SomeType"
              , isCorrect = False
              }
            ]
        }


exercise2 : Exercise
exercise2 =
    SingleExpression
        { id = 2
        , title = "Stringausdruck"
        , description = Just "Welchen Typ hat der folgende Ausdruck?"
        , expression = "\"Hallo\""
        , answers =
            [ { code = "Int"
              , isCorrect = False
              }
            , { code = "String"
              , isCorrect = True
              }
            , { code = "Float"
              , isCorrect = False
              }
            , { code = "SomeType"
              , isCorrect = False
              }
            ]
        }


exercise3 : Exercise
exercise3 =
    SingleExpression
        { id = 3
        , title = "Boolausdruck"
        , description = Just "Welchen Typ hat der folgende Ausdruck?"
        , expression = "True"
        , answers =
            [ { code = "Int"
              , isCorrect = False
              }
            , { code = "String"
              , isCorrect = False
              }
            , { code = "Float"
              , isCorrect = False
              }
            , { code = "Bool"
              , isCorrect = True
              }
            ]
        }


exercise4 : Exercise
exercise4 =
    SingleExpression
        { id = 4
        , title = "Listenausdruck"
        , description = Just "Welchen Typ hat der folgende Ausdruck?"
        , expression = "[1, 2, 3]"
        , answers =
            [ { code = "[Int]"
              , isCorrect = True
              }
            , { code = "String"
              , isCorrect = False
              }
            , { code = "Float"
              , isCorrect = False
              }
            , { code = "SomeType"
              , isCorrect = False
              }
            ]
        }


exercise5 : Exercise
exercise5 =
    SingleExpression
        { id = 5
        , title = "Tupleausdruck"
        , description = Just "Welchen Typ hat der folgende Ausdruck?"
        , expression = "(1, \"Hallo\")"
        , answers =
            [ { code = "(Int, String)"
              , isCorrect = True
              }
            , { code = "String"
              , isCorrect = False
              }
            , { code = "Float"
              , isCorrect = False
              }
            , { code = "SomeType"
              , isCorrect = False
              }
            ]
        }


exercise6 : Exercise
exercise6 =
    SingleExpression
        { id = 6
        , title = "Maybeausdruck"
        , description = Just "Welchen Typ hat der folgende Ausdruck?"
        , expression = "Just 1"
        , answers =
            [ { code = "Maybe Int"
              , isCorrect = True
              }
            , { code = "String"
              , isCorrect = False
              }
            , { code = "Float"
              , isCorrect = False
              }
            , { code = "SomeType"
              , isCorrect = False
              }
            ]
        }


exercise7 : Exercise
exercise7 =
    SingleExpression
        { id = 7
        , title = "Maybeausdruck"
        , description = Just "Welchen Typ hat der folgende Ausdruck?"
        , expression = "Nothing"
        , answers =
            [ { code = "Maybe a"
              , isCorrect = True
              }
            , { code = "String"
              , isCorrect = False
              }
            , { code = "Float"
              , isCorrect = False
              }
            , { code = "SomeType"
              , isCorrect = False
              }
            ]
        }
