module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, a, button, div, h4, h5, nav, text)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Images.Images as Img
import Json.Decode as Decode
import Random
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
    , image : Maybe (String -> Html Msg)
    }


type alias User =
    { name : String
    , badges : List Badge
    }


type Error
    = UsernameIncorrect


type Model
    = Landing (Maybe String) (Maybe Error)
    | CoursesOverview User (List Course)
    | CoursePage User Course
    | LecturePage User Course Lecture
    | LearningContentPage User Course Lecture Int
    | RunningQuiz User Course Lecture (List Exercise) (List ( Exercise, Answer ))
    | FinishedQuiz User Course Lecture (List ( Exercise, Answer )) Int
    | WinningQuiz User Course Lecture


type Msg
    = EnteringName String
    | EnteringNameDone
    | SelectCourse Course
    | SelectLecture Lecture
    | StartLecture
    | ShuffleExercises
    | ShuffleAnswers (List Answer)
    | StartQuiz (List Exercise)
    | SelectAnswer Exercise Answer
    | GoToCoursesOverview
    | Next
    | Prev
    | AddBadge Badge
    | NoOp


init : () -> ( Model, Cmd Msg )
init _ =
    ( Landing Nothing Nothing, Cmd.none )



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

        GoToCoursesOverview ->
            case model of
                CoursesOverview user _ ->
                    ( CoursesOverview user coursesExamples, Cmd.none )

                CoursePage user _ ->
                    ( CoursesOverview user coursesExamples, Cmd.none )

                LecturePage user _ _ ->
                    ( CoursesOverview user coursesExamples, Cmd.none )

                LearningContentPage user _ _ _ ->
                    ( CoursesOverview user coursesExamples, Cmd.none )

                RunningQuiz user _ _ _ _ ->
                    ( CoursesOverview user coursesExamples, Cmd.none )

                FinishedQuiz user _ _ _ _ ->
                    ( CoursesOverview user coursesExamples, Cmd.none )

                WinningQuiz user _ _ ->
                    ( CoursesOverview user coursesExamples, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EnteringName name ->
            case model of
                Landing _ _ ->
                    ( Landing
                        (Just name)
                        (if xor (checkUsername (Just name)) (String.length name == 0) then
                            Nothing

                         else
                            Just UsernameIncorrect
                        )
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EnteringNameDone ->
            case model of
                Landing maybeUsername maybeError ->
                    case maybeError of
                        Just _ ->
                            ( model, Cmd.none )

                        Nothing ->
                            case maybeUsername of
                                Just name ->
                                    ( CoursesOverview (User name []) coursesExamples, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectCourse course ->
            case model of
                CoursesOverview user _ ->
                    ( CoursePage user course, Cmd.none )

                LecturePage user _ _ ->
                    ( CoursePage user course, Cmd.none )

                LearningContentPage user _ _ _ ->
                    ( CoursePage user course, Cmd.none )

                RunningQuiz user _ _ _ _ ->
                    ( CoursePage user course, Cmd.none )

                FinishedQuiz user _ _ _ _ ->
                    ( CoursePage user course, Cmd.none )

                WinningQuiz user _ _ ->
                    ( CoursePage user course, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectLecture lecture ->
            case model of
                CoursePage user course ->
                    ( LecturePage user course lecture, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartLecture ->
            case model of
                LecturePage user course lecture ->
                    ( LearningContentPage user course lecture 0, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectAnswer exercise answer ->
            case model of
                RunningQuiz user course lecture remainingExercises answeredExercises ->
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
                            ( WinningQuiz user course lecture, Cmd.none )

                        else
                            let
                                wrongExercises =
                                    List.filter (\( _, a ) -> not a.isCorrect) newAnswers
                            in
                            ( FinishedQuiz user course lecture wrongExercises 0, Cmd.none )

                    else
                        ( RunningQuiz user course lecture newRemainingExercises newAnswers
                        , case List.head newRemainingExercises of
                            Just h ->
                                Random.generate
                                    ShuffleAnswers
                                    (shuffle
                                        (case h of
                                            SingleExpression m ->
                                                m.answers

                                            BinaryExpression m ->
                                                m.answers

                                            FunctionExpression m ->
                                                m.answers

                                            GuardExpression m ->
                                                m.answers

                                            PatternMatchingExpression m ->
                                                m.answers
                                        )
                                    )

                            Nothing ->
                                Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        Next ->
            case model of
                LecturePage user course lecture ->
                    ( LearningContentPage user course lecture 0, Cmd.none )

                FinishedQuiz user course lecture wrongExercises i ->
                    let
                        nextIndex =
                            min (List.length wrongExercises - 1) (i + 1)
                    in
                    ( FinishedQuiz user course lecture wrongExercises nextIndex, Cmd.none )

                LearningContentPage user course lecture i ->
                    let
                        nextIndex =
                            min (List.length lecture.learningContent.examples) (i + 1)
                    in
                    if nextIndex >= List.length lecture.learningContent.examples then
                        ( RunningQuiz user course lecture lecture.exercises [], Cmd.none )

                    else
                        ( LearningContentPage user course lecture nextIndex, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Prev ->
            case model of
                LecturePage user course _ ->
                    ( CoursePage user course, Cmd.none )

                FinishedQuiz user course lecture wrongExercises i ->
                    ( FinishedQuiz user course lecture wrongExercises (max 0 (i - 1)), Cmd.none )

                LearningContentPage user course lecture i ->
                    if i <= 0 then
                        ( CoursePage user course, Cmd.none )

                    else
                        ( LearningContentPage user course lecture (max 0 (i - 1)), Cmd.none )

                CoursePage user _ ->
                    ( CoursesOverview user coursesExamples, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AddBadge badge ->
            case model of
                WinningQuiz user course _ ->
                    ( CoursePage
                        { user
                            | badges =
                                if List.any (\b -> b == badge) user.badges then
                                    user.badges

                                else
                                    badge :: user.badges
                        }
                        course
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ShuffleExercises ->
            case model of
                LearningContentPage _ _ lecture _ ->
                    ( model, Random.generate StartQuiz (shuffle lecture.exercises) )

                FinishedQuiz _ _ lecture _ _ ->
                    ( model, Random.generate StartQuiz (shuffle lecture.exercises) )

                _ ->
                    ( model, Cmd.none )

        StartQuiz shuffledExercises ->
            let
                command =
                    case List.head shuffledExercises of
                        Just h ->
                            Random.generate
                                ShuffleAnswers
                                (shuffle
                                    (case h of
                                        SingleExpression m ->
                                            m.answers

                                        BinaryExpression m ->
                                            m.answers

                                        FunctionExpression m ->
                                            m.answers

                                        GuardExpression m ->
                                            m.answers

                                        PatternMatchingExpression m ->
                                            m.answers
                                    )
                                )

                        Nothing ->
                            Cmd.none
            in
            case model of
                LearningContentPage user course lecture _ ->
                    ( RunningQuiz user course lecture shuffledExercises [], command )

                FinishedQuiz user course lecture _ _ ->
                    ( RunningQuiz user course lecture shuffledExercises [], command )

                _ ->
                    ( model, Cmd.none )

        ShuffleAnswers shuffled ->
            case model of
                RunningQuiz user course lecture exercises studentAnswers ->
                    ( RunningQuiz
                        user
                        course
                        lecture
                        (case List.head exercises of
                            Just h ->
                                (case h of
                                    SingleExpression m ->
                                        SingleExpression
                                            { m
                                                | answers = shuffled
                                            }

                                    BinaryExpression m ->
                                        BinaryExpression
                                            { m
                                                | answers = shuffled
                                            }

                                    FunctionExpression m ->
                                        FunctionExpression
                                            { m
                                                | answers = shuffled
                                            }

                                    GuardExpression m ->
                                        GuardExpression
                                            { m
                                                | answers = shuffled
                                            }

                                    PatternMatchingExpression m ->
                                        PatternMatchingExpression
                                            { m
                                                | answers = shuffled
                                            }
                                )
                                    :: (case List.tail exercises of
                                            Just tail ->
                                                tail

                                            Nothing ->
                                                []
                                       )

                            Nothing ->
                                []
                        )
                        studentAnswers
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.class "d-flex flex-column min-vh-100" ]
        (case model of
            CoursesOverview user courses ->
                [ header (Just user) Nothing
                , coursesOverview user courses
                ]

            CoursePage user course ->
                [ header (Just user) (Just course)
                , coursePage user course
                ]

            LecturePage user course lecture ->
                [ header (Just user) (Just course)
                , lectureView lecture
                ]

            RunningQuiz user course lecture remainingExercises _ ->
                [ header (Just user) (Just course)
                , Html.main_
                    [ Html.Attributes.class "m-2" ]
                    [ h4 []
                        [ text lecture.title
                        ]
                    ]
                , case List.head remainingExercises of
                    Just e ->
                        Html.footer
                            [ Html.Attributes.class "mt-auto m-2" ]
                            [ exerciseView e
                            , if List.length lecture.exercises == 1 then
                                text ""

                              else
                                let
                                    l =
                                        List.length lecture.exercises

                                    progress =
                                        (l - List.length remainingExercises + 1)
                                            * 100
                                            // l
                                in
                                div
                                    [ Html.Attributes.class "mt-1" ]
                                    [ progressBarView progress "1em" ]
                            ]

                    Nothing ->
                        text ""
                ]

            WinningQuiz user course lecture ->
                [ header (Just user) (Just course)
                , Html.main_
                    [ Html.Attributes.class "m-2" ]
                    [ h4
                        []
                        [ text lecture.title ]
                    ]
                , Html.footer
                    [ Html.Attributes.class "footer mt-auto m-2" ]
                    [ Html.p
                        []
                        [ text ("Herzlichen Glückwunsch, " ++ user.name ++ "! Du hast die Lektion erfolgreich abgeschlossen.") ]
                    , button
                        [ Html.Attributes.class "btn btn-lg text-white w-100"
                        , Html.Attributes.style "background-color" "#6f42c1"
                        , onClick (AddBadge lecture.badge)
                        ]
                        [ text "Belohnung einsacken"
                        , Maybe.withDefault Img.genericBadgeSvg lecture.badge.image "3em"
                        ]
                    ]
                ]

            FinishedQuiz user course lecture wrongExercises i ->
                [ header (Just user) (Just course)
                , case wrongExercises of
                    [] ->
                        text "FinishedLecture: Hier stimmt was nicht!"

                    w ->
                        case get i w of
                            Just ( exercise, answer ) ->
                                Html.footer
                                    [ Html.Attributes.class "footer m-2 mt-auto" ]
                                    [ div
                                        []
                                        [ text
                                            ("Du hast "
                                                ++ String.fromInt (List.length lecture.exercises - List.length wrongExercises)
                                                ++ " von "
                                                ++ String.fromInt (List.length lecture.exercises)
                                                ++ " Aufgaben richtig gelöst."
                                            )
                                        ]
                                    , finishedExerciseView exercise answer
                                    ]

                            Nothing ->
                                text ""
                ]

            LearningContentPage user course lecture i ->
                [ header (Just user) (Just course)
                , Html.main_
                    [ Html.Attributes.class "container-fluid" ]
                    [ h4
                        []
                        [ text lecture.learningContent.title
                        ]
                    ]
                , runningLearningContentView lecture i
                ]

            Landing user error ->
                [ header Nothing Nothing
                , Html.main_
                    [ Html.Attributes.class "main fill-height"
                    ]
                    []
                , Html.footer
                    [ Html.Attributes.class "footer mt-auto m-2" ]
                    [ div
                        [ Html.Attributes.class "alert bg-danger-subtle"
                        , Html.Attributes.hidden
                            (case error of
                                Just err ->
                                    case err of
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
                            , Html.Attributes.autofocus True
                            ]
                            []
                        ]
                    , button
                        [ onClick EnteringNameDone
                        , Html.Attributes.class "btn btn-lg w-100 text-white"
                        , Html.Attributes.style "background-color" "#6f42c1"
                        , Html.Attributes.disabled (not (checkUsername user))
                        ]
                        [ text "Start" ]
                    ]
                ]
        )


coursesOverview : User -> List Course -> Html Msg
coursesOverview user courses =
    div
        [ Html.Attributes.class "m-1" ]
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
                    [ Html.Attributes.class "row row-cols-1 row-cols-sm-2 row-cols-md-2 g-3"
                    ]
                    (List.map
                        (\course ->
                            div
                                [ Html.Attributes.class "col-md-6" ]
                                [ div
                                    [ Html.Attributes.class "card shadow-sm"
                                    , onClick (SelectCourse course)
                                    , Html.Attributes.style "cursor" "pointer"
                                    ]
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
                                        [ if progress < 100 then
                                            progressBarView progress "2em"

                                          else
                                            div
                                                [ Html.Attributes.class "d-flex justify-content-between align-items-center" ]
                                                [ div [] []
                                                , div
                                                    []
                                                    [ Img.checkSvg
                                                    ]
                                                ]
                                        ]
                                    ]
                                ]
                        )
                        courses
                    )
                ]
            ]
        ]


progressBarView : Int -> String -> Html Msg
progressBarView progress height =
    div
        [ Html.Attributes.class "progress"
        , Html.Attributes.style "height" height
        ]
        [ div
            [ Html.Attributes.class "progress-bar progress-bar"
            , Html.Attributes.style "background-color" "#6f42c1"
            , Html.Attributes.attribute "role" "progressbar"
            , Html.Attributes.attribute "aria-valuenow" (String.fromInt progress)
            , Html.Attributes.attribute "aria-valuemin" "0"
            , Html.Attributes.attribute "aria-valuemax" "100"
            , Html.Attributes.style "width" (String.fromInt progress ++ "%")
            ]
            []
        ]


coursePage : User -> Course -> Html Msg
coursePage user course =
    div [ Html.Attributes.class "m-1" ]
        [ h4
            [ Html.Attributes.class "display-5 text-center" ]
            [ text course.title
            ]
        , div
            [ Html.Attributes.class "album" ]
            [ div
                [ Html.Attributes.class "container" ]
                [ div
                    [ Html.Attributes.class "row row-cols-1 row-cols-sm-2 row-cols-md-2 g-3"
                    ]
                    (List.map
                        (\lecture ->
                            div
                                [ Html.Attributes.class "col-md-6" ]
                                [ div
                                    [ Html.Attributes.class "card shadow-sm h-100"
                                    , onClick (SelectLecture lecture)
                                    , Html.Attributes.style "cursor" "pointer"
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
                                            ]
                                        ]
                                    , div
                                        [ Html.Attributes.class "card-footer" ]
                                        [ div
                                            [ Html.Attributes.class "d-flex justify-content-between align-items-center" ]
                                            [ Html.small
                                                [ Html.Attributes.class "muted" ]
                                                []
                                            , Html.small
                                                [ Html.Attributes.class "muted" ]
                                                [ if List.any (\b -> b == lecture.badge) user.badges then
                                                    Maybe.withDefault Img.genericBadgeSvg lecture.badge.image "2em"

                                                  else
                                                    text
                                                        (let
                                                            l =
                                                                List.length lecture.exercises
                                                         in
                                                         (String.fromInt (List.length lecture.learningContent.examples)
                                                            ++ " Beispiele | "
                                                         )
                                                            ++ (String.fromInt l
                                                                    ++ (if l == 1 then
                                                                            " Aufgabe"

                                                                        else
                                                                            " Aufgaben"
                                                                       )
                                                               )
                                                        )
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                        )
                        (List.sortBy
                            (\l ->
                                if List.any (\b -> b == l.badge) user.badges then
                                    1

                                else
                                    0
                            )
                            course.lectures
                        )
                    )
                ]
            ]
        ]


lectureView : Lecture -> Html Msg
lectureView l =
    div [ Html.Attributes.class "fixed-bottom m-2" ]
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
            [ Html.Attributes.class "btn btn-lg text-white w-100"
            , Html.Attributes.style "background-color" "#6f42c1"
            , onClick StartLecture
            ]
            [ text "Lektion starten"
            ]
        ]


runningLearningContentView : Lecture -> Int -> Html Msg
runningLearningContentView lecture exampleIndex =
    case get exampleIndex lecture.learningContent.examples of
        Just example ->
            Html.footer
                [ Html.Attributes.class "mt-auto m-2" ]
                [ div
                    [ Html.Attributes.class "card"
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
                        [ case List.head lecture.learningContent.examples of
                            Just head ->
                                if head == example then
                                    div [] []

                                else
                                    button
                                        [ Html.Attributes.class "btn btn-lg text-white"
                                        , Html.Attributes.style "background-color" "#6f42c1"
                                        , onClick Prev
                                        ]
                                        [ text "<<"
                                        ]

                            Nothing ->
                                text ""
                        , let
                            lastIndex =
                                List.length lecture.learningContent.examples - 1
                          in
                          case get lastIndex lecture.learningContent.examples of
                            Just last ->
                                if last == example then
                                    button
                                        [ Html.Attributes.class "btn btn-lg text-white"
                                        , Html.Attributes.style "background-color" "#6f42c1"
                                        , onClick ShuffleExercises
                                        ]
                                        [ text "Quiz starten"
                                        ]

                                else
                                    button
                                        [ Html.Attributes.class "btn btn-lg text-white"
                                        , Html.Attributes.style "background-color" "#6f42c1"
                                        , onClick Next
                                        ]
                                        [ text ">>"
                                        ]

                            Nothing ->
                                text ""
                        ]
                    ]
                , let
                    progress =
                        (exampleIndex + 1)
                            * 100
                            // List.length lecture.learningContent.examples
                  in
                  div
                    [ Html.Attributes.class "mt-1" ]
                    [ progressBarView progress "1em" ]
                ]

        -- für den Fall, dass es keine Beispiele gibt
        Nothing ->
            Html.footer
                [ Html.Attributes.class "mt-auto m-2"
                ]
                [ button
                    [ Html.Attributes.class "btn btn-lg text-white w-100"
                    , Html.Attributes.style "background-color" "#6f42c1"
                    , onClick ShuffleExercises
                    ]
                    [ text ("Quiz \"" ++ lecture.title ++ "\" starten")
                    ]
                ]


exerciseView : Exercise -> Html Msg
exerciseView exercise =
    div
        [ Html.Attributes.class "card" ]
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
        [ Html.Attributes.class "card-footer btn-toolbar d-flex gap-2" ]
        (List.map
            (\answer ->
                div
                    [ Html.Attributes.class "btn btn-lg bg-white btn-outline-dark w-100"
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
        [ Html.Attributes.class "btn-toolbar d-flex gap-2" ]
        (List.map
            (\answer ->
                if answer == studentAnswer then
                    div
                        [ Html.Attributes.class "btn btn-lg bg-white m-1 pe-none w-100"
                        , Html.Attributes.classList
                            [ ( "btn-outline-success", answer.isCorrect )
                            , ( "btn-outline-danger", not answer.isCorrect )
                            ]
                        ]
                        (highlightedInlineView answer.code)

                else if answer.isCorrect then
                    div
                        [ Html.Attributes.class "btn btn-lg bg-white m-1 pe-none w-100"
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
                [ Html.Attributes.class "card" ]
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
                [ Html.Attributes.class "card" ]
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
                    [ Html.Attributes.class "card" ]
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
                    [ Html.Attributes.class "card" ]
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
                    [ Html.Attributes.class "card" ]
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
            [ Html.Attributes.class "btn btn-lg text-white"
            , Html.Attributes.style "background-color" "#6f42c1"
            , onClick Prev
            ]
            [ text "<"
            ]
        , button
            [ Html.Attributes.class "btn btn-lg btn-outline-warning", onClick ShuffleExercises ]
            [ text "Quiz wiederholen" ]
        , button
            [ Html.Attributes.class "btn btn-lg text-white"
            , Html.Attributes.style "background-color" "#6f42c1"
            , onClick Next
            ]
            [ text ">"
            ]
        ]


header : Maybe User -> Maybe Course -> Html Msg
header user course =
    Html.header
        [ Html.Attributes.class "sticky-top" ]
        [ nav
            [ Html.Attributes.class "navbar navbar-expand-sm bg-body-tertiary" ]
            [ div
                [ Html.Attributes.class "container-fluid" ]
                (a
                    [ Html.Attributes.class "navbar-brand" ]
                    [ Img.logo
                    ]
                    :: (case user of
                            Just u ->
                                [ button
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
                                    [ Html.Attributes.class "collapse navbar-collapse justify-content-between"
                                    , Html.Attributes.id "navbarNav"
                                    ]
                                    [ Html.ul
                                        [ Html.Attributes.class "navbar-nav mr-auto border-right pr-3" ]
                                        [ Html.li
                                            [ Html.Attributes.class "nav-item"
                                            , Html.Attributes.style "cursor" "pointer"
                                            ]
                                            [ a
                                                [ Html.Attributes.class "nav-link"
                                                , onClick GoToCoursesOverview
                                                ]
                                                [ text "Kursübersicht"
                                                ]
                                            ]
                                        , case course of
                                            Just c ->
                                                Html.li
                                                    [ Html.Attributes.class "nav-item"
                                                    , Html.Attributes.style "cursor" "pointer"
                                                    ]
                                                    [ a
                                                        [ Html.Attributes.class "nav-link"
                                                        , Html.Attributes.classList
                                                            [ ( "nav-link", True )
                                                            , ( "active", True )
                                                            ]
                                                        , onClick (SelectCourse c)
                                                        ]
                                                        [ text c.title
                                                        ]
                                                    ]

                                            Nothing ->
                                                text ""
                                        ]
                                    , div
                                        []
                                        [ div
                                            [ Html.Attributes.class "btn-lg rounded text-white p-1"
                                            , Html.Attributes.style "background-color" "#6f42c1"
                                            ]
                                            [ text u.name
                                            , case u.badges of
                                                [] ->
                                                    text ""

                                                badges ->
                                                    Html.span
                                                        [ Html.Attributes.class "badge badge-pill" ]
                                                        [ text (String.fromInt (List.length badges))
                                                        , Img.genericBadgeSvg "2em"
                                                        ]
                                            ]
                                        ]
                                    ]
                                ]

                            Nothing ->
                                []
                       )
                )
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



-- Helper to sequence a list of Random.Generator into a Generator of list


sequence : List (Random.Generator a) -> Random.Generator (List a)
sequence gens =
    List.foldr
        (\gen acc ->
            Random.map2 (::) gen acc
        )
        (Random.constant [])
        gens



-- Shuffle function


shuffle : List a -> Random.Generator (List a)
shuffle list =
    let
        generatePairs : Random.Generator (List ( Float, a ))
        generatePairs =
            list
                |> List.map (\item -> Random.map (\r -> ( r, item )) (Random.float 0 1))
                |> sequence
    in
    Random.map (List.map Tuple.second << List.sortBy Tuple.first) generatePairs



-- EXAMPLES
--


coursesExamples : List Course
coursesExamples =
    [ foundations, course1 ]


course1 : Course
course1 =
    { id = 1
    , title = "Ausdrücke"
    , description = "Lerne unterschiedliche Arten von Ausdrücken in Haskell kennen."
    , lectures =
        [ lecture1
        , lecture2
        , lecture3
        , lecture4
        , lecture5
        ]
    }


lecture2 : Lecture
lecture2 =
    { id = 2
    , title = "Typen von zweistelligen Ausdrücken"
    , description = "Diese Lektion beinhaltet Aufgaben mit zweistelligen Ausdrücken, die über einfache Operatoren miteinander verbunden sind."
    , badge =
        { id = "binaryexpression"
        , name = "Zweistellige Ausdrücke"
        , image = Nothing
        }
    , learningContent =
        { id = 1
        , title = "Zweistellige Ausdrücke"
        , examples =
            [ { id = 1
              , title = "Zweistelliger Ausdruck"
              , expression = "x :: Int\nx = 1 + 2"
              , description = Just "In Haskell gibt es viele Operatoren, die über zwei Ausdrücke gelegt werden können. Diese Operatoren sind z.B. +, -, *, /, ++, &&, || und viele mehr."
              }
            ]
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
        , image = Nothing
        }
    , learningContent =
        { id = 1
        , title = "Funktionen in Haskell"
        , examples =
            [ { id = 1
              , title = "Einfache Funktion"
              , expression = "add :: Int -> Int -> Int\nadd x y = x + y"
              , description = Just "Funktionen sind ein zentraler Bestandteil von Haskell. Funktionen können mehrere Argumente haben und verschiedene Typen zurückgeben. In diesem Beispiel wird eine Funktion `add` definiert, die zwei Ganzzahlen addiert und das Ergebnis zurückgibt."
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
        , image = Nothing
        }
    , learningContent =
        { id = 1
        , title = "Guards in Haskell"
        , examples =
            [ { id = 0
              , title = "Guard-Ausdrücke"
              , expression = "absolute :: Int -> Int\nabsolute x\n    | x >= 0 = x\n    | otherwise = -x"
              , description = Just "Guards sind eine Möglichkeit, Bedingungen in Haskell übersichtlich und lesbar zu gestalten. Sie ermöglichen es, verschiedene Fälle einer Funktion durch Bedingungen zu unterscheiden. Guards werden mit einem senkrechten Strich (|) eingeleitet und können mehrere Bedingungen enthalten, die nacheinander geprüft werden."
              }
            , { id = 1
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
        , image = Nothing
        }
    , learningContent =
        { id = 1
        , title = "Pattern Matching in Haskell"
        , examples =
            [ { id = 1
              , title = "Einfaches Pattern Matching"
              , expression = ""
              , description = Just "Pattern Matching ist eine leistungsstarke Funktion in Haskell, die es ermöglicht, Werte anhand ihrer Struktur zu zerlegen und zu analysieren. Es wird häufig in Funktionen verwendet, um verschiedene Fälle zu behandeln."
              }
            , { id = 1
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
        , image = Nothing
        }
    , learningContent =
        { id = 1
        , title = "Einfache Ausdrücke bzw. Typen"
        , examples =
            [ { id = 1
              , title = "Ausdrücke"
              , expression = ""
              , description = Just "In Haskell ist alles ein Ausdruck. Um Wikipedia zu zitieren: \"Ein Ausdruck ist in vielen Programmiersprachen ein Konstrukt, das gemäß einer gegebenen Semantik in Bezug auf einen Kontext ausgewertet werden kann, also einen Wert liefert.\"."
              }
            , { id = 1
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


foundations : Course
foundations =
    { id = 0
    , title = "Grundlagen"
    , description = "Lerne die Grundlagen von Haskell kennen."
    , lectures =
        [ simpleHaskellProgramLecture
        , simpleDataTypesLecture
        , dataTypesLecture
        , operatorsLecture
        , syntaxLecture
        ]
    }


syntaxLecture : Lecture
syntaxLecture =
    { id = 9
    , title = "Let-In und Where"
    , description = "In dieser Lektion lernst du die Unterschiede und Anwendungsfälle von `let-in` und `where` in Haskell kennen."
    , badge =
        { id = "let-in-where"
        , name = "Let-In und Where"
        , image = Nothing
        }
    , learningContent =
        { id = 1
        , title = "Let-In und Where in Haskell"
        , examples =
            [ { id = 1
              , title = "Let-In Beispiel"
              , expression = "let x = 5\n    y = 6\nin x + y"
              , description = Just "`let-in` wird verwendet, um lokale Bindungen innerhalb eines Ausdrucks zu definieren. In diesem Beispiel werden `x` und `y` innerhalb des `let`-Blocks definiert und im `in`-Block verwendet."
              }
            , { id = 2
              , title = "Where Beispiel"
              , expression = "f x = x + y\n  where y = 5"
              , description = Just "`where` wird verwendet, um lokale Bindungen am Ende einer Funktion zu definieren. In diesem Beispiel wird `y` im `where`-Block definiert und in der Funktion `f` verwendet."
              }
            , { id = 3
              , title = "Unterschiede zwischen Let-In und Where"
              , expression = "let x = 5 in x * 2  -- Gültig nur innerhalb des Ausdrucks\nf x = x + y\n  where y = 5  -- Gültig für die gesamte Funktion"
              , description = Just "`let-in` ist ein Ausdruck und kann überall verwendet werden, während `where` nur in Funktionsdefinitionen vorkommt. `let-in` ist nützlich für temporäre Berechnungen, während `where` für Klarheit in Funktionsdefinitionen sorgt."
              }
            ]
        }
    , exercises =
        [ SingleExpression
            { id = 21
            , title = "Let-In Übung"
            , description = Just "Was ist das Ergebnis des folgenden Ausdrucks?"
            , expression = "let x = 3\n    y = 4\nin x * y"
            , answers =
                [ { code = "12"
                  , isCorrect = True
                  }
                , { code = "7"
                  , isCorrect = False
                  }
                , { code = "0"
                  , isCorrect = False
                  }
                , { code = "Fehler"
                  , isCorrect = False
                  }
                ]
            }
        , SingleExpression
            { id = 22
            , title = "Where Übung"
            , description = Just "Was ist das Ergebnis des Aufrufs der Funktion `f 2`?"
            , expression = "f x = x + y\n  where y = 3"
            , answers =
                [ { code = "5"
                  , isCorrect = True
                  }
                , { code = "2"
                  , isCorrect = False
                  }
                , { code = "3"
                  , isCorrect = False
                  }
                , { code = "Fehler"
                  , isCorrect = False
                  }
                ]
            }
        , SingleExpression
            { id = 23
            , title = "Let-In und Where Vergleich"
            , description = Just "Welcher der folgenden Aussagen ist korrekt?"
            , expression = ""
            , answers =
                [ { code = "`let-in` kann überall verwendet werden, `where` nur in Funktionsdefinitionen."
                  , isCorrect = True
                  }
                , { code = "`where` kann überall verwendet werden, `let-in` nur in Funktionsdefinitionen."
                  , isCorrect = False
                  }
                , { code = "`let-in` und `where` sind identisch."
                  , isCorrect = False
                  }
                , { code = "`let-in` ist schneller als `where`."
                  , isCorrect = False
                  }
                ]
            }
        ]
    }


operatorsLecture : Lecture
operatorsLecture =
    { id = 6
    , title = "Operatoren"
    , description = "In dieser Lektion lernst du einige Operatoren in Haskell kennen."
    , badge =
        { id = "operators"
        , name = "Operatoren"
        , image = Nothing
        }
    , learningContent =
        { id = 0
        , title = "Operatoren in Haskell"
        , examples =
            [ { id = 0
              , title = "Operatoren in Haskell"
              , expression = ""
              , description = Just "Haskell bietet eine Vielzahl von Operatoren, die auf verschiedene Datentypen angewendet werden können. Diese Operatoren ermöglichen es, Ausdrücke zu kombinieren und zu manipulieren."
              }
            , { id = 1
              , title = "Plus-Operator (Addition)"
              , expression = "1 + 1"
              , description = Just "`+` nimmt zwei Zahlen und rechnet diese zusammen. Weitere arithmetische Operatoren wie `-`, `*` und `/` gibt es ebenfalls."
              }
            , { id = 2
              , title = "Vergleichsoperatoren `<`, `>`, `==`"
              , expression = "2 < 3"
              , description = Just "Vergleichen Werte miteinander (in diesem Fall Zahlen) und liefert einen booleschen Wert zurück."
              }
            , { id = 3
              , title = "++-Operator (Konkatenation)"
              , expression = "[1, 2] ++ [3, 4]"
              , description = Just "`++` nimmt zwei Listen und fügt diese zusammen. Da String gleichbedeutend ist mit [Char] (List von Char), kann `++` auch auf diese angewendet werden."
              }
            , { id = 4
              , title = ":-Operator (cons)"
              , expression = "1 : [2, 3]"
              , description = Just "`:` ist ein spezieller Operator, der ein Element an den Anfang einer Liste anfügt. Er wird auch als `cons` bezeichnet."
              }
            , { id = 5
              , title = "&&-Operator (und)"
              , expression = "True && False"
              , description = Just "`&&` nimmt zwei Boolesche Werte und gibt `True` zurück, wenn beide `True` sind. `||` als oder-Operator ist ebenfalls vorhanden."
              }
            , { id = 6
              , title = "&&-Operator (und)"
              , expression = "(1 < 3) && (5 < 6)"
              , description = Just "Mit diesem Beispiel soll verdeutlicht werden, dass man Operatoren miteinander verknüpfen kann."
              }
            ]
        }
    , exercises =
        [ SingleExpression
            { id = 19
            , title = "Verstanden?"
            , description = Just "Bist du bereit weiter zu machen?"
            , expression = "42"
            , answers =
                [ { code = "Ja"
                  , isCorrect = True
                  }
                ]
            }
        ]
    }


dataTypesLecture : Lecture
dataTypesLecture =
    { id = 8
    , title = "Weitere Datentypen"
    , description = "In dieser Lektion lernst du Datentypen in Haskell kennen und wie man sie selbst definiert."
    , badge =
        { id = "data-types"
        , name = "Datentypen"
        , image = Nothing
        }
    , learningContent =
        { id = 0
        , title = "Datentypen"
        , examples =
            [ { id = 0
              , title = "Datentypen in Haskell"
              , expression = ""
              , description = Just "Haskell erlaubt es, eigene Datentypen zu definieren. Diese können komplexe Strukturen repräsentieren und ermöglichen eine klare und präzise Modellierung von Daten."
              }
            , { id = 0
              , title = "Auto-Datentyp"
              , expression = "data Auto = Audi \n | BMW \n | Mercedes \n | Tesla \n\nx = Mercedes"
              , description = Just "Das Schlüsselwort `data` leitet einen neuen Datentyp ein, gefolgt vom Namen. Daann folgenden die möglichen Werte (Typkonstruktoren) des Datentyps, die durch das Pipe-Zeichen (`|`) getrennt sind."
              }
            , { id = 1
              , title = "Person-Datentyp mit Feldern"
              , expression = "data Person = Person String Int\n\njoscha = Person \"Joscha\" 32"
              , description = Just "Hier wird ein Datentyp `Person` mit einem Typkonstruktor definiert, der zwei Felder hat: einen `String` für den Namen und einen `Int` für das Alter. Der Ausdruck `joscha = Person \"Joscha\" 20` erstellt eine Instanz des Datentyps `Person` mit dem Namen \"Joscha\" und dem Alter 32."
              }
            , { id = 2
              , title = "Generische Datentypen - Beispiel Maybe"
              , expression = "data Maybe a = Nothing | Just a\n\nx = Just 42"
              , description = Just "Hier wird ein generischer Datentyp `Maybe` definiert, der entweder den Wert `Nothing` oder einen Wert des Typs `a` enthalten kann. Der Ausdruck `x = Just 42` erstellt eine Instanz des Datentyps `Maybe Int`, die den Wert `42` enthält. Der Typ `Maybe` ist ein Beispiel für einen parametrisierten Datentyp, der es ermöglicht, verschiedene Typen zu verwenden."
              }
            , { id = 3
              , title = "Generische Datentypen - Liste"
              , expression = "data List a = Nil | Cons a (List a)\n\nx = Cons 1 (Cons 2 (Cons 3 Nil))"
              , description = Just "Hier wird ein generischer Datentyp `List` definiert, der entweder leer (`Nil`) oder ein Element (`Cons a (List a)`) enthalten kann. Der Ausdruck `x = Cons 1 (Cons 2 (Cons 3 Nil))` erstellt eine Instanz des Datentyps `List Int`, die die Werte `1`, `2` und `3` enthält. Dieser Datentyp ist zugleich generisch und rekursiv, da der Typkonstruktor Cons als zweiten Parameter den Typen List selbst enthält."
              }
            ]
        }
    , exercises =
        [ SingleExpression
            { id = 20
            , title = "Verstanden?"
            , description = Just "Bist du bereit weiter zu machen?"
            , expression = "42"
            , answers =
                [ { code = "Ja"
                  , isCorrect = True
                  }
                , { code = "Nein"
                  , isCorrect = False
                  }
                , { code = "Weiß nicht"
                  , isCorrect = False
                  }
                ]
            }
        ]
    }


simpleDataTypesLecture : Lecture
simpleDataTypesLecture =
    { id = 7
    , title = "Einfache Datentypen"
    , description = "In dieser Lektion lernst du einfache Datentypen in Haskell kennen."
    , badge =
        { id = "simple-data-types"
        , name = "Einfache Datentypen"
        , image = Nothing
        }
    , learningContent =
        { id = 0
        , title = "Einfache Datentypen"
        , examples =
            [ { id = 0
              , title = "Einfache Datentypen in Haskell"
              , expression = ""
              , description = Just "In Haskell gibt es verschiedene einfache Datentypen wie `Int`, `Float`, `Bool` und `String`. Diese Datentypen sind die Bausteine für komplexere Datenstrukturen und ermöglichen es, verschiedene Arten von Werten zu repräsentieren."
              }
            , { id = 0
              , title = "Ganzzahl"
              , expression = "x :: Int\nx = 42"
              , description = Just "Eine Ganzzahl ist ein einfacher Datentyp, der ganze Zahlen repräsentiert."
              }
            , { id = 1
              , title = "Ganzzahl"
              , expression = "x = 42"
              , description = Just "Haskell kann einen Typ ableiten (in den meisten Fällen), auch wenn er nicht explizit angegeben wird. Das nennt sich Typinferenz."
              }
            , { id = 2
              , title = "Gleitkommazahl"
              , expression = "x :: Float\nx = 3.14"
              , description = Just "Eine Gleitkommazahl ist ein Datentyp, der Fließkommazahlen repräsentiert."
              }
            , { id = 3
              , title = "String"
              , expression = "x :: String\nx = \"Hallo Welt\""
              , description = Just "Strings sind Zeichenketten, die Text repräsentieren."
              }
            , { id = 4
              , title = "String bzw. Liste von Zeichen"
              , expression = "x :: [Char]\nx =  \"Hallo Welt\""
              , description = Just "Strings sind in Haskell Listen von Zeichen, die auch als `[Char]` dargestellt werden können. Das bedeutet, dass ein String in Haskell eine Liste von `Char`-Elementen ist."
              }
            , { id = 5
              , title = "Bool"
              , expression = "x :: Bool\nx = True"
              , description = Just "Ein Boolescher Datentyp kann entweder `True` oder `False` sein."
              }
            ]
        }
    , exercises =
        [ SingleExpression
            { id = 19
            , title = "Verstanden?"
            , description = Just "Bist du bereit weiter zu machen?"
            , expression = "42"
            , answers =
                [ { code = "Ja"
                  , isCorrect = True
                  }
                , { code = "Nein"
                  , isCorrect = False
                  }
                , { code = "Weiß nicht"
                  , isCorrect = False
                  }
                ]
            }
        ]
    }


simpleHaskellProgramLecture : Lecture
simpleHaskellProgramLecture =
    { id = 6
    , title = "Einfaches Haskell-Programm"
    , description = "In dieser Lektion wird ein einfaches Haskell-Programm vorgestellt."
    , badge =
        { id = "simple-haskell-program"
        , name = "Einfaches Haskell-Programm"
        , image = Nothing
        }
    , learningContent =
        { id = 0
        , title = "Einfaches Haskell-Programm"
        , examples =
            [ { id = 0
              , title = "module"
              , expression = "module Main (main) where\n\nmain :: IO ()\nmain = do\n    putStrLn \"Hallo, Welt!\""
              , description = Nothing
              }
            , { id = 0
              , title = "module"
              , expression = "module Main (main) where"
              , description = Just "Diese Zeile definiert ein Haskell-Modul namens `Main`, das die Funktion `main` exportiert. Das Modul ist der Einstiegspunkt für das Programm. In den Klammern sind all die Funktionen aufgelistet, die von diesem Modul exportiert werden und somit von anderen Modulen verwendet werden können."
              }
            , { id = 1
              , title = "main-Funktion"
              , expression = "main :: IO ()"
              , description = Just "Die `main`-Funktion ist der Einstiegspunkt für das Programm. Sie hat den Typ `IO ()`, was bedeutet, dass innerhalb dieser Funktion Eingabe/Ausgabe-Operation verwendet werden."
              }
            , { id = 2
              , title = "main-Funktion"
              , expression = "main = do\n    putStrLn \"Hallo, Welt!\""
              , description = Just "Die `do`-Notation ermöglicht es, mehrere IO-Operationen in einer Sequenz auszuführen. In diesem Fall wird die Funktion `putStrLn` verwendet, um den Text \"Hallo, Welt!\" auf der Konsole auszugeben. Diese `do`-Notation ist eine spezielle Syntax in Haskell, die es ermöglicht, mehrere IO-Operationen in einer lesbaren Weise zu kombinieren und wird hier nicht weiter behandelt."
              }
            , { id = 3
              , title = "earn you a haskell"
              , expression = "add :: Int -> Int -> Int\nadd x y = x + y"
              , description = Just "Die Beispiele in dieser Anwendung werden außerhalb der main-Funktion defininiert und können dann in der main-Funktion oder in anderen Funktionen verwendet werden. In diesem Beispiel wird eine Funktion `add` definiert, die zwei Ganzzahlen addiert und das Ergebnis zurückgibt. Der Typ der Funktion ist `Int -> Int -> Int`, was bedeutet, dass sie zwei Ganzzahlen als Eingabe nimmt und eine Ganzzahl zurückgibt."
              }
            ]
        }
    , exercises =
        [ SingleExpression
            { id = 16
            , title = "Einfaches Haskell-Programm"
            , description = Just "Welchen Typ hat die folgende Funktion?"
            , expression = "main :: IO ()\nmain = do\n    putStrLn \"Hallo, Welt!\""
            , answers =
                [ { code = "IO ()"
                  , isCorrect = True
                  }
                , { code = "String"
                  , isCorrect = False
                  }
                , { code = "Int"
                  , isCorrect = False
                  }
                , { code = "SomeType"
                  , isCorrect = False
                  }
                ]
            }
        , SingleExpression
            { id = 17
            , title = "Einfaches Haskell-Programm"
            , description = Just "Wo müssen Funktionen eines Moduls stehen, damit sie in anderen Modulen verwendet werden können?"
            , expression = "module Main (main) where"
            , answers =
                [ { code = "In der main-Funktion"
                  , isCorrect = False
                  }
                , { code = "In den Klammern nach dem Modulnamen"
                  , isCorrect = True
                  }
                , { code = "In der Datei, die das Programm ausführt"
                  , isCorrect = False
                  }
                , { code = "In der Datei, die die Funktion importiert"
                  , isCorrect = False
                  }
                ]
            }
        , SingleExpression
            { id = 18
            , title = "Einfaches Haskell-Programm"
            , description = Just "Was ist der Typ der Funktion `putStrLn`?"
            , expression = "putStrLn :: String -> IO ()"
            , answers =
                [ { code = "String -> IO ()"
                  , isCorrect = True
                  }
                , { code = "IO ()"
                  , isCorrect = False
                  }
                , { code = "String"
                  , isCorrect = False
                  }
                , { code = "Int"
                  , isCorrect = False
                  }
                ]
            }
        ]
    }
