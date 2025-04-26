module Main exposing (..)

import Browser
import Browser.Events
import Course.Course as Course exposing (Course, Lecture, course1)
import Html exposing (Html, a, button, div, h3, h4, h6, nav, text)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Svg exposing (g, path, svg)
import Svg.Attributes exposing (d, fill, height, preserveAspectRatio, stroke, transform, version, viewBox, width)



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
    }


type Page
    = Landing LandingPageModel
    | CoursesOverview CoursesOverviewPageModel
    | Course CoursePageModel


type alias Model =
    { page : Page
    , user : Maybe User
    }


type Msg
    = EnteringName String
    | EnteringNameDone
    | SelectCourse Course
    | SelectLecture Lecture
    | NoOp


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = Landing { username = Nothing, error = Nothing }
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
                                | page = Landing { l | error = Just UsernameIncorrect }
                              }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        SelectCourse course ->
            ( { model | page = Course { course = course, selectedLecture = Nothing } }, Cmd.none )

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


coursesOverview : CoursesOverviewPageModel -> Html Msg
coursesOverview c =
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
    div [ Html.Attributes.class "container" ]
        [ h3
            []
            [ text c.course.title ]
        , case c.selectedLecture of
            Just l ->
                lectureView l

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
        , div
            [ Html.Attributes.class "card-body" ]
            [ div
                [ Html.Attributes.class "card-text" ]
                [ text
                    ("Diese Lektion beinhaltet " ++ String.fromInt (List.length l.exercises) ++ " Übung(en).")
                ]
            ]
        ]


header : Maybe User -> Html Msg
header user =
    nav [ Html.Attributes.class "navbar" ]
        (a
            [ Html.Attributes.class "navbar-brand" ]
            [ logo
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


logo : Html msg
logo =
    svg
        [ version "1.0"
        , width "3em"
        , height "3em"
        , viewBox "0 0 1024.000000 1024.000000"
        , preserveAspectRatio "xMidYMid meet"
        ]
        [ g
            [ transform "translate(0.000000,1024.000000) scale(0.100000,-0.100000)"
            , fill "#000000"
            , stroke "none"
            ]
            [ path [ d """
            M2000 7440
            c-11 -11 -20 -31 -20 -44
            0 -16 228 -365 630 -967
            387 -579 630 -952 630 -966
            0 -14 -221 -356 -570 -879
            -434 -651 -570 -862 -570 -885
            0 -19 10 -39 25 -55
            l25 -25
            369 3
            c355 3 371 4 398 24
            15 11 295 418 621 905
            582 866 593 885 589 926
            -4 35 -101 186 -633 984
            -346 518 -641 955 -655 970
            l-27 29
            -396 0
            c-383 0 -397 -1 -416 -20
            z
            """ ] []
            , path [ d """
            M3261 7434
            c-12 -15 -21 -35 -21 -44
            0 -10 284 -442 630 -961
            408 -611 630 -952 630 -968
            0 -15 -201 -325 -567 -874
            -312 -468 -569 -863 -571 -878
            -3 -20 3 -35 21 -53
            l26 -26
            359 0
            c357 0 359 0 393 23
            24 16 145 187 397 562
            200 297 368 550 373 563
            11 27 56 31 66 5
            15 -38 727 -1090 759 -1120
            l34 -33
            169 0
            169 0
            50 -103
            c250 -511 824 -773 1359 -622
            112 32 288 118 378 186
            481 361 615 1030 310 1549
            -146 249 -368 431 -633 520
            -170 57 -197 58 -865 51
            l-609 -6
            -24 -28
            c-42 -49 -34 -72 90 -257
            l114 -170
            -53 -85
            c-45 -72 -80 -146 -120 -250
            -6 -15 -9 -17 -13 -5
            -3 8 -60 95 -127 195
            -67 99 -265 394 -440 655
            -1146 1708 -1431 2129 -1465 2163
            l-39 37
            -380 0
            -380 0
            -20 -26
            z

            m4194 -2484
            c297 -77 541 -304 641 -597
            45 -132 58 -223 51 -368
            -17 -369 -236 -682 -572 -816
            -169 -67 -351 -85 -520 -50
            -194 40 -348 123 -485 261
            -137 136 -218 286 -255 469
            -24 115 -16 324 15 431
            82 285 282 517 539 625
            66 28 142 49 251 69
            57 10 258 -4 335 -24
            z
            """ ] []
            , path [ d """
            M6935 4376
            c-37 -17 -70 -52 -84 -89
            -38 -98 37 -207 142 -207
            87 0 148 57 155 144
            4 56 -18 101 -69 139
            -34 25 -105 31 -144 13
            z
            """ ] []
            , path [ d """
            M7415 4376
            c-99 -43 -125 -175 -50 -251
            79 -78 212 -52 251 50
            30 81 -2 160 -80 196
            -48 22 -80 23 -121 5
            z
            """ ] []
            , path [ d """
            M6935 3896
            c-65 -29 -105 -107 -91 -177
            26 -130 200 -166 278 -57
            31 44 33 134 2 174
            -48 64 -126 89 -189 60
            z
            """ ] []
            , path [ d """
            M7415 3896
            c-37 -16 -83 -68 -91 -102
            -10 -39 2 -100 26 -134
            60 -85 190 -84 252 2
            31 44 33 134 2 174
            -48 64 -126 89 -189 60
            z
            """ ] []
            , path [ d """
            M5459 6131
            c-48 -48 -41 -79 47 -212
            161 -241 245 -359 266 -374
            20 -13 127 -15 924 -15
            l903 0
            20 26
            c20 26 21 39 21 299
            0 251 -1 273 -18 288
            -17 16 -106 17 -1076 17
            l-1058 0
            -29 -29
            z
            """ ] []
            ]
        ]
