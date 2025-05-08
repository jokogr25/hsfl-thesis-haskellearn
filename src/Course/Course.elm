module Course.Course exposing (..)


type alias Course =
    { id : Int
    , title : String
    , description : String
    , lectures : List Lecture
    }


type alias Lecture =
    { id : Int
    , title : String
    , description : String
    , exercises : List Exercise
    }


type Exercise
    = SingleExpression SingleExpressionModel


type alias SingleExpressionModel =
    { id : Int
    , title : String
    , description : Maybe String
    , expression : String
    , answers : List Answer
    }


type alias Answer =
    { code : String
    , isCorrect : Bool
    }



-- examples


course1 : Course
course1 =
    { id = 1
    , title = "Ausdrücke"
    , description = "Lerne etwas zu Ausdrücken in Haskell"
    , lectures = [ lecture1 ]
    }


lecture1 : Lecture
lecture1 =
    { id = 1
    , title = "Typen von einfachen Ausdrücken"
    , description = "In dieser Lektion wird dein Wissen über Typen von einfachen Ausdrücken getestet."
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
