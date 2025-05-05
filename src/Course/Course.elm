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
    { description : Maybe String
    , code : String
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
    , description = "Lektion über einfache Ausdrücke"
    , exercises = [ exercise1, exercise2, exercise3 ]
    }


exercise1 : Exercise
exercise1 =
    SingleExpression
        { id = 1
        , title = "Zahlenausdruck"
        , description = Just "Welchen Typ hat der folgende Ausdruck?"
        , expression = "1"
        , answers =
            [ { description = Nothing
              , code = "Int"
              , isCorrect = True
              }
            , { description = Nothing
              , code = "String"
              , isCorrect = False
              }
            , { description = Nothing
              , code = "Float"
              , isCorrect = False
              }
            , { description = Nothing
              , code = "SomeType"
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
            [ { description = Nothing
              , code = "Int"
              , isCorrect = False
              }
            , { description = Nothing
              , code = "String"
              , isCorrect = True
              }
            , { description = Nothing
              , code = "Float"
              , isCorrect = False
              }
            , { description = Nothing
              , code = "SomeType"
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
            [ { description = Nothing
              , code = "Int"
              , isCorrect = False
              }
            , { description = Nothing
              , code = "String"
              , isCorrect = False
              }
            , { description = Nothing
              , code = "Float"
              , isCorrect = False
              }
            , { description = Nothing
              , code = "Bool"
              , isCorrect = True
              }
            ]
        }
