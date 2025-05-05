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
    , exercises = [ exercise1 ]
    }


exercise1 : Exercise
exercise1 =
    SingleExpression
        { id = 1
        , title = "Zahlenausdruck"
        , description = Just "Welchen Typ hat der folgende Ausdruck?"
        , expression = "1"
        , answers =
            [ answer1
            , answer2
            , answer3
            , answer4
            ]
        }


answer1 : Answer
answer1 =
    { description = Nothing
    , code = "Int"
    , isCorrect = True
    }


answer2 : Answer
answer2 =
    { description = Nothing
    , code = "String"
    , isCorrect = False
    }


answer3 : Answer
answer3 =
    { description = Nothing
    , code = "Float"
    , isCorrect = False
    }


answer4 : Answer
answer4 =
    { description = Nothing
    , code = "SomeType"
    , isCorrect = False
    }
