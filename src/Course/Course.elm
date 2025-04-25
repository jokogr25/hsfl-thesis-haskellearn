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


type alias SingleExpressionModel =
    { id : Int
    , title : String
    , description : Maybe String
    , expression : String
    , solutions : List Answer
    }


type Exercise
    = SingleExpression SingleExpressionModel


type alias Answer =
    { description : Maybe String
    , code : String
    , isCorrect : Bool
    }



-- examples


course1 : Course
course1 =
    { id = 1
    , title = "Expressions"
    , description = "Learn about expressions in Haskell"
    , lectures = [ lecture1 ]
    }


lecture1 : Lecture
lecture1 =
    { id = 1
    , title = "Unary Expressions"
    , description = "Lecture on unary expressions"
    , exercises = [ exercise1 ]
    }


exercise1 : Exercise
exercise1 =
    SingleExpression
        { id = 1
        , title = "Number Expression"
        , description = Just "What type does this expression have?"
        , expression = "1"
        , solutions = [ solution1 ]
        }


solution1 : Answer
solution1 =
    { description = Nothing
    , code = "Int"
    , isCorrect = True
    }


solution2 : Answer
solution2 =
    { description = Nothing
    , code = "String"
    , isCorrect = False
    }


solution3 : Answer
solution3 =
    { description = Nothing
    , code = "Float"
    , isCorrect = False
    }


solution4 : Answer
solution4 =
    { description = Nothing
    , code = "SomeType"
    , isCorrect = False
    }
