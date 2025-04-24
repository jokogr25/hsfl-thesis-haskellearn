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


type alias Exercise =
    { id : Int
    , title : String
    , description : String
    , solution : List ExerciseSolution
    }


type alias ExerciseSolution =
    { description : String
    , code : String
    , isCorrect : Bool
    }



-- examples


course1 : Course
course1 =
    { id = 1
    , title = "Introduction to Elm"
    , description = "Learn how to get started with Elm"
    , lectures = [ lecture1 ]
    }


course2 : Course
course2 =
    { id = 2
    , title = "Introduction to Haskell"
    , description = "Learn how to get started with Haskell"
    , lectures = [ lecture1 ]
    }


lecture1 : Lecture
lecture1 =
    { id = 1
    , title = "Getting Started"
    , description = "Learn how to get started with Elm"
    , exercises = [ exercise1 ]
    }


exercise1 : Exercise
exercise1 =
    { id = 1
    , title = "Hello World"
    , description = "Write a program that prints 'Hello World'"
    , solution = [ solution1 ]
    }


solution1 : ExerciseSolution
solution1 =
    { description = "Print 'Hello World'"
    , code = "main = text \"Hello World\""
    , isCorrect = True
    }
