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



-- examples


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
