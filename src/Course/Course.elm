module Course.Course exposing (..)

import Lecture.Lecture exposing (Lecture)


type alias Course =
    { id : String
    , title : String
    , lectures : List Lecture
    }


type alias Lecture =
    { id : String
    , title : String
    , description : String
    , exercises : List Exercise
    }


type alias Exercise =
    { id : String
    , title : String
    , description : String
    }
