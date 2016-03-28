module Config (..) where


accessToken : String
accessToken =
  "XXX"


backendUrl : String
backendUrl =
  "http://backend.example.com/"


debugMode : Bool
debugMode =
  False


{-| Optional project to log the hours to.
-}
project =
  { label = "Project name"
  , id = 1
  }
