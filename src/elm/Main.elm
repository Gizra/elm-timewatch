module Main exposing (..)

import App exposing (init, update, view)
import Task
import Html.App as Html


app =
  Html.program
    { init = init
    , update = update
    , view = view
    , inputs = [ Signal.map App.SetTouchDevice isTouchDevice ]
    }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


port isTouchDevice : Signal Bool
