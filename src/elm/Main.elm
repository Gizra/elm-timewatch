module Main (..) where

import Effects exposing (Never)
import App exposing (init, update, view)
import StartApp
import Task


app =
  StartApp.start
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
