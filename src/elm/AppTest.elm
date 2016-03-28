module AppTest (..) where

import ElmTest exposing (..)
import App exposing (Model)
import Effects exposing (Effects)


addDigitSuite : Test
addDigitSuite =
  suite
    "Add digit Action Suite"
    [ test "Send first digit" (assertEqual "5" (.pincode <| fst (addDigit 5)))
    , test "Send last digit" (assertEqual "1234" (.pincode <| fst (addDigitOnFullPincode 4)))
    ]


addDigit : Int -> ( App.Model, Effects App.Action )
addDigit val =
  App.update (App.AddDigit val) App.initialModel


addDigitOnFullPincode : Int -> ( App.Model, Effects App.Action )
addDigitOnFullPincode val =
  let
    model =
      App.initialModel

    model' =
      { model | pincode = "123" }
  in
    App.update (App.AddDigit val) model'


all : Test
all =
  suite
    "All App tests"
    [ addDigitSuite
    ]
