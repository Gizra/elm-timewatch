module Hi where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (Error)
import String exposing (length, repeat)
import Task exposing (Task, succeed)

import Debug

-- MODEL

type Status =
  Init
  | Fetching
  | Fetched
  | HttpError Http.Error

type alias Model =
  { pinCode : String
  , status : Status
  }

initialModel : Model
initialModel =
  { pinCode = ""
  , status = Init
  }

init : (Model, Effects Action)
init =
  ( initialModel
  , Effects.none
  )


-- UPDATE

type Action
  = AddDigit Int
  | SubmitCode

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    AddDigit digit ->
      let
        pinCode' =
          if length model.pinCode < 4
            then model.pinCode ++ toString(digit)
            else ""
        effects' =
          if length model.pinCode == 3
            then Task.succeed SubmitCode |> Effects.task
            else Effects.none
      in
        ( { model | pinCode <- pinCode' }
        , effects'
        )

    SubmitCode ->
      let
        _ = Debug.log model.pinCode True
      in
        ( { model | pinCode <- "" }
        , Effects.none )

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "number-pad" ]
    [ div
        [ class "number-buttons" ]
        ( List.map (digitButton address) [0..9] |> List.reverse )
        , div [] [ text <| repeat (length model.pinCode) "*" ]
    ]

digitButton : Signal.Address Action -> Int -> Html
digitButton address digit =
  button [ onClick address (AddDigit digit) ] [ text <| toString digit ]
