module Hi where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Events exposing (onClick)


-- MODEL

type alias Model = String

initialModel : Model
initialModel = ""

init : (Model, Effects Action)
init =
  ( initialModel
  , Effects.none
  )


-- UPDATE

type Action = AddDigit Int

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    AddDigit digit ->
      ( model ++ toString(digit)
      , Effects.none
      )


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ button [ onClick address (AddDigit 1) ] [ text "1" ]
    , button [ onClick address (AddDigit 2) ] [ text "2" ]
    , button [ onClick address (AddDigit 3) ] [ text "3" ]
    , button [ onClick address (AddDigit 4) ] [ text "4" ]
    , div [ ] [ text (toString model) ]
    ]
