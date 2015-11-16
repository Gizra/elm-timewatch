module Hi where

import Config exposing (backendUrl)
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import Json.Encode as JE
import String exposing (length)
import Task

import Debug

-- MODEL

type Message =
  Empty
  | Error String
  | Success String

type Status =
  Init
  | Fetching
  | Fetched
  | HttpError Http.Error

type alias Model =
  { pincode : String
  , status : Status
  , message : Message
  }

initialModel : Model
initialModel =
  { pincode = ""
  , status = Init
  , message = Empty
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
  | ShowResponse (Result Http.Error String)
  | SetMessage Message

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    AddDigit digit ->
      let
        pincode' =
          if length model.pincode < 4
            then model.pincode ++ toString(digit)
            else ""
        effects' =
          if length model.pincode == 3
            then Task.succeed SubmitCode |> Effects.task
            else Effects.none
      in
        ( { model | pincode <- pincode' }
        , effects'
        )

    SubmitCode ->
      let
        url : String
        url = Config.backendUrl ++ "/api/v1.0/session"

      in
        if model.status == Fetching || model.status == Fetched
          then
            (model, Effects.none)
          else
            ( { model
              | pincode <- ""
              , status <- Fetching
              }
            , getJson url model.pincode
            )


    ShowResponse result ->
      case result of
        Ok token ->
          ( { model | status <- Fetched }
          , Task.succeed (SetMessage (Success "Success")) |> Effects.task
          )
        Err msg ->
          ( { model | status <- HttpError msg }
          , Task.succeed (SetMessage (Error "something is wrong")) |> Effects.task
          )

    SetMessage message ->
      ( { model | message <- message }
      , Effects.none
      )
-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "keypad" ]
    [ (viewMessage model.message)
    ,  div
        [ class "number-buttons" ]
        ( List.map (digitButton address) [0..9] |> List.reverse )
    ]

viewMessage : Message -> Html
viewMessage message =
  let
    (className, string) =
      case message of
        Empty -> ("none", "")
        Error msg -> ("error", msg)
        Success msg -> ("success", msg)
  in
    div [ class className ] [ text string ]


digitButton : Signal.Address Action -> Int -> Html
digitButton address digit =
  button [ onClick address (AddDigit digit) ] [ text <| toString digit ]


-- EFFECTS

getJson : String -> String -> Effects Action
getJson url pincode =
  Http.send Http.defaultSettings
    { verb = "POST"
    , headers = [ ("access-token", "lXlTh7PR30mQN316SN3LofK95krQjCltBnygfjkleyQ") ]
    , url = url
    , body = ( Http.string <| dataToJson pincode )
    }
    |> Http.fromJson decodePincode
    |> Task.toResult
    |> Task.map ShowResponse
    |> Effects.task




dataToJson : String -> String
dataToJson code =
  JE.encode 0
    <| JE.object
        [ ("pincode", JE.string code) ]

decodeAccessToken : JD.Decoder String
decodeAccessToken =
  JD.at ["access_token"] <| JD.string


decodePincode : JD.Decoder String
decodePincode =
  JD.at ["pincode"] <| JD.string
