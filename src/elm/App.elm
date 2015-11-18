module App where

import Char
import Config exposing (backendUrl)
import Date exposing (..)
import Date.Format as DF exposing (format)
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing ((:=))
import Json.Encode as JE
import String exposing (length)
import Task
import TaskTutorial exposing (getCurrentTime)
import Time exposing (second)

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

type TickStatus = Ready | Waiting

type alias Response =
  { employee : String
  , action : String
  }

type alias Model =
  { pincode : String
  , status : Status
  , message : Message
  , tickStatus : TickStatus
  , date : Maybe Time.Time
  }

initialModel : Model
initialModel =
  { pincode = ""
  , status = Init
  , message = Empty
  , tickStatus = Ready
  , date = Nothing
  }

init : (Model, Effects Action)
init =
  ( initialModel
  , Effects.batch [getDate, tick]
  )

pincodeLength = 4


-- UPDATE

type Action
  = AddDigit Int
  | Reset
  | SetDate Time.Time
  | SetMessage Message
  | SubmitCode
  | Tick
  | UpdateDataFromServer (Result Http.Error Response)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    AddDigit digit ->
      let
        pincode' =
          if length model.pincode < pincodeLength
            then model.pincode ++ toString(digit)
            else model.pincode

        effects' =
          -- Calling submit code when pincode length is one less than the needed
          -- length, since at this point the model isn't updated yet with the
          -- current digit.
          if length model.pincode == pincodeLength - 1
            then Task.succeed SubmitCode |> Effects.task
            else Effects.none

      in
        ( { model
          | pincode <- pincode'
          , message <- Empty
          , status <- Init
          }
        , effects'
        )

    SubmitCode ->
      let
        url : String
        url = Config.backendUrl ++ "/api/v1.0/session"

      in
        ( { model | status <- Fetching }
        , getJson url model.pincode
        )

    SetDate time ->
        ( { model
          | tickStatus <- Ready
          , date <- Just time
          }
        , Effects.none
        )

    Tick ->
      let
        effects =
          if model.tickStatus == Ready
            then Effects.batch [ getDate, tick ]
            else Effects.none
      in
        ( { model | tickStatus <- Waiting }
        , effects
        )

    UpdateDataFromServer result ->
      case result of
        Ok response ->
          let
            message = response.employee ++ " " ++ response.action
          in
            ( { model
              | status <- Fetched
              , pincode <- ""
              }
            , Task.succeed (SetMessage (Success message)) |> Effects.task
            )
        Err error ->
          let
            message =
              getErrorMessageFromHttpResponse error
          in
            ( { model
              | status <- HttpError error
              , pincode <- ""
              }
            , Task.succeed (SetMessage <| Error message) |> Effects.task
            )

    SetMessage message ->
      ( { model | message <- message }
      , Effects.none
      )


getErrorMessageFromHttpResponse : Http.Error -> String
getErrorMessageFromHttpResponse error =
  case error of
    Http.Timeout ->
      "Connection has timed out"

    Http.BadResponse code message ->
      -- TODO: Print the error's title
      if | code == 400 -> "Wrong pincode"
         | code == 401 -> "Invalid access token"
         | code == 429 -> "Too many login requests with the wrong username or password. Wait a few hours before trying again"
         | code >= 500 -> "Some error has occurred on the server"
         | otherwise -> "Unknown error has occurred"

    Http.NetworkError ->
      "A network error has occured"

    Http.UnexpectedPayload message ->
      "Unexpected response: " ++ message

    _ ->
      "Unexpected error: " ++ toString error


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    digitButton digit =
      button [ onClick address (AddDigit digit) ] [ text <| toString digit ]


    simpleDiv class' =
      div [ class  class' ] []

    pincodeText delta =
      let
        text' =
          String.slice delta (delta + 1) model.pincode
      in
        div [ class  "item pin" ] [ text text']

    pincode =
      div
      [ class "col-xs-5 main-header pin-code text-center" ]
      [ div
        [ class "code clearfix" ]
        [ simpleDiv "item icon fa fa-lock"
        , pincodeText 0
        , pincodeText 1
        , pincodeText 2
        , pincodeText 3
        , simpleDiv "item icon -dynamic-icon"
        ]
      ]

    clockIcon =
      i [ class "fa fa-clock-o icon" ] []


    dateString =
      case model.date of
        Just time ->
          let
            date =
              Date.fromTime time
          in

            DF.format "%A, %d %B, %Y" date
        Nothing -> ""

    timeString =
      case model.date of
        Just time ->
          let
            date =
              Date.fromTime time
          in

            DF.format "%H:%M" date
        Nothing -> ""

    date =
      div
        [ class "col-xs-5 main-header info text-center" ]
        [ span
            []
            [ span
                []
                [ text dateString ]
            , span
                [ class "time "]
                [ clockIcon
                , span [] [text timeString ]
                ]
            ]
        ]
  in
    div
      [ class "container"]
      [ div
          [ class "row dashboard" ]
          [ pincode
          , date
          ]
      , viewMainContent address model
      ]


viewMainContent : Signal.Address Action -> Model -> Html
viewMainContent address model =
  let
    digitButton digit =
      button [ onClick address (AddDigit digit) ] [ text <| toString digit ]
  in
    div
      [ class "keypad" ]
      [ div
          [ class "number-buttons" ]
          ( List.map digitButton [0..9] |> List.reverse )
      , (viewMessage model.message)
      , div [ class "model-debug" ] [ text (toString model) ]
      ]

viewMessage : Message -> Html
viewMessage message =
  let
    (className, string) =
      case message of
        Empty -> ("", "")
        Error msg -> ("error", msg)
        Success msg -> ("success", msg)
  in
    div [ id "status-message", class className ] [ text string ]



digitPreview : Char -> Html
digitPreview digit =
    -- TODO: Converting the char to int, to avoid the quotes when printing it.
    -- Is there a proper way to do that?
    div [ ] [ text <| toString <| (Char.toCode digit) - (Char.toCode '0') ]


-- EFFECTS

getJson : String -> String -> Effects Action
getJson url pincode =
  Http.send Http.defaultSettings
    { verb = "POST"
    , headers = [ ("access-token", "stF0R_j4DTYlpycjoXCCH0zezfKBJYq1sx_ULQFsYy8") ]
    , url = url
    , body = ( Http.string <| dataToJson pincode )
    }
    |> Http.fromJson decodeResponse
    |> Task.toResult
    |> Task.map UpdateDataFromServer
    |> Effects.task

dataToJson : String -> String
dataToJson code =
  JE.encode 0
    <| JE.object
        [ ("pincode", JE.string code) ]

decodeResponse : Json.Decoder Response
decodeResponse =
  Json.at ["data"]
    <| Json.object2 Response
      ("employee" := Json.string)
      ("action" := Json.string)


getDate : Effects Action
getDate =
  Task.map SetDate getCurrentTime |> Effects.task

tick : Effects Action
tick =
  Task.sleep (1 * Time.second)
    |> Task.map (\_ -> Tick)
    |> Effects.task
