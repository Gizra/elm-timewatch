module App where

import Char
import Config exposing (backendUrl, accessToken)
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
  | Fetched UserAction
  | HttpError Http.Error

type TickStatus = Ready | Waiting

type UserAction = Enter | Leave

type alias Response =
  { employee : String
  , start : Int
  , end : Maybe Int
  }

type alias Model =
  { pincode : String
  , status : Status
  , message : Message
  , tickStatus : TickStatus
  , date : Maybe Time.Time
  , connected : Bool
  }

initialModel : Model
initialModel =
  { pincode = ""
  , status = Init
  , message = Empty
  , tickStatus = Ready
  , date = Nothing
  , connected = False
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
        url = Config.backendUrl ++ "/api/v1.0/timewatch-punch"

      in
        ( { model | status <- Fetching }
        , getJson url Config.accessToken model.pincode
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
            operation =
              case response.end of
                -- When the session has no end date, it means a session was
                -- opened.
                Nothing -> Enter

                -- When the end date exist, it means the session was closed.
                Just int -> Leave


            greeting =
              if operation == Enter then "Hi" else "Bye"


            message = greeting ++ " " ++ response.employee

          in
            ( { model
              | status <- Fetched operation
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


    ledLight =
      let
        className =
          case model.connected of
            False -> "-off"
            True -> "-on"

      in
        div
          [ class "col-xs-2 main-header led text-center" ]
          [ span [ class <| "light " ++ className ] []]


    simpleDiv class' =
      div [ class  class' ] []


    pincodeText delta =
      let
        text' =
          String.slice delta (delta + 1) model.pincode
      in
        div [ class  "item pin" ] [ text text']


    icon =
      let
        className =
          case model.status of
            Init -> ""
            Fetching -> "fa-circle-o-notch fa-spin"
            Fetched Enter -> "fa-check -success -in"
            Fetched Leave -> "fa-check -success -out"
            HttpError error -> "fa-exclamation-triangle -error"

      in
        i [ class  <| "fa " ++ className ] []


    pincode =
      div
          [ class "col-xs-5 main-header pin-code text-center" ]
          [ div
              [ class "code clearfix" ]
              [ simpleDiv "item icon fa fa-lock"
                , span [] ( List.map pincodeText [0..3] )
                , div [ class "item icon -dynamic-icon" ] [ icon ]
              ]
          ]


    clockIcon =
      i [ class "fa fa-clock-o icon" ] []


    dateString =
      case model.date of
        Just time ->
        Date.fromTime time |> DF.format "%A, %d %B, %Y"

        Nothing -> ""


    timeString =
      case model.date of
        Just time ->
          Date.fromTime time |> DF.format " %H:%M"

        Nothing -> ""


    date =
      div
          [ class "col-xs-5 main-header info text-center" ]
          [ span [][ text dateString ]
          , span
                [ class "time" ]
                [ clockIcon , span [] [ text timeString ] ]
          ]


    numberPad =
      div [ class "col-xs-5 text-center" ] []


    -- Adding a "class" to toggle the view display (hide/show).
    mainViewClass =
      let
        className = "-active"

      in
        case model.status of
          Fetched Enter -> className
          Fetched Leave -> className
          HttpError error -> className
          _ -> ""


    responseMessage =
      let
        className =
          case model.status of
            Fetched Enter -> "-success -in"
            Fetched Leave -> "-success -out"
            HttpError error -> "-error"
            _ -> ""

        icon =
          case model.status of
            HttpError error ->
              i [ class "fa fa-exclamation-triangle icon" ] []

            _ ->
              i [ class "fa fa-check icon" ] []



      in
        div
            [ class <| "message " ++ className]
            [ span [] [ icon , text "Hi yaron good morning! and have a nice day :)" ] ]

    view =
      div
          [ class "col-xs-7 view" ]
          [ div
              [ class <| "main " ++ mainViewClass ]
              [ div
                  [ class "wrapper" ] [ responseMessage ]
                  , div [ class "text-center" ] [ i [ class "symbol fa-4x fa fa-sign-out"] [] ]
              ]
          ]


  in
    div
        [ class "container" ]
        [ div
            [ class "row dashboard" ]
            [ pincode
              , date
              , ledLight
              , numberPad
              , view
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
      , div [ class "model-debug" ] [ text <| toString model ]
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


-- EFFECTS

getJson : String -> String -> String -> Effects Action
getJson url accessToken pincode =
  Http.send Http.defaultSettings
    { verb = "POST"
    , headers = [ ("access-token", accessToken) ]
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
    <| Json.object3 Response
      ("employee" := Json.string)
      ("start" := Json.int)
      (Json.maybe ("end" := Json.int))


getDate : Effects Action
getDate =
  Task.map SetDate getCurrentTime |> Effects.task

tick : Effects Action
tick =
  Task.sleep (1 * Time.second)
    |> Task.map (\_ -> Tick)
    |> Effects.task
