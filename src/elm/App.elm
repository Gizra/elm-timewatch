module App exposing (..)

import Config exposing (backendUrl, accessToken, pincodeLength)
import Date exposing (..)
import Date.Format as DF exposing (format)
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (class, classList, id, disabled, hidden)
import Html.Events exposing (onClick, on)
import Http
import Json.Decode as Json exposing ((:=), value)
import Json.Encode as JE
import String exposing (length)
import Task
import TaskTutorial exposing (getCurrentTime)
import Time exposing (second)


-- MODEL


type Message
  = Empty
  | Error String
  | Success String


type Status
  = Init
    -- User is in the middle of typing, before a request is sent.
  | InProgress
  | Fetching
  | Fetched UserAction
  | HttpError Http.Error


type TickStatus
  = Ready
  | Waiting


type UserAction
  = Enter
  | Leave


type alias Response =
  { employee : String
  , start : Int
  , end : Maybe Int
  , project : Maybe String
  }


type alias Project =
  { label : String
  , id : Int
  }


type alias Model =
  { pincode : String
  , status : Status
  , message : Message
  , tickStatus : TickStatus
  , date : Maybe Time.Time
  , connected : Bool
  , projects : List Project
  , selectedProject : Maybe Int
  , isTouchDevice : Bool
  , pressedButton : Maybe Int
  }


initialModel : Model
initialModel =
  { pincode = ""
  , status = Init
  , message = Empty
  , tickStatus = Ready
  , date = Nothing
  , connected = False
  , projects =
      [ { label = .label Config.project
        , id = .id Config.project
        }
      ]
  , selectedProject = Nothing
  , isTouchDevice = False
  , pressedButton = Nothing
  }


init : ( Model, Effects Action )
init =
  ( initialModel
  , Effects.batch [ getDate, (tick 1 Tick) ]
  )



-- UPDATE


type Action
  = AddDigit Int
  | DeleteDigit
  | NoOp
  | Reset
  | SetDate Time.Time
  | SetProject Int
  | SetMessage Message
  | SetTouchDevice Bool
  | SetPressedButton Int
  | SubmitCode
  | Tick
  | UpdateDataFromServer (Result Http.Error Response)
  | UnsetPressedButton


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    AddDigit digit ->
      let
        pincode' =
          if length model.pincode < pincodeLength then
            model.pincode ++ toString (digit)
          else
            model.pincode

        defaultEffect =
          [ Task.succeed (SetPressedButton digit) |> Effects.task ]

        effects' =
          -- Calling submit code when pincode length is one less than the needed
          -- length, since at this point the model isn't updated yet with the
          -- current digit.
          if length model.pincode == pincodeLength - 1 then
            (Task.succeed SubmitCode |> Effects.task) :: defaultEffect
          else
            defaultEffect
      in
        ( { model
            | pincode = pincode'
            , status = InProgress
          }
        , Effects.batch effects'
        )

    DeleteDigit ->
      let
        pincodeLength =
          length model.pincode

        ( pincode', status' ) =
          if pincodeLength > 0 then
            ( String.slice 0 (pincodeLength - 1) model.pincode, InProgress )
          else
            ( "", Init )
      in
        ( { model
            | pincode = pincode'
          }
        , Task.succeed (SetPressedButton -1) |> Effects.task
        )

    NoOp ->
      ( model
      , Effects.none
      )

    Reset ->
      let
        model' =
          if model.status == InProgress then
            model
          else
            { model
              | status = Init
              , selectedProject = Nothing
            }
      in
        ( model'
        , Effects.none
        )

    SetDate time ->
      ( { model
          | tickStatus = Ready
          , date = Just time
        }
      , Effects.none
      )

    SetProject projectId ->
      let
        id =
          case model.selectedProject of
            -- In case we want to disable the current selected project.
            Just val ->
              Nothing

            -- In case we have no selecte project and want to assign one.
            Nothing ->
              Just projectId
      in
        ( { model | selectedProject = id }
        , Effects.none
        )

    SetMessage message ->
      ( { model | message = message }
      , tick 3.5 Reset
      )

    SetTouchDevice val ->
      ( { model | isTouchDevice = val }
      , Effects.none
      )

    SetPressedButton val ->
      ( { model | pressedButton = Just val }
      , Effects.none
      )

    SubmitCode ->
      let
        url =
          Config.backendUrl ++ "/api/v1.0/timewatch-punch"

        projectId =
          case model.selectedProject of
            Just val ->
              toString val

            Nothing ->
              ""
      in
        ( { model | status = Fetching }
        , getJson url Config.accessToken model.pincode projectId
        )

    Tick ->
      let
        effects =
          if model.tickStatus == Ready then
            Effects.batch [ getDate, (tick 1 Tick) ]
          else
            Effects.none
      in
        ( { model | tickStatus = Waiting }
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
                Nothing ->
                  Enter

                -- When the end date exist, it means the session was closed.
                Just int ->
                  Leave

            greeting =
              if operation == Enter then
                "Hi"
              else
                "Bye"

            message =
              greeting ++ " " ++ response.employee
          in
            ( { model
                | status = Fetched operation
                , pincode = ""
                , selectedProject = Nothing
              }
            , Task.succeed (SetMessage (Success message)) |> Effects.task
            )

        Err error ->
          let
            message =
              getErrorMessageFromHttpResponse error
          in
            ( { model
                | status = HttpError error
                , pincode = ""
              }
            , Task.succeed (SetMessage <| Error message) |> Effects.task
            )

    UnsetPressedButton ->
      ( { model | pressedButton = Nothing }
      , Effects.none
      )


setTimeOut : Float -> Action -> Effects Action
setTimeOut milliseconds action =
  Task.sleep milliseconds
    |> Task.map (\_ -> action)
    |> Effects.task


getErrorMessageFromHttpResponse : Http.Error -> String
getErrorMessageFromHttpResponse error =
  case error of
    Http.Timeout ->
      "Connection has timed out"

    Http.BadResponse code message ->
      -- TODO: Print the error's title
      if code == 400 then
        "Wrong pincode"
      else if code == 401 then
        "Invalid access token"
      else if code == 429 then
        "Too many login requests with the wrong username or password. Wait a few hours before trying again"
      else if code >= 500 then
        "Some error has occurred on the server"
      else
        "Unknown error has occurred"

    Http.NetworkError ->
      -- "A network error has occured"
      toString error

    Http.UnexpectedPayload message ->
      "Unexpected response: " ++ message


isButtonPressed : Int -> Maybe Int -> Bool
isButtonPressed id pressedButoon =
  case pressedButoon of
    Just val ->
      id == val

    Nothing ->
      False


clickEvent : Bool -> ( String, String )
clickEvent isTouchDevice =
  if isTouchDevice then
    ( "touchstart", "touchend" )
  else
    ( "mousedown", "mouseup" )



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  let
    ledLight =
      div
        [ class "col-xs-2 main-header led text-center" ]
        [ span [ class "light -on" ] [] ]

    pincodeText delta =
      let
        text' =
          String.slice delta (delta + 1) model.pincode
      in
        div [ class "item pin" ] [ text text' ]

    icon =
      let
        className =
          case model.status of
            Init ->
              ""

            InProgress ->
              ""

            Fetching ->
              "fa-circle-o-notch fa-spin"

            Fetched Enter ->
              "fa-check -success -in"

            Fetched Leave ->
              "fa-check -success -out"

            HttpError error ->
              "fa-exclamation-triangle -error"
      in
        i [ class <| "fa " ++ className ] []

    pincode =
      div
        [ class "col-xs-5 main-header pin-code text-center" ]
        [ div
            [ class "code clearfix" ]
            [ div [ class "item icon fa fa-lock" ] []
            , span [] (List.map pincodeText [0..3])
            , div [ class "item icon -dynamic-icon" ] [ icon ]
            ]
        ]

    clockIcon =
      i [ class "fa fa-clock-o icon" ] []

    dateString =
      case model.date of
        Just time ->
          Date.fromTime time |> DF.format "%A, %d %B, %Y"

        Nothing ->
          ""

    timeString =
      case model.date of
        Just time ->
          Date.fromTime time |> DF.format " %H:%M"

        Nothing ->
          ""

    date =
      div
        [ class "col-xs-5 main-header info text-center" ]
        [ span [] [ text dateString ]
        , span
            [ class "time" ]
            [ clockIcon
            , span [] [ text timeString ]
            ]
        ]

    message =
      let
        -- Adding a "class" to toggle the view display (hide/show).
        visibilityClass =
          if model.status == Init then
            ""
          else if model.status == InProgress then
            ""
          else if model.status == Fetching then
            ""
          else
            "-active"

        msgClass =
          case model.status of
            Fetched Enter ->
              "-success -in"

            Fetched Leave ->
              "-success -out"

            HttpError error ->
              "-error"

            _ ->
              ""

        msgIcon =
          case model.status of
            HttpError error ->
              i [ class "fa icon fa-exclamation-triangle" ] []

            Init ->
              i [] []

            InProgress ->
              i [] []

            _ ->
              i [ class "fa icon fa-check" ] []

        msgText =
          case model.message of
            Error msg ->
              msg

            Success msg ->
              msg

            _ ->
              ""

        actionIcon =
          let
            baseClass =
              "symbol fa-4x fa fa-sign"
          in
            case model.status of
              Fetched Enter ->
                i [ class <| baseClass ++ "-in" ] []

              Fetched Leave ->
                i [ class <| baseClass ++ "-out" ] []

              _ ->
                i [] []
      in
        div
          [ class "col-xs-7 view" ]
          [ div
              [ class <| "main " ++ visibilityClass ]
              [ div
                  [ class "wrapper" ]
                  [ div
                      [ class <| "message " ++ msgClass ]
                      [ span [] [ msgIcon, text msgText ] ]
                  ]
              , div [ class "text-center" ] [ actionIcon ]
              ]
          ]

    projectsButtons : Project -> Html
    projectsButtons project =
      let
        className =
          [ ( "-with-icon clear-btn project", True )
          , ( "-active", isButtonPressed project.id model.selectedProject )
          ]

        ( clickStartEvent, clickEndEvent ) =
          clickEvent model.isTouchDevice
      in
        button
          [ classList className
          , on clickStartEvent Json.value (\_ -> Signal.message address (SetProject project.id))
          ]
          [ i [ class "fa fa-server icon" ] []
          , text <| " " ++ project.label
          ]

    projects =
      span [] (List.map projectsButtons model.projects)

    digitButton digit =
      let
        className =
          [ ( "clear-btn digit", True )
          , ( "-double", digit == 0 )
          , ( "-active", isButtonPressed digit model.pressedButton )
          ]

        disable =
          if model.status == Fetching then
            True
          else
            False

        action digit =
          if disable then
            NoOp
          else
            AddDigit digit

        ( clickStartEvent, clickEndEvent ) =
          clickEvent model.isTouchDevice
      in
        button
          [ classList className
          , on clickStartEvent Json.value (\_ -> Signal.message address (action digit))
          , on clickEndEvent Json.value (\_ -> Signal.message address UnsetPressedButton)
          , disabled disable
          ]
          [ text <| toString digit ]

    deleteButton =
      let
        className =
          [ ( "clear-btn -delete", True )
          , ( "-active", isButtonPressed -1 model.pressedButton )
          ]

        disable =
          if (length model.pincode == 0 || model.status == Fetching) then
            True
          else
            False

        action =
          if disable then
            NoOp
          else
            DeleteDigit

        ( clickStartEvent, clickEndEvent ) =
          clickEvent model.isTouchDevice
      in
        button
          [ classList className
          , on clickStartEvent Json.value (\_ -> Signal.message address action)
          , on clickEndEvent Json.value (\_ -> Signal.message address UnsetPressedButton)
          , disabled disable
          ]
          [ i [ class "fa fa-long-arrow-left" ] [] ]

    padButtons =
      div
        [ class "numbers-pad" ]
        [ span [] (List.map digitButton [0..9] |> List.reverse)
        , deleteButton
        ]

    debugBlock =
      div
        [ class "model-debug", hidden (not Config.debugMode) ]
        [ text <| toString model
        , (viewMessage model.message)
        ]
  in
    div
      [ class "container" ]
      [ div
          [ class "row dashboard" ]
          [ pincode
          , date
          , ledLight
          , div
              [ class "col-xs-5 text-center" ]
              [ projects, padButtons ]
          , message
          ]
      , debugBlock
      ]


viewMessage : Message -> Html
viewMessage message =
  let
    ( className, string ) =
      case message of
        Empty ->
          ( "", "" )

        Error msg ->
          ( "error", msg )

        Success msg ->
          ( "success", msg )
  in
    div [ id "status-message", class className ] [ text string ]



-- EFFECTS


getJson : String -> String -> String -> String -> Effects Action
getJson url accessToken pincode projectId =
  Http.send
    Http.defaultSettings
    { verb = "POST"
    , headers = [ ( "access-token", accessToken ) ]
    , url = url
    , body = (Http.string <| dataToJson pincode projectId)
    }
    |> Http.fromJson decodeResponse
    |> Task.toResult
    |> Task.map UpdateDataFromServer
    |> Effects.task


dataToJson : String -> String -> String
dataToJson code projectId =
  JE.encode 0
    <| JE.object
        [ ( "pincode", JE.string code )
        , ( "project", JE.string projectId )
        ]


decodeResponse : Json.Decoder Response
decodeResponse =
  Json.at [ "data" ]
    <| Json.object4
        Response
        ("employee" := Json.string)
        ("start" := Json.int)
        (Json.maybe ("end" := Json.int))
        (Json.maybe ("project" := Json.string))


getDate : Effects Action
getDate =
  Task.map SetDate getCurrentTime |> Effects.task


tick : Float -> Action -> Effects Action
tick sec action =
  Task.sleep (sec * Time.second)
    |> Task.map (\_ -> action)
    |> Effects.task
