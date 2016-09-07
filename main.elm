port module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Http 
import Json.Decode exposing (..)
import Task
import Array
import String


main =
  App.programWithFlags { init = init, view = view, subscriptions = subscriptions, update = update }


-- Functions

getSessionStats : String -> Cmd Msg
getSessionStats playerId =
  let
    url =
      "https://api.redmetrics.io/v1/event?game=67475d78-09af-4f23-95fa-45a700f08057&type=statistics&player=" ++ playerId
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeStatistics url)

decodeStatistics : Decoder (List Statistics)
decodeStatistics =
  list ("customData" := (object6 Statistics 
    ("foundShapeCount" := int) 
    ("newShapeCount" := int)
    ("categoryCount" := int)
    ("meanCreated" := int)
    ("beautifulPercent" := int)
    ("foundPopularShape" := bool)
   )) 

getPlayerId : String -> Maybe String
getPlayerId path = 
  let 
    a = Array.fromList (String.split "/" path)
    l = Array.length a
  in 
    if l /= 2 then
      Nothing
    else 
      Array.get 1 a


-- MODEL

type alias Statistics =
  {
    foundShapeCount : Int
  , newShapeCount: Int
  , categoryCount: Int
  , meanCreated: Int
  , beautifulPercent: Int
  , foundPopularShape: Bool
  }

type alias Model = 
  { playerId: Maybe String
  , statistics: Maybe Statistics
  }


init : { path: Maybe String } -> (Model, Cmd Msg)
init flags = 
  case flags.path of
    Just path -> 
      let 
        playerId = getPlayerId path
      in 
        case playerId of
          Just playerId -> ({ playerId = Just playerId, statistics = Nothing }, getSessionStats playerId)
          Nothing -> ({ playerId = Nothing, statistics = Nothing }, Cmd.none)
    Nothing -> ({ playerId = Nothing, statistics = Nothing }, Cmd.none)


-- UPDATE

type Msg = FetchSucceed (List Statistics)
  | FetchFail Http.Error 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchSucceed value -> 
      case (List.head value) of 
        Nothing -> (model, Cmd.none)
        Just statistics -> ({ model | statistics = Just statistics }, Cmd.none)
    FetchFail _ -> (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div []
  [
    h1 [] [text "Creative Leaps - Feedback"]
  , case model.statistics of 
      Just statistics -> p [] [text ("You found " ++ (toString statistics.foundShapeCount) ++ " shapes")]
      Nothing -> p [] [text ("Can't find any statistics for you")]
  ]

