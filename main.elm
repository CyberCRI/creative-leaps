import Html exposing (..)
import Html.App as App
import Http 
import Json.Decode exposing (..)
import Task

main =
  App.program { init = init, view = view, subscriptions = subscriptions, update = update }


-- Functions

getSessionStats : String -> Cmd Msg
getSessionStats playerId =
  let
    url =
      "https://api.redmetrics.io/v1/event?game=67475d78-09af-4f23-95fa-45a700f08057&type=statistics&player=" ++ playerId
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeStatistics url)

decodeStatistics : Decoder (List Int)
decodeStatistics =
  list ("customData" := ("foundShapeCount" := int)) 


-- MODEL

type alias Model = 
  { foundShapeCount : Int
  }


init : (Model, Cmd Msg)
init = 
  ({ foundShapeCount = 2 }, getSessionStats "d5625a22-f1dd-4887-bbee-c9bdcde9597b")


-- UPDATE

type Msg = FetchSucceed (List Int)
  | FetchFail Http.Error 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchSucceed value -> 
      case (List.head value) of 
        Nothing -> (model, Cmd.none)
        Just x -> ({ model | foundShapeCount = x }, Cmd.none)
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
  , p [] [text "Hello, World from Elm!"]
  , p [] [text ("You found " ++ (toString model.foundShapeCount) ++ " shapes")]
  ]

