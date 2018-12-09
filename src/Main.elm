import Html exposing (Html, button, div, img, text)
import Browser
import Html.Attributes exposing(..)
import List exposing (map)
import Http
import Random exposing (..)
import Joke exposing (..)

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions 
    }

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- MODEL

type alias Model = 
  { page : Int
  , jokes : List Joke
  }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 0 [], getPage)

-- UPDATE

type Msg = ReceiveJokes JokeReceiver 
         | ReceivePage Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceiveJokes (Ok newJokes) ->
      ({ model | jokes = List.concat [newJokes, model.jokes] }, Cmd.none)
    ReceiveJokes (Err _) ->
      (model, Cmd.none)
    ReceivePage page ->
      let updatedModel = { model | page = page } 
      in (updatedModel, getJokes page)

-- VIEW
itemStyle : List (Html.Attribute msg)
itemStyle = 
  [ style "display" "inline-block" 
  , style "margin-left" "1vw"
  ]

renderJoke : Joke -> Html Msg
renderJoke joke =
  div [ style "padding" "1vw"
      , style "margin" "1vw"
      , style "border"  "1px solid black"
  ] [
    img (itemStyle ++ [src joke.avatarUrl, width 50, height 50]) [text(joke.avatarUrl)],
    div (itemStyle ++ [style "font-style" "italic"]) [text(joke.id)],
    div (itemStyle ++ [style "font-weight" "bold"]) [text(joke.message)],
    div (itemStyle ++ [style "font-style" "italic", style "float" "right"]) [ text("(" ++ (String.fromInt joke.wordCount) ++ " words)")]
  ]

view : Model -> Html Msg
view model =
  div [] [
    div itemStyle [
      div [style "display" "inline-block"] [text "future home of the filter stuff"],
      div [style "float" "right", style "display" "inline-block"] [text "future home of the sort btn"]
    ],
    div [] (List.map renderJoke model.jokes)
  ]
  

getPage : Cmd Msg
getPage =
  Random.generate ReceivePage (Random.int 0 totalJokePages)

getJokes : Int -> Cmd Msg
getJokes page = jokeRequest page ReceiveJokes
