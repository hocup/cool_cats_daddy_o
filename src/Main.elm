import Html exposing (Html, button, div, img, text)
import Browser
import Html.Attributes exposing(..)
import Html.Events exposing (onClick)
import List exposing (map)
import Http
import Random exposing (..)
import Joke exposing (..)
import Filter exposing (..)

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
  , jokesByWordCount : List Joke
  , sortType : SortOrder
  , filter : Filter.Model
  }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 0 [] [] None Filter.init, getPage)


getJokesList : Model -> List Joke
getJokesList model = 
  let 
    applyFilter = (\ joke -> {joke | message = (Filter.filterText model.filter joke.message) })
  in
    case model.sortType of
      None ->
        List.map applyFilter model.jokes
      Asc ->
        List.map applyFilter model.jokesByWordCount
      Desc ->
        List.map applyFilter (List.reverse model.jokesByWordCount)

-- UPDATE

type Msg = FilterMsg Filter.Msg | ReceiveJokes JokeReceiver 
         | ReceivePage Int | SwapSort

type SortOrder = None | Asc | Desc

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceiveJokes (Ok newJokes) ->
      let 
        newList = List.concat [newJokes, model.jokes]
        newListSorted = List.sortBy .wordCount newList
      in
        ({ model | jokes = newList, jokesByWordCount = newListSorted}, Cmd.none)
    ReceiveJokes (Err _) ->
      (model, Cmd.none)
    ReceivePage page ->
      let updatedModel = { model | page = page } 
      in (updatedModel, getJokes page)
    SwapSort ->
      case model.sortType of
        None ->
          ({model | sortType = Asc}, Cmd.none)
        Asc -> 
          ({model | sortType = Desc}, Cmd.none)
        Desc ->
          ({model | sortType = None}, Cmd.none)
    FilterMsg subMsg ->
      let
        (updatedFilterModel, filterCmd) =
          Filter.update subMsg model.filter
      in
        ({ model | filter = updatedFilterModel }, Cmd.map FilterMsg filterCmd)

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
      div [style "display" "inline-block"] [Html.map FilterMsg (Filter.getView model.filter)],
      div [style "float" "right", style "display" "inline-block"] [
        button [ onClick SwapSort] [text (getSortButtonText model)]
      ]
    ],
    div [] (List.map renderJoke (getJokesList model))
  ]
  

getSortButtonText : Model -> String
getSortButtonText model = 
  case model.sortType of
    None ->
      "Sort By Word Count: None"
    Asc -> 
      "Sort By Word Count: Ascending"
    Desc ->
      "Sort By Word Count: Descending"

getPage : Cmd Msg
getPage =
  Random.generate ReceivePage (Random.int 0 totalJokePages)

getJokes : Int -> Cmd Msg
getJokes page = jokeRequest page ReceiveJokes
