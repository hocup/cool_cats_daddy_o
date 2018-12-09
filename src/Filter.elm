module Filter exposing (..)

import Html exposing (Html, button, div, img, text, input, label)
import Html.Attributes exposing (type_, checked, value, name, for, id)
import Html.Events exposing (onInput, onCheck, onClick)

-- MODEL
type alias Model = 
    { on: Bool
    , showList: Bool
    , filterPairs: List (String, String)
    }

init =
    { on = False, showList = False, filterPairs = [("What", "WHAT!!!"), ("Why", "WHY?!?!")] }


filterText : Model -> String -> String
filterText model text =
    if model.on then
        let 
            doReplace toReplace (from, to) =
                String.replace from to toReplace
            doFilterText toFilter pairs =
                let 
                    nextPair = List.head pairs
                in
                    case nextPair of
                        Nothing ->
                            toFilter
                        Just pair ->
                            doFilterText (doReplace toFilter pair) (List.drop 1 pairs)
        in
            doFilterText text model.filterPairs
    else
        text

-- UPDATE
type Msg = ToggleEnabled Bool | AddFilterPair (String, String)
    | ToggleShowList

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ToggleShowList -> 
            ({model | showList = not model.showList}, Cmd.none)
        ToggleEnabled checked ->
            ({model | on = checked}, Cmd.none)
        AddFilterPair pair ->
            (model, Cmd.none)
        

-- VIEW
getView : Model -> Html Msg
getView model =
    div [] [
        label [for "filter-enabled"] [text "Filter Enabled"]
        , input [type_ "checkbox", id "filter-enabled", onCheck ToggleEnabled][]
        ,if model.showList then
            div [] [
                button [onClick ToggleShowList] [text "Hide List"],
                div [] (List.map getWordPairView model.filterPairs)
            ]
        else 
            div [][
                button [onClick ToggleShowList] [text "Show List"]
            ]
    ]

getWordPairView : (String, String) -> Html Msg
getWordPairView (strA, strB) =
    div [] [text strA, text " -> " ,text strB]