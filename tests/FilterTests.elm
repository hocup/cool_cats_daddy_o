module FilterTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Filter exposing (..)

suite : Test
suite =
    describe "The Filter module" 
        [ describe "filterText" 
            [ test "changes words in a string, when on" <|
                \_ -> 
                    let
                        testModel = 
                            {
                                on = True,
                                showList = False,
                                filterPairs = [("test", "REPLACED"), ("potato", "yam")],
                                newFrom = "",
                                newTo = ""
                            }
                    in
                        Expect.equal [
                            (Filter.filterText testModel "this is a test")
                            , (Filter.filterText testModel "this is a test potato") 
                            ] ["this is a REPLACED", "this is a REPLACED yam"]
                , test "does not change words when not on" <|
                    \_ ->
                        let
                            testModel =
                                {
                                    on = False,
                                    showList = False,
                                    filterPairs = [("test", "REPLACED"), ("potato", "yam")],
                                    newFrom = "",
                                    newTo = ""
                                }
                        in
                            Expect.equal (Filter.filterText testModel "this is a test") "this is a test"
            ]
        ]