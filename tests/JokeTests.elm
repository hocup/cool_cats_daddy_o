module JokeTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Joke exposing (..)


suite : Test
suite =
  describe "The Joke module"
    [ describe "imageIdFromMessage" -- Nest as many descriptions as you like.
      [ test "builds avatar id from message length" <|
        \_ ->
          let
            id1 = imageIdFromMessage (String.repeat 50 "x") -- mod 16 = 2
            id2 = imageIdFromMessage (String.repeat 53 "x") -- mod 16 = 5
          in
            Expect.equal [2, 5] [id1, id2]
      ]
      ,describe "wordCount"
      [ test "builds avatar id from message length" <|
        \_ ->
          let
            count1 = wordCount (String.repeat 50 "x")
            count2 = wordCount "sentence with four words"
            count3 = wordCount "sentence with, four words"
            count4 = wordCount "sentence with  : four words !"
          in
            Expect.equal [1, 4, 4, 4] [count1, count2, count3, count4]
      ]
    ]
