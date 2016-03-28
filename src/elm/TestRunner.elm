module Main (..) where

import Graphics.Element exposing (Element)
import ElmTest exposing (..)
import AppTest


allTests : Test
allTests =
  suite
    "All tests"
    [ AppTest.all
    ]


main : Element
main =
  elementRunner allTests
