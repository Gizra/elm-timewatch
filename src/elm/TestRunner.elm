module Main where

import Graphics.Element exposing (Element)

import ElmTest.Test exposing (Test, suite)
import ElmTest.Runner.Element exposing (runDisplay)

import AppTest

allTests : Test
allTests =
  suite "All tests"
    [ AppTest.all
    ]

main : Element
main =
  runDisplay allTests
