module Main where

import Signal exposing (Signal)

import ElmTest exposing (consoleRunner)
import Console exposing (IO, run)
import Task
import ObjectTests


console : IO ()
console = consoleRunner ObjectTests.all


port runner : Signal (Task.Task x ())
port runner = run console
