{-# LANGUAGE OverloadedStrings #-}

module Main where

import Dicewarist (dicewarist)
import Graphics.UI.TinyFileDialogs
  ( IconType (Info),
    OK (..),
    messageBox,
  )

main :: IO ()
main = do
  initialize
  dicewarist
  pure ()

initialize :: IO OK
initialize =
  messageBox
    "Dicewarist"
    "Welcome to Dicewarist. This application, to a \
    \wordlist specified purely with one word per line, appends Diceware numbers for a \
    \selected number of dice. The application will ask you to select a pre-formatted \
    \file, input a number of dice, and select a file to output to."
    Info
    OK