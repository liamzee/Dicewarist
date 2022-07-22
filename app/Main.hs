{-# LANGUAGE OverloadedStrings #-}

module Main where

import Dicewarist ( dicewarist )
import Graphics.UI.TinyFileDialogs
import Control.Monad.Trans.State ( StateT(..) )

main :: IO ()
main = do
    initialize
    (runStateT dicewarist) (0, "", "")
    pure ()

initialize :: IO OK
initialize = messageBox "Dicewarist" "Welcome to Dicewarist. This application, to a \
    \wordlist specified purely with one word per line, appends Diceware numbers for a \
    \selected number of dice. The application will ask you to select a pre-formatted \
    \file, input a number of dice, and select a file to output to." Info OK