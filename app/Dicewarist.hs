{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Dicewarist where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Data.List (iterate')
import Data.Text
  ( Text,
    cons,
    empty,
    filter,
    lines,
    unpack,
    unwords,
  )
import Data.Text.IO (putStrLn, readFile, writeFile)
import GHC.Settings.Utils (maybeRead)
import Graphics.UI.TinyFileDialogs
  ( IconType (Error, Info),
    OK (OK),
    inputBox,
    messageBox,
    notifyPopup,
    openFileDialog,
    saveFileDialog,
  )
import Prelude hiding (filter, lines, readFile, unwords, writeFile)

-- Some general type synonyms.

type NumberOfDice = Int

type InputFile = Text

type OutputPath = FilePath

data Stores = MkStore !NumberOfDice !InputFile !OutputPath

-- names using InvalidMessage to pass data to functions, heavy in Texts
-- InvalidMessage is a type used specifically to package data.

data InvalidMessage a = IM
  { messageTitle :: Text,
    messageContent :: Text,
    errorMessage :: Text,
    responseFunction :: IO (Maybe a)
  }

diceMessage :: InvalidMessage Text
diceMessage =
  IM
    "Dice Selection Error"
    "You did not input a valid entry. Choose up to 20, or \
    \press cancel to abort."
    "Aborted on dice selection."
    numberOfDicePrompt

sourceMessage :: InvalidMessage [Text]
sourceMessage =
  IM
    "Error on Selecting Source File"
    "You did not select a file. Try again, or \
    \hit cancel to abort."
    "Aborted on source file selection."
    fileSelectorPrompt

outputPathMessage :: InvalidMessage Text
outputPathMessage =
  IM
    "Error on Selecting Output File"
    "You did not select or create a file. Try \
    \again, or hit cancel to abort."
    "Aborted on output file selection."
    outputFilePrompt

-- Common or reusable functions.

errorMessageBox :: Text -> IO a
errorMessageBox message =
  notifyPopup "Program Aborted" message Error
    >>= error (unpack message)

-- Functions closely tied up with message prompts, mainly the prompts and
-- some basic error-handling code.

numberOfDicePrompt :: IO (Maybe Text)
numberOfDicePrompt =
  (fmap . fmap) removeCarriageReturns
    $ inputBox
      "Number of Dice?"
      "How many dice would you like in \
      \your diceware annotations? Choose between 1 and 20."
    $ Just "Input your number here."
  where
    removeCarriageReturns :: Text -> Text
    removeCarriageReturns = filter (`notElem` ("\n\r" :: String))

fileSelectorPrompt :: IO (Maybe [Text])
fileSelectorPrompt =
  openFileDialog "Select a File to Read" "" ["*.txt"] "" False

outputFilePrompt :: IO (Maybe Text)
outputFilePrompt =
  saveFileDialog "Select or Create a File to Output to" "" ["*.txt"] ""

invalidMessage :: InvalidMessage a -> (Maybe a -> IO b) -> IO b
invalidMessage IM {..} callback = do
  messageBox messageTitle messageContent Info OK
  responseFunction >>= \case
    Nothing -> errorMessageBox errorMessage
    u -> callback u

-- Main program.

dicewarist :: IO ()
dicewarist = inputs >>= runReaderT process

inputs :: IO Stores
inputs = MkStore <$> getNumberOfDice <*> getSourceFile <*> getOutputPath
  where
    getNumberOfDice :: IO NumberOfDice
    getNumberOfDice = numberOfDicePrompt >>= sanitizeNumberOfDice
      where
        sanitizeNumberOfDice :: Maybe Text -> IO NumberOfDice
        sanitizeNumberOfDice (Just text)
          | Just diceNumber <- maybeRead (unpack text) :: Maybe Int,
            diceNumber <= 20 && diceNumber > 0 =
              pure diceNumber
        sanitizeNumberOfDice Nothing = invalidMessage diceMessage sanitizeNumberOfDice

    getSourceFile :: IO InputFile
    getSourceFile = fileSelectorPrompt >>= sanitizeGetSourceFile >>= readFile
      where
        sanitizeGetSourceFile :: Maybe [Text] -> IO FilePath
        sanitizeGetSourceFile (Just [filePath]) = pure $ unpack filePath
        sanitizeGetSourceFile Nothing = invalidMessage sourceMessage sanitizeGetSourceFile

    getOutputPath :: IO FilePath
    getOutputPath = outputFilePrompt >>= sanitizeGetOutputPath
      where
        sanitizeGetOutputPath :: Maybe Text -> IO FilePath
        sanitizeGetOutputPath (Just filePath) = pure $ unpack filePath
        sanitizeGetOutputPath Nothing = invalidMessage outputPathMessage sanitizeGetOutputPath

process :: ReaderT Stores IO ()
process = do
  MkStore _ outputFile outputPath <- asks processInner
  lift $ writeFile outputPath outputFile
  where
    processInner :: Stores -> Stores
    processInner (MkStore numberOfDice inputFile outputPath) =
      MkStore
        numberOfDice
        (unwords . appendNumbers numberOfDice . lines $ inputFile)
        outputPath

    appendNumbers :: NumberOfDice -> [Text] -> [Text]
    appendNumbers numberOfDice inputFile =
      zipWith (\a b -> a <> ":" <> b) inputFile $
        iterate' (addAnotherDice <*>) [empty] !! numberOfDice
      where
        addAnotherDice :: [Text -> Text]
        addAnotherDice = cons <$> ['1' .. '6']
