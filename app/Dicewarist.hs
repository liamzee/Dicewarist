{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Dicewarist where

import Data.Function ((&))
import Data.List (iterate')
import Data.Char (intToDigit)
import Data.Text hiding (zipWith)
import Data.Text.IO ( readFile , writeFile, putStrLn )
import Prelude hiding (init, filter, putStrLn, writeFile , unwords , readFile , lines )
import Data.Functor ( ( <&> ) )
import Data.Maybe (fromMaybe)
import Graphics.UI.TinyFileDialogs
import GHC.Plugins (HasCallStack)
import GHC.IO (unsafePerformIO)
import GHC.Settings.Utils (maybeRead)
import qualified System.IO

type NumberOfDice = Int
type InputFile = Text
type OutputFile = Text

-- Common or reusable functions.

putBetweenSemigroups :: Semigroup a => a -> a -> a -> a
putBetweenSemigroups itemInBetween = ( <> ) . ( <> itemInBetween)


{-# NOINLINE errorMessageBox #-}
errorMessageBox :: forall b. HasCallStack => Text -> b
errorMessageBox message =
    unsafePerformIO $ notifyPopup "Program Aborted" message Error >>=
    (error ( unpack message ) )

-- Functions closely tied up with message prompts.

numberOfDicePrompt :: IO ( Maybe Text )
numberOfDicePrompt = 

    removeCarriageReturn $ inputBox "Number of Dice?" "How many dice would you like in \
    \your diceware annotations? Choose at least 1, up to 20." $ Just "Input your number here."

    where
    
    removeCarriageReturn :: IO (Maybe Text) -> IO (Maybe Text)
    removeCarriageReturn = fmap.fmap $ filter (\u -> u /= '\n' && u /= '\r') 

fileSelectorPrompt :: IO ( Maybe [ Text ] )
fileSelectorPrompt = openFileDialog "Select a File to Read" "" ["*.txt"] "" False

outputFilePrompt :: IO ( Maybe Text )
outputFilePrompt = saveFileDialog "Select or Create a File to Output to" "" ["*.txt"] ""

invalidMessage :: ( Text , Text , Text , IO ( Maybe a ) ) -> ( Maybe a -> IO b ) -> IO b
invalidMessage ( messageTitle , messageContent , errorMessage , responseFunction ) callback = 

    messageBox messageTitle messageContent Info OK >> responseFunction >>=
    pure . pure . fromMaybe (errorMessageBox errorMessage) >>= callback

diceMessage :: ( Text , Text , Text , IO ( Maybe Text ) )
diceMessage = ( "Dice Selection Error" , "You did not input a valid entry. Choose up to 20, or \
    \press cancel to abort." , "Aborted on dice selection." , numberOfDicePrompt )

sourceMessage :: ( Text , Text , Text , IO ( Maybe [ Text ] ) )
sourceMessage = ( "Error on Selecting Source File" , "You did not select a file. Try again, or \
    \hit cancel to abort." , "Aborted on source file selection." , fileSelectorPrompt )

outputPathMessage :: ( Text , Text , Text , IO ( Maybe Text ) )
outputPathMessage = ( "Error on Selecting Output File" , "You did not select or create a file. Try \
    \again, or hit cancel to abort." , "Aborted on output file selection." , outputFilePrompt )

-- Main program.

dicewarist :: IO ()
dicewarist = inputs <&> process >>= writeToFile

    where

    inputs :: IO ( NumberOfDice , InputFile , FilePath )
    inputs = (,,) <$> getNumberOfDice <*> getSourceFile <*> getOutputPath

    getNumberOfDice :: IO NumberOfDice
    getNumberOfDice = numberOfDicePrompt >>= sanitizeNumberOfDice

    sanitizeNumberOfDice :: Maybe Text -> IO NumberOfDice
    sanitizeNumberOfDice (Just text) 
        | Just diceNumber <- maybeRead (unpack text) :: Maybe Int,
          diceNumber <= 20 || diceNumber > 0 = pure diceNumber
    sanitizeNumberOfDice _ = invalidMessage diceMessage sanitizeNumberOfDice

            
    getSourceFile :: IO InputFile
    getSourceFile = fileSelectorPrompt >>= sanitizeGetSourceFile >>= readFile

    sanitizeGetSourceFile :: Maybe [ Text ] -> IO FilePath
    sanitizeGetSourceFile (Just (filePath:[])) = pure $ unpack filePath
    sanitizeGetSourceFile _ = invalidMessage sourceMessage sanitizeGetSourceFile

    getOutputPath :: IO FilePath
    getOutputPath = outputFilePrompt >>= sanitizeGetOutputPath

    sanitizeGetOutputPath :: Maybe Text -> IO FilePath
    sanitizeGetOutputPath (Just filePath) = pure $ unpack filePath
    sanitizeGetOutputPath _ = invalidMessage outputPathMessage sanitizeGetOutputPath

    process :: ( NumberOfDice , InputFile , FilePath ) -> ( OutputFile , FilePath )
    process ( numberOfDice , inputFile , filePath) = ( outputFile , filePath )

        where

                outputFile = unwords . appendNumbers numberOfDice . lines $ inputFile

    appendNumbers :: NumberOfDice -> [Text] -> [Text]
    appendNumbers numberOfDice inputFile =

        zipWith (putBetweenSemigroups ":") inputFile $

        ( iterate' ( addAnotherDice <*> ) [empty] ) !! numberOfDice

        where

            addAnotherDice = cons . intToDigit <$> [ 1 .. 6 ]

    writeToFile :: ( OutputFile , FilePath ) -> IO ()
    writeToFile ( outputFile , filePath ) = writeFile filePath outputFile