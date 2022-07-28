{-# LANGUAGE LambdaCase, OverloadedStrings, NamedFieldPuns, RecordWildCards #-}

module Dicewarist where

import Data.List ( iterate' )
import Data.Text
    ( Text , cons , filter , lines , unwords , empty , unpack )
import Data.Text.IO ( readFile , writeFile , putStrLn )
import Prelude hiding ( filter , writeFile , unwords , readFile , lines )
import Data.Maybe ( fromMaybe )
import Graphics.UI.TinyFileDialogs
    ( inputBox ,
      messageBox ,
      notifyPopup ,
      openFileDialog ,
      saveFileDialog ,
      IconType ( Info , Error ) ,
      OK ( OK ) )
import GHC.Plugins ( HasCallStack )
import GHC.IO ( unsafePerformIO )
import GHC.Settings.Utils ( maybeRead )
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Control.Monad.Trans (lift)
import Data.Functor ((<&>))

type NumberOfDice = Int
type InputFile = Text
type OutputPath = FilePath
type Stores = ( NumberOfDice , InputFile , OutputPath )

data InvalidMessage a =
    
    IM

    { messageTitle :: Text
    , messageContent :: Text
    , errorMessage :: Text
    , responseFunction :: IO ( Maybe a )
    }

-- names using InvalidMessage to pass data to functions, heavy in Texts

diceMessage :: InvalidMessage Text
diceMessage = 
    
    IM "Dice Selection Error" "You did not input a valid entry. Choose up to 20, or \
    \press cancel to abort." "Aborted on dice selection." numberOfDicePrompt

sourceMessage :: InvalidMessage [Text]
sourceMessage = 
    
    IM "Error on Selecting Source File" "You did not select a file. Try again, or \
    \hit cancel to abort." "Aborted on source file selection." fileSelectorPrompt

outputPathMessage :: InvalidMessage Text
outputPathMessage = 
    
    IM "Error on Selecting Output File" "You did not select or create a file. Try \
    \again, or hit cancel to abort." "Aborted on output file selection." outputFilePrompt

-- Common or reusable functions.

putBetweenSemigroups :: Semigroup a => a -> a -> a -> a
putBetweenSemigroups itemInBetween = ( <> ) . ( <> itemInBetween )


{-# NOINLINE errorMessageBox #-}
errorMessageBox :: forall b. HasCallStack => Text -> b
errorMessageBox message =

    unsafePerformIO $ notifyPopup "Program Aborted" message Error >>=
    (error ( unpack message ) )

-- Functions closely tied up with message prompts, mainly the prompts and
-- some basic error-handling code.



removeCarriageReturns ::  Text -> Text 
removeCarriageReturns = filter ( \u -> u /= '\n' && u /= '\r' )

numberOfDicePrompt :: IO ( Maybe Text )
numberOfDicePrompt = 

    ((removeCarriageReturns <$>) <$>) $ inputBox "Number of Dice?" "How many dice would you like in \
    \your diceware annotations? Choose at least 1, up to 20." $ Just "Input your number here."

  where
    

fileSelectorPrompt :: IO ( Maybe [ Text ] )
fileSelectorPrompt = openFileDialog "Select a File to Read" "" ["*.txt"] "" False

outputFilePrompt :: IO ( Maybe Text )
outputFilePrompt = saveFileDialog "Select or Create a File to Output to" "" ["*.txt"] ""

invalidMessage :: InvalidMessage a -> ( Maybe a -> IO b ) -> IO b
invalidMessage IM { .. } callback =

    messageBox messageTitle messageContent Info OK >>
    responseFunction >>= callback
    <$> pure . fromMaybe (errorMessageBox errorMessage)

-- Main program.

dicewarist :: IO ()
dicewarist = runReaderT process =<< inputs

inputs :: IO Stores
inputs = ((,,) <$> getNumberOfDice <*> getSourceFile <*> getOutputPath)

  where

    getNumberOfDice :: IO NumberOfDice
    getNumberOfDice = numberOfDicePrompt >>= sanitizeNumberOfDice
         
      where 

        sanitizeNumberOfDice :: Maybe Text -> IO NumberOfDice
        sanitizeNumberOfDice (Just text) 

            | Just diceNumber <- maybeRead (unpack text) :: Maybe Int,
              diceNumber <= 20 && diceNumber > 0 = pure diceNumber

        sanitizeNumberOfDice _ = invalidMessage diceMessage sanitizeNumberOfDice

    getSourceFile :: IO InputFile
    getSourceFile = fileSelectorPrompt >>= sanitizeGetSourceFile >>= readFile

      where

        sanitizeGetSourceFile :: Maybe [ Text ] -> IO FilePath
        sanitizeGetSourceFile (Just (filePath:[])) = pure $ unpack filePath
        sanitizeGetSourceFile _ = invalidMessage sourceMessage sanitizeGetSourceFile

    getOutputPath :: IO FilePath
    getOutputPath = outputFilePrompt >>= sanitizeGetOutputPath

      where

        sanitizeGetOutputPath :: Maybe Text -> IO FilePath
        sanitizeGetOutputPath (Just filePath) = pure $ unpack filePath
        sanitizeGetOutputPath _ = invalidMessage outputPathMessage sanitizeGetOutputPath

process :: ReaderT Stores IO ()
process = do

    ( numberOfDice , outputFile , outputPath ) <- asks processInner
    lift $ writeFile outputPath outputFile

  where 
            
    processInner ( numberOfDice , inputFile , outputPath ) =
            
      ( numberOfDice , unwords . appendNumbers numberOfDice . lines $ inputFile ,
      outputPath )

    appendNumbers :: NumberOfDice -> [ Text ] -> [ Text ] 
    appendNumbers numberOfDice inputFile =

        zipWith (putBetweenSemigroups ":") inputFile $
        ( iterate' ( addAnotherDice <*> ) [empty] ) !! numberOfDice

      where

        addAnotherDice :: [ Text -> Text ]
        addAnotherDice = cons <$> [ '1' .. '6' ]