module Main (
    main
  ) where

import Prelude ()
import Prelude'

import Control.Exception
import System.Exit

import ApplicativeParsec

import Smith
import Parser

----------------------------------------------------------

main :: IO ()
main = handleJust (
  \e -> case e of
    ExitSuccess -> Just ()
    _ -> Nothing
  ) return $ do
  let sourceFile = "test"
  sourceCode <- readFile sourceFile
  case parse smithParser sourceFile sourceCode of
    Left err -> error $ show err
    Right oprs -> smith oprs
