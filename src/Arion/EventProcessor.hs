{-# LANGUAGE OverloadedStrings #-}
module Arion.EventProcessor (
    processEvent, respondToEvent
) where

import           Arion.Types
import           Control.Applicative       ((<$>))
import           Data.List                 (isSuffixOf, isInfixOf)
import           Data.List                 (nub)
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import           System.FSNotify           (Event (..))



respondToEvent (Modified filePath time) = Just (filePath,time)
respondToEvent (Added filePath time) = Just (filePath,time)
respondToEvent _ = Nothing

processEvent :: M.Map String [TestFile] -> String -> String -> (FilePath, t) -> [Command]
processEvent sourceToTestFileMap sourceFolder testFolder (filePath,_)
        | (not . isInfixOf ".#") filePath && isSuffixOf "hs" filePath =
          let fileType = typeOf filePath
              commandCandidates = case fileType of
                Source -> nub . map testFilePath . fromMaybe []
                          $ M.lookup filePath sourceToTestFileMap
                Test ->   [filePath]
              maybeLacksTests = if commandCandidates == [] then [Echo (filePath ++ " does not have any associated tests...")] else []
          in Echo (filePath ++ " changed") : maybeLacksTests ++
             map (RunHaskell sourceFolder testFolder ) commandCandidates
        | otherwise = []
