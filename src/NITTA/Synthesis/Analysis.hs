{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

{- |
Module      : NITTA.Synthesis.Analysis
Description : Analysis synthesis proccess.
Copyright   : (c) Daniil Prohorov, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Analysis (
    getTreeInfo,
    TreeInfo (..),
) where

import Control.Concurrent.STM
import qualified Data.HashMap.Strict as HM
import GHC.Generics
import Data.List (foldl1')
import NITTA.Model.TargetSystem (processDuration)
import NITTA.Synthesis.Explore (isComplete, isLeaf)
import NITTA.Synthesis.Types
import Control.Parallel.Strategies

-- |Metrics of synthesis tree process
data TreeInfo = TreeInfo
    { nodes :: Int
    , success :: Int
    , failed :: Int
    , notProcessed :: Int
    , durationSuccess :: HM.HashMap Int Int
    , stepsSuccess :: HM.HashMap Int Int
    }
    deriving (Generic, Show)

instance Semigroup TreeInfo where
    -- (<>) !synthesisInfo1 !synthesisInfo2 =
    --     let synthesisInfoList = [synthesisInfo1, synthesisInfo2]
    --         durationSuccessList = map durationSuccess synthesisInfoList
    --         stepsSuccessList = map stepsSuccess synthesisInfoList
    --      in TreeInfo
    --             { nodes = sum $ map nodes synthesisInfoList
    --             , success = sum $ map success synthesisInfoList
    --             , failed = sum $ map failed synthesisInfoList
    --             , notProcessed = sum $ map notProcessed synthesisInfoList
    --             , durationSuccess = if not $ null durationSuccessList then foldl1' (HM.unionWith (+)) durationSuccessList else HM.empty
    --             , stepsSuccess = if not $ null stepsSuccessList then foldl1' (HM.unionWith (+)) stepsSuccessList else HM.empty
    --             }

    (<>) !synthesisInfo1 !synthesisInfo2 =
         let
            (nodes1, nodes2) = (nodes synthesisInfo1, nodes synthesisInfo2)
            (success1, success2) = (success synthesisInfo1, success synthesisInfo2)
            (failed1, failed2) = (failed synthesisInfo1, failed synthesisInfo2)
            (notProcessed1, notProcessed2) = (notProcessed synthesisInfo1, notProcessed synthesisInfo2)
            (durationSuccess1, durationSuccess2) = (durationSuccess synthesisInfo1, durationSuccess synthesisInfo2)
            (stepsSuccess1, stepsSuccess2) = (stepsSuccess synthesisInfo1, stepsSuccess synthesisInfo2)
         in TreeInfo
                { nodes = nodes1 + nodes2
                , success = success1 + success2
                , failed = failed1 + failed2
                , notProcessed =  notProcessed1 + notProcessed2
                , durationSuccess = HM.unionWith (+) durationSuccess1 durationSuccess2
                , stepsSuccess = HM.unionWith (+) stepsSuccess1 stepsSuccess2
                }

instance Monoid TreeInfo where
    mempty =
        TreeInfo
            { nodes = 0
            , success = 0
            , failed = 0
            , notProcessed = 0
            , durationSuccess = HM.empty
            , stepsSuccess = HM.empty
            }

getTreeInfo !(tree@Tree{sID = SID sid, sSubForestVar}) = do
    subForestM <- atomically $ tryReadTMVar sSubForestVar
    subForestInfo <- maybe (return mempty) (fmap mconcat . sequence . parMap rpar getTreeInfo) subForestM
    let isSuccess = isComplete tree && isLeaf tree
    let isFail = (not . isComplete) tree && isLeaf tree
    let duration = fromEnum $ processDuration $ sTarget $ sState tree
    let successDepends value field =
            if not isSuccess
                then field subForestInfo
                else HM.alter (Just . maybe 1 (+ 1)) value $ field subForestInfo
    return $
        TreeInfo
            { nodes = 1 + nodes subForestInfo
            , success = if isSuccess then 1 else 0 + success subForestInfo
            , failed = if isFail then 1 else 0 + failed subForestInfo
            , notProcessed = maybe 1 (const 0) subForestM + notProcessed subForestInfo
            , durationSuccess = successDepends duration durationSuccess
            , stepsSuccess = successDepends (length sid) stepsSuccess
            }

-- getTreeInfo nodes_ success_ failed_ notProcessed_ durationSuccess_ stepsSuccess_ !(tree@Tree{sID = SID sid, sSubForestVar}) = do
--     subForestM <- atomically $ tryReadTMVar sSubForestVar
--     subForestInfo <- maybe (return mempty) (fmap mconcat . sequence . parMap rpar getTreeInfo) subForestM
--     let isSuccess = isComplete tree && isLeaf tree
--     let isFail = (not . isComplete) tree && isLeaf tree
--     let duration = fromEnum $ processDuration $ sTarget $ sState tree
--     let successDepends value field =
--             if not isSuccess
--                 then field subForestInfo
--                 else HM.alter (Just . maybe 1 (+ 1)) value $ field subForestInfo
--     return $
--         TreeInfo
--             { nodes = 1 + nodes subForestInfo
--             , success = if isSuccess then 1 else 0 + success subForestInfo
--             , failed = if isFail then 1 else 0 + failed subForestInfo
--             , notProcessed = maybe 1 (const 0) subForestM + notProcessed subForestInfo
--             , durationSuccess = successDepends duration durationSuccess
--             , stepsSuccess = successDepends (length sid) stepsSuccess
--             }
