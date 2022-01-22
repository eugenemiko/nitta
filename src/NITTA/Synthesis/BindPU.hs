{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

module NITTA.Synthesis.BindPU (
    BindPUMetrics (..),
) where

import Data.Aeson (ToJSON)
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Generics
import NITTA.Model.Networks.Bus
import NITTA.Model.Networks.Types
import NITTA.Model.Problems.BindPU
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.TargetSystem
import NITTA.Synthesis.Types
import NITTA.Utils

data BindPUMetrics = BindPUMetrics
    { mParallelism :: T.Text
    , mRelatedRemains :: Int
    , mMinPusForRemains :: Int
    }
    deriving (Generic)

instance ToJSON BindPUMetrics

instance
    (UnitTag tag) =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) tag v x t) tag v x t) -- ctx
        (TargetSystem (BusNetwork tag v x t) tag v x t) -- m
        (BindPU tag) -- o
        (BindPU tag) -- d
        BindPUMetrics -- p
    where
    decisions SynthesisState{sTarget} o = [(o, bindPUDecision sTarget o)]

    -- parameters :: ctx -> o -> d -> p
    parameters SynthesisState{sTarget = TargetSystem{mUnit}} BindPU{puTag} _ =
        let reserve = bnPUReserve mUnit
            pus = M.elems $ bnPus mUnit
            tmp = reserve M.! puTag
            canProcessTmp PUTemplate{putUnit} f = allowToProcess f putUnit
            canProcessPU PU{unit} f = allowToProcess f unit
            relatedRemains = filter (canProcessTmp tmp) $ bnRemains mUnit
            mParallelism PUTemplate{putUnit} = showText $ parallelism putUnit
         in BindPUMetrics
                { mParallelism = mParallelism tmp
                , mRelatedRemains = length relatedRemains
                , mMinPusForRemains = foldr (min . (\f -> length $ filter (`canProcessPU` f) pus)) (maxBound :: Int) relatedRemains
                }

    -- estimate :: ctx -> o -> d -> p -> Float
    estimate _ctx _o _d BindPUMetrics{mParallelism, mRelatedRemains, mMinPusForRemains} =
        let remains = fromIntegral mRelatedRemains + 0.2 :: Float
            minPus = fromIntegral mMinPusForRemains + 0.2 :: Float
            pDivider =
                case mParallelism of
                    "Full" -> remains
                    "Pipe" -> remains / 4
                    "None" -> 1
                    _ -> error "Can't estimate BindPU: Unknown type of mParallelism"
         in remains / minPus * 1000 / pDivider
