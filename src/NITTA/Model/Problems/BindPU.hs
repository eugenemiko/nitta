{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : NITTA.Model.Problems.Bind
Description : Function distribution between processor units
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.BindPU (
    BindPU (..),
    BindPUProblem (..),
) where

import Data.String.ToString
import GHC.Generics (Generic)

data BindPU tag = BindPU
    { bnTag :: tag
    , puTag :: tag
    }
    deriving (Generic)

instance (ToString tag) => Show (BindPU tag) where
    show BindPU{bnTag, puTag} = "BindPU " <> toString bnTag <> toString puTag

class BindPUProblem u tag | u -> tag where
    bindPUOptions :: u -> [BindPU tag]
    bindPUDecision :: u -> BindPU tag -> u
