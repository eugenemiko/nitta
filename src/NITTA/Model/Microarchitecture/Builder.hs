{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : NITTA.Model.Microarchitecture.Builder
Description : Create micro architecture functions
Copyright   : (c) Daniil Prohorov, Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Microarchitecture.Builder (
    MicroarchitectureDesc (..),
    NetworkDesc (..),
    UnitDesc (..),
    microarchitectureDesc,
) where

import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics
import NITTA.Model.Networks.Bus
import NITTA.Model.Networks.Types
import NITTA.Utils.Base

data MicroarchitectureDesc tag = MicroarchitectureDesc
    { networks :: [NetworkDesc tag]
    , ioSyncMode :: IOSynchronization
    }
    deriving (Generic)

instance (ToJSON tag) => ToJSON (MicroarchitectureDesc tag)

data NetworkDesc tag = NetworkDesc
    { networkTag :: tag
    , valueType :: T.Text
    , units :: [UnitDesc tag]
    }
    deriving (Generic)

instance (ToJSON tag) => ToJSON (NetworkDesc tag)

data UnitDesc tag = UnitDesc
    { unitTag :: tag
    , unitType :: T.Text
    }
    deriving (Generic)

instance (ToJSON tag) => ToJSON (UnitDesc tag)

microarchitectureDesc :: forall tag v x t. (Typeable x) => BusNetwork tag v x t -> MicroarchitectureDesc tag
microarchitectureDesc BusNetwork{bnName, bnPus, ioSync} =
    MicroarchitectureDesc
        { networks =
            [ NetworkDesc
                { networkTag = bnName
                , valueType = showText $ typeRep (Proxy :: Proxy x)
                , units =
                    map
                        ( \(tag, PU{unit}) ->
                            UnitDesc
                                { unitTag = tag
                                , unitType = T.pack $ takeWhile (' ' /=) $ show $ typeOf unit
                                }
                        )
                        $ M.assocs bnPus
                }
            ]
        , ioSyncMode = ioSync
        }
