{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.API.Marshalling
Description : Marshalling data for REST API
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.API.Marshalling where

import           Data.Aeson
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Data.String.Utils     as S
import qualified Data.Text             as T
import           Data.Typeable
import           NITTA.BusNetwork
import           NITTA.Model
import           NITTA.Types
import           NITTA.Types.Project
import           NITTA.Types.Synthesis
import           NITTA.Utils           (transferred)
import           Numeric.Interval
import           Servant



-- *Option/Decision
instance ( ToJSON title
         , ToJSONKey v, ToJSON v, Var v
         , ToJSON (TimeConstrain t)
         ) => ToJSON (Option (SynthesisDT (BusNetwork title v x t)))
instance ( ToJSON title
         , ToJSONKey v, ToJSON v, Var v
         , ToJSON (TimeConstrain t), Time t
         ) => ToJSON (Decision (SynthesisDT (BusNetwork title v x t)))
instance ( ToJSON v ) => ToJSON (Option (RefactorDT v))
instance ( ToJSON v ) => ToJSON (Decision (RefactorDT v))
instance ( ToJSON t, Time t ) => ToJSON (Option (EndpointDT String t)) where
    toJSON EndpointO{ epoRole=Source vs, epoAt } = toJSON ("Source: " ++ S.join ", " (S.elems vs) ++ " at " ++ show epoAt)
    toJSON EndpointO{ epoRole=Target v, epoAt } = toJSON ("Target: " ++ v ++ " at " ++ show epoAt)



-- *Process units
instance ( ToJSONKey title, ToJSON title, Typeable title, Ord title, Show title
         , Var v
         , Time t, ToJSON t
         , Typeable x, ToJSON x, ToJSONKey x
         ) => ToJSON (BusNetwork title v x t) where
    toJSON n@BusNetwork{..} = object
        [ "width"              .= bnSignalBusWidth
        , "remain"             .= bnRemains
        , "forwardedVariables" .= map (String . T.pack . show) (transferred n)
        , "binds"              .= bnBinded
        , "processLength"      .= nextTick (process n)
        , "processUnits"       .= M.keys bnPus
        , "process"            .= process n
        ]



-- *Model
instance ( ToJSON v, Var v, ToJSON x ) => ToJSON (DataFlowGraph v x)

instance ToJSON Relation where
    toJSON (Vertical a b) = toJSON [ a, b ]

instance ( ToJSONKey title, ToJSON title, Show title, Ord title, Typeable title
         , ToJSON v, Var v
         , ToJSON t, Time t
         , ToJSONKey v
         , Show x, Ord x, Typeable x, ToJSON x, ToJSONKey x
         ) => ToJSON (ModelState (BusNetwork title v x t) v x)

instance ( ToJSON t, Time t, Show v
         ) => ToJSON (Process v x t) where
    toJSON Process{ steps, nextTick, relations } = object
        [ "steps"     .= steps
        , "nextTick"  .= nextTick
        , "relations" .= relations
        ]

instance ( ToJSON t, Time t, Show v
         ) => ToJSON (Step v x t) where
    toJSON Step{ sKey, sTime, sDesc } = object
        [ "sKey"   .= sKey
        , "sDesc"  .= show sDesc
        , "sTime"  .= sTime
        , "sLevel" .= level sDesc
        , "sPU"    .= showPU sDesc
        ]



-- *Synthesis
instance ToJSON NId where
    toJSON nId = toJSON $ show nId

instance FromJSON NId where
    parseJSON v = read <$> parseJSON v

instance FromHttpApiData NId where
    parseUrlPiece = Right . read . T.unpack


instance
        ( ToJSON x, ToJSONKey x, Typeable x, Ord x, Show x
        , ToJSON t, Time t
        ) => ToJSON (Node (ModelState (BusNetwork String String x t) String x) (SynthesisDT (BusNetwork String String x t))) where
    toJSON Node{ nId, nModel, nIsComplete } = object
        [ "nModel"      .= nModel
        , "nIsComplete" .= nIsComplete
        , "nId"         .= nId
        ]

instance ToJSON TestbenchReport


-- *Simple synthesis
instance ToJSON ChConf
instance ToJSON Characteristics

instance
        ( ToJSON x, ToJSONKey x, Typeable x, Ord x, Show x
        , ToJSON t, Time t
        ) => ToJSON (Edge m (SynthesisDT (BusNetwork String String x t))) where
    toJSON Edge{ eCharacteristic, eCharacteristics, eOption, eDecision } = object
        [ "eCharacteristic"  .= eCharacteristic
        , "eCharacteristics" .= eCharacteristics
        , "eOption"          .= eOption
        , "eDecision"        .= eDecision
        ]



-- *Basic data
instance ( ToJSON tag, ToJSON t ) => ToJSON (TaggedTime tag t)

instance ( ToJSON t, Time t ) => ToJSON (PlaceInTime t) where
    toJSON (Event t)    = toJSON [ fromEnum t, fromEnum t ]
    toJSON (Activity i) = toJSON [ fromEnum $ inf i, fromEnum $ sup i ]

instance ( Show v ) => ToJSON (F v x) where
    toJSON = String . T.pack . show

instance ( ToJSON t, Time t ) => ToJSON (TimeConstrain t) where
    toJSON TimeConstrain{..} = object
        [ "available" .= tcAvailable
        , "duration"  .= tcDuration
        ]

instance ToJSONKey (IntX w) where
    toJSONKey
        = let
            ToJSONKeyText f g = toJSONKey
        in ToJSONKeyText (\(IntX x) -> f x) (\(IntX x) -> g x)


instance ToJSON (IntX w) where
    toJSON ( IntX x ) = toJSON x

instance ToJSONKey (FX m b) where
    toJSONKey
        = let
            ToJSONKeyText f g = toJSONKey
        in ToJSONKeyText (\( FX x ) -> f $ show x) (\( FX x ) -> g $ show x)

instance ToJSON (FX m b) where
    toJSON ( FX x ) = toJSON $ show x



-- *System
instance ( Show a, Bounded a ) => ToJSON (Interval a) where
    toJSON = String . T.pack . S.replace (show (maxBound :: a)) "∞" . show
