{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

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
import qualified Data.Text             as T
import           Data.Typeable
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.DataFlow
import           NITTA.Types
import           NITTA.Types.Synthesis
import           Numeric.Interval
import           Servant



-- *Option/Decision
instance ( ToJSON title
         , ToJSONKey v, ToJSON v, Var v
         , ToJSON (TimeConstrain t)
         ) => ToJSON (Option (CompilerDT title tag v t))
instance ( ToJSON title
         , ToJSONKey v, ToJSON v, Var v
         , ToJSON (TimeConstrain t), Time t
         ) => ToJSON (Decision (CompilerDT title tag v t))
instance ( ToJSON title
         , ToJSONKey v
         , ToJSON (TimeConstrain t)
         ) => ToJSON (Option (DataFlowDT title v t))
instance ( ToJSON title
         , ToJSONKey v
         , ToJSON (TimeConstrain t), Time t
         ) => ToJSON (Decision (DataFlowDT title v t))
instance ( Show title
         ) => ToJSON (Option (BindingDT title io)) where
    toJSON (BindingO fb title) = toJSON [ show fb, show title ]
instance ( Show title
         ) => ToJSON (Decision (BindingDT title io)) where
    toJSON (BindingD fb title) = toJSON [ show fb, show title ]
instance ( ToJSON v, Var v ) => ToJSON (Option (ControlDT v))
instance ( ToJSON v, Var v ) => ToJSON (Decision (ControlDT v))



-- *Process units
instance ( ToJSONKey title, ToJSON title, Typeable title, Ord title, Show title
         , Var v
         , Time t, ToJSON t
         , Typeable x, ToJSON x, ToJSONKey x
         ) => ToJSON (BusNetwork title v x t) where
    toJSON n@BusNetwork{..} = object
        -- , bnSignalBusWidth     :: Int
        [ "width" .= bnSignalBusWidth
        --   bnRemains            :: [F (Parcel v) v]
        , "remain" .= bnRemains
        -- , bnForwardedVariables :: [v]
        , "forwardedVariables" .= map (String . T.pack . show) (transfered n)
        -- , bnBinded             :: M.Map title [F (Parcel v) v]
        , "binds" .= bnBinded
        -- , bnProcess            :: Process v t
        , "processLength" .= nextTick (process n)
        -- , bnPus                :: M.Map title spu
        , "processUnits" .= M.keys bnPus
        , "process" .= process n
        ]



-- *Synthesis
instance ( ToJSON v, Var v ) => ToJSON (DataFlowGraph v)

instance ToJSON Relation where
    toJSON (Vertical a b) = toJSON [ a, b ]

instance ( ToJSONKey title, ToJSON title, Show title, Ord title, Typeable title
         , ToJSON tag
         , ToJSON v, Var v
         , ToJSON t, Time t
         , ToJSONKey v
         , Show x, Ord x, Typeable x, ToJSON x, ToJSONKey x
         ) => ToJSON (SystemState title tag x v t)
         
instance ( ToJSON t, Time t, Show v
         ) => ToJSON (Process (Parcel v x) t) where
    toJSON Process{ steps, nextTick, relations } = object
        [ "steps" .= steps
        , "nextTick" .= nextTick
        , "relations" .= relations
        ]

instance ( ToJSON t, Time t, Show v
         ) => ToJSON (Step (Parcel v x) t) where
    toJSON Step{ sKey, sTime, sDesc } = object
        [ "sKey" .= sKey
        , "sDesc" .= show sDesc
        , "sTime" .= sTime
        , "sLevel" .= level sDesc
        , "sPU" .= showPU sDesc
        ]



-- * Synthesis
instance ToJSON Nid where
    toJSON nid = toJSON $ show nid

instance FromJSON Nid where
    parseJSON v = read <$> parseJSON v

instance FromHttpApiData Nid where
    parseUrlPiece = Right . read . T.unpack



-- *Simple compiler
instance ToJSON NaiveOpt
instance ToJSON (CompilerStep String String String Int (TaggedTime String Int))
instance ToJSON SpecialMetrics
instance ToJSON GlobalMetrics



-- * Basic data
instance ( ToJSON tag, ToJSON t ) => ToJSON (TaggedTime tag t)

instance ( ToJSON t, Time t ) => ToJSON (PlaceInTime t) where
    toJSON (Event t)    = toJSON [ fromEnum t, fromEnum t ]
    toJSON (Activity i) = toJSON [ fromEnum $ inf i, fromEnum $ sup i ]

instance ( Show v ) => ToJSON (F (Parcel v x)) where
    toJSON = String . T.pack . show

instance ( ToJSON t, Time t ) => ToJSON (TimeConstrain t) where
    toJSON TimeConstrain{..} = object
        [ "available" .= tcAvailable
        , "duration" .= tcDuration
        ]



-- *System
instance ( Show a ) => ToJSON (Interval a) where
    toJSON = String . T.pack . show