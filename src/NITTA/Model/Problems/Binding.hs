{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Model.Problems.Endpoint
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Binding
    ( Bind(..), BindProblem(..)
    ) where

import           GHC.Generics
import           NITTA.Intermediate.Types

data Bind tag v x = Bind (F v x) tag deriving ( Generic )

class BindProblem u tag v x | u -> tag v x where
    bindOptions :: u -> [ Bind tag v x ]
    bindDecision :: u -> Bind tag v x -> u