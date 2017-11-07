{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.FunctionBlocks where

import           Data.Bits
import           Data.Typeable (Typeable, cast)
import           NITTA.Types



class ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, Bits a ) => Addr a
instance ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, Bits a ) => Addr a


castFB :: ( Typeable v, Typeable (fb io v) ) => FB io v -> Maybe (fb io v)
castFB (FB fb) = cast fb

boxFB :: ( FunctionalBlock (fb io v) v, Variables (fb io v) v, IOType io v, Show (fb io v) ) => fb io v -> FB io v
boxFB fb = FB fb


----------------------------------------


data FramInput io v = FramInput Int (O io v) deriving ( Typeable )
deriving instance IOType io v => Show (FramInput io v)
deriving instance IOType io v => Eq (FramInput io v)
framInput addr vs = FB $ FramInput addr vs

instance IOType io v => FunctionalBlock (FramInput io v) v where
  outputs (FramInput _ o) = variables o
  isCritical _ = True





data FramOutput io v = FramOutput Int (I io v) deriving ( Typeable )
deriving instance IOType io v => Show (FramOutput io v)
deriving instance IOType io v => Eq (FramOutput io v)
framOutput addr v = FB $ FramOutput addr v

instance IOType io v => FunctionalBlock (FramOutput io v) v where
  inputs (FramOutput _ o) = variables o
  isCritical _ = True



data Reg io v = Reg (I io v) (O io v) deriving ( Typeable )
deriving instance IOType io v => Show (Reg io v)
deriving instance IOType io v => Eq (Reg io v)
reg a b = FB $ Reg a b

instance IOType io v => FunctionalBlock (Reg io v) v where
  inputs  (Reg a _b) = variables a
  outputs (Reg _a b) = variables b
  dependency (Reg i o) = [ (b, a) | a <- variables i
                                  , b <- variables o
                                  ]



data Loop io v = Loop (O io v) (I io v) deriving ( Typeable )
deriving instance IOType io v => Show (Loop io v)
deriving instance IOType io v => Eq (Loop io v)
loop bs a = FB $ Loop bs a

instance IOType io v => FunctionalBlock (Loop io v) v where
  inputs  (Loop _a b) = variables b
  outputs (Loop a _b) = variables a
  insideOut _ = True



data Add io v = Add (I io v) (I io v) (O io v) deriving ( Typeable )
deriving instance IOType io v => Show (Add io v)
deriving instance IOType io v => Eq (Add io v)
-- add a b c = FB $ Add a b c

instance IOType io v => FunctionalBlock (Add io v) v where
  inputs  (Add  a  b _c) = variables a ++ variables b
  outputs (Add _a _b  c) = variables c
  dependency (Add a b c) = [ (y, x) | x <- variables a ++ variables b
                                    , y <- variables c
                                    ]



data Constant io v = Constant Int (O io v) deriving ( Typeable )
deriving instance ( IOType io v ) => Show (Constant io v)
deriving instance ( IOType io v ) => Eq (Constant io v)

instance ( IOType io v ) => FunctionalBlock (Constant io v) v where
  outputs (Constant _ o) = variables o
