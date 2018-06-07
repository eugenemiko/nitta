{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

module NITTA.Test.ProcessUnits.Fram where

import           Control.Applicative       ((<$>))
import           Data.Default
import qualified Data.Map                  as M
import           Data.Proxy
import qualified NITTA.FunctionBlocks      as FB
import           NITTA.ProcessUnits.Fram
import           NITTA.Test.FunctionBlocks ()
import           NITTA.Test.ProcessUnits
import           NITTA.Types
import           Test.QuickCheck


framProxy = Proxy :: Proxy (Fram String Int Int)


instance Arbitrary (FSet (Fram String Int t)) where
  -- TODO: Сделать данную операцию через Generics.
  arbitrary = oneof [ FramInput' <$> (arbitrary :: Gen (FB.FramInput (Parcel String Int)))
                    , FramOutput' <$> (arbitrary :: Gen (FB.FramOutput (Parcel String Int)))
                    , Loop' <$> (arbitrary :: Gen (FB.Loop (Parcel String Int)))
                    , Reg' <$> (arbitrary :: Gen (FB.Reg (Parcel String Int)))
                    , Constant' <$> (arbitrary :: Gen (FB.Constant (Parcel String Int)))
                    ]

-----------------------------------------------------------

framRegAndOut = unitTestbench "framRegAndOut" framProxy
  def{ cntxVars=M.fromList [("aa", [42]), ("ac", [0x1003])] }
  [ FB.reg "aa" ["ab"]
  , FB.framOutput 9 "ac"
  ]

framRegAndConstant = unitTestbench "framRegAndConstant" framProxy
  def{ cntxVars=M.fromList [("dzw", [975])] }
  [ FB.reg "dzw" ["act","mqt"]
  , FB.constant 11 ["ovj"]
  ]



-- Ниже приведённый код раньше использовался для генерации максимально плотной программы для Fram, с
-- учётом особенностей внутренней диспетчеризации. Остальная часть алгоритма - в истории.
--
-- TODO: Необходимо реимплементировать для новых условий в виде через создание контролируемого
-- processGen и сортировки / фильтрации алгоритма.
--
-- data ST = ST { acc           :: [FB Parcel String]
--              , forInput      :: [Int]
--              , forOutput     :: [Int]
--              , numberOfLoops :: Int
--              , usedVariables :: [String]
--              , values        :: [(String, Int)]
--              } deriving (Show)
--
-- framDataFlowGen checkCellUsage generalPred =
--   suchThat (do
--                size <- sized pure -- getSize
--                n <- choose (0, size)
--                foldM maker (ST [] [] [] 0 [] []) [ 0..n ]
--            ) generalPred
--   where
--     maker st0@ST{..} _ = nextState st0 <$> do
--       fb <- suchThat (oneof [ FB <$> (arbitrary :: Gen (FramInput Parcel String))
--                             , FB <$> (arbitrary :: Gen (FramOutput Parcel String))
--                             , FB <$> (arbitrary :: Gen (Loop Parcel String))
--                             , FB <$> (arbitrary :: Gen (Reg Parcel String))
--                             ]
--                      ) check
--       v <- choose (0 :: Int, 0xFF)
--       return (fb, v)
--         where
--           nextState st (fb, v) = specificUpdate fb v st
--             { acc=fb : acc
--             , usedVariables=variables fb ++ usedVariables
--             }
--           specificUpdate fb value st
--             | Just (FramInput addr _vs) <- castFB fb = st{ forInput=addr : forInput }
--             | Just (FramOutput addr (I v)) <- castFB fb = st{ forOutput=addr : forOutput
--                                                             , values=(v, value) : values
--                                                             }
--             | Just (Loop _bs (I a)) <- castFB fb = st{ numberOfLoops=numberOfLoops + 1
--                                                      , values=(a, value) : values
--                                                      }
--             | Just (Reg (I a) _bs) <- castFB fb = st{ values=(a, value) : values }
--             | otherwise = error $ "Bad FB: " ++ show fb
--           check fb
--             | not $ null (variables fb `intersect` usedVariables) = False
--             | Just (Reg _ _ :: Reg Parcel String) <- castFB fb = True
--             | not checkCellUsage = True
--             | not (dfIoUses < framDefSize) = False
--             | Just (FramInput addr _) <- castFB fb = addr `notElem` forInput
--             | Just (FramOutput addr _) <- castFB fb = addr `notElem` forOutput
--             | otherwise = True -- for Loop
--           dfIoUses = length (nub $ forInput `union` forOutput) + numberOfLoops