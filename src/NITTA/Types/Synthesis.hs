{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.Types.Synthesis
Description : Types to describe synthesis process
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.Types.Synthesis
    ( -- *Synthesis graph
      Node(..)
    , Edge(..)
    , NId(..)
    , mkNode, mkNodeIO
    , getNode, getNodeIO
    , getEdges, getEdgesIO
      -- *Characteristics & synthesis decision type
    , SynthesisDT, synthesis
    , Conf(..)
    , SpecialMetrics(..)
      -- *Utils
    , endpointOption2action
    , isSchedulingCompletable
    , isSchedulingComplete
    , option2decision
    , targetProcessDuration
    ) where

import           Control.Arrow          (second)
import           Control.Concurrent.STM
import           Control.Monad          (unless)
import           Control.Monad          (forM)
import           Data.Default
import           Data.List              (find)
import           Data.List.Split
import qualified Data.Map               as M
import           Data.Maybe
import           Data.Proxy
import           Data.Semigroup         (Semigroup, (<>))
import           Data.Set               (Set, fromList, intersection, member)
import qualified Data.Set               as S
import           Data.Typeable          (Typeable)
import           GHC.Generics
import           NITTA.BusNetwork
import           NITTA.DataFlow         (ModelState (..))
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Lens
import           Numeric.Interval       (Interval, (...))



data Node title v x t
    = Node
        { nId         :: NId
        , nModel      :: ModelState title v x t
        , nIsComplete :: Bool
        , nEdges      :: TVar (Maybe [Edge title v x t])
        }
    deriving ( Generic )


data Edge title v x t
    = Edge
        { eNode            :: Node title v x t
        , eCharacteristic  :: Float
        , eCharacteristics :: SpecialMetrics
        , eOption          :: Option (SynthesisDT title v x t)
        , eDecision        :: Decision (SynthesisDT title v x t)
        }
    deriving ( Generic )



-- |Create initial synthesis.
mkNodeIO nId model = atomically $ mkNode nId model

mkNode nId model = do
    nEdges <- newTVar Nothing
    return Node
        { nId=nId
        , nModel=model
        , nIsComplete=isSchedulingComplete model
        , nEdges
        }



getEdgesIO node = atomically $ getEdges node

getEdges node@Node{ nEdges }
    = readTVar nEdges >>= \case
        Just edges -> return edges
        Nothing -> do
            edges <- mkEdges node
            writeTVar nEdges $ Just edges
            return edges



mkEdges Node{ nId, nModel } = do
    let Conf{ threshhold } = def
        opts = options synthesis nModel
        bindedVars = unionsMap variables $ concat $ M.elems $ bnBinded $ processor nModel
        possibleDeadlockBinds = fromList [ f
            | (BindingOption f _) <- opts
            , Lock{ locked } <- locks f
            , locked `member` bindedVars
            ]
        cntx = MeasureCntx
            { numberOfBindOptions=length $ filter isBinding opts
            , numberOfDFOptions=length $ filter isDataFlow opts
            , dataflowThreshhold=threshhold
            , possibleDeadlockBinds
            , allOptions=opts
            , model=nModel
            }

    forM (zip [0..] opts) $ \(i, eOption) -> do
        let eDecision = option2decision eOption
            eCharacteristics = measure cntx eOption
            eCharacteristic = fromIntegral $ integral cntx eCharacteristics
        eNode <- mkNode (nId <> NId [i]) $ decision synthesis nModel eDecision
        return Edge{ eOption, eDecision, eCharacteristic, eCharacteristics, eNode }



-- |Get specific by @nId@ node from a synthesis tree.
getNodeIO node nId = atomically $ getNode node nId

getNode node (NId []) = return node
getNode node nId@(NId (i:is)) = do
    edges <- getEdges node
    unless (i < length edges) $ error $ "getNode - wrong nId: " ++ show nId
    getNode (eNode $ edges !! i) (NId is)




-- |Synthesis identical.
newtype NId = NId [Int]
nIdSep = ':'

instance Show NId where
    show (NId []) = [nIdSep]
    show (NId is) = show' is
        where
            show' []     = ""
            show' (x:xs) = nIdSep : show x ++ show' xs

instance Read NId where
    readsPrec _ [x] | x == nIdSep    = [(NId [], "")]
    readsPrec d (x:xs)
        | x == nIdSep
        , let is = map (readsPrec d) $ splitOn [nIdSep] xs
        , all (not . null) is
        = [(NId $ map fst $ concat is, "")]
    readsPrec _ _ = []

instance Semigroup NId where
    (NId a) <> (NId b) = NId (a <> b)

instance Monoid NId where
    mempty = NId []
    mappend = (<>)



---------------------------------------------------------------------
-- *Compiler Decision Type


data SynthesisDT title v x t
synthesis = Proxy :: Proxy SynthesisDT


instance DecisionType (SynthesisDT title v x t) where
    data Option (SynthesisDT title v x t)
        = BindingOption (F v x) title
        | DataFlowOption (Source title (TimeConstrain t)) (Target title v (TimeConstrain t))
        deriving ( Generic, Show )

    data Decision (SynthesisDT title v x t)
        = BindingDecision (F v x) title
        | DataFlowDecision (Source title (Interval t)) (Target title v (Interval t))
        deriving ( Generic, Show )

isBinding = \case BindingOption{} -> True; _ -> False
isDataFlow = \case DataFlowOption{} -> True; _ -> False

specializeDataFlowOption (DataFlowOption s t) = DataFlowO s t
specializeDataFlowOption _ = error "Can't specialize non DataFlow option!"

generalizeDataFlowOption (DataFlowO s t) = DataFlowOption s t
generalizeBindingOption (BindingO s t) = BindingOption s t



instance ( Var v, Typeable x, Time t
         ) => DecisionProblem (SynthesisDT String v x t)
                  SynthesisDT (ModelState String v x t)
        where
    options _ f@Frame{ processor }
        =  map generalizeBindingOption (options binding f)
        ++ map generalizeDataFlowOption (options dataFlowDT processor)

    decision _ fr (BindingDecision f title) = decision binding fr $ BindingD f title
    decision _ fr@Frame{ processor } (DataFlowDecision src trg) = fr{ processor=decision dataFlowDT processor $ DataFlowD src trg }

option2decision (BindingOption fb title) = BindingDecision fb title
option2decision (DataFlowOption src trg)
    = let
        pushTimeConstrains = map snd $ catMaybes $ M.elems trg
        pullStart    = maximum $ (snd src^.avail.infimum) : map (\o -> o^.avail.infimum) pushTimeConstrains
        pullDuration = maximum $ map (\o -> o^.dur.infimum) $ snd src : pushTimeConstrains
        pullEnd = pullStart + pullDuration - 1
        pushStart = pullStart
        mkEvent (from_, tc) = Just (from_, pushStart ... (pushStart + tc^.dur.infimum - 1))
        pushs = map (second $ maybe Nothing mkEvent) $ M.assocs trg
    in DataFlowDecision ( fst src, pullStart ... pullEnd ) $ M.fromList pushs



-----------------------------------------------------------
-- *Characteristics


-- |Synthesis process setup, which determines next synthesis step selection.
newtype Conf
    = Conf
        { -- |Порог колличества вариантов, после которого пересылка данных станет приоритетнее, чем
          -- привязка функциональных блоков.
          threshhold :: Int
        }
    deriving ( Generic, Show, Eq, Ord )

instance Default Conf where
    def = Conf
        { threshhold=2
        }



-- | Метрики для принятия решения компилятором.
data SpecialMetrics
    = -- | Решения о привязке функциональных блоков к ВУ.
        BindingMetrics
        { -- |Устанавливается для таких функциональных блоков, привязка которых может быть заблокирована
          -- другими. Пример - занятие Loop-ом адреса, используемого FramInput.
          critical         :: Bool
          -- |Колличество альтернативных привязок для функционального блока.
        , alternative      :: Int
          -- |Привязка данного функционального блока может быть активировано только спустя указанное
          -- колличество тактов.
        , restless         :: Int
          -- |Данная операция может быть привязана прямо сейчас и это приведёт к разрешению указанного
          -- количества пересылок.
        , allowDataFlow    :: Int
          -- |Если была выполнена привязка функции из серидины алгоритма, то это может
          -- привести к deadlock-у. В такой ситуации может быть активирована пересылка
          -- в вычислительный блок, в то время как часть из входных данных не доступна,
          -- так как требуемая функция ещё не привязана, а после привязки не сможет быть
          -- вычисленна, так как ресурс уже занят.
        , possibleDeadlock :: Bool
        }
    | DataFlowMetrics { waitTime :: Int, restrictedTime :: Bool }
    deriving ( Show, Generic )



data MeasureCntx f o m
    = MeasureCntx
        { possibleDeadlockBinds :: Set f
        , numberOfBindOptions   :: Int
        , numberOfDFOptions     :: Int
        , dataflowThreshhold    :: Int
        , allOptions            :: o
        , model                 :: m
        }



measure
        MeasureCntx{ possibleDeadlockBinds, allOptions, model=Frame{ processor=net@BusNetwork{ bnPus } } }
        (BindingOption f title)
    = BindingMetrics
        { critical=isCritical f
        , alternative=length (howManyOptionAllow (filter isBinding allOptions) M.! f)
        , allowDataFlow=sum $ map (length . variables) $ filter isTarget $ optionsAfterBind f (bnPus M.! title)
        , restless=fromMaybe 0 $ do
            (_var, tcFrom) <- find (\(v, _) -> v `elem` variables f) $ waitingTimeOfVariables net
            return $ fromEnum tcFrom
        , possibleDeadlock=f `member` possibleDeadlockBinds
        }
measure MeasureCntx{} opt@DataFlowOption{} = DataFlowMetrics
    { waitTime=fromEnum (specializeDataFlowOption opt^.at.avail.infimum)
    , restrictedTime=fromEnum (specializeDataFlowOption opt^.at.dur.supremum) /= maxBound
    }

integral MeasureCntx{} BindingMetrics{ possibleDeadlock=True }            = 2000000
integral MeasureCntx{ numberOfDFOptions, dataflowThreshhold } DataFlowMetrics{ waitTime }
    | numberOfDFOptions >= dataflowThreshhold                             = 10000 + 200 - waitTime
integral MeasureCntx{} BindingMetrics{ critical=True }                    = 2000
integral MeasureCntx{} BindingMetrics{ alternative=1 }                    = 500
integral MeasureCntx{} BindingMetrics{ allowDataFlow, restless }          = 200 + allowDataFlow * 10 - restless * 2
integral MeasureCntx{} DataFlowMetrics{ restrictedTime } | restrictedTime = 200 + 100
integral MeasureCntx{} DataFlowMetrics{ waitTime }                        = 200 - waitTime



-- | Подсчитать, сколько вариантов для привязки функционального блока определено.
-- Если вариант всего один, может быть стоит его использовать сразу?
howManyOptionAllow bOptions
    = foldl ( \st (BindingOption fb title) -> M.alter (countOption title) fb st ) (M.fromList []) bOptions
    where
        countOption title (Just titles) = Just $ title : titles
        countOption title Nothing       = Just [ title ]


targetProcessDuration Frame{ processor } = nextTick $ process processor


-- | Время ожидания переменных.
waitingTimeOfVariables net =
    [ (variable, tc^.avail.infimum)
    | DataFlowO{ dfoSource=(_, tc@TimeConstrain{}), dfoTargets } <- options dataFlowDT net
    , (variable, Nothing) <- M.assocs dfoTargets
    ]


-- | Оценить, сколько новых вариантов развития вычислительного процесса даёт привязка
-- функциоанльного блока.
optionsAfterBind fb pu = case tryBind fb pu of
    Right pu' -> filter (\(EndpointO act _) -> act `optionOf` fb) $ options endpointDT pu'
    _         -> []
    where
        act `optionOf` fb' = not $ S.null (variables act `intersection` variables fb')


-- * Утилиты

endpointOption2action o@EndpointO{ epoRole }
    = let
        a = o^.at.avail.infimum
        -- "-1" - необходимо, что бы не затягивать процесс на лишний такт, так как интервал включает
        -- граничные значения.
        b = o^.at.avail.infimum + o^.at.dur.infimum - 1
    in EndpointD epoRole (a ... b)


isSchedulingComplete Frame{ processor, dfg }
    | let inWork = S.fromList $ transfered processor
    , let inAlg = variables dfg
    = inWork == inAlg



-- | Проверка является процесс планирования вычислительного процесса полностью завершимым (все
-- функционаные блоки могут быть выполнены). Данная функция используется для проверки возможности
-- привязки функционального блока.
isSchedulingCompletable pu
    = case options endpointDT pu of
        (o:_os) -> let
                d = endpointOption2action o
                pu' = decision endpointDT pu d
                in isSchedulingCompletable pu'
        _ -> let
                algVars = unionsMap variables $ functions pu
                processVars = unionsMap variables $ getEndpoints $ process  pu
            in algVars == processVars
