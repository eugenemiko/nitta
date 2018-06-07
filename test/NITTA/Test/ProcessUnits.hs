{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

module NITTA.Test.ProcessUnits where

import           Data.Atomics.Counter
import           Data.Default
import qualified Data.Map                as M
import           Data.Proxy
import           Data.Set                (difference, elems, empty, fromList,
                                          intersection, union)
import           NITTA.Compiler
import           NITTA.Project
import           NITTA.Types
import           NITTA.Utils
import           System.FilePath.Posix   (joinPath)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty.HUnit        ((@?))

import           Debug.Trace


-- | Данный генератор создаёт список независимых по переменным функциональных блоков.
instance {-# OVERLAPS #-} ( Eq v
                          , Ord v
                          , Variables (FSet pu) v
                          , FunctionalSet pu, Arbitrary (FSet pu)
                          ) => Arbitrary [FSet pu] where
  arbitrary = onlyUniqueVar <$> listOf1 arbitrary
    where
      onlyUniqueVar = snd . foldl (\(used, fbs) fb -> let vs = variables fb
                                                      in if null (vs `intersection` used)
                                                        then ( vs `union` used, fb:fbs )
                                                        else ( used, fbs ) )
                                  (empty, [])


-- В значительной степени служебная функция, используемая для генерации процесса указанного
-- вычислительного блока под случайный алгоритм. Возвращает вычислительный блок со спланированым
-- вычислительным процессом и алгоритм.
processGen proxy = arbitrary >>= processGen' proxy def
  where
    processGen' :: ( DecisionProblem (EndpointDT String Int) EndpointDT pu
                   , ProcessUnit pu (Parcel String Int) Int
                   , WithFunctionalBlocks (FSet pu) (FB (Parcel String Int))
                   ) => Proxy pu -> pu -> [FSet pu] -> Gen (pu, [FB (Parcel String Int)])
    processGen' _ pu specialAlg = endpointWorkGen pu $ concatMap functionalBlocks specialAlg



data Opt a b = SchedOpt a | BindOpt b

-- | Автоматическое планирование вычислительного процесса, в рамках которого решения принимаются
-- случайным образом. В случае если какой-либо функциональный блок не может быть привязан к
-- вычислительному блоку (например по причине закончившихся внутренних ресурсов), то он просто
-- отбрасывается.
endpointWorkGen pu0 alg0 = endpointWorkGen' pu0 alg0 []
  where
    endpointWorkGen' pu alg passedAlg = do
      let opts = map SchedOpt (options endpointDT pu) ++ map BindOpt alg
      i <- choose (0 :: Int, length opts - 1)
      if null opts
        then return (pu, passedAlg)
        else case opts !! i of
          SchedOpt o -> do
            d <- passiveOption2action <$> endpointGen o
            endpointWorkGen' (decision endpointDT pu d) alg passedAlg
          BindOpt fb
            ->  let alg' = filter (/= fb) alg
                in case bind fb pu of
                  Right pu' -> endpointWorkGen' pu' alg' (fb : passedAlg)
                  Left _err -> endpointWorkGen' pu alg' passedAlg
      where
        endpointGen o@EndpointO{ epoRole=s@Source{} } = do
          vs' <- suchThat (sublistOf $ elems $ variables s) (not . null)
          return o{ epoRole=Source $ fromList vs' }
        endpointGen o = return o



-- | Генерация случайных входных данных для заданного алгорима.
--
-- TODO: Генерируемые значения должны типизироваться с учётом особенностей вычислительного блока.
inputsGen (pu, fbs) = do
  values <- infiniteListOf $ choose (0, 1000)
  let is = elems $ unionsMap inputs fbs
  return (pu, fbs, def{ cntxVars=M.fromList $ zip is (map (:[]) values) })


-- | Проверка вычислительного блока на соответсвие работы аппаратной реализации и его модельного
-- поведения.
prop_simulation n counter (pu, _fbs, values) = monadicIO $ do
  i <- run $ incrCounter 1 counter
  let path = joinPath ["hdl", "gen", n ++ show i]
  res <- run $ writeAndRunTestBench (Project n "../.." path pu) values
  assert res


-- | Формальнаяа проверка полноты выполнения работы вычислительного блока.
prop_completness (pu, fbs0)
  = let p = process pu
        processVars = unionsMap variables $ getEndpoints p
        algVars = unionsMap variables $ elems fbs
        fbs = fromList fbs0
        processFBs = fromList $ getFBs p
        in    processFBs == fbs -- функции в алгоритме соответствуют выполненным функциям в процессе
          && processVars == algVars -- пересылаемые данные в алгоритме соответствуют пересылаемым данным в процессе
          && null (options endpointDT pu)
          || trace (  "delta vars: " ++ show (algVars `difference` processVars) ++ "\n"
                    ++ "fbs: " ++ concatMap (\fb -> (if fb `elem` processFBs then "+" else "-") ++ "\t" ++ show fb ++ "\n" ) fbs ++ "\n"
                    ++ "fbs: " ++ show processFBs ++ "\n"
                    ++ "algVars: " ++ show algVars ++ "\n"
                    ++ "processVars: " ++ show processVars ++ "\n"
                    ++ show pu
                    ) False


unitTestbench title proxy cntx alg
  = let lib = joinPath ["..", ".."]
        wd = joinPath ["hdl", "gen", title]
        pu = bindAllAndNaiveSchedule alg (def `asProxyTypeOf` proxy)
    in writeAndRunTestBench (Project title lib wd pu) cntx @? title