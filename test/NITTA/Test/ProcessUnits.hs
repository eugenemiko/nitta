{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

module NITTA.Test.ProcessUnits
    ( unitTestBench
    , processUnitTests
    -- *Generators
    , inputsGen
    , processGen
    -- *Properties
    , isFinished
    , coSimulation
    ) where

import           Data.Atomics.Counter          (incrCounter)
import           Data.Default
import qualified Data.Map                      as M
import           Data.Proxy
import           Data.Set                      (difference, elems, empty,
                                                fromList, intersection, union)
import           Debug.Trace
import           NITTA.DataFlow                (endpointOption2action)
import           NITTA.Functions
import qualified NITTA.Functions               as F
import           NITTA.ProcessUnits.Divider
import           NITTA.ProcessUnits.Fram
import           NITTA.ProcessUnits.Multiplier
import           NITTA.Project
import           NITTA.Test.Functions          ()
import           NITTA.Test.LuaFrontend
import           NITTA.Test.Microarchitectures
import           NITTA.Types
import           NITTA.Types.Project
import           NITTA.Utils
import           System.FilePath.Posix         (joinPath)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.HUnit              (testCase, (@?))
import           Test.Tasty.QuickCheck         (Gen, arbitrary, testProperty)
import           Test.Tasty.TH
import           Text.InterpolatedString.Perl6 (qc)


test_fram =
    [ testCase "reg_out" $ unitTestBench "reg_out" proxy
        (Just def{ cntxVars=M.fromList [("aa", [42]), ("ac", [0x1003])] })
        [ F.reg "aa" ["ab"]
        , F.framOutput 9 "ac"
        ]
    , testCase "reg_constant" $ unitTestBench "reg_constant" proxy
        (Just def{ cntxVars=M.fromList [("dzw", [975])] })
        [ F.reg "dzw" ["act","mqt"]
        , F.constant 11 ["ovj"]
        ]
    , testProperty "isFinished" $ isFinished <$> gen
    , testProperty "coSimulation" $ fmap (coSimulation "prop_simulation_fram") $ inputsGen =<< gen
    ]
    where
        proxy = Proxy :: Proxy (Fram String Int Int)
        gen = processGen (def :: (Fram String Int Int))
            [ F <$> (arbitrary :: Gen (Constant String Int))
            , F <$> (arbitrary :: Gen (FramInput String Int))
            , F <$> (arbitrary :: Gen (FramOutput String Int))
            , F <$> (arbitrary :: Gen (Loop String Int))
            , F <$> (arbitrary :: Gen (Reg String Int))
            ]


test_shift =
    [ algTestCase "left_right" march
        [ F.loop 16 "g1" ["f1"]
        , F.shiftL "f1" ["g1"]
        , F.loop 16 "g2" ["f2"]
        , F.shiftR "f2" ["g2"]
        ]
    ]


test_multiplier =
    [ algTestCase "simple_mul" march
        [ F.constant 2 ["a"]
        , F.loop 1 "c" ["b"]
        , F.multiply "a" "b" ["c"]

        , F.constant 3 ["x"]
        , F.loop 1 "z" ["y"]
        , F.multiply "y" "x" ["z"]
        ]
    , testProperty "isFinished" $ isFinished <$> gen
    , testProperty "coSimulation" $ fmap (coSimulation "prop_simulation_multiplier") $ inputsGen =<< gen
    ]
    where
        gen = processGen (multiplier True)
            [ F <$> (arbitrary :: Gen (Multiply String Int))
            ]



test_divider =
    [ algTestCase "simple_div" march
        [ F.constant 100 ["a"]
        , F.loop 2 "e" ["b"]
        , F.division "a" "b" ["c"] ["d"]
        , F.add "c" "d" ["e"]

        , F.constant 200 ["a1"]
        , F.loop 2 "e1" ["b1"]
        , F.division "a1" "b1" ["c1"] ["d1"]
        , F.add "c1" "d1" ["e1"]
        ]
    , luaTestCase "single"
        [qc|function f(a)
                a, _b = a / 2
                f(a)
            end
            f(1024)
        |]
    , luaTestCase "pair"
        [qc|function f(a, b)
                a, _ = a / 2
                b, _ = b / 3
                f(a, b)
            end
            f(1024, 1024)
        |]
    -- FIXME: Auto text can't work correctly, because processGen don't take into account the
    -- facts that some variables may go out.
    -- , testProperty "isFinished" $ isFinished <$> dividerGen
    -- , testProperty "coSimulation" $ fmap (coSimulation "prop_simulation_divider") $ inputsGen =<< dividerGen
    ]
    where
        _gen = processGen (divider 4 True)
            [ F <$> (arbitrary :: Gen (Division String Int))
            ]



processUnitTests :: TestTree
processUnitTests = $(testGroupGenerator)


-----------------------------------------------------------


-- |В значительной степени служебная функция, используемая для генерации процесса указанного
-- вычислительного блока под случайный алгоритм. Возвращает вычислительный блок со спланированым
-- вычислительным процессом и алгоритм.
processGen pu gens = onlyUniqueVar <$> listOf1 (oneof gens) >>= processGen' pu
    where
        processGen' ::
            ( DecisionProblem (EndpointDT String Int) EndpointDT pu
            , ProcessUnit pu String Int Int
            ) => pu -> [F String Int] -> Gen (pu, [F String Int])
        processGen' pu' specialAlg = endpointWorkGen pu' specialAlg
        onlyUniqueVar = snd . foldl (\(used, fbs) fb -> let vs = variables fb
                                                        in if null (vs `intersection` used)
                                                            then ( vs `union` used, fb:fbs )
                                                            else ( used, fbs ) )
                                    (empty, [])



data Opt a b = SchedOpt a | BindOpt b

-- |Автоматическое планирование вычислительного процесса, в рамках которого решения принимаются
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
                        d <- endpointOption2action <$> endpointGen o
                        endpointWorkGen' (decision endpointDT pu d) alg passedAlg
                    BindOpt fb
                        ->  let alg' = filter (/= fb) alg
                            in case tryBind fb pu of
                                Right pu' -> endpointWorkGen' pu' alg' (fb : passedAlg)
                                Left _err -> endpointWorkGen' pu alg' passedAlg
            where
                endpointGen o@EndpointO{ epoRole=s@Source{} } = do
                    vs' <- suchThat (sublistOf $ elems $ variables s) (not . null)
                    return o{ epoRole=Source $ fromList vs' }
                endpointGen o = return o



-- |Генерация случайных входных данных для заданного алгорима.
inputsGen (pu, fbs) = do
    values <- infiniteListOf $ choose (0, 1000 :: Int)
    let is = elems $ unionsMap inputs fbs
    return (pu, fbs, Just def{ cntxVars=M.fromList $ zip is (map (:[]) values) })


-- |Проверка вычислительного блока на соответсвие работы аппаратной реализации и его модельного
-- поведения.
coSimulation n (pu, _fbs, values) = monadicIO $ do
    i <- run $ incrCounter 1 externalTestCntr
    let path = joinPath ["hdl", "gen", n ++ "_" ++ show i]
    res <- run $ writeAndRunTestBench $ Project n "../.." path pu values [Makefile]
    assert $ tbStatus res


-- |Формальнаяа проверка полноты выполнения работы вычислительного блока.
isFinished (pu, fbs0)
    = let
        p = process pu
        processVars = unionsMap variables $ getEndpoints p
        algVars = unionsMap variables $ elems fbs
        fbs = fromList fbs0
        processFBs = fromList $ getFBs p
    in processFBs == fbs -- функции в алгоритме соответствуют выполненным функциям в процессе
        && processVars == algVars -- пересылаемые данные в алгоритме соответствуют пересылаемым данным в процессе
        && null (options endpointDT pu)
        || trace (  "delta vars: " ++ show (algVars `difference` processVars) ++ "\n"
                ++ "fbs: " ++ concatMap (\fb -> (if fb `elem` processFBs then "+" else "-") ++ "\t" ++ show fb ++ "\n" ) fbs ++ "\n"
                ++ "fbs: " ++ show processFBs ++ "\n"
                ++ "algVars: " ++ show algVars ++ "\n"
                ++ "processVars: " ++ show processVars ++ "\n"
                ) False


unitTestBench title proxy cntx alg = do
    let
        lib = joinPath ["..", ".."]
        wd = joinPath ["hdl", "gen", title]
        pu = bindAllAndNaiveSchedule alg (def `asProxyTypeOf` proxy)
    (tbStatus <$> writeAndRunTestBench (Project title lib wd pu cntx [Makefile])) @? title



-- |Выполнить привязку списка функциональных блоков к указанному вычислительному блоку и наивным
-- образом спланировать вычислительный процесс.
bindAllAndNaiveSchedule alg pu0 = naiveSchedule $ foldl (flip bind) pu0 alg
    where
        naiveSchedule pu
            | opt : _ <- options endpointDT pu = naiveSchedule $ decision endpointDT pu $ endpointOption2action opt
            | otherwise = pu
