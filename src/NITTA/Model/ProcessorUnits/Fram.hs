{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Model.ProcessorUnits.Fram
Description : Register file
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Fram
  ( Fram(..)
  , Ports(..), IOPorts(..)
  ) where

import           Control.Applicative              ((<|>))
import           Control.Monad
import qualified Data.Array                       as A
import           Data.Bits                        (finiteBitSize, testBit)
import           Data.Default
import qualified Data.List                        as L
import           Data.Maybe
import qualified Data.Set                         as S
import qualified Data.String.Utils                as S
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.Problems.Refactor
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Snippets
import           NITTA.Project.Types
import           NITTA.Utils
import           NITTA.Utils.ProcessDescription
import           Numeric.Interval                 (inf, sup, (...))
import           Text.InterpolatedString.Perl6    (qc)



data Fram v x t = Fram
    { memory     :: A.Array Int (Cell v x t) -- ^memory cell array
    , remainRegs :: [ (Reg v x, Job v x t) ] -- ^register queue
    , process_   :: Process v x t
    , size       :: Int -- ^memory size
    } deriving ( Show )

instance ( Default t, Default x
        ) => Default (Fram v x t) where
    def = Fram
        { memory=A.listArray (0, defaultSize - 1) $ repeat def
        , remainRegs=[]
        , process_=def
        , size=defaultSize
        }
        where
            defaultSize = 16

instance ( VarValTime v x t
        ) => WithFunctions (Fram v x t) (F v x) where
    functions Fram{ remainRegs, memory }
        = map (F . fst) remainRegs ++ concatMap functions (A.elems memory)

instance ( VarValTime v x t
        ) => Variables (Fram v x t) v where
    variables fram = S.unions $ map variables $ functions fram


-- |Memory cell
data Cell v x t = Cell
        { state        :: CellState v x t
        , lastWrite    :: Maybe t
        , job          :: Maybe (Job v x t) -- ^current job description
        , history      :: [ F v x ]
        , initialValue :: x
        }
    deriving ( Show )


data Job v x t = Job
        { function         :: F v x
        , startAt          :: Maybe t
        , binds, endpoints :: [ ProcessUid ]
        }
    deriving ( Show, Eq )

defJob f = Job
    { function=f
    , startAt=Nothing
    , binds=[]
    , endpoints=[]
    }


instance WithFunctions (Cell v x t) (F v x) where
    functions Cell{ history, job=Just Job{ function } } = function : history
    functions Cell{ history }                           = history

instance ( Default x ) => Default (Cell v x t) where
    def = Cell
        { state=NotUsed
        , lastWrite=Nothing
        , job=Nothing
        , history=[]
        , initialValue=def
        }


-- |Memory cell states. Add Loop&Reg for optimisation.
-- @
--                bind                    source
--     NotUsed ----------> DoConstant ------------+----> Done
--      |                        ^                |
--      |                        |                |
--      |                        \----------------/
--      |
--      |    bind
--      +-------------------> ForReg <-------------\
--      |                         |                |
--      |                         |                |
--      |                  target |                |
--      |                         |     /----------+
--      |                         |     |          |
--      |    target               v     v  source  |
--      +-------------------> DoReg ---------------/
--      |
--      |              refactor              source                      target
--      \-- NotBrokenLoop --> DoLoopSource ----------+---> DoLoopTarget --------> Done
--                                ^                  |
--                                |                  |
--                                \------------------/
-- @
data CellState v x t
    = NotUsed | Done
    | DoConstant [v]
    | DoReg [v] | ForReg
    | NotBrokenLoop | DoLoopSource [v] (Job v x t) | DoLoopTarget v
    deriving ( Show, Eq )


isFree Cell{ state=NotUsed } = True
isFree _                     = False

isForReg Cell{ state=ForReg } = True
isForReg _                    = False

lockableNotUsedCell Fram{ memory, remainRegs } = let
        free = filter (isFree . snd) $ A.assocs memory
        n = length free
    in if null remainRegs && n >= 1 || not (null remainRegs) && n >= 2
        then Just $ head free
        else Nothing

findForRegCell Fram{ memory }
    = case L.find (isForReg . snd) $ A.assocs memory of
        x@(Just _) -> x
        Nothing    -> L.find (isFree . snd) $ A.assocs memory


oJobV Job{ function }
    | Just (LoopIn _ (I v)) <- castF function = v
    | otherwise = undefined


instance ( VarValTime v x t
         ) => ProcessorUnit (Fram v x t) v x t where
    tryBind f fram
        | not $ null (variables f `S.intersection` variables fram)
        = Left "can not bind (self transaction)"

    tryBind f fram@Fram{ memory, remainRegs }
        | Just (Constant (X x) (O vs)) <- castF f
        , Just (addr, _) <- lockableNotUsedCell fram
        , let
            (binds, process_) = runSchedule fram $ scheduleFunctionBind f
            cell = Cell
                { state=DoConstant $ S.elems vs
                , job=Just (defJob f){ binds }
                , history=[ f ]
                , lastWrite=Nothing
                , initialValue=x
                }
        = Right fram
            { memory=memory A.// [ (addr, cell) ]
            , process_
            }

        | Just (Loop (X x) (O _) (I _)) <- castF f
        , Just (addr, _) <- lockableNotUsedCell fram
        , let
            (binds, process_) = runSchedule fram $ scheduleFunctionBind f
            cell = Cell
                { state=NotBrokenLoop
                , job=Just (defJob f){ binds }
                , history=[ f ]
                , lastWrite=Nothing
                , initialValue=x
                }
        = Right fram
            { memory=memory A.// [ (addr, cell) ]
            , process_
            }

        | Just r@Reg{} <- castF f
        , any (\case ForReg{} -> True; DoReg{} -> True; NotUsed{} -> True; _ -> False) $ map state $ A.elems memory
        , let
            (binds, process_) = runSchedule fram $ scheduleFunctionBind f
            job = (defJob f){ binds }
        = Right fram
            { remainRegs=(r, job) : remainRegs
            , process_
            }

        | otherwise = Left $ "unsupport or cells over: " ++ show f

    process Fram{ process_ } = process_
    setTime t fram@Fram{ process_ } = fram{ process_=process_{ nextTick=t } }


instance ( Var v ) => Locks (Fram v x t) v where
    -- FIXME:
    locks _ = []

instance ( VarValTime v x t ) => RefactorProblem (Fram v x t) v x where
    refactorOptions Fram{ memory } =
        [ BreakLoop x o i
        | (_, Cell{ state=NotBrokenLoop, job=Just Job{ function } }) <- A.assocs memory
        , let Just (Loop (X x) (O o) (I i)) = castF function
        ]
    refactorDecision fram@Fram{ memory } bl@BreakLoop{ loopO } = let
            Just ( addr, cell@Cell{ history, job=Just Job{ binds } } )
                = L.find (\case
                    (_, Cell{job=Just Job{ function } }) -> function == F (recLoop bl)
                    _ -> False
                    ) $ A.assocs memory
            ((iPid, oPid), process_) = runSchedule fram $ do
                revoke <- scheduleFunctionRevoke $ recLoop bl
                f1 <- scheduleFunctionBind $ F $ recLoopOut bl
                f2 <- scheduleFunctionBind $ F $ recLoopIn bl
                establishVerticalRelations binds (f1 ++ f2 ++ revoke)
                return (f1, f2)
            iJob = (defJob $ F $ recLoopOut bl){ binds=iPid, startAt=Just 0 }
            oJob = (defJob $ F $ recLoopIn bl){ binds=oPid }
            cell' = cell
                { job=Just iJob
                , history=[ F $ recLoopOut bl, F $ recLoopIn bl ] ++ history
                , state=DoLoopSource (S.elems loopO) oJob
                }
        in fram
            { memory=memory A.// [ (addr, cell') ]
            , process_
            }
    refactorDecision _ d = error $ "fram not suport refactor: " ++ show d

instance ( VarValTime v x t
        ) => EndpointProblem (Fram v x t) v t
        where
    endpointOptions Fram{ process_=Process{ nextTick }, remainRegs, memory } = let
            target v = EndpointO (Target v) $ TimeConstrain (nextTick ... maxBound) (1 ... maxBound)
            source True vs = EndpointO (Source $ S.fromList vs) $ TimeConstrain (1 + 1 + nextTick ... maxBound) (1 ... maxBound)
            source False vs = EndpointO (Source $ S.fromList vs) $ TimeConstrain (1 + nextTick ... maxBound) (1 ... maxBound)

            fromRemain = if any (\case ForReg{} -> True; NotUsed{} -> True; _ -> False) $ map state $ A.elems memory
                then map ( \(Reg (I v) (O _)) -> target v ) $ map fst remainRegs
                else []

            foo Cell{ state=NotUsed } = Nothing
            foo Cell{ state=Done } = Nothing

            foo Cell{ state=DoConstant vs } = Just $ source False vs

            foo Cell{ state=DoReg vs, lastWrite } = Just $ source (fromMaybe 0 lastWrite == nextTick - 1) vs
            foo Cell{ state=ForReg } = Nothing

            foo Cell{ state=NotBrokenLoop } = Nothing
            foo Cell{ state=DoLoopSource vs _, lastWrite } = Just $ source (fromMaybe 0 lastWrite == nextTick - 1) vs
            foo Cell{ state=DoLoopTarget v } = Just $ target v

            fromCells = mapMaybe foo $ A.elems memory
        in fromRemain ++ fromCells


    -- Constant
    endpointDecision fram@Fram{ memory } d@EndpointD{ epdRole=Source vs, epdAt }
        | Just ( addr, cell@Cell{ state=DoConstant vs', job=Just Job{ function, binds, endpoints } } )
            <- L.find (\case
                (_, Cell{ state=DoConstant vs' }) -> (vs' L.\\ S.elems vs) /= vs'
                _ -> False
                ) $ A.assocs memory
        , let
            vsRemain = vs' L.\\ S.elems vs
            ( (), process_' ) = runSchedule fram $ do
                updateTick (sup epdAt + 1)
                endpoints' <- scheduleEndpoint d $ scheduleInstruction (shiftI (-1) epdAt) $ ReadCell addr
                when (null vsRemain) $ do
                    fPID <- scheduleFunction (0 ... sup epdAt) function
                    establishVerticalRelations binds fPID
                    establishVerticalRelations fPID (endpoints ++ endpoints')
            cell' = case vsRemain of
                    [] -> cell
                        { job=Nothing
                        , state=Done
                        }
                    _ -> cell
                        { state=DoConstant vsRemain
                        }
        = fram
            { memory=memory A.// [ (addr, cell') ]
            , process_=process_'
            }

    -- Loop
    endpointDecision fram@Fram{ memory } d@EndpointD{ epdRole=Source vs, epdAt }
        | Just ( addr, cell@Cell{ state=DoLoopSource vs' oJob, job=Just job@Job{ binds, function, startAt, endpoints } } )
            <- L.find (\case
                (_, Cell{ state=DoLoopSource vs' _ }) -> (vs' L.\\ S.elems vs) /= vs'
                _ -> False
                ) $ A.assocs memory
        , let
            vsRemain = vs' L.\\ S.elems vs
            (endpoints', process_) = runSchedule fram $ do
                updateTick (sup epdAt + 1)
                eps <- scheduleEndpoint d $ scheduleInstruction (shiftI (-1) epdAt) $ ReadCell addr
                when (null vsRemain) $ do
                    fPID <- scheduleFunction (0 ... sup epdAt) function
                    establishVerticalRelations binds fPID
                    establishVerticalRelations fPID $ eps ++ endpoints
                return eps
            cell' = if not $ null vsRemain
                then cell
                    { job=Just job{ startAt=startAt <|> (Just $ inf epdAt - 1), endpoints=endpoints' ++ endpoints }
                    , state=DoLoopSource vsRemain oJob
                    }
                else cell
                    { job=Just oJob{ startAt=startAt <|> (Just $ inf epdAt - 1) }
                    , state=DoLoopTarget $ oJobV oJob
                    }
        = fram{ process_, memory=memory A.// [ (addr, cell') ] }

    endpointDecision fram@Fram{ memory } d@EndpointD{ epdRole=Target v, epdAt }
        | Just ( addr, cell@Cell{ job=Just Job{ function, binds, endpoints } } )
            <- L.find (\case (_, Cell{ state=DoLoopTarget v' }) -> v == v'; _ -> False) $ A.assocs memory
        , let
            ((), process_) = runSchedule fram $ do
                endpoints' <- scheduleEndpoint d $ scheduleInstruction epdAt $ WriteCell addr
                updateTick (sup epdAt + 1)
                fPID <- scheduleFunction epdAt function
                establishVerticalRelations binds fPID
                establishVerticalRelations fPID (endpoints ++ endpoints')
            cell' = cell
                { job=Nothing
                , state=Done
                }
        = fram
            { memory=memory A.// [ (addr, cell') ]
            , process_
            }

    -- Reg Target
    endpointDecision fram@Fram{ memory, remainRegs } d@EndpointD{ epdRole=Target v, epdAt }
        | Just ( addr, cell@Cell{ history } ) <- findForRegCell fram
        , ([ ( Reg (I _) (O vs), j@Job{ function } ) ], remainRegs' ) <- L.partition (\(Reg (I v') (O _), _) -> v' == v) remainRegs
        , let
            (endpoints, process_) = runSchedule fram $ do
                updateTick (sup epdAt + 1)
                scheduleEndpoint d $ scheduleInstruction epdAt $ WriteCell addr
            cell' = cell
                { job=Just j{ startAt=Just $ inf epdAt, endpoints }
                , state=DoReg $ S.elems vs
                , lastWrite=Just $ sup epdAt
                , history=function : history
                }
        = fram
            { memory=memory A.// [ (addr, cell') ]
            , remainRegs=remainRegs'
            , process_
            }

    endpointDecision fram@Fram{ memory } d@EndpointD{ epdRole=Source vs, epdAt }
        | Just ( addr, cell@Cell{ state=DoReg vs', job=Just Job{ function, startAt=Just fBegin, binds, endpoints } } )
            <- L.find (\case
                (_, Cell{ state=DoReg vs' }) -> (vs' L.\\ S.elems vs) /= vs'
                _ -> False
                ) $ A.assocs memory
        , let
            vsRemain = vs' L.\\ S.elems vs
            ( (), process_ ) = runSchedule fram $ do
                updateTick (sup epdAt + 1)
                endpoints' <- scheduleEndpoint d $ scheduleInstruction (shiftI (-1) epdAt) $ ReadCell addr
                when (null vsRemain) $ do
                    fPID <- scheduleFunction (fBegin ... sup epdAt) function
                    establishVerticalRelations binds fPID
                    establishVerticalRelations fPID (endpoints ++ endpoints')
            cell' = case vsRemain of
                    [] -> cell
                        { job=Nothing
                        , state=ForReg
                        }
                    _ -> cell
                        { state=DoReg vsRemain
                        }
        = fram
            { memory=memory A.// [ (addr, cell') ]
            , process_
            }

    endpointDecision Fram{ memory } d
        = error $ "fram model internal error: "
            ++ show d ++ "\n cells state: \n"
            ++ S.join "\n" (map (\(i, c) -> show i ++ ": " ++ show (state c)) $ A.assocs memory)



---------------------------------------------------------------------


instance Controllable (Fram v x t) where
    data Instruction (Fram v x t)
        = ReadCell Int
        | WriteCell Int
        deriving (Show)

    data Microcode (Fram v x t) = Microcode
            { oeSignal :: Bool
            , wrSignal :: Bool
            , addrSignal :: Maybe Int
            }
        deriving (Show, Eq, Ord)

    mapMicrocodeToPorts Microcode{ oeSignal, wrSignal, addrSignal } FramPorts{ oe, wr, addr } =
        [ (oe, Bool oeSignal)
        , (wr, Bool wrSignal)
        ] ++ addrs
        where
            addrs = map (\(linkId, i) -> ( linkId
                                        , maybe Undef Bool $ fmap (`testBit` i) addrSignal
                                        )
                        ) $ zip (reverse addr) [0..]

    portsToSignals FramPorts{ oe, wr, addr } = oe : wr : addr

    signalsToPorts (oe:wr:ss) = FramPorts{ oe, wr, addr=take 4 ss }
    signalsToPorts _          = error "pattern match error in signalsToPorts Fram"


instance Connected (Fram v x t) where
    data Ports (Fram v x t)
        = FramPorts
            { oe, wr :: SignalTag
            , addr :: [SignalTag]
            }
        deriving ( Show )

instance IOConnected (Fram v x t) where
    data IOPorts (Fram v x t) = FramIO
        deriving ( Show )

getAddr (ReadCell addr)  = addr
getAddr (WriteCell addr) = addr


instance Default (Microcode (Fram v x t)) where
    def = Microcode False False Nothing

instance UnambiguouslyDecode (Fram v x t) where
    decodeInstruction (ReadCell addr)  = Microcode True False $ Just addr
    decodeInstruction (WriteCell addr) = Microcode False True $ Just addr



instance ( VarValTime v x t
         ) => Simulatable (Fram v x t) v x where
    simulateOn cntx _fram f
        | Just f'@Constant{} <- castF f = simulate cntx f'
        | Just f'@Loop{} <- castF f = simulate cntx f'
        | Just f'@Reg{} <- castF f = simulate cntx f'
        | otherwise = error $ "Can't simulate " ++ show f ++ " on Fram."


---------------------------------------------------

instance ( VarValTime v x t
         ) => Testable (Fram v x t) v x where
    testBenchImplementation Project{ pName, pUnit=fram@Fram{ process_=Process{ steps } }, pTestCntx=Cntx{ cntxProcess } }
        = Immediate (moduleName pName fram ++ "_tb.v") testBenchImp
        where
            cntx = head cntxProcess

            hardwareInstance' = hardwareInstance pName fram
                TargetEnvironment
                    { signalClk="clk"
                    , signalRst="rst"
                    , signalCycle="cycle"
                    , inputPort=undefined
                    , outputPort=undefined
                    , inoutPort=undefined
                    , unitEnv=ProcessUnitEnv
                        { parameterAttrWidth=IntParam 4
                        , dataIn="data_in"
                        , attrIn="attr_in"
                        , dataOut="data_out"
                        , attrOut="attr_out"
                        , signal= \(SignalTag i) -> case i of
                            0 -> "oe"
                            1 -> "wr"
                            j -> "addr[" ++ show (3 - (j - 2)) ++ "]"
                        }
                    }
                FramPorts
                    { oe=SignalTag 0
                    , wr=SignalTag 1
                    , addr=map SignalTag [ 2, 3, 4, 5 ]
                    }
                FramIO
            testBenchImp = codeBlock [qc|
                {"module"} { moduleName pName fram }_tb();
                parameter DATA_WIDTH = { finiteBitSize (def :: x) };
                parameter ATTR_WIDTH = 4;

                /*
                Context:
                { show cntx }

                Algorithm:
                { unlines $ map show $ functions fram }

                Process:
                { unlines $ map show steps }
                */
                reg clk, rst, wr, oe;
                reg [3:0] addr;
                reg [DATA_WIDTH-1:0]  data_in;
                reg [ATTR_WIDTH-1:0]  attr_in;
                wire [DATA_WIDTH-1:0] data_out;
                wire [ATTR_WIDTH-1:0] attr_out;

                { inline $ hardwareInstance' }

                { inline $ snippetDumpFile $ moduleName pName fram }
                { inline $ snippetClkGen }

                initial
                  begin
                    $dumpfile("{ moduleName pName fram }_tb.vcd");
                    $dumpvars(0, { moduleName pName fram }_tb);
                    @(negedge rst);
                    forever @(posedge clk);
                  end

                { inline $ snippetInitialFinish $ controlSignals fram }
                { inline $ snippetInitialFinish $ testDataInput fram cntx }
                { inline $ snippetInitialFinish $ testDataOutput pName fram cntx }

                endmodule
                |]

controlSignals fram@Fram{ process_=Process{ nextTick } }
    = concatMap ( ("      " ++) . (++ " @(posedge clk)\n") . showMicrocode . microcodeAt fram) [ 0 .. nextTick + 1 ]
    where
        showMicrocode Microcode{ oeSignal, wrSignal, addrSignal } = concat
            [ "oe <= ", bool2verilog oeSignal, "; "
            , "wr <= ", bool2verilog wrSignal, "; "
            , "addr <= ", maybe "0" show addrSignal, "; "
            ]



testDataInput Fram{ process_=p@Process{ nextTick } } cntx
    = concatMap ( ("      " ++) . (++ " @(posedge clk);\n") . busState ) [ 0 .. nextTick + 1 ]
    where
        busState t
            | Just (Target v) <- endpointAt t p
            = "data_in <= " ++ (either (error . ("testDataInput: " ++)) show $ getX cntx v) ++ ";"
            | otherwise = "/* NO INPUT */"

testDataOutput tag fram@Fram{ memory, process_=p@Process{ nextTick, steps } } cntx
    = concatMap ( ("      @(posedge clk); " ++) . (++ "\n") . busState ) [ 0 .. nextTick + 1 ] ++ bankCheck
    where
        busState t
            | Just (Source vs) <- endpointAt t p, let v = oneOf vs
            = checkBus vs $ either (error . ("testDataOutput: " ++) ) show (getX cntx v)
            | otherwise
            = "$display( \"data_out: %d\", data_out ); "

        checkBus vs value = concat
            [ "$write( \"data_out: %d == %s\t(%s)\", data_out, " ++ show value ++ ", " ++ S.join ", " (map show $ S.elems vs) ++ " ); "
            ,  "if ( !( data_out === " ++ value ++ " ) ) "
            ,   "$display(\" FAIL\");"
            ,  " else $display();"
            ]

        bankCheck
            = "\n      @(posedge clk);\n"
            ++ unlines [ "  " ++ checkBank addr v (either (error . ("bankCheck: " ++)) show $ getX cntx v)
                        | FStep f <- filter (\case FStep{} -> True; _ -> False) $ map (descent . sDesc) steps
                        , let addr_v = outputStep fram f
                        , isJust addr_v
                        , let Just (addr, v) = addr_v
                        ]
        outputStep pu' f
            | Just (Loop _ _bs (I v)) <- castF f = Just (findAddress v pu', v)
            | Just (LoopIn l (I v)) <- castF f
            , Just (addr, _) <- L.find (L.elem (F l) . history . snd) $ A.assocs memory
            = Just (addr, v)
            | otherwise = Nothing

        checkBank addr v value = concatMap ("    " ++)
            [ "if ( !( " ++ tag ++ ".bank[" ++ show addr ++ "] === " ++ show value ++ " ) ) "
            ,   "$display("
            ,     "\""
            ,       "FAIL wrong value of " ++ show' v ++ " in fram bank[" ++ show' addr ++ "]! "
            ,       "(got: %h expect: %h)"
            ,     "\","
            ,     "data_out, " ++ show value
            ,   ");"
            ]
        show' s = filter (/= '\"') $ show s


findAddress var fram@Fram{ process_=Process{ steps } }
    | [ time ] <- variableSendAt var
    , [ instr ] <- map getAddr $ extractInstructionAt fram (inf time)
    = instr
    | otherwise = error $ "Can't find instruction for effect of variable: " ++ show var ++ " " ++ show steps
    where
        variableSendAt v = [ sTime | Step{ sTime, sDesc=info } <- steps
                           , v `elem` f info
                           ]
        f (EndpointRoleStep rule) = variables rule
        f _                       = S.empty


softwareFile tag pu = moduleName tag pu ++ "." ++ tag ++ ".dump"

instance ( VarValTime v x t ) => TargetSystemComponent (Fram v x t) where
    moduleName _ _ = "pu_fram"
    hardware tag pu = FromLibrary $ moduleName tag pu ++ ".v"
    software tag fram@Fram{ memory }
        = Immediate
            (softwareFile tag fram)
            $ unlines $ map
                (\Cell{ initialValue=initialValue } -> hdlValDump initialValue)
                $ A.elems memory
    hardwareInstance tag fram@Fram{ size } TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk } FramPorts{..} FramIO
        = codeBlock [qc|
            pu_fram #
                    ( .DATA_WIDTH( { finiteBitSize (def :: x) } )
                    , .ATTR_WIDTH( { show parameterAttrWidth } )
                    , .RAM_SIZE( { show size } )
                    , .FRAM_DUMP( "$path${ softwareFile tag fram }" )
                    ) { tag }
                ( .clk( { signalClk } )
                , .signal_addr( \{ { S.join ", " (map signal addr) } } )
                , .signal_wr( { signal wr } )
                , .data_in( { dataIn } )
                , .attr_in( { attrIn } )
                , .signal_oe( { signal oe } )
                , .data_out( { dataOut } )
                , .attr_out( { attrOut } )
                );
            |]
    hardwareInstance _title _pu TargetEnvironment{ unitEnv=NetworkEnv{} } _ports _io
        = error "Should be defined in network."

instance IOTestBench (Fram v x t) v x
