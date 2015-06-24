{-# LANGUAGE RecordWildCards #-}

import Sprockell.System
import Control.Monad

type TestSuite = (String, Int, [Instruction], SystemState -> String)

getRegs :: SystemState -> Int -> [Reg] -> [Value]
getRegs sysState spid = getRegs' (regbank (sprs sysState !! spid))
    where getRegs' rs = map (rs !)

-- Can we write to registers and does zero stay zero? --
writeRegProg :: [Instruction]
writeRegProg = [Const 10 RegA, Const 11 RegB, Const 15 Zero, EndProg]
writeRegSuite :: (String, Int, [Instruction], SystemState -> String)
writeRegSuite = ("RegTest", 1, writeRegProg, writeRegTest)

writeRegTest :: SystemState -> String
writeRegTest sysState
        | getRegs sysState 0 [RegA, RegB, Zero] == [10, 11, 0] = "OK"
        | otherwise = "FAIL"

-- Does computing work? --
computeProg :: [Instruction]
computeProg = [Const 3 RegA, Const 2 RegB, Compute Mul RegA RegB RegC, EndProg]
computeSuite :: (String, Int, [Instruction], SystemState -> String)
computeSuite = ("ComputeTest", 1, computeProg, computeTest)

computeTest :: SystemState -> String
computeTest sysState
        | getRegs sysState 0  [RegA, RegB, RegC] == [3, 2, 6] = "OK"
        | otherwise = "FAIL"

-- Indirect Load --
indirectLoadSuite :: (String, Int, [Instruction], SystemState -> String)
indirectLoadSuite = ("IndirectLoadTest", 1, indirectLoadProg, indirectLoadTest)
  where
    indirectLoadProg =
      [ Const 2 RegA
      , Const 3 RegB
      , Store RegA (Addr 3)
      , Load (Deref RegB) RegC
      , EndProg
      ]

indirectLoadTest :: SystemState -> String
indirectLoadTest sysState
         | getRegs sysState 0 [RegA, RegB, RegC] == [2, 3, 2] = "OK"
         | otherwise = "FAIL"

-- Write to Zero
writeZeroProg :: [Instruction]
writeZeroProg = [Const 2 Zero, Compute Add Zero RegA RegA, EndProg]
writeZeroSuite :: (String, Int, [Instruction], SystemState -> String)
writeZeroSuite = ("ZeroTest", 1, writeZeroProg, writeZeroTest)

writeZeroTest :: SystemState -> String
writeZeroTest sysState
        | getRegs sysState 0 [RegA, Zero] == [0, 0] = "OK"
        | otherwise = "FAIL"

-- Indirect Store --
indirectStoreSuite :: (String, Int, [Instruction], SystemState -> String)
indirectStoreSuite = ("IndirectStoreTest", 1, indirectStoreProg, indirectStoreTest)
  where
    indirectStoreProg =
      [ Const 2 RegA
      , Const 3 RegB
      , Store RegA (Deref RegB)
      , Load (Addr 3) RegC
      , EndProg
      ]

indirectStoreTest :: SystemState -> String
indirectStoreTest sysState
         | getRegs sysState 0 [RegA, RegB, RegC] == [2, 3, 2] = "OK"
         | otherwise = "FAIL"

-- Check the value of local mem which was not previously written to --
unwrittenLocalSuite :: (String, Int, [Instruction], SystemState -> String)
unwrittenLocalSuite = ("UnwrittenLocalTest", 1, unwrittenLocalProg, unwrittenLocalTest)
  where
    unwrittenLocalProg =
      [ Const 2 RegA
        , Const 3 RegB
        , Load (Addr 0) RegA
        , Load (Deref RegB) RegB
        , EndProg
        ]
unwrittenLocalTest :: SystemState -> String
unwrittenLocalTest sysState
         | getRegs sysState 0 [RegA, RegB] == [0, 0] = "OK"
         | otherwise = "FAIL"

-- Check the value of shared mem which was not previously written to --
unwrittenSharedSuite :: (String, Int, [Instruction], SystemState -> String)
unwrittenSharedSuite = ("UnwrittenSharedTest", 1, unwrittenSharedProg, unwrittenSharedTest)
  where
    unwrittenSharedProg =
        [ Const 2 RegA
        , Const 3 RegB
        , Read (Addr 0)
        , Receive RegA
        , Read (Deref RegB)
        , Receive RegB
        , EndProg
        ]
unwrittenSharedTest :: SystemState -> String
unwrittenSharedTest sysState
         | getRegs sysState 0 [RegA, RegB] == [0, 0] = "OK"
         | otherwise = "FAIL"

testAndSetSuite :: (String, Int, [Instruction], SystemState -> String)
testAndSetSuite = ("TestAndSetTest", 1, testAndSetProg, testAndSetTest)
  where
    testAndSetProg =
      [ TestAndSet (Addr 0)
      , Receive RegA
      , TestAndSet (Addr 0)
      , Receive RegB
      , EndProg
      ]

testAndSetTest :: SystemState -> String
testAndSetTest sysState@SysState{ .. }
         | (sharedMem !!! 0) == 1 && getRegs sysState 0 [RegA, RegB] == [1, 0] = "OK"
         | otherwise             = "Fail"




-- Running test logic --
runSuite :: TestSuite -> IO ()
runSuite (name, nSprockells, prog, test) = do
    putStr name >> putStr " (n=" >> putStr (show nSprockells) >> putStr "): "
    liftM test (run nSprockells prog) >>= putStr
    putChar '\n'
    return ()

main :: IO()
main = do
    runSuite writeRegSuite
    runSuite computeSuite
    runSuite writeZeroSuite
    runSuite indirectLoadSuite
    runSuite indirectStoreSuite
    runSuite unwrittenLocalSuite
    runSuite unwrittenSharedSuite
    runSuite testAndSetSuite
    return ()
