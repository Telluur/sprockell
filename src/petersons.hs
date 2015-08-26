{-# LANGUAGE RecordWildCards #-}
import Sprockell.System

prog :: [Instruction]
prog = [

     Const 0 RegA,
     Branch SPID (Rel (3)),
     Write RegA (Addr 1),
     Jump (Rel (3)),
     Read (Addr 1),
     Receive RegA,
     Store SPID (Addr 0),
     Const 0 RegA,
     Branch SPID (Rel (3)),
     Write RegA (Addr 2),
     Jump (Rel (3)),
     Read (Addr 2),
     Receive RegA,
     Const 0 RegA,
     Branch SPID (Rel (3)),
     Write RegA (Addr 3),
     Jump (Rel (3)),
     Read (Addr 3),
     Receive RegA,
     Const 1 RegA,
     Branch SPID (Rel (3)),
     Write RegA (Addr 4),
     Jump (Rel (3)),
     Read (Addr 4),
     Receive RegA,
     Const 1 RegA,
     Branch SPID (Rel (3)),
     Write RegA (Addr 5),
     Jump (Rel (3)),
     Read (Addr 5),
     Receive RegA,
     Const 0 RegA,
     Branch SPID (Rel (3)),
     Write RegA (Addr 6),
     Jump (Rel (3)),
     Read (Addr 6),
     Receive RegA,
     Const 0 RegA,
     Store RegA (Addr 1),
     Load (Addr 1) RegA,
     Const 20 RegB,
     Compute Lt RegA RegB RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (52)),
     TestAndSet (Addr 0),
     Receive RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (-3)),
     Const 2 RegA,
     Load (Addr 0) RegB,
     Compute Add RegA RegB RegB,
     Const 1 RegA,
     Write RegA (Deref RegB),
     Const 1 RegA,
     Load (Addr 0) RegB,
     Compute Sub RegA RegB RegA,
     Write RegA (Addr 6),
     Write Zero (Addr 0),
     Const 1 RegA,
     Load (Addr 0) RegB,
     Compute Sub RegA RegB RegA,
     Const 2 RegB,
     Compute Add RegB RegA RegA,
     Read (Deref RegA),
     Receive RegB,
     Const 1 RegA,
     Load (Addr 0) RegC,
     Compute Sub RegA RegC RegA,
     Read (Addr 6),
     Receive RegC,
     Compute Equal RegC RegA RegC,
     Compute And RegB RegC RegB,
     Branch RegB (Rel (2)),
     Jump (Rel (2)),
     Jump (Rel (-16)),
     Read (Addr 1),
     Receive RegA,
     Const 1 RegB,
     Compute Add RegA RegB RegA,
     Write RegA (Addr 1),
     TestAndSet (Addr 0),
     Receive RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (-3)),
     Const 2 RegA,
     Load (Addr 0) RegB,
     Compute Add RegA RegB RegB,
     Const 0 RegA,
     Write RegA (Deref RegB),
     Write Zero (Addr 0),
     Load (Addr 1) RegA,
     Const 1 RegB,
     Compute Add RegA RegB RegA,
     Store RegA (Addr 1),
     Jump (Rel (-55)),
     Const 4 RegA,
     Load (Addr 0) RegB,
     Compute Add RegA RegB RegB,
     Const 0 RegA,
     Write RegA (Deref RegB),
     Load (Addr 0) RegA,
     Const 0 RegB,
     Compute Equal RegA RegB RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (18)),
     Const 4 RegA,
     Const 0 RegB,
     Compute Add RegA RegB RegB,
     Read (Deref RegB),
     Receive RegA,
     Const 4 RegB,
     Const 1 RegC,
     Compute Add RegB RegC RegC,
     Read (Deref RegC),
     Receive RegB,
     Compute Or RegA RegB RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (2)),
     Jump (Rel (-13)),
     Read (Addr 1),
     Receive RegA,
     Write RegA stdio,
     Load (Addr 0) RegA,
     Load (Addr 1) RegB,
     Read (Addr 1),
     Receive RegE,
     Nop,
     EndProg
        ]

main :: IO()
main = do
  _ <- runDebug debug 2 prog
  return ()


debug :: SystemState -> IO()
debug state = printregs $ map (getReg state) [SPID, PC, SP, RegA, RegB, RegC, RegD, RegE]

printregs :: [String] -> IO()
printregs ("":xs) = return()
printregs [spid, pc, sp, a, b, c, d, e] = do
  putStr "\n"
  putStr $ "SPID | " ++ spid ++ "\n"
  putStr $ "PC   | " ++ pc ++ "\n"
  putStr $ "SP   | " ++ sp ++ "\n"
  putStr $ "RegA | " ++ a ++ "\n"
  putStr $ "RegB | " ++ b ++ "\n"
  putStr $ "RegC | " ++ c ++ "\n"
  putStr $ "RegD | " ++ d ++ "\n"
  putStr $ "RegE | " ++ e ++ "\n"
  putStr "===================\n"
  return()
printregs _ = return ()
getReg :: SystemState -> Reg -> String
getReg SysState{..} reg = concatMap printrega sprs
  where
    printrega SprState{..}
      | instrs!pc == Nop = show regvalue
      | otherwise = ""
        where
          regvalue = regbank ! reg
          pc = regbank ! PC
