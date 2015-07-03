{-# LANGUAGE RecordWildCards #-}

import Sprockell.System

prog :: [Instruction]
prog = [ Const 5 RegA
  , Push RegA
  , Load (Deref SP) RegA
  , Const 3 RegB
  , Nop
  , Compute Gt RegA RegB RegA
  , Branch RegA (Rel 2)
  , Jump (Rel 3)
  , Const 10 RegA
  , Store RegA (Deref SP)
  , Load (Deref SP) RegA
  , Nop
  , EndProg
  ]

main :: IO()
main = do
  _ <- runDebug debug 1 prog
  return ()


debug :: SystemState -> IO()
debug state = printregs $ map (getReg state) [SPID, PC, SP, RegA, RegB, RegC, RegD, RegE]

printregs :: [String] -> IO()
printregs ("":xs) = return()
printregs [spid, pc, sp, a, b, c, d, e] = do
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
printregs _ = do
  putStr "kek \n"
  return()

getReg :: SystemState -> Reg -> String
getReg SysState{..} reg = concatMap printrega sprs
  where
    printrega SprState{..}
      | instrs!pc == Nop = show regvalue
      | otherwise = ""
        where
          regvalue = regbank ! reg
          pc = regbank ! PC
