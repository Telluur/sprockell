{-# LANGUAGE RecordWildCards #-}

import Sprockell.System

prog :: [Instruction]
prog = [ Const 5 RegA
  , Push RegA
  , Load (Deref SP) RegA
  , Const 3 RegB
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
  runDebug debug 1 prog >> putChar '\n' 
  return ()

debug :: SystemState -> String
debug SysState{..} = concatMap printrega sprs
  where
    printrega SprState{..}
      | instrs!pc == Nop = "Value in RegA " ++ show rega ++ "\n"
      | otherwise = ""
        where
          rega = regbank ! RegA
          pc = regbank ! PC
