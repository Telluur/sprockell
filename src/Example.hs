import Sprockell.System

prog :: [Instruction]
prog = [ Const 2 RegA
  , Push RegA
  , Load (Deref SP) RegA
  , Const 3 RegB
  , Compute Gt RegA RegB RegA
  , Branch RegA (Rel 2)
  , Jump (Rel 3)
  , Const 10 RegA
  , Store RegA (Deref SP)
  , EndProg
  ]

main :: IO()
main = do
  _ <- runDebug debug 1 prog
  putStrLn "_"
  return ()

debug :: SystemState -> String
debug SysState{sprs=sprs, instrs=instrs} = concatMap printrega sprs
  where
    printrega SprState{regbank=regs}
      | instrs!pc == Nop = "Value in RegA " ++ show rega ++ "\n"
      | otherwise = ""
        where
          rega = regs ! RegA
          pc = regs ! PC
