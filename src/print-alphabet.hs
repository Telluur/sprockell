{-# LANGUAGE RecordWildCards #-}
import Sprockell.System

prog :: [Instruction]
prog = [

     Const 0 RegA,
     Store RegA (Addr 0),
     Load (Addr 0) RegA,
     Const 26 RegB,
     Compute Lt RegA RegB RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (10)),
     Load (Addr 0) RegA,
     Const 65 RegB,
     Compute Add RegA RegB RegA,
     Write RegA stdio,
     Load (Addr 0) RegA,
     Const 1 RegB,
     Compute Add RegA RegB RegA,
     Store RegA (Addr 0),
     Jump (Rel (-13)),
     Nop,
     EndProg
        ]

main :: IO()
main = do
  putStr "###BEGIN###"
  _ <- run 1 prog
  putStr "###END###\n"
  return ()


