{-# LANGUAGE RecordWildCards #-}
import Sprockell.System

prog :: [Instruction]
prog = [

     Const 0 RegA,
     Store RegA (Addr 0),
     Const 0 RegA,
     Store RegA (Addr 1),
     Load (Addr 0) RegA,
     Const 3 RegB,
     Compute Lt RegA RegB RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (22)),
     Const 0 RegA,
     Store RegA (Addr 1),
     Load (Addr 1) RegA,
     Const 5 RegB,
     Compute Lt RegA RegB RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (10)),
     Const 65 RegA,
     Load (Addr 1) RegB,
     Compute Add RegA RegB RegA,
     Write RegA stdio,
     Load (Addr 1) RegA,
     Const 1 RegB,
     Compute Add RegA RegB RegA,
     Store RegA (Addr 1),
     Jump (Rel (-13)),
     Load (Addr 0) RegA,
     Const 1 RegB,
     Compute Add RegA RegB RegA,
     Store RegA (Addr 0),
     Jump (Rel (-25)),
     Nop,
     EndProg
        ]

main :: IO()
main = do
  putStr "###BEGIN###"
  _ <- run 1 prog
  putStr "###END###\n"
  return ()


