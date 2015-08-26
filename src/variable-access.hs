{-# LANGUAGE RecordWildCards #-}
import Sprockell.System

prog :: [Instruction]
prog = [

     Const 5 RegA,
     Store RegA (Addr 0),
     Load (Addr 0) RegA,
     Store RegA (Addr 1),
     Const 6 RegA,
     Store RegA (Addr 0),
     Load (Addr 0) RegA,
     Write RegA stdio,
     Load (Addr 1) RegA,
     Write RegA stdio,
     Const 0 RegA,
     Store RegA (Addr 2),
     Load (Addr 2) RegA,
     Const 1 RegB,
     Compute Or RegA RegB RegA,
     Store RegA (Addr 3),
     Load (Addr 0) RegA,
     Const 3 RegB,
     Compute Lt RegA RegB RegA,
     Store RegA (Addr 4),
     Load (Addr 3) RegA,
     Write RegA stdio,
     Load (Addr 4) RegA,
     Write RegA stdio,
     Const 1 RegA,
     Store RegA (Addr 5),
     Const 2 RegA,
     Store RegA (Addr 6),
     Const 3 RegA,
     Store RegA (Addr 7),
     Const 2 RegA,
     Store RegA (Addr 8),
     Const 1 RegA,
     Store RegA (Addr 9),
     Const 5 RegA,
     Const 2 RegB,
     Compute Add RegA RegB RegB,
     Load (Deref RegB) RegA,
     Const 1 RegB,
     Compute Add RegA RegB RegA,
     Store RegA (Addr 10),
     Load (Addr 10) RegA,
     Write RegA stdio,
     Const 5 RegA,
     Const 1 RegB,
     Compute Add RegA RegB RegB,
     Load (Deref RegB) RegA,
     Const 5 RegB,
     Const 3 RegC,
     Compute Add RegB RegC RegC,
     Load (Deref RegC) RegB,
     Compute Equal RegA RegB RegA,
     Store RegA (Addr 11),
     Load (Addr 11) RegA,
     Write RegA stdio,
     Nop,
     EndProg
        ]

main :: IO()
main = do
  putStr "###BEGIN###"
  _ <- run 1 prog
  putStr "###END###\n"
  return ()


