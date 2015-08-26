{-# LANGUAGE RecordWildCards #-}
import Sprockell.System

prog :: [Instruction]
prog = [

     Const 5 RegA,
     Store RegA (Addr 0),
     Const 1 RegA,
     Store RegA (Addr 1),
     Load (Addr 0) RegA,
     Const 3 RegB,
     Compute Gt RegA RegB RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (3)),
     Const 88 RegA,
     Write RegA stdio,
     Load (Addr 0) RegA,
     Const 5 RegB,
     Compute GtE RegA RegB RegA,
     Load (Addr 0) RegB,
     Const 5 RegC,
     Compute LtE RegB RegC RegB,
     Compute And RegA RegB RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (3)),
     Const 88 RegA,
     Write RegA stdio,
     Load (Addr 0) RegA,
     Const 10 RegB,
     Compute Lt RegA RegB RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (3)),
     Const 88 RegA,
     Write RegA stdio,
     Load (Addr 0) RegA,
     Const 5 RegB,
     Compute Equal RegA RegB RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (3)),
     Const 88 RegA,
     Write RegA stdio,
     Load (Addr 0) RegA,
     Const 5 RegB,
     Compute Equal RegA RegB RegA,
     Load (Addr 1) RegB,
     Compute And RegA RegB RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (3)),
     Const 88 RegA,
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


