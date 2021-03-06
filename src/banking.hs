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
     Const 1 RegA,
     Branch SPID (Rel (3)),
     Write RegA (Addr 2),
     Jump (Rel (3)),
     Read (Addr 2),
     Receive RegA,
     Const 1 RegA,
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
     Store SPID (Addr 0),
     Const 0 RegA,
     Store RegA (Addr 1),
     Load (Addr 1) RegA,
     Const 10 RegB,
     Compute Lt RegA RegB RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (16)),
     Load (Addr 1) RegA,
     Const 1 RegB,
     Compute Add RegA RegB RegA,
     Store RegA (Addr 1),
     TestAndSet (Addr 0),
     Receive RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (-3)),
     Read (Addr 1),
     Receive RegA,
     Const 4 RegB,
     Compute Add RegA RegB RegA,
     Write RegA (Addr 1),
     Write Zero (Addr 0),
     Jump (Rel (-19)),
     Const 0 RegA,
     Store RegA (Addr 1),
     Load (Addr 1) RegA,
     Const 10 RegB,
     Compute Lt RegA RegB RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (16)),
     Load (Addr 1) RegA,
     Const 1 RegB,
     Compute Add RegA RegB RegA,
     Store RegA (Addr 1),
     TestAndSet (Addr 0),
     Receive RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (-3)),
     Read (Addr 1),
     Receive RegA,
     Const 2 RegB,
     Compute Sub RegA RegB RegA,
     Write RegA (Addr 1),
     Write Zero (Addr 0),
     Jump (Rel (-19)),
     Const 2 RegA,
     Load (Addr 0) RegB,
     Compute Add RegA RegB RegB,
     Const 0 RegA,
     Write RegA (Deref RegB),
     Load (Addr 0) RegA,
     Const 0 RegB,
     Compute Equal RegA RegB RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (24)),
     Const 2 RegA,
     Const 0 RegB,
     Compute Add RegA RegB RegB,
     Read (Deref RegB),
     Receive RegA,
     Const 2 RegB,
     Const 1 RegC,
     Compute Add RegB RegC RegC,
     Read (Deref RegC),
     Receive RegB,
     Compute Or RegA RegB RegA,
     Const 2 RegB,
     Const 2 RegC,
     Compute Add RegB RegC RegC,
     Read (Deref RegC),
     Receive RegB,
     Compute Or RegA RegB RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (2)),
     Jump (Rel (-19)),
     Read (Addr 1),
     Receive RegA,
     Write RegA stdio,
     Nop,
     EndProg
        ]

main :: IO()
main = do
  putStr "###BEGIN###"
  _ <- run 3 prog
  putStr "###END###\n"
  return ()


