{-# LANGUAGE RecordWildCards #-}
import Sprockell.System

prog :: [Instruction]
prog = [

     Const 6 RegA,
     Store RegA (Addr 0),
     Load (Addr 0) RegA,
     Write RegA stdio,
     Load (Addr 0) RegA,
     Const 5 RegB,
     Compute Gt RegA RegB RegA,
     Branch RegA (Rel (2)),
     Jump (Rel (13)),
     Const 20 RegA,
     Store RegA (Addr 0),
     Load (Addr 0) RegA,
     Write RegA stdio,
     Const 1 RegA,
     Store RegA (Addr 0),
     Load (Addr 0) RegA,
     Write RegA stdio,
     Const 0 RegA,
     Store RegA (Addr 0),
     Load (Addr 0) RegA,
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


