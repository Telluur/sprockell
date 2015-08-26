{-# LANGUAGE RecordWildCards #-}
import Sprockell.System

prog :: [Instruction]
prog = [

     Const 44 RegA,
     Store RegA (Addr 0),
     Load (Addr 0) RegA,
     Write RegA stdio,
     Load (Addr 0) RegA,
     Const 44 RegB,
     Compute Add RegA RegB RegA,
     Write RegA stdio,
     Const 1 RegA,
     Write RegA stdio,
     Const 0 RegA,
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


