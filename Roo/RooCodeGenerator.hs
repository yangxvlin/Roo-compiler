-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yang                            --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------
module RooCodeGenerator(runCodeGenerator) where

import Control.Monad.State
import Data.Monoid
import RooAST
import OzInstruction
import Text.ParserCombinators.Parsec.Pos


data Gstate = Gstate
  { regCounter :: Int, labelCounter :: Int, instructions :: Endo [Instruction]}

type Generator a = State Gstate a

runCodeGenerator :: DRooProgram -> [Instruction]
runCodeGenerator dRooProgram
  = let state = Gstate { regCounter = 0, labelCounter = 0, instructions = Endo ([]<>) }
        s = execState (genProgram dRooProgram) state
    in (appEndo (instructions s)) []

