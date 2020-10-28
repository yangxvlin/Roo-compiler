-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yang, Wenrui Zhang              --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------
module Codegen(ozCode, Consequence(..)) where

import OzCode
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import RooAST
import SymbolTable
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Either

type Consequence = Either String [OzInstruction]

data Btype = Int
           | Bool
           | String
           | BRecord String

ozCode :: SymTable -> Program -> Consequence
ozCode st prog = evalStateT (codeGeneration prog) st

codeGeneration :: Program -> SymTableState [OzInstruction]
codeGeneration (Program _ _ procedures)
  =
    do
      let generatedCode = [ProcedureInstruction $ ICall "proc_main",
                          ProcedureInstruction $ IHalt]

      mapM_ appendInstruction generatedCode
      mapM_ generateProcedure procedures

      st <- get
      return $ instructions st

generateProcedure :: Procedure -> SymTableState ()
generateProcedure p@(Procedure (ProcedureHeader procID params)
                               (ProcedureBody _ stmts))
  =
    do
      appendInstruction (Label $ "proc_" ++ procID)
      pushLocalVariableTable
      insertProcedureVariable p

      -- generate code of procedure's statements
      appendInstruction (Comment "prologue")
      slotNum <- getSlotCounter
      if slotNum /= 0 then 
        do
          appendInstruction (StackInstruction $ PushStackFrame slotNum)
      else 
        return ()

      -- load parameters
      let paraNum = length params
      appendInstruction (Comment "load parameters")
      mapM_ (\i ->
              do
                  appendInstruction(StackInstruction $ Store i i)
              ) [0..(paraNum -1)]

      -- init variables
      reg_init <- getRegisterCounter
      appendInstruction (Comment "initialise variables")
      appendInstruction (ConstantInstruction $ OzIntConst reg_init 0)
      mapM_ (\i ->
              do
                  appendInstruction (StackInstruction $ Store i reg_init)
              ) [paraNum..(slotNum -1)]
      setRegisterCounter reg_init

      mapM_ generateStatement stmts

      -- end of the procedure
      appendInstruction (Comment "epilogue")
      if slotNum /= 0 then 
        do
          appendInstruction (StackInstruction $ PopStackFrame slotNum)
      else 
        return ()

      appendInstruction (ProcedureInstruction IReturn)

      popLocalVariableTable

-- ---------------------------------------------------------------------------
-- Generate Statement
-- ---------------------------------------------------------------------------

generateStatement :: Stmt -> SymTableState ()
generateStatement (Assign (LId lId) (Lval rValue))
  =
    do
      (_, _, lVarType, lTotalSlot) <- getVariableType lId
      appendInstruction (Comment $ show lId ++ " <- " ++ show rValue)
      case lVarType of
        -- assign the array by array reference
        (ArrayVar _) ->
          do
            assignEle lTotalSlot (LId lId) rValue
        -- assign the record by record reference
        (RecordVar _)->
          do
            assignEle lTotalSlot (LId lId) rValue
        -- other case
        _ ->
          do
            reg <- getRegisterCounter
            loadExp reg (Lval rValue)
            storeVal reg (LId lId)
            setRegisterCounter reg
generateStatement (Assign (LBrackets lId lexp) (Lval rValue))
  =
    do
      (_, _, lVarType, lTotalSlot) <- getVariableType lId
      appendInstruction (Comment $ show (LBrackets lId lexp)
        ++ " <- " ++ show rValue)
      case lVarType of
        (ArrayVar alias) ->
          do
            (size, _) <- getArrayType alias
            assignEle (div lTotalSlot size) (LBrackets lId lexp) rValue
        _ -> liftEither $ throwError $ "Expect Array as type"
generateStatement (Assign lValue exp)
  =
    do
      appendInstruction (Comment $ show lValue ++ " <- " ++ show exp)
      reg <- getRegisterCounter
      loadExp reg exp
      storeVal reg lValue
      setRegisterCounter reg
generateStatement (Read lValue)
  =
    do
      appendInstruction (Comment $ "Read " ++ show lValue)
      bType <- getType $ Lval lValue
      reg <- getRegisterCounter
      let cmd = case bType of Int -> "read_int"
                              Bool -> "read_bool"
                              String -> "read_string"
      appendInstruction (ProcedureInstruction $ ICallBuiltIn cmd)
      storeVal reg lValue
      setRegisterCounter reg
generateStatement (Write exp)
  =
    do
      appendInstruction (Comment $ "Write " ++ show exp)
      bType <- getType exp
      reg <- getRegisterCounter
      loadExp reg exp
      let cmd = case bType of Int -> "print_int"
                              Bool -> "print_bool"
                              String -> "print_string"
      appendInstruction (ProcedureInstruction $ ICallBuiltIn cmd)
      setRegisterCounter reg
generateStatement (Writeln exp)
    =
      do
        appendInstruction (Comment $ "Writeln " ++ show exp)
        bType <- getType exp
        reg <- getRegisterCounter
        loadExp reg exp
        let cmd = case bType of Int -> "print_int"
                                Bool -> "print_bool"
                                String -> "print_string"
        appendInstruction (ProcedureInstruction $ ICallBuiltIn cmd)
        appendInstruction (ProcedureInstruction $ ICallBuiltIn "print_newline")
        setRegisterCounter reg
generateStatement (Call procID params)
  =
    do
      appendInstruction (Comment $ "Call " ++ show procID)
      let paraNum = length params
      (formalParams, _) <- getProcedure procID
      mapM_ (\i ->
              do
                  reg <- getRegisterCounter
                  let (byValue, _) = formalParams !! i
                  case byValue of
                      True -> loadExp reg $ params !! i
                      False -> case (params !! i) of
                                      Lval lValue -> loadVarAddress reg lValue
                                      _ -> return ()
              ) [0..(paraNum - 1)]
      appendInstruction (ProcedureInstruction $ ICall $ "proc_" ++ procID)
      setRegisterCounter 0
generateStatement (IfThen exp stmts)
  =
    do
      trueLabel <- getlabelCounter
      falseLabel <- getlabelCounter
      appendInstruction (Comment $ "if " ++ show(exp))

      -- guard
      reg <- getRegisterCounter
      loadExp reg exp
      appendInstruction (BranchInstruction $ Cond False reg falseLabel)
      setRegisterCounter reg

      -- then
      appendInstruction (Label trueLabel)
      appendInstruction (Comment $ "then")
      mapM_ generateStatement stmts
      appendInstruction (Comment $ "fi")

      -- after if then
      appendInstruction (Label falseLabel)
generateStatement (IfThenElse exp stmts1 stmts2)
  =
    do
      trueLabel <- getlabelCounter
      falseLabel <- getlabelCounter
      endLabel <- getlabelCounter
      appendInstruction (Comment $ "if " ++ show(exp))

      -- guard
      reg <- getRegisterCounter
      loadExp reg exp
      appendInstruction (BranchInstruction $ Cond False reg falseLabel)
      setRegisterCounter reg

      -- then
      appendInstruction (Label trueLabel)
      appendInstruction (Comment $ "then")
      mapM_ generateStatement stmts1
      appendInstruction (BranchInstruction $ Uncond endLabel)

      -- else
      appendInstruction (Label falseLabel)
      appendInstruction (Comment $ "else")
      mapM_ generateStatement stmts2
      appendInstruction (Comment $ "fi")

      -- after if then else
      appendInstruction (Label endLabel)
generateStatement (While exp stmts)
  =
    do
      trueLabel <- getlabelCounter
      falseLabel <- getlabelCounter
      appendInstruction (Comment $ "While " ++ show(exp))
      appendInstruction (Label trueLabel)

      -- guard
      reg <- getRegisterCounter
      loadExp reg exp
      appendInstruction (BranchInstruction $ Cond False reg falseLabel)
      setRegisterCounter reg

      -- whileloop body
      appendInstruction (Comment $ "do")
      mapM_ generateStatement stmts
      appendInstruction (BranchInstruction $ Uncond trueLabel)
      appendInstruction (Comment $ "od")

      -- after loop
      appendInstruction (Label falseLabel)

-- load an expression to the given register
loadExp :: Int -> Exp -> SymTableState ()
loadExp reg (Lval lValue) = loadVal reg lValue
loadExp reg (BoolConst vl)
  = appendInstruction (ConstantInstruction $ OzIntConst reg $ boolToInt vl)
loadExp reg (IntConst vl)
  = appendInstruction (ConstantInstruction $ OzIntConst reg vl)
loadExp reg (StrConst vl)
  = appendInstruction (ConstantInstruction
        $ OzStringConst reg (strReplace vl))
loadExp reg (Op_or lExp rExp)
  =
    do
      loadExp reg lExp
      reg_1 <- getRegisterCounter
      loadExp reg_1 rExp
      appendInstruction (LogicInstruction $ LogicOr reg reg reg_1)
      setRegisterCounter reg_1
loadExp reg (Op_and lExp rExp)
  =
    do
      loadExp reg lExp
      reg_1 <- getRegisterCounter
      loadExp reg_1 rExp
      appendInstruction (LogicInstruction $ LogicAnd reg reg reg_1)
      setRegisterCounter reg_1
loadExp reg (Op_eq lExp rExp)
  =
    do
      loadExp reg lExp
      reg_1 <- getRegisterCounter
      loadExp reg_1 rExp
      appendInstruction (ComparisonInstruction
          $ CmpInstruction Eq OpInt reg reg reg_1)
      setRegisterCounter reg_1
loadExp reg (Op_neq lExp rExp)
  =
    do
      loadExp reg lExp
      reg_1 <- getRegisterCounter
      loadExp reg_1 rExp
      appendInstruction (ComparisonInstruction
          $ CmpInstruction Ne OpInt reg reg reg_1)
      setRegisterCounter reg_1
loadExp reg (Op_less lExp rExp)
  =
    do
      loadExp reg lExp
      reg_1 <- getRegisterCounter
      loadExp reg_1 rExp
      appendInstruction (ComparisonInstruction
          $ CmpInstruction Lt OpInt reg reg reg_1)
      setRegisterCounter reg_1
loadExp reg (Op_less_eq lExp rExp)
  =
    do
      loadExp reg lExp
      reg_1 <- getRegisterCounter
      loadExp reg_1 rExp
      appendInstruction (ComparisonInstruction
          $ CmpInstruction Le OpInt reg reg reg_1)
      setRegisterCounter reg_1
loadExp reg (Op_large lExp rExp)
  =
    do
      loadExp reg lExp
      reg_1 <- getRegisterCounter
      loadExp reg_1 rExp
      appendInstruction (ComparisonInstruction
          $ CmpInstruction Gt OpInt reg reg reg_1)
      setRegisterCounter reg_1
loadExp reg (Op_large_eq lExp rExp)
  =
    do
      loadExp reg lExp
      reg_1 <- getRegisterCounter
      loadExp reg_1 rExp
      appendInstruction (ComparisonInstruction
          $ CmpInstruction Ge OpInt reg reg reg_1)
      setRegisterCounter reg_1
loadExp reg (Op_add lExp rExp)
  =
    do
      loadExp reg lExp
      reg_1 <- getRegisterCounter
      loadExp reg_1 rExp
      appendInstruction (ArithmeticInstruction
          $ Add OpInt reg reg reg_1)
      setRegisterCounter reg_1
loadExp reg (Op_sub lExp rExp)
  =
    do
      loadExp reg lExp
      reg_1 <- getRegisterCounter
      loadExp reg_1 rExp
      appendInstruction (ArithmeticInstruction
          $ Sub OpInt reg reg reg_1)
      setRegisterCounter reg_1
loadExp reg (Op_mul lExp rExp)
  =
    do
      loadExp reg lExp
      reg_1 <- getRegisterCounter
      loadExp reg_1 rExp
      appendInstruction (ArithmeticInstruction
          $ Mul OpInt reg reg reg_1)
      setRegisterCounter reg_1
loadExp reg (Op_div lExp rExp)
  =
    do
      loadExp reg lExp
      reg_1 <- getRegisterCounter
      loadExp reg_1 rExp
      appendInstruction (ArithmeticInstruction
          $ Div OpInt reg reg reg_1)
      setRegisterCounter reg_1
loadExp reg (Op_not exp)
  =
    do
      loadExp reg exp
      appendInstruction (LogicInstruction
          $ LogicNot reg reg)
loadExp reg (Op_neg exp)
=
do
      loadExp reg exp
      appendInstruction (ArithmeticInstruction
          $ Neg OpInt reg reg)


loadVal :: Int -> LValue -> SymTableState ()
loadVal reg lValue
  =
    do
      loadVarAddress reg lValue
      appendInstruction (StackInstruction $ LoadIndirect reg reg)

storeVal :: Int -> LValue -> SymTableState ()
storeVal reg lValue
  =
    do
      reg_1 <- getRegisterCounter
      loadVarAddress reg_1 lValue
      appendInstruction (StackInstruction $ StoreIndirect reg_1 reg)
      setRegisterCounter reg_1

loadVarAddress :: Int -> LValue -> SymTableState ()
loadVarAddress reg (LId ident)
  =
    do
      (byValue, slotNum, _, _) <- getVariableType ident
      if byValue then 
      -- load the address of the slot use loadAddress
        appendInstruction (StackInstruction $ LoadAddress reg slotNum)
      -- load the address directly as the slot store reference(address)
      else 
        appendInstruction (StackInstruction $ Load reg slotNum)

loadVarAddress reg (LBrackets arrayID exp)
  =
    do
      loadExp reg exp
      reg_1 <- getRegisterCounter
      loadVarAddress reg_1 (LId arrayID)
      (_, _, varType, totalSlot) <- getVariableType arrayID
      case varType of
        (ArrayVar alias) ->
          do
              (size, _) <- getArrayType alias
              reg_2 <- getRegisterCounter
              appendInstruction (ConstantInstruction
                  $ OzIntConst reg_2 $ div totalSlot size)
              appendInstruction (ArithmeticInstruction
                  $ Mul OpInt reg reg reg_2)
              appendInstruction (ArithmeticInstruction
                  $ SubOff reg reg_1 reg)
        _ -> liftEither $ throwError $ "Expect Array as type"
      setRegisterCounter reg_1

loadVarAddress reg (LDot recordID fieldID)
  =
    do
      loadVarAddress reg (LId recordID)
      (_, _, varType, _) <- getVariableType recordID
      case varType of
        (RecordVar alias) ->
          do
            (_, offset) <- getRecordField alias fieldID
            reg_1 <- getRegisterCounter
            appendInstruction (ConstantInstruction
                $ OzIntConst reg_1 $ offset)
            appendInstruction (ArithmeticInstruction
                $ SubOff reg reg reg_1)
            setRegisterCounter reg_1
          _ -> liftEither $ throwError $ "Expect Record as type"

loadVarAddress reg (LBracketsDot arrayID exp fieldID)
    =
      do
        loadVarAddress reg (LBrackets arrayID exp)
        (_, _, varType, _) <- getVariableType arrayID
        case varType of
          (ArrayVar alias) ->
            do
              (size, dataType) <- getArrayType alias
              case dataType of
                (AliasDataType recordName) ->
                    do
                      (_, offset) <- getRecordField recordName fieldID
                      reg_1 <- getRegisterCounter
                      appendInstruction (ConstantInstruction
                          $ OzIntConst reg_1 $ offset)
                      appendInstruction (ArithmeticInstruction
                          $ SubOff reg reg reg_1)
                      setRegisterCounter reg_1
                _ -> liftEither $ throwError
                        $ "Expect record as type"
          _ -> liftEither $ throwError $ "Expect Array as type"

getType :: Exp -> SymTableState (Btype)
getType (BoolConst _) = return Bool
getType (IntConst _) = return Int
getType (StrConst _) = return String
getType (Op_or _ _) = return Bool
getType (Op_and _ _) = return Bool
getType (Op_eq _ _) = return Bool
getType (Op_neq _ _) = return Bool
getType (Op_less _ _) = return Bool
getType (Op_less_eq _ _) = return Bool
getType (Op_large _ _) = return Bool
getType (Op_large_eq _ _) = return Bool
getType (Op_add _ _) = return Int
getType (Op_sub _ _) = return Int
getType (Op_mul _ _) = return Int
getType (Op_div _ _) = return Int
getType (Op_not _) = return Bool
getType (Op_neg _) = return Int

getType (Lval (LId ident))
  =
    do
      (_, _, varType, _) <- getVariableType ident
      let result = case varType of BooleanVar -> Bool
                                    IntegerVar -> Int
      return result

getType (Lval (LBrackets arrayID _))
  =
    do
      (_, _, varType, _) <- getVariableType arrayID
      case varType of
        (ArrayVar alias) ->
          do
            (_, dataType) <- getArrayType alias
            case dataType of
              (BaseDataType BooleanType) -> return Bool
              (BaseDataType IntegerType) -> return Int
              (AliasDataType alias) -> return (BRecord alias)
              _ -> liftEither $ throwError $ "Expect Int/Bool"
          _ -> liftEither $ throwError $ "Expect Array as type"

getType (Lval (LDot recordID fieldID))
  =
    do
      (_, _, varType, _) <- getVariableType recordID
      case varType of
        (RecordVar alias) ->
          do
            (baseType, _) <- getRecordField alias fieldID
            case baseType of
              BooleanType -> return Bool
              IntegerType -> return Int
              _ -> liftEither $ throwError $ "Expect Int/Bool"
        _ -> liftEither $ throwError $ "Expect Record as type"

getType (Lval (LBracketsDot arrayID exp fieldID))
  =
    do
      varType <- getType (Lval (LBrackets arrayID exp))
      case varType of
        (BRecord alias) ->
          do
            (baseType, _) <- getRecordField alias fieldID
            case baseType of
              BooleanType -> return Bool
              IntegerType -> return Int
              _ -> liftEither $ throwError $ "Expect Int/Bool"
        _ -> liftEither $ throwError $ "Expect Record as type"


boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0


-- assign the element in the array or record one by one
assignEle :: Int -> LValue -> LValue -> SymTableState ()
assignEle rTotalSlot lValue rValue
  =
    do
      mapM_ (\n ->
        do
          reg <- getRegisterCounter
          loadVarAddress reg rValue
          reg_1 <- getRegisterCounter
          loadVarAddress reg_1 lValue
          reg_2 <- getRegisterCounter
          appendInstruction (ConstantInstruction
              $ OzIntConst reg_2 n)
          appendInstruction (ArithmeticInstruction
              $ SubOff reg reg reg_2)
          appendInstruction (StackInstruction
              $ LoadIndirect reg reg)
          appendInstruction (ArithmeticInstruction
              $ SubOff reg_1 reg_1 reg_2)
          appendInstruction (StackInstruction
              $ StoreIndirect reg_1 reg)
          setRegisterCounter reg
          )[0..(rTotalSlot -1)]
