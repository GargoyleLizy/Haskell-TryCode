{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import qualified Syntax as S

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))
-- translate args String to (Type,Name)
-- Only available type right now is "double"

{-Expr = Function Name [Name] Expr
       | Extern Name [Name]
       | ....
-}
codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) = do
  define double name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca double
        store var (local (AST.Name a))
        assign a var
      cgen body >>= ret
--In the case of function, define(codegen.hs) a global function return LLVM()
{-define retty label argtys body-}
-- which will take a AST.module state value and add that function to the
-- moduleDefinitions compnont.
-- fnargs need to be translated into (Type,Name). Only type is 'double'(codegen.hs)
-- focuse on the bls part line by line, start with do parts:
-- 1, entry block is created as empty block
-- 2, set current block is the entryBlock.
-- 3, for each arguments(from left to right, see forM), 
--   3.1, creat Alloca instruction for that argument and put that to the block stack, return the local reference
--   3.2, store the local arguements to "var"(the local reference in stack)
--   3.3, in symbol table, assign that argument(String) with corresponding var(instruction in the blockstate stack)
-- 4,  

codegenTop (S.Extern name args) = do
  external double name fnargs
  where fnargs = toSig args
-- In the case of external. 
-- just define a external function and put it in the "Future AST.Module" definitions.

codegenTop exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret
-- all other expressions belongs to main.

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test
-- ULT as predicate is unordered or less than.
-- the result is unsigned integer. but as kaleidoscope only has double
-- use uitofp convert the result. 


-- the association list between the mathmatic symbol and instruction functions.
binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
  ]

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.UnaryOp op a) = do
  cgen $ S.Call ("unary" ++ op) [a]
-- take the unaryOp as a function.

cgen (S.BinaryOp "=" (S.Var var) val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  return cval
-- assignment, get the operand "a" which is the value of var.
-- then store cval to 'a'(pointer), return cval as assignment value.

cgen (S.BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
-- lookup the operator in the map. then apply corresponding function.

cgen (S.Var x) = getvar x >>= load
-- for variable, get its from symbol table then load it

cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
-- if constant or floating values, just return the constant values in LLVM IR.

cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs
-- evaluate each arguments, then call it. and return the Constant Operand

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn
-- data Context. a Context object holds the state of the LLVM systems needs for one thread of LLVM compilation
{-withContext :: (Context -> IO a) -> IO a-}
-- Create a Context, run an action(to which it is provided), then destory the Context.
{-withModuleFromAST :: Context -> Module -> (Module -> IO a) -> ExceptT String IO a-}
-- Build an LLVM.General.Module from a LLVM.General.AST.Module, ie. lower an AST from Haskell into C++ objects.
{-moduleLLVMAssembly :: Module -> IO String-}
-- generate LLVM assembly from a Module

