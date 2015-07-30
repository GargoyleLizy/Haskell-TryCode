{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.FloatingPointPredicate as FP

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)
-- first unLLVM LLVM a -> State AST.Module a
-- then execState (State AST.Module a) AST.Module
-- Why not flip the args in declration?

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }
-- create a empty module with only name 
-- module is the top level of llvm IR.
{- data Module
  = Module {moduleName :: String,
            moduleDataLayout :: Maybe LLVM.General.AST.DataLayout.DataLayout,
            moduleTargetTriple :: Maybe String,
            moduleDefinitions :: [Definition]}
-}

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }
-- Definition is anything that could be at the toplevel of a module
-- Module {..., moduleDefinition :: [Definitions]}
{-data Definition
  = GlobalDefinition Global
  | TypeDefinition Name (Maybe Type)
  | MetadataNodeDefinition MetadataNodeID [Maybe Operand]
  | NamedMetadataDefinition String [MetadataNodeID]
  | ModuleInlineAssembly String
-}

define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }
-- defines a global function definition and add it to the LLVM state value.

external ::  Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }
-- defines a external global function definition (which means its body- basic blocks is empty and not here) 
-- add it to the LLVM state value as previous function.

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- IEEE 754 double
double :: Type
double = FloatingPointType 64 IEEE
--data Type = ... | FloatingPointType {...} | ...
-- FloatingPointType :: Word32 -> FloatingPointFormat -> Type
-- where the Word32 should specify the "typeBits"
--data FloatingPointFormat = IEEE | DoubleExtended | PairOfFloats


-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map String Int
-- Names is an map for String(name) and Int(as count/postfix)

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)
-- check if the supported name is exist or not
-- if not, add it to current map with 1 (used once)
-- if exits a same name, give it the postfix 

instance IsString Name where
  fromString = Name . fromString

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(String, Operand)]
-- an operand is roughly that which is an argument to an Instruction
{-data Operand
  = LocalReference Type Name
  | ConstantOperand C.Constant
  | MetadataStringOperand String
  | MetadataNodeOperand MetadataNode
-}
-- ?? Dont quit understand what metadata is right now. need to review the llvm documentation

data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )
-- So what is Codegen a ? 
-- It is a datatype that contains a State? where is the function?
-- Why wrap the State? where did we gain the facility?

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))
-- Sort. by the block index.
-- But why sort? I thought they should be incremented automatically.
-- So, where did these index come from. -"addBlock"

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)
-- get the blockStates from CodegenState blocks as list.
-- So we need to sort them, because the disorder from map to list?
-- then make the blocks inside the list.


makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

-- get a name produce the blockState as basicblock.
-- But the index is lost. so would the name "l" be produced related with idx.
-- probably not. Since then they should be put in one function. 
-- Then who produce "l"?
-- So where did t come from? who produce it as Maybe value.
{-data BasicBlock
  = BasicBlock Name [Named Instruction] (Named Terminator)
-}

entryBlockName :: String
entryBlockName = "entry"
-- constant value

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing
-- empty block state. with appropriate index. see "addBlock".
-- index is based on the CodegenState blockcount.

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty
-- empty CodegenState. current block name is entry.

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen
-- provide a emptyCodegen 


fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

instr :: Instruction -> Codegen (Operand)
instr ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = i ++ [ref := ins] } )
  return $ local ref
-- fresh give a unnamed word for current unnamed instruction and increment the state value
-- create a Name using the Word(number) as "ref"
-- get the current block as "blk"
--data Named a = Name := a | Do a
-- ref(Name) := ins (a which is instruction)
-- add the provided instruction "ins" to stack (Isn't stack++ins kind of less efficiency? There should be a more efficient list appending or stack management)

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

-------------------------------------------------------------------------------

-- References
local ::  Name -> Operand
local = LocalReference double
-- data Operand = ...| LocalReference Type(double) Name(provided)
-- and double is "Type" LLVM.General.AST.Type. 
-- defined in previous part as below:
-- double :: Type
-- double = FloatingPointType 64 IEEE

global ::  Name -> C.Constant
global = C.GlobalReference double
--data C.Constant = ... | C.GlobalReference Type Name | ...

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference double
--data Operand = ... | ConstantOperand C.Constant | ...

-- Arithmetic and Constants
fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv NoFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

cons :: C.Constant -> Operand
cons = ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call False CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []
