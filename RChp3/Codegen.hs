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
-- This supposed to be used with the "fresh" to give a unique %{number} for 
-- unnamed instructions. The "nm" supplied should be the block name.

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
-- It will wrap a function that works on the State value and return 'a' value
-- as result.
-- Why wrap the State? where did we gain the facility?
-- THis is useful since we will to access different parts of the CodegenState value and return different corresponding values as result.

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
-- supply the unnamed instruction a fresh count to ensure its uniqueness 

instr :: Instruction -> Codegen (Operand)
instr ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = i ++ [ref := ins] } )
  return $ local ref
-- "fresh" give a unnamed word for current unnamed instruction and increment the state value
-- create a Name using the Word(number) as "ref"
-- get the current block as "blk"
--data Named a = Name := a | Do a
-- ref(Name) := ins (a which is instruction)
-- add the provided instruction "ins" to stack (Isn't stack++ins kind of less efficiency? There should be a more efficient list appending or stack management)
-- And why return the "local ref" ? who would use that returned value?
-- the local ref would be used in Emit.hs, for each instruction, we need to store it to corresponding pointer/address. 
-- But how come that this local ref is a pointer/address?

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm
-- modify the current blk. There should be a function realise that current blk
-- need to be ended. and swith the current blk name. 
-- return "trm" ? why again

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
-- add an empty block, create its block name based on the input "bname"
-- what is "bname"? 

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname
-- set the current block name.

getBlock :: Codegen Name
getBlock = gets currentBlock
-- get the currentBlock name

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }
-- active is current block's name.
-- Map.insert, insert a new key and value to the map
-- if the key is alreay there, replace the value.
-- It is actually replace the old/Empty BlockState with the "new"

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c
-- get the current blockstate. first get name then trace the name in blocks.

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }
-- get lcls(?) refers the symboltable in the codegen statevalue
-- data CodegenState = CodegenState {..., symtab :: SymbolTable, ...}
-- Then add that String(Variable) and Operand (Operand here is the body/actual value of the Variable) to the symbol table.

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var
-- getvar, if the variable with that name is in the symbol table.
-- return it as Maybe

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
-- data Instruction = FAdd FastMathFlags operand0 operand1 metadata
-- 
fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub NoFastMathFlags a b []
-- same as above, FSub FastMathFlags operand0 operand1 metadata

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul NoFastMathFlags a b []
-- same as above, FMul FastMathFlags operand0 operand1 metadata

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv NoFastMathFlags a b []
-- same as above.

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []
-- same as above. Just the Predicate would indicate which condition comparison (16 kinds of comp) would be used upon the two operands.
-- should return a true/false boolean value.

cons :: C.Constant -> Operand
cons = ConstantOperand
{-data Operand = ... | ConstantOperand C.Constant | ...-}
-- It will get  a C.Constant somehow.

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []
-- Unsigned Integer to Floating Point
-- ty here should be the "double" defined earlier.

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))
-- So here A is LLVM.General.AST.Attribute. Its parameterAttribute is empty?
-- ParameterAttribute is used to indicate the information for the parameterAttribute 

-- Effects
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call False CC.C [] (Right fn) (toArgs args) [] []
-- Call is an "Instruction = Call ..."
{-isTailCall :: Bool
  callingConvention :: CallingConvention :: C/Fast/Cold/GHC/Numbered Word32
  -- so what is calling convention,is it about how ot manipulate the registers?
  returnAttributes :: [ParameterAttribute]=> [] means no requirements on the return value?
  function :: CallableOperand
  {-type CallableOperand =
  Either LLVM.General.AST.InlineAssembly.InlineAssembly Operand
    -- the callee can be inline assembly
  -}
  arguments :: [(Operand,[ParameterAttribute])] 
    --"toArgs" 
  functionAttributes :: [FunctionAttribute]
  metadata :: InstructionMetadata
-}

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []
-- create a pointer to a stack allocated uninitialized value of the given type
{-allocatedType :: Type; Type here should be double
  numElements :: Maybe Operand; numelements here 
  alignment :: Word32; here is 0
  metadata :: InstructionMetadata; [] empty as also
-}

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []
-- store instruction is used to write to memory
-- two arguments: a value(val) to store and a address(ptr) at which to store it
{-volatile :: Bool; marked as volatile means optimizer can not modify the number or order of the 'store' execution.
  address :: Operand
  value :: Operand
  maybeAtomicity :: Maybe Atomicity
    {-data Atomicity
      = Atomicity {crossThread :: Bool, memoryOrdering :: MemoryOrdering
    -}
  -- If an atomic operation is marked singlethread, it only synchronizes with or participates in modification and seq_cst total orderings with other operations running in the same thread (for example, in signal handlers).
  alignment :: Word32;
  metadata :: InstructionMetadata
-}

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []
{-volatile :: Bool;
  address :: Operand;
  maybeAtomicity:: Maybe Atomicity;
  alignment:: Word32;
  metadata:: InstructionMetadata; 
-}


-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []
{-Br :: Name -> InstructionMetadata -> Terminator-}
{-data Named a = Name := a | Do a-}
-- why Named has "Name := a"? what is the meaning of :=. just to differ from Name?
{-data Name = Name String | UnName Word -}

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []
{-condition :: Operand;
  trueDest:: Name; Name should be the name of corresponding block;
  falseDest :: Name; 
  metadata' :: InstructionMetadata; Why add ' after the metadata?
-}


ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []
{-returnOperand :: Maybe Operand
  metadata':: Instruction Metadata
-}
