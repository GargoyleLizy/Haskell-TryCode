double :: Type
double = FloatingPointType 64 IEEE

type SymbolTable = [(String,Operand)]
data CodegenState
  = CodegenState {
    currentBlock :: Name  -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState -- blocks for function
  , symtab       :: SymbolTable  -- Function scope symbol table
  , blockCount   :: Int  --Count of basic blocks
  , count        :: Word
  , names        :: Names  --Name Sypply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int  -- Block index
  , stack :: [Named Instruction] -- Stack of instructions
  , term  :: Maybe (Named Terminator) -- block terminator
  } deriving Show

newtype Codegen a = Codegen {runCodegen :: State CodegenState a}
  deriving (Functor, Applicative, Monad, MonadState CodegenState)


