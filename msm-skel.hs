module MSM where

-- we want to use monads here
import Control.Monad

-- and you might also find Maps useful
import qualified Data.Map as Map

-- | The type of instructions for the MSM.
data Inst 
    = PUSH Int
    | POP
    | DUP  
    | SWAP  
    | NEWREG Int  
    | LOAD  
    | STORE  
    | NEG  
    | ADD  
    | JMP
    | CJMP Int
    | HALT
    deriving (Eq,Show)
 
-- | The type `Prog` represents programs for the MSM.
type Prog = ...

-- | The type `Stack` represents the stack of the MSM.
type Stack = ...

-- | Regs is the type for keeping track of registers
type Regs = ...


-- | This data type encapsulates the state of a running MSM.
data State = State
             { prog  :: ...
             , pc    :: ...
             , stack :: ...
             , regs  :: ...
             }
           deriving (Show)

-- | `initial` constructs the initial state of an MSM running the
-- given program.
initial :: Prog -> State
initial p = ...

-- | This is the monad that is used to implement the MSM. 
newtype MSM a = MSM (State -> ...)

instance Monad MSM where
    -- (>>=) :: MSM a -> (a -> MSM b) -> MSM b
    (MSM p) >>= k = ...

    -- return :: a -> MSM a
    return a = ...
    
    -- fail :: String -> MSM a
    fail s = ...

-- | get returns the current state of the running MSM.
get :: MSM State
get = ...

-- | set a new state for the running MSM.
set :: State -> MSM ()
set m = ...

-- | modify the state for the running MSM according to
-- the provided function argument
modify :: (State -> State) -> MSM ()
modify f = ...

-- | This function provides the instruction the PC currently points
-- to. If the PC is out of bounds, the MSM halts with an error.
getInst :: ...
getInst = ...

-- | This function runs the MSM.
interp :: MSM ()
interp = run
    where run = do inst <- getInst
                   cont <- interpInst inst
                   when cont run

-- | This function interprets the given instruction. It returns True
-- if the MSM is supposed to continue it's execution after this
-- instruction.
interpInst :: Inst -> MSM Bool
interpInst inst = ...

-- | Run the given program on the MSM
runMSM :: Prog -> ...
runMSM p = let (MSM f) = interp 
           in fmap snd $ f $ initial p



-- example program, when it terminates it leaves 42 on the top of the stack
p42 = [NEWREG 0, PUSH 1, DUP, NEG, ADD, PUSH 40, STORE, PUSH 2, PUSH 0, LOAD, ADD, HALT]

