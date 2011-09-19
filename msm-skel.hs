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
    | FORK
    | READ Int
    | WRITE Int
    deriving (Eq,Show)
 
-- | The type `Prog` represents programs for the MSM.
-- | This is represented by a list of Inst's
type Prog = [Inst]

-- | The type `Stack` represents the stack of the MSM.
-- | This is represented by a list of Int's
type Stack = [Int]

-- | Regs is the type for keeping track of registers
-- | This is represented by a list of Int's
type Regs = [Int]


-- | This data type encapsulates the state of a running MSM.
data State = State
             { prog  :: Prog
             , pc    :: Int
             , stack :: Stack
             , regs  :: Regs
             }
           deriving (Show)

-- | `initial` constructs the initial state of an MSM running the
-- given program.
-- State should have prog = p, pc = 0, stack = new Stack, regs = new Regs
initial :: Prog -> State
initial p = State { prog = p
                  , pc = 0                     
                  , stack = []                   
                  , regs = []                      
                  }

-- | This is the monad that is used to implement the MSM. 
newtype MSM a = MSM (State -> (a,State))


instance Monad MSM where
    -- (>>=) :: MSM a -> (a -> MSM b) -> MSM b
    (MSM p) >>= k = MSM (\s -> let (r, state1) = p s
                                   (MSM p1) = k r 
                               in p1 state1)

    -- return :: a -> MSM a
    return a = MSM (\x -> (a,x))
    
    -- fail :: String -> MSM a
    fail s = undefined

-- | get returns the current state of the running MSM.
get :: MSM State
get = MSM (\x -> (x,x))

-- | set a new state for the running MSM.
set :: State -> MSM ()
set m = MSM (const ((),m))

-- | modify the state for the running MSM according to
-- the provided function argument
modify :: (State -> State) -> MSM ()
modify f = MSM (\s -> ((), f s))

-- | This function provides the instruction the PC currently points
-- to. If the PC is out of bounds, the MSM halts with an error.
-- Get the current State. Get the PC from the State, then get the Prog, 
-- then get Inst PC points to from Prog, return that, if out of bounds fail s
getInst :: MSM Inst
getInst = do
  stat <- get
  if pc stat > length ( prog stat)
    then fail "out of bounds"
    else return $ prog stat !! pc stat

          
-- | This function runs the MSM.
interp :: MSM ()
interp = run
    where run = do inst <- getInst
                   cont <- interpInst inst
                   when cont run

-- | This function interprets the given instruction. It returns True
-- if the MSM is supposed to continue it's execution after this
-- instruction.
-- generel logic: get the current state do the instruction, update values in state
-- to new state and set the new state to the current lastly return MSM True or MSM False
interpInst :: Inst -> MSM Bool
interpInst inst = do
  stat <- get
  case inst of
    PUSH a     ->  let update = set stat{stack = a:stack stat, pc = pc stat +1} in
                   return True
    POP        ->  let update = set stat{stack = tail $ stack stat, pc = pc stat +1 } in
                   return True
    DUP        ->  let update = set stat{stack = head( stack stat) : stack stat, pc = pc stat +1 } in 
      return True
    SWAP       ->  let update = set stat{stack = swapStack (stack stat), pc = pc stat +1 } in 
      return True
    --               in 
     --   _ -> return False
  --   NEWREG a   ->  True
  --   LOAD       ->  True
  --   STORE      ->  True
  --   NEG        ->  True
  --   ADD        ->  True
  --   JMP        ->  True
  --   CJMP a     ->  True
  --   HALT       ->  False
  --   FORK       ->  False
  --   READ a     ->  False
  --   WRITE a    ->  False
    
swapStack :: Stack -> Stack
swapStack xs = let (a,b) = (head(xs),head(tail(xs)))
 in b : a : (drop 2 xs)
-- | Run the given program on the MSM
--runMSM :: Prog -> Prog
--runMSM p = let (MSM f) = interp             
--           in fmap snd $ f $ initial p



-- example program, when it terminates it leaves 42 on the top of the stack
p42 = [NEWREG 0, PUSH 1, DUP, NEG, ADD, PUSH 40, STORE, PUSH 2, PUSH 0, LOAD, ADD, HALT]

