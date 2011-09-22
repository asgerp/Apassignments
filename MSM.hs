{-
Author: Asger Pedersen and Kristoffer Cobley
-}
module MSM where

-- we want to use monads here
import Control.Monad
import Data.Maybe
import Data.List as List

-- used for debugging
import Debug.Trace as Trace

-- and you might also find Maps useful
import Data.Map as Map

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
    | MULT
    | SUB
    | JMP
    | CJMP Int
    | HALT
 --   | FORK
 --   | READ Int
 --   | WRITE Int
    deriving (Eq,Show)
 
-- | The type `Prog` represents programs for the MSM.
-- | This is represented by a list of Inst's
type Prog = [Inst]

-- | The type `Stack` represents the stack of the MSM.
-- | This is represented by a list of Int's
type Stack = [Int] 


-- | Regs is the type for keeping track of registers
-- | This is represented by a map of Int Int
type Regs = Map.Map Int Int


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
--initial p | Trace.trace ("initial p called with " ++ show p  ) False = undefined
initial p = State { prog = p
                  , pc = 0                     
                  , stack = []                   
                  , regs = Map.empty                      
                  }

-- | This is the monad that is used to implement the MSM. 
newtype MSM a = MSM (State -> Either String (a,State))


instance Monad MSM where
    -- (>>=) :: MSM a -> (a -> MSM b) -> MSM b
    (MSM p) >>= k = MSM (\s -> case p s of
                            Right v -> let Right (r, state1) = p s;
                                               (MSM p1) = k r
                                         in p1 state1
                            Left v -> Left v
                            )
 
    -- return :: a -> MSM a
    return a = MSM (\x -> Right (a,x))
    
    -- fail :: String -> MSM a
    fail s = MSM (\x -> Left s)

-- | get returns the current state of the running MSM.
get :: MSM State
get = MSM (\x -> Right (x,x))

-- | set a new state for the running MSM.
set :: State -> MSM ()
--set m| Trace.trace("called set state " ++ show m) False = undefined
set m = MSM (\x -> Right ((),m))

-- | modify the state for the running MSM according to
-- the provided function argument
modify :: (State -> State) -> MSM ()
modify f = MSM (\s -> Right ((), f s))

-- | This function provides the instruction the PC currently points
-- to. If the PC is out of bounds, the MSM halts with an error.
-- Get the current State. Get the PC from the State, then get the Prog, 
-- then get Inst PC points to from Prog, return that, if out of bounds fail s
getInst :: MSM Inst
getInst = do
  stat <- get
  if pc stat > length ( prog stat) || pc stat < 0
    then fail "PC out of bounds"
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
-- generel logic: get the current state, do the instruction, update values in state
-- to new state and set the new state to the current lastly return MSM True, MSM False or fails with message
interpInst :: Inst -> MSM Bool
--interpInst inst | Trace.trace("called interpinst with inst "++ show inst) False = undefined
interpInst inst = do
  stat <- get
  case inst of
    PUSH a     ->  
      do 
        set stat{stack = a:stack stat, pc = pc stat +1} 
        return True
    POP        ->  
      do 
        if List.null (stack stat) then emptyStack 
          else set stat{stack = tail $ stack stat, pc = pc stat +1 } 
        return True
    DUP        ->  
      do
        if List.null (stack stat) then emptyStack
          else set stat{stack = head( stack stat) : stack stat, pc = pc stat +1 } 
        return True
    SWAP       ->  
      do
        if length( stack stat)  >= 2 then set stat{stack = swapStack (stack stat), pc = pc stat +1 } 
          else stackLTE2Elem "SWAP"
        return True
    NEG        ->  
      do
        if List.null (stack stat) then emptyStack
        else set stat{stack = head(stack stat)*(-1) : tail(stack stat), pc = pc stat +1 } 
        return True
    ADD        ->
      do
        if length (stack stat) < 2 then stackLTE2Elem "ADD"
           else set stat{stack = head(stack stat) + head(tail(stack stat)) : drop 2 (stack stat), pc = pc stat +1 } 
        return True
    MULT       ->  
      do 
         if length (stack stat) < 2 then stackLTE2Elem "MULT"
           else set stat{stack = head(stack stat) * head(tail(stack stat)) : drop 2 (stack stat), pc = pc stat +1 } 
         return True
    SUB       ->
      do 
         if length (stack stat) < 2 then stackLTE2Elem "SUB"
            else set stat{stack = head(stack stat) - head(tail(stack stat)) : drop 2 (stack stat), pc = pc stat +1 } 
         return True
    NEWREG a   -> 
      do 
         if Map.member a (regs stat) then alreadyAllocated a 
            else set stat{regs = Map.insert a 0 (regs stat), pc = pc stat + 1 } 
         return True
    JMP        ->  
      do 
         if List.null (stack stat) then emptyStack 
            else set stat{pc = head(stack stat), stack = tail(stack stat) } 
         return True
    LOAD       ->  let update | List.null (stack stat) = emptyStack
                              | not (Map.member (head (stack stat)) (regs stat)) = notAllocated (head (stack stat))
                              | otherwise = set stat{stack = regs stat ! head (stack stat) : tail (stack stat), pc = pc stat + 1}
                   in update >> return True
    STORE      ->  let update | length (stack stat) < 2 = stackLTE2Elem "STORE"
                              | not (Map.member (stack stat !! 1) (regs stat)) = notAllocated (stack stat !! 1) 
                              | otherwise = set stat{regs = Map.insert (stack stat !! 1) (head (stack stat)) (regs stat), 
                                                                        stack = drop 2 (stack stat), pc = pc stat + 1}
                   in update >> return True
    HALT       ->  return False 
    CJMP a     ->  let update | stack stat == [] = emptyStack
                              | head (stack stat) < 0 = set stat{stack = tail (stack stat), pc = a}
                              | otherwise = set stat{stack = tail (stack stat), pc = pc stat + 1}
                   in update >> return True
    _       -> fail "something went the wrong"
  --   FORK       ->  False
  --   READ a     ->  False
  --   WRITE a    ->  False


-- helper functions

-- swap two first elements on the stack
-- only called after check for empty list
swapStack :: Stack -> Stack
swapStack xs = let (a,b) = (head xs,head(tail xs))
 in b : a : drop 2 xs


-- error reporting
emptyStack :: MSM ()
emptyStack = fail "Stack is empty"

stackLTE2Elem :: String -> MSM ()
stackLTE2Elem s = fail ("Not enough variables on stack for " ++ s ++ " operation")

notAllocated :: Int -> MSM ()
notAllocated x = fail ("register " ++ show x ++ " not allocated")

alreadyAllocated :: Int -> MSM ()
alreadyAllocated x = fail ("register " ++ show x ++ " already allocated")  


-- | Run the given program on the MSM
runMSM :: Prog -> Either String State
runMSM p = let (MSM f) = interp
           in fmap snd $ f $ initial p



