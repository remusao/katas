{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

module Main
  ( main,
  )
where

import Data.Array.Unboxed qualified as A
import Data.Bits (complement, shiftL, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.Char qualified as C
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Word (Word16, Word8)
import System.Environment qualified as Env
import Text.Printf qualified as P

-- import qualified Debug.Trace as D
newtype Program
  = Program (A.UArray Int Word16)
  deriving stock (Show)

newtype Register = Register
  { getValue :: Word16
  }
  deriving stock (Show, Eq, Ord)
  deriving newtype (P.PrintfArg)

-- | Interpreter internal state
data Synacor = Synacor
  { pos :: Int,
    mem :: M.Map Register Word16,
    stack :: [Word16],
    program :: Program
  }
  deriving stock (Show)

data VirtualMachineState s
  = Halt
  | Error String
  | PutChar Char s
  | GetChar (Char -> s)
  | Ok s

packLittleEndian :: Word8 -> Word8 -> Word16
packLittleEndian b0 b1 = fromIntegral b0 .|. (fromIntegral b1 `shiftL` 8)

mkProgram :: BS.ByteString -> Program
mkProgram bs = Program array
  where
    array = A.listArray (0, length list) list
    list = go (BS.unpack bs)
    go [] = []
    go (b0 : b1 : ws) = packLittleEndian b0 b1 : go ws
    go (b0 : ws) = packLittleEndian b0 0 : go ws

initVM :: Program -> Synacor
initVM program = Synacor {pos = 0, mem = M.empty, stack = [], program = program}

debug :: String -> b -> b
debug _ = id -- D.trace

{- Executs the next opcode from the program. It takes as argument the current
   state of the virtual machine, fetches the next opcode, runs it and then
   return the new state of Synacor VM. The new state is wrapped into a
   VirtualMachineState, which acts like a continuation.
-}
execNextOpCode :: Synacor -> VirtualMachineState Synacor
execNextOpCode vm@Synacor {program = Program program, mem, pos, stack}
  | pos < 0 || fromIntegral pos >= snd (A.bounds program) = Halt
  | otherwise =
    debug (P.printf "Interpreting opcode %d" opcode) $
      case opcode of
        {-
        -- halt: 0
        --   stop execution and terminate the program
        -}
        0 -> Halt
        {-
        -- set: 1 a b
        --   set register <a> to the value of <b>
        -}
        1 ->
          debug (P.printf "set a:%d = b:%d" r1 v2) $
            Ok vm {pos = pos + 3, mem = M.insert r1 v2 mem}
        {-
        -- push: 2 a
        --   push <a> onto the stack
        -}
        2 ->
          debug (P.printf "push a:%d on stack" v1) $
            Ok vm {pos = pos + 2, stack = v1 : stack}
        {-
        -- pop: 3 a
        --   remove the top element from the stack and write it into <a>; empty stack = error
        -}
        3 ->
          if null stack
            then Error "Trying to pop an empty stack"
            else
              let top = head stack
               in debug (P.printf "pop a:%d = %d from stack" r1 top) $
                    Ok
                      vm
                        { pos = pos + 2,
                          mem = M.insert r1 top mem,
                          stack = tail stack
                        }
        {-
        -- eq: 4 a b c
        --   set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
        -}
        4 ->
          debug (P.printf "eq a:%d = b:%d == c:%d from stack" r1 v2 v3) $
            Ok
              vm
                { pos = pos + 4,
                  mem =
                    M.insert
                      r1
                      ( if v2 == v3
                          then 1
                          else 0
                      )
                      mem
                }
        {-
        -- gt: 5 a b c
        --   set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
        -}
        5 ->
          debug (P.printf "gt a:%d = b:%d > c:%d from stack" r1 v2 v3) $
            Ok
              vm
                { pos = pos + 4,
                  mem =
                    M.insert
                      r1
                      ( if v2 > v3
                          then 1
                          else 0
                      )
                      mem
                }
        {-
        -- jmp: 6 a
        --   jump to <a>
        -}
        6 -> debug (P.printf "jmp to a:%d" v1) $ Ok vm {pos = fromIntegral v1}
        {-
        -- jt: 7 a b
        --   if <a> is nonzero, jump to <b>
        -}
        7 ->
          debug (P.printf "jt to b:%d if a:%d /= 0" v2 v1) $
            Ok
              vm
                { pos =
                    if v1 /= 0
                      then fromIntegral v2
                      else pos + 3
                }
        {-
        -- jf: 8 a b
        --   if <a> is zero, jump to <b>
        -}
        8 ->
          debug (P.printf "jf to b:%d if a:%d == 0" v2 v1) $
            Ok
              vm
                { pos =
                    if v1 == 0
                      then fromIntegral v2
                      else pos + 3
                }
        {-
        -- add: 9 a b c
        --   assign into <a> the sum of <b> and <c> (modulo 32768)
        -}
        9 ->
          debug (P.printf "add a:%d = b:%d + c:%d" r1 v2 v3) $
            Ok vm {pos = pos + 4, mem = M.insert r1 ((v2 + v3) `mod` 32768) mem}
        {-
        -- mult: 10 a b c
        --   store into <a> the product of <b> and <c> (modulo 32768)
        -}
        10 ->
          debug (P.printf "mult a:%d = b:%d * c:%d" r1 v2 v3) $
            Ok vm {pos = pos + 4, mem = M.insert r1 ((v2 * v3) `mod` 32768) mem}
        {-
        -- mod: 11 a b c
        --   store into <a> the remainder of <b> divided by <c>
        -}
        11 ->
          debug (P.printf "mod a:%d = b:%d `mod` c:%d" r1 v2 v3) $
            Ok vm {pos = pos + 4, mem = M.insert r1 (v2 `mod` v3) mem}
        {-
        -- and: 12 a b c
        --   stores into <a> the bitwise and of <b> and <c>
        -}
        12 ->
          debug (P.printf "and a:%d = b:%d & c:%d" r1 v2 v3) $
            Ok vm {pos = pos + 4, mem = M.insert r1 (v2 .&. v3) mem}
        {-
        -- or: 13 a b c
        --   stores into <a> the bitwise or of <b> and <c>
        -}
        13 ->
          debug (P.printf "or a:%d = b:%d | c:%d" r1 v2 v3) $
            Ok vm {pos = pos + 4, mem = M.insert r1 (v2 .|. v3) mem}
        {-
        -- not: 14 a b
        --   stores 15-bit bitwise inverse of <b> in <a>
        -}
        14 ->
          debug (P.printf "not a:%d = ~b:%d" r1 v2) $
            Ok vm {pos = pos + 3, mem = M.insert r1 (complement v2 .&. 32767) mem}
        {-
        -- rmem: 15 a b
        --   read memory at address <b> and write it to <a>
        -}
        15 ->
          debug (P.printf "rmem a:%d = b:%d" r1 v2) $
            Ok
              vm
                { pos = pos + 3,
                  mem = M.insert r1 (program A.! fromIntegral v2) mem
                }
        {-
        -- wmem: 16 a b
        --   write the value from <b> into memory at address <a>
        -}
        16 ->
          debug (P.printf "wmem program[a:%d] = b:%d" v1 v2) $
            Ok
              vm
                { pos = pos + 3,
                  program = Program $ program A.// [(fromIntegral v1, v2)]
                }
        {-
        -- call: 17 a
        --   write the address of the next instruction to the stack and jump to <a>
        -}
        17 ->
          debug (P.printf "call jump to a:%d" v1) $
            Ok vm {pos = fromIntegral v1, stack = (fromIntegral pos + 2) : stack}
        {-
        -- ret: 18
        --   remove the top element from the stack and jump to it; empty stack = halt
        -}
        18 ->
          case stack of
            [] -> Error "Trying to pop element from empty stack"
            (x : xs) ->
              debug (P.printf "ret to %d" x) $
                Ok vm {pos = fromIntegral x, stack = xs}
        {-
        -- out: 19 a
        --   write the character represented by ascii code <a> to the terminal
        -}
        19 ->
          debug (P.printf "out a:%d" v1) $
            PutChar (C.chr $ fromIntegral v1) vm {pos = pos + 2}
        {-
        -- in: 20 a
        --   read a character from the terminal and write its ascii code to <a>;
        --   it can be assumed that once input starts, it will continue until a
        --   newline is encountered; this means that you can safely read whole
        --   lines from the keyboard and trust that they will be fully read
        -}
        20 ->
          debug (P.printf "in a:%d" r1) $
            GetChar $ \chr ->
              vm {pos = pos + 2, mem = M.insert r1 (fromIntegral $ C.ord chr) mem}
        {-
        -- noop: 21
        --   no operation
        -}
        21 -> Ok vm {pos = pos + 1}
        o -> Error $ "Unknown opcode: " ++ show o
  where
    opcode = program A.! pos
    r1 = Register $ program A.! (pos + 1)
    r2 = Register $ program A.! (pos + 2)
    r3 = Register $ program A.! (pos + 3)
    v1 =
      if getValue r1 >= 32768
        then M.findWithDefault 0 r1 mem
        else getValue r1
    v2 =
      if getValue r2 >= 32768
        then M.findWithDefault 0 r2 mem
        else getValue r2
    v3 =
      if getValue r3 >= 32768
        then M.findWithDefault 0 r3 mem
        else getValue r3

newtype Place
  = Place String
  deriving stock (Eq, Ord)

instance Show Place where
  show (Place place) = place

newtype Item
  = Item String
  deriving stock (Eq, Ord)

instance Show Item where
  show (Item item) = item

newtype Inventory
  = Inventory [Item]
  deriving stock (Show, Eq, Ord)

{- A Frame represents a unique situation, or "moment" in the simulation. It is a
   combination of a given place, a set of items available to pick-up, a set of
   directions to go to as well as a given inventory of objects which could be
   used in this situation. The goal of the simulation is to find the set of all
   unique "Frames" of the game (meaning, explore it completely.)

   A new Frame will be created each time some user input is required. The
   description will be the text accumulated since the previous Frame.

   TODO: We should include the text of the action as well? Or the next action
   which will be performed as a field of the record.
-}
data Frame = Frame
  { getDescription :: String,
    getPlaces :: [Place],
    getItems :: [Item],
    getInventory :: Inventory
  }
  deriving stock (Show, Eq, Ord)

data Action
  = Use Item
  | Take Item
  | Look Item
  | Go Place

instance Show Action where
  show (Use item) = unwords ["use", show item]
  show (Take item) = unwords ["take", show item]
  show (Look item) = unwords ["look", show item]
  show (Go place) = unwords ["go", show place]

{- Given a Frame of the simulation, list all possible actions. -}
listPossibleActions :: Frame -> ([Action], [Action])
listPossibleActions frame = (mandatoryActions, choiceActions)
  where
    mandatoryActions = map Take items
    choiceActions =
      concat
        [ map Look items,
          map Look inventory,
          map Use items,
          map Use inventory,
          map Go places
        ]
    places = getPlaces frame
    items = getItems frame
    (Inventory inventory) = getInventory frame

{- Internal state of the parser. It is only used to hold intermediary data
   structures as well as the current state of parsing.
-}
data Parser = Parser
  { isPlace :: Bool,
    isItem :: Bool,
    isInv :: Bool,
    places :: [Place],
    items :: [Item],
    inv :: [Item]
  }

{-
   Given a partial output from the running program, extract the place the we
   are at the moment, a list of items to pick and a list of possible places
   to go next.
-}
extractActions :: String -> ([Item], [Item], [Place])
extractActions str =
  go
    (lines str)
    Parser
      { isPlace = False,
        isItem = False,
        isInv = False,
        places = [],
        items = [],
        inv = []
      }
  where
    go [] Parser {items, inv, places} = (items, inv, places)
    go (x : xs) p
      | "Things of interest" `L.isPrefixOf` x =
        go xs p {isItem = True, isPlace = False, isInv = False}
      | "There are " `L.isPrefixOf` x =
        go xs p {isPlace = True, isItem = False, isInv = False}
      | "There is " `L.isPrefixOf` x =
        go xs p {isPlace = True, isItem = False, isInv = False}
      | "You inventory:" `L.isPrefixOf` x =
        go xs p {isPlace = False, isItem = False, isInv = True}
      | "- " `L.isPrefixOf` x =
        case (isPlace p, isItem p, isInv p) of
          (True, False, False) ->
            go xs p {places = Place (L.drop 2 x) : places p}
          (False, True, False) -> go xs p {items = Item (L.drop 2 x) : items p}
          (False, False, True) -> go xs p {inv = Item (L.drop 2 x) : inv p}
          _ -> go xs p
      | otherwise = go xs p

{-
   Keeps track of the internal state of the simulation.
-}
data Simulation = Simulation
  { acc :: String, -- Current accumulated output from the program
    past :: [String], -- List of already-visited places, to avoid loops
    input :: String -- Input used when `GetChar` command is emitted
  }
  deriving stock (Show)

{- Depth-first search of the Synacor game -}
-- simulate :: Program -> [Frame]
-- simulate program = go start vm
--   where
--     vm = initVM program
--
--     -- Initial state of the simulation
--     start = Simulation {
--       acc = "",
--       input = "",
--       past = []
--     }
--
--     -- Sequentially feed different inputs to the same simulation state,
--     -- accumulating both visited places (to not visit several times the same
--     -- place) and output of the simulation.
--     explore :: Synacor -> Simulation -> [String] -> Simulation
--     explore vm = goExplore
--       where
--         goExplore :: Simulation -> [String] -> Simulation
--         goExplore s [] = s
--         goExplore s (x:xs) =
--           let newState = go s {
--             acc = "",
--             input = x
--           } vm
--           in goExplore s{
--             acc = acc s ++ acc newState,
--             past = L.nub $ past s ++ past newState } xs
--
--     -- Recursively explore the game, starting at `start` state.
--     go :: Simulation -> Synacor -> Simulation
--     go s@Simulation{ acc, input, past } vm =
--       case execNextOpCode vm of
--         Halt -> s
--         Error err -> s{ acc = acc ++ ">>Program errored: " ++ show err ++ "<<" }
--         Ok vm2 -> go s vm2
--         PutChar chr vm2 -> go s{ acc = chr : acc } vm2
--
--         -- When a char is requested by the program, there are two situations to
--         -- handle:
--         -- 1. Either there is at least one char available in `input`, in which
--         -- case we feed it to the simulation and continue the execution.
--         -- 2. Or there is none, in which case we process the accumulated output
--         -- of the program `acc` and extract possible exits and objects to
--         -- pick-up. We then continue the exploration using all the possible
--         -- valid actions.
--         GetChar f ->
--           case input of
--             (c:cs) -> go s { acc = c : acc, input = cs } (f c)
--             [] ->
--               let
--                 (items, inv, places ) = extractActions (reverse acc)
--                 takeItems = L.unlines ["take " ++ i | (Item i) <- items]
--               in
--                 if acc `L.elem` past
--                   then s -- Back to a previously explored place
--                   else
--                     -- Recursively explore the game, starting by picking all
--                     -- available object, then following each available path.
--                     -- If there are some objects in the inventory, try to
--                     -- use them as well before moving to the next location.
--                     explore vm s{ past = acc:past } $ [
--                       takeItems ++ "inv\n" ++ "go " ++ p ++ "\n"
--                       | (Place p) <- places
--                     ] ++ [
--                       takeItems ++ ("use " ++ i ++ "\n") ++ "inv\n" ++ "go " ++ p ++ "\n"
--                       | let allItems = L.sort $ items ++ inv,
--                       (Item i) <- allItems,
--                       (Place p) <- places
--                     ]

-- | Execute one instruction
run :: Program -> IO ()
run = go . initVM
  where
    go :: Synacor -> IO ()
    go vm =
      case execNextOpCode vm of
        Halt -> putStrLn "Program is exiting..."
        Error err -> putStrLn $ "Program errored: " ++ show err
        PutChar chr s -> putChar chr >> go s
        GetChar f -> do
          chr <- getChar
          go (f chr)
        Ok s -> go s

runWithInput :: Program -> String -> IO ()
runWithInput = go . initVM
  where
    go :: Synacor -> String -> IO ()
    go vm input =
      case execNextOpCode vm of
        Halt -> putStrLn "Program is exiting..."
        Error err -> putStrLn $ "Program errored: " ++ show err
        PutChar chr s -> putChar chr >> go s input
        GetChar f ->
          case input of
            [] -> putStrLn "No more input, exiting..."
            (x : xs) -> putChar x >> go (f x) xs
        Ok s -> go s input

{- runWithInputPure is the same as runWithInput but runs as a pure computation.
  This was only used to check the performance of the evaluation given no
  side-effect is performed.
-}
runWithInputPure :: Program -> String -> ()
runWithInputPure = go . initVM
  where
    go :: Synacor -> String -> ()
    go vm input =
      case execNextOpCode vm of
        Halt -> ()
        Error _ -> ()
        PutChar _ s -> go s input
        GetChar f ->
          case input of
            [] -> ()
            (x : xs) -> go (f x) xs
        Ok s -> go s input

{- TODO: could run, simulate, runWithInput and runWithInputPure be implemented
by the same generic function parameterized? -}
main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [filepath] -> do
      program <- BS.readFile filepath
      -- let program = V.fromList [9,32768,32769,4,19,32768]
      if BS.null program
        then putStrLn "Program is empty."
        else -- else putStrLn $ simulate (mkProgram program)
        -- else runWithInput (mkProgram program) instructions -- simulate $ mkProgram program
          run (mkProgram program)
    _ -> putStrLn "Expected one argument: path to program"
