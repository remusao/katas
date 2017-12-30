{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.Bits ((.&.), (.|.), shiftL, complement)
import qualified Data.ByteString as BS
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Word as W
import qualified System.Environment as Env
import qualified Text.Printf as P
import qualified Debug.Trace as D

-- | Interpreter internal state
data Synacor = Synacor
  { pos :: Int
  , mem :: M.Map Register W.Word16
  , stack :: [W.Word16]
  , program :: Program
  } deriving Show

type Program = V.Vector W.Word16

data VirtualMachineState s
  = Halt
  | Error String
  | PutChar Char s
  | GetChar (Char -> s)
  | Ok s
  deriving (Functor)

newtype Register = Register { getValue :: W.Word16 }
  deriving (Show, Eq, Ord, P.PrintfArg)

initVM :: Program -> Synacor
initVM program = Synacor
  { pos = 0
  , mem = M.empty
  , stack = []
  , program = program
  }


debug :: String -> b -> b
debug _ = id
-- debug = D.trace

execNextOpCode :: Synacor -> VirtualMachineState Synacor
execNextOpCode vm@Synacor{ program, mem, pos, stack }
  | pos < 0 || fromIntegral pos >= V.length program = Halt
  | otherwise =
    debug (P.printf "Interpreting opcode %d" opcode) $
      case opcode of
        -- halt: 0
        --   stop execution and terminate the program
        0 -> Halt
        -- set: 1 a b
        --   set register <a> to the value of <b>
        1 ->
          debug (P.printf "set a:%d = b:%d" r1 v2) $ Ok vm
            { pos=pos + 3
            , mem=M.insert r1 v2 mem
            }
        -- push: 2 a
        --   push <a> onto the stack
        2 ->
          debug (P.printf "push a:%d on stack" v1) $ Ok vm
            { pos=pos + 2
            , stack=v1:stack
            }
        -- pop: 3 a
        --   remove the top element from the stack and write it into <a>; empty stack = error
        3 ->
          if null stack
            then Error "Trying to pop an empty stack"
            else let top = head stack in
              debug (P.printf "pop a:%d = %d from stack" r1 top) $ Ok vm
                { pos=pos + 2
                , mem=M.insert r1 top mem
                , stack=tail stack
                }
        -- eq: 4 a b c
        --   set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
        4 ->
          debug (P.printf "eq a:%d = b:%d == c:%d from stack" r1 v2 v3) $ Ok vm
            { pos=pos + 4
            , mem=M.insert r1 (if v2 == v3 then 1 else 0) mem
            }
        -- gt: 5 a b c
        --   set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
        5 ->
          debug (P.printf "gt a:%d = b:%d > c:%d from stack" r1 v2 v3) $ Ok vm
            { pos=pos + 4
            , mem=M.insert r1 (if v2 > v3 then 1 else 0) mem
            }
        -- jmp: 6 a
        --   jump to <a>
        6 ->
          debug (P.printf "jmp to a:%d" v1) $ Ok vm { pos=fromIntegral v1 }
        -- jt: 7 a b
        --   if <a> is nonzero, jump to <b>
        7 ->
          debug (P.printf "jt to b:%d if a:%d /= 0" v2 v1) $ Ok
            vm { pos=if v1 /= 0 then fromIntegral v2 else pos + 3 }
        -- jf: 8 a b
        --   if <a> is zero, jump to <b>
        8 ->
          debug (P.printf "jf to b:%d if a:%d == 0" v2 v1) $ Ok
            vm { pos=if v1 == 0 then fromIntegral v2 else pos + 3 }
        -- add: 9 a b c
        --   assign into <a> the sum of <b> and <c> (modulo 32768)
        9 ->
          debug (P.printf "add a:%d = b:%d + c:%d" r1 v2 v3) $ Ok vm
            { pos=pos + 4
            , mem=M.insert r1 ((v2 + v3) `mod` 32768) mem
            }
        -- mult: 10 a b c
        --   store into <a> the product of <b> and <c> (modulo 32768)
        10 ->
          debug (P.printf "mult a:%d = b:%d * c:%d" r1 v2 v3) $ Ok vm
            { pos=pos + 4
            , mem=M.insert r1 ((v2 * v3) `mod` 32768) mem
            }
        -- mod: 11 a b c
        --   store into <a> the remainder of <b> divided by <c>
        11 ->
          debug (P.printf "mod a:%d = b:%d `mod` c:%d" r1 v2 v3) $ Ok vm
            { pos=pos + 4
            , mem=M.insert r1 (v2 `mod` v3) mem
            }
        -- and: 12 a b c
        --   stores into <a> the bitwise and of <b> and <c>
        12 ->
          debug (P.printf "and a:%d = b:%d & c:%d" r1 v2 v3) $ Ok vm
            { pos=pos + 4
            , mem=M.insert r1 (v2 .&. v3) mem
            }
        -- or: 13 a b c
        --   stores into <a> the bitwise or of <b> and <c>
        13 ->
          debug (P.printf "or a:%d = b:%d | c:%d" r1 v2 v3) $ Ok vm
            { pos=pos + 4
            , mem=M.insert r1 (v2 .|. v3) mem
            }
        -- not: 14 a b
        --   stores 15-bit bitwise inverse of <b> in <a>
        14 ->
          debug (P.printf "not a:%d = ~b:%d" r1 v2) $ Ok vm
            { pos=pos + 3
            , mem=M.insert r1 (complement v2 .&. 32767) mem
            }
        -- rmem: 15 a b
        --   read memory at address <b> and write it to <a>
        15 ->
          debug (P.printf "rmem a:%d = b:%d" r1 v2) $ Ok vm
            { pos=pos + 3
            , mem=M.insert r1 (program V.! fromIntegral v2) mem
            }
        -- wmem: 16 a b
        --   write the value from <b> into memory at address <a>
        16 ->
          debug (P.printf "wmem program[a:%d] = b:%d" v1 v2) $ Ok vm
            { pos=pos + 3
            , program=program V.// [(fromIntegral v1, v2)]
            }
        -- call: 17 a
        --   write the address of the next instruction to the stack and jump to <a>
        17 ->
          debug (P.printf "call jump to a:%d" v1) $ Ok vm
            { pos=fromIntegral v1
            , stack=(fromIntegral pos + 2):stack
            }
        -- ret: 18
        --   remove the top element from the stack and jump to it; empty stack = halt
        18 ->
          case stack of
            [] -> Error "Trying to pop element from empty stack"
            (x:xs) -> debug (P.printf "ret to %d" x) $ Ok vm
              { pos=fromIntegral x
              , stack=xs
              }
        -- out: 19 a
        --   write the character represented by ascii code <a> to the terminal
        19 ->
          debug (P.printf "out a:%d" v1) $ PutChar
            (C.chr $ fromIntegral v1)
            vm { pos=pos + 2 }
        -- in: 20 a
        --   read a character from the terminal and write its ascii code to <a>; it can be assumed that once input starts, it will continue until a newline is encountered; this means that you can safely read whole lines from the keyboard and trust that they will be fully read
        20 ->
          debug (P.printf "in a:%d" r1) $ GetChar $ \chr -> vm
            { pos=pos + 2
            , mem=M.insert r1 (fromIntegral $ C.ord chr) mem
            }
        -- noop: 21
        --   no operation
        21 -> Ok vm { pos=pos + 1 }
        o -> Error $ "Unknown opcode: " ++ show o
  where
    opcode = program V.! pos
    r1 = Register $ program V.! (pos + 1)
    r2 = Register $ program V.! (pos + 2)
    r3 = Register $ program V.! (pos + 3)
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

newtype Place = Place String
  deriving (Show, Eq, Ord)

newtype Item = Item String
  deriving (Show, Eq, Ord)

data Parser = Parser
  { isPlace :: Bool
  , isItem :: Bool
  , isInv :: Bool
  , places :: [Place]
  , items :: [Item]
  , inv :: [Item]
  }

{-
   Given a partial output from the running program, extract the place the we
   are at the moment, a list of items to pick and a list of possible places
   to go next.
-}
extractActions :: String -> ([Item], [Item], [Place])
extractActions str = go (lines str) Parser{
  isPlace = False,
  isItem = False,
  isInv = False,
  places = [],
  items = [],
  inv = [] }
  where
    go [] Parser{ items, inv, places } = (items, inv, places)
    go (x:xs) p
      | "Things of interest" `L.isPrefixOf` x = go xs p{ isItem = True, isPlace = False, isInv = False }
      | "There are " `L.isPrefixOf` x = go xs p{ isPlace = True, isItem = False, isInv = False }
      | "There is " `L.isPrefixOf` x = go xs p{ isPlace = True, isItem = False, isInv = False }
      | "You inventory:" `L.isPrefixOf` x = go xs p{ isPlace = False, isItem = False, isInv = True }
      | "- " `L.isPrefixOf` x =
        case (isPlace p, isItem p, isInv p) of
          (True, False, False) -> go xs p{ places=(Place $ L.drop 2 x):places p }
          (False, True, False) -> go xs p{ items=(Item $ L.drop 2 x):items p }
          (False, False, True) -> go xs p{ inv=(Item $ L.drop 2 x):inv p }
          _ -> go xs p
      | otherwise = go xs p

{-
   Keeps track of the internal state of the simulation.
-}
data Simulation = Simulation
  { acc :: String         -- Current accumulated output from the program
  , past :: [String]      -- List of already-visited places, to avoid loops
  , input :: String       -- Input used when `GetChar` command is emitted
  } deriving Show

{-
  The simulation proceeds by exploring the game in a depth-first search way. It
  will start at the first location of the game, and will try to explore all
  possible exists, as well as use all possible objects found on the way.
-}
simulate :: Program -> String
simulate = reverse . acc . go start . initVM
  where
    -- Initial state of the simulation
    start = Simulation {
      acc = "",
      input = "",
      past = []
    }

    -- Sequentially feed different inputs to the same simulation state,
    -- accumulating both visited places (to not visit several times the same
    -- place) and output of the simulation.
    explore :: Synacor -> Simulation -> [String] -> Simulation
    explore vm = goExplore
      where
        goExplore :: Simulation -> [String] -> Simulation
        goExplore s [] = s
        goExplore s (x:xs) =
          let newState = go s {
            acc = "",
            input = x
          } vm
          in goExplore s{
            acc = acc s ++ acc newState,
            past = L.nub $ past s ++ past newState } xs

    -- Recursively explore the game, starting at `start` state.
    go :: Simulation -> Synacor -> Simulation
    go s@Simulation{ acc, input, past } vm =
      case execNextOpCode vm of
        Halt -> s
        Error err -> s{ acc = acc ++ ">>Program errored: " ++ show err ++ "<<" }
        Ok vm2 -> go s vm2
        PutChar chr vm2 -> go s{ acc = chr : acc } vm2

        -- When a char is requested by the program, there are two situations to
        -- handle:
        -- 1. Either there is at least one char available in `input`, in which
        -- case we feed it to the simulation and continue the execution.
        -- 2. Or there is none, in which case we process the accumulated output
        -- of the program `acc` and extract possible exits and objects to
        -- pick-up. We then continue the exploration using all the possible
        -- valid actions.
        GetChar f ->
          case input of
            (c:cs) -> go s { acc = c : acc, input = cs } (f c)
            [] ->
              let
                (items, inv, places ) = extractActions (reverse acc)
                takeItems = L.unlines ["take " ++ i | (Item i) <- items]
              in
                if acc `L.elem` past
                  then s -- Back to a previously explored place
                  else
                    -- Recursively explore the game, starting by picking all
                    -- available object, then following each available path.
                    -- If there are some objects in the inventory, try to
                    -- use them as well before moving to the next location.
                    explore vm s{ past = acc:past } $ [
                      takeItems ++ "inv\n" ++ "go " ++ p ++ "\n"
                      | (Place p) <- places
                    ] ++ [
                      takeItems ++ ("use " ++ i ++ "\n") ++ "inv\n" ++ "go " ++ p ++ "\n"
                      | let allItems = L.sort $ items ++ inv,
                      (Item i) <- allItems,
                      (Place p) <- places
                    ]

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

packLittleEndian :: W.Word8 -> W.Word8 -> W.Word16
packLittleEndian b0 b1 = fromIntegral b0 .|. (fromIntegral b1 `shiftL` 8)

mkProgram :: BS.ByteString -> Program
mkProgram bs = V.fromList $ go (BS.unpack bs)
  where
    go [] = []
    go (b0:b1:ws) = packLittleEndian b0 b1 : go ws
    go (b0:ws) = packLittleEndian b0 0 : go ws

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [filepath] -> do
      program <- BS.readFile filepath
      -- let program = V.fromList [9,32768,32769,4,19,32768]
      if BS.null program
         then putStrLn "Program is empty."
         else putStrLn $ simulate $ mkProgram program
    _ -> putStrLn "Expected one argument: path to program"
