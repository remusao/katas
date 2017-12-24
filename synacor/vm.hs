{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Bits ((.&.), (.|.), shiftR, shiftL, complement)
import qualified Data.ByteString as BS
import qualified Data.Char as C
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Word as W
import qualified System.Environment as Env
import qualified Text.Printf as P

-- | Interpreter internal state
data VM = VM
  { pos :: Int
  , mem :: M.Map Register W.Word16
  , stack :: [W.Word16]
  , program :: Program
  } deriving Show

type Program = V.Vector W.Word16

data ReturnCode
  = Halt
  | Error String
  | Continue VM

newtype Register = Register W.Word16
  deriving (Show, Eq, Ord, P.PrintfArg)

initVM :: Program -> VM
initVM program = VM
  { pos = 0
  , mem = M.empty
  , stack = []
  , program = program
  }

debug :: IO () -> IO ()
debug _ = return ()
-- debug = id

packLittleEndian :: W.Word8 -> W.Word8 -> W.Word16
packLittleEndian b0 b1 = fromIntegral b0 .|. (fromIntegral b1 `shiftL` 8)

unpackLittleEndian :: W.Word16 -> (W.Word8, W.Word8)
unpackLittleEndian w = (b0, b1)
  where
    b0 = fromIntegral $ w .&. 255
    b1 = fromIntegral $ (w `shiftR` 8) .&. 255

getNumber :: BS.ByteString -> W.Word16 -> W.Word16
getNumber program pos = packLittleEndian b0 b1
  where
    b0 = BS.index program $ fromIntegral pos
    b1 = BS.index program $ fromIntegral (pos + 1)

getRegister :: BS.ByteString -> W.Word16 -> Register
getRegister program pos = Register $ getNumber program pos

-- | Interpret a complete program
execNextOpCode :: VM -> IO ReturnCode
execNextOpCode VM{ program, mem, pos, stack }
  | pos < 0 || fromIntegral pos >= V.length program = return Halt
  | otherwise = do
    let opcode = program V.! pos
    debug $ P.printf "Interpreting opcode %d\n" opcode
    case opcode of
      -- halt: 0
      --   stop execution and terminate the program
      0 -> return Halt
      -- set: 1 a b
      --   set register <a> to the value of <b>
      1 -> do
        let op1 = Register $ program V.! (pos + 1)
        let op2 = program V.! (pos + 2)
        let reg =
              if op2 >= 32768
                then M.findWithDefault 0 (Register op2) mem
                else op2
        debug $ P.printf "set a:%d = b:%d\n" op1 reg
        return $ Continue VM{ pos=pos + 3, mem=M.insert op1 reg mem, program, stack }
      -- push: 2 a
      --   push <a> onto the stack
      2 -> do
        let op = program V.! (pos + 1)
        let reg =
              if op >= 32768
                then M.findWithDefault 0 (Register op) mem
                else op
        debug $ P.printf "push a:%d on stack\n" reg
        return $ Continue VM{ pos=pos + 2, stack=reg:stack, mem, program }
      -- pop: 3 a
      --   remove the top element from the stack and write it into <a>; empty stack = error
      3 ->
        if null stack
           then return $ Error "Trying to pop an empty stack"
           else do
             let top = head stack
             let op = Register $ program V.! (pos + 1)
             debug $ P.printf "pop a:%d = %d from stack\n" op top
             return $ Continue VM{ pos=pos + 2, stack=tail stack, mem=M.insert op top mem, program }
      -- eq: 4 a b c
      --   set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
      4 -> do
        let a = Register $ program V.! (pos + 1)
        let op1 = program V.! (pos + 2)
        let b =
              if op1 >= 32768
                then M.findWithDefault 0 (Register op1) mem
                else op1
        let op2 = program V.! (pos + 3)
        let c =
              if op2 >= 32768
                then M.findWithDefault 0 (Register op2) mem
                else op2
        debug $ P.printf "eq a:%d = b:%d == c:%d from stack\n" a b c
        return $ Continue VM{ pos=pos + 4, mem=M.insert a (if b == c then 1 else 0) mem, program, stack }
      -- gt: 5 a b c
      --   set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
      5 -> do
        let a = Register $ program V.! (pos + 1)
        let op1 = program V.! (pos + 2)
        let b =
              if op1 >= 32768
                then M.findWithDefault 0 (Register op1) mem
                else op1
        let op2 = program V.! (pos + 3)
        let c =
              if op2 >= 32768
                then M.findWithDefault 0 (Register op2) mem
                else op2
        debug $ P.printf "gt a:%d = b:%d > c:%d from stack\n" a b c
        return $ Continue VM{ pos=pos + 4, mem=M.insert a (if b > c then 1 else 0) mem, program, stack }
      -- jmp: 6 a
      --   jump to <a>
      6 -> do
        let op = program V.! (pos + 1)
        let a =
              if op >= 32768
                then M.findWithDefault 0 (Register op) mem
                else op
        debug $ P.printf "jmp to a:%d\n" a
        return $ Continue VM{ pos=fromIntegral a, mem, program, stack }
      -- jt: 7 a b
      --   if <a> is nonzero, jump to <b>
      7 -> do
        let op1 = program V.! (pos + 1)
        let a =
              if op1 >= 32768
                then M.findWithDefault 0 (Register op1) mem
                else op1
        let op2 = program V.! (pos + 2)
        let b =
              if op2 >= 32768
                then M.findWithDefault 0 (Register op2) mem
                else op2
        debug $ P.printf "jt to b:%d if a:%d /= 0\n" b a
        return $ Continue VM{ pos=if a /= 0 then fromIntegral b else pos + 3, mem, program, stack }
      -- jf: 8 a b
      --   if <a> is zero, jump to <b>
      8 -> do
        let op1 = program V.! (pos + 1)
        let a =
              if op1 >= 32768
                then M.findWithDefault 0 (Register op1) mem
                else op1
        let op2 = program V.! (pos + 2)
        let b =
              if op2 >= 32768
                then M.findWithDefault 0 (Register op2) mem
                else op2
        debug $ P.printf "jf to b:%d if a:%d == 0\n" b a
        return $ Continue VM{ pos=if a == 0 then fromIntegral b else pos + 3, mem, program, stack }
      -- add: 9 a b c
      --   assign into <a> the sum of <b> and <c> (modulo 32768)
      9 -> do
        let a = Register $ program V.! (pos + 1)
        let op1 = program V.! (pos + 2)
        let b =
              if op1 >= 32768
                then M.findWithDefault 0 (Register op1) mem
                else op1
        let op2 = program V.! (pos + 3)
        let c =
              if op2 >= 32768
                then M.findWithDefault 0 (Register op2) mem
                else op2
        debug $ P.printf "add a:%d = b:%d + c:%d\n" a b c
        return $ Continue VM{ pos=pos + 4, mem=M.insert a ((b + c) `mod` 32768) mem, program, stack }
      -- mult: 10 a b c
      --   store into <a> the product of <b> and <c> (modulo 32768)
      10 -> do
        let a = Register $ program V.! (pos + 1)
        let op1 = program V.! (pos + 2)
        let b =
              if op1 >= 32768
                then M.findWithDefault 0 (Register op1) mem
                else op1
        let op2 = program V.! (pos + 3)
        let c =
              if op2 >= 32768
                then M.findWithDefault 0 (Register op2) mem
                else op2
        debug $ P.printf "mult a:%d = b:%d * c:%d\n" a b c
        return $ Continue VM{ pos=pos + 4, mem=M.insert a ((b * c) `mod` 32768) mem, program, stack }
      -- mod: 11 a b c
      --   store into <a> the remainder of <b> divided by <c>
      11 -> do
        let a = Register $ program V.! (pos + 1)
        let op1 = program V.! (pos + 2)
        let b =
              if op1 >= 32768
                then M.findWithDefault 0 (Register op1) mem
                else op1
        let op2 = program V.! (pos + 3)
        let c =
              if op2 >= 32768
                then M.findWithDefault 0 (Register op2) mem
                else op2
        debug $ P.printf "mod a:%d = b:%d `mod` c:%d\n" a b c
        return $ Continue VM{ pos=pos + 4, mem=M.insert a (b `rem` c) mem, program, stack }
      -- and: 12 a b c
      --   stores into <a> the bitwise and of <b> and <c>
      12 -> do
        let a = Register $ program V.! (pos + 1)
        let op1 = program V.! (pos + 2)
        let b =
              if op1 >= 32768
                then M.findWithDefault 0 (Register op1) mem
                else op1
        let op2 = program V.! (pos + 3)
        let c =
              if op2 >= 32768
                then M.findWithDefault 0 (Register op2) mem
                else op2
        debug $ P.printf "and a:%d = b:%d & c:%d\n" a b c
        return $ Continue VM{ pos=pos + 4, mem=M.insert a (b .&. c) mem, program, stack }
      -- or: 13 a b c
      --   stores into <a> the bitwise or of <b> and <c>
      13 -> do
        let a = Register $ program V.! (pos + 1)
        let op1 = program V.! (pos + 2)
        let b =
              if op1 >= 32768
                then M.findWithDefault 0 (Register op1) mem
                else op1
        let op2 = program V.! (pos + 3)
        let c =
              if op2 >= 32768
                then M.findWithDefault 0 (Register op2) mem
                else op2
        debug $ P.printf "or a:%d = b:%d | c:%d\n" a b c
        return $ Continue VM{ pos=pos + 4, mem=M.insert a (b .|. c) mem, program, stack }
      -- not: 14 a b
      --   stores 15-bit bitwise inverse of <b> in <a>
      14 -> do
        let a = Register $ program V.! (pos + 1)
        let op1 = program V.! (pos + 2)
        let b =
              if op1 >= 32768
                then M.findWithDefault 0 (Register op1) mem
                else op1
        debug $ P.printf "or a:%d = ~b:%d\n" a b
        return $ Continue VM{ pos=pos + 3, mem=M.insert a (complement b .&. 32767) mem, program, stack }
      -- rmem: 15 a b
      --   read memory at address <b> and write it to <a>
      15 -> return $ Error "Unsupported instruction rmem"
      -- wmem: 16 a b
      --   write the value from <b> into memory at address <a>
      16 -> return $ Error "Unsupported instruction wmem"
      -- call: 17 a
      --   write the address of the next instruction to the stack and jump to <a>
      17 -> do
        let op1 = program V.! (pos + 1)
        let a =
              if op1 >= 32768
                then M.findWithDefault 0 (Register op1) mem
                else op1
        debug $ P.printf "call jump to a:%d\n" a
        return $ Continue VM { pos=fromIntegral a, stack=(fromIntegral pos + 2):stack, mem, program }
      -- ret: 18
      --   remove the top element from the stack and jump to it; empty stack = halt
      18 ->
        case stack of
          [] -> return $ Error "Trying to pop element from empty stack"
          (x:xs) -> return $ Continue VM { pos=fromIntegral x, stack=xs, mem, program }
      -- out: 19 a
      --   write the character represented by ascii code <a> to the terminal
      19 -> do
        let op1 = program V.! (pos + 1)
        let a = C.chr $ fromIntegral $
              if op1 >= 32768
                then M.findWithDefault 0 (Register op1) mem
                else op1
        debug $ P.printf "out a:%d\n" a
        putChar a
        debug $ putStrLn ""
        return $ Continue VM{ pos=pos + 2, mem, program, stack }
      -- in: 20 a
      --   read a character from the terminal and write its ascii code to <a>; it can be assumed that once input starts, it will continue until a newline is encountered; this means that you can safely read whole lines from the keyboard and trust that they will be fully read
      20 -> do
        chr <- getChar
        let code = fromIntegral $ C.ord chr
        let a = Register $ program V.! (pos + 1)
        return $ Continue VM{ pos=pos + 2, mem=M.insert a code mem, program, stack }
      -- noop: 21
      --   no operation
      21 -> return $ Continue VM { pos=pos + 1, mem, program, stack }
      o -> return $ Error $ "Unknown opcode: " ++ show o


-- | Execute one instruction
run :: Program -> IO ()
run = go . initVM
  where
    go vm = do
      result <- execNextOpCode vm
      case result of
        Halt -> putStrLn "Program is exiting..."
        Error err -> putStrLn $ "Program errored: " ++ show err
        Continue vm2 -> go vm2

mkProgram :: BS.ByteString -> Program
mkProgram bs = V.fromList $ go (BS.unpack bs)
  where
    go [] = []
    go (b0:b1:ws) = packLittleEndian b0 b1 : go ws

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [filepath] -> do
      program <- BS.readFile filepath
      -- let program = V.fromList [9,32768,32769,4,19,32768]
      if BS.null program
         then putStrLn "Program is empty."
         else run $ mkProgram program
    _ -> putStrLn "Expected one argument: path to program"
