import Control.Monad

main = readLn >>= (\n -> replicateM_ n (putStrLn "Hello World"))
