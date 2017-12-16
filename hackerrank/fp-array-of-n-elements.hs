fn n = replicate n 42

main = do
n <- readLn :: IO Int
print (length(fn(n)))
