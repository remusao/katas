{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import Control.Applicative
import Control.Monad

buildCounter :: [B.ByteString] -> M.Map B.ByteString Int
buildCounter strings = M.fromListWith (+) $ zip strings (repeat 1)

main :: IO ()
main = do
    -- Build counter
    n :: Int <- readLn
    strings <- replicateM n B.getLine
    let counter = buildCounter strings
    -- Answer queries
    q :: Int <- readLn
    replicateM_ q $ do
        query <- B.getLine
        print (M.findWithDefault 0 query counter)
