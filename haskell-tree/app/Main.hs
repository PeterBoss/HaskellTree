module Main where

import Data.IORef
import System.IO

main :: IO ()
main = do
    treeRef <- newIORef BinaryEmpty
    writeIORef treeRef BinaryEmpty
    tree <- readIORef treeRef
    consoleInterface tree

consoleInterface :: BinaryTree Int -> IO ()
consoleInterface tree = do
    putStrLn "\nOptions:"
    putStrLn "(1) Search"
    putStrLn "(2) Insert"
    putStrLn "(3) List All"
    putStrLn "(4) Load File"
    line <- getLine
    let cmd = (read line :: Int)
    case cmd of
        1 -> do
            didFindValue <- handleSearch tree
            if didFindValue == True then
                putStrLn "It's in here"
            else
                putStrLn "Didn't find it"
            consoleInterface tree
        2 -> do
            treeAfterInsert <- handleInsert tree
            putStrLn (show $ inOrderBinaryTree treeAfterInsert)
            consoleInterface treeAfterInsert
        3 -> do
            putStrLn (show $ inOrderBinaryTree tree)
            consoleInterface tree
        _ -> do
            treeFromFile <- fromIntegerFile "numbers.txt"
            consoleInterface treeFromFile

data BinaryTree d = BinaryEmpty
                    | BinaryNode d (BinaryTree d) (BinaryTree d)
                    deriving (Show)

handleInsert :: BinaryTree Int -> IO (BinaryTree Int)
handleInsert tree = do
    putStrLn "\nInput Value"
    line <- getLine
    let value = (read line :: Int)
    return (insertBinaryTree value tree)

handleSearch :: BinaryTree Int -> IO Bool
handleSearch tree = do
    putStrLn "\nInput Value"
    line <- getLine
    let value = (read line :: Int)
    return (searchBinaryTree value tree)

searchBinaryTree :: (Ord d) => d -> BinaryTree d -> Bool
searchBinaryTree _ BinaryEmpty = False
searchBinaryTree x (BinaryNode value binaryTreeLeft binaryTreeRight)
    | x == value = True
    | x < value  = searchBinaryTree x binaryTreeLeft
    | x > value  = searchBinaryTree x binaryTreeRight

insertBinaryTree :: (Ord d) => d -> BinaryTree d -> BinaryTree d
insertBinaryTree newValue BinaryEmpty = BinaryNode newValue BinaryEmpty BinaryEmpty
insertBinaryTree newValue (BinaryNode value left right)
    | newValue < value = BinaryNode value (insertBinaryTree newValue left) right
    | otherwise        = BinaryNode value left (insertBinaryTree newValue right)

inOrderBinaryTree :: (Ord d) => BinaryTree d -> [d]
inOrderBinaryTree BinaryEmpty = []
inOrderBinaryTree (BinaryNode v binaryTreeLeft binaryTreeRight) =
    inOrderBinaryTree binaryTreeLeft ++ [v] ++ inOrderBinaryTree binaryTreeRight

fromIntegerFile :: String -> IO (BinaryTree Int)
fromIntegerFile text = do
    withFile text ReadMode (\handle -> do
        contents <- hGetContents handle
        let contentLines = lines contents
        let values = map read contentLines :: [Int]
        let tree = foldr insertBinaryTree BinaryEmpty values
        putStrLn (show $ inOrderBinaryTree tree)
        return tree
        )