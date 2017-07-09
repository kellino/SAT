{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Vector as V hiding (forM_, (++), concat)
import Data.Bits

import Prelude hiding (elem, notElem, null, reverse, head, filter, tail, foldr, map)
import qualified Prelude as P
import Control.Monad (forM_, join)
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as C
import Data.List (intercalate, nub)

import System.Environment (getArgs)
import Formatting (fprint, (%))
import Formatting.Clock
import System.Clock
import System.Console.CmdArgs.GetOpt
import System.Exit 

---------------------------
-- DataTypes and Aliases --
---------------------------

type Literal = Int
type Clause = Vector Literal
type Formula = Vector Clause
type Solution = Vector Literal

data SAT = SAT {formula :: Formula, solution :: Solution} deriving Show

dpll :: SAT -> Maybe Solution
dpll s
    | null f = return r
    | otherwise = do
        l <- chooseLiteral f
        case dpll (SAT (simplify f l) (l `cons` r)) of
             Just res -> return $ reverse res
             Nothing  -> dpll $ SAT {formula = simplify f (flipBit l) ,  solution = (flipBit l) `cons` r }
    where 
        s' = propagate findPure . propagate getUnit $ s
        r  = solution s'
        f  = formula s'

propagate :: (Formula -> Maybe Literal) -> SAT -> SAT
propagate p (SAT f r) =
    case p f of
         Nothing -> SAT f r
         Just u -> propagate p $ SAT (simplify f u) (u `cons` r)

getUnit :: Formula -> Maybe Literal
getUnit xs = safeHead .vecToMaybe $ filter (\x -> V.length x == 1) xs

chooseLiteral :: Formula -> Maybe Literal 
chooseLiteral = vecToMaybe . join

isPure :: Formula -> Literal -> Bool
isPure f l = flipBit l `notElem` join f

findPure :: Formula -> Maybe Literal
findPure f = locate (vecNub . join $ f)
    where locate ls = vecToMaybe (filter (\x -> isPure f x) ls)

simplify :: Formula -> Literal -> Formula
simplify f l = 
    let xs = filter (\x -> l `notElem` x) f
     in map (\x -> filter (/= flipBit l) x) xs

--simpl (filter (\x -> l `notElem` x) f) l
--where simpl c l' =  filter (/= flipBit l') c

safeHead :: Maybe (Vector a) -> Maybe a
safeHead Nothing = Nothing
safeHead (Just xs) | null xs = Nothing
                | otherwise = Just $ head xs
    
vecToMaybe :: Vector a -> Maybe a 
vecToMaybe xs | null xs = Nothing
              | otherwise = Just (head xs)

vecNub :: (Eq a) => Vector a -> Vector a
vecNub = foldr (\x acc -> if x `elem` acc then acc else x `cons` acc) empty

solve :: Formula -> Maybe Solution
--solve = dpll $ flip SAT empty
solve f = dpll $ SAT { formula = f, solution = empty }

-- Just ([1,2,3] :: Solution) --  dpll . flip SAT (fromList [])

----------------
-- CNF Parser --
----------------

comment :: Parser ()
comment = do
    _ <- char 'c' <|> char 'p'
    _ <- manyTill anyChar endOfLine
    return ()

parsePos :: Parser Literal
parsePos = do
    x <- decimal
    return $ 2 * x

parseNeg :: Parser Literal
parseNeg = do
    _ <- char '-'
    x <- decimal
    return $ 2 * x + 1

literal :: Parser Literal
literal = (parseNeg <|> parsePos) <* skipSpace

clause :: Parser Clause
clause = do
    xs <- skipSpace *> literal`manyTill` string "0\n"
    return $ fromList xs

cnf :: Parser Formula
cnf  = do
    skipMany (comment <|> endOfLine)
    xs <- clause `manyTill` string "%\n0\n"
    return $ fromList xs

------------------
-- Bit Diddling --
------------------

flipBit :: Int -> Int
flipBit x = x `xor` 1

------------------------------------
-- Command Line Parser
------------------------------------

data Flag = Help | Verbose | Silent deriving Eq

flags :: [OptDescr Flag]
flags = [ Option ['v'] [] (NoArg Verbose) 
        "Prints witnesses to stdout"
        , Option ['s'] [] (NoArg Silent)
        "Keep stdout to a minimum"
        , Option [] ["help"] (NoArg Help) 
        "Prints this message"]

parseCmds :: [String] -> IO ([Flag], [String])
parseCmds argv = 
    if P.null argv
       then do putStrLn $ "Please enter one of the following option:\n" ++ usageInfo header flags
               exitFailure
       else
            case getOpt Permute flags argv of
              (args, fs, []) -> do
                  let files = if P.null fs then ["-"] else fs
                  if Help `P.elem` args
                     then do putStrLn (usageInfo header flags)
                             exitSuccess
                     else return (nub args, files)
              (_,_,errs) -> do
                  putStrLn (concat errs ++ usageInfo header flags)
                  exitWith (ExitFailure 1)
              where header = "Usage: DPLL [-vs] [file ...]"

----------
-- Main --
----------

process :: Flag -> [FilePath] -> IO ()
process arg files =
    case arg of
         Verbose -> 
             forM_ files $ \f -> do
             contents <- C.readFile f
             case parse cnf contents of
                  Fail i xs err -> die err
                  Done i xs -> do
                      case solve xs of
                           Nothing -> die $ "File " ++ f ++ " is unsatisfiable."
                           Just res -> print res
         Silent -> 
             forM_ files $ \f -> do
             contents <- C.readFile f
             case parse cnf contents of
                  Fail i xs err -> die err
                  Done i xs -> do
                      case solve xs of
                           Nothing -> die $ "File " ++ f ++ " is unsatisfiable."
                           Just res -> return ()
         _ -> error "unknown flag"

main :: IO ()
main = do
    (args, files) <- getArgs >>= parseCmds
    if P.null files 
       then die "you didn't supply any files"
       else do start <- getTime Monotonic
               process (P.head args) files
               end <- getTime Monotonic
               putStrLn "All Satisfiable."
               fprint (timeSpecs % "\n") start end
