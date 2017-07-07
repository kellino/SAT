{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad (void, forM_)
import Text.Megaparsec
import Text.Megaparsec.ByteString
import qualified Text.Megaparsec.Lexer as L
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Data.List (nub, intercalate)

import Formatting (fprint, (%))
import Formatting.Clock
import System.Clock
import System.Console.CmdArgs.GetOpt
import System.Environment
import System.Exit 

-- | A simple, unoptimized, implementation of DPLL
-- https://en.wikipedia.org/wiki/DPLL_algorithm 

-----------------------------
-- DataTypes and Instances --
-----------------------------

data Literal a = Pos a | Neg a deriving (Eq, Show)
type Clause a  = [Literal a]
type Formula a = [Clause a]
type Solution a  = [Literal a]

class PPrint a where
    printString :: a -> String

instance (PPrint a) => PPrint (Literal a) where
    printString (Pos x) = printString x
    printString (Neg x) = "-" ++ printString x

instance (PPrint C.ByteString) where
    printString = C.unpack

data SAT a = SAT {formula :: Formula a, solution :: Solution a} deriving Show

--------------------
-- Main Algorithm --
--------------------

dpll :: (Eq a) => SAT a -> Maybe (Solution a)
dpll s
    | null f = return r
    | otherwise = do
        l <- chooseLiteral f
        case dpll (SAT (simplify f l) (l:r)) of
             Just res -> return $ reverse res
             Nothing  -> dpll $ SAT (simplify f (flipLit l)) (flipLit l:r)
    where 
        s' = propagate findPure . propagate getUnit $ s
        r  = solution s'
        f  = formula s'

propagate :: (Eq a) => (Formula a -> Maybe (Literal a)) -> SAT a -> SAT a
propagate p (SAT f r) =
    case p f of
         Nothing -> SAT f r
         Just u -> propagate p $ SAT (simplify f u) (u:r)

getUnit :: (Eq a) => Formula a -> Maybe (Literal a)         
getUnit xs = listToMaybe [x | [x] <- xs]
{-# INLINE getUnit #-}

chooseLiteral :: (Eq a) => Formula a -> Maybe (Literal a)
chooseLiteral = listToMaybe . concat 
{-# INLINE chooseLiteral #-}

isPure :: (Eq a) => Formula a -> Literal a -> Bool
isPure f l = flipLit l `notElem` concat f

findPure :: (Eq a) => Formula a -> Maybe (Literal a)
findPure f = locate (nub . concat $ f)
    where locate ls = listToMaybe [x | x <- ls, isPure f x]
{-# INLINE findPure #-}

simplify :: (Eq a) => Formula a -> Literal a -> Formula a
simplify f l = [simp x l | x <- f, l `notElem` x]
    where simp c l' = filter (/= flipLit l') c

flipLit :: (Eq a) => Literal a -> Literal a
flipLit (Pos x) = Neg x
flipLit (Neg x) = Pos x
{-# INLINE flipLit #-}

solve :: (Eq a) => Formula a -> Maybe (Clause a)
solve = dpll . flip SAT []

-----------------------
-- Simple CNF Parser -- 
-----------------------

sc :: Parser ()
sc = L.space (void space) lineCmnt formatCmnt

scn :: Parser ()
scn = L.space (void spaceChar) lineCmnt formatCmnt

lineCmnt :: Parser ()
lineCmnt = L.skipLineComment "c"

formatCmnt :: Parser ()
formatCmnt = L.skipLineComment "p"

pos :: Parser (Literal C.ByteString)
pos = do
    lit <- some digitChar 
    return $ Pos (C.pack lit)

neg :: Parser (Literal C.ByteString)
neg = do
    _ <- char '-'
    lit <- some digitChar 
    return $ Neg (C.pack lit)

literal :: Parser (Literal C.ByteString)
literal = neg <|> pos

clause :: Parser (Clause C.ByteString)
clause = scn *> (literal <* space) `manyTill` string "0\n"

cnf :: Parser (Formula C.ByteString)
cnf = clause `manyTill` string "%\n0"

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
    if null argv
       then do putStrLn $ "Please enter one of the following option:\n" ++ usageInfo header flags
               exitFailure
       else
            case getOpt Permute flags argv of
              (args, fs, []) -> do
                  let files = if null fs then ["-"] else fs
                  if Help `elem` args
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
             case parse cnf f contents of
                  Left err -> die (parseErrorPretty err)
                  Right xs -> 
                    case solve xs of
                         Nothing -> die $ "File " ++ f ++ " is unsatisfiable."
                         Just x -> putStrLn $ "Satisfiable: " ++ intercalate ", " (map printString x)
         Silent -> 
             forM_ files $ \f -> do
             contents <- C.readFile f
             case parse cnf f contents of
                  Left err -> die (parseErrorPretty err)
                  Right xs -> 
                    case solve xs of
                         Nothing -> die $ "File " ++ f ++ " is unsatisfiable."
                         Just _ -> return ()
         _ -> error "unknown flag"

main :: IO ()
main = do
    (args, files) <- getArgs >>= parseCmds
    if null files 
       then die "you didn't supply any files"
       else do start <- getTime Monotonic
               process (head args) files
               end <- getTime Monotonic
               putStrLn "All Satisfiable."
               fprint (timeSpecs % "\n") start end
