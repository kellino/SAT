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
import System.Environment
import System.Exit (die)
import Data.Maybe
import Data.List (nub, intercalate)


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

chooseLiteral :: (Eq a) => Formula a -> Maybe (Literal a)
chooseLiteral = listToMaybe . concat 

isPure :: (Eq a) => Formula a -> Literal a -> Bool
isPure f l = flipLit l `notElem` concat f

findPure :: (Eq a) => Formula a -> Maybe (Literal a)
findPure f = locate (nub . concat $ f)
    where locate ls = listToMaybe [x | x <- ls, isPure f x]

simplify :: (Eq a) => Formula a -> Literal a -> Formula a
simplify f l = [simp x l | x <- f, l `notElem` x]
    where simp c l' = filter (/= flipLit l') c

flipLit :: (Eq a) => Literal a -> Literal a
flipLit (Pos x) = Neg x
flipLit (Neg x) = Pos x

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

----------
-- Main --
----------

main :: IO ()
main = do
    files <- getArgs
    if null files 
       then die "you didn't supply any files"
       else forM_ files $ \f -> do
                    contents <- C.readFile f
                    case parse cnf f contents of
                         Left err -> putStr (parseErrorPretty err)
                         Right xs ->
                             case solve xs of
                                  Nothing -> putStrLn "Unsatisfiable"
                                  Just x -> putStrLn $ "Satisfiable: " ++ intercalate ", " (map printString x)
