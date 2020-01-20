module Main where

import System.IO
import Text.Read
import ExprBuilder
import BinaryExpr
import System.Environment (getArgs)
import Data.Maybe (isJust)
import System.Exit
import SatIntegration
import Common

asInt s = read s :: Integer

readInt = asInt <$> getLine

zeros = 0 : zeros

findDivider::Number -> BinExp -> Integer
findDivider n e =
  findDividerH (tail n) e 1 2
  where findDividerH [] _ s _ = s
        findDividerH (b:t) (And ne) s mn =
          let newE = And (Or[b] : ne) in
          if isSatisfiable newE then findDividerH t newE (s + mn) (mn * 2)
          else findDividerH t (And (Or[bnot b] :ne)) s (mn * 2)

main :: IO ()
main = do
  args <- getArgs
  num <- case args of
    [a] -> return (asInt a)
    _ -> readInt
  let (s,bits) = sizeInBits num
  let ((fstN, _, number), exprs) = runBuilder s
  let expr = transformToCNF $ And [setBits number (bits ++ zeros), exprs]
  let cnfForm = showCNF expr
  let numVars = maxVarNumber expr
  putStrLn $ show num ++ " " ++ show numVars ++ " " ++ show (length cnfForm)
  writeFile ("./result_" ++ show num ++ ".cnf") (joinNewLine cnfForm)
  let ifIsPrime = isPrime num
  let divid = if ifIsPrime then 1 else findDivider fstN expr
  let r = num `div` divid
  putStrLn $ show divid ++ " * " ++ show r ++ " = " ++ show (divid * r)
  case (isPrime num, isSatisfiable expr) of
    (a,b) | a /= b -> putStrLn "OK" >> exitSuccess
    _ -> putStrLn "ERROR" >> exitFailure


setBits::Number -> [Integer] -> BinExp
setBits n b = And $ zipWith (\e b -> if b == 1 then e else bnot e) n b

showCNF::BinExp -> [String]
showCNF (And v) = map showOrs v
  where showOrs (Or v) = foldl (\a b -> a ++ showVar b ++ " ") "" v ++ "0"
        showVar (Variable e) = show e
        showVar (Not (Variable e)) = show (-1 * e)


