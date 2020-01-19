{-# LANGUAGE TupleSections #-}
module ExprBuilder where

import Control.Monad.State
import Control.Monad.Writer
import BinaryExpr

type Builder = WriterT [BinExp] (State Integer)

tellS::BinExp -> Builder()
tellS = tell . (:[])

-- n liczba bitów
runBuilder::Integer -> ((Number, Number, Number), BinExp)
runBuilder n =
  let (num, es) = evalState (runWriterT $ buildMultiplicaton n) 1 in
  (num, And es)

newVar::Builder BinExp
newVar = do
  c <- get
  modify (+ 1)
  return (Variable c)

falseRep = flip replicate BFalse

replI n = replicate (fromIntegral n)

buildMultiplicaton::Integer -> Builder (Number, Number, Number)
buildMultiplicaton n = do
  let half = n `div` 2 + n `mod` 2
  fBits <- sequence (replI half newVar)
  sBits <- sequence (replI n newVar)
  tellS $ head fBits -- nieparzysta
  tellS $ Or $ tail fBits -- rózna od jeden
  let numbers = mulH sBits fBits (length fBits) 0 []
  (fBits, sBits, ) <$> sumMany numbers
  where mulH b [] _ _ r = reverse r
        mulH b (h:t) end start r =
          let anded = map (h &^&) b in
          let curr = falseRep start ++ anded ++ falseRep end in
          mulH b t (end - 1) (start + 1) (curr : r)

sumMany::[Number] -> Builder Number
sumMany l = head <$> sumManyH l
  where sumManyH [s] = return [s]
        sumManyH [f,s] = (:[]) <$> bSum f s
        sumManyH (f:s:t)  = do
          fr <- bSum f s
          rest <- sumManyH t
          sumManyH (fr:rest)

bSum::Number -> Number -> Builder Number
bSum a b = bSumH a b BFalse []
  where bSumH [] [] _ n = return $ reverse n
        bSumH (ah: at) (bh: bt) c number = do
          let (r, nCarry) = calcSum ah bh c
          nr <- newVar
          nc <- newVar
          tellS $ nr =^= r
          tellS $ nc =^= nCarry
          bSumH at bt nc (nr: number)