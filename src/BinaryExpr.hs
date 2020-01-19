module BinaryExpr where

import Data.List
import qualified Data.Set as Set

mUnique::(Ord a) => [a] -> [a]
mUnique = Set.toList . Set.fromList

data BinExp = Variable Integer | Not BinExp | Or [BinExp] | And [BinExp] | BTrue | BFalse deriving (Eq, Ord)

type BinOp a = a -> a -> a

bnot::BinExp -> BinExp
bnot (Not e) = e
bnot e@Variable{} = Not e
bnot (Or e) = And (map bnot e)
bnot (And e) = Or (map bnot e)
bnot BTrue = BFalse
bnot BFalse = BTrue


--and
(&^&)::BinOp BinExp
(&^&) a b = And [a, b]

infixr 4 &^&

--or
(|^|)::BinOp BinExp
(|^|) a b = Or [a, b]

infixr 3 |^|

--xor
(^^^)::BinOp BinExp
(^^^) a b = bnot a &^& b |^| a &^& bnot b

infixr 5 ^^^

-- <=>
(=^=)::BinOp BinExp
(=^=) a b = bnot (a ^^^ b)

infixr 7 =^=

-- f + s + c = (sum, carry)
calcSum::BinExp -> BinExp -> BinExp -> (BinExp, BinExp)
calcSum a b c =
  (a ^^^ b ^^^ c, a &^& b |^| a &^& c |^| b &^& c)

type Number = [BinExp]

instance Show BinExp where
  show (Variable n) = "x_" ++ show n
  show (Not v) = "~ " ++ show v
  show (Or v) = "(" ++ foldl1 (\ a b -> a ++ " or " ++ b) (map show v) ++ ")"
  show (And v) = "(" ++ foldl1 (\ a b -> a ++ " and " ++ b) (map show v) ++ ")"
  show BFalse = "false"
  show BTrue = "true"

isAnd And{} = True
isAnd _ = False

isOr Or{} = True
isOr _ = False


flatten::BinExp -> BinExp
flatten (Or v) | any isOr v =
  let (ors, rest) = partition isOr v in
  let simpl = concat [ov | Or ov <- ors] in
  flatten $ Or $ simpl ++ rest
flatten (Or v) = Or $ map flatten v
flatten (And v) | any isAnd v =
  let (ands, rest) = partition isAnd v in
  let simpl = concat [andv | And andv <- ands] in
  flatten $ And $ simpl ++ rest
flatten (And v) = And $ map flatten v
flatten e = e

simplify::BinExp -> BinExp
simplify BTrue = BTrue
simplify BFalse = BFalse
simplify e@Variable{} = e

simplify (And v) =
  let s = map simplify v in
  if BFalse `elem` s then BFalse
  else
    let woTrue = filter (/= BTrue) s in
    case woTrue of
      [] -> BTrue
      [a] -> a
      _ -> And $ mUnique woTrue
simplify (Or v) =
  let s = map simplify v in
  if BTrue `elem` s then BTrue
  else
    let woFalse = filter (/= BFalse) s in
    case woFalse of
      [] -> BFalse
      [a] -> a
      _ -> Or $ mUnique woFalse
simplify e@(Not Variable{}) = e
simplify e = error $ "Incorrect expression: " ++ show e -- Not should be already propagated

isSimple Variable{} = True
isSimple (Not Variable{}) = True
isSimple _ = False

alwaysTrueOr::BinExp -> Bool
alwaysTrueOr (Or v) =
  let pos = Set.fromList [var | var@Variable{} <- v] in
  let neg = Set.fromList [var | Not var <- v] in
  not $ Set.null $ Set.intersection pos neg

alwaysFalseAnd::BinExp -> Bool
alwaysFalseAnd (And a) =
  let neg = Set.fromList $ map bnot a in
  let pos = Set.fromList a in
  not $ Set.null $ Set.intersection pos neg


simplifyCNF::BinExp -> BinExp
simplifyCNF (And v) =
  let filt = filter (not . alwaysTrueOr) v in
  And $ map (\(Or ov) -> Or $ mUnique ov) $ mUnique filt

toCNF::BinExp -> BinExp
toCNF e@Variable{} = And [Or[e]]
toCNF e@(Not Variable{}) = And [Or[e]]
toCNF (And v) =
  let c = concat [vc | And vc <- map toCNF v] in
  simplifyCNF $ And c
toCNF (Or v) | all isSimple v = And [Or v]

toCNF (Or v) =
  let ands = map toCNF v in
  let combined = combine ands in
  simplifyCNF $ And combined


-- lista andów orów do orów
combine::[BinExp] -> [BinExp]
combine [And l] = filter (not .alwaysTrueOr) l
combine (a:t) | alwaysFalseAnd a = combine t
combine (And l : t) = do
  (Or o) <- l
  (Or l) <- filter (not . alwaysTrueOr) $ combine t
  let or = Or $ mUnique $ o ++ l
  return or


maxVarNumber (And v) =
  let vs = concat [a | Or a <- v] in
  let posNums = [n | Variable n <- vs] in
  let negNums = [n | Not (Variable n) <- vs] in
  maximum (maximum posNums : negNums)


transformToCNF::BinExp -> BinExp
transformToCNF = simplifyCNF . toCNF . flatten . simplify 