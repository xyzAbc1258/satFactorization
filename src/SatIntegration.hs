module SatIntegration where

import qualified SAT.MiniSat as SAT
import BinaryExpr
import ExprBuilder


isSatisfiable::BinExp -> Bool
isSatisfiable e = SAT.satisfiable (toSatFormula e)

toSatFormula::BinExp -> SAT.Formula Integer
toSatFormula (And a) =
  let mapOrs = [SAT.Some $ map mapVars v | Or v <- a] in
      SAT.All mapOrs
  where mapVars (Variable e) = SAT.Var e
        mapVars (Not (Variable e)) = SAT.Not $ SAT.Var e
