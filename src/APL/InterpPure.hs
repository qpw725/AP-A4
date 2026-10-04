module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' r s (Free (TryCatchOp m1 m2 k)) =
      let (ps1, res1) = runEval' r s m1
      in case res1 of
        Right v ->
          let (ps2, res2) = runEval' r s $ k v
          in (ps1 ++ ps2, res2)
        Left _ ->
          let (ps2, res2) = runEval' r s m2
          in case res2 of
            Right v ->
              let (ps3, res3) = runEval' r s $ k v
              in (ps1 ++ ps2 ++ ps3, res3)
            Left e -> (ps1 ++ ps2, Left e)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)
