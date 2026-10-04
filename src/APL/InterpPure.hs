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
    runEval' r s m =
      let (_, ps, res) = runEvalState r s m
       in (ps, res)

    -- Return the final state internally so an enclosing transaction can
    -- choose whether to commit it. The public result stays unchanged.
    runEvalState :: Env -> State -> EvalM a -> (State, [String], Either Error a)
    runEvalState _ s (Pure x) = (s, [], Right x)
    runEvalState r s (Free (ReadOp k)) = runEvalState r s $ k r
    runEvalState r s (Free (PrintOp p m)) =
      let (s', ps, res) = runEvalState r s m
       in (s', p : ps, res)
    runEvalState _ s (Free (ErrorOp e)) = (s, [], Left e)
    runEvalState r s (Free (TransactionOp m k)) =
      let (s', ps, res) = runEvalState r s m
       in case res of
            -- Roll back the store, but keep output and propagate the error.
            Left e -> (s, ps, Left e)
            Right val ->
              -- The continuation runs after the transaction has committed.
              let (s'', ps', res') = runEvalState r s' $ k val
               in (s'', ps ++ ps', res')
