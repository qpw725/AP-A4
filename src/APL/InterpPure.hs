module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
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
    -- Task 1's branch logic, with state passed through for transactions.
    runEvalState r s (Free (TryCatchOp m1 m2 k)) =
      let (s1, ps1, res1) = runEvalState r s m1
      in case res1 of
        Right v ->
          let (s2, ps2, res2) = runEvalState r s1 $ k v
          in (s2, ps1 ++ ps2, res2)
        Left _ ->
          let (s2, ps2, res2) = runEvalState r s m2
          in case res2 of
            Right v ->
              let (s3, ps3, res3) = runEvalState r s2 $ k v
              in (s3, ps1 ++ ps2 ++ ps3, res3)
            Left e -> (s2, ps1 ++ ps2, Left e)
    -- Task 2's lookup and update logic uses the same state as transactions.
    runEvalState r s (Free (KvGetOp key k)) =
      case lookup key s of
        Just val -> runEvalState r s $ k val
        Nothing -> (s, [], Left "value not in evironment")
    runEvalState r s (Free (KvPutOp key val m)) =
      let s' = (key, val) : filter (\(k, _) -> k /= key) s in
        runEvalState r s' m
    runEvalState r s (Free (TransactionOp m k)) =
      let (s', ps, res) = runEvalState r s m
       in case res of
            -- Roll back the store, but keep output and propagate the error.
            Left e -> (s, ps, Left e)
            Right val ->
              -- The continuation runs after the transaction has committed.
              let (s'', ps', res') = runEvalState r s' $ k val
               in (s'', ps ++ ps', res')
