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
    runEvalState r s (Free (TransactionOp m k)) =
      let (s', ps, res) = runEvalState r s m
       in case res of
            -- Roll back the store, but keep output and propagate the error.
            Left e -> (s, ps, Left e)
            Right val ->
              -- The continuation runs after the transaction has committed.
              let (s'', ps', res') = runEvalState r s' $ k val
               in (s'', ps ++ ps', res')
