module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests, transactionTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),
      --
      testCase "TryCatch" $
        eval'
          (TryCatch
            (Div (CstInt 1) (CstInt 0))
            (CstInt 2))
          @?= ([], Right (ValInt 2)),
      --
      testCase "TryCatch - first succeeds" $
        runEval
          (catch (pure $ ValInt 5) (pure $ ValInt 10))
          @?= ([], Right (ValInt 5)),
      --
      testCase "TryCatch - fallback succeeds" $
        runEval
          (catch (failure "Some error") (pure $ ValInt 10))
          @?= ([], Right (ValInt 10)),
      --
      testCase "TryCatch - both fail" $
        runEval
          (catch (failure "First error") (failure "Second error"))
          @?= ([], Left "Second error"),
      --
      testCase "TryCatch - continues after success" $
        runEval
          ( do
              v <- catch (pure $ ValInt 5) (pure $ ValInt 10)
              case v of
                ValInt x -> pure $ ValInt (x + 1)
                _ -> failure "Error - not an Integer"
          )
          @?= ([], Right (ValInt 6)),
      --
      testCase "TryCatch - print before success" $
        runEval
          ( catch
              (do 
                evalPrint "print"
                pure $ ValInt 1)
              (pure $ ValInt 2)
          )
          @?= (["print"], Right (ValInt 1)),
      --
      testCase "TryCatch - print before failure" $
        runEval
          ( catch
              (do 
                evalPrint "print"
                failure "failed")
              (pure $ ValInt 2)
          )
          @?= (["print"], Right (ValInt 2)),
      --
      testCase "TryCatch - localEnv in first branch" $
        eval'
          ( Let
              "x"
              (CstInt 1)
              (TryCatch
                (Var "x")
                (CstInt 2))
          )
          @?= ([], Right (ValInt 1)),
      --
      testCase "TryCatch - localEnv in fallback branch" $
        eval'
          ( Let
              "x"
              (CstInt 1)
              (TryCatch
                (Var "NotExisting")
                (Var "x"))
          )
          @?= ([], Right (ValInt 1)),
      --
      testCase "TryCatch - fallback not evaluated" $
        runEval
          ( catch
              (pure $ ValInt 1)
              (do
                evalPrint "catch"
                pure $ ValInt 2)
          )
          @?= ([], Right (ValInt 1))
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ()),
        -- NOTE: This test will give a runtime error unless you replace the
        -- version of `eval` in `APL.Eval` with a complete version that supports
        -- `Print`-expressions. Uncomment at your own risk.
        -- testCase "print 2" $ do
        --    (out, res) <-
        --      captureIO [] $
        --        evalIO' $
        --          Print "This is also 1" $
        --            Print "This is 1" $
        --              CstInt 1
        --    (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1)
        --
        testCase "TryCatch IO" $ do
          res <-
            evalIO' $
              TryCatch
                (Div (CstInt 1) (CstInt 0))
                (CstInt 2)

          res @?= Right (ValInt 2),
        --
        testCase "TryCatch IO - first succeeds" $ do
          res <-
            runEvalIO $
              catch
                (pure $ ValInt 1)
                (pure $ ValInt 2)
          res @?= Right (ValInt 1),
        --
        testCase "TryCatch IO - fallback succeeds" $ do
          res <-
            runEvalIO $
              catch
                (failure "Error")
                (pure $ ValInt 1)
          res @?= Right (ValInt 1),
        --
        testCase "TryCatch IO - both fail" $ do
          res <-
            runEvalIO $
              catch
                (failure "First")
                (failure "Second")
          res @?= Left "Second",
        --
        testCase "TryCatch IO - second branch ignored" $ do
          (out, res) <-
            captureIO [] $
              runEvalIO $
                catch
                  (do
                    evalPrint "try"
                    pure $ ValInt 1)
                  (do
                    evalPrint "catch"
                    pure $ ValInt 2)
          (out, res) @?= (["try"], Right (ValInt 1)),
        --
        testCase "TryCatch IO - continues after success" $ do
          res <-
            runEvalIO $ do
              v <- catch
                (failure "Error")
                (pure $ ValInt 5)

              case v of
                ValInt x -> pure $ ValInt (x + 1)
                _ -> failure "Error - not an Integer"

          res @?= Right (ValInt 6) 
    ]

-- Task 3 examples that do not need the Task 1 or Task 2 implementations.
transactionTests :: TestTree
transactionTests =
  testGroup
    "Transactions"
    [ check "Result, continuation, and variable scope"
        (eval $ Let "x" (CstInt 7) $ Add (Transaction $ Var "x") (CstInt 1))
        ([], Right $ ValInt 8),
      check "Failure keeps printed output and propagates the error"
        (transaction $ evalPrint "hello" >> failure "abort")
        (["hello"], Left "abort"),
      check "Nested success returns the inner value"
        (transaction $ transaction $ pure $ ValInt 7)
        ([], Right $ ValInt 7),
      check "Nested failure keeps output from both levels"
        ( transaction $ do
            evalPrint "outer"
            transaction $ evalPrint "inner" >> failure "abort"
        )
        (["outer", "inner"], Left "abort")
    ]
  where
    check :: String -> EvalM Val -> ([String], Either Error Val) -> TestTree
    check name m expected =
      testGroup name
        [ testCase "Pure" $ runEval m @?= expected,
          testCase "IO" $ do
            actual <- captureIO [] $ runEvalIO m
            actual @?= expected
        ]
