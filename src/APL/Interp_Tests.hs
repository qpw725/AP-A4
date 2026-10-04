module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import qualified APL.Util as Util (captureIO)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (removeFile)
import System.Info (os)
import System.IO (SeekMode (AbsoluteSeek), hClose, hFlush, hGetContents', hPutStr, hSeek, openTempFile, stdin, stdout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests, transactionTests]

captureIO :: [String] -> IO a -> IO ([String], a)
captureIO inputs action
  | os /= "mingw32" = Util.captureIO inputs action
  | otherwise =
      withTemp "apl-input" $ \input ->
        withTemp "apl-output" $ \output -> do
          hPutStr input $ unlines inputs
          hSeek input AbsoluteSeek 0
          threadDelay 50000
          hFlush stdout
          bracket
            ((,) <$> hDuplicate stdin <*> hDuplicate stdout)
            ( \(savedIn, savedOut) -> do
                hFlush stdout
                hDuplicateTo savedIn stdin
                hDuplicateTo savedOut stdout
                mapM_ hClose [savedIn, savedOut]
            )
            ( \_ -> do
                hDuplicateTo input stdin
                hDuplicateTo output stdout
                result <- action
                hFlush stdout
                hSeek output AbsoluteSeek 0
                outputText <- hGetContents' output
                pure (lines outputText, result)
            )
  where
    withTemp name use =
      bracket (openTempFile "." name)
        (\(path, handle) -> hClose handle >> removeFile path)
        (\(_, handle) -> use handle)

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
          @?= ([], Right (ValInt 1)),
      --
      testCase "KvPut and KvGet" $
        runEval
          ( do
              evalKvPut (ValInt 0) (ValInt 42)
              evalKvGet (ValInt 0)
          )
          @?= ([], Right (ValInt 42)),
      --
      testCase "KvPut overwrite" $
        runEval
          ( do
              evalKvPut (ValInt 0) (ValInt 1)
              evalKvPut (ValInt 0) (ValInt 2)
              evalKvGet (ValInt 0)
          )
          @?= ([], Right (ValInt 2))
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

          res @?= Right (ValInt 6) ,
        --
        testCase "KvPut and KvGet (IO)" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalKvPut (ValInt 0) (ValInt 10)
              evalKvGet (ValInt 0)
        (out, res) @?= ([], Right (ValInt 10)),
      --
        testCase "KvPut overwrite (IO)" $ do
          (out, res) <-
            captureIO [] $
              runEvalIO $ do
                evalKvPut (ValInt 1) (ValBool False)
                evalKvPut (ValInt 1) (ValBool True)
                evalKvGet (ValInt 1)
          (out, res) @?= ([], Right (ValBool True)),
        --
        testCase "Missing key prompt (Valid ValInt)" $ do
          (out, res) <-
            captureIO ["ValInt 5"] $
              runEvalIO $
                evalKvGet (ValInt 0)
          out @?= ["Invalid key: ValInt 0. Enter a replacement: "]
          res @?= Right (ValInt 5),
        --
        testCase "Missing key prompt (Valid ValBool)" $ do
          (out, res) <-
            captureIO ["ValBool True"] $
              runEvalIO $
                evalKvGet (ValInt 99)
          out @?= ["Invalid key: ValInt 99. Enter a replacement: "]
          res @?= Right (ValBool True),
        --
        testCase "Missing key prompt (Invalid input string)" $ do
          (out, res) <-
            captureIO ["lol"] $
              runEvalIO $
                evalKvGet (ValInt 0)
          out @?= ["Invalid key: ValInt 0. Enter a replacement: "]
          res @?= Left "Invalid value input: lol"
    ]

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
      check "Nested success commits through try/catch"
        ( do
            _ <- transaction (transaction (evalKvPut (ValInt 0) (ValInt 7) >> pure (ValInt 7)))
              `catch` failure "unexpected failure"
            evalKvGet (ValInt 0)
        )
        ([], Right $ ValInt 7),
      check "Nested failure keeps output from both levels"
        ( transaction $ do
            evalPrint "outer"
            transaction $ evalPrint "inner" >> failure "abort"
        )
        (["outer", "inner"], Left "abort"),
      check "Rollback restores the old value"
        ( do
            evalKvPut (ValInt 0) (ValInt 10)
            transaction (evalKvPut (ValInt 0) (ValInt 20) >> failure "abort")
              `catch` evalKvGet (ValInt 0)
        )
        ([], Right $ ValInt 10),
      check "Caught inner failure preserves outer writes"
        ( do
            _ <- transaction $ do
              evalKvPut (ValInt 0) (ValInt 1)
              transaction (evalKvPut (ValInt 0) (ValInt 2) >> failure "inner")
                `catch` pure (ValBool True)
            evalKvGet (ValInt 0)
        )
        ([], Right $ ValInt 1),
      check "Outer failure rolls back an inner success"
        ( do
            evalKvPut (ValInt 0) (ValInt 10)
            transaction
              ( do
                  _ <- transaction $ evalKvPut (ValInt 0) (ValInt 20) >> pure (ValInt 20)
                  failure "outer"
              )
              `catch` evalKvGet (ValInt 0)
        )
        ([], Right $ ValInt 10)
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
