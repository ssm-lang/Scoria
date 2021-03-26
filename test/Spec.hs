
import Evaluation
import Frontend
import Interpreter
import Fib
import Core
import Trace

main :: IO ()
main = do
    r <- testSingle $ myfib (int 13) (Ptr ("r", Ref TInt))
    case r of
        Left err -> putStrLn err
        Right _  -> putStrLn "test OK"

{- | Tests a fully applied SSM program by evaluating both the interpreter and running
the C code and comparing the output. -}
testSingle :: SSM () -> IO (Either String ())
testSingle program = do
    out1 <- runCG program
    out2 <- interpret' program
    let trace1 = mkTrace out1
    let trace2 = mkTrace $ unlines out2
    putStrLn $ show trace1
    putStrLn $ show trace2
    if trace1 /= trace2
        then return $ Left $ "--* output from c *--\n" ++ out1 ++
                             "\n\n--* output from interpreter --*\n" ++ unlines out2
        else return $ Right ()