import Parser
import Lexer
import Compiler

main = do
    input <- getContents
    putStrLn input
    let tokens = scanTokens input
    print tokens
    let ast = parse tokens
    print ast
    putStrLn $ case runCompiler ast of
      Left e -> show e
      Right o -> output o
