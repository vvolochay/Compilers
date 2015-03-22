import Parser
import Lexer
import TypedAST

main = do
    input <- getContents
    putStrLn input
    let tokens = scanTokens input
    print tokens
    let ast = parse tokens
    print ast
    print $ runTypecheck ast
