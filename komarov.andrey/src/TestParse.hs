import Parser
import Lexer

main = do
    input <- getContents
    putStrLn input
    print $ scanTokens $ input
    print $ parse $ scanTokens $ input
