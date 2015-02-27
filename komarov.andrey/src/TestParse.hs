import Parser
import Lexer

main = do
    input <- getContents
    putStrLn input
    print $ parse $ scanTokens $ input
