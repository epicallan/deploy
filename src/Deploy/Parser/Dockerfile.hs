{-# LANGUAGE MultiParamTypeClasses #-}

module Deploy.Parser.Dockerfile
    (
      exposedPort
    , parseDockerFile
    , test
    ) where

import           Data.HashMap.Lazy
import           Data.String       (String)
import           Data.Text         as T
import           Prelude           (read)
import           Protolude
import qualified Text.Parsec       as P



type Parser = P.Parsec String ()

-- parses docker statement so that we have something like (Expose, 9999), (start, "some command")
statement :: Parser (String, String)
statement = do
    key   <- P.many P.letter
    P.spaces
    value <- P.many (P.noneOf "\n")
    P.skipMany1 P.endOfLine
    return (key, value)


statements :: Parser [(String, String)]
statements =  P.spaces *> P.many1 statement <* P.eof

parseDockerFile :: String -> Either P.ParseError [(String, String)]
parseDockerFile = P.parse statements ""

-- The docker statement we want has exposed as key
exposedPort :: String -> Maybe Integer
exposedPort dockerFileContent =
    case parseDockerFile dockerFileContent of
        Left _      -> Nothing
        Right statementsList ->
            read <$> lookup "EXPOSE" (fromList statementsList)


test :: IO ()
test = do
 content <- readFile "./src/Deploy/Parser/Dockerfile"
 print content
 case parseDockerFile (T.unpack content) of
   Right results -> print results
   Left err      -> print err
