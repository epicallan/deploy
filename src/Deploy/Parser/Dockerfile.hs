{-# LANGUAGE MultiParamTypeClasses #-}

module Deploy.Parser.Dockerfile
    (
      exposedPort
    , parseDockerFile
    ) where

import           Data.HashMap.Lazy
import           Data.Text                  as T
import           Prelude                    (read)
import           Protolude
import           Text.Megaparsec            (Parsec, eof, runParser)
import           Text.Megaparsec.Char       (letterChar, notChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L (lexeme, skipBlockComment,
                                                  skipLineComment, space)
import           Text.Megaparsec.Error      (parseErrorPretty')




type Parser = Parsec Void Text

-- skip docker line comments
lineComment :: Parser ()
lineComment = L.skipLineComment "#"

-- I am not sure whether docker has blockcomments, these are just there.
blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- parses docker statement so that we have something like (EXPOSE, 9999), (START, "some start command")
statement :: Parser (Text, Text)
statement = lexeme $ do
    key   <- many letterChar
    space1
    value <- many (notChar '\n')
    return (T.pack key, T.pack value)


statements :: Parser [(Text, Text)]
statements = spaceConsumer >> many statement <* eof

-- Parses Dockerfile and outputs an array of [(key),(value)] where key is some dockerfile command such as RUN and value
-- is its value
parseDockerFile :: Text -> Either Text [(Text, Text)]
parseDockerFile text = prettifyError $ runParser statements "" text
  where
    prettifyError = first (T.pack . parseErrorPretty' text)

-- Helper function that gets us the exposed PORT in a dockerfile 
-- The docker statement we want has EXPOSE as key
exposedPort :: Text -> Maybe Integer
exposedPort dockerFileContent =
    case parseDockerFile dockerFileContent of
        Left _      -> Nothing
        Right statementsList ->
            read . T.unpack <$> lookup "EXPOSE" (fromList statementsList)

