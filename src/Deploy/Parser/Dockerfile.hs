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

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- parses docker statement so that we have something like (Expose, 9999), (start, "some command")
statement :: Parser (Text, Text)
statement = lexeme $ do
    key   <- many letterChar
    space1
    value <- many (notChar '\n')
    return (T.pack key, T.pack value)


statements :: Parser [(Text, Text)]
statements = spaceConsumer >> many statement <* eof

parseDockerFile :: Text -> Either Text [(Text, Text)]
parseDockerFile text = prettifyError $ runParser statements "" text
  where
    prettifyError = first (T.pack . parseErrorPretty' text)

-- The docker statement we want has exposed as key
exposedPort :: Text -> Maybe Integer
exposedPort dockerFileContent =
    case parseDockerFile dockerFileContent of
        Left _      -> Nothing
        Right statementsList ->
            read . T.unpack <$> lookup "EXPOSE" (fromList statementsList)

