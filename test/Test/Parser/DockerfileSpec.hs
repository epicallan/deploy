
module Test.Parser.DockerfileSpec (spec) where

import           Data.String              (String)
import           Data.Text                (unpack)
import           Deploy.Parser.Dockerfile (parseDockerFile)
import           Protolude
import           Test.Hspec


-- TODO: fix hspec tests
spec :: Spec
spec = do
  content <- readFile "./test/Dockerfile"

  describe "parseDockerfile" $
    it "successfully parsed docker content" $
    case parseDockerFile (unpack content) of
      Right results -> results `shouldBe` [("FROM", "scratch")]
      Left err      -> err `shouldBe` "error"



