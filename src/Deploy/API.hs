module Deploy.API (startApp) where

import           Protolude                            hiding (decodeUtf8, get)


import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Parse                    (FileInfo (..))
import           Web.Scotty

import           Deploy.Execute.Core                  (buildContainer,
                                                       runContainer,
                                                       unarchiveFile)
import           Deploy.Types                         (Repo (..))

import qualified Data.ByteString.Char8                as BS
import qualified Data.ByteString.Lazy                 as B
import           Data.Text                            (pack)



startApp :: IO ()
startApp = scotty 8888 $ do
    -- Add any WAI middleware, they are run top-down.
    middleware logStdoutDev

    get "/" $ text "deploy server is live"

    get "/upload" $ do
        fs <- files
        let fs' = [ (BS.unpack (fileName fi), fileContent fi) | (_, fi) <- fs ]
        case headMay fs' of
            Nothing        -> pure ()
            Just (_fileName, _fileContent) -> do
                let filePath = "uploads" <> "/" <> _fileName
                let repo = Repo (Just $ pack _fileName) (Just $ pack filePath) Nothing Nothing
                liftIO $ B.writeFile filePath _fileContent
                text $ show repo
                -- liftIO $ runReaderT unarchiveFile repo
                -- ebuildResults <- liftIO $ runReaderT buildContainer repo
                -- case ebuildResults of
                --     Left err  ->  text $ show err
                --     Right id' -> do
                --         erunResult <- liftIO $ runContainer id'
                --         case erunResult of
                --             Left err ->  text $ show err
                --             Right _  ->  text "started container"
