{-# LANGUAGE ScopedTypeVariables #-}

module Deploy.API (startApp) where

import           Protolude                            hiding (decodeUtf8, get)

import           Data.ByteString.Builder              (hPutBuilder)

import           GHC.IO.Handle                        (BufferMode (BlockBuffering),
                                                       hSetBinaryMode,
                                                       hSetBuffering)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Web.Scotty

import           Deploy.Execute                       (buildContainer,
                                                       runContainer,
                                                       unarchiveFile)
import           Deploy.Types                         (Repo (..))

import qualified Blaze.ByteString.Builder             as B
import qualified Data.ByteString                      as BS
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as L

startApp :: IO ()
startApp = scotty 8888 $ do
    -- Add any WAI middleware, they are run top-down.
    middleware logStdoutDev

    get "/" $ text "deploy server is live"

    post "/upload/:name" $ do
        (name :: Text) <- param "name"
        rd             <- bodyReader
        let uploadArchivePath = "uploads/" <> name <> ".tar.gz"
        let writeHandle       =  openFile (T.unpack uploadArchivePath) WriteMode
        let repo              = Repo (Just name) (Just uploadArchivePath) Nothing Nothing
        let buildFile acc     = do -- accumulates a builder
                    chunk <- rd
                    let len      = BS.length chunk
                    let newBuilderChunk = acc <> B.insertByteString chunk
                    if len > 0
                        then buildFile newBuilderChunk
                        else return acc
        builder <- liftIO $ buildFile mempty
        _       <-  return B.flush
        wHandle <- liftIO writeHandle
        liftIO $ hSetBinaryMode wHandle True
        liftIO $ hSetBuffering wHandle (BlockBuffering Nothing)
        liftIO $ hPutBuilder wHandle builder
        liftIO $ runReaderT unarchiveFile repo
        ebuildResults <- liftIO $ runReaderT buildContainer repo
        case ebuildResults of
            Left err  ->  text $ show err
            Right containerId -> do
                erunResult <- liftIO $ runContainer containerId
                case erunResult of
                    Left err ->  text $ show err
                    Right _  ->  text $ "started container for: " <> L.fromStrict name
