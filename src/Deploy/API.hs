{-# LANGUAGE ScopedTypeVariables #-}

module Deploy.API (startApp) where

import           Protolude                            hiding (decodeUtf8, get)

import           Data.ByteString.Builder              (hPutBuilder)

import           GHC.IO.Handle                        (BufferMode (BlockBuffering),
                                                       hClose, hSetBinaryMode,
                                                       hSetBuffering)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           System.Directory                     (createDirectoryIfMissing,
                                                       getCurrentDirectory,
                                                       removeDirectoryRecursive)

import           Web.Scotty

import           Deploy.Execute                       (buildContainer,
                                                       runContainer,
                                                       unarchiveFile)
import           Deploy.Types                         (RepoEx (..))

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
        currentDir        <- liftIO getCurrentDirectory
        let uploadPath   = T.pack currentDir <> "/" <> "uploads" <> "/" <> name
        liftIO $ createDirectoryIfMissing True (T.unpack uploadPath)
        let fileArchive   = uploadPath <> "/" <> name <> ".tar.gz"
        let writeHandle   = openFile (T.unpack fileArchive) ReadWriteMode
        let repo          = RepoEx name uploadPath fileArchive
        let buildFile acc = do -- accumulates a builder
                    chunk <- rd
                    let len             = BS.length chunk
                    let newBuilderChunk = acc <> B.insertByteString chunk
                    if len > 0
                        then buildFile newBuilderChunk
                        else return acc
        builder <- liftIO $ buildFile mempty
        _       <- return B.flush
        wHandle <- liftIO writeHandle
        liftIO $ hSetBinaryMode wHandle True
        liftIO $ hSetBuffering wHandle (BlockBuffering Nothing)
        liftIO $ hPutBuilder wHandle builder
        liftIO $ hClose wHandle
        liftIO $ runReaderT unarchiveFile repo
        ebuildResults <- liftIO $ runReaderT buildContainer repo
        case ebuildResults of
            Left err          ->  text $ show err
            Right containerId -> do
                erunResult <- liftIO $ runContainer containerId
                -- clean up
                liftIO $ removeDirectoryRecursive (T.unpack uploadPath)
                case erunResult of
                    Left err ->  text $ "docker error: " <> show err
                    Right _  ->  text $ "started container for: " <> L.fromStrict name
