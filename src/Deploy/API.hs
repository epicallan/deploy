{-# LANGUAGE ScopedTypeVariables #-}

module Deploy.API (startApp) where

import           Protolude                            hiding (decodeUtf8, get)

import           Data.ByteString.Builder              (hPutBuilder)
import           Data.Default.Class                   (def)
import           GHC.IO.Handle                        (BufferMode (BlockBuffering),
                                                       hClose, hSetBinaryMode,
                                                       hSetBuffering)
import           Network.Wai.Handler.Warp             (setPort, setTimeout)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           System.Directory                     (createDirectoryIfMissing,
                                                       getCurrentDirectory)

import           Web.Scotty

import           Deploy.Execute                       (executeDeploy)
import           Deploy.Types                         (RepoEx (..))

import qualified Blaze.ByteString.Builder             as B
import qualified Data.ByteString                      as BS
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as L

opts :: Options
opts = def { verbose = 1
           , settings = setTimeout 1000000 $ setPort 8888 $ settings def
           }
-- TODO: stream indications of whats going on during deployment
startApp :: IO ()
startApp = scottyOpts opts $ do
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
        _ <- liftIO $ forkIO $ executeDeploy repo
        text $ L.fromStrict $ "successfully started deployment for" <> rxName repo
