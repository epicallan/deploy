module Deploy.API (startApp) where

import           Protolude                            hiding (decodeUtf8, get)


import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Web.Scotty

-- import           Deploy.Execute.Core                  (buildContainer,
--                                                        runContainer,
--                                                        unarchiveFile)
-- import           Deploy.Types                         (Repo (..))

import qualified Blaze.ByteString.Builder             as B
import qualified Data.ByteString                      as BS
import           Data.ByteString.Builder              (hPutBuilder)
import qualified Data.ByteString.Lazy                 as BSL
-- import           Data.Text                            (pack)
import           GHC.IO.Handle                        (BufferMode (BlockBuffering),
                                                       hSetBinaryMode,
                                                       hSetBuffering)


-- buildFile :: IO BS.ByteString -> B.Builder
-- buildFile reader = do
--     chuck <- reader
--     let len = BS.length chuck
--     let builder =  B.insertByteString chunk
--     if len > 0
--         then

startApp :: IO ()
startApp = scotty 8888 $ do
    -- Add any WAI middleware, they are run top-down.
    middleware logStdoutDev

    get "/" $ text "deploy server is live"

    post "/upload" $ do
        rd      <- bodyReader
        let buildFile acc = do -- accumulates a builder
                    chunk <- rd
                    let len      = BS.length chunk
                    let newBuilderChunk = acc <> B.insertByteString chunk
                    if len > 0
                        then buildFile newBuilderChunk
                        else return acc
        builder <- liftIO $ buildFile mempty
        _ <-  return B.flush
        wHandle <- liftIO writeHandle
        liftIO  $ hSetBinaryMode wHandle True
        liftIO  $ hSetBuffering wHandle (BlockBuffering Nothing)
        liftIO  $ hPutBuilder wHandle builder
        text $ "added new upload of size: " <> show (BSL.length $ B.toLazyByteString builder)
        where
          writeHandle :: IO Handle
          writeHandle =  openFile "uploads/test.tar.gz" WriteMode
            -- let handle =
            -- in   handle >>= flip hSetBinaryMode True
            -- >>=>>= flip hSetBuffering (BlockBuffering Nothing)

    --     let fs' = [ (BS.unpack (fileName fi), fileContent fi) | (_, fi) <- fs ]
    --     case headMay fs' of
    --         Nothing        -> pure ()
    --         Just (_fileName, _fileContent) -> do
    --             let filePath = "uploads" <> "/" <> _fileName
    --             let repo = Repo (Just $ pack _fileName) (Just $ pack filePath) Nothing Nothing
    --             liftIO $ B.writeFile filePath _fileContent
    --             -- text $ show repo
    --             liftIO $ runReaderT unarchiveFile repo
    --             ebuildResults <- liftIO $ runReaderT buildContainer repo
    --             case ebuildResults of
    --                 Left err  ->  text $ show err
    --                 Right id' -> do
    --                     erunResult <- liftIO $ runContainer id'
    --                     case erunResult of
    --                         Left err ->  text $ show err
    --                         Right _  ->  text "started container"
