module Deploy.Initiate.Core where

{-|
  functions;
  runCmd:: String -> ExceptT CmdErrors IO ()

  archiveGitFile :: Except cmdErrors IO ()

  archiveExtraFiles :: [GlobPattern] -> Except cmdErrors IO ()

  CmdErrorHandler :: CmdErrors -> Except cmdErrors Text ()

  getRepo :: IO (Either Text Text)

  pushUpStream :: Archive -> ReaderT Repo IO ()

  runDeployInit :: ReaderT Repo IO ()

-}
