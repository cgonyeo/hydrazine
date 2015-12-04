module Hydrazine.Server.Config where

import Options.Applicative

data Config = Config { filesDir :: FilePath
                     }

configParser :: Parser Config
configParser = Config <$> strOption ( long "files-dir"
                                   <> short 'd'
                                   <> help "directory to store images in"
                                    )
