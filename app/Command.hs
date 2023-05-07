module Command (Command(..), run) where

import           Command.Add

data Command = Add [FilePath] Bool
  deriving stock (Eq)

run :: Command -> IO ()
run (Add paths dryRun) = runAdd paths dryRun