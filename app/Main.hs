module Main (main) where

import           Command

import           Options.Applicative

main :: IO ()
main = run =<< execParser (info (opts <**> helper) fullDesc)

opts :: Parser Command
opts = subparser (command "add" addParser)

addParser :: ParserInfo Command
addParser = info
  (Add <$> some (strArgument (metavar "PATH..." <> help "Files to add"))
   <*> switch
     (long "dry-run"
      <> short 'n'
      <> help "Don't actually add the files to the index")
   <**> helper)
  (fullDesc
   <> progDesc "Add file contents to the index"
   <> header "git-add - Add file contents to the index")
