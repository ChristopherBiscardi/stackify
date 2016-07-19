module Main where

import Options.Applicative

import Lib

config :: Parser Config
config = Config
     <$> strOption
         ( long "input-dir"
        <> short 'i'
        <> metavar "SOURCE"
        <> help "Source directory to read templates from" )
     <*> strOption
         ( long "output-dir"
        <> short 'o'
        <> metavar "DESTINATION"
        <> help "Directory to output .hsfiles to" )

main :: IO ()
main = execParser opts >>= makeTemplates
  where
    opts = info (helper <*> config)
      ( fullDesc
     <> progDesc "Construct Stack templates from a directory of projects"
     <> header "stackify - stack template creation helper" )
