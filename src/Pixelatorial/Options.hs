{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE ApplicativeDo #-}

module Pixelatorial.Options
  ( PixelatorialOptions(..)
  , parsePixelatorialOptions
  )
where

import Options.Applicative

data PixelatorialOptions
  = PixelatorialOptions
  { cycles :: Maybe Int
  , colorSet :: FilePath
  , skip :: Maybe Int
  , outputFolder :: Maybe String
  , height :: Int
  , width :: Int
  } deriving (Show, Eq, Read)

parsePixelatorialOptions = execParser $ info
  (pixelatorialOptionsParser <**> helper)
  (fullDesc <> header "Pixelatorial - Exhaustive image painter" <> progDesc
    "Draw every image for any canvas with any amount of colors"
  )

pixelatorialOptionsParser = do
  cycles <- optional $ read <$> strOption
    (  long "cycles"
    <> short 'c'
    <> help "Amount of iterations to perform. If not specified, it will iterate until the end. Applied after offset."
    <> metavar "CYCLES"
    )
  colorSet <- strOption
    (  long "color-set"
    <> help
         "Path to color set. Should be a .csv with the RGB colors (format: red,green,blue) or a list of hex; either of them separated by newline."
    <> metavar "COLOR-SET"
    )
  skip <- optional $ read <$> strOption
    (long "skip" <> short 's' <> help "Skip OFFSET combinations before drawing." <> metavar "OFFSET")
  outputFolder <- optional $ read <$> strOption
    (long "output" <> short 'o' <> help "Folder to put the drawings. Default is svg/" <> metavar "OUTPUT")
  height <- read <$> strOption (long "height" <> help "Canvas' height" <> metavar "HEIGHT")
  width  <- read <$> strOption (long "width" <> help "Canvas' width" <> metavar "WIDTH")
  return PixelatorialOptions { .. }
