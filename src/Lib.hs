module Lib
    ( someFunc
    ) where

import Protolude
import System.IO (IO(..))

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: Text)

