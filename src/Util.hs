module Util where

import Data.Text

trim = unpack . strip . pack
