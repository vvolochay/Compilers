module FCC.ARM (
  Assembly(..),
  Segment(..)
  ) where


type Assembly = String

data Segment = Data | Text
             deriving (Eq, Ord)

instance Show Segment where
  show Data = "data"
  show Text = "text"

