module ARM (
  Assembly(..),
  Segment(..)
  ) where


type Assembly = String

data Segment = Data | Text
             deriving (Eq)

instance Show Segment where
  show Data = "data"
  show Text = "text"

