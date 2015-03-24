module ARM (
  Assembly(..),
  Segment(..)
  ) where


type Assembly = String

data Segment = Data | Text
             deriving (Show)
