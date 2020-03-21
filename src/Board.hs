{-# LANGUAGE OverloadedStrings #-}

module Board where

-- scotty
import           Web.Scotty  (Parsable(..))

-- text
import           Data.Text.Lazy   (Text)
import qualified Data.Text.Lazy   as T

data Player = PlayerX | PlayerO

instance Parsable Player where
  parseParam "x" = Right PlayerX
  parseParam "o" = Right PlayerO
  parseParam _   = Left "Unexpected"

data Cell = Empty | X | O

newtype MiniBoard = MiniBoard [[Cell]]

data Board = Board
  { player :: Player
  , board  :: [[MiniBoard]]
  }

startBoard :: Board
startBoard =
  let grid = replicate 3 . replicate 3
  in Board PlayerX . grid . MiniBoard $ grid Empty

render :: Player -> Board -> Text
render _ _ = "hello"

play :: Player -> (Int, Int) -> (Int, Int) -> Board -> Board
play _ _ _ board = board
