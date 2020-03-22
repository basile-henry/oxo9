{-# LANGUAGE OverloadedStrings #-}

module Board where

-- scotty
import           Web.Scotty  (Parsable(..))

-- text
import           Data.Text.Lazy   (Text)
-- import qualified Data.Text.Lazy   as T

data Player = PlayerX | PlayerO deriving (Eq, Show)

instance Parsable Player where
  parseParam "x" = Right PlayerX
  parseParam "o" = Right PlayerO
  parseParam _   = Left "Unexpected"

data Cell = Empty | X | O deriving (Eq, Show)

newtype MiniBoard = MiniBoard [[Cell]] deriving Show

type Pos = (Int, Int)

data Board = Board
  { getPlayer :: Player
  , lastPosition :: Maybe Pos
  , getBoard :: [[MiniBoard]]
  } deriving Show

startBoard :: Board
startBoard =
  let grid = replicate 3 . replicate 3
  in Board PlayerX Nothing . grid . MiniBoard $ grid Empty

render :: Player -> Board -> Text
render _ _ = "hello"

getMiniBoard :: Board -> Pos -> MiniBoard
getMiniBoard (Board _ _ b) (x, y) = b !! y !! x

alreadyPlayed :: Pos -> Pos -> Board -> Bool
alreadyPlayed pos (mx,my) b =
  let MiniBoard mb = getMiniBoard b pos
  in mb !! my !! mx /= Empty 

miniBoardComplete :: MiniBoard -> Bool
miniBoardComplete (MiniBoard l) = all (notElem Empty) l

invalidPlacement :: Pos -> Pos -> Board -> Bool
invalidPlacement pos mpos b = 
  alreadyPlayed pos mpos b ||
    case lastPosition b of
      Nothing -> False
      Just lp -> not (miniBoardComplete (getMiniBoard b lp)) && (pos /= lp)

otherPlayer :: Player -> Player
otherPlayer PlayerX = PlayerO
otherPlayer PlayerO = PlayerX

playerCell :: Player -> Cell
playerCell PlayerX = X
playerCell PlayerO = O

update :: Int -> (a -> a) -> [a] -> [a]
update _ _ []     = []
update 0 f (x:xs) = f x : xs
update i f (x:xs) = x : update (i - 1) f xs

update2D :: Pos -> (a -> a) -> [[a]] -> [[a]]
update2D (x, y) f = update y (update x f)

updateMiniboard :: Cell -> Pos -> MiniBoard -> MiniBoard
updateMiniboard cell pos (MiniBoard mb) =
  MiniBoard (update2D pos (const cell) mb)

updateBoard :: Cell -> Pos -> Pos -> [[MiniBoard]] -> [[MiniBoard]]
updateBoard cell pos mpos = update2D pos (updateMiniboard cell mpos)

play :: Player -> Pos -> Pos -> Board -> Board
play player pos mpos board
  | player /= getPlayer board
  || invalidPlacement pos mpos board
  = board

  | otherwise =
    Board
      (otherPlayer player)
      (Just mpos)
      (updateBoard (playerCell player) pos mpos (getBoard board))
