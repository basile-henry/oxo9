{-# LANGUAGE OverloadedStrings #-}

module Main where

-- base
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import           Data.Foldable

-- scotty
import           Web.Scotty

import           Board
import           Html

main :: IO ()
main = do
  board <- newMVar startBoard
  scotty 3000 $ do
    get "/reset" $
      liftIO $ putMVar board startBoard

    get "/:player" $ do
      player <- param "player"
      newBoard <- liftIO $ readMVar board
      liftIO $ print newBoard
      html . renderHtml $ page player newBoard

    get "/:player/:x:y:mx:my" $ do
      player <- param "player"
      x <- param "x"
      y <- param "y"
      mx <- param "mx"
      my <- param "my"

      liftIO $ print (player, x, y, mx, my)
      liftIO $ modifyMVar_ board
        (pure . play player (x, y) (mx, my))

      redirect $ "/" <> playerURL player

page :: Player -> Board -> Html
page player board = fold
  [ Html ["<!DOCTYPE HTML>"]
  , node "html" [] $ fold
    [ node "head" [] $ fold
      [ inlineNode "title" [] "OXO 9"
      , node "style" [] $ Html
        [ "body {"
        , "  font-size: 2em;"
        , "}"
        , "td {"
        , "  border: 1px solid #333;"
        , "}"
        , "a {"
        , "  -webkit-appearance: button;"
        , "  -moz-appearance: button;"
        , "  appearance: button;"
        , "  text-decoration: none;"
        , "  color: initial;"
        , "}"
        ]
      ]
    , node "body" [] (boardHtml player board)
    ]
  ]
