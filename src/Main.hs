{-# LANGUAGE OverloadedStrings #-}

module Main where

-- base
import           Data.Monoid (mconcat)
import           Control.Concurrent.MVar
import Control.Monad.IO.Class

-- scotty
import           Web.Scotty  (get, html, param, scotty)

-- text
import           Data.Text.Lazy   (Text)
import qualified Data.Text.Lazy   as T

import Board

main :: IO ()
main = do
  board <- newMVar startBoard
  scotty 3000 $ do
    get "/reset" $
      liftIO $ putMVar board startBoard

    get "/:player/:x:y:mx:my" $ do
      player <- param "player"
      x <- param "x"
      y <- param "y"
      mx <- param "mx"
      my <- param "my"

      liftIO $ modifyMVar_ board
        (pure . play player (x, y) (mx, my))

      newBoard <- liftIO $ readMVar board

      html $ page player newBoard

page :: Player -> Board -> Text
page player board = T.unlines
  [ "<!DOCTYPE HTML>"
  , "<html>"
  , "  <head>"
  , "    <title>OXO 9</title>"
  , "    <style>"
  , "      body {"
  , "        font-size: 2em;"
  , "      }"
  , "      td {"
  , "          border: 1px solid #333;"
  , "      }"
  , "      a {"
  , "        -webkit-appearance: button;"
  , "        -moz-appearance: button;"
  , "        appearance: button;"
  , "        text-decoration: none;"
  , "        color: initial;"
  , "      }"
  , "    </style>"
  , "  </head>"
  , "  <body>"
  , render player board
  , "  </body>"
  , "</html>"
  ]
