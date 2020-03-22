{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Html where

-- base
import           Data.Foldable

-- text
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

newtype Html = Html [Text] deriving (Semigroup, Monoid)
type Attribute = (Text, Text)
type Attributes = [Attribute]
type Pos = (Int, Int)

url :: Text -> Attributes -> Text
url path attrs = path <> args
  where
    args = fold $
      zipWith
        (\c (k, v) -> c <> k <> "=" <> v)
        ("?" : repeat "&")
        attrs

renderHtml :: Html -> Text
renderHtml (Html inner) = T.unlines inner

renderAttributes :: Attributes -> Text
renderAttributes [] = ""
renderAttributes ((k,v):attrs) =
  " " <> k <> "=\"" <> v <> "\"" <> renderAttributes attrs

node :: Text -> Attributes -> Html -> Html
node tag attrs (Html inner) =
  Html $
    ("<" <> tag <> renderAttributes attrs <> ">") :
    map ("  " <>) inner ++
    [ "</" <> tag <> ">" ]

inlineNode :: Text -> Attributes -> Text -> Html
inlineNode tag attrs inner =
  Html [ "<" <> tag <> renderAttributes attrs <> ">" <> inner <> "</" <> tag <> ">" ]

p :: Text -> Html
p = inlineNode "p" []

a :: Attributes -> Text -> Html
a = inlineNode "a"

table :: Attributes -> (Pos -> a -> Html) -> [[a]] -> Html
table tdAttrs f rows =
  node "table" [] $ fold
    [ node "tr" [] $ fold
      [ node "td" tdAttrs $ f (x, y) e
      | (x, e) <- zip [0..] row
      ]
    | (y, row) <- zip [0..] rows
    ]
