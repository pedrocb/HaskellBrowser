module HTML where

import Data.List
import Text.Parsec

data HtmlElement
  = HtmlText String
  | HtmlElement String [HtmlElement]

newtype HTML = HTML [HtmlElement]

showElement' :: Int -> HtmlElement -> String
showElement' _ (HtmlText text) = "Text: " ++ text ++ "\n"
showElement' depth (HtmlElement tag childs) = "Tag: " ++ tag ++ "\n" ++ concatMap (showElementWithDepth' (depth+1)) childs

showElementWithDepth' :: Int -> HtmlElement -> String
showElementWithDepth' depth element = prependStars depth ++ showElement' (depth) element

prependStars :: Int -> String
prependStars depth = replicate depth '*'

printTree :: Either ParseError [HtmlElement] -> IO ()
printTree (Right elements) = putStr (concatMap (showElementWithDepth' 1) elements)
printTree (Left err) = putStr (show err)
