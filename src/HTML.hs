module HTML where

import Data.List
import Text.Parsec
import Data.Char

type HyperlinkList = [String]

data HtmlElement
  = HtmlText String
  | HtmlElement HtmlTag [HtmlElement]

data HtmlTag
  = Emphasis
  | Bold
  | Paragraph
  | OrderedList
  | UnorderedList
  | ListItem
  | Header Int
  | Root
  | Invisible
  | Hyperlink
  | Undefined


getChild :: HtmlElement -> [HtmlElement]
getChild (HtmlText _) = []
getChild (HtmlElement _ a) = a

instance Eq HtmlTag where
  Emphasis == Emphasis = True
  Paragraph == Paragraph = True
  ListItem == ListItem = True
  Header _ == Header _ = True
  Bold == Bold = True
  _ == _ = False

instance Ord HtmlTag where
  Paragraph `compare` _ = GT
  _ `compare` Paragraph = LT
  (Header n) `compare` _ = GT
  _ `compare` (Header n) = LT
  (Emphasis) `compare` _ = GT
  _ `compare` (Emphasis) = LT
  (Bold) `compare` _ = GT
  _ `compare` (Bold) = LT
  _ `compare` _ = EQ

instance Show HtmlElement where
  show = showTree

instance Show HtmlTag where
  show Emphasis = "em"
  show Paragraph = "p"
  show OrderedList = "ol"
  show UnorderedList = "ul"
  show Hyperlink = "a"
  show ListItem = "li"
  show (Header n) = "h"++ show n
  show Root = "html"
  show Invisible = "invisible"
  show Undefined = "undefined"

toTag :: String -> HtmlTag
toTag "em" = Emphasis
toTag "p" = Paragraph
toTag "ol" = OrderedList
toTag "ul" = UnorderedList
toTag "li" = ListItem
toTag "html" = Root
toTag "b" = Bold
toTag "a" = Hyperlink
toTag ('h':[n])
  | isDigit n && nInt < 7 && nInt > 0 = Header (nInt :: Int)
  | otherwise = Undefined
  where nInt = digitToInt n
toTag tag
  | tag `elem` ["script", "head", "style"]  = Invisible
  | otherwise = Undefined

showElement' :: Int -> HtmlElement -> String
showElement' _ (HtmlText text) = "Text: " ++ text
showElement' depth (HtmlElement tag childs) = "Tag: " ++ show tag ++ concatMap (showElementWithDepth' (depth+1)) childs

showElementWithDepth' :: Int -> HtmlElement -> String
showElementWithDepth' 0 element = showElement' 0 element
showElementWithDepth' depth element = "\n" ++ prependPoints depth ++ showElement' (depth) element

prependPoints :: Int -> String
prependPoints depth = replicate (depth*2) '.'

showTree :: HtmlElement -> String
showTree = showElementWithDepth' 0

getHyperlinks :: [HtmlElement] -> HyperlinkList -> HyperlinkList
getHyperlinks [] list = list
getHyperlinks ((HtmlElement Hyperlink childs):xs) list = case getTextFromElement childs of
  Just text -> getHyperlinks xs (list++[text])
  Nothing -> getHyperlinks xs list
getHyperlinks ((HtmlElement _ childs):xs) list = getHyperlinks xs (getHyperlinks childs list)
getHyperlinks (_:xs) list = getHyperlinks xs list

getTextFromElement :: [HtmlElement] -> Maybe String
getTextFromElement ((HtmlText text):xs) = Just text
getTextFromElement [] = Nothing
getTextFromElement (_:xs) = getTextFromElement xs
