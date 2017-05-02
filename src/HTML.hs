module HTML where

data HtmlElement
  = HtmlText String
  | HtmlElement String [HtmlElement]

newtype HTML = HTML [HtmlElement]

instance Show HtmlElement where
  show = showHtmlElement

showHtmlElement :: HtmlElement -> String
showHtmlElement (HtmlText s) = "Text " ++ s
showHtmlElement (HtmlElement s _) = "Element " ++ s
