module HTML where

data HtmlElement
  = HtmlText String
  | HtmlElement String [HtmlElement]

newtype HTML = HTML [HtmlElement]
