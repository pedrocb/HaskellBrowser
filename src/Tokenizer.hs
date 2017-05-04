module Tokenizer where

import Text.Parsec
import Text.Parsec.String
import HTML
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec.Combinator

data HtmlToken
  = StartTag String
  | EndTag String
  | Text String

instance Show HtmlToken where
  show (StartTag s)= "<Start "++s++">"
  show (EndTag s)= "<End "++s++">"
  show (Text s)= "<Data "++s++">"

lexer = P.makeTokenParser emptyDef

angles = P.angles lexer
identifier = P.identifier lexer
symbol = P.symbol lexer


text :: Parsec String () HtmlToken
text = do
  text <- many1 (noneOf "<")
  return(Text text)


startTag :: Parsec String () HtmlToken
startTag = do
  char '<'
  name <- tagIdentifier
  char '>'
  return(StartTag name)

endTag :: Parsec String () HtmlToken
endTag = do
  string "</"
  name <- tagIdentifier
  char '>'
  return(EndTag name)

tagIdentifier :: Parsec String () String
tagIdentifier = do
  id <- many1 letter
  return(id)

htmlFile :: Parsec String () [HtmlToken]
htmlFile = do
  tags <- many (try startTag <|> try endTag <|> try text)
  return(tags)

tokenizeHtml :: String -> Either ParseError [HtmlToken]
tokenizeHtml htmlContent = parse htmlFile "stdin" htmlContent

testTag htmlContent = parse tagIdentifier "stdin" htmlContent
