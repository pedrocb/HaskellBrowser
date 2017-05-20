module Tokenizer where

import Text.Parsec
import HTML
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec.Combinator

data HtmlToken
  = StartTag String
  | EndTag String
  | Text String

instance Show HtmlToken where
  show (StartTag s) = "<Start:"++s++">"
  show (EndTag s) = "<End:"++s++">"
  show (Text s) = "<Data:"++s++">"

type Parser = Parsec String ()

lexer = P.makeTokenParser emptyDef

angles = P.angles lexer
identifier = P.identifier lexer
symbol = P.symbol lexer


text :: Parser HtmlToken
text = do
  text <- many1 (noneOf "<>")
  return(Text text)

startTagOptional :: Parser ()
startTagOptional = do
  text <- many1 (noneOf ">")
  return ()


startTag :: Parser HtmlToken
startTag = do
  char '<';
  name <- tagIdentifier;
  optional startTagOptional;
  char '>';
  return(StartTag name);

endTag :: Parser HtmlToken
endTag = do
  string "</";
  name <- tagIdentifier;
  char '>';
  return(EndTag name);

tagIdentifier :: Parser String
tagIdentifier = do
  id <- many1 letter;
  return(id);


htmlTokenP :: Parser HtmlToken
htmlTokenP = (try startTag <|> try endTag <|> try text)

htmlFileP :: Parser [HtmlToken]
htmlFileP = do
  tokens <- many htmlTokenP;
  eof;
  return(tokens)

tokenizeHtml :: String -> Either ParseError [HtmlToken]
tokenizeHtml htmlContent = parse htmlFileP "stdin" htmlContent

testTag htmlContent = parse tagIdentifier "stdin" htmlContent
