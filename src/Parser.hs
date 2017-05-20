module Parser where

import Text.Parsec
import HTML
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec.Combinator

type Parser = Parsec String ()

tagIdentifier :: Parser String
tagIdentifier = do
  id <- many1 letter;
  return(id);

startTagOptional :: Parser ()
startTagOptional = do
  text <- many1 (noneOf ">")
  return ()

startTag :: Parser String
startTag = do
  char '<';
  name <- tagIdentifier;
  optional startTagOptional;
  char '>';
  return(name);

endTag :: String -> Parser ()
endTag name = do
  string "</";
  name <- tagIdentifier;
  char '>';
  return ();

htmlTextP :: Parser HtmlElement
htmlTextP = do
  text <- many1 (noneOf "<>")
  return(HtmlText text)


htmlTagP:: Parser HtmlElement
htmlTagP = do
  name <- startTag
  elements <- htmlElementP
  endTag name
  return(HtmlElement name elements)

htmlElementP:: Parser [HtmlElement]
htmlElementP = many (try htmlTagP <|> htmlTextP)

htmlFileP :: Parser [HtmlElement]
htmlFileP = do
  tokens <- htmlElementP;
  eof;
  return(tokens)


parseHtml :: String -> Either ParseError [HtmlElement]
parseHtml htmlContent = parse htmlFileP "stdin" htmlContent
