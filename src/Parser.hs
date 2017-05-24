module Parser where

import Text.Parsec
import HTML
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec.Combinator

type Parser = Parsec String ()

tagIdentifier :: Parser String
tagIdentifier = do
  id <- many1 (alphaNum);
  return(id);

startTagOptional :: Parser ()
startTagOptional = do
  text <- many1 (noneOf ">")
  return ()

startTag :: Parser String -> Parser String
startTag tag = do
  char '<';
  name <- tag;
  optional startTagOptional;
  char '>';
  spaces
  return(name);

endTag :: String -> Parser ()
endTag name = do
  string "</";
  tag <- string name;
  char '>';
  spaces
  return ();

htmlTextP :: Parser HtmlElement
htmlTextP = do
  text <- many1 (noneOf "<>")
  return(HtmlText $ map (\c -> case c of
                            '\n' -> ' '
                            otherwise -> c) text)


htmlTagP :: Parser String -> Parser HtmlElement
htmlTagP parser = do
  name <- startTag parser
  elements <- htmlElementP
  endTag name
  return(HtmlElement (toTag name) elements)

htmlElementP:: Parser [HtmlElement]
htmlElementP = many (try $ htmlTagP tagIdentifier <|> htmlTextP)

htmlFileP :: Parser [HtmlElement]
htmlFileP = do
  spaces
  optional $ try $ startTag (try $ string "!DOCTYPE")
  htmlTags <- htmlTagP $ tagIdentifier
  eof
  return([htmlTags])

parseHtml :: String -> Either ParseError [HtmlElement]
parseHtml htmlContent = parse htmlFileP "stdin" htmlContent

parseHtmlFromFile :: FilePath -> IO (Either ParseError [HtmlElement])
parseHtmlFromFile filePath
  = do { contents <- readFile filePath;
         return(parse htmlFileP filePath contents)
       }
