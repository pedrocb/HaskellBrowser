module Parser where

import Text.Parsec
import HTML
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

lexer = P.makeTokenParser emptyDef

angles = P.angles lexer
identifier = P.identifier lexer
text = many anyChar

htmlFile :: Parsec String () [HtmlElement]
htmlFile = (do {
               eof <- eof;
               return([])
               })
           <|>(do {
                  id <- angles identifier;
                  next <- htmlFile;
                  return(HtmlElement id []:next)
                  }
              )
           <|> (do {
                   tx <- text;
                     next <- htmlFile;
                     return(HtmlText tx:next)
                   })

parseHTML :: String -> Either ParseError [HtmlElement]
parseHTML string = parse topValue "stdin" string where
  topValue =
    do {
      v <- htmlFile; eof; return v
      }
