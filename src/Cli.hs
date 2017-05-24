module Cli where

import Console.Options
import Data.Char
import Data.Monoid
import Data.List
import Parser
import HTML
import System.IO
import Printer
import Text.PrettyPrint (render)
import Control.Monad
import Network.HTTP
import Text.Read

runCli = defaultMain $ do
  fileFlag <- flagParam (FlagShort 'f' <> FlagLong "file") (FlagRequired $ \s -> Right s)
  treeFlag <- flag (FlagShort 't' <> FlagLong "tree")
  stringFlag <- flagParam (FlagShort 's' <> FlagLong "string") (FlagRequired $ \s -> Right s)
  action $ \toParam -> case toParam fileFlag of
                         Just file -> do
                           case toParam treeFlag of
                             True -> printTreeFromFile file
                             False -> do
                               final <- readFromFile file
                               putStrLn final
                         Nothing -> case toParam stringFlag of
                                      Just string -> do
                                        case toParam treeFlag of
                                          True -> printTreeFromString string
                                          False -> do
                                            final <- readFromString string
                                            putStrLn final
                                      Nothing -> askForWebsites []

askForWebsites :: HyperlinkList -> IO ()
askForWebsites links = do
  putStrLn "Enter website (or link): "
  putStr "http://"
  hFlush stdout
  l <- getLine
  putStrLn "#######################################################"
  hyperlinks <- case readMaybe l :: Maybe Int of
    Just n -> do
      hyperlinks <- readFromUrl("http://"++links!!n);
      return(hyperlinks)
    Nothing -> do
      hyperlinks <- readFromUrl("http://"++l)
      return(hyperlinks)
  putStrLn "\n#######################################################"
  askForWebsites hyperlinks

readFromUrl :: String -> IO HyperlinkList
readFromUrl url = do
  rsp <- Network.HTTP.simpleHTTP (getRequest url)
  page <- getResponseBody rsp
  case parseHtml page of
    Right tree -> do
      let hyperlinks = getHyperlinks tree [];
      putStrLn $ printHtml tree
      return(hyperlinks)
    Left error -> do
      putStrLn page
      putStrLn (show error)
      return ([])

readFromString :: String -> IO String
readFromString string = do
  case parseHtml string of
    Right tree -> do
      let hyperlinks = getHyperlinks tree [];
      return(printHtml tree)
    Left error -> do
      return(show error)


readFromFile :: FilePath -> IO String
readFromFile filePath = do
  html <- parseHtmlFromFile filePath
  case html of
    Right tree -> return(printHtml tree)
    Left error -> return(show error)

printTreeFromFile :: FilePath -> IO ()
printTreeFromFile filePath = do
  tree <- parseHtmlFromFile filePath
  case tree of
    Right tree -> do
      putStrLn (concatMap show tree)
    Left error -> do
      putStrLn (show error)

printTreeFromString :: String -> IO ()
printTreeFromString string = do
  case parseHtml string of
    Right tree -> do
      putStrLn (concatMap show tree)
    Left error -> do
      putStrLn (show error)
