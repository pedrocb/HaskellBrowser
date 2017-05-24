module Printer where

import HTML
import Parser
import Data.List

printRecursive :: HyperlinkList -> [HtmlTag] -> HtmlElement -> String
printRecursive list tags (HtmlText content) = foldl (flip $ toMarkdown) content (sort (nub tags))
printRecursive list tags (HtmlElement Emphasis childs) = concatMap (printRecursive list (Emphasis:tags)) childs
printRecursive list tags (HtmlElement Paragraph xs) = toMarkdown (Paragraph) (concatMap (printRecursive list tags) xs)
printRecursive list tags (HtmlElement (Header n) xs) = concatMap (printRecursive list ((Header n):tags)) xs
printRecursive list tags (HtmlElement Bold childs) = concatMap (printRecursive list (Bold:tags)) childs
printRecursive list tags (HtmlElement OrderedList childs) = printOrderlistChilds list childs tags 0 2
printRecursive list tags (HtmlElement UnorderedList childs) = concatMap (printUnorderedListChild list tags 2) childs
printRecursive list tags (HtmlElement Hyperlink childs) =
  case getTextFromElement childs of
    Just x -> case x `elemIndex` list of
      Just n -> toMarkdown Hyperlink (show n ++ " " ++ x)
      Nothing -> ""
    Nothing -> ""

printRecursive list styles (HtmlElement tag elements) = concatMap (printRecursive list (styles)) elements

printUnorderedListChild :: HyperlinkList -> [HtmlTag] -> Int -> HtmlElement -> String
printUnorderedListChild list tags indent (HtmlElement OrderedList childs)
  = printOrderlistChilds list childs tags 0 (indent+2)
printUnorderedListChild list tags indent (HtmlElement UnorderedList childs)
  = concatMap (printUnorderedListChild list tags (indent+2)) childs
printUnorderedListChild list tags indent (HtmlElement ListItem childs)
  = "\n"++ replicate indent ' ' ++ "-" ++ concatMap (printRecursive list tags) childs ++ "\n"
printUnorderedListChild list tags indent element
  = "\n"++ replicate indent ' ' ++ printRecursive list tags element

-- Atribui ordem aos filhos List Items de uma OrderList
-- Lista de filhos de ol -> Lista de estilos a aplicar -> Numero de itens ja desenhados -> Indentaçâo da lista
printOrderlistChilds :: HyperlinkList -> [HtmlElement] -> [HtmlTag] -> Int -> Int -> String
-- Caso de paragem
printOrderlistChilds _ [] _ _ _ = ""
-- Caso o elemento seja um list item imprimir ordem do item com identançâo correspondente, os filhos e percorrer resto da lista recursivamente
printOrderlistChilds list ((HtmlElement (ListItem) childs):xs) tags n indent
  = "\n"++ replicate indent ' ' ++ show (n+1) ++ ". " ++ concatMap (printRecursive list tags) childs ++ "\n"++ printOrderlistChilds list xs tags (n+1) (indent)
-- Caso seja outra lista ordenada
printOrderlistChilds list ((HtmlElement (OrderedList) childs):xs) tags n indent
  = printOrderlistChilds list childs tags 0 (indent+2) ++ printOrderlistChilds list xs tags n (indent)
printOrderlistChilds list ((HtmlElement (UnorderedList) childs):xs) tags n indent
  = concatMap (printUnorderedListChild list tags (indent+2)) childs ++ printOrderlistChilds list xs tags n (indent)
printOrderlistChilds list (x:xs) tags n indent = printRecursive list tags x ++ printOrderlistChilds list xs tags n (indent)


toMarkdown :: HtmlTag -> String -> String
toMarkdown (Paragraph) content = "\n" ++ content
toMarkdown (Emphasis) content= "_" ++ content ++ "_"
toMarkdown (Bold) content= "**" ++ content ++ "**"
toMarkdown (Header n) content = replicate n '#' ++ content
toMarkdown (Hyperlink) content = " [[" ++ content ++ "]] "
toMarkdown _ _ = ""

printHtml :: [HtmlElement] -> String
printHtml elements = concatMap (printRecursive (getHyperlinks elements []) []) elements
