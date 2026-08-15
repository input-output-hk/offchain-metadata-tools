{-# LANGUAGE OverloadedStrings #-}

import Text.Pandoc.JSON
import Text.Pandoc.Walk

removeTags :: Inline -> Inline
removeTags (Span (_id, classes, _kvs) xs) | "tag" `elem` classes
  = Span nullAttr []
removeTags x
  = x

-- Org links to a headline, e.g. [[* Choosing a UTxO][the next section]], are
-- not resolvable by pandoc's org reader, which emits them as a Span classed
-- "spurious-link" carrying a "target" attribute. The markdown writer then
-- renders that as bracketed-span syntax, [text]{.spurious-link target="..."},
-- which mkdocs does not parse and shows verbatim. Drop the attributes and
-- keep the link text.
unwrapSpuriousLinks :: Inline -> Inline
unwrapSpuriousLinks (Span (_id, classes, _kvs) xs) | "spurious-link" `elem` classes
  = Span nullAttr xs
unwrapSpuriousLinks x
  = x

sanitizeCodeBlocks :: Block -> Block
sanitizeCodeBlocks (CodeBlock (_id, _classes, _kvs) contents)
  = CodeBlock (mempty, mempty, mempty) contents
sanitizeCodeBlocks x
  = x

main :: IO ()
main = toJSONFilter (sanitizeCodeBlocks . walk (unwrapSpuriousLinks . removeTags) :: Block -> Block)
