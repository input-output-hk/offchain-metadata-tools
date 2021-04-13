{-# LANGUAGE OverloadedStrings #-}

import Text.Pandoc.JSON
import Text.Pandoc.Walk

removeTags :: Inline -> Inline
removeTags (Span (_id, classes, _kvs) xs) | "tag" `elem` classes
  = Span nullAttr []
removeTags x
  = x

sanitizeCodeBlocks :: Block -> Block
sanitizeCodeBlocks (CodeBlock (_id, _classes, _kvs) contents)
  = CodeBlock (mempty, mempty, mempty) contents
sanitizeCodeBlocks x
  = x

main :: IO ()
main = toJSONFilter (sanitizeCodeBlocks . walk removeTags :: Block -> Block)
