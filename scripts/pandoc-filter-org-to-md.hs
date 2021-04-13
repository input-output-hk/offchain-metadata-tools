{-# LANGUAGE OverloadedStrings #-}

import Text.Pandoc.JSON
import Text.Pandoc.Walk

removeTags :: Inline -> Inline
removeTags (Span (_id, classes, _kvs) xs) | "tag" `elem` classes
  = Span nullAttr []
removeTags x
  = x

removeKVsFromCodeBlocks :: Block -> Block
removeKVsFromCodeBlocks (CodeBlock (id, classes, kvs) contents)
  = CodeBlock (id, classes, []) contents
removeKVsFromCodeBlocks x
  = x

removeClassesFromCodeBlocks :: Block -> Block
removeClassesFromCodeBlocks (CodeBlock (id, _classes, kvs) contents)
  = CodeBlock (id, [], kvs) contents
removeClassesFromCodeBlocks x
  = x

main :: IO ()
main = toJSONFilter (removeClassesFromCodeBlocks . removeKVsFromCodeBlocks . walk removeTags :: Block -> Block)
