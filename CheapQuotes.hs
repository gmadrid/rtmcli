{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module CheapQuotes (CheapQuotes.words) where

import ClassyPrelude as CP

-- Analogous to words from the Prelude, but knows about quotes,
-- and won't break a word inside quotes.
words :: Text -> [Text]
words = CP.words

{-

Using < and > to delimit strings in the example to avoid the need to
escape quotes in the input examples. < and > have no special meaning.

Examples:
<foo bar baz>   -> ["foo", "bar", "baz"]
<"foo bar" baz> -> ["foo bar", "baz"]

Coming later:
<fo\"o bar baz> -> ["fo\"o", "bar", "baz"]  -- Escaped quotes
<fo"o b"ar baz> -> ["foo bar", "baz"]       -- Quotes in middle of words
<'foo bar' baz> -> ["foo bar", "baz"]       -- Single quotes

-}


