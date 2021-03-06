This package provides functions to parse a CSS file into a stream of tokens
and from that into rules or declarations. The exact algorithm is defined
in the [css-syntax] module (it is the same algorithm that Blink uses since mid
2015 when it replaced their old Bison based parser).

Note: Only the tokenizer is currently implemented. Parsing the token stream
into rules or declarations is available as of yet.


### Motivation

I needed a library which would allow me to process a CSS file, modify all
image URLs and write the result to disk. Existing CSS libraries for Haskell
either focus only on generating CSS (through a DLS) or parse the source into
a format that is too high-level and makes this kind of processing difficult.

A token stream is just the right level of abstraction for that task. The spec
defines `<url-token>` and `<function-token>`, which is what I needed to
extract the image URLs from the source.

More advanced processing likely requires a higher level abstraction than the
token stream provides. E.g. to expand vendor prefixes you'll need to parse
the token stream into a list of rules and declarations, so you can pick the
declarations you want to process.


### Tokenizer

The tokenizer uses Attoparsec to convert the input to a list of tokens. This
process removes all comments and collapses consecutive whitespace into a single
space character (U+0020). There may be other occasions where the tokenizer
looses information from the input stream.


### Example

In the following example I replace all URLs in the source CSS file with
links to predefined image.

```haskell
import qualified Data.Text.IO as T
import           Data.CSS.Syntax.Tokens (tokenize, serialize)

main :: IO ()
main = do
    source <- T.readFile "path-to-your.css"
    let Right tokens = tokenize source

    putStrLn $ "The CSS file has " ++ show (length tokens)
        ++ " tokens"

    let newTokens = map f tokens

    T.writeFile "path-to-lolcatted.css" $
        serialize newTokens

f :: Token -> Token
f (Url _) = Url "http://lol.cat/img.png"
f x       = x

```


[css-syntax]: https://drafts.csswg.org/css-syntax
