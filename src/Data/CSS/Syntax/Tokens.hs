{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}

module Data.CSS.Syntax.Tokens
    ( Token(..)
    , NumericValue(..)
    , HashFlag(..)
    , Unit

    , tokenize
    , serialize
    ) where


import           Control.Applicative
import           Control.Monad

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Text.Parser.LookAhead
import           Text.PrettyPrint.ANSI.Leijen (displayS, renderCompact)
import           Text.Trifecta hiding (parseString)
import           Data.Monoid
import           Data.Char
import           Data.Scientific
import           Numeric

import           Prelude

data Token l
    = Whitespace l

    | CDO l -- CommentDelimiterOpen
    | CDC l -- CommentDelimiterClose

    | Comma l
    | Colon l
    | Semicolon l

    | LeftParen l
    | RightParen l
    | LeftSquareBracket l
    | RightSquareBracket l
    | LeftCurlyBracket l
    | RightCurlyBracket l

    | SuffixMatch l
    | SubstringMatch l
    | PrefixMatch l
    | DashMatch l
    | IncludeMatch l

    | Column l

    | String l !Char !Text
    | BadString l !Char !Text

    | Number l !Text !NumericValue
    | Percentage l !Text !NumericValue
    | Dimension l !Text !NumericValue !Unit

    | Url l !Text
    | BadUrl l !Text

    | Ident l !Text

    | AtKeyword l !Text

    | Function l !Text

    | Hash l !HashFlag !Text

    | Delim l !Char

    | UserComment l !Text
    deriving (Show, Eq, Functor)


data NumericValue
    = NVInteger !Scientific
    | NVNumber !Scientific
    deriving (Show, Eq)

data HashFlag = HId | HUnrestricted
    deriving (Show, Eq)

type Unit = Text

type SrcToken = Token Span

-- Tokenization
-------------------------------------------------------------------------------


-- | Parse a 'Text' into a list of 'Token's.
--
-- https://drafts.csswg.org/css-syntax/#tokenization

tokenize :: Text -> Either String [SrcToken]
tokenize t = case result of
  Success r -> Right r
  Failure d -> Left $ displayS (renderCompact d) ""
  where
    result = parseByteString (many parseToken) mempty . T.encodeUtf8 $ preprocessInputStream t



-- | Before sending the input stream to the tokenizer, implementations must
-- make the following code point substitutions: (see spec)
--
-- https://drafts.csswg.org/css-syntax/#input-preprocessing

preprocessInputStream :: Text -> Text
preprocessInputStream = T.pack . f . T.unpack
  where
    f []                    = []

    f ('\x000D':'\x000A':r) = '\x000A' : f r

    f ('\x000D':r)          = '\x000A' : f r
    f ('\x000C':r)          = '\x000A' : f r

    f ('\x0000':r)          = '\xFFFD' : f r

    f (x:r)                 = x : f r



-- Serialization
-------------------------------------------------------------------------------


-- | Serialize a list of 'Token's back into 'Text'. Round-tripping is not
-- guaranteed to be identity. The tokenization step drops some information
-- from the source.
--
-- https://drafts.csswg.org/css-syntax/#serialization

serialize :: [SrcToken] -> Text
serialize = mconcat . map renderToken


renderToken :: SrcToken -> Text
renderToken (Whitespace _)         = " "

renderToken (CDO _)                = "<!--"
renderToken (CDC _)                = "-->"

renderToken (Comma _)              = ","
renderToken (Colon _)              = ":"
renderToken (Semicolon _)          = ";"

renderToken (LeftParen _)          = "("
renderToken (RightParen _)         = ")"
renderToken (LeftSquareBracket _)  = "["
renderToken (RightSquareBracket _) = "]"
renderToken (LeftCurlyBracket _)   = "{"
renderToken (RightCurlyBracket _)  = "}"

renderToken (SuffixMatch _)        = "$="
renderToken (SubstringMatch _)     = "*="
renderToken (PrefixMatch _)        = "^="
renderToken (DashMatch _)          = "|="
renderToken (IncludeMatch _)       = "~="

renderToken (Column _)             = "||"

renderToken (String _ d x)         = T.singleton d <> renderString x <> T.singleton d
renderToken (BadString _ d x)      = T.singleton d <> renderString x <> T.singleton d

renderToken (Number _ x _)         = x
renderToken (Percentage _ x _)     = x <> "%"
renderToken (Dimension _ x _ u)    = x <> u

renderToken (Url _ x)              = "url(" <> x <> ")"
renderToken (BadUrl _ x)           = "url(" <> x <> ")"

renderToken (Ident _ x)            = x

renderToken (AtKeyword _ x)        = "@" <> x

renderToken (Function _ x)         = x <> "("

renderToken (Hash _ _ x)           = "#" <> x

renderToken (Delim _ x)            = T.singleton x

renderToken (UserComment _ x)      = "/*" <> x <> "*/"

renderString :: Text -> Text
renderString = T.pack . concatMap f . T.unpack
  where
    nonPrintableCodePoint c
        | c >= '\x0000' && c <= '\x0008' = True -- NULL through BACKSPACE
        | c == '\x000B'                  = True -- LINE TABULATION
        | c >= '\x000E' && c <= '\x001F' = True -- SHIFT OUT through INFORMATION SEPARATOR ONE
        | c == '\x007F'                  = True -- DELETE
        | otherwise                      = False

    nonASCIICodePoint c = c >= '\x0080' -- control

    f c = if nonPrintableCodePoint c || nonASCIICodePoint c
        then "\\" <> showHex (ord c) ""
        else [c]


parseComment :: Parser SrcToken
parseComment = do
  (commentText :~ pos) <- spanned $ do
    void $ text "/*"
    manyTill anyChar (text "*/")
  return $ UserComment pos $ T.pack commentText

parseWhitespace :: Parser SrcToken
parseWhitespace = do
  Whitespace <$> spanning (skipSome (satisfy isWhitespace))

parseChar :: Char -> Parser Span
parseChar c = spanning $ char c

parseStr :: Text -> Parser Span
parseStr str = spanning $ text str

nextInputCodePoint :: Parser Char
nextInputCodePoint = escapedCodePoint' <|> anyChar


whenNext :: Char -> a -> Parser a
whenNext c a = do
    mbChar <- peekChar
    if mbChar == Just c
        then return a
        else fail "whenNext"

-- 4.3.4. Consume a string token
parseString :: Char -> Parser SrcToken
parseString endingCodePoint = do
    _ <- char endingCodePoint
    (f :~ s) <- spanned $ go mempty
    return $ f s

  where
    go acc = choice
        [ (eof <|> void (char endingCodePoint)) *> return (\l -> String l endingCodePoint acc)
        , string "\\\n" *> go acc
        , whenNext '\n' (\l -> BadString l endingCodePoint acc)
        , nextInputCodePoint >>= \ch -> go (acc <> T.singleton ch)
        ]


parseHash :: Parser SrcToken
parseHash = do
  (name :~ s) <- spanned (char '#' *> parseName)
  return $ Hash s HId name

isNameStartCodePoint :: Char -> Bool
isNameStartCodePoint c = isLetter c || c >= '\x0080' || c == '_'

isNameCodePoint :: Char -> Bool
isNameCodePoint c = isNameStartCodePoint c || isDigit c || c == '-'

parseNumeric :: Parser SrcToken
parseNumeric = do
  f :~ s <- spanned $ do
    (repr, nv) <- parseNumericValue
    dimNum repr nv <|> pctNum repr nv <|> return (\l -> Number l repr nv)
  return $ f s
  where
    dimNum repr nv = do
        unit <- parseName
        return $ \l -> Dimension l repr nv unit
    pctNum repr nv = do
        _ <- char '%'
        return $ \l -> Percentage l repr nv

nameCodePoint :: Parser Char
nameCodePoint = satisfy isNameCodePoint

escapedCodePoint' :: Parser Char
escapedCodePoint' = do
  _ <- char '\\'
  (eof *> pure '\xFFFD') <|> do
    t <- go [] (0 :: Int)
    skipOptional $ choice [ void $ char ' '
                          , void $ char '\t'
                          , void $ char '\f'
                          , void $ text "\r\n"
                          , void $ char '\n'
                          , void $ char '\r'
                          ]
    case unhex t of
      Nothing -> fail $ "escapedCodePoint: unable to parse hex " ++ t
      Just cp -> return $ if cp == 0 || cp > 0x10FFFF
                   then chr 0xFFFD
                   else chr cp
  where
    -- todo deal with ungetting last char if it's not hex char
    go acc i = if (i < 6)
      then do
        mch <- peekChar
        case mch of
          Just ch -> if isHexChar ch
            then do
              void anyChar
              (ch :) <$> go acc (succ i)
            else pure acc
          Nothing -> pure acc
      else pure acc

parseName :: Parser Text
parseName = try $ do
    chars <- some $
        nameCodePoint <|> escapedCodePoint'

    case chars of
        '-':xs -> case xs of
            _:_ -> return $ T.pack chars
            _ -> fail "parseName: Not a valid name start"
        _ -> return $ T.pack chars


parseSign :: Parser (Text, Int)
parseSign = do
    mbChar <- peekChar
    case mbChar of
        Just '+' -> anyChar >> return ("+", 1)
        Just '-' -> anyChar >> return ("-", (-1))
        _        -> return ("", 1)

parseNumericValue :: Parser (Text, NumericValue)
parseNumericValue = try $ do
    -- Sign
    (sS, s) <- parseSign

    -- Digits before the decimal dot. They are optional (".1em").
    (iS, i) <- do
        digits <- many (satisfy isDigit)
        return $ if (null digits)
            then ("", 0)
            else (T.pack digits, read digits)

    -- Decimal dot and digits after it. If the decimal dot is there then it
    -- MUST be followed by one or more digits. This is not allowed: "1.".
    (fS, f, fB) <- option ("", 0, False) $ do
        _ <- char '.'
        digits <- some (satisfy isDigit)
        return ("." <> T.pack digits, read digits, True)

    -- Exponent (with optional sign).
    (tS, t, eS, e, eB) <- option ("", 1, "", 0, False) $ do
        e <- char 'E' <|> char 'e'
        (tS, t) <- parseSign
        eS <- T.pack <$> some (satisfy isDigit)

        return (T.singleton e <> tS, t, eS, read $ T.unpack eS, True)

    let repr = sS <> iS <> fS <>tS <> eS
    if T.null repr || repr == "-" || repr == "+" || T.head repr == 'e' || T.head repr == 'E'
        then fail "parseNumericValue: no parse"
        else do
            let v = fromIntegral s * (i + f*10^^(-(T.length fS - 1))) * 10^^(t*e)
            return $ if fB || eB
                then (repr, NVNumber v)
                else (repr, NVInteger v)


parseUrl :: Parser (Span -> SrcToken)
parseUrl = do
  skipMany (satisfy isWhitespace)
  go mempty

  where
    endOfUrl acc = (eof <|> void (char ')')) *> return (\l -> Url l acc)

    go acc = choice
        [ endOfUrl acc
        , (char '"' <|> char '\'' <|> char '(') >>= \ch -> badUrl (acc <> T.singleton ch)
        , string "\\\n" *> badUrl (acc <> "\\\n")
        , some (satisfy isWhitespace) >>= \c -> (endOfUrl acc <|> badUrl (acc <> T.pack c))
        , nextInputCodePoint >>= \ch -> go (acc <> T.singleton ch)
        ]

    badUrl acc = choice
        [ (eof <|> void (char ')')) *> return (\l -> BadUrl l acc)
        , nextInputCodePoint >>= \ch -> badUrl (acc <> T.singleton ch)
        ]


parseIdentLike :: Parser SrcToken
parseIdentLike = do
  f :~ s <- spanned $ do
    name <- parseName
    choice
      [ do
          -- Special handling of url() functions (they are not really
          -- functions, they have their own Token type).
          guard $ T.isPrefixOf "url" (T.map toLower name)

          void $ char '('
          skipMany (satisfy isWhitespace)

          whenNext '"' (\l -> Function l name) <|> whenNext '\'' (\l -> Function l name) <|> parseUrl

      , char '(' *> return (\l -> Function l name)
      , return (\l -> Ident l name)
      ]
  return $ f s


parseEscapedIdentLike :: Parser SrcToken
parseEscapedIdentLike = do
    mbChar <- peekChar
    case mbChar of
        Just '\\' -> parseIdentLike <|> (spanning anyChar >>= \l -> return $ Delim l '\\')
        _         -> fail "parseEscapedIdentLike: Does not start with an escape code"

parseAtKeyword :: Parser SrcToken
parseAtKeyword = do
    (name :~ s) <- spanned (char '@' *> parseName)
    return $ AtKeyword s name


parseToken :: Parser SrcToken
parseToken = choice
    [ parseWhitespace

    , CDO <$> spanning (text "<!--")
    , CDC <$> spanning (text "-->")

    , Comma <$> parseChar ','
    , Colon <$> parseChar ':'
    , Semicolon <$> parseChar ';'
    , LeftParen <$> parseChar '('
    , RightParen <$> parseChar ')'
    , LeftSquareBracket <$> parseChar '['
    , RightSquareBracket <$> parseChar ']'
    , LeftCurlyBracket <$> parseChar '{'
    , RightCurlyBracket <$> parseChar '}'

    , SuffixMatch <$> parseStr "$="
    , SubstringMatch <$> parseStr "*="
    , PrefixMatch <$> parseStr "^="
    , DashMatch <$> parseStr "|="
    , IncludeMatch <$> parseStr "~="

    , Column <$> parseStr "||"

    , parseNumeric
    , parseIdentLike

    , parseEscapedIdentLike
    , parseHash

    , parseString '"'
    , parseString '\''

    , parseAtKeyword
    , parseComment
    , spanned anyChar >>= \(c :~ s) -> return $ Delim s c
    ] <?> "token"



isWhitespace :: Char -> Bool
isWhitespace '\x0009' = True
isWhitespace '\x000A' = True
isWhitespace '\x0020' = True
isWhitespace _        = False


isHexChar :: Char -> Bool
isHexChar ch
    | ch >= '0' && ch <= '9' = True
    | ch >= 'A' && ch <= 'F' = True
    | ch >= 'a' && ch <= 'f' = True
    | otherwise              = False


unhex :: (Functor m, Monad m) => String -> m Int
unhex = fmap toInt . go []
  where

    go :: Monad m => [Int] -> String -> m [Int]
    go acc []    = return acc
    go acc (a:r) = do
        x <- c a
        go (x:acc) r

    toInt = sum . map (\(e, x) -> 16 ^ e * x) . zip [(0::Int)..]

    c :: Monad m => Char -> m Int
    c '0' = return 0
    c '1' = return 1
    c '2' = return 2
    c '3' = return 3
    c '4' = return 4
    c '5' = return 5
    c '6' = return 6
    c '7' = return 7
    c '8' = return 8
    c '9' = return 9
    c 'A' = return 10
    c 'B' = return 11
    c 'C' = return 12
    c 'D' = return 13
    c 'E' = return 14
    c 'F' = return 15
    c 'a' = return 10
    c 'b' = return 11
    c 'c' = return 12
    c 'd' = return 13
    c 'e' = return 14
    c 'f' = return 15
    c _   = fail "Invalid hex digit!"

peekChar :: (CharParsing m, LookAheadParsing m) => m (Maybe Char)
peekChar = lookAhead ((Just <$> anyChar) <|> (eof *> pure Nothing))

