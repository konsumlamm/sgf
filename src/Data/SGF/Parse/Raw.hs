-- boilerplate {{{
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module Data.SGF.Parse.Raw (
    collection,
    Property(..),
    SGFParser,
    enum
) where

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Data.Char
import Data.Tree
import Data.Word
import Prelude hiding (lex)
import Text.Parsec (SourcePos, incSourceColumn)
import Text.Parsec.Prim
import Text.Parsec.Combinator

type SGFParser a = Parsec [Word8] () a

-- }}}
data Property = Property {
    position :: SourcePos, -- ^
                           -- Currently, this is pretty lame: it doesn't track
                           -- line number and character number, only byte
                           -- offset from the beginning of the file.  This is
                           -- because I don't really understand how to
                           -- correctly track line number and character number
                           -- properly in the face of dynamically changing
                           -- encodings, whereas byte number is a totally
                           -- braindead statistic to track.
    name     :: String,    -- ^
                           -- The literal name of the property.  This is
                           -- guaranteed to be a non-empty string of
                           -- upper-case ASCII characters.
    values   :: [[Word8]]  -- ^ The arguments to the property.
} deriving (Eq, Ord, Show)

-- |
-- Handy way to convert known-ASCII characters from 'Word8' to 'Char', among other
-- things.
enum :: (Enum a, Enum b) => a -> b
enum = toEnum . fromEnum
ensure :: (Monad m, Alternative m) => (b -> Bool) -> b -> m b
ensure p x = guard (p x) >> return x

satisfy :: (Word8 -> Bool) -> SGFParser Word8
satisfy p = tokenPrim
    ((\x -> ['\'', x, '\'']) . enum)
    (\pos _ _ -> incSourceColumn pos 1)
    (ensure p)
satisfyChar :: Enum b => (b -> Bool) -> SGFParser Word8
satisfyChar = satisfy . (. enum)

anyWord :: SGFParser Word8
anyWord     = satisfy (const True)
exactWord :: Char -> SGFParser Word8
exactWord   = satisfy . (==) . enum
noWord :: [Char] -> SGFParser Word8
noWord      = satisfy . flip notElem . map enum

whitespace :: SGFParser [Word8]
whitespace  = many (satisfyChar isSpace)

-- assumed: the current byte is literally ASCII '\\' iff the current byte is
-- the last byte of the encoding of the actual character '\\' and neither of
-- the bytes that are literally ASCII ']' and ASCII ':' occur after the first
-- byte of any multi-byte encoded character
-- (in particular, UTF-8, ASCII, and ISO 8859-1 satisfy this property)
escapedChar :: SGFParser [Word8]
escapedChar             = liftM2 (\x y -> [x, y]) (exactWord '\\') anyWord
unescapedExcept :: [Char] -> SGFParser [Word8]
unescapedExcept      ws = fmap return (noWord ws)
literalTextExcept :: [Char] -> SGFParser [Word8]
literalTextExcept    ws = fmap concat $ many (escapedChar <|> unescapedExcept ws)

property :: SGFParser Property
property = liftM3 ((. map enum) . Property)
    (getPosition)
    (many1 (satisfyChar (liftM2 (&&) isUpper (< '\128'))))
    (sepEndBy1 (exactWord '[' >> literalTextExcept "]" <* exactWord ']') whitespace)

node :: SGFParser [Property]
node = do
    _ <- exactWord ';'
    _ <- whitespace
    sepEndBy property whitespace

gameTree :: SGFParser (Tree [Property])
gameTree = do
    _ <- exactWord '('
    _ <- whitespace
    (node':nodes) <- sepEndBy1 node     whitespace
    trees        <- sepEndBy  gameTree whitespace
    _ <- exactWord ')'
    return (Node node' (foldr ((return .) . Node) trees nodes))

-- |
-- Parse the tree-structure of an SGF file, but without any knowledge of the
-- semantics of the properties, etc.
collection :: SGFParser [Tree [Property]]
collection = whitespace >> sepEndBy1 gameTree whitespace <* whitespace <* eof
