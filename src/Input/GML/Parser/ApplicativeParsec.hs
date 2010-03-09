-- file: ch16/ApplicativeParsec.hs
module Input.GML.Parser.ApplicativeParsec
    (
      module Control.Applicative
    , module Text.ParserCombinators.Parsec
    ) where

{-Extension ripped from the Real World Haskell book.
  Only works with parsec 2.1, newer vesrion uses a different parsing system.
-}

import Control.Applicative
import Control.Monad (MonadPlus(..), ap)

-- Hide a few names that are provided by Applicative.
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

-- The Applicative instance for every Monad looks like this.
instance Applicative (GenParser s a) where
    pure  = return
    (<*>) = ap

-- The Alternative instance for every MonadPlus looks like this.
instance Alternative (GenParser s a) where
    empty = mzero
    (<|>) = mplus
