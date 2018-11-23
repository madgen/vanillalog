module Language.Vanillalog.Generic.Error
  ( Error(..)
  , Severity(..)
  ) where

import Protolude hiding ((<>))

import Text.PrettyPrint

import Language.Vanillalog.Generic.Pretty (Pretty(..))
import Language.Vanillalog.Generic.Parser.SrcLoc (SrcSpan)

data Severity =
    Impossible -- |Error that should never be thrown
  | User       -- |Standard user error
  | Warning    -- |Warning
  deriving (Eq)

data Error = Error Severity (Maybe SrcSpan) Text

instance Pretty Severity where
  pretty Impossible = "Impossible happened! Please submit a bug report"
  pretty User       = "Error"
  pretty Warning    = "Warning"

instance Pretty Error where
  pretty (Error severity mSpan msg) =
    brackets (pretty severity) <> colon $+$
      nest 2 (pretty mSpan $+$ pretty msg)
