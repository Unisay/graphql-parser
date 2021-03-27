module Graphql.Client.Parser.Types where

import Relude
import Text.Megaparsec (ParsecT)

type Parser = ParsecT Void Text Identity
