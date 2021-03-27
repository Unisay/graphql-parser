module Test.Parser where

import qualified Graphql.Client.Parser as Parser
import qualified Graphql.Client.Printer as Printer
import Relude
import Test.Tasty (TestTree, testGroup)
import Text.Megaparsec (eof, errorBundlePretty, runParser)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import System.FilePath (replaceExtension, takeBaseName)
import Text.Pretty.Simple (pShowNoColor)
import qualified Data.Text.IO as TIO 

test_parser :: IO TestTree
test_parser =  do
  gqlFiles <- findByExtension [".graphql"]  "test/golden/parser"
  pure $ testGroup "Parse GraphQL executable expressions" do
    gqlFile <- gqlFiles
    let goldenPath :: FilePath = replaceExtension gqlFile ".golden.txt"
    return
      $ goldenVsString (takeBaseName gqlFile) goldenPath
      $ TIO.readFile gqlFile <&> 
        encodeUtf8 @_ @LByteString
        . pShowNoColor
        . first errorBundlePretty
        . runParser Parser.document gqlFile 

test_roundtrip :: IO TestTree
test_roundtrip =  do
  gqlFiles <- findByExtension [".graphql"]  "test/golden/parser"
  pure $ testGroup "Parse GraphQL executable expressions" do
    gqlFile <- gqlFiles
    let goldenPath :: FilePath = replaceExtension gqlFile ".printed.txt"
    return
      $ goldenVsString (takeBaseName gqlFile) goldenPath
      $ TIO.readFile gqlFile <&> 
        encodeUtf8 @_ @LByteString
        . either identity identity
        . bimap errorBundlePretty (toString . Printer.printDocument)
        . runParser (Parser.document <* eof) gqlFile 