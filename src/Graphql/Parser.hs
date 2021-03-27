{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeFamilies #-}

module Graphql.Parser where

import qualified Control.Monad.Combinators.NonEmpty as NE
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Graphql.Parser.Types (Parser)
import Graphql.Types
import Relude hiding (many, some)
import Text.Megaparsec
  ( MonadParsec (label),
    between,
    choice,
    count,
    many,
    manyTill,
    notFollowedBy,
    satisfy,
    sepBy,
    single,
    some,
    try,
  )
import Text.Megaparsec.Char (alphaNumChar, string)
import qualified Text.Megaparsec.Char.Lexer as L
-- import Text.Megaparsec.Debug

document :: Parser Document
document = label "Document" do
  Document <$> NE.some definition

definition :: Parser Definition
definition = label "Definition" do
  choice
    [ DefinitionExecutable <$> executableDefinition,
      DefinitionTypeSystem <$> typeSystemDefinition
    ]

typeSystemDefinition :: Parser TypeSystemDefinition
typeSystemDefinition = label "TypeSystemDefinition" do
  choice
    [ TypeSystemDefinitionSchema <$> schemaDefinition,
      TypeSystemDefinitionType <$> typeDefinition
    ]

schemaDefinition :: Parser SchemaDefinition
schemaDefinition = label "SchemaDefinition" do
  rotds <- keyword "schema" *> braces (NE.some rootOperationTypeDefinition)
  pure $ SchemaDefinition rotds

rootOperationTypeDefinition :: Parser RootOperationTypeDefinition
rootOperationTypeDefinition = label "RootOperationTypeDefinition" do
  ot <- operationType <* symbol ":"
  RootOperationTypeDefinition ot <$> namedType

typeDefinition :: Parser TypeDefinition
typeDefinition = label "TypeDefinition" do
  choice
    [ TypeDefinitionScalar <$> scalarTypeDefinition,
      TypeDefinitionObject <$> objectTypeDefinition,
      TypeDefinitionInterface <$> interfaceTypeDefinition,
      TypeDefinitionUnion <$> unionTypeDefinition,
      TypeDefinitionEnum <$> enumTypeDefinition,
      TypeDefinitionInputObject <$> inputObjectTypeDefinition
    ]

scalarTypeDefinition :: Parser ScalarTypeDefinition
scalarTypeDefinition = label "ScalarTypeDefinition" $ try do
  _stdDescription <- optional description
  keyword "scalar"
  _stdName <- name
  pure ScalarTypeDefinition {_stdDirectives = [], ..}

objectTypeDefinition :: Parser ObjectTypeDefinition
objectTypeDefinition = label "ObjectTypeDefinition" $ try do
  _otdDescription <- optional description
  keyword "type"
  _otdName <- name
  _otdInterfaces <- optional implementsInterfaces
  let _otdDirectives = mempty
  _otdFieldsDefinition <- optional fieldsDefinition
  pure ObjectTypeDefinition {..}

interfaceTypeDefinition :: Parser InterfaceTypeDefinition
interfaceTypeDefinition = label "InterfaceTypeDefinition" $ try do
  _itdDescription <- optional description
  keyword "interface"
  _itdName <- name
  let _itdDirectives = mempty
  _itdFieldsDefinition <- optional fieldsDefinition
  pure InterfaceTypeDefinition {..}

unionTypeDefinition :: Parser UnionTypeDefinition
unionTypeDefinition = label "UnionTypeDefinition" $ try do
  _utdDescription <- optional description
  keyword "union"
  _utdName <- name
  let _utdDirectives = mempty
      pipe = symbol "|"
  _utdMemberTypes <- symbol "=" *> optional pipe *> sepBy namedType pipe
  pure UnionTypeDefinition {..}

enumTypeDefinition :: Parser EnumTypeDefinition
enumTypeDefinition = label "EnumTypeDefinition" $ try do
  _etdDescription <- optional description
  keyword "enum"
  _etdName <- name
  let _etdDirectives = mempty
  _etdValuesDefinition <- optional enumValuesDefinition
  pure EnumTypeDefinition {..}

enumValuesDefinition :: Parser EnumValuesDefinition
enumValuesDefinition = label "EnumValuesDefinition" do
  EnumValuesDefinition <$> braces (some enumValueDefinition)

enumValueDefinition :: Parser EnumValueDefinition
enumValueDefinition = label "EnumValueDefinition" do
  _evdDescription <- optional description
  _evdEnumValue <- name
  let _evdDirectives = mempty
  pure EnumValueDefinition {..}

inputObjectTypeDefinition :: Parser InputObjectTypeDefinition
inputObjectTypeDefinition = label "InputObjectTypeDefinition" $ try do
  _iotdDescription <- optional description
  keyword "input"
  _iotdName <- name
  let _iotdDirectives = mempty
  _iotdFieldsDefinition <- optional inputObjectFieldsDefinition
  pure InputObjectTypeDefinition {..}

inputObjectFieldsDefinition :: Parser InputObjectFieldsDefinition
inputObjectFieldsDefinition = label "InputObjectFieldsDefinition" do
  InputObjectFieldsDefinition <$> braces (some inputValueDefinition)

implementsInterfaces :: Parser (NonEmpty NamedType)
implementsInterfaces = label "ImplementsInterfaces" do
  let ampersand = symbol "&"
  keyword "implements" *> optional ampersand *> NE.sepBy1 namedType ampersand

fieldsDefinition :: Parser FieldsDefinition
fieldsDefinition = label "FieldsDefinition" do
  FieldsDefinition <$> braces (some fieldDefinition)

fieldDefinition :: Parser FieldDefinition
fieldDefinition = label "FieldDefinition" do
  _fdDescription <- optional description
  _fdName <- name
  _fdArgumentsDefinition <- optional argumentsDefinition
  keyword ":"
  _fdType <- typeWithNullability
  let _fdDirectives = mempty
  pure FieldDefinition {..}

typeWithNullability :: Parser TypeWithNullability
typeWithNullability = label "TypeWithNullability" do
  _twnType <- choice [gqlTypeNamed, gqlTypeList]
  _twnNullability <- fromMaybe True <$> optional (symbol "!" $> False)
  pure TypeWithNullability {..}
  where
    gqlTypeNamed :: Parser GqlType
    gqlTypeNamed = label "GqlTypeNamed" do
      GqlTypeNamed <$> namedType

    gqlTypeList :: Parser GqlType
    gqlTypeList = label "GqlTypeList" do
      GqlTypeList <$> brackets (some typeWithNullability)

argumentsDefinition :: Parser ArgumentsDefinition
argumentsDefinition = label "ArgumentsDefinition" do
  ArgumentsDefinition <$> parens (some inputValueDefinition)

inputValueDefinition :: Parser InputValueDefinition
inputValueDefinition = label "InputValueDefinition" do
  _ivdDescription <- optional description
  _ivdName <- name
  keyword ":"
  _ivdType <- typeWithNullability
  _ivdDefaultValue <- optional (symbol "=" *> value)
  let _ivdDirectives = mempty
  pure InputValueDefinition {..}

namedType :: Parser NamedType
namedType = label "NamedType" do NamedType <$> name

description :: Parser Description
description = label "Description" do Description <$> stringValue

executableDefinition :: Parser ExecutableDefinition
executableDefinition = label "ExecutableDefinition" do
  ExecutableDefinition <$> operationDefinition

operationDefinition :: Parser OperationDefinition
operationDefinition = label "OperationDefinition" do
  operationDefinitionSelectionSet <|> operationDefinitionOperation

operationDefinitionSelectionSet :: Parser OperationDefinition
operationDefinitionSelectionSet = label "OperationDefinitionSelectionSet" do
  OperationDefinitionSelectionSet <$> selectionSet

operationDefinitionOperation :: Parser OperationDefinition
operationDefinitionOperation = label "OperationDefinitionOperation" do
  OperationDefinitionOperation <$> operation

operation :: Parser Operation
operation = label "Operation" do
  _operationType <- operationType
  _operationName <- optional name
  _operationSelectionSet <- selectionSet
  let _operationVariableDefs = mempty -- todo
      _operationDirectives = mempty -- todo
  pure Operation {..}

selectionSet :: Parser SelectionSet
selectionSet = label "SelectionSet" do
  SelectionSet <$> braces (many selection)

operationType :: Parser OperationType
operationType = label "OperationType" do
  choice
    [ keyword "query" $> Query,
      keyword "mutation" $> Mutation,
      keyword "subscription" $> Subscription
    ]

selection :: Parser Selection
selection = label "Selection" do
  SelectionField <$> field

field :: Parser Field
field = label "Field" do
  _fieldAlias <- optional alias
  _fieldName <- name
  _fieldArguments <- optional arguments
  let _fieldDirectives = mempty -- todo
  _fieldSelectionSet <- optional selectionSet
  pure Field {..}

arguments :: Parser Arguments
arguments = label "Arguments" do
  Arguments <$> parens (many argument)

argument :: Parser Argument
argument = label "Argument" do
  n <- name <* symbol ":"
  Argument n <$> value

value :: Parser Value
value = label "Value" do
  lexeme $
    choice
      [ valueVariable,
        valueInt,
        valueFloat,
        valueBool,
        valueString,
        valueNull,
        valueEnum,
        valueList,
        valueObject
      ]

valueVariable :: Parser Value
valueVariable = label "ValueVariable" do
  ValueVariable <$> do symbol "$" *> name

valueInt :: Parser Value
valueInt = label "ValueInt" do
  ValueInt <$> L.signed sc L.decimal

valueFloat :: Parser Value
valueFloat = label "ValueFloat" do
  ValueFloat <$> L.signed sc L.float

valueBool :: Parser Value
valueBool = label "ValueBool" do
  ValueBool <$> choice [keyword "true" $> True, keyword "false" $> False]

valueString :: Parser Value
valueString = label "ValueString" do
  ValueString <$> stringValue

stringValue :: Parser Text
stringValue = label "stringValue" do
  choice [stringBlock, stringLiteral] <* sc
  where
    stringLiteral :: Parser Text
    stringLiteral = label "StringLiteral" do
      Text.concat <$> (string "\"" *> manyTill stringCharacter (string "\""))

    stringCharacter :: Parser Text
    stringCharacter = label "Character" do
      choice
        [ Text.singleton <$> satisfy (`elem` allowedChars),
          escapedUnicode,
          escapedCharacter
        ]
      where
        allowedChars =
          '\x0009' :
          '\x0020' :
          '\x0021' :
          ['\x0023' .. '\x005B'] <> ['\x005D' .. '\xFFFF']

    escapedUnicode :: Parser Text
    escapedUnicode = label "EscapedUnicode" $ try do
      bs <- single '\\' <* single 'u'
      c <- Text.singleton . toEnum . mkNum <$> count 4 (satisfy Char.isHexDigit)
      pure $ Text.cons bs c
      where
        mkNum :: [Char] -> Int
        mkNum = foldl' step 0
        step :: Int -> Char -> Int
        step a c = a * 16 + Char.digitToInt c

    escapedCharacter :: Parser Text
    escapedCharacter = label "EscapedCharacter" $ do
      _ <- single '\\'
      choice
        [ string "\"",
          string "\\",
          string "/",
          string "b" $> "\b",
          string "f" $> "\f",
          string "n" $> "\n",
          string "r" $> "\r",
          string "t" $> "\t"
        ]

    stringBlock :: Parser Text
    stringBlock = label "StringBlock" do
      blockStringValue . toText
        <$> (try delim *> manyTill sourceCharacter delim)
      where
        delim = string "\"\"\""

blockStringValue :: Text -> Text
blockStringValue t =
  formatLines $ mapMaybe (unindentLine commonIndent) (zip [1 :: Int ..] lns)
  where
    lns = Text.split (\c -> c == '\n' || c == '\r') t
    isWs = liftA2 (||) (== '\t') (== ' ')
    commonIndent = snd $ foldl' accumCommonIndent (True, Nothing) lns
      where
        accumCommonIndent :: (Bool, Maybe Int) -> Text -> (Bool, Maybe Int)
        accumCommonIndent (True, indent) _ = (False, indent)
        accumCommonIndent (False, indent) l =
          let lineLen = Text.length l
              lineInd = Text.length (Text.takeWhile isWs l)
           in if lineInd < lineLen
                && (isNothing indent || Just lineInd < indent)
                then (False, Just lineInd)
                else (False, indent)
    unindentLine :: Maybe Int -> (Int, Text) -> Maybe Text
    unindentLine mbCommonIndent (index, line) =
      case mbCommonIndent of
        Nothing -> Just line
        -- If the first line contains only whitespace - remove it
        Just _ | index == 1 && Text.all isWs line -> Nothing
        -- Otherwise leave the first line as is
        Just _ | index == 1 -> Just line
        -- If the last line contains only whitespace - remove it
        Just _ | index == length lns && Text.all isWs line -> Nothing
        Just indent -> Just $ Text.drop indent line
    formatLines :: [Text] -> Text
    formatLines = snd . foldl' accum (True, "")
      where
        accum :: (Bool, Text) -> Text -> (Bool, Text)
        accum (isFirstLine, formatted) l =
          if isFirstLine
            then (False, l)
            else (False, Text.snoc formatted '\x000A' <> l)

valueNull :: Parser Value
valueNull = label "ValueNull" do keyword "null" $> ValueNull

valueEnum :: Parser Value
valueEnum = label "ValueEnum" do ValueEnum <$> name

valueList :: Parser Value
valueList = label "ValueList" do ValueList <$> brackets (many value)

valueObject :: Parser Value
valueObject = label "ValueObject" do
  ValueObject . Map.fromList <$> braces (many objectField)
  where
    objectField = (,) <$> (name <* symbol ":") <*> value

-- https://spec.graphql.org/June2018/#sec-Source-Text
sourceCharacter :: Parser Char
sourceCharacter = satisfy (`elem` allowedChars)
  where
    allowedChars = '\x0009' : '\x000A' : '\x000D' : ['\x0020' .. '\xFFFF']

-- https://spec.graphql.org/June2018/#sec-White-Space
whiteSpace :: Parser ()
whiteSpace = label "WhiteSpace" do
  void $
    choice
      [ single '\x0009',
        single '\x000A', -- LF
        single '\x0020', -- Space
        single ','
      ]

sc :: Parser ()
sc = L.space whiteSpace (L.skipLineComment "#") empty

-- https://spec.graphql.org/June2018/#sec-Names
name :: Parser Name
name = label "Name" $ lexeme do
  c0 <- satisfy (`elem` alphas)
  cs <- many $ satisfy (`elem` alphanums)
  pure $ Name $ toText $ c0 : cs
  where
    alphas :: [Char] = "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    alphanums :: [Char] = ['0' .. '9'] <> alphas

alias :: Parser Alias
alias = Alias <$> try (name <* symbol ":")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

keyword :: Text -> Parser ()
keyword kw = void $ lexeme (string kw <* notFollowedBy alphaNumChar)
