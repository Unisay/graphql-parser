module Graphql.Client.Printer where

import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import Graphql.Client.Types
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Relude

printDocument :: Document -> Text
printDocument = printDoc . documentDoc

printOperationDefinition :: OperationDefinition -> Text
printOperationDefinition = printDoc . operationDefinitionDoc

printDoc :: Doc ann -> Text
printDoc = renderStrict . layoutPretty defaultLayoutOptions

documentDoc :: Document -> Doc Document
documentDoc (Document definitions) =
  vsep $
    NEL.toList definitions
      & mapMaybe \case
        DefinitionExecutable ed ->
          Just . unAnnotate . operationDefinitionDoc $
            unwrapExecutableDefinition ed
        DefinitionTypeSystem _ -> Nothing -- TODO: type definitions

operationDefinitionDoc :: OperationDefinition -> Doc OperationDefinition
operationDefinitionDoc = \case
  OperationDefinitionOperation op -> unAnnotate $ operationDoc op
  OperationDefinitionSelectionSet ss -> unAnnotate $ selectionSetDoc ss

operationDoc :: Operation -> Doc Operation
operationDoc Operation {..} =
  unAnnotate (operationTypeDoc _operationType)
    <+> unAnnotate (selectionSetDoc _operationSelectionSet)

operationTypeDoc :: OperationType -> Doc OperationType
operationTypeDoc = \case
  Query -> "query"
  Mutation -> "mutation"
  Subscription -> "subscription"

selectionSetDoc :: SelectionSet -> Doc SelectionSet
selectionSetDoc (SelectionSet selections) =
  encloseSep "{ " " }" space selectionsDoc
  where
    selectionsDoc :: [Doc ann]
    selectionsDoc = unAnnotate . selectionDoc <$> selections

selectionDoc :: Selection -> Doc Selection
selectionDoc (SelectionField field) = unAnnotate (fieldDoc field)

fieldDoc :: Field -> Doc Field
fieldDoc Field {..} =
  withAliasPrefix name
    <> sep
      ( maybe [] (pure . unAnnotate . argumentsDoc) _fieldArguments
          ++ maybe [] (pure . unAnnotate . selectionSetDoc) _fieldSelectionSet
      )
  where
    name :: Doc Name
    name = nameDoc _fieldName
    withAliasPrefix :: Doc Name -> Doc Field
    withAliasPrefix (unAnnotate -> docName) =
      case _fieldAlias of
        Nothing -> docName
        Just alias ->
          unAnnotate (fieldAliasDoc alias)
            & (<> colon <+> docName)

argumentsDoc :: Arguments -> Doc Arguments
argumentsDoc (Arguments arguments) =
  tupled $ unAnnotate . argumentDoc <$> arguments

argumentDoc :: Argument -> Doc Argument
argumentDoc (Argument name value) =
  unAnnotate (nameDoc name) <> colon <> space <> unAnnotate (valueDoc value)

valueDoc :: Value -> Doc Value
valueDoc = \case
  ValueVariable name -> "$" <> unAnnotate (nameDoc name)
  ValueInt int -> pretty int
  ValueFloat float -> pretty float
  ValueString text -> pretty text
  ValueBool b -> pretty b
  ValueNull -> "null"
  ValueEnum name -> unAnnotate (nameDoc name)
  ValueList values -> list $ valueDoc <$> values
  ValueObject nameValues ->
    encloseSep "{ " " }" space $
      uncurry objectFieldDoc
        <$> Map.toList nameValues

objectFieldDoc :: Name -> Value -> Doc Value
objectFieldDoc name value =
  unAnnotate (nameDoc name) <> colon <> space <> valueDoc value

fieldAliasDoc :: Alias -> Doc Alias
fieldAliasDoc (Alias name) = unAnnotate (nameDoc name)

nameDoc :: Name -> Doc Name
nameDoc = pretty . toText
