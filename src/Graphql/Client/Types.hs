module Graphql.Client.Types where

import Relude

data Query

newtype Name = Name Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, ToText, IsString)

-- https://spec.graphql.org/June2018/#sec-Language.Document
newtype Document
  = Document (NonEmpty Definition)
  deriving stock (Show)

data Definition
  = DefinitionExecutable ExecutableDefinition
  | DefinitionTypeSystem TypeSystemDefinition
  deriving stock (Show)

-- | The GraphQL Type system describes the capabilities of a GraphQL server
-- and is used to determine if a query is valid.
data TypeSystemDefinition
  = TypeSystemDefinitionSchema SchemaDefinition
  | TypeSystemDefinitionType TypeDefinition
  --  TypeSystemDefinitionDirective DirectiveDefinition
  deriving stock (Show)

newtype SchemaDefinition
  = SchemaDefinition (NonEmpty RootOperationTypeDefinition)
  deriving stock (Show)

data RootOperationTypeDefinition = RootOperationTypeDefinition
  { _rotdOperationType :: OperationType,
    _rotdNamedType :: NamedType
  }
  deriving stock (Show)

newtype NamedType = NamedType
  { _namedTypeName :: Name
  }
  deriving stock (Show)

data TypeDefinition
  = TypeDefinitionScalar ScalarTypeDefinition
  | TypeDefinitionObject ObjectTypeDefinition
  | TypeDefinitionInterface InterfaceTypeDefinition
  | TypeDefinitionUnion UnionTypeDefinition
  | TypeDefinitionEnum EnumTypeDefinition
  | TypeDefinitionInputObject InputObjectTypeDefinition
  deriving stock (Show)

data ScalarTypeDefinition = ScalarTypeDefinition
  { _stdDescription :: Maybe Description,
    _stdName :: Name,
    _stdDirectives :: [Directive]
  }
  deriving stock (Show)

newtype Description = Description Text
  deriving stock (Show)
  deriving newtype (ToText, IsString)

data ObjectTypeDefinition = ObjectTypeDefinition
  { _otdDescription :: Maybe Description,
    _otdName :: Name,
    _otdInterfaces :: Maybe (NonEmpty NamedType),
    _otdDirectives :: [Directive],
    _otdFieldsDefinition :: Maybe FieldsDefinition
  }
  deriving stock (Show)

newtype FieldsDefinition = FieldsDefinition [FieldDefinition]
  deriving stock (Show)

data FieldDefinition = FieldDefinition
  { _fdDescription :: Maybe Description,
    _fdName :: Name,
    _fdArgumentsDefinition :: Maybe ArgumentsDefinition,
    _fdType :: TypeWithNullability,
    _fdDirectives :: [Directive]
  }
  deriving stock (Show)

newtype ArgumentsDefinition = ArgumentsDefinition [InputValueDefinition]
  deriving stock (Show)

data InputValueDefinition = InputValueDefinition
  { _ivdDescription :: Maybe Description,
    _ivdName :: Name,
    _ivdType :: TypeWithNullability,
    _ivdDefaultValue :: Maybe Value,
    _ivdDirectives :: [Directive]
  }
  deriving stock (Show)

data TypeWithNullability = TypeWithNullability
  { _twnType :: GqlType,
    _twnNullability :: Bool
  }
  deriving stock (Show)

data GqlType
  = GqlTypeNamed NamedType
  | GqlTypeList [TypeWithNullability]
  deriving stock (Show)

data InterfaceTypeDefinition = InterfaceTypeDefinition
  { _itdDescription :: Maybe Description,
    _itdName :: Name,
    _itdDirectives :: [Directive],
    _itdFieldsDefinition :: Maybe FieldsDefinition
  }
  deriving stock (Show)

data UnionTypeDefinition = UnionTypeDefinition
  { _utdDescription :: Maybe Description,
    _utdName :: Name,
    _utdDirectives :: [Directive],
    _utdMemberTypes :: [NamedType]
  }
  deriving stock (Show)

data EnumTypeDefinition = EnumTypeDefinition
  { _etdDescription :: Maybe Description,
    _etdName :: Name,
    _etdDirectives :: [Directive],
    _etdValuesDefinition :: Maybe EnumValuesDefinition
  }
  deriving stock (Show)

newtype EnumValuesDefinition
  = EnumValuesDefinition [EnumValueDefinition]
  deriving stock (Show)

data EnumValueDefinition = EnumValueDefinition
  { _evdDescription :: Maybe Description,
    _evdEnumValue :: Name,
    _evdDirectives :: [Directive]
  }
  deriving stock (Show)

data InputObjectTypeDefinition = InputObjectTypeDefinition
  { _iotdDescription :: Maybe Description,
    _iotdName :: Name,
    _iotdDirectives :: [Directive],
    _iotdFieldsDefinition :: Maybe InputObjectFieldsDefinition
  }
  deriving stock (Show)

newtype InputObjectFieldsDefinition
  = InputObjectFieldsDefinition [InputValueDefinition]
  deriving stock (Show)

newtype ExecutableDefinition = ExecutableDefinition
  { unwrapExecutableDefinition :: OperationDefinition
  }
  deriving stock (Show)

data OperationType = Query | Mutation | Subscription
  deriving stock (Show)

data OperationDefinition
  = OperationDefinitionOperation Operation
  | OperationDefinitionSelectionSet SelectionSet
  deriving stock (Show)

data Operation = Operation
  { _operationType :: OperationType,
    _operationName :: Maybe Name,
    _operationVariableDefs :: [VariableDefinition],
    _operationDirectives :: [Directive],
    _operationSelectionSet :: SelectionSet
  }
  deriving stock (Show)

newtype SelectionSet = SelectionSet [Selection]
  deriving stock (Show)

newtype Selection
  = SelectionField Field
  --  SelectionFragmentSpread
  --  SelectionInlineFragment
  deriving stock (Show)

data Directive = Directive
  deriving stock (Show)

data VariableDefinition = VariableDefinition -- TODO
  deriving stock (Show)

data Field = Field
  { _fieldAlias :: Maybe Alias,
    _fieldName :: Name,
    _fieldArguments :: Maybe Arguments,
    _fieldDirectives :: [Directive],
    _fieldSelectionSet :: Maybe SelectionSet
  }
  deriving stock (Show)

newtype Arguments = Arguments [Argument]
  deriving stock (Show)

newtype Alias = Alias Name
  deriving stock (Show)
  deriving newtype (ToText, IsString)

data Argument = Argument Name Value
  deriving stock (Show)

data Value
  = ValueVariable Name
  | ValueInt Int
  | ValueFloat Double
  | ValueString Text
  | ValueBool Bool
  | ValueNull
  | ValueEnum Name
  | ValueList [Value]
  | ValueObject (Map Name Value)
  deriving stock (Show)
