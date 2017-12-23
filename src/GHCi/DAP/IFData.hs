
module GHCi.DAP.IFData where

  
-- |
--   A Variable is a name/value pair.
--   If the value is structured (has children), a handle is provided to retrieve the children with the VariablesRequest.
--
data Variable =
  Variable {
    nameVariable               :: String  -- The variable's name.
  , valueVariable              :: String  -- The variable's value. This can be a multi-line text, e.g. for a function the body of a function.
  , typeVariable               :: String  -- The type of the variable's value. Typically shown in the UI when hovering over the value.
  , presentationHintVariable   :: Maybe VariablePresentationHint -- Properties of a variable that can be used to determine how to render the variable in the UI.
  , evaluateNameVariable       :: Maybe String  -- Optional evaluatable name of this variable which can be passed to the 'EvaluateRequest' to fetch the variable's value.
  , variablesReferenceVariable :: Int           -- If variablesReference is > 0, the variable is structured and its children can be retrieved by passing variablesReference to the VariablesRequest.
  , namedVariablesVariable     :: Maybe Int     -- The number of named child variables.
  , indexedVariablesVariable   :: Maybe Int     -- The number of indexed child variables. The client can use this optional information to present the children in a paged UI and fetch them in chunks.
  } deriving (Show, Read, Eq)

defaultVariable :: Variable
defaultVariable = Variable {
    nameVariable = ""
  , valueVariable = ""
  , typeVariable = ""
  , presentationHintVariable = Nothing
  , evaluateNameVariable = Nothing
  , variablesReferenceVariable = 0
  , namedVariablesVariable = Nothing
  , indexedVariablesVariable = Nothing
  }

-- |
--   Optional properties of a variable that can be used to determine how to render the variable in the UI.
--
data VariablePresentationHint =
  VariablePresentationHint {
    {-
		  The kind of variable. Before introducing additional values, try to use the listed values.
			Values: 
			'property': Indicates that the object is a property.
			'method': Indicates that the object is a method.
			'class': Indicates that the object is a class.
			'data': Indicates that the object is data.
			'event': Indicates that the object is an event.
			'baseClass': Indicates that the object is a base class.
			'innerClass': Indicates that the object is an inner class.
			'interface': Indicates that the object is an interface.
			'mostDerivedClass': Indicates that the object is the most derived class.
			'virtual': Indicates that the object is virtual, that means it is a synthetic object introduced by the adapter for rendering purposes, e.g. an index range for large arrays.
			etc.
    -}
    kindVariablePresentationHint       :: String
    {-
		  Set of attributes represented as an array of strings. Before introducing additional values, try to use the listed values.
			Values: 
			'static': Indicates that the object is static.
			'constant': Indicates that the object is a constant.
			'readOnly': Indicates that the object is read only.
			'rawString': Indicates that the object is a raw string.
			'hasObjectId': Indicates that the object can have an Object ID created for it.
			'canHaveObjectId': Indicates that the object has an Object ID associated with it.
			'hasSideEffects': Indicates that the evaluation had side effects.
			etc.
    -}
  , attributesVariablePresentationHint :: [String]
    {-
		  Visibility of variable. Before introducing additional values, try to use the listed values.
			Values: 'public', 'private', 'protected', 'internal', 'final', etc.
    -}
  , visibilityVariablePresentationHint :: String
  } deriving (Show, Read, Eq)


-- |
--    Response to "evaluate" request. 
--
data EvaluateBody =
  EvaluateBody {
    resultEvaluateBody             :: String -- The result of the evaluate.
  , typeEvaluateBody               :: String -- The optional type of the evaluate result. 
  , presentationHintEvaluateBody   :: Maybe VariablePresentationHint -- Properties of a evaluate result that can be used to determine how to render the result in the UI.
  , variablesReferenceEvaluateBody :: Int       -- If variablesReference is > 0, the evaluate result is structured and its children can be retrieved by passing variablesReference to the VariablesRequest.
  , namedVariablesEvaluateBody     :: Maybe Int -- The number of named child variables. The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
  , indexedVariablesEvaluateBody   :: Maybe Int -- The number of indexed child variables. The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
  } deriving (Show, Read, Eq)

-- |
--
defaultEvaluateBody :: EvaluateBody
defaultEvaluateBody = EvaluateBody {
    resultEvaluateBody = ""
  , typeEvaluateBody   = ""
  , presentationHintEvaluateBody   = Nothing
  , variablesReferenceEvaluateBody = 0
  , namedVariablesEvaluateBody     = Nothing
  , indexedVariablesEvaluateBody   = Nothing
  }


-- |
--   SResponse to 'scopes' request.
--
data ScopesBody =
  ScopesBody {
    scopesScopesBody :: [Scope]  -- The scopes of the stackframe. If the array has length zero, there are no scopes available.
  } deriving (Show, Read, Eq)


-- |
--   A Scope is a named container for variables. Optionally a scope can map to a source or a range within a source. 
--
data Scope =
  Scope {
    nameScope               :: String     -- Name of the scope such as 'Arguments', 'Locals'. 
  , variablesReferenceScope :: Int        -- The variables of this scope can be retrieved by passing the value of variablesReference to the VariablesRequest.
  , namedVariablesScope     :: Maybe Int  -- The number of named variables in this scope. The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
  , indexedVariablesScope   :: Maybe Int  -- The number of indexed variables in this scope. The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
  , expensiveScope          :: Bool       -- If true, the number of variables in this scope is large or expensive to retrieve.
  } deriving (Show, Read, Eq)

-- |
--
defaultScope :: Scope
defaultScope = Scope {
    nameScope = ""
  , variablesReferenceScope = 0
  , namedVariablesScope = Nothing
  , indexedVariablesScope = Nothing
  , expensiveScope = False
  }
