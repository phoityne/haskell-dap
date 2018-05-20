
{-|
Module      : GHCi.DAP.IFData
Description : Implementation of DAP interface data type.
Copyright   : 2017-2018 phoityne_hs
License     : BSD3

Implementation of DAP interface data type.

@see : https://github.com/Microsoft/vscode-debugadapter-node/blob/master/protocol/src/debugProtocol.ts

-}
module GHCi.DAP.IFData (
    -- * setBreakpoints
    SetBreakpointsArguments(..)
  , SourceBreakpoint(..)
  , SetBreakpointsResponseBody(..)
  , Breakpoint(..)
  , defaultBreakpoint
    -- * setFunctionBreakpoints
  , SetFunctionBreakpointsArguments(..)
  , FunctionBreakpoint(..)
  , SetFunctionBreakpointsResponseBody(..)
    -- * continue
  , ContinueArguments(..)
  , StoppedEventBody(..)
  , defaultStoppedEventBody
    -- * next
  , NextArguments(..)
    -- * stepIn
  , StepInArguments(..)
    -- * scopes
  , ScopesArguments(..)
  , ScopesBody(..)
  , Scope(..)
  , defaultScope
    -- * stackTrace
  , StackTraceArguments(..)
  , StackTraceBody(..)
  , defaultStackTraceBody
  , StackFrame(..)
  , defaultStackFrame
    -- * variables
  , VariablesArguments(..)
  , VariablesBody(..)
  , Variable(..)
  , defaultVariable
    -- * evaluate
  , EvaluateArguments(..)
  , EvaluateBody(..)
  , defaultEvaluateBody
    -- * event
  , OutputEventBody(..)
  , defaultOutputEventBody
    -- * commons
  , VariablePresentationHint(..)
  , Source(..)
  , defaultSource
) where


-- |
--   Arguments for 'variables' request. 
--
data VariablesArguments =
  VariablesArguments {
    variablesReferenceVariablesArguments :: Int  -- ^The Variable reference.
  } deriving (Show, Read, Eq)


-- |
--    Response to "variables" request.
--
data VariablesBody =
  VariablesBody {
    variablesVariablesBody :: [Variable]  -- ^All (or a range) of variables for the given variable reference.
  } deriving (Show, Read, Eq)


-- |
--   A Variable is a name/value pair.
--   If the value is structured (has children), a handle is provided to retrieve the children with the VariablesRequest.
--
data Variable =
  Variable {
    nameVariable               :: String  -- ^The variable's name.
  , valueVariable              :: String  -- ^The variable's value. This can be a multi-line text, e.g. for a function the body of a function.
  , typeVariable               :: String  -- ^The type of the variable's value. Typically shown in the UI when hovering over the value.
  , presentationHintVariable   :: Maybe VariablePresentationHint -- ^Properties of a variable that can be used to determine how to render the variable in the UI.
  , evaluateNameVariable       :: Maybe String  -- ^Optional evaluatable name of this variable which can be passed to the 'EvaluateRequest' to fetch the variable's value.
  , variablesReferenceVariable :: Int           -- ^If variablesReference is > 0, the variable is structured and its children can be retrieved by passing variablesReference to the VariablesRequest.
  , namedVariablesVariable     :: Maybe Int     -- ^The number of named child variables.
  , indexedVariablesVariable   :: Maybe Int     -- ^The number of indexed child variables. The client can use this optional information to present the children in a paged UI and fetch them in chunks.
  } deriving (Show, Read, Eq)

-- |
-- 
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
    {-|
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
    -}
    kindVariablePresentationHint       :: String
    {-|
		  Set of attributes represented as an array of strings. Before introducing additional values, try to use the listed values.
			Values: 

      'static': Indicates that the object is static.

      'constant': Indicates that the object is a constant.

      'readOnly': Indicates that the object is read only.

      'rawString': Indicates that the object is a raw string.

      'hasObjectId': Indicates that the object can have an Object ID created for it.

      'canHaveObjectId': Indicates that the object has an Object ID associated with it.

      'hasSideEffects': Indicates that the evaluation had side effects.
    -}
  , attributesVariablePresentationHint :: [String]
    {-|
		  Visibility of variable. Before introducing additional values, try to use the listed values.
			Values: 'public', 'private', 'protected', 'internal', 'final', etc.
    -}
  , visibilityVariablePresentationHint :: String
  } deriving (Show, Read, Eq)


-- |
--   rguments for 'evaluate' request.
--
data EvaluateArguments =
  EvaluateArguments {
    expressionEvaluateArguments :: String     -- ^The expression to evaluate. 
  , frameIdEvaluateArguments    :: Maybe Int  -- ^Evaluate the expression in the scope of this stack frame. If not specified, the expression is evaluated in the global scope. 

  {-|
    The context in which the evaluate request is run.
    Values:
    'watch': evaluate is run in a watch.

    'repl': evaluate is run from REPL console.

    'hover': evaluate is run from a data hover.
    
    etc. 
  -}
  , contextEvaluateArguments    :: String
    } deriving (Show, Read, Eq)

    
-- |
--    Response to "evaluate" request. 
--
data EvaluateBody =
  EvaluateBody {
    resultEvaluateBody             :: String -- ^The result of the evaluate.
  , typeEvaluateBody               :: String -- ^The optional type of the evaluate result. 
  , presentationHintEvaluateBody   :: Maybe VariablePresentationHint -- ^Properties of a evaluate result that can be used to determine how to render the result in the UI.
  , variablesReferenceEvaluateBody :: Int       -- ^If variablesReference is > 0, the evaluate result is structured and its children can be retrieved by passing variablesReference to the VariablesRequest.
  , namedVariablesEvaluateBody     :: Maybe Int -- ^The number of named child variables. The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
  , indexedVariablesEvaluateBody   :: Maybe Int -- ^The number of indexed child variables. The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
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
--   Arguments for "scopes" request.
--
data ScopesArguments =
  ScopesArguments {
    frameIdScopesArguments :: Int  -- ^Retrieve the scopes for this stackframe.
  } deriving (Show, Read, Eq)


-- |
--   Response to 'scopes' request.
--
data ScopesBody =
  ScopesBody {
    scopesScopesBody :: [Scope]  -- ^The scopes of the stackframe. If the array has length zero, there are no scopes available.
  } deriving (Show, Read, Eq)


-- |
--   A Scope is a named container for variables. Optionally a scope can map to a source or a range within a source. 
--
data Scope =
  Scope {
    nameScope               :: String     -- ^Name of the scope such as 'Arguments', 'Locals'. 
  , variablesReferenceScope :: Int        -- ^The variables of this scope can be retrieved by passing the value of variablesReference to the VariablesRequest.
  , namedVariablesScope     :: Maybe Int  -- ^The number of named variables in this scope. The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
  , indexedVariablesScope   :: Maybe Int  -- ^The number of indexed variables in this scope. The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
  , expensiveScope          :: Bool       -- ^If true, the number of variables in this scope is large or expensive to retrieve.
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


-- |
--   Arguments for 'setBreakpoints' request.
--
data SetBreakpointsArguments =
  SetBreakpointsArguments {
    sourceSetBreakpointsArguments         :: Source              -- ^The source location of the breakpoints; either source.path or source.reference must be specified. 
  , breakpointsSetBreakpointsArguments    :: [SourceBreakpoint]  -- ^The code locations of the breakpoints.
  } deriving (Show, Read, Eq)


-- |
--   A Source is a descriptor for source code. It is returned from the debug adapter as part of a StackFrame and it is used by clients when specifying breakpoints.
--
data Source =
  Source {
    nameSource             :: Maybe String  -- ^The short name of the source. Every source returned from the debug adapter has a name. When sending a source to the debug adapter this name is optional.
  , pathSource             :: String        -- ^The path of the source to be shown in the UI. It is only used to locate and load the content of the source if no sourceReference is specified (or its vaule is 0).
  , sourceReferenceSource  :: Maybe Int     -- ^If sourceReference > 0 the contents of the source must be retrieved through the SourceRequest (even if a path is specified). A sourceReference is only valid for a session, so it must not be used to persist a source.
  , origineSource          :: Maybe String     -- ^The (optional) origin of this source: possible values 'internal module', 'inlined content from source map', etc.
  } deriving (Show, Read, Eq)


-- |
--
defaultSource :: Source
defaultSource = Source {
    nameSource             = Nothing
  , pathSource             = ""
  , sourceReferenceSource  = Nothing
  , origineSource          = Nothing
  }


-- |
--   Properties of a breakpoint passed to the setBreakpoints request.
--
data SourceBreakpoint =
  SourceBreakpoint {
    lineSourceBreakpoint         :: Int           -- ^The source line of the breakpoint.
  , columnSourceBreakpoint       :: Maybe Int     -- ^An optional source column of the breakpoint. 
  , conditionSourceBreakpoint    :: Maybe String  -- ^An optional expression for conditional breakpoints.
  , hitConditionSourceBreakpoint :: Maybe String  -- ^An optional expression that controls how many hits of the breakpoint are ignored. The backend is expected to interpret the expression as needed.
  , logMessageSourceBreakpoint   :: Maybe String  -- ^If this attribute exists and is non-empty, the backend must not 'break' (stop) but log the message instead. Expressions within {} are interpolated.
  } deriving (Show, Read, Eq)


-- |
--   Response to "setBreakpoints" request.
--   Returned is information about each breakpoint created by this request.
--   This includes the actual code location and whether the breakpoint could be verified.
--   The breakpoints returned are in the same order as the elements of the 'breakpoints'
--   (or the deprecated 'lines') in the SetBreakpointsArguments.
--
data SetBreakpointsResponseBody =
  SetBreakpointsResponseBody {
    breakpointsSetBreakpointsResponseBody :: [Breakpoint]  -- ^Information about the breakpoints. The array elements are in the same order as the elements of the 'breakpoints' (or the deprecated 'lines') in the SetBreakpointsArguments.
  } deriving (Show, Read, Eq)


-- |
--   Information about a Breakpoint created in setBreakpoints or setFunctionBreakpoints.
--
data Breakpoint =
  Breakpoint {
    idBreakpoint        :: Maybe Int -- ^An optional unique identifier for the breakpoint.
  , verifiedBreakpoint  :: Bool      -- ^If true breakpoint could be set (but not necessarily at the desired location).
  , messageBreakpoint   :: String    -- ^An optional message about the state of the breakpoint. This is shown to the user and can be used to explain why a breakpoint could not be verified.
  , sourceBreakpoint    :: Source    -- ^The source where the breakpoint is located.
  , lineBreakpoint      :: Int       -- ^The start line of the actual range covered by the breakpoint.
  , columnBreakpoint    :: Int       -- ^An optional start column of the actual range covered by the breakpoint.
  , endLineBreakpoint   :: Int       -- ^An optional end line of the actual range covered by the breakpoint.
  , endColumnBreakpoint :: Int       -- ^An optional end column of the actual range covered by the breakpoint. If no end line is given, then the end column is assumed to be in the start line.
  } deriving (Show, Read, Eq)


-- |
--
defaultBreakpoint :: Breakpoint
defaultBreakpoint = Breakpoint {
    idBreakpoint        = Nothing
  , verifiedBreakpoint  = False
  , messageBreakpoint   = ""
  , sourceBreakpoint    = defaultSource
  , lineBreakpoint      = 0
  , columnBreakpoint    = 0
  , endLineBreakpoint   = 0
  , endColumnBreakpoint = 0
  }


------------------------------------------------------------------------------------

-- |
--   Arguments for 'setFunctionBreakpoints' request.  
--
data SetFunctionBreakpointsArguments =
  SetFunctionBreakpointsArguments {
    breakpointsSetFunctionBreakpointsArguments    :: [FunctionBreakpoint]  -- The function names of the breakpoints.
  } deriving (Show, Read, Eq)


-- |
--   Properties of a breakpoint passed to the setFunctionBreakpoints request.
--
data FunctionBreakpoint =
  FunctionBreakpoint {
    nameFunctionBreakpoint         :: String        -- The name of the function. 
  , conditionFunctionBreakpoint    :: Maybe String  -- An optional expression for conditional breakpoints.
  , hitConditionFunctionBreakpoint :: Maybe String  -- An optional expression that controls how many hits of the breakpoint are ignored. The backend is expected to interpret the expression as needed.
  } deriving (Show, Read, Eq)


-- |
--  Response to 'setFunctionBreakpoints' request.
--  Returned is information about each breakpoint created by this request.
--
data SetFunctionBreakpointsResponseBody =
  SetFunctionBreakpointsResponseBody {
    breakpointsSetFunctionBreakpointsResponseBody :: [Breakpoint]  -- Information about the breakpoints. The array elements correspond to the elements of the 'breakpoints' array.
  } deriving (Show, Read, Eq)

------------------------------------------------------------------------------------

-- |
--   Arguments for 'continue' request.
--
data ContinueArguments =
  ContinueArguments {
    threadIdContinueArguments :: Int          -- ^Continue execution for the specified thread (if possible). If the backend cannot continue on a single thread but will continue on all threads, it should set the allThreadsContinued attribute in the response to true.
  , exprContinueArguments     :: Maybe String -- ^ADD: haskell-dap
  } deriving (Show, Read, Eq)


-- |
--   Arguments for 'next' request.
--
data NextArguments =
  NextArguments {
    threadIdNextArguments :: Int --  Execute 'next' for this thread. 
  } deriving (Show, Read, Eq)


-- |
--   Arguments for 'stepIn' request. 
--
data StepInArguments =
  StepInArguments {
    threadIdStepInArguments :: Int --  Execute 'stepIn' for this thread.
  } deriving (Show, Read, Eq)


-- |
--   Event message for 'stopped' event type.
--   The event indicates that the execution of the debuggee has stopped due to some condition.
--   This can be caused by a break point previously set, a stepping action has completed, by executing a debugger statement etc.
--
data StoppedEventBody =
  StoppedEventBody {
    reasonStoppedEventBody            :: String  -- ^The reason for the event.For backward compatibility this string is shown in the UI if the 'description' attribute is missing (but it must not be translated).Values: 'step', 'breakpoint', 'exception', 'pause', 'entry', etc.
  , descriptionStoppedEventBody       :: String  -- ^The full reason for the event, e.g. 'Paused on exception'. This string is shown in the UI as is.
  , threadIdStoppedEventBody          :: Int     -- ^The thread which was stopped.
  , textStoppedEventBody              :: String  -- ^Additional information. E.g. if reason is 'exception', text contains the exception name. This string is shown in the UI. 

  {-|
     If allThreadsStopped is true, a debug adapter can announce that all threads have stopped.
     The client should use this information to enable that all threads can be expanded to access their stacktraces.
     If the attribute is missing or false, only the thread with the given threadId can be expanded.
  -}
  , allThreadsStoppedStoppedEventBody :: Bool
  } deriving (Show, Read, Eq)

-- |
--
defaultStoppedEventBody :: StoppedEventBody
defaultStoppedEventBody = StoppedEventBody {
    reasonStoppedEventBody = "breakpoint"
  , descriptionStoppedEventBody = ""
  , threadIdStoppedEventBody = 0
  , textStoppedEventBody = ""
  , allThreadsStoppedStoppedEventBody = True
  }


-- |
--  Arguments for 'stackTrace' request.
--
data StackTraceArguments =
  StackTraceArguments {
    threadIdStackTraceArguments   :: Int        -- ^Retrieve the stacktrace for this thread.
  , startFrameStackTraceArguments :: Maybe Int  -- ^The index of the first frame to return; if omitted frames start at 0.
  , levelsStackTraceArguments     :: Int        -- ^The maximum number of frames to return. If levels is not specified or 0, all frames are returned.
  } deriving (Show, Read, Eq)


-- |
--   Response to 'stackTrace' request.
--
data StackTraceBody =
  StackTraceBody {
    stackFramesStackTraceBody :: [StackFrame]  -- ^The frames of the stackframe. If the array has length zero, there are no stackframes available. This means that there is no location information available.
  , totalFramesStackTraceBody :: Int           -- ^The total number of frames available. 
  } deriving (Show, Read, Eq)

-- |
--
defaultStackTraceBody :: StackTraceBody
defaultStackTraceBody = StackTraceBody {
    stackFramesStackTraceBody = []
  , totalFramesStackTraceBody = 0
  }

-- |
--   A Stackframe contains the source location.
--
data StackFrame =
  StackFrame {
    idStackFrame        :: Int     -- ^An identifier for the stack frame. It must be unique across all threads. This id can be used to retrieve the scopes of the frame with the 'scopesRequest' or to restart the execution of a stackframe.
  , nameStackFrame      :: String  -- ^The name of the stack frame, typically a method name.
  , sourceStackFrame    :: Source  -- ^The optional source of the frame.
  , lineStackFrame      :: Int     -- ^The line within the file of the frame. If source is null or doesn't exist, line is 0 and must be ignored.
  , columnStackFrame    :: Int     -- ^The column within the line. If source is null or doesn't exist, column is 0 and must be ignored. 
  , endLineStackFrame   :: Int     -- ^An optional end line of the range covered by the stack frame.
  , endColumnStackFrame :: Int     -- ^An optional end column of the range covered by the stack frame.
  } deriving (Show, Read, Eq)


-- |
--
defaultStackFrame = StackFrame {
    idStackFrame = 0
  , nameStackFrame = ""
  , sourceStackFrame = defaultSource
  , lineStackFrame = 0
  , columnStackFrame = 0
  , endLineStackFrame = 0
  , endColumnStackFrame = 0
}


------------------------------------------------------------------------------------


-- |
--   Event message for "output" event type. The event indicates that the target has produced output.
--
data OutputEventBody =
  OutputEventBody {
    categoryOutputEventBody :: String        -- The category of output (such as: 'console', 'stdout', 'stderr', 'telemetry'). If not specified, 'console' is assumed. 
  , outputOutputEventBody   :: String        -- The output to report.
  , dataOutputEventBody     :: Maybe String  -- Optional data to report. For the 'telemetry' category the data will be sent to telemetry, for the other categories the data is shown in JSON format.
  } deriving (Show, Read, Eq)

-- |
--
defaultOutputEventBody :: OutputEventBody
defaultOutputEventBody = OutputEventBody "console" "" Nothing


