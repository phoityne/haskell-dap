{-|
Module      : Haskell.DAP
Description : Implementation of DAP interface data type.
Copyright   : 2017-2019 phoityne_hs
License     : BSD3

Implementation of DAP interface data type.

@see : https://microsoft.github.io/debug-adapter-protocol/

@see : https://github.com/Microsoft/vscode-debugadapter-node/blob/master/protocol/src/debugProtocol.ts

-}
module Haskell.DAP (

    -- * commons
    _THREAD_ID
  , Request(..)
  , defaultRequest
  , Response(..)
  , ColumnDescriptor(..)
  , Source(..)
  , defaultSource
  , Breakpoint(..)
  , defaultBreakpoint
  , ExceptionBreakpointsFilter(..)

    -- * initialize
  , InitializeRequest(..)
  , defaultInitializeRequest
  , InitializeRequestArguments(..)
  , defaultInitializeRequestArguments
  , InitializeResponse(..)
  , defaultInitializeResponse
  , InitializeResponseBody(..)
  , defaultInitializeResponseBody

    -- * disconnect
  , DisconnectRequest(..)
  , DisconnectRequestArguments(..)
  , DisconnectResponse(..)
  , defaultDisconnectResponse

    -- * pause
  , PauseRequest(..)
  , PauseRequestArguments(..)
  , PauseResponse(..)
  , defaultPauseResponse

    -- * terminate
  , TerminateRequest(..)
  , TerminateRequestArguments(..)
  , TerminateResponse(..)
  , defaultTerminateResponse

    -- * launch
  , LaunchRequest(..)
  , LaunchRequestArguments(..)
  , LaunchResponse(..)
  , defaultLaunchResponse

    -- * setBreakpoints
  , SourceBreakpoint(..)
  , SetBreakpointsRequest(..)
  , SetBreakpointsRequestArguments(..)
  , SetBreakpointsResponse(..)
  , SetBreakpointsResponseBody(..)
  , defaultSetBreakpointsResponse
  , defaultSetBreakpointsResponseBody

    -- * setFunctionBreakpoints
  , FunctionBreakpoint(..)
  , SetFunctionBreakpointsRequest(..)
  , SetFunctionBreakpointsRequestArguments(..)
  , SetFunctionBreakpointsResponse(..)
  , SetFunctionBreakpointsResponseBody(..)
  , defaultSetFunctionBreakpointsResponse

    -- * setExceptionBreakpoints
  , SetExceptionBreakpointsRequest(..)
  , SetExceptionBreakpointsRequestArguments(..)
  , SetExceptionBreakpointsResponse(..)
  , defaultSetExceptionBreakpointsResponse

    -- * configurationDone
  , ConfigurationDoneRequest(..)
  , ConfigurationDoneResponse(..)
  , defaultConfigurationDoneResponse

    -- * threads
  , ThreadsRequest(..)
  , defaultThreadsResponse
  , Thread(..)
  , ThreadsResponse(..)
  , ThreadsResponseBody(..)
  , defaultThreadsResponseBody

    -- * stackTrace
  , StackTraceRequest(..)
  , StackTraceRequestArguments(..)
  , StackFrame(..)
  , defaultStackFrame
  , StackTraceResponse(..)
  , defaultStackTraceResponse
  , StackTraceResponseBody(..)
  , defaultStackTraceResponseBody

    -- * scopes
  , ScopesRequest(..)
  , ScopesRequestArguments(..)
  , Scope(..)
  , defaultScope
  , ScopesResponse(..)
  , defaultScopesResponse
  , ScopesResponseBody(..)

    -- * variables
  , VariablesRequest(..)
  , VariablesRequestArguments(..)
  , Variable(..)
  , defaultVariable
  , VariablePresentationHint(..)
  , VariablesResponse(..)
  , defaultVariablesResponse
  , VariablesResponseBody(..)
  , defaultVariablesResponseBody

    -- * continue
  , ContinueRequest(..)
  , ContinueRequestArguments(..)
  , defaultContinueRequestArguments
  , ContinueResponse(..)
  , defaultContinueResponse

    -- * next
  , NextRequest(..)
  , NextRequestArguments(..)
  , NextResponse(..)
  , defaultNextResponse

    -- * stepIn
  , StepInRequest(..)
  , StepInRequestArguments(..)
  , StepInResponse(..)
  , defaultStepInResponse

    -- * evaluate
  , EvaluateRequest(..)
  , EvaluateRequestArguments(..)
  , EvaluateResponse(..)
  , defaultEvaluateResponse
  , EvaluateResponseBody(..)
  , defaultEvaluateResponseBody

    -- * completions
  , CompletionsRequest(..)
  , CompletionsRequestArguments(..)
  , CompletionsItem(..)
  , CompletionsResponse(..)
  , defaultCompletionsResponse
  , CompletionsResponseBody(..)
  , defaultCompletionsResponseBody

    -- * event
  , OutputEvent(..)
  , defaultOutputEvent
  , OutputEventBody(..)
  , defaultOutputEventBody

  , InitializedEvent(..)
  , defaultInitializedEvent

  , TerminatedEvent(..)
  , defaultTerminatedEvent
  , TerminatedEventBody(..)
  , defaultTerminatedEventBody

  , ExitedEvent(..)
  , defaultExitedEvent
  , ExitedEventBody(..)
  , defaultExitedEventBody

  , ContinuedEvent(..)
  , defaultContinuedEvent
  , ContinuedEventBody(..)
  , defaultContinuedEventBody

  , StoppedEvent(..)
  , defaultStoppedEvent
  , StoppedEventBody(..)
  , defaultStoppedEventBody

) where

import qualified Data.Map as M


----------------------------------------------------------------------------
--  commons
----------------------------------------------------------------------------

-- |
--   The debugee thread id is fixed 0.
--
_THREAD_ID :: Int
_THREAD_ID = 0


-- |
--   Client-initiated request
--
data Request =
  Request {
    seqRequest       :: Int     -- ^Sequence number.
  , typeRequest      :: String  -- ^Message type. Values: 'request', 'response', 'event', etc.
  , commandRequest   :: String  -- ^The command to execute
  } deriving (Show, Read, Eq)


-- |
--
defaultRequest :: Request
defaultRequest = Request {
    seqRequest     = 0
  , typeRequest    = "request"
  , commandRequest = ""
  }


-- |
--  Response for a request.
--
data Response =
  Response {
    seqResponse         :: Int     -- ^Sequence number.
  , typeResponse        :: String  -- ^Message type. Values: 'request', 'response', 'event', etc.
  , request_seqResponse :: Int     -- ^Sequence number of the corresponding request.
  , successResponse     :: Bool    -- ^Outcome of the request.
  , commandResponse     :: String  -- ^The command requested.
  , messageResponse     :: String  -- ^Contains error message if success == false.
  } deriving (Show, Read, Eq)


-- |
--   A ColumnDescriptor specifies what module attribute to show in a column of the ModulesView,
--
--   how to format it, and what the column's label should be.
--
--   It is only used if the underlying UI actually supports this level of customization.
--
data ColumnDescriptor =
  ColumnDescriptor {
    attributeNameColumnDescriptor :: String        -- ^Name of the attribute rendered in this column.
  , labelColumnDescriptor         :: String        -- ^Header UI label of column.
  , formatColumnDescriptor        :: Maybe String  -- ^Format to use for the rendered values in this column. TBD how the format strings looks like.
  , typeColumnDescriptor          :: Maybe String  -- ^Datatype of values in this column.  Defaults to 'string' if not specified. 'string' | 'number' | 'boolean' | 'unixTimestampUTC';
  , widthColumnDescriptor         :: Maybe Int     -- ^Width of this column in characters (hint only).
  } deriving (Show, Read, Eq)


-- |
--   A Source is a descriptor for source code.
--
--   It is returned from the debug adapter as part of a StackFrame and it is used by clients
--
--   when specifying breakpoints.
--
data Source =
  Source {
    nameSource             :: Maybe String  -- ^The short name of the source. Every source returned from the debug adapter has a name. When sending a source to the debug adapter this name is optional.
  , pathSource             :: String        -- ^The path of the source to be shown in the UI. It is only used to locate and load the content of the source if no sourceReference is specified (or its vaule is 0).
  , sourceReferenceSource  :: Maybe Int     -- ^If sourceReference > 0 the contents of the source must be retrieved through the SourceRequest (even if a path is specified). A sourceReference is only valid for a session, so it must not be used to persist a source.
  , origineSource          :: Maybe String  -- ^The (optional) origin of this source: possible values 'internal module', 'inlined content from source map', etc.
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


-- |
--   An ExceptionBreakpointsFilter is shown in the UI as an option for configuring how exceptions are dealt with.
--
data ExceptionBreakpointsFilter =
  ExceptionBreakpointsFilter {
    filterExceptionBreakpointsFilter  :: String  -- ^The internal ID of the filter. This value is passed to the setExceptionBreakpoints request.
  , labelExceptionBreakpointsFilter   :: String  -- ^The name of the filter. This will be shown in the UI.
  , defaultExceptionBreakpointsFilter :: Bool    -- ^Initial value of the filter. If not specified a value 'false' is assumed.
  } deriving (Show, Read, Eq)


----------------------------------------------------------------------------
--  Initialize
----------------------------------------------------------------------------

-- |
--   Initialize request; value of command field is 'initialize'.
--
--   The 'initialize' request is sent as the first request from the client to the debug adapter in order to configure it with client capabilities and to retrieve capabilities from the debug adapter.
--
--   Until the debug adapter has responded to with an 'initialize' response, the client must not send any additional requests or events to the debug adapter. In addition the debug adapter is not allowed to send any requests or events to the client until it has responded with an 'initialize' response.
--
--   The 'initialize' request may only be sent once.
--
data InitializeRequest =
  InitializeRequest {
    seqInitializeRequest       :: Int                         -- ^Sequence number
  , typeInitializeRequest      :: String                      -- ^One of "request", "response", or "event"
  , commandInitializeRequest   :: String                      -- ^The command to execute
  , argumentsInitializeRequest :: InitializeRequestArguments  -- ^Object containing arguments for the command
  } deriving (Show, Read, Eq)


-- |
--
defaultInitializeRequest :: InitializeRequest
defaultInitializeRequest = InitializeRequest {
    seqInitializeRequest       = 0
  , typeInitializeRequest      = "request"
  , commandInitializeRequest   = "initialize"
  , argumentsInitializeRequest = defaultInitializeRequestArguments
  }


-- |
--   Arguments for 'initialize' request.
--
data InitializeRequestArguments =
  InitializeRequestArguments {
    adapterIDInitializeRequestArguments       :: String  -- ^The ID of the debugger adapter. Used to select or verify debugger adapter.
  , linesStartAt1InitializeRequestArguments   :: Bool    -- ^If true all line numbers are 1-based (default).
  , columnsStartAt1InitializeRequestArguments :: Bool    -- ^If true all column numbers are 1-based (default).
  , pathFormatInitializeRequestArguments      :: String  -- ^Determines in what format paths are specified. Possible values are 'path' or 'uri'. The default is 'path', which is the native format.
  } deriving (Show, Read, Eq)


-- |
--
defaultInitializeRequestArguments :: InitializeRequestArguments
defaultInitializeRequestArguments = InitializeRequestArguments {
    adapterIDInitializeRequestArguments       = ""
  , linesStartAt1InitializeRequestArguments   = False
  , columnsStartAt1InitializeRequestArguments = False
  , pathFormatInitializeRequestArguments      = ""
  }


-- |
--   Response to 'initialize' request.
--
data InitializeResponse =
  InitializeResponse {
    seqInitializeResponse         :: Int     -- ^Sequence number
  , typeInitializeResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqInitializeResponse :: Int     -- ^Sequence number of the corresponding request
  , successInitializeResponse     :: Bool    -- ^Outcome of the request
  , commandInitializeResponse     :: String  -- ^The command requested
  , messageInitializeResponse     :: String  -- ^Contains error message if success == false.
  , bodyInitializeResponse        :: InitializeResponseBody  -- ^The capabilities of this debug adapter
  } deriving (Show, Read, Eq)


-- |
--
defaultInitializeResponse :: InitializeResponse
defaultInitializeResponse = InitializeResponse {
    seqInitializeResponse         = 0
  , typeInitializeResponse        = "response"
  , request_seqInitializeResponse = 0
  , successInitializeResponse     = False
  , commandInitializeResponse     = "initialize"
  , messageInitializeResponse     = ""
  , bodyInitializeResponse        = defaultInitializeResponseBody
  }


-- |
--   Information about the capabilities of a debug adapter.
--
data InitializeResponseBody =
  InitializeResponseBody {
    supportsConfigurationDoneRequestInitializeResponseBody  :: Bool  -- ^The debug adapter supports the 'configurationDone' request.
  , supportsFunctionBreakpointsInitializeResponseBody       :: Bool  -- ^The debug adapter supports functionBreakpoints.
  , supportsConditionalBreakpointsInitializeResponseBody    :: Bool  -- ^The debug adapter supports conditionalBreakpoints.
  , supportsHitConditionalBreakpointsInitializeResponseBody :: Bool  -- ^The debug adapter supports breakpoints that break execution after a specified number of hits.
  , supportsEvaluateForHoversInitializeResponseBody         :: Bool  -- ^The debug adapter supports a (side effect free) evaluate request for data hovers.
  , exceptionBreakpointFiltersInitializeResponseBody        :: [ExceptionBreakpointsFilter]  -- ^Available filters for the setExceptionBreakpoints request.
  , supportsStepBackInitializeResponseBody                  :: Bool  -- ^The debug adapter supports stepping back.
  , supportsSetVariableInitializeResponseBody               :: Bool  -- ^The debug adapter supports setting a variable to a value.
  , supportsRestartFrameInitializeResponseBody              :: Bool  -- ^The debug adapter supports restarting a frame.
  , supportsGotoTargetsRequestInitializeResponseBody        :: Bool  -- ^The debug adapter supports the gotoTargetsRequest.
  , supportsStepInTargetsRequestInitializeResponseBody      :: Bool  -- ^The debug adapter supports the stepInTargetsRequest.
  , supportsCompletionsRequestInitializeResponseBody        :: Bool  -- ^The debug adapter supports the completionsRequest.
  , supportsModulesRequestInitializeResponseBody            :: Bool  -- ^The debug adapter supports the modules request.
  , additionalModuleColumnsInitializeResponseBody           :: [ColumnDescriptor] -- ^The set of additional module information exposed by the debug adapter.
  , supportsLogPointsInitializeResponseBody                 :: Bool  -- ^The debug adapter supports logpoints by interpreting the 'logMessage' attribute of the SourceBreakpoint.
  , supportsTerminateRequestInitializeResponseBody          :: Bool  -- ^The debug adapter supports the 'terminate' request.
  } deriving (Show, Read, Eq)

-- |
--
defaultInitializeResponseBody :: InitializeResponseBody
defaultInitializeResponseBody = InitializeResponseBody {
    supportsConfigurationDoneRequestInitializeResponseBody  = False
  , supportsFunctionBreakpointsInitializeResponseBody       = False
  , supportsConditionalBreakpointsInitializeResponseBody    = False
  , supportsHitConditionalBreakpointsInitializeResponseBody = False
  , supportsEvaluateForHoversInitializeResponseBody         = False
  , exceptionBreakpointFiltersInitializeResponseBody        = []
  , supportsStepBackInitializeResponseBody                  = False
  , supportsSetVariableInitializeResponseBody               = False
  , supportsRestartFrameInitializeResponseBody              = False
  , supportsGotoTargetsRequestInitializeResponseBody        = False
  , supportsStepInTargetsRequestInitializeResponseBody      = False
  , supportsCompletionsRequestInitializeResponseBody        = False
  , supportsModulesRequestInitializeResponseBody            = False
  , additionalModuleColumnsInitializeResponseBody           = []
  , supportsLogPointsInitializeResponseBody                 = False
  , supportsTerminateRequestInitializeResponseBody          = False
  }


----------------------------------------------------------------------------
--  Disconnect
----------------------------------------------------------------------------

-- |
--   Disconnect request; value of command field is 'disconnect'.
--
--	The 'disconnect' request is sent from the client to the debug adapter in order to stop debugging.
--
--  It asks the debug adapter to disconnect from the debuggee and to terminate the debug adapter.
--
--  If the debuggee has been started with the 'launch' request, the 'disconnect' request terminates the debuggee.
--
--  If the 'attach' request was used to connect to the debuggee, 'disconnect' does not terminate the debuggee.
--
-- This behavior can be controlled with the 'terminateDebuggee' argument (if supported by the debug adapter).
--
data DisconnectRequest =
  DisconnectRequest {
    seqDisconnectRequest       :: Int                        -- ^Sequence number
  , typeDisconnectRequest      :: String                     -- ^One of "request", "response", or "event"
  , commandDisconnectRequest   :: String                     -- ^The command to execute
  , argumentsDisconnectRequest :: Maybe DisconnectRequestArguments  -- ^Arguments for "disconnect" request.
  } deriving (Show, Read, Eq)


-- |
--   Arguments for 'disconnect' request.
--
data DisconnectRequestArguments =
  DisconnectArguments {
    restartDisconnectRequestArguments :: Maybe Bool  -- ^A value of true indicates that this 'disconnect' request is part of a restart sequence.
  } deriving (Show, Read, Eq)


-- |
--   Response to 'disconnect' request. This is just an acknowledgement, so no body field is required.
--
data DisconnectResponse =
  DisconnectResponse {
    seqDisconnectResponse         :: Int     -- ^Sequence number
  , typeDisconnectResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqDisconnectResponse :: Int     -- ^Sequence number of the corresponding request
  , successDisconnectResponse     :: Bool    -- ^Outcome of the request
  , commandDisconnectResponse     :: String  -- ^The command requested
  , messageDisconnectResponse     :: String  -- ^Contains error message if success == false.
  } deriving (Show, Read, Eq)


-- |
--
defaultDisconnectResponse :: DisconnectResponse
defaultDisconnectResponse = DisconnectResponse {
    seqDisconnectResponse         = 0
  , typeDisconnectResponse        = "response"
  , request_seqDisconnectResponse = 0
  , successDisconnectResponse     = False
  , commandDisconnectResponse     = "disconnect"
  , messageDisconnectResponse     = ""
  }



----------------------------------------------------------------------------
--  PauseRequest
----------------------------------------------------------------------------

-- |
--   Pause request; value of command field is "pause".
--
--     The request suspenses the debuggee.
--
--     The debug adapter first sends the response and then a 'stopped' event (with reason 'pause') after the thread has been paused successfully.
--
data PauseRequest =
  PauseRequest {
    seqPauseRequest       :: Int                        -- ^Sequence number
  , typePauseRequest      :: String                     -- ^One of "request", "response", or "event"
  , commandPauseRequest   :: String                     -- ^The command to execute
  , argumentsPauseRequest :: Maybe PauseRequestArguments  -- ^Arguments for "pause" request.
  } deriving (Show, Read, Eq)


-- |
--   Arguments for "pause" request.
--
data PauseRequestArguments =
  PauseArguments {
    threadIdPauseRequestArguments :: Int  -- ^Pause execution for this thread.
  } deriving (Show, Read, Eq)


-- |
--   Response to "pause" request. This is just an acknowledgement, so no body field is required.
--
data PauseResponse =
  PauseResponse {
    seqPauseResponse         :: Int     -- ^Sequence number
  , typePauseResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqPauseResponse :: Int     -- ^Sequence number of the corresponding request
  , successPauseResponse     :: Bool    -- ^Outcome of the request
  , commandPauseResponse     :: String  -- ^The command requested
  , messagePauseResponse     :: String  -- ^Contains error message if success == false.
  } deriving (Show, Read, Eq)


-- |
--
defaultPauseResponse :: PauseResponse
defaultPauseResponse = PauseResponse {
    seqPauseResponse         = 0
  , typePauseResponse        = "response"
  , request_seqPauseResponse = 0
  , successPauseResponse     = False
  , commandPauseResponse     = "pause"
  , messagePauseResponse     = ""
  }




----------------------------------------------------------------------------
--  Terminate
----------------------------------------------------------------------------

-- |
--   Terminate request; value of command field is 'terminate'.
--
--	The 'terminate' request is sent from the client to the debug adapter in order to give the debuggee a chance for terminating itself.
--
data TerminateRequest =
  TerminateRequest {
    seqTerminateRequest       :: Int                        -- ^Sequence number
  , typeTerminateRequest      :: String                     -- ^One of "request", "response", or "event"
  , commandTerminateRequest   :: String                     -- ^The command to execute
  , argumentsTerminateRequest :: Maybe TerminateRequestArguments  -- ^Arguments for "terminate" request.
  } deriving (Show, Read, Eq)


-- |
--   Arguments for 'terminate' request.
--
data TerminateRequestArguments =
  TerminateArguments {
    restartTerminateRequestArguments :: Maybe Bool  -- ^A value of true indicates that this 'terminate' request is part of a restart sequence.
  } deriving (Show, Read, Eq)


-- |
--   Response to 'terminate' request. This is just an acknowledgement, so no body field is required.
--
data TerminateResponse =
  TerminateResponse {
    seqTerminateResponse         :: Int     -- ^Sequence number
  , typeTerminateResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqTerminateResponse :: Int     -- ^Sequence number of the corresponding request
  , successTerminateResponse     :: Bool    -- ^Outcome of the request
  , commandTerminateResponse     :: String  -- ^The command requested
  , messageTerminateResponse     :: String  -- ^Contains error message if success == false.
  } deriving (Show, Read, Eq)


-- |
--
defaultTerminateResponse :: TerminateResponse
defaultTerminateResponse = TerminateResponse {
    seqTerminateResponse         = 0
  , typeTerminateResponse        = "response"
  , request_seqTerminateResponse = 0
  , successTerminateResponse     = False
  , commandTerminateResponse     = "terminate"
  , messageTerminateResponse     = ""
  }


----------------------------------------------------------------------------
--  Launch
----------------------------------------------------------------------------

-- |
--   Launch request; value of command field is 'launch'.
--
--		The launch request is sent from the client to the debug adapter to start the debuggee with or without debugging (if 'noDebug' is true).
--
--     Since launching is debugger/runtime specific, the arguments for this request are not part of this specification.
--
data LaunchRequest =
  LaunchRequest {
    seqLaunchRequest       :: Int                     -- ^Sequence number
  , typeLaunchRequest      :: String                  -- ^One of "request", "response", or "event"
  , commandLaunchRequest   :: String                  -- ^The command to execute
  , argumentsLaunchRequest :: LaunchRequestArguments  -- ^Arguments for "launch" request.
  } deriving (Show, Read, Eq)


-- |
--   Arguments for 'launch' request. Additional attributes are implementation specific.
--
data LaunchRequestArguments =
  LaunchRequestArguments {
    noDebugLaunchRequestArguments      :: Maybe Bool -- ^If noDebug is true the launch request should launch the program without enabling debugging.
  , nameLaunchRequestArguments         :: String     -- ^Phoityne specific argument. Must be "haskell-debug-adapter".
  , typeLaunchRequestArguments         :: String     -- ^Phoityne specific argument. Must be "ghc".
  , requestLaunchRequestArguments      :: String     -- ^Phoityne specific argument. Must be "launch".
  , startupLaunchRequestArguments      :: String     -- ^Phoityne specific argument. The path to debug start file.
  , workspaceLaunchRequestArguments    :: String     -- ^Phoityne specific argument. The path to debugee workspace.
  , logFileLaunchRequestArguments      :: String     -- ^Phoityne specific argument. The path to the log file.
  , logLevelLaunchRequestArguments     :: String     -- ^Phoityne specific argument. The Logging Prioryt
  , ghciPromptLaunchRequestArguments   :: String     -- ^Phoityne specific argument. The ghci prompt used by hda.
  , ghciCmdLaunchRequestArguments      :: String     -- ^Phoityne specific argument. The command to start debugging.
  , stopOnEntryLaunchRequestArguments  :: Bool       -- ^Phoityne specific argument. Stop at the debugged function entry point.
  , mainArgsLaunchRequestArguments     :: Maybe String         -- ^Phoityne specific argument. required. Arguments of main function.
  , ghciEnvLaunchRequestArguments      :: M.Map String String  -- ^Phoityne specific argument. required. Additional Environments while debugging.
  , ghciInitialPromptLaunchRequestArguments :: Maybe String    -- ^Phoityne specific argument. The ghci initial prompt.
  , startupFuncLaunchRequestArguments  :: Maybe String         -- ^Phoityne specific argument. The debug entry function.
  , startupArgsLaunchRequestArguments  :: Maybe String         -- ^Phoityne specific argument. Arguments of the debug entry function.
  , forceInspectLaunchRequestArguments :: Maybe Bool           -- ^Phoityne specific argument. Inspect variable force.
  } deriving (Show, Read, Eq)


-- |
--   Response to 'launch' request. This is just an acknowledgement, so no body field is required.
--
data LaunchResponse =
  LaunchResponse {
    seqLaunchResponse         :: Int     -- ^Sequence number
  , typeLaunchResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqLaunchResponse :: Int     -- ^Sequence number of the corresponding request
  , successLaunchResponse     :: Bool    -- ^Outcome of the request
  , commandLaunchResponse     :: String  -- ^The command requested
  , messageLaunchResponse     :: String  -- ^Contains error message if success == false.
  } deriving (Show, Read, Eq)


-- |
--
defaultLaunchResponse :: LaunchResponse
defaultLaunchResponse = LaunchResponse {
    seqLaunchResponse         = 0
  , typeLaunchResponse        = "response"
  , request_seqLaunchResponse = 0
  , successLaunchResponse     = False
  , commandLaunchResponse     = "launch"
  , messageLaunchResponse     = ""
  }


----------------------------------------------------------------------------
--  SetBreakpoints
----------------------------------------------------------------------------

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
--   SetBreakpoints request; value of command field is "setBreakpoints".
--
--   Sets multiple breakpoints for a single source and clears all previous breakpoints in that source.
--
--   To clear all breakpoint for a source, specify an empty array.
--
--   When a breakpoint is hit, a StoppedEvent (event type 'breakpoint') is generated.
--
data SetBreakpointsRequest =
  SetBreakpointsRequest {
    seqSetBreakpointsRequest       :: Int                      -- ^Sequence number
  , typeSetBreakpointsRequest      :: String                   -- ^One of "request", "response", or "event"
  , commandSetBreakpointsRequest   :: String                   -- ^The command to execute
  , argumentsSetBreakpointsRequest :: SetBreakpointsRequestArguments  -- ^Arguments for "setBreakpoints" request.
  } deriving (Show, Read, Eq)



-- |
--   Arguments for 'setBreakpoints' request.
--
data SetBreakpointsRequestArguments =
  SetBreakpointsRequestArguments {
    sourceSetBreakpointsRequestArguments         :: Source              -- ^The source location of the breakpoints; either source.path or source.reference must be specified.
  , breakpointsSetBreakpointsRequestArguments    :: [SourceBreakpoint]  -- ^The code locations of the breakpoints.
  } deriving (Show, Read, Eq)



-- |
--   Response to "setBreakpoints" request.
--
--   Returned is information about each breakpoint created by this request.
--
--   This includes the actual code location and whether the breakpoint could be verified.
--
--   The breakpoints returned are in the same order as the elements of the 'breakpoints'
--
--   (or the deprecated 'lines') in the SetBreakpointsRequestArguments.
--
data SetBreakpointsResponse =
  SetBreakpointsResponse {
    seqSetBreakpointsResponse         :: Int     -- ^Sequence number
  , typeSetBreakpointsResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqSetBreakpointsResponse :: Int     -- ^Sequence number of the corresponding request
  , successSetBreakpointsResponse     :: Bool    -- ^Outcome of the request
  , commandSetBreakpointsResponse     :: String  -- ^The command requested
  , messageSetBreakpointsResponse     :: String  -- ^Contains error message if success == false.
  , bodySetBreakpointsResponse        :: SetBreakpointsResponseBody -- ^The body of SetBreakpointsResponse.
  } deriving (Show, Read, Eq)


-- |
--
defaultSetBreakpointsResponse :: SetBreakpointsResponse
defaultSetBreakpointsResponse = SetBreakpointsResponse {
    seqSetBreakpointsResponse         = 0
  , typeSetBreakpointsResponse        = "response"
  , request_seqSetBreakpointsResponse = 0
  , successSetBreakpointsResponse     = False
  , commandSetBreakpointsResponse     = "setBreakpoints"
  , messageSetBreakpointsResponse     = ""
  , bodySetBreakpointsResponse        = defaultSetBreakpointsResponseBody
  }

-- |
--   Response to "setBreakpoints" request.
--
--   Returned is information about each breakpoint created by this request.
--
--   This includes the actual code location and whether the breakpoint could be verified.
--
--   The breakpoints returned are in the same order as the elements of the 'breakpoints'
--
--   (or the deprecated 'lines') in the SetBreakpointsRequestArguments.
--
data SetBreakpointsResponseBody =
  SetBreakpointsResponseBody {
    breakpointsSetBreakpointsResponseBody :: [Breakpoint]  -- ^Information about the breakpoints. The array elements are in the same order as the elements of the 'breakpoints' (or the deprecated 'lines') in the SetBreakpointsRequestArguments.
  } deriving (Show, Read, Eq)


-- |
--
defaultSetBreakpointsResponseBody :: SetBreakpointsResponseBody
defaultSetBreakpointsResponseBody = SetBreakpointsResponseBody {
    breakpointsSetBreakpointsResponseBody = []
  }


----------------------------------------------------------------------------
--  SetFunctionBreakpoints
----------------------------------------------------------------------------

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
--   SetFunctionBreakpoints request; value of command field is "setFunctionBreakpoints".
--
--   Sets multiple function breakpoints and clears all previous function breakpoints.
--
--   To clear all function breakpoint, specify an empty array.
--
--   When a function breakpoint is hit, a StoppedEvent (event type 'function breakpoint') is generated.
--
data SetFunctionBreakpointsRequest =
  SetFunctionBreakpointsRequest {
    seqSetFunctionBreakpointsRequest       :: Int                             -- ^Sequence number
  , typeSetFunctionBreakpointsRequest      :: String                          -- ^One of "request", "response", or "event"
  , commandSetFunctionBreakpointsRequest   :: String                          -- ^The command to execute
  , argumentsSetFunctionBreakpointsRequest :: SetFunctionBreakpointsRequestArguments -- ^Arguments for "setFunctionBreakpoints" request.
  } deriving (Show, Read, Eq)


-- |
--   Arguments for 'setFunctionBreakpoints' request.
--
data SetFunctionBreakpointsRequestArguments =
  SetFunctionBreakpointsRequestArguments {
    breakpointsSetFunctionBreakpointsRequestArguments    :: [FunctionBreakpoint]  -- ^The function names of the breakpoints.
  } deriving (Show, Read, Eq)


-- |
--   Response to "setFunctionBreakpoints" request.
--
data SetFunctionBreakpointsResponse =
  SetFunctionBreakpointsResponse {
    seqSetFunctionBreakpointsResponse         :: Int     -- ^Sequence number
  , typeSetFunctionBreakpointsResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqSetFunctionBreakpointsResponse :: Int     -- ^Sequence number of the corresponding request
  , successSetFunctionBreakpointsResponse     :: Bool    -- ^Outcome of the request
  , commandSetFunctionBreakpointsResponse     :: String  -- ^The command requested
  , messageSetFunctionBreakpointsResponse     :: String  -- ^Contains error message if success == false.
  , bodySetFunctionBreakpointsResponse        :: SetFunctionBreakpointsResponseBody  -- ^The body of the SetFunctionBreakpointsResponse
  } deriving (Show, Read, Eq)


-- |
--
defaultSetFunctionBreakpointsResponse :: SetFunctionBreakpointsResponse
defaultSetFunctionBreakpointsResponse = SetFunctionBreakpointsResponse {
    seqSetFunctionBreakpointsResponse         = 0
  , typeSetFunctionBreakpointsResponse        = "response"
  , request_seqSetFunctionBreakpointsResponse = 0
  , successSetFunctionBreakpointsResponse     = False
  , commandSetFunctionBreakpointsResponse     = "setFunctionBreakpoints"
  , messageSetFunctionBreakpointsResponse     = ""
  , bodySetFunctionBreakpointsResponse        = defaultSetFunctionBreakpointsResponseBody
  }


-- |
--  Response to 'setFunctionBreakpoints' request.
--
--  Returned is information about each breakpoint created by this request.
--
data SetFunctionBreakpointsResponseBody =
  SetFunctionBreakpointsResponseBody {
    breakpointsSetFunctionBreakpointsResponseBody :: [Breakpoint]  -- ^Information about the breakpoints. The array elements correspond to the elements of the 'breakpoints' array.
  } deriving (Show, Read, Eq)


-- |
--
defaultSetFunctionBreakpointsResponseBody :: SetFunctionBreakpointsResponseBody
defaultSetFunctionBreakpointsResponseBody = SetFunctionBreakpointsResponseBody {
    breakpointsSetFunctionBreakpointsResponseBody = []
  }


----------------------------------------------------------------------------
--  SetExceptionBreakpoints
----------------------------------------------------------------------------

-- |
--   SetExceptionBreakpoints request; value of command field is 'setExceptionBreakpoints'.
--
--   The request configures the debuggers response to thrown exceptions. If an exception is configured to break,
--
--   a StoppedEvent is fired (event type 'exception').
--
data SetExceptionBreakpointsRequest =
  SetExceptionBreakpointsRequest {
    seqSetExceptionBreakpointsRequest       :: Int                                     -- ^Sequence number
  , typeSetExceptionBreakpointsRequest      :: String                                  -- ^One of "request", "response", or "event"
  , commandSetExceptionBreakpointsRequest   :: String                                  -- ^The command to execute
  , argumentsSetExceptionBreakpointsRequest :: SetExceptionBreakpointsRequestArguments -- ^Arguments for "setExceptionBreakpoints" request.
  } deriving (Show, Read, Eq)


-- |
--   Arguments for 'setExceptionBreakpoints' request.
--
data SetExceptionBreakpointsRequestArguments =
  SetExceptionBreakpointsRequestArguments {
    filtersSetExceptionBreakpointsRequestArguments :: [String]  -- ^IDs of checked exception options. The set of IDs is returned via the 'exceptionBreakpointFilters' capability.
  } deriving (Show, Read, Eq)



-- |
--   Response to 'setExceptionBreakpoints' request. This is just an acknowledgement, so no body field is required.
--
data SetExceptionBreakpointsResponse =
  SetExceptionBreakpointsResponse {
    seqSetExceptionBreakpointsResponse         :: Int     -- ^Sequence number
  , typeSetExceptionBreakpointsResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqSetExceptionBreakpointsResponse :: Int     -- ^Sequence number of the corresponding request
  , successSetExceptionBreakpointsResponse     :: Bool    -- ^Outcome of the request
  , commandSetExceptionBreakpointsResponse     :: String  -- ^The command requested
  , messageSetExceptionBreakpointsResponse     :: String  -- ^Contains error message if success == false.
  } deriving (Show, Read, Eq)


-- |
--
defaultSetExceptionBreakpointsResponse :: SetExceptionBreakpointsResponse
defaultSetExceptionBreakpointsResponse = SetExceptionBreakpointsResponse {
    seqSetExceptionBreakpointsResponse         = 0
  , typeSetExceptionBreakpointsResponse        = "response"
  , request_seqSetExceptionBreakpointsResponse = 0
  , successSetExceptionBreakpointsResponse     = False
  , commandSetExceptionBreakpointsResponse     = "setExceptionBreakpoints"
  , messageSetExceptionBreakpointsResponse     = ""
  }


----------------------------------------------------------------------------
--  ConfigurationDone
----------------------------------------------------------------------------

-- |
--   ConfigurationDone request; value of command field is 'configurationDone'.
--
--   The client of the debug protocol must send this request at the end of the sequence of configuration requests
--
--   (which was started by the InitializedEvent).
--
data ConfigurationDoneRequest =
  ConfigurationDoneRequest {
    seqConfigurationDoneRequest       :: Int               -- ^Sequence number
  , typeConfigurationDoneRequest      :: String            -- ^One of "request", "response", or "event"
  , commandConfigurationDoneRequest   :: String            -- ^The command to execute
  } deriving (Show, Read, Eq)


-- |
--   Response to 'configurationDone' request. This is just an acknowledgement, so no body field is required.
--
data ConfigurationDoneResponse =
  ConfigurationDoneResponse {
    seqConfigurationDoneResponse         :: Int     -- ^Sequence number
  , typeConfigurationDoneResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqConfigurationDoneResponse :: Int     -- ^Sequence number of the corresponding request
  , successConfigurationDoneResponse     :: Bool    -- ^Outcome of the request
  , commandConfigurationDoneResponse     :: String  -- ^The command requested
  , messageConfigurationDoneResponse     :: String  -- ^Contains error message if success == false.
  } deriving (Show, Read, Eq)


-- |
--
defaultConfigurationDoneResponse :: ConfigurationDoneResponse
defaultConfigurationDoneResponse = ConfigurationDoneResponse {
    seqConfigurationDoneResponse         = 0
  , typeConfigurationDoneResponse        = "response"
  , request_seqConfigurationDoneResponse = 0
  , successConfigurationDoneResponse     = False
  , commandConfigurationDoneResponse     = "configurationDone"
  , messageConfigurationDoneResponse     = ""
  }


----------------------------------------------------------------------------
--  Thread
----------------------------------------------------------------------------

-- |
--   Thread request; value of command field is "threads".
--
--   The request retrieves a list of all threads.
--
data ThreadsRequest =
  ThreadsRequest {
    seqThreadsRequest       :: Int              -- ^Sequence number
  , typeThreadsRequest      :: String           -- ^One of "request", "response", or "event"
  , commandThreadsRequest   :: String           -- ^The command to execute
  } deriving (Show, Read, Eq)


-- |
--  Response to "threads" request.
--
data ThreadsResponse =
  ThreadsResponse {
    seqThreadsResponse         :: Int     -- ^Sequence number
  , typeThreadsResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqThreadsResponse :: Int     -- ^Sequence number of the corresponding request
  , successThreadsResponse     :: Bool    -- ^Outcome of the request
  , commandThreadsResponse     :: String  -- ^The command requested
  , messageThreadsResponse     :: String  -- ^Contains error message if success == false.
  , bodyThreadsResponse        :: ThreadsResponseBody -- ^The body of ThreadsResponse
  } deriving (Show, Read, Eq)


-- |
--
defaultThreadsResponse :: ThreadsResponse
defaultThreadsResponse = ThreadsResponse {
    seqThreadsResponse         = 0
  , typeThreadsResponse        = "response"
  , request_seqThreadsResponse = 0
  , successThreadsResponse     = False
  , commandThreadsResponse     = "threads"
  , messageThreadsResponse     = ""
  , bodyThreadsResponse        = defaultThreadsResponseBody
  }


-- |
--    Response to "threads" request.
--
data ThreadsResponseBody =
  ThreadsResponseBody {
    threadsThreadsResponseBody :: [Thread]  -- ^All threads.
  } deriving (Show, Read, Eq)


-- |
--
defaultThreadsResponseBody :: ThreadsResponseBody
defaultThreadsResponseBody = ThreadsResponseBody [defaultThread]


-- |
--   A Thread is a name/value pair.
--
--   If the value is structured (has children), a handle is provided to retrieve the children with the ThreadsRequest.
--
data Thread =
  Thread {
    idThread   :: Int     -- ^Unique identifier for the thread.
  , nameThread :: String  -- ^A name of the thread.
  } deriving (Show, Read, Eq)


-- |
--
defaultThread :: Thread
defaultThread = Thread _THREAD_ID "ghci main thread"


----------------------------------------------------------------------------
--  StackTrace
----------------------------------------------------------------------------

-- |
--   StackTrace request; value of command field is "stackTrace".
--
--   The request returns a stacktrace from the current execution state.
--
data StackTraceRequest =
  StackTraceRequest {
    seqStackTraceRequest       :: Int                  -- ^Sequence number
  , typeStackTraceRequest      :: String               -- ^One of "request", "response", or "event"
  , commandStackTraceRequest   :: String               -- ^The command to execute
  , argumentsStackTraceRequest :: StackTraceRequestArguments  -- ^Arguments for "stackTrace" request.
  } deriving (Show, Read, Eq)


-- |
--  Arguments for 'stackTrace' request.
--
data StackTraceRequestArguments =
  StackTraceRequestArguments {
    threadIdStackTraceRequestArguments   :: Int        -- ^Retrieve the stacktrace for this thread.
  , startFrameStackTraceRequestArguments :: Maybe Int  -- ^The index of the first frame to return; if omitted frames start at 0.
  , levelsStackTraceRequestArguments     :: Maybe Int  -- ^The maximum number of frames to return. If levels is not specified or 0, all frames are returned.
  } deriving (Show, Read, Eq)


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
defaultStackFrame :: StackFrame
defaultStackFrame = StackFrame {
    idStackFrame = 0
  , nameStackFrame = ""
  , sourceStackFrame = defaultSource
  , lineStackFrame = 0
  , columnStackFrame = 0
  , endLineStackFrame = 0
  , endColumnStackFrame = 0
}

-- |
--  Response to "stackTrace" request.
--
data StackTraceResponse =
  StackTraceResponse {
    seqStackTraceResponse         :: Int     -- ^Sequence number
  , typeStackTraceResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqStackTraceResponse :: Int     -- ^Sequence number of the corresponding request
  , successStackTraceResponse     :: Bool    -- ^Outcome of the request
  , commandStackTraceResponse     :: String  -- ^The command requested
  , messageStackTraceResponse     :: String  -- ^Contains error message if success == false.
  , bodyStackTraceResponse        :: StackTraceResponseBody  -- ^The body of StackTraceResponse
  } deriving (Show, Read, Eq)

defaultStackTraceResponse :: StackTraceResponse
defaultStackTraceResponse = StackTraceResponse {
    seqStackTraceResponse         = 0
  , typeStackTraceResponse        = "response"
  , request_seqStackTraceResponse = 0
  , successStackTraceResponse     = False
  , commandStackTraceResponse     = "stackTrace"
  , messageStackTraceResponse     = ""
  , bodyStackTraceResponse        = defaultStackTraceResponseBody
  }


-- |
--   Response to 'stackTrace' request.
--
data StackTraceResponseBody =
  StackTraceResponseBody {
    stackFramesStackTraceResponseBody :: [StackFrame]  -- ^The frames of the stackframe. If the array has length zero, there are no stackframes available. This means that there is no location information available.
  , totalFramesStackTraceResponseBody :: Int           -- ^The total number of frames available.
  } deriving (Show, Read, Eq)

-- |
--
defaultStackTraceResponseBody :: StackTraceResponseBody
defaultStackTraceResponseBody = StackTraceResponseBody {
    stackFramesStackTraceResponseBody = []
  , totalFramesStackTraceResponseBody = 0
  }

----------------------------------------------------------------------------
--  Scopes
----------------------------------------------------------------------------


-- |
--   Scopes request; value of command field is "scopes".
--
--   The request returns the variable scopes for a given stackframe ID.
--
data ScopesRequest =
  ScopesRequest {
    seqScopesRequest       :: Int              -- ^Sequence number
  , typeScopesRequest      :: String           -- ^One of "request", "response", or "event"
  , commandScopesRequest   :: String           -- ^The command to execute
  , argumentsScopesRequest :: ScopesRequestArguments  -- ^Arguments for "scopes" request.
  } deriving (Show, Read, Eq)


-- |
--   Arguments for "scopes" request.
--
data ScopesRequestArguments =
  ScopesRequestArguments {
    frameIdScopesRequestArguments :: Int  -- ^Retrieve the scopes for this stackframe.
  } deriving (Show, Read, Eq)


-- |
--  Response to "scopes" request.
--
data ScopesResponse =
  ScopesResponse {
    seqScopesResponse         :: Int     -- ^Sequence number
  , typeScopesResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqScopesResponse :: Int     -- ^Sequence number of the corresponding request
  , successScopesResponse     :: Bool    -- ^Outcome of the request
  , commandScopesResponse     :: String  -- ^The command requested
  , messageScopesResponse     :: String  -- ^Contains error message if success == false.
  , bodyScopesResponse        :: ScopesResponseBody  -- ^The body of ScopesResponse.
  } deriving (Show, Read, Eq)

-- |
--
defaultScopesResponse :: ScopesResponse
defaultScopesResponse = ScopesResponse {
    seqScopesResponse         = 0
  , typeScopesResponse        = "response"
  , request_seqScopesResponse = 0
  , successScopesResponse     = False
  , commandScopesResponse     = "scopes"
  , messageScopesResponse     = ""
  , bodyScopesResponse        = defaultScopesResponseBody
  }

-- |
--   Response to 'scopes' request.
--
data ScopesResponseBody =
  ScopesResponseBody {
    scopesScopesResponseBody :: [Scope]  -- ^The scopes of the stackframe. If the array has length zero, there are no scopes available.
  } deriving (Show, Read, Eq)


-- |
--
defaultScopesResponseBody :: ScopesResponseBody
defaultScopesResponseBody = ScopesResponseBody {
    scopesScopesResponseBody = []
  }

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


----------------------------------------------------------------------------
--  Variables
----------------------------------------------------------------------------

-- |
--   Variables request; value of command field is "variables".
--
--   Retrieves all children for the given variable reference.
--
data VariablesRequest =
  VariablesRequest {
    seqVariablesRequest       :: Int                 -- ^Sequence number
  , typeVariablesRequest      :: String              -- ^One of "request", "response", or "event"
  , commandVariablesRequest   :: String              -- ^The command to execute
  , argumentsVariablesRequest :: VariablesRequestArguments  -- ^Arguments for "variables" request.
  } deriving (Show, Read, Eq)


-- |
--  Response to "variables" request.
--
data VariablesResponse =
  VariablesResponse {
    seqVariablesResponse         :: Int     -- ^Sequence number
  , typeVariablesResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqVariablesResponse :: Int     -- ^Sequence number of the corresponding request
  , successVariablesResponse     :: Bool    -- ^Outcome of the request
  , commandVariablesResponse     :: String  -- ^The command requested
  , messageVariablesResponse     :: String  -- ^Contains error message if success == false.
  , bodyVariablesResponse        :: VariablesResponseBody  -- ^The body of VariablesResponse
  } deriving (Show, Read, Eq)


-- |
--
defaultVariablesResponse :: VariablesResponse
defaultVariablesResponse = VariablesResponse {
    seqVariablesResponse         = 0
  , typeVariablesResponse        = "response"
  , request_seqVariablesResponse = 0
  , successVariablesResponse     = False
  , commandVariablesResponse     = "variables"
  , messageVariablesResponse     = ""
  , bodyVariablesResponse        = defaultVariablesResponseBody
  }


-- |
--   Arguments for 'variables' request.
--
data VariablesRequestArguments =
  VariablesRequestArguments {
    variablesReferenceVariablesRequestArguments :: Int  -- ^The Variable reference.
  } deriving (Show, Read, Eq)


-- |
--    Response to "variables" request.
--
data VariablesResponseBody =
  VariablesResponseBody {
    variablesVariablesResponseBody :: [Variable]  -- ^All (or a range) of variables for the given variable reference.
  } deriving (Show, Read, Eq)


-- |
--
defaultVariablesResponseBody :: VariablesResponseBody
defaultVariablesResponseBody = VariablesResponseBody []


-- |
--   A Variable is a name/value pair.
--
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



----------------------------------------------------------------------------
--  Continue
----------------------------------------------------------------------------

-- |
--   Continue request; value of command field is "continue".
--
--   The request starts the debuggee to run again.
--
data ContinueRequest =
  ContinueRequest {
    seqContinueRequest       :: Int               -- ^Sequence number
  , typeContinueRequest      :: String            -- ^One of "request", "response", or "event"
  , commandContinueRequest   :: String            -- ^The command to execute
  , argumentsContinueRequest :: ContinueRequestArguments -- ^Arguments for "continue" request.
  } deriving (Show, Read, Eq)



-- |
--   Arguments for 'continue' request.
--
data ContinueRequestArguments =
  ContinueRequestArguments {
    threadIdContinueRequestArguments :: Int          -- ^Continue execution for the specified thread (if possible). If the backend cannot continue on a single thread but will continue on all threads, it should set the allThreadsContinued attribute in the response to true.
  , exprContinueRequestArguments     :: Maybe String -- ^ADD: haskell-dap
  } deriving (Show, Read, Eq)


-- |
--
defaultContinueRequestArguments :: ContinueRequestArguments
defaultContinueRequestArguments = ContinueRequestArguments {
    threadIdContinueRequestArguments = _THREAD_ID
  , exprContinueRequestArguments = Nothing
  }


-- |
--   Response to "continue" request. This is just an acknowledgement, so no body field is required.
--
data ContinueResponse =
  ContinueResponse {
    seqContinueResponse         :: Int     -- ^Sequence number
  , typeContinueResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqContinueResponse :: Int     -- ^Sequence number of the corresponding request
  , successContinueResponse     :: Bool    -- ^Outcome of the request
  , commandContinueResponse     :: String  -- ^The command requested
  , messageContinueResponse     :: String  -- ^Contains error message if success == false.
  } deriving (Show, Read, Eq)


-- |
--
defaultContinueResponse :: ContinueResponse
defaultContinueResponse = ContinueResponse {
    seqContinueResponse         = 0
  , typeContinueResponse        = "response"
  , request_seqContinueResponse = 0
  , successContinueResponse     = False
  , commandContinueResponse     = "continue"
  , messageContinueResponse     = ""
  }


----------------------------------------------------------------------------
--  Next
----------------------------------------------------------------------------

-- |
--   Next request; value of command field is "next".
--
--   The request starts the debuggee to run again for one step.
--
--   penDebug will respond with a StoppedEvent (event type 'step') after running the step.
--
data NextRequest =
  NextRequest {
    seqNextRequest       :: Int            -- ^Sequence number
  , typeNextRequest      :: String         -- ^One of "request", "response", or "event"
  , commandNextRequest   :: String         -- ^The command to execute
  , argumentsNextRequest :: NextRequestArguments  -- ^Arguments for "disconnect" request.
  } deriving (Show, Read, Eq)


-- |
--   Arguments for 'next' request.
--
data NextRequestArguments =
  NextRequestArguments {
    threadIdNextRequestArguments :: Int -- ^Execute 'next' for this thread.
  } deriving (Show, Read, Eq)


-- |
--   Response to "next" request. This is just an acknowledgement, so no body field is required.
--
data NextResponse =
  NextResponse {
    seqNextResponse         :: Int     -- ^Sequence number
  , typeNextResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqNextResponse :: Int     -- ^Sequence number of the corresponding request
  , successNextResponse     :: Bool    -- ^Outcome of the request
  , commandNextResponse     :: String  -- ^The command requested
  , messageNextResponse     :: String  -- ^Contains error message if success == false.
  } deriving (Show, Read, Eq)


-- |
--
defaultNextResponse :: NextResponse
defaultNextResponse = NextResponse {
    seqNextResponse         = 0
  , typeNextResponse        = "response"
  , request_seqNextResponse = 0
  , successNextResponse     = False
  , commandNextResponse     = "next"
  , messageNextResponse     = ""
  }


----------------------------------------------------------------------------
--  StepIn
----------------------------------------------------------------------------

-- |
--   StepIn request; value of command field is "stepIn".
--
--   The request starts the debuggee to run again for one step.
--
--   The debug adapter will respond with a StoppedEvent (event type 'step') after running the step.
--
data StepInRequest =
  StepInRequest {
    seqStepInRequest       :: Int              -- ^Sequence number
  , typeStepInRequest      :: String           -- ^One of "request", "response", or "event"
  , commandStepInRequest   :: String           -- ^The command to execute
  , argumentsStepInRequest :: StepInRequestArguments  -- ^Arguments for "stepIn" request.
  } deriving (Show, Read, Eq)


-- |
--   Arguments for 'stepIn' request.
--
data StepInRequestArguments =
  StepInRequestArguments {
    threadIdStepInRequestArguments :: Int -- ^Execute 'stepIn' for this thread.
  } deriving (Show, Read, Eq)


-- |
--  Response to "stepIn" request. This is just an acknowledgement, so no body field is required.
--
data StepInResponse =
  StepInResponse {
    seqStepInResponse         :: Int     -- ^Sequence number
  , typeStepInResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqStepInResponse :: Int     -- ^Sequence number of the corresponding request
  , successStepInResponse     :: Bool    -- ^Outcome of the request
  , commandStepInResponse     :: String  -- ^The command requested
  , messageStepInResponse     :: String  -- ^Contains error message if success == false.
  } deriving (Show, Read, Eq)


-- |
--
defaultStepInResponse :: StepInResponse
defaultStepInResponse = StepInResponse {
    seqStepInResponse         = 0
  , typeStepInResponse        = "response"
  , request_seqStepInResponse = 0
  , successStepInResponse     = False
  , commandStepInResponse     = "stepIn"
  , messageStepInResponse     = ""
  }


----------------------------------------------------------------------------
--  Evaluate
----------------------------------------------------------------------------

-- |
--   Evaluate request; value of command field is "evaluate".
--
--   Evaluates the given expression in the context of the top most stack frame.
--
--   The expression has access to any variables and arguments that are in scope.
--
data EvaluateRequest =
  EvaluateRequest {
    seqEvaluateRequest       :: Int                -- ^Sequence number
  , typeEvaluateRequest      :: String             -- ^One of "request", "response", or "event"
  , commandEvaluateRequest   :: String             -- ^The command to execute
  , argumentsEvaluateRequest :: EvaluateRequestArguments  -- ^Arguments for "evaluate" request.
  } deriving (Show, Read, Eq)


-- |
--
defaultEvaluateResponse :: EvaluateResponse
defaultEvaluateResponse = EvaluateResponse {
    seqEvaluateResponse         = 0
  , typeEvaluateResponse        = "response"
  , request_seqEvaluateResponse = 0
  , successEvaluateResponse     = False
  , commandEvaluateResponse     = "evaluate"
  , messageEvaluateResponse     = ""
  , bodyEvaluateResponse        = defaultEvaluateResponseBody
  }


-- |
--   rguments for 'evaluate' request.
--
data EvaluateRequestArguments =
  EvaluateRequestArguments {
    expressionEvaluateRequestArguments :: String     -- ^The expression to evaluate.
  , frameIdEvaluateRequestArguments    :: Maybe Int  -- ^Evaluate the expression in the scope of this stack frame. If not specified, the expression is evaluated in the global scope.

  {-|
    The context in which the evaluate request is run.
    Values:
    'watch': evaluate is run in a watch.

    'repl': evaluate is run from REPL console.

    'hover': evaluate is run from a data hover.

    etc.
  -}
  , contextEvaluateRequestArguments    :: Maybe String
    } deriving (Show, Read, Eq)


-- |
--  Response to "evaluate" request.
--
data EvaluateResponse =
  EvaluateResponse {
    seqEvaluateResponse         :: Int     -- ^Sequence number
  , typeEvaluateResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqEvaluateResponse :: Int     -- ^Sequence number of the corresponding request
  , successEvaluateResponse     :: Bool    -- ^Outcome of the request
  , commandEvaluateResponse     :: String  -- ^The command requested
  , messageEvaluateResponse     :: String  -- ^Contains error message if success == false.
  , bodyEvaluateResponse        :: EvaluateResponseBody  -- The body of EvaluateResponse
  } deriving (Show, Read, Eq)


-- |
--    Response to "evaluate" request.
--
data EvaluateResponseBody =
  EvaluateResponseBody {
    resultEvaluateResponseBody             :: String -- ^The result of the evaluate.
  , typeEvaluateResponseBody               :: String -- ^The optional type of the evaluate result.
  , presentationHintEvaluateResponseBody   :: Maybe VariablePresentationHint -- ^Properties of a evaluate result that can be used to determine how to render the result in the UI.
  , variablesReferenceEvaluateResponseBody :: Int       -- ^If variablesReference is > 0, the evaluate result is structured and its children can be retrieved by passing variablesReference to the VariablesRequest.
  , namedVariablesEvaluateResponseBody     :: Maybe Int -- ^The number of named child variables. The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
  , indexedVariablesEvaluateResponseBody   :: Maybe Int -- ^The number of indexed child variables. The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
  } deriving (Show, Read, Eq)

-- |
--
defaultEvaluateResponseBody :: EvaluateResponseBody
defaultEvaluateResponseBody = EvaluateResponseBody {
    resultEvaluateResponseBody = ""
  , typeEvaluateResponseBody   = ""
  , presentationHintEvaluateResponseBody   = Nothing
  , variablesReferenceEvaluateResponseBody = 0
  , namedVariablesEvaluateResponseBody     = Nothing
  , indexedVariablesEvaluateResponseBody   = Nothing
  }


----------------------------------------------------------------------------
--  CompletionsRequest
----------------------------------------------------------------------------

-- |
--   CompletionsRequest request; value of command field is 'completions'.
--
--   Returns a list of possible completions for a given caret position and text.
--
--   The CompletionsRequest may only be called if the 'supportsCompletionsRequest' capability exists and is true.
--
data CompletionsRequest =
  CompletionsRequest {
    seqCompletionsRequest       :: Int                   -- ^Sequence number
  , typeCompletionsRequest      :: String                -- ^One of "request", "response", or "event"
  , commandCompletionsRequest   :: String                -- ^The command to execute
  , argumentsCompletionsRequest :: CompletionsRequestArguments  -- ^Arguments for "completions" request.
  } deriving (Show, Read, Eq)


-- |
--   Arguments for 'completions' request.
--
data CompletionsRequestArguments =
  CompletionsRequestArguments {
    frameIdCompletionsRequestArguments :: Maybe Int  -- ^Returns completions in the scope of this stack frame. If not specified, the completions are returned for the global scope.
  , textCompletionsRequestArguments :: String        -- ^One or more source lines. Typically this is the text a user has typed into the debug console before he asked for completion.
  , columnCompletionsRequestArguments :: Int         -- ^The character position for which to determine the completion proposals.
  , lineCompletionsRequestArguments :: Maybe Int     -- ^An optional line for which to determine the completion proposals. If missing the first line of the text is assumed.
  } deriving (Show, Read, Eq)


-- |
--  Response to 'completions' request.
--
data CompletionsResponse =
  CompletionsResponse {
    seqCompletionsResponse         :: Int     -- ^Sequence number
  , typeCompletionsResponse        :: String  -- ^One of "request", "response", or "event"
  , request_seqCompletionsResponse :: Int     -- ^Sequence number of the corresponding request
  , successCompletionsResponse     :: Bool    -- ^Outcome of the request
  , commandCompletionsResponse     :: String  -- ^The command requested
  , messageCompletionsResponse     :: String  -- ^Contains error message if success == false.
  , bodyCompletionsResponse        :: CompletionsResponseBody  -- ^The body of CompletionsResponse
  } deriving (Show, Read, Eq)


-- |
--
defaultCompletionsResponse :: CompletionsResponse
defaultCompletionsResponse = CompletionsResponse {
    seqCompletionsResponse         = 0
  , typeCompletionsResponse        = "response"
  , request_seqCompletionsResponse = 0
  , successCompletionsResponse     = False
  , commandCompletionsResponse     = "completions"
  , messageCompletionsResponse     = ""
  , bodyCompletionsResponse        = defaultCompletionsResponseBody
  }

-- |
--    Response to 'completions' request.
--
data CompletionsResponseBody =
  CompletionsResponseBody {
    targetsCompletionsResponseBody :: [CompletionsItem]  -- ^The possible completions for .
  } deriving (Show, Read, Eq)


-- |
--
defaultCompletionsResponseBody :: CompletionsResponseBody
defaultCompletionsResponseBody = CompletionsResponseBody []

-- |
--   CompletionItems are the suggestions returned from the CompletionsRequest.
--
data CompletionsItem =
  CompletionsItem {
    labelCompletionsItem  :: String  -- ^The label of this completion item. By default this is also the text that is inserted when selecting this completion.
  {-
  , textCompletionsItem :: String   -- If text is not falsy then it is inserted instead of the label.
  , typeCompletionsItem :: CompletionItemType  -- The item's type. Typically the client uses this information to render the item in the UI with an icon.
  , startCompletionsItem  :: Int     -- When a completion is selected it replaces 'length' characters starting at 'start' in the text passed to the CompletionsRequest.
  , lengthCompletionsItem :: Int    --If missing the frontend will try to determine these values heuristically.
  -}
  } deriving (Show, Read, Eq)


----------------------------------------------------------------------------
--  Event
----------------------------------------------------------------------------

-- |
--   Event message for "output" event type. The event indicates that the target has produced output.
--
data OutputEvent =
  OutputEvent {
    seqOutputEvent   :: Int     -- ^Sequence number
  , typeOutputEvent  :: String  -- ^One of "request", "response", or "event"
  , eventOutputEvent :: String  -- ^Type of event
  , bodyOutputEvent  :: OutputEventBody -- ^The body of OutputEvent
  } deriving (Show, Read, Eq)

-- |
--
defaultOutputEvent :: OutputEvent
defaultOutputEvent = OutputEvent 0 "event" "output" defaultOutputEventBody


-- |
--   Event message for "output" event type. The event indicates that the target has produced output.
--
data OutputEventBody =
  OutputEventBody {
    categoryOutputEventBody :: String        -- ^The category of output (such as: 'console', 'stdout', 'stderr', 'telemetry'). If not specified, 'console' is assumed.
  , outputOutputEventBody   :: String        -- ^The output to report.
  , dataOutputEventBody     :: Maybe String  -- ^Optional data to report. For the 'telemetry' category the data will be sent to telemetry, for the other categories the data is shown in JSON format.
  } deriving (Show, Read, Eq)

-- |
--
defaultOutputEventBody :: OutputEventBody
defaultOutputEventBody = OutputEventBody "console" "" Nothing


-- |
--   Server-initiated response to client request
--
data InitializedEvent =
  InitializedEvent {
    seqInitializedEvent   :: Int     -- ^Sequence number
  , typeInitializedEvent  :: String  -- ^One of "request", "response", or "event"
  , eventInitializedEvent :: String  -- ^Type of event
  } deriving (Show, Read, Eq)

-- |
--
defaultInitializedEvent :: InitializedEvent
defaultInitializedEvent = InitializedEvent 0 "event" "initialized"


-- |
--   Event message for "terminated" event types.
--
--   The event indicates that debugging of the debuggee has terminated.
--
data TerminatedEvent =
  TerminatedEvent {
    seqTerminatedEvent   :: Int     -- ^Sequence number
  , typeTerminatedEvent  :: String  -- ^One of "request", "response", or "event"
  , eventTerminatedEvent :: String  -- ^Type of event
  , bodyTerminatedEvent  :: TerminatedEventBody  -- ^The body of TerminatedEvent
  } deriving (Show, Read, Eq)


-- |
--
defaultTerminatedEvent :: TerminatedEvent
defaultTerminatedEvent = TerminatedEvent {
    seqTerminatedEvent = 0
  , typeTerminatedEvent = "event"
  , eventTerminatedEvent = "terminated"
  , bodyTerminatedEvent = defaultTerminatedEventBody
  }

-- |
--   Event message for "terminated" event types.
--
-- The event indicates that debugging of the debuggee has terminated.
--
data TerminatedEventBody =
  TerminatedEventBody {
    restartTerminatedEventBody :: Bool  -- ^A debug adapter may set 'restart' to true to request that the front end restarts the session.
  } deriving (Show, Read, Eq)


-- |
--
defaultTerminatedEventBody :: TerminatedEventBody
defaultTerminatedEventBody = TerminatedEventBody {
    restartTerminatedEventBody = False
  }


-- |
--   Event message for "exited" event types.
--
data ExitedEvent =
  ExitedEvent {
    seqExitedEvent   :: Int     -- ^Sequence number
  , typeExitedEvent  :: String  -- ^One of "request", "response", or "event"
  , eventExitedEvent :: String  -- ^Type of event
  , bodyExitedEvent  :: ExitedEventBody  -- ^The body of TerminatedEvent
  } deriving (Show, Read, Eq)


-- |
--
defaultExitedEvent :: ExitedEvent
defaultExitedEvent = ExitedEvent {
    seqExitedEvent = 0
  , typeExitedEvent = "event"
  , eventExitedEvent = "exited"
  , bodyExitedEvent = defaultExitedEventBody
  }

-- |
--   Event message for "exited" event types.
--
--   The exit code returned from the debuggee.
--
data ExitedEventBody =
  ExitedEventBody {
    exitCodeExitedEventBody :: Int
  } deriving (Show, Read, Eq)


-- |
--
defaultExitedEventBody :: ExitedEventBody
defaultExitedEventBody = ExitedEventBody {
    exitCodeExitedEventBody = -1
  }



-- |
--   Event message for 'continued' event type.
--
--		The event indicates that the execution of the debuggee has continued.
--
--		Please note: a debug adapter is not expected to send this event in response to a request that implies that execution continues, e.g. 'launch' or 'continue'.
--
--		It is only necessary to send a 'continued' event if there was no previous request that implied this.
--
data ContinuedEvent =
  ContinuedEvent {
    seqContinuedEvent   :: Int     -- ^Sequence number
  , typeContinuedEvent  :: String  -- ^One of "request", "response", or "event"
  , eventContinuedEvent :: String  -- ^Type of event
  , bodyContinuedEvent  :: ContinuedEventBody -- ^The body of ContinuedEvent
  } deriving (Show, Read, Eq)


-- |
--
defaultContinuedEvent :: ContinuedEvent
defaultContinuedEvent = ContinuedEvent {
    seqContinuedEvent = 0
  , typeContinuedEvent = "event"
  , eventContinuedEvent = "continued"
  , bodyContinuedEvent = defaultContinuedEventBody
  }

-- |
--   Body of ContinuedEvent
--
data ContinuedEventBody =
  ContinuedEventBody {
    threadIdContinuedEventBody :: Int              -- ^The thread which was continued.
  , allThreadsContinuedContinuedEventBody :: Bool  -- ^If 'allThreadsContinued' is true, a debug adapter can announce that all threads have continued.
  } deriving (Show, Read, Eq)


-- |
--
defaultContinuedEventBody :: ContinuedEventBody
defaultContinuedEventBody = ContinuedEventBody {
    threadIdContinuedEventBody = 0
  , allThreadsContinuedContinuedEventBody = True
  }



-- |
--   Event message for "stopped" event type.
--
--   The event indicates that the execution of the debuggee has stopped due to some condition.
--
--   This can be caused by a break point previously set, a stepping action has completed, by executing a debugger statement etc.
--
data StoppedEvent =
  StoppedEvent {
    seqStoppedEvent   :: Int     -- ^Sequence number
  , typeStoppedEvent  :: String  -- ^One of "request", "response", or "event"
  , eventStoppedEvent :: String  -- ^Type of event
  , bodyStoppedEvent  :: StoppedEventBody -- ^The body of StoppedEvent
  } deriving (Show, Read, Eq)


-- |
--
defaultStoppedEvent :: StoppedEvent
defaultStoppedEvent = StoppedEvent {
    seqStoppedEvent = 0
  , typeStoppedEvent = "event"
  , eventStoppedEvent = "stopped"
  , bodyStoppedEvent = defaultStoppedEventBody
  }


-- |
--   Event message for 'stopped' event type.
--
--   The event indicates that the execution of the debuggee has stopped due to some condition.
--
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

