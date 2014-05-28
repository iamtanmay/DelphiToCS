{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  06.07.09 pk                                        TN4585.4  New: Create/DestroyObject
  30.07.09 pk                                        TN4585.5    Various Changes
  19.10.10 pk                                        TN5305    New AliasNameExists,etc
  19.02.12 pk                                        TN5809.1  Changes for ZACoreClientX dll
  20.04.12 wl  cControlEventIDStopPromptModal        TN5946   entfernt
  15.08.13 wl                                        TN6109   RequestStartableMethods vorbereitet
  11.03.14 tp  cCoreControllerCallIDRequestMethodLayouts, cCoreControllerCallIDRequestMethodIcons,
  cCoreControllerCallIDRequestBuildingBlockBools, cCoreControllerCallIDRequestEditableBools,
  cCoreControllerCallIDRequestMethodParameters, cCoreControllerCallIDRequestAllLayouts,
  cCoreControllerCallIDCreateMethod, cCoreControllerCallIDSQLWriteQuery, cCoreControllerCallIDSQLReadQuery,
  cCoreControllerCallIDVerifierQuery                 TN6375   C# GUI commands added
  ----------------------------------------------------------------------------------------------------------------------- }

unit RemoteMessageConstants;


interface


const
    cCoreCallIDCreateObject = 'CreateObject';
    cCoreCallIDDestroyObject = 'DestroyObject';

    cCoreCallIDGetError = 'GetError';
    cCoreCallIDCloseConnection = 'CloseConnection';
    cCoreCallIDCheckConnection = 'CheckConnection';
    cCoreCallIDRegisterController = 'RegisterController';
    cCoreCallIDUnRegisterController = 'UnRegisterController';
    cCoreCallIDRegisterEventHost = 'RegisterEventHost';
    cCoreCallIDUnRegisterEventHost = 'UnRegisterEventHost';
    cCoreCallIDRegisterRunEventHost = 'RegisterRunEventHost';
    cCoreCallIDIsEventHostRegistered = 'IsEventHostRegistered';

    cCoreCallIDAliasNameExists = 'AliasNameExists';
    cCoreCallIDGetAliasPath = 'GetAliasPath';
    cCoreCallIDGetAllAliasNames = 'GetAllAliasNames';
    cCoreCallIDGetMainDBAlias = 'GetMainDBAlias';

    cCoreCallIDCreateDataConnectionParams = 'CreateDataConnectionParams';
    cCoreCallIDCreateConnectionParamsByAlias = 'CreateConnectionParamsByAlias';

    cCoreCallIDGetNewClientRunID = 'GetNewClientRunID';
    cCoreCallIDAddClientToRun = 'AddClientToRun';
    cCoreCallIDRemoveClientFromRun = 'RemoveClientFromRun';
    cCoreCallIDGetClientRunIDsAtAddress = 'GetClientRunIDsAtAddress';

    cCoreControllerCallIDSetControlEventMask = 'SetControlEventMask';
    cCoreControllerCallIDStartMethod = 'StartMethod';
    cCoreControllerCallIDSimulateMethod = 'SimulateMethod';
    cCoreControllerCallIDRequestStartableMethods = 'RequestStartableMethods';

    cCoreControllerCallIDRequestMethodLayouts = 'RequestStartableMethodLayouts';
    cCoreControllerCallIDRequestMethodIcons = 'RequestStartableMethodIcons';
    cCoreControllerCallIDRequestBuildingBlockBools = 'RequestStartableBuildingBlockBools';
    cCoreControllerCallIDRequestEditableBools = 'RequestStartableEditableBools';
    cCoreControllerCallIDRequestMethodParameters = 'RequestMethodParameters';
    cCoreControllerCallIDRequestAllLayouts = 'RequestAllLayouts';
    cCoreControllerCallIDCreateMethod = 'CreateMethod';
    cCoreControllerCallIDSQLWriteQuery = 'SQLWrite';
    cCoreControllerCallIDSQLReadQuery = 'SQLRead';

    cCoreControllerCallIDVerifierQuery = 'CreateVerifierShutdownReset';

    cCoreControllerCallIDInterruptStart = 'InterruptStart';
    cCoreControllerCallIDInterruptFinish = 'InterruptFinish';
    cCoreControllerCallIDSetGlobalError = 'SetGlobalError';
    cCoreControllerCallIDAskStatus = 'AskStatus';
    cCoreControllerCallIDGetPendingControlEventID = 'GetPendingControlEventID';
    cCoreControllerCallIDUserStopInterrupt = 'UserStopInterrupt';

    cCoreControllerCallIDBaseEvent = '';

    cCoreControllerCallIDGetErrorBoxEvent = cCoreControllerCallIDBaseEvent + 'GetErrorBoxEvent';
    cCoreControllerCallIDSetErrorBoxEventHandled = cCoreControllerCallIDBaseEvent + 'SetErrorBoxEventHandled';

    cCoreControllerCallIDGetMessageBoxEvent = cCoreControllerCallIDBaseEvent + 'GetMessageBoxEvent';
    cCoreControllerCallIDSetMessageBoxEventHandled = cCoreControllerCallIDBaseEvent +
        'SetMessageBoxEventHandled';

    cCoreControllerCallIDGetAskRunStartEvent = cCoreControllerCallIDBaseEvent + 'GetAskRunStartEvent';
    cCoreControllerCallIDSetAskRunStartEventHandled = cCoreControllerCallIDBaseEvent +
        'SetAskRunStartEventHandled';

    cCoreControllerCallIDGetLoadDisplayComponentEvent = cCoreControllerCallIDBaseEvent +
        'GetLoadDisplayComponentEvent';
    cCoreControllerCallIDSetLoadDisplayComponentEventHandled = cCoreControllerCallIDBaseEvent +
        'SetLoadDisplayComponentEventHandled';

    cCoreControllerCallIDGetProcessStartedEvent = cCoreControllerCallIDBaseEvent + 'GetProcessStartedEvent';
    cCoreControllerCallIDSetProcessStartedEventHandled = cCoreControllerCallIDBaseEvent +
        'SetProcessStartedEventHandled';

    cCoreControllerCallIDGetProcessFinishedEvent = cCoreControllerCallIDBaseEvent + 'GetProcessFinishedEvent';
    cCoreControllerCallIDSetProcessFinishedEventHandled = cCoreControllerCallIDBaseEvent +
        'SetProcessFinishedEventHandled';

    cCoreEventCallIDControlEventPending = 'ControlEventPending'; // 2001;

    cCoreStatusEventCallIDEventBase = 'CoreStatusEvent';
    cCoreStatusEventCallIDWriteLog = cCoreStatusEventCallIDEventBase + 'WriteLog';
    cCoreStatusEventCallIDLoadDisplayComponent = cCoreStatusEventCallIDEventBase + 'LoadDisplayComponent';
    cCoreStatusEventCallIDLinkRunIDToProcess = cCoreStatusEventCallIDEventBase + 'LinkRunIDToProcess';

    cCoreErrorGeneral = 1;

    cControlEventIDNone = 0;
    cControlEventIDBase = 10000;
    cControlEventIDErrorBox = cControlEventIDBase + 1;
    cControlEventIDMessageBox = cControlEventIDBase + 2;
    // cControlEventIDStopPromptModal = cControlEventIDBase + 3;
    cControlEventIDAskRunStart = cControlEventIDBase + 4;
    // cControlEventIDLoadDisplayComponent = cControlEventIDBase + 4;
    cControlEventIDProcessStarted = cControlEventIDBase + 10;
    cControlEventIDProcessFinished = cControlEventIDBase + 11;


implementation


end.
