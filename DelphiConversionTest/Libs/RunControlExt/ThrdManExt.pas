{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : TThreadManagerExt: Thread-Manager that deals with Scheduler-Threads
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  14.11.02 wl                               TN1293.2 moved here from ThrdMan.pas
  27.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  12.03.03 wl                               TN1293.5 uses posTools
  24.07.03 wl TThreadManagerExt muß von TSamplerThrMan abgeleitet werden, sonst gehen Funktionen verloren
  08.12.03 pk                               TN1697   various changes
  04.02.04 pk  TSamplerThrMan               TN1719 ThreadIsStarted/Stopped changed to accept TThread instead of TRunThread
  23.02.04 pk                               TN1719   gGlobalEvents renamed to gSystemEvents
  28.06.04 pk  UserInterrupt                TN2009.7 uses VortReqThread functions from DevicesVortex
  21.07.04 pk  SessionEndedEvent            TN2049   call TerminateVortReqThread and evEndMethodOrScript
  21.07.04 pk  CreateSession                TN2049   create Script or RunSessionThread based on DatasourceType
  06.01.05 wl  TThreadManagerRun            TN2246.4 entspricht TSamplerThrMan (Objects)
  07.01.05 pk                               TN2281.0 fSchedSessionThread changed to fSchedSessionManager + fSamplerThread
  12.01.05 pk  UserInterrupt                TN2281   Use emptyArg instead of null
  12.01.05 pk  UserInterrupt                TN2281   Use TMsgArgs.EmptyArgs
  18.01.05 pk  OnApplicationInterrupt       TN2281.0 callback called when Global error is set.
  27.01.05 pk  UserInterrupt                TN2281.2 register INT_MESSAGE_SESSION_SUSPEND/RESUME messages
  15.02.05 pk  CreateSession                TN2314.1 Create SchedMaker and pass as argument to SchedSessionRunner
  04.03.05 pk                               TN2330.1 TSchedMaker.Create no longer requires DatasourceType
  11.03.05 pk                               TN2339.2 passes GUIManager to TSchedThread.Create
  18.03.05 pk  CreateThreadRegistry         TN2352.1 Create TThreadRegistryRun
  18.03.05 pk  GetCurrentCallStack          TN2352.1 New : lookup currentthreadID in Registry and return the callstack
  31.03.05 pk                               TN2362   Session-related code moved to LisGlobe and TExecHanlder
  06.04.05 pk  RegisterMessage              TN2374   SeparateThread parameter changed from true to false
  19.04.05 pk  TThreadManagerExt            TN2392   Removed - Some code moved to TSessionExecHandler
  07.11.05 pk                               TN2737   UserInterrupt : Dialog shown via GUIManager
  08.11.05 wl                               TN2745   uses Schedule entfernt
  24.11.05 pk                               TN2805    ObjSampl replaced by ZARunnerObjects
  22.12.05 pk                               TN2875   Various changes for Interrupt handling
  23.01.06 pk  StandardInterruptRoutine     TN2894   Reset the keystate by cally GetAsynKeyState
  26.01.06 pk  HandleInterrupt              TN2904   do not check global error - causes zp02 to hang
  06.04.06 pk  CreateThreadAdvanced         TN3001   New
  18.04.06 pk  GetCurrentDatabaseSession    TN3001   Name the session using the priority number
  29.08.06 thr                              TN3264   Vortexerthread eingebunden
  09.11.06 pk  HandleInterrupt              TN3399   some code moved to StopRunInterruptRoutine
  13.11.06 pk  HandleInterrupt              TN3401   assertion check for interruptitem function pointer moved to TInterruptRequestItem
  21.11.06 pk  HandleInterrupt              TN3424   Now checks globalerr
  01.12.06 pk  StopRunInterruptRoutine      TN2441   system events moved to HandleInterrupt
  07.12.06 pk  TThreadRegistryRun           TN3455   inherits from TThreadRegistrySetup
  22.02.07 pk  OnThreadFinish               TN3583   New: Tell main thread to Free threads after thread is finished
  22.02.07 pk  CreateRegistryItem           TN3583   with new ownsthread parameter
  03.07.08 wl                               TN4157
  21.07.08 pk  GetSupervisorThreadTypes     TN4175   thrBalance added
  08.09.08 pk                               TN4215   various changes
  20.09.08 pk  CurrentRootDataPath          TN4215   New property
  06.11.08 pk  TThreadingProcessHandlerRun  TN4280   New
  17.11.08 pk                               TN4280   Unused code removed
  17.11.08 pk  TThreadRegistryRun           TN4280   removed
  19.11.08 pk  HandleInterrupt              TN4280   moved back to TThreadManagerRun
  09.06.09 pk  InterruptStart/Finish        TN4585.1 New
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  09.03.10 pk                               TN5015   ThreadmanagerSetup is no longer inherited from ThreadManager
  23.04.10 pk                               TN5015   remove SamDDE
  13.09.11 ts  HandleInterrupt              TN5523   ESC-Window nur anzeigen, wenn result von DoInterruptStart true
  19.01.12 wl  HandleInterrupt              TN5759.2 Parameter MaxWaitTime wird auf 2000 gesetzt, wenn IgnoreInterruptStartError
  19.01.12 wl  HandleInterrupt              TN5759.2 DDE-Zeilen wieder deaktiviert
  20.04.12 wl  RequestStopRunInterrupt      TN5858   neu implementiert: entspricht der Pause-Taste
  04.05.12 wl                               TN5858   Animation endgültig endfernt
  09.05.12 wl  RequestStopRunInterrupt      TN5858   Inhalt --> GUIManagerRunner
  14.11.12 wl                               TN5858   uses geändert
  30.07.13 wl                               TN6160   TSystemEvents: geänderte Aufrufe
  15.08.13 wl                               TN6223   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit ThrdManExt;


interface


uses
    Classes,
    Controls,
    SysUtils,
    ThrdMan,
    InterruptManager;

type
    TServicesProcessHandlerRun = class(TServicesProcessHandler)
    protected
        procedure Initialize(); override;
    end;

    TThreadManagerRun = class(TThreadManagerSetup)
    private
        function GetInterruptDescription(const aInterruptorThreadID: cardinal;
            const aInterruptText: string): string;
    protected
        procedure CreateServicesProcessHandler(); override;
        procedure HandleInterrupt(aInterruptItem: TInterruptRequest); override;
        function DoInterruptFinish(const aInterruptorThreadID: cardinal; const aInterruptText: string)
            : boolean; override;
        function DoInterruptStart(const aInterruptorThreadID: cardinal; const aInterruptText: string;
            const aInterruptMaxWaitTime: cardinal): boolean; override;
    public
        destructor Destroy(); override;
        procedure RequestStopRunInterrupt(const aInterruptText: string; aWaitTillHandled: boolean); override;

        class function Instance(): TThreadManagerRun;
    end;


implementation


uses
    AppTypes,
    LogManager,
    ErrorManager,
    CommonTypes,
    AppSettings,
    SystemEvents,
    ThreadAPI,
    GUIManagerRun,
    ResourceManager,
    ResourceManagerRun;

{ TThreadManagerRun }

procedure TThreadManagerRun.RequestStopRunInterrupt(const aInterruptText: string; aWaitTillHandled: boolean);
begin
    TGUIManagerRun.Instance.RequestAndStateSetAbort;
end;

class function TThreadManagerRun.Instance: TThreadManagerRun;
begin
    result := ThrMan as TThreadManagerRun;
end;

function TThreadManagerRun.GetInterruptDescription(const aInterruptorThreadID: cardinal;
    const aInterruptText: string): string;
begin
    result := Format('Handle interrupt: %s from thread: %d', [aInterruptText, aInterruptorThreadID]);
end;

procedure TThreadManagerRun.HandleInterrupt(aInterruptItem: TInterruptRequest);
// This function is called in the context of the interrupt manager thread
var
    xDoInterrupt: boolean;
    xMaxWaitTimeBeforeStart: cardinal;
begin
    // spezielle Behandlung für Fehler, die aus ZP02 kommen
    if aInterruptItem.IgnoreInterruptStartError then
        xMaxWaitTimeBeforeStart := 2000
    else
        xMaxWaitTimeBeforeStart := INFINITE;

    xDoInterrupt := DoInterruptStart(aInterruptItem.InterruptorThreadID, aInterruptItem.InterruptText,
        xMaxWaitTimeBeforeStart);
    if not xDoInterrupt then
        EXIT;

    aInterruptItem.HandleInterrupt();

    DoInterruptFinish(aInterruptItem.InterruptorThreadID, aInterruptItem.InterruptText);
end;

function TThreadManagerRun.DoInterruptStart(const aInterruptorThreadID: cardinal;
    const aInterruptText: string; const aInterruptMaxWaitTime: cardinal): boolean;
var
    xDescription: string;
begin
    result := false;
    xDescription := GetInterruptDescription(aInterruptorThreadID, aInterruptText);

    if gErrorManager.IsGlobalErr or (not SamThreadRunning(false)) then
    begin
        gLogManager.LogF('%s - interrupt ignored', [xDescription], false);
        EXIT;
    end;

    gLogManager.LogF('%s - BEGIN', [xDescription], false);
    SuspendExec(aInterruptMaxWaitTime);

    // SendDDEStatus(ddeStopButton);

    TSystemEvents.Instance.InterruptStart(xDescription);

    result := true;
end;

procedure TThreadManagerRun.CreateServicesProcessHandler;
begin
    fServicesProcessHandler := TServicesProcessHandlerRun.Create();

end;

destructor TThreadManagerRun.Destroy;
begin
    gResourceManager.Terminate();
    gResourceManager := nil;

    inherited;
end;

function TThreadManagerRun.DoInterruptFinish(const aInterruptorThreadID: cardinal;
    const aInterruptText: string): boolean;
var
    xDescription: string;
begin
    xDescription := GetInterruptDescription(aInterruptorThreadID, aInterruptText);

    TSystemEvents.Instance.InterruptFinish();

    // SendDDEStatus(ddeRunning);

    ResumeExec();

    gLogManager.LogF('%s - END', [xDescription], false);

    result := true;
end;

procedure TServicesProcessHandlerRun.Initialize();
begin
    inherited;
    gResourceManager := TResourceManagerRun.Create;
    // --------------------------------------------------------- Resource Manager Thread
    TThreadAPI.CreateThread('Resource Manager', false, true, false, gResourceManager);
    gResourceManager.Unpause();
end;


end.
