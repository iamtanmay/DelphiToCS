{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------- --------  ----------------------------------------------------------
  06.11.08 pk                                        TN4280    ThreadManager changed to ThreadAPI
  07.04.09 pk                                        TN4503    ThreadStarted/Stopped removed
  14.04.09 pk Init                                   TN4523    New
  09.03.10 pk                                        TN5015    Massive Changes in ThreadClasses
  16.11.10 wl  ConnectIfNecessary                    TN5341    Wenn es ein UserProtection- oder anderes Device gibt, wird ConnectModules durchgeführt
  19.11.10 wl  InterruptStart                        TN5358    bei existierender UserProtection wird DeactivateModules aufgerufen
  19.11.10 wl  InterruptFinish                       TN5358    bei existierender UserProtection wird ActivateModules aufgerufen
  08.12.10 pk  fIsUserProtectionActivated            TN5383    fIsUserProtectionActivated - in InterruptFinish only try to turn on system state if it was on to begin with
  14.12.11 wl                                        TN5765   uses geändert
  27.12.11 wl                                        TN5768   an geändertes TRunGlobals angepasst
  17.10.12 ts                                        TN5993   UserProtection als property, damit InterruptMonitor in ExecHandler beendet werden kann
  18.10.12 ts  evEndMethodOrScript                   TN5995   ProtectionPoll für Flush korrekt beenden
  26.07.13 wl                                        TN6160   an IStateSignalDevice-Änderungen angepasst
  -----------------------------------------------------------------------------------------------------------
  SystemEventsRunner:
  30.07.13 wl                                        TN6160   neue Struktur, beinhaltet ObjSystemState
  31.07.13 ts   ModalMessageStart/Finish             TN6160.1 wird nur geschaltet wenn vorher aktiv bzw. InterruptMessage
  15.08.13 wl                                        TN6223   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit SystemEventsRunner;


interface


uses
    SystemEvents,
    IntfStateSignalDevice,
    DllCall,
    AppTypes,
    IntfUserProtectionDevice,
    IntfSoftwareProtectionDevice;

type
    TStateSignalDll = class
    private const
        STR_DLLFUNC_STATEACTIVE = 'SetStateActive';
        STR_DLLFUNC_STATESETUP = 'SetStateSetup';
        STR_DLLFUNC_STATEERROR = 'SetStateError';
    strict private
        fDllHandle: TDllHandle;
        fActiveFunc, fSetupFunc, fErrorFunc: TDllFunc;
        function GetDllFunc(aState: TSystemState): TDllFunc;
    public
        constructor Create;
        destructor Destroy; override;
        procedure CallDllFunction(aState: TSystemState; aMessage: string);
    end;

    TSystemEventsRunner = class(TSystemEvents)
    private
        fUserProtection: IUserProtectionDevice;
        fIsUserProtectionActivated: boolean;
        fSoftwareProtection: ISoftwareProtectionDevice;
        FMethodOrScriptRunning: boolean;

        fStateSignalDll: TStateSignalDll;

        class function InitUserProtection: IUserProtectionDevice;
    protected
        procedure CallDllFunction(aState: TSystemState; aMessage: string); override;
        procedure InitDevices(); override;
    public
        constructor Create;
        destructor Destroy; override;

        procedure MethodStart(); override;
        procedure MethodFinish(); override;

        procedure InterruptStart(const aReason: string); override;
        procedure InterruptFinish(); override;

        procedure ModalMessageStart(); override;
        procedure ModalMessageFinish(); override;

        procedure ModalDllMessageStart(); override;
        procedure ModalDllMessageFinish(); override;

        procedure QuitProtectionPoll(); override;
    end;


implementation


uses
    DeviceManager,
    CommonTypes,
    ThrdMan,
    LogManager,
    AppSettings,
    ObjModul,
    ObjModulA,
    RunGlobals,
    ErrorManager,
    ThreadAPI,
    InterruptMonitor,
    SysUtils;

{ TStateSignalDll }

constructor TStateSignalDll.Create;
var
    xIniAccess: IWinlissyIniAccess;
    xDllName: string;
begin
    inherited Create;

    xIniAccess := gCommonDll.CreateRobotIni;
    fDllHandle := nil;
    fActiveFunc := nil;
    fSetupFunc := nil;
    fErrorFunc := nil;
    xDLLName := xIniAccess.ReadString(STR_ISEC_STATEMONITOR, 'DLLName');
    if xDllName <> '' then
    begin
        fDllHandle := TDllCall.CreateDllHandle(xDllName, true, TDllInterfaceVersion.Version8);
        fActiveFunc := TDllCall.CreateFuncWithHandle(fDllHandle, false, STR_DLLFUNC_STATEACTIVE, '', 0);
        fSetupFunc := TDllCall.CreateFuncWithHandle(fDllHandle, false, STR_DLLFUNC_STATESETUP, '', 0);
        fErrorFunc := TDllCall.CreateFuncWithHandle(fDllHandle, false, STR_DLLFUNC_STATEERROR, '', 0);
    end;
end;

destructor TStateSignalDll.Destroy;
begin
    fActiveFunc.Free;
    fSetupFunc.Free;
    fErrorFunc.Free;
    fDllHandle.Free;

    inherited;
end;

procedure TStateSignalDll.CallDllFunction(aState: TSystemState; aMessage: string);
var
    xFunc: TDllFunc;
begin
    if Assigned(fDllHandle) then
    begin
        xFunc := GetDllFunc(aState);
        if Assigned(xFunc) then
            xFunc.CallFuncWithParams(aMessage);
    end;
end;

function TStateSignalDll.GetDllFunc(aState: TSystemState): TDllFunc;
begin
    case aState of
        sStateActive:
            result := fActiveFunc;
        sStateReady:
            result := fSetupFunc;
        sStateInterruptError, sStateInterruptMessage:
            result := fErrorFunc;
        else
            result := nil;
    end;
end;

{ TSystemEventsRunner }

constructor TSystemEventsRunner.Create;
begin
    inherited Create;
    fIsUserProtectionActivated := false;

    // Create User Protection
    fUserProtection := self.InitUserProtection();

    fSoftwareProtection := gModules.FindSoftwareProtectionDevice();
    if Assigned(fSoftwareProtection) then
        fSoftwareProtection.OnCreateThread := TThreadAPI.CreateThread;

    fStateSignalDll := TStateSignalDll.Create;
end;

destructor TSystemEventsRunner.Destroy;
begin
    FreeAndNil(fStateSignalDll);

    inherited Destroy;
end;

procedure TSystemEventsRunner.InitDevices;
begin
    inherited;

    if Assigned(fUserProtection) then
        TActionModules.ConnectDevice(fUserProtection);
    if Assigned(fSoftwareProtection) then
        TActionModules.ConnectDevice(fSoftwareProtection);
end;

class function TSystemEventsRunner.InitUserProtection: IUserProtectionDevice;
begin
    result := nil;
    if not gDeviceManager.FindFirst(IUserProtectionDevice, result) then
        EXIT;

    ASSERT(TThreadManagerSetup.Instance <> nil, 'ThrMan not assigned');
    result.OnCreateProtectionPoll := TThreadManagerSetup.Instance.CreateInterruptMonitor;
end;

procedure TSystemEventsRunner.CallDllFunction(aState: TSystemState; aMessage: string);
begin
    fStateSignalDll.CallDllFunction(aState, aMessage);
end;

procedure TSystemEventsRunner.MethodStart();
begin
    // Ein Connect wurde an dieser Stelle noch nicht unbedigt durchgeführt
    InitDevices();

    if Assigned(fUserProtection) then
    begin
        fUserProtection.ProtectionOn;
        fIsUserProtectionActivated := true;
    end;

    if Assigned(fSoftwareProtection) then
        fSoftwareProtection.ProtectionOn();

    SetState(sStateActive, 'Run ' + TRunGlobals.Instance.PMethodName + ' started!');
    FMethodOrScriptRunning := true;
end;

procedure TSystemEventsRunner.MethodFinish();
begin
    SetState(sStateReady, 'Run ' + TRunGlobals.Instance.PMethodName + ' ended!');
    if not FMethodOrScriptRunning then
        exit;
    if Assigned(fUserProtection) then
    begin
        if Assigned(fUserProtection.ProtectionPoll) then
        begin
            try
                fUserProtection.ProtectionPoll.Quit;
                Sleep(fUserProtection.ProtectionPollDelay);
                fUserProtection.ProtectionPoll := nil;
            except
                fUserProtection.ProtectionPoll := nil;
                // thread wurde bereits beendet (vor Methodenende)
            end;
        end;
        fUserProtection.ProtectionOff;
        fIsUserProtectionActivated := false;
    end;

    if Assigned(fSoftwareProtection) then
        fSoftwareProtection.ProtectionOff();
    // global.CloseZipArchive;
    FMethodOrScriptRunning := false;
end;

procedure TSystemEventsRunner.InterruptStart(const aReason: string);
begin
    SetState(sStateInterruptError, 'Run ' + TRunGlobals.Instance.PMethodName + ' stopped.  Reason: '
        + aReason);
    if Assigned(fUserProtection) then
    begin
        if fIsUserProtectionActivated then
            fUserProtection.ProtectionOff;

        // Durch die Userprotection könne manche Module per SPS deaktiviert worden sein
        // Mit dieser Funktion kann man den Status des Moduls auf deaktiviert setzen
        TActionModules.DeactivateModules();
    end;
end;

procedure TSystemEventsRunner.QuitProtectionPoll;
begin
    if Assigned(fUserProtection) and Assigned(fUserProtection.ProtectionPoll) and
        not fUserProtection.ProtectionPoll.Terminated then
        fUserProtection.ProtectionPoll.Quit;
end;

procedure TSystemEventsRunner.InterruptFinish();
begin
    if (not gErrorManager.IsGlobalErr) then
    begin
        if Assigned(fUserProtection) then
        begin
            // exit if user protection could not be turned on
            // do this before systemstatemanager.setstate because we dont want to turn on the doorclose
            // magnets if the door is still open
            // Only turn it on if it was turned on in evStartMethodOrScript
            if fIsUserProtectionActivated then
                fUserProtection.ProtectionOn;

            // Durch die Userprotection könne manche Module per SPS deaktiviert worden sein
            // Diese müssen nun wieder aktiviert werden!
            TActionModules.ActivateModules();
        end;

        SetState(sStateActive, 'State: Run ' + TRunGlobals.Instance.PMethodName + ' resumed by user!');
    end
    else
    begin
        SetState(sStateReady, 'State: Run ' + TRunGlobals.Instance.PMethodName + ' terminated by user!');
    end;
end;

procedure TSystemEventsRunner.ModalMessageStart;
begin
    if (self.CurrentState = sStateActive) then // Fehler geht vor (nur schalten, wenn active)
    begin
        SetState(sStateInterruptMessage, 'Run ' + TRunGlobals.Instance.PMethodName +
            ' paused. Interaction needed.');
    end;
end;

procedure TSystemEventsRunner.ModalMessageFinish;
begin
    if (self.CurrentState = sStateInterruptMessage) then
    // Fehler geht vor (nur setzen, wenn InterruptMessage war)
    begin
        if (not gErrorManager.IsGlobalErr) then
            SetState(sStateActive, 'State: Run ' + TRunGlobals.Instance.PMethodName + ' unpaused!');
    end;
end;

procedure TSystemEventsRunner.ModalDllMessageStart;
begin
    ModalMessageStart;

    if Assigned(fUserProtection) then
        fUserProtection.ProtectionPause;
end;

procedure TSystemEventsRunner.ModalDllMessageFinish;
begin
    ModalMessageFinish;

    if Assigned(fUserProtection) then
        fUserProtection.ProtectionUnpause;
end;


end.
