{ --------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  13.08.09 wl                               TN4585.5 Initial revision
  27.08.09 pk  LaunchRunClient              TN4753   some code commented out for now
  27.05.10 wl                               TN5116   enthält neue Methoden für ZARunner/ZADesigner
  28.05.10 wl  RegisterController           TN5116   spezielle Methoden für RunClient
  04.06.10 wl                               TN5116   mit neuen Funktionen für Runner
  18.06.10 wl                               TN5116   Moss, Cherry Picking und Run Table-Funktionen entfernt
  19.10.10 pk                               TN5305   changes needed for CoreClient/Server
  14.12.11 wl                               TN5765   GetMethDlg ohne Session
  20.04.12 wl  InterruptStart,InterruptFinish,SetGlobalError  TN5858  kapselt die TCoreControllerClientCalls-Methoden
  04.05.12 wl  SetAnimationImage            TN5858   Animation endgültig endfernt
  07.08.12 wl  StartFlushMethod             TN5946   Parameter geändert
  03.09.13 wl  StartFlushMethod             TN6238   neu: DryAfterWash
  -------------------------------------------------------------------------------------------------- }

unit EdExternClient;


interface


uses
    Classes,
    Buttons,
    Controls,
    ExtCtrls,
    GeneralTypes,
    EdExtern,
    AppTypes,
    IntfArmDevice,
    CoreControllerClientCalls;

type
    TEdExternClient = class(TEdExtern)
    private
        fCurrentRunID: string;
        fMultiRunsAllowed: boolean;
        fRunIDs: TStringList;
        function LaunchRunClient(const aRunID: string): boolean;
        function VerifyRunClient(out oRunID: string): boolean;
        function GetRunIDWithCheck(): string;
        function WaitTillRunClientLoaded(const aRunID: string): boolean;
        procedure RegisterController(const aRunID: string);
        class function ContainsRunID(const aRunIDs: TStringArray; const aSearchRunID: string): boolean;
    protected
        function GetCurrentMethodName: string; override;
    public
        constructor Create();
        destructor Destroy; override;

        function MethodStart(const aMethodName: string; const aDontShowMessages, aSimulate: boolean)
            : boolean; override;

        // hier gibt es noch zu tun:
        function ExecuteDllCall(const aDLLName, aDLLFunction, aDLLParameter: string): string; override;
        procedure StartFlushMethod(const aPipDeviceName: string; aTipMap: TIPMAP; aAddDilutorMap: TIPMAP;
            const aDiluentNames: TArray<string>; aVolume, aCycles: integer; aUseCh1, aUseCh2: boolean;
            aUsePeriPump, aUseAddDilutors: boolean; aDryAfterWash: boolean); override;
        function ThreadIsRunning(aShowMsg: boolean): Boolean; override;
        procedure UserInterrupt; override;
        function SetCurrentLayoutName(const aDialogText: string): boolean; override;
        procedure InitSystem(); override;
        function ReloadValues: boolean; override;
        procedure StartMethod(aAlwaysSelect: boolean; const aDefaultMethod: string); override;
        procedure StartCurrentMethod(); override;
        procedure SetOnSafeAppClose(const aValue: TNotifyEvent); override;
        function AnyProcessesRunning(): boolean; override;
        procedure RequestSafeAppClose(const aMaxWaitTime: cardinal); override;
        procedure ResetGlobalName(); override;
        function InterruptStart(const aInterruptText: string): boolean; override;
        function InterruptFinish(const aInterruptText: string): boolean; override;
        procedure SetGlobalError(const aErrorText: string); override;
        // nur für RunClient:
        procedure RegisterControllerIfNew(const aRunID: string); override;
        procedure RegisterControllerForAllRunIDs(); override;
    end;


implementation


uses
    SysUtils,
    LogManager,
    AppInstanceCoreClientLib,
    GetMeth;

{ TEdExternClient }

constructor TEdExternClient.Create;
begin
    inherited Create();
    fMultiRunsAllowed := false;
    fRunIDs := TStringList.Create();
end;

destructor TEdExternClient.Destroy();
begin
    fRunIDs.Free;
    inherited;
end;

function TEdExternClient.ExecuteDllCall(const aDLLName, aDLLFunction, aDLLParameter: string): string;
begin
    result := '';
    { TODO -owl : DllCall integrieren }
end;

procedure TEdExternClient.StartFlushMethod(const aPipDeviceName: string; aTipMap: TIPMAP;
    aAddDilutorMap: TIPMAP; const aDiluentNames: TArray<string>; aVolume, aCycles: integer;
    aUseCh1, aUseCh2: boolean; aUsePeriPump, aUseAddDilutors: boolean; aDryAfterWash: boolean);
begin
    { TODO -owl : integrieren }
end;

function TEdExternClient.AnyProcessesRunning: boolean;
begin
    { TODO -owl : integrieren }
    result := false;
end;

class function TEdExternClient.ContainsRunID(const aRunIDs: TStringArray; const aSearchRunID: string)
    : boolean;
var
    x: integer;
begin
    result := false;
    for x := 0 to high(aRunIDs) do
    begin
        if SameText(aSearchRunID, aRunIDs[x]) then
        begin
            result := true;
            EXIT;
        end;
    end;
end;

function TEdExternClient.WaitTillRunClientLoaded(const aRunID: string): boolean;
const
    cMaxTries = 10;
    cSleepTime = 1000;
var
    xRunIDs: TStringArray;
    x: integer;
begin
    result := true;
    Sleep(cSleepTime);

    for x := 1 to cMaxTries do
    begin
        xRunIDs := TCoreClient.Instance.GetClientRunIDsAtAddress('', true);
        if ContainsRunID(xRunIDs, aRunID) then
            EXIT;
        Sleep(cSleepTime);
    end;

    result := false;
end;

function TEdExternClient.LaunchRunClient(const aRunID: string): boolean;
{ const
  cRunClientProcessName = '..\ZARunClient\ZARunClient.exe ';
  cStartParameters = '';
  var
  xProcExec :TProcExec;
  xStartParameters : string;

  begin
  xStartParameters := cStartParameters;

  xProcExec := TProcExec.Create( cRunClientProcessName +  xStartParameters );
  xProcExec.ExecProcess();
  xProcExec.Free;
}
begin
    result := WaitTillRunClientLoaded(aRunID);
end;

function TEdExternClient.VerifyRunClient(out oRunID: string): boolean;

var
    xRunIDs: TStringArray;
    xRunID: string;
    xMustLaunchRunClient: boolean;
    xMustAddRunID: boolean;
begin
    result := true;
    xMustLaunchRunClient := false;
    xMustAddRunID := false;

    xRunIDs := TCoreClient.Instance.GetClientRunIDsAtAddress
        ('' { TCoreClient.Instance.ClientAddress } , true);

    if Length(xRunIDs) = 0 then
    begin
        if fRunIDs.Count = 0 then
        begin
            xRunID := TCoreClient.Instance.GetNewClientRunID();
            xMustAddRunID := true;

        end
        else
        begin
            xRunID := fRunIDs[0];
        end;
        xMustLaunchRunClient := true;
    end
    else
    begin
        xRunID := xRunIDs[0];
    end;

    if xMustAddRunID then
    begin
        fRunIDs.Add(xRunID);
        TCoreClient.Instance.AddClientToRun(xRunID);
    end;

    if xMustLaunchRunClient then
    begin
        result := LaunchRunClient(xRunID);
    end;

    oRunID := xRunID;
end;

function TEdExternClient.GetCurrentMethodName: string;
begin

end;

function TEdExternClient.GetRunIDWithCheck(): string;
begin
    if not VerifyRunClient(result) then
    begin
        raise Exception.Create('RunClient could not be started');
    end;
end;

procedure TEdExternClient.InitSystem;
begin

end;

function TEdExternClient.InterruptFinish(const aInterruptText: string): boolean;
begin
    EXIT(TCoreControllerClientCalls.Instance.InterruptFinish(aInterruptText));
end;

function TEdExternClient.InterruptStart(const aInterruptText: string): boolean;
begin
    EXIT(TCoreControllerClientCalls.Instance.InterruptStart(aInterruptText));
end;

function TEdExternClient.MethodStart(const aMethodName: string;
    const aDontShowMessages, aSimulate: boolean): boolean;
begin
    if aSimulate then
        result := TCoreControllerClientCalls.Instance.SimulateMethod(aMethodName, aDontShowMessages,
            self.GetRunIDWithCheck())
    else
        result := TCoreControllerClientCalls.Instance.StartMethod(aMethodName, aDontShowMessages,
            self.GetRunIDWithCheck())
end;

function TEdExternClient.ReloadValues: boolean;
begin
    result := false;
end;

procedure TEdExternClient.RequestSafeAppClose(const aMaxWaitTime: cardinal);
begin
    inherited;

end;

procedure TEdExternClient.ResetGlobalName;
begin
    inherited;

end;

function TEdExternClient.SetCurrentLayoutName(const aDialogText: string): boolean;
begin
    result := false;
end;

procedure TEdExternClient.SetGlobalError(const aErrorText: string);
begin
    TCoreControllerClientCalls.Instance.SetGlobalError(aErrorText);
end;

procedure TEdExternClient.SetOnSafeAppClose(const aValue: TNotifyEvent);
begin

end;

procedure TEdExternClient.StartCurrentMethod;
begin

end;

procedure TEdExternClient.StartMethod(aAlwaysSelect: boolean; const aDefaultMethod: string);
var
    xSelectName, xStartName: string;
begin
    if (TGetMethDlg.Call(xSelectName) = actStart) then
        xStartName := xSelectName;

    if (xStartName <> '') then
    begin
        self.MethodStart(xStartName, false, false);
    end;
end;

function TEdExternClient.ThreadIsRunning(aShowMsg: boolean): Boolean;
begin
    result := false;
end;

procedure TEdExternClient.UserInterrupt;
begin

end;

procedure TEdExternClient.RegisterControllerIfNew(const aRunID: string);
begin
    if not SameText(aRunID, fCurrentRunID) then
    begin
        RegisterController(aRunID);
    end;
end;

procedure TEdExternClient.RegisterControllerForAllRunIDs();
var
    xRunIDs: TStringArray;
    x: integer;
begin
    xRunIDs := TCoreClient.Instance.GetClientRunIDsAtAddress('', false);
    if Length(xRunIDs) = 0 then
    begin
        fCurrentRunID := TCoreClient.Instance.GetNewClientRunID();
        RegisterController(fCurrentRunID);
    end
    else
    begin
        for x := 0 to high(xRunIDs) do
        begin
            self.RegisterController(xRunIDs[x]);
        end;
    end;
end;

procedure TEdExternClient.RegisterController(const aRunID: string);
var
    xSuccess: boolean;
begin
    TCoreClient.Instance.AddClientToRun(aRunID);
    gLogManager.LogF('Add ClientRunID:', [aRunID], false);

    if TAppInstanceCoreClientLib.Instance.CoreClientSettingsRec.IsNoController then
        EXIT;

    xSuccess := TCoreClient.Instance.RegisterController(aRunID);

    if xSuccess then
    begin

        { TODO -owl : muss anders implementiert werden }

        // self.sbMethod.Enabled := true;
    end
    else
    begin
        raise Exception.Create('Another controller is already registered');
    end;
end;


end.
