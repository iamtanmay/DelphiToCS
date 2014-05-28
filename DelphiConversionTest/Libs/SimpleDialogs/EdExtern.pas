{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  : For DDE communications
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.11.05 pk                               TN2737   Initial revision
  14.11.05 pk  IsRunning                    TN2758   Returns true only if status is NOT "Ready"
  18.11.05 pk  SendAndGetData               TN2758   New
  24.11.06 wl  Create                       TN3432   wenn GetZARunnerConnection = false, nimmt Designer keine Verbindung mit ZARunner auf
  08.02.07 pk  SendAndGetData               TN3557   If connection is not assigned exit function
  08.09.08 pk  Start                        TN4215   Don't change Job title!
  17.11.08 wl  Send                         TN4312   für Delphi 2009 auskommentiert
  17.12.08 pk  fZARunnerConnection          TN4372   removed
  06.07.09 pk                               TN4585.4 massive changes
  13.07.09 pk                               TN4585.4 DDE replaced by TCP/IP
  30.07.09 pk  LaunchRunClient              TN4585.5 New: Start Runner Client
  13.08.09 wl                               TN4585.5 nur noch Basisklasse für TEdExternClient & TEdExternClassic
  27.05.10 wl                               TN5116   enthält neue Methoden für ZARunner/ZADesigner
  28.05.10 wl  RegisterController           TN5116   spezielle Methoden für RunClient
  04.06.10 wl                               TN5116   mit neuen Funktionen für Runner
  18.06.10 wl                               TN5116   Moss, Cherry Picking und Run Table-Funktionen entfernt
  20.04.12 wl  InterruptStart,InterruptFinish,SetGlobalError  TN5858  neu
  04.05.12 wl  SetAnimationImage            TN5858   Animation endgültig endfernt
  09.05.12 wl  RequestAndStateSetAbort      TN5858   neu
  07.08.12 wl  StartFlushMethod             TN5946   Parameter geändert
  03.09.13 wl  StartFlushMethod             TN6238   neu: DryAfterWash
  18.09.13 wl  ShowFlushDialog              TN6252.3 neu
  24.01.14 tp  Calibrate                    TN6341   neu
  08.04.14 ts  ShowTableEditor              TN6393   neu
  -------------------------------------------------------------------------------------------------- }

unit EdExtern;


interface


uses
    StdCtrls,
    Buttons,
    Classes,
    Controls,
    ExtCtrls,
    AppTypes;

type
    TEdExtern = class
    private
        class var uInstance: TEdExtern;
        class procedure SetInstance(aInstance: TEdExtern); static;
        class function GetInstance: TEdExtern; static;
    protected
        procedure Connect(); virtual;
        procedure Disconnect(); virtual;
        function GetCurrentMethodName: string; virtual; abstract;
    public
        class property Instance: TEdExtern read GetInstance write SetInstance;
        class procedure InstanceDestroy();
        //
        function MethodStart(const aMethodName: string; const aDontShowMessages, aSimulate: boolean): boolean;
            virtual; abstract;
        function ExecuteDllCall(const aDLLName, aDLLFunction, aDLLParameter: string): string;
            virtual; abstract;
        procedure StartFlushMethod(const aPipDeviceName: string; aTipMap: TIPMAP; aAddDilutorMap: TIPMAP;
            const aDiluentNames: TArray<string>; aVolume, aCycles: integer; aUseCh1, aUseCh2: boolean;
            aUsePeriPump, aUseAddDilutors: boolean; aDryAfterWash: boolean); virtual; abstract;
        function ThreadIsRunning(aShowMsg: boolean): Boolean; virtual; abstract;
        procedure UserInterrupt; virtual; abstract;
        function SetCurrentLayoutName(const aDialogText: string): boolean; virtual; abstract;
        procedure InitSystem(); virtual; abstract;
        procedure Calibrate(aGUILoadPreset, aGUIPresetName, aGUISystemLiq, aGUIDensity, aGUICVTolerance,
            aGUIMaxErrorPercent, aGUICalibrateWithSysLiq, aGUICountTries, aGUICountStep, aGUIManualFill,
            aGUIBalanceWait, aGUIBalancePrecision, aGUITipArray, aGUILHP: string); virtual; abstract;
        function ReloadValues: boolean; virtual; abstract;
        procedure StartMethod(aAlwaysSelect: boolean; const aDefaultMethod: string); virtual; abstract;
        procedure StartCurrentMethod(); virtual; abstract;
        property CurrentMethodName: string read GetCurrentMethodName;
        class procedure ReadDllMenuItems(var aMenuItems: TDLLMenuItems); static;
        class procedure ReadEditTableMenuItems(var aMenuItems: TArray<string>); static;
        procedure SetOnSafeAppClose(const aValue: TNotifyEvent); virtual; abstract;
        function AnyProcessesRunning(): boolean; virtual; abstract;
        procedure RequestSafeAppClose(const aMaxWaitTime: cardinal); virtual; abstract;
        procedure ResetGlobalName(); virtual; abstract;
        function InterruptStart(const aInterruptText: string): boolean; virtual; abstract;
        function InterruptFinish(const aInterruptText: string): boolean; virtual; abstract;
        procedure SetGlobalError(const aErrorText: string); virtual; abstract;
        procedure RequestAndStateSetAbort; virtual;
        procedure ShowFlushDialog(); virtual;
        procedure ShowCalibrateDialog(); virtual;
        procedure ShowTableEditor(aTableName: string); virtual;
        procedure RegisterControllerIfNew(const aRunID: string); virtual; abstract;
        procedure RegisterControllerForAllRunIDs(); virtual; abstract;
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    AppSettings,
    CommonTypes,
    StringUtilities;

{ TEdExtern }

procedure TEdExtern.Connect;
begin
    //
end;

procedure TEdExtern.Disconnect;
begin
    //
end;

class function TEdExtern.GetInstance: TEdExtern;
begin
    uInstance.Connect();
    result := uInstance;
end;

class procedure TEdExtern.InstanceDestroy();
begin
    if not Assigned(uInstance) then
        EXIT;
    // if uEdExtern.IsConnected() then EXIT;
    uInstance.Disconnect();
    FreeAndNil(uInstance);
end;

class procedure TEdExtern.SetInstance(aInstance: TEdExtern);
begin
    uInstance := aInstance;
end;

procedure TEdExtern.ShowCalibrateDialog;
begin
    // Dummy
end;

procedure TEdExtern.ShowFlushDialog;
begin
    // Dummy
end;

procedure TEdExtern.ShowTableEditor(aTableName: string);
begin
    // Dummy
end;

class procedure TEdExtern.ReadDllMenuItems(var aMenuItems: TDLLMenuItems);
var
    xIniAccess: IWinlissyIniAccess;
    xNames: TStringArray;
    x: integer;
begin
    // Vorbereitung für TN2662

    xIniAccess := gCommonDll.CreateAppIni;

    xNames := xIniAccess.ReadAllowedSection('MenuTool', 'M');
    SetLength(aMenuItems, Length(xNames));
    for x := 0 to Length(xNames) - 1 do
    begin

        // Übernehmen beliebig vieler Tool-Menu-Einträge
        aMenuItems[x] := xIniAccess.ReadDLLMenuItem('MenuTool', xNames[x]);
        aMenuItems[x].MenuName := 'actView_' + xNames[x];
    end;
end;

class procedure TEdExtern.ReadEditTableMenuItems(var aMenuItems: TArray<string>);
var
    xIniAccess: IWinlissyIniAccess;
    xNames: string;
begin
    xIniAccess := gCommonDll.CreateAppIni;

    xNames := xIniAccess.ReadString('Display', 'TablesEditableInRunner');
    aMenuItems := TStringUtilities.StringToStringArray(xNames, ',');
end;

procedure TEdExtern.RequestAndStateSetAbort;
begin
    // Dummy
end;


end.
