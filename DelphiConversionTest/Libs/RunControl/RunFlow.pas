{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  17.12.08 pk  SimulationMode               TN4372   result is now also based on GetIsCurrentThreadSimulated
  02.04.09 wl  AppSleep                     TN4500   if the wait time is shorter than 100 msec, the global error will not be requested
  08.04.09 pk  GetIsCurrentThreadSimulated  TN4513   Calls IsAPIAvailable
  07.12.09 pk  GetAppSimulationMode         TN4920   New: only checks if entire app was started in simulation
  12.11.10 wl  GetAppSimulationMode         TN5112   an Änderungen von Licence angepasst
  27.04.11 wl  WriteValues                  TN5554   Simulation-Werte werden in LocalIni geschrieben
  27.04.11 wl  Create                       TN5554   Simulation-Werte werden aus LocalIni gelesen
  15.08.13 wl                               TN6223   uses geändert
  20.08.13 wl  IsRecoveryMovement           TN6231   neu: wird bei Abbruch für Recovery-Bewegungen gesetzt
  ---------------------------------------------------------------------------------------------------------------------- }

unit RunFlow;


interface


uses
    SysUtils,
    CommonTypes;

type
    ERecoveryMovementException = class(Exception);

    TRunFlow = class
    private
        fSimulationSpeed_Percent: integer;
        fSimulationAskWeight: boolean;
        fIsSafeExitRequested: boolean;
        fIsRecoveryMovement: boolean;
        class var uInstance: TRunFlow;
        procedure SetSimulationSpeed_Percent(Value: integer);
        function GetIsCurrentThreadSimulated: boolean;
        function GetSimulationMode: boolean; virtual;
        function GetAppSimulationMode: boolean;
        procedure ReadValues;
    public
        constructor Create();
        class procedure CreateInstance;
        class procedure DestroyInstance;

        procedure AppSleep(aRealModeWait_MSec: int64; aSimModeWait_MSec: int64 = 0);
        procedure WriteValues();

        property AppSimulationMode: boolean read GetAppSimulationMode;
        property SimulationMode: boolean read GetSimulationMode;
        property SimulationSpeed_Percent: integer read fSimulationSpeed_Percent
            write SetSimulationSpeed_Percent;
        property SimulationAskWeight: boolean read fSimulationAskWeight write fSimulationAskWeight;
        property IsSafeExitRequested: boolean read fIsSafeExitRequested write fIsSafeExitRequested;
        property IsRecoveryMovement: boolean read fIsRecoveryMovement write fIsRecoveryMovement;
        class property Instance: TRunFlow read uInstance write uInstance;
    end;

function gRunFlow: TRunFlow;


implementation


uses
    ThreadAPI,
    ErrorManager,
    AppSettings,
    AppInstanceLicencing,
    ConfigurationFile;

function gRunFlow: TRunFlow;
begin
    EXIT(TRunFlow.Instance);
end;

{ TRunFlow }

constructor TRunFlow.Create();
begin
    inherited Create();

    fIsSafeExitRequested := false;
    fIsRecoveryMovement := false;
    ReadValues();
end;

class procedure TRunFlow.CreateInstance;
begin
    uInstance := TRunFlow.Create();
end;

class procedure TRunFlow.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;

procedure TRunFlow.AppSleep(aRealModeWait_MSec: int64; aSimModeWait_MSec: int64 = 0);

const
    INT_SLEEP_TIME = 200;
var
    xRestTime, xWaitTime_MSec: int64;
begin
    if self.GetSimulationMode() then
        xWaitTime_MSec := (((aRealModeWait_MSec - aSimModeWait_MSec) * fSimulationSpeed_Percent) div 100) +
            aSimModeWait_MSec
    else
        xWaitTime_MSec := aRealModeWait_MSec;

    // up to 100 msec: simple version
    if (xWaitTime_MSec <= 100) then
    begin
        Sleep(xWaitTime_MSec);
        EXIT;
    end;

    // over 100 msec: request global error
    Sleep(1);
    xRestTime := xWaitTime_MSec - 1;

    while (xRestTime > 0) and (not gErrorManager.IsGlobalErr()) do
    begin

        if (xRestTime <= INT_SLEEP_TIME) then
            xWaitTime_MSec := xRestTime
        else
            xWaitTime_MSec := INT_SLEEP_TIME;

        Sleep(xWaitTime_MSec);
        xRestTime := xRestTime - INT_SLEEP_TIME;
    end;
end;

procedure TRunFlow.SetSimulationSpeed_Percent(Value: integer);
begin
    if Value < 0 then
        Value := 0;
    if Value > 100 then
        Value := 100; // Werte von 0 bis 100 erlaubt

    fSimulationSpeed_Percent := Value;
end;

procedure TRunFlow.WriteValues;
var
    xLocalIniFile: IConfigurationSet;
begin
    xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(true);
    try
        xLocalIniFile.WriteInteger('RunFlow', 'SimulationSpeedPercent', fSimulationSpeed_Percent);
        xLocalIniFile.WriteBool('RunFlow', 'SimulationAskWeight', fSimulationAskWeight);
    finally
        xLocalIniFile.Close;
    end;
end;

function TRunFlow.GetIsCurrentThreadSimulated: boolean;
begin
    result := TThreadAPI.IsAPIAvailable() and TThreadAPI.GetIsCurrentThreadSimulated()
end;

function TRunFlow.GetAppSimulationMode: boolean;
begin
    // AppSimulationMode should only be called if we want to check if the application was started with Simulation Flag or not
    // This should NOT be used in functions that are called during a run, use GetSimulateionMode instead.
    // Conversely, we MUST use this function in functions where no runs are currently running
    result := (TAppInstanceLicencing.Instance.Licence.AppMode = appModeSim);
end;

function TRunFlow.GetSimulationMode: boolean;
begin
    result := self.AppSimulationMode or GetIsCurrentThreadSimulated(); // im Editor immer Simulationsmodus!
end;

procedure TRunFlow.ReadValues;
var
    xLocalIniFile: IConfigurationSet;
begin
    xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(true);
    try
        self.SimulationSpeed_Percent := xLocalIniFile.ReadInteger('RunFlow', 'SimulationSpeedPercent', 100);
        fSimulationAskWeight := xLocalIniFile.ReadBool('RunFlow', 'SimulationAskWeight', true);
    finally
        xLocalIniFile.Close;
    end;
end;


end.
