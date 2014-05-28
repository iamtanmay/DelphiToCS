{ ------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  27.05.10 wl                                    TN5116   enthält neue Methoden für ZARunner/ZADesigner
  28.05.10 wl  RegisterController                TN5116   spezielle Methoden für RunClient
  04.06.10 wl                                    TN5116   mit neuen Funktionen für Runner
  18.06.10 wl                                    TN5116   Moss, Cherry Picking und Run Table-Funktionen entfernt
  02.02.11 wl  ChangeLayout                      TN5466   von RunMain hierher
  14.12.11 wl                                    TN5765   GetMethDlg ohne Session
  15.12.11 wl  ExecuteDllCall                    TN5767   TDllCall.DirectExecute jetzt mit Versionsnummer
  27.12.11 wl                                    TN5768   an geändertes TRunGlobals angepasst
  20.04.12 wl  InterruptStart,InterruptFinish    TN5858   ruft ThreadManager-Funktionen auf
  20.04.12 wl  SetGlobalError                    TN5858   ruft ErrorManager-Funktionen auf
  04.05.12 wl  SetAnimationImage                 TN5858   Animation endgültig endfernt
  09.05.12 wl  RequestAndStateSetAbort           TN5858   neu
  07.08.12 wl  StartFlushMethod,InitSystem       TN5946   nutzen temporäre Methode
  07.08.12 wl  MethodStartIntern                 TN5946   INitAtMethodEnd wird hier bestimmt
  13.08.12 wl  StartFlushMethod                  TN5947   mit geänderten Optionen
  01.10.12 wl  StartFlushMethod                  TN5989   Flush-Methode wurde falsch geschrieben
  20.02.13 wl                                    TN6055   uses geändert
  24.04.13 wl                                    TN6137   TLiquids.Instance statt gLiquids
  20.06.13 ts                                    TN6146   TLayoutDataAdaptorExt.cTemporaryMethodName anstatt TMethodDataAdaptorExt.cTemporaryMethodName
  15.08.13 wl                                    TN6223   uses geändert
  30.08.13 wl                                    TN6236   uses geändert
  03.09.13 wl  StartFlushMethod                  TN6238   neu: DryAfterWash
  18.09.13 wl  Show
  FlushDialog                   TN6252.3 neu: Übergibt alle Eventfunktionen an dlgFlush
  24.01.14 tp  Calibrate()                       TN6341   Calibrate Dialog added
  08.04.14 ts  ShowTableEditor                   TN6393   neu
  ------------------------------------------------------------------------------------------------------------ }

unit EdExternDirect;


interface


uses
    Classes,
    EdExtern,
    AppTypes;

type
    TEdExternDirect = class(TEdExtern)
    protected
        function GetCurrentMethodName: string; override;
        procedure ChangeLayout(aEmptyLayoutAllowed: boolean; const aCaption: string);
        function MethodStartIntern(const aMethodName: string;
            const aDontShowMessages, aSimulate, aInitAtEnd: boolean): boolean;

        function PeriPumpExists(): boolean;
        function GetSystemLiquidNames(): TArray<string>;
        function GetPipDeviceNames(): TArray<string>;
        function PipDeviceHasMultiPumps(const aPipDeviceName: string): boolean;
        function PipDeviceGetLiquidIndices(const aPipDeviceName: string; aTips: TIPMAP;
            aPumpIndex1, aPumpIndex2: integer): TArray<integer>;
        function PipDeviceGetTipCount(const aPipDeviceName: string): integer;
    public
        function MethodStart(const aMethodName: string; const aDontShowMessages, aSimulate: boolean)
            : boolean; override;
        function ExecuteDllCall(const aDLLName, aDLLFunction, aDLLParameter: string): string; override;
        procedure StartFlushMethod(const aPipDeviceName: string; aTipMap: TIPMAP; aAddDilutorMap: TIPMAP;
            const aDiluentNames: TArray<string>; aVolume, aCycles: integer; aUseCh1, aUseCh2: boolean;
            aUsePeriPump, aUseAddDilutors: boolean; aDryAfterWash: boolean); override;
        function ThreadIsRunning(aShowMsg: boolean): Boolean; override;
        procedure UserInterrupt; override;
        function SetCurrentLayoutName(const aDialogText: string): boolean; override;
        procedure InitSystem(); override;
        procedure Calibrate(aGUILoadPreset, aGUIPresetName, aGUISystemLiq, aGUIDensity, aGUICVTolerance,
            aGUIMaxErrorPercent, aGUICalibrateWithSysLiq, aGUICountTries, aGUICountStep, aGUIManualFill,
            aGUIBalanceWait, aGUIBalancePrecision, aGUITipArray, aGUILHP: string); override;
        function ReloadValues: boolean; override;
        procedure StartMethod(aAlwaysSelect: boolean; const aDefaultMethod: string); override;
        procedure RegisterControllerIfNew(const aRunID: string); override;
        procedure RegisterControllerForAllRunIDs(); override;
        procedure StartCurrentMethod(); override;
        procedure SetOnSafeAppClose(const aValue: TNotifyEvent); override;
        function AnyProcessesRunning(): boolean; override;
        procedure RequestSafeAppClose(const aMaxWaitTime: cardinal); override;
        procedure ResetGlobalName(); override;
        function InterruptStart(const aInterruptText: string): boolean; override;
        function InterruptFinish(const aInterruptText: string): boolean; override;
        procedure SetGlobalError(const aErrorText: string); override;
        procedure RequestAndStateSetAbort; override;
        procedure ShowFlushDialog(); override;
        procedure ShowCalibrateDialog(); override;
        procedure ShowTableEditor(aTableName: string); override;
    end;


implementation


uses
    Windows,
    SysUtils,
    Forms,

    GeneralTypes,
    MethodStarter,
    GUIManagerRunner,
    MethodDataAdaptor,
    SQLTermsDataAdaptor,
    MethodDataAdaptorExt,
    MethodVariableTypes,
    QueryDataAdaptor,
    DllCall,
    ThrdMan,
    Action,
    ExecHandler,
    RunGlobals,
    AppSettings,
    DialogUtils,
    GUIManager,
    ErrorManager,
    ExternDllFunc,
    LayoutDataAdaptor,
    ThrdManExt,
    ThreadAPI,
    FlushRunStep,
    GetMeth,
    Liquids,
    dlgFlush,
    dlgCalibrate,
    ObjModul,
    Module,
    IntfPipDevice,
    LiquidManager,
    PipDeviceManager,
    IntfPeriPumpDevice,
    GUIManagerRun,
    LayoutManager,
    LayoutDataAdaptorExt,
    TableEditor,
    DatabaseConfig,
    LocalDataProviderFactory;

{ TEdExternDirect }

function TEdExternDirect.AnyProcessesRunning: boolean;
begin
    result := true;

    try
        result := not TThreadAPI.WaitTillAllProcessesExited(0);
    except
    end;
end;

function TEdExternDirect.ExecuteDllCall(const aDLLName, aDLLFunction, aDLLParameter: string): string;
begin
    result := TDllCall.DirectExecute(aDLLName, aDLLFunction, aDLLParameter, TDllInterfaceVersion.Version8);
end;

procedure TEdExternDirect.StartFlushMethod(const aPipDeviceName: string; aTipMap: TIPMAP;
    aAddDilutorMap: TIPMAP; const aDiluentNames: TArray<string>; aVolume, aCycles: integer;
    aUseCh1, aUseCh2: boolean; aUsePeriPump, aUseAddDilutors: boolean; aDryAfterWash: boolean);
var
    xUsedDiluents: TArray<string>;
    xMethodRec: TArray<TMethodRec>;
    x: integer;
begin
    if Length(aDiluentNames) = 0 then
        EXIT;

    if (Length(aDiluentNames) > 1) and (aUsePeriPump) then
    begin
        SetLength(xUsedDiluents, 1); // Bei Peripumpe ist nur die erste Systemflüssigkeit möglich
        xUsedDiluents[0] := TLiquids.Instance.SystemLiquids[0].Diluent;
    end
    else
    begin
        SetLength(xUsedDiluents, Length(aDiluentNames));
        xUsedDiluents := Copy(aDiluentNames);
    end;

    SetLength(xMethodRec, Length(aDiluentNames));
    for x := 0 to high(xMethodRec) do
    begin
        xMethodRec[x].Valid := true;
        xMethodRec[x].name := TLayoutDataAdaptorExt.cTemporaryMethodName;
        xMethodRec[x].Seq := x;
        xMethodRec[x].Action := 'FLUSH';
        xMethodRec[x].Inactive := false;
        xMethodRec[x].Options := TMethodStepSetting_Flush.WriteFlushOptions(aPipDeviceName, aTipMap,
            aAddDilutorMap, aDiluentNames[x], aVolume, aCycles, aUseCh1, aUseCh2, aUsePeriPump,
            aUseAddDilutors, aDryAfterWash);
    end;

    TMethodDataAdaptorExt.SaveTemporaryMethod(xMethodRec, TRunGlobals.Instance.LayoutName);
    MethodStartIntern(TLayoutDataAdaptorExt.cTemporaryMethodName, true, false, false);
end;

function TEdExternDirect.GetCurrentMethodName: string;
begin
    result := TRunGlobals.Instance.PMethodName;
end;

procedure TEdExternDirect.ChangeLayout(aEmptyLayoutAllowed: boolean; const aCaption: string);
var
    xNewLayoutName: string;
begin
    // find new layout name
    xNewLayoutName := TGUIManagerRun.Instance.ChooseLayout(aCaption);
    if (xNewLayoutName = '') then
        EXIT;
    if (xNewLayoutName = TRunGlobals.Instance.LayoutName) then
        EXIT;

    if (TRunGlobals.Instance.PMethodName = '') then
    begin
        if (xNewLayoutName = '') then
        begin
            TRunGlobals.Instance.ChangeGlobalName('', '', false, false);
            TLayoutManager.Instance.UnregisterCurrentLayout();
        end
        else
        begin
            TRunGlobals.Instance.ChangeGlobalName('', xNewLayoutName, false, false);
            TLayoutManager.Instance.RegisterLayout('', xNewLayoutName);
            TLayoutManager.Instance.Load();
        end;
        EXIT;
    end;
end;

procedure TEdExternDirect.InitSystem;
var
    xMethodRec: TArray<TMethodRec>;
begin
    if not SetCurrentLayoutName(TLanguageString.Read('Initialize system', 'System Initialisieren')) then
        EXIT;

    SetLength(xMethodRec, 1);
    xMethodRec[0].Valid := true;
    xMethodRec[0].name := TLayoutDataAdaptorExt.cTemporaryMethodName;
    xMethodRec[0].Seq := 0;
    xMethodRec[0].Action := 'BLANK';
    xMethodRec[0].Inactive := false;
    xMethodRec[0].Options := '';

    TMethodDataAdaptorExt.SaveTemporaryMethod(xMethodRec, TRunGlobals.Instance.LayoutName);
    MethodStartIntern(TLayoutDataAdaptorExt.cTemporaryMethodName, true, false, false);
end;

function TEdExternDirect.InterruptFinish(const aInterruptText: string): boolean;
var
    xInterruptText: string;
begin
    xInterruptText := aInterruptText;
    if xInterruptText = '' then
    begin
        xInterruptText := 'Interrupt Finish';
    end;

    result := TThreadManagerSetup.Instance.RequestInterruptFinish(TThreadAPI.GetCurrentThreadID(),
        aInterruptText);
end;

// Starts a calibrate method. Its a temporary method that is called by code. All steps are below
procedure TEdExternDirect.Calibrate(aGUILoadPreset, aGUIPresetName, aGUISystemLiq, aGUIDensity,
    aGUICVTolerance, aGUIMaxErrorPercent, aGUICalibrateWithSysLiq, aGUICountTries, aGUICountStep,
    aGUIManualFill, aGUIBalanceWait, aGUIBalancePrecision, aGUITipArray, aGUILHP: string);
var
    xQuery: TQueryDataAdaptor;
    xSQLTerms: TSQLTermsDataAdaptor;

    xMethodRec: TArray<TMethodRec>;
    xSQLExport: TArray<TSQLTermRec>;

    xMethodLineCount: integer;
    xSQLTermCount: integer;

    xCALIBCreateTables: TSQLTermRec;
    xCALIBExportData: TSQLTermRec;
    xCALIBGetAllWeights: TSQLTermRec;
    xCALIBGetAvgWeight: TSQLTermRec;
    xCALIBGetErrorArray: TSQLTermRec;
    xCALIBGetMaxError: TSQLTermRec;
    xCALIBGetMinError: TSQLTermRec;
    xCALIBGetVolcorrData: TSQLTermRec;
    xCALIBGetVolcurve: TSQLTermRec;
    xCALIBInsertPipetingSteps: TSQLTermRec;
    xCALIBSavePreset: TSQLTermRec;
    xCALIBResultsDelete: TSQLTermRec;
    xCALIBSQLDelete: TSQLTermRec;
    xCALIBTablesExist: TSQLTermRec;
    xCALIBUpdateCV: TSQLTermRec;
    xCALIBUpdateError: TSQLTermRec;
    xCALIBUpdateResults: TSQLTermRec;
    xCALIBUpdateStatus: TSQLTermRec;
    xCALIBUpdateVolcorr: TSQLTermRec;

    xCALIBReadPreset: TSQLTermRec;

    xMethodName: string;
    i: integer;
begin
    // Initialize handles to write to SQL tables
    xSQLTerms := TSQLTermsDataAdaptor.Create;
    xQuery := TQueryDataAdaptor.Create('CALIBTemp');

    // Delete Calibration SQL queries, if left over
    xQuery.ExecSQL('Delete from SQLTERMS where LEFTSTR(Name,5) = ' + chr(39) + 'CALIB' + chr(39));
    xQuery.ExecSQL('Delete from METHOD where Name = ' + chr(39) + '~TemporaryMethod~' + chr(39));
    xQuery.ExecSQL('Delete from METHODVARIABLES where MethodName = ' + chr(39) + '~TemporaryMethod~'
        + chr(39));

    // Number of lines in the method
    xMethodLineCount := 159;
    // Number of SQLTerms used
    xSQLTermCount := 20;

    // Number of lines for the Method
    SetLength(xMethodRec, xMethodLineCount);

    // Number of SQL queries to put in the table
    SetLength(xSQLExport, xSQLTermCount);

    // Name for Temporary method
    xMethodName := '~TemporaryMethod~';

    // Store variables from GUI
    // Load preset or use custom preset ?
    xMethodRec[14].Options := 'STOREKEY=_0LoadPreset,STOREVALUE=' + aGUILoadPreset;

    // Name of the preset to use/name of new custom preset
    xMethodRec[15].Options := 'STOREKEY=_0PresetName,STOREVALUE=' + aGUIPresetName;

    // Name of the system liquid
    xMethodRec[16].Options := 'STOREKEY=_0SystemLiq,STOREVALUE=' + aGUISystemLiq;

    // Its called System Liquid density, but its actually the density of the calibrating liquid
    xMethodRec[17].Options := 'STOREKEY=_0Density,STOREVALUE=' + aGUIDensity;

    // Bool flag, for calibrating with System liquid
    xMethodRec[18].Options := 'STOREKEY=_0CalibrateWithSysLiq,STOREVALUE=' + aGUICalibrateWithSysLiq;

    // No of averaging steps per try per volume per tip
    xMethodRec[19].Options := 'STOREKEY=_0CountSteps,STOREVALUE=' + aGUICountStep;

    // No of tries to correct the factor, per volume, per tip
    xMethodRec[20].Options := 'STOREKEY=_0CountTries,STOREVALUE=' + aGUICountTries;

    // Maximum deviation in the measurements before we cancel
    xMethodRec[21].Options := 'STOREKEY=_0CVTolerance,STOREVALUE=' + aGUICVTolerance;

    // The maximum error before we cancel
    xMethodRec[22].Options := 'STOREKEY=_0PipettingTolerancePercent,STOREVALUE=' + aGUIMaxErrorPercent;

    // The Liquid Handling Parameter to use
    xMethodRec[23].Options := 'STOREKEY=_0LQH,STOREVALUE=' + aGUILHP;

    // ManualFill volume
    xMethodRec[24].Options := 'STOREKEY=_0ManualFill,STOREVALUE=' + aGUIManualFill;

    // Wait time for Tare and Weigh from BALANCE
    xMethodRec[25].Options := 'STOREKEY=_0BalanceWaitTime,STOREVALUE=' + aGUIBalanceWait;

    // The number of digits after decimal point to use from the BALANCE
    xMethodRec[26].Options := 'STOREKEY=_0BalanceAccuracy,STOREVALUE=' + aGUIBalancePrecision;

    // All the tips
    xMethodRec[27].Options := 'STOREKEY=_0TipArray,STOREVALUE={A(' + aGUITipArray + ')}';

    xMethodRec[0].Valid := true;
    xMethodRec[0].name := xMethodName;
    xMethodRec[0].Seq := 10;
    xMethodRec[0].Action := 'BLANK';
    xMethodRec[0].Inactive := false;
    xMethodRec[0].Options := '';

    xMethodRec[1].Valid := true;
    xMethodRec[1].name := xMethodName;
    xMethodRec[1].Seq := 20;
    xMethodRec[1].Action := 'BLANK';
    xMethodRec[1].Inactive := false;
    xMethodRec[1].Options := '';

    xMethodRec[2].Valid := true;
    xMethodRec[2].name := xMethodName;
    xMethodRec[2].Seq := 30;
    xMethodRec[2].Action := 'BLANK';
    xMethodRec[2].Inactive := false;
    xMethodRec[2].Options := '';

    xMethodRec[3].Valid := true;
    xMethodRec[3].name := xMethodName;
    xMethodRec[3].Seq := 40;
    xMethodRec[3].Action := 'BLANK';
    xMethodRec[3].Inactive := false;
    xMethodRec[3].Options := '';

    xMethodRec[4].Valid := true;
    xMethodRec[4].name := xMethodName;
    xMethodRec[4].Seq := 50;
    xMethodRec[4].Action := 'BLANK';
    xMethodRec[4].Inactive := false;
    xMethodRec[4].Options := '';

    xMethodRec[5].Valid := true;
    xMethodRec[5].name := xMethodName;
    xMethodRec[5].Seq := 60;
    xMethodRec[5].Action := 'BLANK';
    xMethodRec[5].Inactive := false;
    xMethodRec[5].Options := '';

    xMethodRec[6].Valid := true;
    xMethodRec[6].name := xMethodName;
    xMethodRec[6].Seq := 70;
    xMethodRec[6].Action := 'BLANK';
    xMethodRec[6].Inactive := false;
    xMethodRec[6].Options := '';

    xMethodRec[7].Valid := true;
    xMethodRec[7].name := xMethodName;
    xMethodRec[7].Seq := 80;
    xMethodRec[7].Action := 'BLANK';
    xMethodRec[7].Inactive := false;
    xMethodRec[7].Options := '';

    xMethodRec[8].Valid := true;
    xMethodRec[8].name := xMethodName;
    xMethodRec[8].Seq := 90;
    xMethodRec[8].Action := 'BLANK';
    xMethodRec[8].Inactive := false;
    xMethodRec[8].Options := '';

    xMethodRec[9].Valid := true;
    xMethodRec[9].name := xMethodName;
    xMethodRec[9].Seq := 100;
    xMethodRec[9].Action := 'BLANK';
    xMethodRec[9].Inactive := false;
    xMethodRec[9].Options := '';

    xMethodRec[10].Valid := true;
    xMethodRec[10].name := xMethodName;
    xMethodRec[10].Seq := 110;
    xMethodRec[10].Action := 'BLANK';
    xMethodRec[10].Inactive := false;
    xMethodRec[10].Options := '';

    xMethodRec[11].Valid := true;
    xMethodRec[11].name := xMethodName;
    xMethodRec[11].Seq := 120;
    xMethodRec[11].Action := 'BLANK';
    xMethodRec[11].Inactive := false;
    xMethodRec[11].Options := '';

    xMethodRec[12].Valid := true;
    xMethodRec[12].name := xMethodName;
    xMethodRec[12].Seq := 130;
    xMethodRec[12].Action := 'BLANK';
    xMethodRec[12].Inactive := false;
    xMethodRec[12].Options := '';

    xMethodRec[13].Valid := true;
    xMethodRec[13].name := xMethodName;
    xMethodRec[13].Seq := 140;
    xMethodRec[13].Action := 'BLANK';
    xMethodRec[13].Inactive := false;
    xMethodRec[13].Options := '';

    xMethodRec[14].Valid := true;
    xMethodRec[14].name := xMethodName;
    xMethodRec[14].Seq := 150;
    xMethodRec[14].Action := 'STORE';
    xMethodRec[14].Inactive := false;

    xMethodRec[15].Valid := true;
    xMethodRec[15].name := xMethodName;
    xMethodRec[15].Seq := 160;
    xMethodRec[15].Action := 'STORE';
    xMethodRec[15].Inactive := false;

    xMethodRec[16].Valid := true;
    xMethodRec[16].name := xMethodName;
    xMethodRec[16].Seq := 170;
    xMethodRec[16].Action := 'STORE';
    xMethodRec[16].Inactive := false;

    xMethodRec[17].Valid := true;
    xMethodRec[17].name := xMethodName;
    xMethodRec[17].Seq := 180;
    xMethodRec[17].Action := 'STORE';
    xMethodRec[17].Inactive := false;

    xMethodRec[18].Valid := true;
    xMethodRec[18].name := xMethodName;
    xMethodRec[18].Seq := 190;
    xMethodRec[18].Action := 'STORE';
    xMethodRec[18].Inactive := false;

    xMethodRec[19].Valid := true;
    xMethodRec[19].name := xMethodName;
    xMethodRec[19].Seq := 200;
    xMethodRec[19].Action := 'STORE';
    xMethodRec[19].Inactive := false;

    xMethodRec[20].Valid := true;
    xMethodRec[20].name := xMethodName;
    xMethodRec[20].Seq := 210;
    xMethodRec[20].Action := 'STORE';
    xMethodRec[20].Inactive := false;

    xMethodRec[21].Valid := true;
    xMethodRec[21].name := xMethodName;
    xMethodRec[21].Seq := 220;
    xMethodRec[21].Action := 'STORE';
    xMethodRec[21].Inactive := false;

    xMethodRec[22].Valid := true;
    xMethodRec[22].name := xMethodName;
    xMethodRec[22].Seq := 230;
    xMethodRec[22].Action := 'STORE';
    xMethodRec[22].Inactive := false;

    xMethodRec[23].Valid := true;
    xMethodRec[23].name := xMethodName;
    xMethodRec[23].Seq := 240;
    xMethodRec[23].Action := 'STORE';
    xMethodRec[23].Inactive := false;

    xMethodRec[24].Valid := true;
    xMethodRec[24].name := xMethodName;
    xMethodRec[24].Seq := 250;
    xMethodRec[24].Action := 'STORE';
    xMethodRec[24].Inactive := false;

    xMethodRec[25].Valid := true;
    xMethodRec[25].name := xMethodName;
    xMethodRec[25].Seq := 260;
    xMethodRec[25].Action := 'STORE';
    xMethodRec[25].Inactive := false;

    xMethodRec[26].Valid := true;
    xMethodRec[26].name := xMethodName;
    xMethodRec[26].Seq := 270;
    xMethodRec[26].Action := 'STORE';
    xMethodRec[26].Inactive := false;

    xMethodRec[27].Valid := true;
    xMethodRec[27].name := xMethodName;
    xMethodRec[27].Seq := 280;
    xMethodRec[27].Action := 'STORE';
    xMethodRec[27].Inactive := false;

    xMethodRec[28].Valid := true;
    xMethodRec[28].name := xMethodName;
    xMethodRec[28].Seq := 290;
    xMethodRec[28].Action := 'STORE';
    xMethodRec[28].Inactive := false;
    xMethodRec[28].Options := 'STOREKEY=_0Tip,STOREVALUE=1';

    xMethodRec[29].Valid := true;
    xMethodRec[29].name := xMethodName;
    xMethodRec[29].Seq := 300;
    xMethodRec[29].Action := 'BLANK';
    xMethodRec[29].Inactive := false;
    xMethodRec[29].Options := '';

    xMethodRec[30].Valid := true;
    xMethodRec[30].name := xMethodName;
    xMethodRec[30].Seq := 310;
    xMethodRec[30].Action := 'STORE';
    xMethodRec[30].Inactive := false;
    xMethodRec[30].Options := 'STOREKEY=_$0TotalVol,STOREVALUE=10';

    xMethodRec[31].Valid := true;
    xMethodRec[31].name := xMethodName;
    xMethodRec[31].Seq := 320;
    xMethodRec[31].Action := 'STORE';
    xMethodRec[31].Inactive := false;
    xMethodRec[31].Options := 'STOREKEY=_$0MaxVolOnBalance,STOREVALUE=25000';

    xMethodRec[32].Valid := true;
    xMethodRec[32].name := xMethodName;
    xMethodRec[32].Seq := 330;
    xMethodRec[32].Action := 'STORE';
    xMethodRec[32].Inactive := false;
    xMethodRec[32].Options := 'STOREKEY=_0SourceRack,STOREVALUE=SOURCE';

    xMethodRec[33].Valid := true;
    xMethodRec[33].name := xMethodName;
    xMethodRec[33].Seq := 340;
    xMethodRec[33].Action := 'STORE';
    xMethodRec[33].Inactive := false;
    xMethodRec[33].Options := 'STOREKEY=_0SourcePos,STOREVALUE=1';

    xMethodRec[34].Valid := true;
    xMethodRec[34].name := xMethodName;
    xMethodRec[34].Seq := 350;
    xMethodRec[34].Action := 'STORE';
    xMethodRec[34].Inactive := false;
    xMethodRec[34].Options := 'STOREKEY=_0DestPos,STOREVALUE=1';

    xMethodRec[35].Valid := true;
    xMethodRec[35].name := xMethodName;
    xMethodRec[35].Seq := 360;
    xMethodRec[35].Action := 'STORE';
    xMethodRec[35].Inactive := false;
    xMethodRec[35].Options := 'STOREKEY=_$0LastTip,STOREVALUE=0';

    xMethodRec[36].Valid := true;
    xMethodRec[36].name := xMethodName;
    xMethodRec[36].Seq := 370;
    xMethodRec[36].Action := 'BLANK';
    xMethodRec[36].Inactive := false;
    xMethodRec[36].Options := '';

    xMethodRec[37].Valid := true;
    xMethodRec[37].name := xMethodName;
    xMethodRec[37].Seq := 380;
    xMethodRec[37].Action := 'BLANK';
    xMethodRec[37].Inactive := false;
    xMethodRec[37].Options := '';

    xMethodRec[38].Valid := true;
    xMethodRec[38].name := xMethodName;
    xMethodRec[38].Seq := 390;
    xMethodRec[38].Action := 'SLSQL';
    xMethodRec[38].Inactive := false;
    xMethodRec[38].Options :=
        'SLSQLASARRAY=NO,SLSQLFILE=CALIBGetVolcurve,SLSQLPARAMS=_0LQH,SLSQLSTORE=_0VolcorrName';

    xMethodRec[39].Valid := true;
    xMethodRec[39].name := xMethodName;
    xMethodRec[39].Seq := 400;
    xMethodRec[39].Action := 'BLANK';
    xMethodRec[39].Inactive := false;
    xMethodRec[39].Options := '';

    xMethodRec[40].Valid := true;
    xMethodRec[40].name := xMethodName;
    xMethodRec[40].Seq := 410;
    xMethodRec[40].Action := 'BLANK';
    xMethodRec[40].Inactive := false;
    xMethodRec[40].Options := '';

    xMethodRec[41].Valid := true;
    xMethodRec[41].name := xMethodName;
    xMethodRec[41].Seq := 420;
    xMethodRec[41].Action := 'IF';
    xMethodRec[41].Inactive := false;
    xMethodRec[41].Options := 'CONDITION=_0LoadPreset <> True';

    xMethodRec[42].Valid := true;
    xMethodRec[42].name := xMethodName;
    xMethodRec[42].Seq := 430;
    xMethodRec[42].Action := 'USQL';
    xMethodRec[42].Inactive := false;
    xMethodRec[42].Options :=
        'USQLFILE=CALIBSavePreset,USQLPARAMS={_0PresetName,_0SystemLiq, _0Density, _0CountSteps, _0CountTries, _0CVTolerance, _0PipettingTolerancePercent,_0CalibrateWithSysLiq,_0ManualFill,_0BalanceWaitTime,_0BalanceAccuracy,_0TipArray,_0LQH}';

    xMethodRec[43].Valid := true;
    xMethodRec[43].name := xMethodName;
    xMethodRec[43].Seq := 440;
    xMethodRec[43].Action := 'IFEND';
    xMethodRec[43].Inactive := false;
    xMethodRec[43].Options := '';

    xMethodRec[44].Valid := true;
    xMethodRec[44].name := xMethodName;
    xMethodRec[44].Seq := 450;
    xMethodRec[44].Action := 'USQL';
    xMethodRec[44].Inactive := false;
    xMethodRec[44].Options := 'USQLFILE=CALIBResultsDelete';

    xMethodRec[45].Valid := true;
    xMethodRec[45].name := xMethodName;
    xMethodRec[45].Seq := 460;
    xMethodRec[45].Action := 'BLANK';
    xMethodRec[45].Inactive := false;
    xMethodRec[45].Options := '';

    xMethodRec[46].Valid := true;
    xMethodRec[46].name := xMethodName;
    xMethodRec[46].Seq := 470;
    xMethodRec[46].Action := 'BLANK';
    xMethodRec[46].Inactive := false;
    xMethodRec[46].Options := '';

    xMethodRec[47].Valid := true;
    xMethodRec[47].name := xMethodName;
    xMethodRec[47].Seq := 480;
    xMethodRec[47].Action := 'ManualFill';
    xMethodRec[47].Inactive := false;
    xMethodRec[47].Options := 'MANUAPOS=_0SourcePos,MANUARACK=_0SourceRack,MANUAVOL=_0ManualFill';

    xMethodRec[48].Valid := true;
    xMethodRec[48].name := xMethodName;
    xMethodRec[48].Seq := 490;
    xMethodRec[48].Action := 'BLANK';
    xMethodRec[48].Inactive := false;
    xMethodRec[48].Options := '';

    xMethodRec[49].Valid := true;
    xMethodRec[49].name := xMethodName;
    xMethodRec[49].Seq := 500;
    xMethodRec[49].Action := 'STORE';
    xMethodRec[49].Inactive := false;
    xMethodRec[49].Options :=
        'STOREKEY=_0PipettingTolerancePercent,STOREVALUE=_0PipettingTolerancePercent/100';

    xMethodRec[50].Valid := true;
    xMethodRec[50].name := xMethodName;
    xMethodRec[50].Seq := 510;
    xMethodRec[50].Action := 'STORE';
    xMethodRec[50].Inactive := false;
    xMethodRec[50].Options := 'STOREKEY=_0PipettingToleranceMax,STOREVALUE=1 + _0PipettingTolerancePercent';

    xMethodRec[51].Valid := true;
    xMethodRec[51].name := xMethodName;
    xMethodRec[51].Seq := 520;
    xMethodRec[51].Action := 'STORE';
    xMethodRec[51].Inactive := false;
    xMethodRec[51].Options := 'STOREKEY=_0PipettingToleranceMin,STOREVALUE=1 - _0PipettingTolerancePercent';

    xMethodRec[52].Valid := true;
    xMethodRec[52].name := xMethodName;
    xMethodRec[52].Seq := 530;
    xMethodRec[52].Action := 'BLANK';
    xMethodRec[52].Inactive := false;
    xMethodRec[52].Options := '';

    xMethodRec[53].Valid := true;
    xMethodRec[53].name := xMethodName;
    xMethodRec[53].Seq := 540;
    xMethodRec[53].Action := 'STORE';
    xMethodRec[53].Inactive := false;
    xMethodRec[53].Options := 'STOREKEY=_$0NoOfTips,STOREVALUE=COUNT(_0TipArray)';

    xMethodRec[54].Valid := true;
    xMethodRec[54].name := xMethodName;
    xMethodRec[54].Seq := 550;
    xMethodRec[54].Action := 'STORE';
    xMethodRec[54].Inactive := false;
    xMethodRec[54].Options := 'STOREKEY=_$0TipCounter,STOREVALUE=1';

    xMethodRec[55].Valid := true;
    xMethodRec[55].name := xMethodName;
    xMethodRec[55].Seq := 560;
    xMethodRec[55].Action := 'WHILE';
    xMethodRec[55].Inactive := false;
    xMethodRec[55].Options := 'CONDITION=_$0TipCounter <= _$0NoOfTips';

    xMethodRec[56].Valid := true;
    xMethodRec[56].name := xMethodName;
    xMethodRec[56].Seq := 570;
    xMethodRec[56].Action := 'BLANK';
    xMethodRec[56].Inactive := false;
    xMethodRec[56].Options := '';

    xMethodRec[57].Valid := true;
    xMethodRec[57].name := xMethodName;
    xMethodRec[57].Seq := 580;
    xMethodRec[57].Action := 'STORE';
    xMethodRec[57].Inactive := false;
    xMethodRec[57].Options := 'STOREKEY=_0Status,STOREVALUE=Start';

    xMethodRec[58].Valid := true;
    xMethodRec[58].name := xMethodName;
    xMethodRec[58].Seq := 590;
    xMethodRec[58].Action := 'BLANK';
    xMethodRec[58].Inactive := false;
    xMethodRec[58].Options := '';

    xMethodRec[59].Valid := true;
    xMethodRec[59].name := xMethodName;
    xMethodRec[59].Seq := 600;
    xMethodRec[59].Action := 'BLANK';
    xMethodRec[59].Inactive := false;
    xMethodRec[59].Options := '';

    xMethodRec[60].Valid := true;
    xMethodRec[60].name := xMethodName;
    xMethodRec[60].Seq := 610;
    xMethodRec[60].Action := 'STORE';
    xMethodRec[60].Inactive := false;
    xMethodRec[60].Options := 'STOREKEY=_0Tip,STOREVALUE=_0TipArray[_$0TipCounter]';

    xMethodRec[61].Valid := true;
    xMethodRec[61].name := xMethodName;
    xMethodRec[61].Seq := 620;
    xMethodRec[61].Action := 'SLSQL';
    xMethodRec[61].Inactive := false;
    xMethodRec[61].Options :=
        'SLSQLASARRAY=YES,SLSQLFILE=CALIBGetVolcorrData,SLSQLPARAMS={_0VolcorrName, _0Tip},SLSQLSTORE={_0VolcorrPipDeviceNameArray,_0VolcorrVolumeArray,_0VolcorrFactorArray}';

    xMethodRec[62].Valid := true;
    xMethodRec[62].name := xMethodName;
    xMethodRec[62].Seq := 630;
    xMethodRec[62].Action := 'STORE';
    xMethodRec[62].Inactive := false;
    xMethodRec[62].Options := 'STOREKEY=_$0NoOfLines,STOREVALUE=COUNT(_0VolcorrFactorArray)';

    xMethodRec[63].Valid := true;
    xMethodRec[63].name := xMethodName;
    xMethodRec[63].Seq := 640;
    xMethodRec[63].Action := 'STORE';
    xMethodRec[63].Inactive := false;
    xMethodRec[63].Options := 'STOREKEY=_$0CountLines,STOREVALUE=1';

    xMethodRec[64].Valid := true;
    xMethodRec[64].name := xMethodName;
    xMethodRec[64].Seq := 650;
    xMethodRec[64].Action := 'BLANK';
    xMethodRec[64].Inactive := false;
    xMethodRec[64].Options := '';

    xMethodRec[65].Valid := true;
    xMethodRec[65].name := xMethodName;
    xMethodRec[65].Seq := 660;
    xMethodRec[65].Action := 'BLANK';
    xMethodRec[65].Inactive := false;
    xMethodRec[65].Options := '';

    xMethodRec[66].Valid := true;
    xMethodRec[66].name := xMethodName;
    xMethodRec[66].Seq := 670;
    xMethodRec[66].Action := 'WHILE';
    xMethodRec[66].Inactive := false;
    xMethodRec[66].Options := 'CONDITION=_$0CountLines <= _$0NoOfLines';

    xMethodRec[67].Valid := true;
    xMethodRec[67].name := xMethodName;
    xMethodRec[67].Seq := 680;
    xMethodRec[67].Action := 'STORE';
    xMethodRec[67].Inactive := false;
    xMethodRec[67].Options :=
        'STOREKEY=_0VolcorrPipDeviceName,STOREVALUE=_0VolcorrPipDeviceNameArray[_$0CountLines]';

    xMethodRec[68].Valid := true;
    xMethodRec[68].name := xMethodName;
    xMethodRec[68].Seq := 690;
    xMethodRec[68].Action := 'STORE';
    xMethodRec[68].Inactive := false;
    xMethodRec[68].Options :=
        'STOREKEY=_0VolcorrVolume,STOREVALUE={CAST(_0VolcorrVolumeArray[_$0CountLines], FLOAT)}';

    xMethodRec[69].Valid := true;
    xMethodRec[69].name := xMethodName;
    xMethodRec[69].Seq := 700;
    xMethodRec[69].Action := 'STORE';
    xMethodRec[69].Inactive := false;
    xMethodRec[69].Options :=
        'STOREKEY=_0VolcorrFactor,STOREVALUE={CAST(_0VolcorrFactorArray[_$0CountLines], FLOAT)}';

    xMethodRec[70].Valid := true;
    xMethodRec[70].name := xMethodName;
    xMethodRec[70].Seq := 710;
    xMethodRec[70].Action := 'STORE';
    xMethodRec[70].Inactive := false;
    xMethodRec[70].Options := 'STOREKEY=_0VolcorrTip,STOREVALUE=_0Tip';

    xMethodRec[71].Valid := true;
    xMethodRec[71].name := xMethodName;
    xMethodRec[71].Seq := 720;
    xMethodRec[71].Action := 'BLANK';
    xMethodRec[71].Inactive := false;
    xMethodRec[71].Options := '';

    xMethodRec[72].Valid := true;
    xMethodRec[72].name := xMethodName;
    xMethodRec[72].Seq := 730;
    xMethodRec[72].Action := 'STORE';
    xMethodRec[72].Inactive := false;
    xMethodRec[72].Options := 'STOREKEY=_0TipBit,STOREVALUE=(2 POWER (_0Tip - 1))';

    xMethodRec[73].Valid := true;
    xMethodRec[73].name := xMethodName;
    xMethodRec[73].Seq := 740;
    xMethodRec[73].Action := 'BLANK';
    xMethodRec[73].Inactive := false;
    xMethodRec[73].Options := '';

    xMethodRec[74].Valid := true;
    xMethodRec[74].name := xMethodName;
    xMethodRec[74].Seq := 750;
    xMethodRec[74].Action := 'IF';
    xMethodRec[74].Inactive := false;
    xMethodRec[74].Options := 'CONDITION=_$0LastTip<> _0VolcorrTip';

    xMethodRec[75].Valid := true;
    xMethodRec[75].name := xMethodName;
    xMethodRec[75].Seq := 760;
    xMethodRec[75].Action := 'FLUSH';
    xMethodRec[75].Inactive := false;
    xMethodRec[75].Options :=
        'DILUENT=_0SystemLiq,FLUSHCYCLES=1,FLUSHPIPDEV=_0VolcorrPipDeviceName,FLUSHTIPMAP=_0TipBit,FLUSHVOLUME=2000';

    xMethodRec[76].Valid := true;
    xMethodRec[76].name := xMethodName;
    xMethodRec[76].Seq := 770;
    xMethodRec[76].Action := 'STORE';
    xMethodRec[76].Inactive := false;
    xMethodRec[76].Options := 'STOREKEY=_$0LastTip,STOREVALUE=_0VolcorrTip';

    xMethodRec[77].Valid := true;
    xMethodRec[77].name := xMethodName;
    xMethodRec[77].Seq := 780;
    xMethodRec[77].Action := 'IFEND';
    xMethodRec[77].Inactive := false;
    xMethodRec[77].Options := '';

    xMethodRec[78].Valid := true;
    xMethodRec[78].name := xMethodName;
    xMethodRec[78].Seq := 790;
    xMethodRec[78].Action := 'STORE';
    xMethodRec[78].Inactive := false;
    xMethodRec[78].Options := 'STOREKEY=_0Status,STOREVALUE=Start';

    xMethodRec[79].Valid := true;
    xMethodRec[79].name := xMethodName;
    xMethodRec[79].Seq := 800;
    xMethodRec[79].Action := 'STORE';
    xMethodRec[79].Inactive := false;
    xMethodRec[79].Options := 'STOREKEY=_$0CV,STOREVALUE=0.0';

    xMethodRec[80].Valid := true;
    xMethodRec[80].name := xMethodName;
    xMethodRec[80].Seq := 810;
    xMethodRec[80].Action := 'STORE';
    xMethodRec[80].Inactive := false;
    xMethodRec[80].Options := 'STOREKEY=_$0Error,STOREVALUE=0.0';

    xMethodRec[81].Valid := true;
    xMethodRec[81].name := xMethodName;
    xMethodRec[81].Seq := 820;
    xMethodRec[81].Action := 'STORE';
    xMethodRec[81].Inactive := false;
    xMethodRec[81].Options := 'STOREKEY=_$0CountTries,STOREVALUE=1';

    xMethodRec[82].Valid := true;
    xMethodRec[82].name := xMethodName;
    xMethodRec[82].Seq := 830;
    xMethodRec[82].Action := 'WHILE';
    xMethodRec[82].Inactive := false;
    xMethodRec[82].Options := 'CONDITION=_$0CountTries <= _0CountTries';

    xMethodRec[83].Valid := true;
    xMethodRec[83].name := xMethodName;
    xMethodRec[83].Seq := 840;
    xMethodRec[83].Action := 'IF';
    xMethodRec[83].Inactive := false;
    xMethodRec[83].Options := 'CONDITION=_0Status <> Done';

    xMethodRec[84].Valid := true;
    xMethodRec[84].name := xMethodName;
    xMethodRec[84].Seq := 850;
    xMethodRec[84].Action := 'STORE';
    xMethodRec[84].Inactive := false;
    xMethodRec[84].Options := 'STOREKEY=_0Status,STOREVALUE=Done';

    xMethodRec[85].Valid := true;
    xMethodRec[85].name := xMethodName;
    xMethodRec[85].Seq := 860;
    xMethodRec[85].Action := 'IF';
    xMethodRec[85].Inactive := false;
    xMethodRec[85].Options := 'CONDITION=_$0TotalVol + _0VolcorrVolume >= _$0MaxVolOnBalance';

    xMethodRec[86].Valid := true;
    xMethodRec[86].name := xMethodName;
    xMethodRec[86].Seq := 870;
    xMethodRec[86].Action := 'MSG';
    xMethodRec[86].Inactive := false;
    xMethodRec[86].Options := 'MSGPAUSERUN=YES,MSGTEXT=Empty Vial on Balance';

    xMethodRec[87].Valid := true;
    xMethodRec[87].name := xMethodName;
    xMethodRec[87].Seq := 880;
    xMethodRec[87].Action := 'STORE';
    xMethodRec[87].Inactive := false;
    xMethodRec[87].Options := 'STOREKEY=_$0TotalVol,STOREVALUE=0';

    xMethodRec[88].Valid := true;
    xMethodRec[88].name := xMethodName;
    xMethodRec[88].Seq := 890;
    xMethodRec[88].Action := 'IFEND';
    xMethodRec[88].Inactive := false;
    xMethodRec[88].Options := '';

    xMethodRec[89].Valid := true;
    xMethodRec[89].name := xMethodName;
    xMethodRec[89].Seq := 900;
    xMethodRec[89].Action := 'STORE';
    xMethodRec[89].Inactive := false;
    xMethodRec[89].Options := 'STOREKEY=_$0CountSteps,STOREVALUE=1';

    xMethodRec[90].Valid := true;
    xMethodRec[90].name := xMethodName;
    xMethodRec[90].Seq := 910;
    xMethodRec[90].Action := 'BLANK';
    xMethodRec[90].Inactive := false;
    xMethodRec[90].Options := '';

    xMethodRec[91].Valid := true;
    xMethodRec[91].name := xMethodName;
    xMethodRec[91].Seq := 920;
    xMethodRec[91].Action := 'WHILE';
    xMethodRec[91].Inactive := false;
    xMethodRec[91].Options := 'CONDITION=_$0CountSteps <= _0CountSteps';

    xMethodRec[92].Valid := true;
    xMethodRec[92].name := xMethodName;
    xMethodRec[92].Seq := 930;
    xMethodRec[92].Action := 'DELAY';
    xMethodRec[92].Inactive := false;
    xMethodRec[92].Options := 'DELAYTIME=_0BalanceWaitTime,DELAYTMUNIT=sec';

    xMethodRec[93].Valid := true;
    xMethodRec[93].name := xMethodName;
    xMethodRec[93].Seq := 940;
    xMethodRec[93].Action := 'TARE';
    xMethodRec[93].Inactive := false;
    xMethodRec[93].Options := 'TAREBALANCE=Balance';

    xMethodRec[94].Valid := true;
    xMethodRec[94].name := xMethodName;
    xMethodRec[94].Seq := 950;
    xMethodRec[94].Action := 'IF';
    xMethodRec[94].Inactive := false;
    xMethodRec[94].Options := 'CONDITION={_0CalibrateWithSysLiq = -1}';

    xMethodRec[95].Valid := true;
    xMethodRec[95].name := xMethodName;
    xMethodRec[95].Seq := 960;
    xMethodRec[95].Action := 'PIPET';
    xMethodRec[95].Inactive := false;
    xMethodRec[95].Options :=
        'DESTFIRSTPOS=_0DestPos,DESTLASTPOS=_0DestPos,DESTRACK=BALANCE,DILUENT=_0SystemLiq,DILVOL=_0VolcorrVolume,LIQPARA=_0LQH,SOURCEFIRSTPOS=_0SourcePos,SOURCELASTPOS=_0SourcePos,SOURCERACK=_0SourceRack,USEDPIPDEVICE=_0VolcorrPipDeviceName,USEDTIPS=_0TipBit';

    xMethodRec[96].Valid := true;
    xMethodRec[96].name := xMethodName;
    xMethodRec[96].Seq := 970;
    xMethodRec[96].Action := 'IFEND';
    xMethodRec[96].Inactive := false;
    xMethodRec[96].Options := '';

    xMethodRec[97].Valid := true;
    xMethodRec[97].name := xMethodName;
    xMethodRec[97].Seq := 980;
    xMethodRec[97].Action := 'IF';
    xMethodRec[97].Inactive := false;
    xMethodRec[97].Options := 'CONDITION={_0CalibrateWithSysLiq = 0}';

    xMethodRec[98].Valid := true;
    xMethodRec[98].name := xMethodName;
    xMethodRec[98].Seq := 990;
    xMethodRec[98].Action := 'PIPET';
    xMethodRec[98].Inactive := false;
    xMethodRec[98].Options :=
        'DESTFIRSTPOS=_0DestPos,DESTLASTPOS=_0DestPos,DESTRACK=BALANCE,DESTVOL=_0VolcorrVolume,DILUENT=_0SystemLiq,LIQPARA=_0LQH,SOURCEFIRSTPOS=_0SourcePos,SOURCELASTPOS=_0SourcePos,SOURCERACK=_0SourceRack,USEDPIPDEVICE=_0VolcorrPipDeviceName,USEDTIPS=_0TipBit';

    xMethodRec[99].Valid := true;
    xMethodRec[99].name := xMethodName;
    xMethodRec[99].Seq := 1000;
    xMethodRec[99].Action := 'IFEND';
    xMethodRec[99].Inactive := false;
    xMethodRec[99].Options := '';

    xMethodRec[100].Valid := true;
    xMethodRec[100].name := xMethodName;
    xMethodRec[100].Seq := 1010;
    xMethodRec[100].Action := 'BLANK';
    xMethodRec[100].Inactive := false;
    xMethodRec[100].Options := '';

    xMethodRec[101].Valid := true;
    xMethodRec[101].name := xMethodName;
    xMethodRec[101].Seq := 1020;
    xMethodRec[101].Action := 'DELAY';
    xMethodRec[101].Inactive := false;
    xMethodRec[101].Options := 'DELAYTIME=_0BalanceWaitTime,DELAYTMUNIT=sec';

    xMethodRec[102].Valid := true;
    xMethodRec[102].name := xMethodName;
    xMethodRec[102].Seq := 1030;
    xMethodRec[102].Action := 'WGHP';
    xMethodRec[102].Inactive := false;
    xMethodRec[102].Options :=
        'SOURCEFIRSTPOS=_0DestPos,SOURCELASTPOS=_0DestPos,SOURCERACK=BALANCE,WEIGHTSTOREKEY=_0CurrentWeight,WGHPBALANCE=BALANCE,WGHPSTOREASTARE=YES,WGHPWAIT=YES';

    xMethodRec[103].Valid := true;
    xMethodRec[103].name := xMethodName;
    xMethodRec[103].Seq := 1040;
    xMethodRec[103].Action := 'STORE';
    xMethodRec[103].Inactive := false;
    xMethodRec[103].Options :=
        'STOREKEY=_0CurrentWeight,STOREVALUE=(ROUND(_0CurrentWeight*_0BalanceAccuracy))/_0BalanceAccuracy';

    xMethodRec[104].Valid := true;
    xMethodRec[104].name := xMethodName;
    xMethodRec[104].Seq := 1050;
    xMethodRec[104].Action := 'STORE';
    xMethodRec[104].Inactive := false;
    xMethodRec[104].Options := 'STOREKEY=_$0Error,STOREVALUE=(_0VolcorrVolume - (_0CurrentWeight/_0Density))';

    xMethodRec[105].Valid := true;
    xMethodRec[105].name := xMethodName;
    xMethodRec[105].Seq := 1060;
    xMethodRec[105].Action := 'BLANK';
    xMethodRec[105].Inactive := false;
    xMethodRec[105].Options := '';

    xMethodRec[106].Valid := true;
    xMethodRec[106].name := xMethodName;
    xMethodRec[106].Seq := 1070;
    xMethodRec[106].Action := 'IF';
    xMethodRec[106].Inactive := false;
    xMethodRec[106].Options :=
        'CONDITION=(_$0Error > (_0PipettingTolerancePercent*_0VolcorrVolume)) || (_$0Error < ((-1) *_0PipettingTolerancePercent*_0VolcorrVolume))';

    xMethodRec[107].Valid := true;
    xMethodRec[107].name := xMethodName;
    xMethodRec[107].Seq := 1080;
    xMethodRec[107].Action := 'STORE';
    xMethodRec[107].Inactive := false;
    xMethodRec[107].Options := 'STOREKEY=_0Status,STOREVALUE=FAILED';

    xMethodRec[108].Valid := true;
    xMethodRec[108].name := xMethodName;
    xMethodRec[108].Seq := 1090;
    xMethodRec[108].Action := 'IFEND';
    xMethodRec[108].Inactive := false;
    xMethodRec[108].Options := '';

    xMethodRec[109].Valid := true;
    xMethodRec[109].name := xMethodName;
    xMethodRec[109].Seq := 1100;
    xMethodRec[109].Action := 'BLANK';
    xMethodRec[109].Inactive := false;
    xMethodRec[109].Options := '';

    xMethodRec[110].Valid := true;
    xMethodRec[110].name := xMethodName;
    xMethodRec[110].Seq := 1110;
    xMethodRec[110].Action := 'USQL';
    xMethodRec[110].Inactive := false;
    xMethodRec[110].Options :=
        'USQLFILE=CALIBInsertPipetingSteps,USQLPARAMS={_$0CountTries, _$0CountSteps, _0VolcorrName, _0VolcorrPipDeviceName, _0Tip, _0VolcorrVolume, _$0Error, _0VolcorrFactor, _0CurrentWeight, _0LQH}';

    xMethodRec[111].Valid := true;
    xMethodRec[111].name := xMethodName;
    xMethodRec[111].Seq := 1120;
    xMethodRec[111].Action := 'BLANK';
    xMethodRec[111].Inactive := false;
    xMethodRec[111].Options := '';

    xMethodRec[112].Valid := true;
    xMethodRec[112].name := xMethodName;
    xMethodRec[112].Seq := 1130;
    xMethodRec[112].Action := 'STORE';
    xMethodRec[112].Inactive := false;
    xMethodRec[112].Options := 'STOREKEY=_$0CountSteps,STOREVALUE=_$0CountSteps +1';

    xMethodRec[113].Valid := true;
    xMethodRec[113].name := xMethodName;
    xMethodRec[113].Seq := 1140;
    xMethodRec[113].Action := 'STORE';
    xMethodRec[113].Inactive := false;
    xMethodRec[113].Options := 'STOREKEY=_$0TotalVol,STOREVALUE=_$0TotalVol + _0VolcorrVolume';

    xMethodRec[114].Valid := true;
    xMethodRec[114].name := xMethodName;
    xMethodRec[114].Seq := 1150;
    xMethodRec[114].Action := 'WHEND';
    xMethodRec[114].Inactive := false;
    xMethodRec[114].Options := '';

    xMethodRec[115].Valid := true;
    xMethodRec[115].name := xMethodName;
    xMethodRec[115].Seq := 1160;
    xMethodRec[115].Action := 'BLANK';
    xMethodRec[115].Inactive := false;
    xMethodRec[115].Options := '';

    xMethodRec[116].Valid := true;
    xMethodRec[116].name := xMethodName;
    xMethodRec[116].Seq := 1170;
    xMethodRec[116].Action := 'STORE';
    xMethodRec[116].Inactive := false;
    xMethodRec[116].Options := 'STOREKEY=_0MinError,STOREVALUE=0';

    xMethodRec[117].Valid := true;
    xMethodRec[117].name := xMethodName;
    xMethodRec[117].Seq := 1180;
    xMethodRec[117].Action := 'STORE';
    xMethodRec[117].Inactive := false;
    xMethodRec[117].Options := 'STOREKEY=_0MaxError,STOREVALUE=0';

    xMethodRec[118].Valid := true;
    xMethodRec[118].name := xMethodName;
    xMethodRec[118].Seq := 1190;
    xMethodRec[118].Action := 'BLANK';
    xMethodRec[118].Inactive := false;
    xMethodRec[118].Options := '';

    xMethodRec[119].Valid := true;
    xMethodRec[119].name := xMethodName;
    xMethodRec[119].Seq := 1200;
    xMethodRec[119].Action := 'BLANK';
    xMethodRec[119].Inactive := false;
    xMethodRec[119].Options := '';

    xMethodRec[120].Valid := true;
    xMethodRec[120].name := xMethodName;
    xMethodRec[120].Seq := 1210;
    xMethodRec[120].Action := 'SLSQL';
    xMethodRec[120].Inactive := false;
    xMethodRec[120].Options :=
        'SLSQLASARRAY=NO,SLSQLFILE=CALIBGetAvgWeight,SLSQLPARAMS={_$0CountTries,_0VolcorrName,_0VolcorrPipDeviceName,_0Tip,_0VolcorrVolume},SLSQLSTORE=_0AvgWeight';

    xMethodRec[121].Valid := true;
    xMethodRec[121].name := xMethodName;
    xMethodRec[121].Seq := 1220;
    xMethodRec[121].Action := 'SLSQL';
    xMethodRec[121].Inactive := false;
    xMethodRec[121].Options :=
        'SLSQLASARRAY=YES,SLSQLFILE=CALIBGetAllWeights,SLSQLPARAMS={_$0CountTries,_0VolcorrName,_0VolcorrPipDeviceName,_0Tip,_0VolcorrVolume},SLSQLSTORE=_0Weights';

    xMethodRec[122].Valid := true;
    xMethodRec[122].name := xMethodName;
    xMethodRec[122].Seq := 1230;
    xMethodRec[122].Action := 'SLSQL';
    xMethodRec[122].Inactive := false;
    xMethodRec[122].Options :=
        'SLSQLASARRAY=YES,SLSQLFILE=CALIBGetErrorArray,SLSQLPARAMS={_$0CountTries,_0VolcorrName,_0VolcorrPipDeviceName,_0Tip,_0VolcorrVolume},SLSQLSTORE=_0ErrorArray';

    xMethodRec[123].Valid := true;
    xMethodRec[123].name := xMethodName;
    xMethodRec[123].Seq := 1240;
    xMethodRec[123].Action := 'SLSQL';
    xMethodRec[123].Inactive := false;
    xMethodRec[123].Options :=
        'SLSQLFILE=CALIBGetMaxError,SLSQLPARAMS={_$0CountTries,_0VolcorrName,_0VolcorrPipDeviceName,_0Tip,_0VolcorrVolume},SLSQLSTORE=_0MaxError';

    xMethodRec[124].Valid := true;
    xMethodRec[124].name := xMethodName;
    xMethodRec[124].Seq := 1250;
    xMethodRec[124].Action := 'SLSQL';
    xMethodRec[124].Inactive := false;
    xMethodRec[124].Options :=
        'SLSQLFILE=CALIBGetMinError,SLSQLPARAMS={_$0CountTries,_0VolcorrName,_0VolcorrPipDeviceName,_0Tip,_0VolcorrVolume},SLSQLSTORE=_0MinError';

    xMethodRec[125].Valid := true;
    xMethodRec[125].name := xMethodName;
    xMethodRec[125].Seq := 1260;
    xMethodRec[125].Action := 'BLANK';
    xMethodRec[125].Inactive := false;
    xMethodRec[125].Options := '';

    xMethodRec[126].Valid := true;
    xMethodRec[126].name := xMethodName;
    xMethodRec[126].Seq := 1270;
    xMethodRec[126].Action := 'IF';
    xMethodRec[126].Inactive := false;
    xMethodRec[126].Options := 'CONDITION=_0MinError > _0MaxError';

    xMethodRec[127].Valid := true;
    xMethodRec[127].name := xMethodName;
    xMethodRec[127].Seq := 1280;
    xMethodRec[127].Action := 'STORE';
    xMethodRec[127].Inactive := false;
    xMethodRec[127].Options := 'STOREKEY=_0MaxError,STOREVALUE=_0MinError';

    xMethodRec[128].Valid := true;
    xMethodRec[128].name := xMethodName;
    xMethodRec[128].Seq := 1290;
    xMethodRec[128].Action := 'IFEND';
    xMethodRec[128].Inactive := false;
    xMethodRec[128].Options := '';

    xMethodRec[129].Valid := true;
    xMethodRec[129].name := xMethodName;
    xMethodRec[129].Seq := 1300;
    xMethodRec[129].Action := 'BLANK';
    xMethodRec[129].Inactive := false;
    xMethodRec[129].Options := '';

    xMethodRec[130].Valid := true;
    xMethodRec[130].name := xMethodName;
    xMethodRec[130].Seq := 1310;
    xMethodRec[130].Action := 'STORE';
    xMethodRec[130].Inactive := false;
    xMethodRec[130].Options := 'STOREKEY=_0CV,STOREVALUE=_0CVTolerance - 1';

    xMethodRec[131].Valid := true;
    xMethodRec[131].name := xMethodName;
    xMethodRec[131].Seq := 1320;
    xMethodRec[131].Action := 'STORE';
    xMethodRec[131].Inactive := false;
    xMethodRec[131].Options := 'STOREKEY=_0AvgVol,STOREVALUE=_0AvgWeight/_0Density';

    xMethodRec[132].Valid := true;
    xMethodRec[132].name := xMethodName;
    xMethodRec[132].Seq := 1330;
    xMethodRec[132].Action := 'USQL';
    xMethodRec[132].Inactive := true;
    xMethodRec[132].Options :=
        'USQLFILE=CALIBUpdateCV,USQLPARAMS={_0CV, _$0CountTries, _0VolcorrName, _0VolcorrPipDeviceName, _0Tip, _0VolcorrVolume}';

    xMethodRec[133].Valid := true;
    xMethodRec[133].name := xMethodName;
    xMethodRec[133].Seq := 1340;
    xMethodRec[133].Action := 'BLANK';
    xMethodRec[133].Inactive := false;
    xMethodRec[133].Options := '';

    xMethodRec[134].Valid := true;
    xMethodRec[134].name := xMethodName;
    xMethodRec[134].Seq := 1350;
    xMethodRec[134].Action := 'IF';
    xMethodRec[134].Inactive := false;
    xMethodRec[134].Options := 'CONDITION=((_0Status == FAILED ))';

    xMethodRec[135].Valid := true;
    xMethodRec[135].name := xMethodName;
    xMethodRec[135].Seq := 1360;
    xMethodRec[135].Action := 'STORE';
    xMethodRec[135].Inactive := false;
    xMethodRec[135].Options := 'STOREKEY=_0NewFactor,STOREVALUE=_0VolcorrFactor*_0VolcorrVolume';

    xMethodRec[136].Valid := true;
    xMethodRec[136].name := xMethodName;
    xMethodRec[136].Seq := 1370;
    xMethodRec[136].Action := 'STORE';
    xMethodRec[136].Inactive := false;
    xMethodRec[136].Options :=
        'STOREKEY=_0NewFactor,STOREVALUE=(ROUND(1000 * (_0NewFactor/_0AvgVol)) / 1000)';

    xMethodRec[137].Valid := true;
    xMethodRec[137].name := xMethodName;
    xMethodRec[137].Seq := 1380;
    xMethodRec[137].Action := 'USQL';
    xMethodRec[137].Inactive := false;
    xMethodRec[137].Options :=
        'USQLFILE=CALIBUpdateVolcorr,USQLPARAMS={_0NewFactor, _$0CountTries, _0VolcorrName, _0VolcorrPipDeviceName, _0Tip, _0VolcorrVolume}';

    xMethodRec[138].Valid := true;
    xMethodRec[138].name := xMethodName;
    xMethodRec[138].Seq := 1390;
    xMethodRec[138].Action := 'USQL';
    xMethodRec[138].Inactive := false;
    xMethodRec[138].Options :=
        'USQLFILE=CALIBUpdateResults,USQLPARAMS={_0NewFactor, _$0CountTries, _0VolcorrName, _0VolcorrPipDeviceName, _0Tip, _0VolcorrVolume}';

    xMethodRec[139].Valid := true;
    xMethodRec[139].name := xMethodName;
    xMethodRec[139].Seq := 1400;
    xMethodRec[139].Action := 'STORE';
    xMethodRec[139].Inactive := false;
    xMethodRec[139].Options := 'STOREKEY=_0VolcorrFactor,STOREVALUE=_0NewFactor';

    xMethodRec[140].Valid := true;
    xMethodRec[140].name := xMethodName;
    xMethodRec[140].Seq := 1410;
    xMethodRec[140].Action := 'BLANK';
    xMethodRec[140].Inactive := false;
    xMethodRec[140].Options := '';

    xMethodRec[141].Valid := true;
    xMethodRec[141].name := xMethodName;
    xMethodRec[141].Seq := 1420;
    xMethodRec[141].Action := 'IFEND';
    xMethodRec[141].Inactive := false;
    xMethodRec[141].Options := '';

    xMethodRec[142].Valid := true;
    xMethodRec[142].name := xMethodName;
    xMethodRec[142].Seq := 1430;
    xMethodRec[142].Action := 'IF';
    xMethodRec[142].Inactive := false;
    xMethodRec[142].Options := 'CONDITION=(_0Status <> FAILED)';

    xMethodRec[143].Valid := true;
    xMethodRec[143].name := xMethodName;
    xMethodRec[143].Seq := 1440;
    xMethodRec[143].Action := 'STORE';
    xMethodRec[143].Inactive := false;
    xMethodRec[143].Options := 'STOREKEY=_0Status,STOREVALUE=Done';

    xMethodRec[144].Valid := true;
    xMethodRec[144].name := xMethodName;
    xMethodRec[144].Seq := 1450;
    xMethodRec[144].Action := 'IFEND';
    xMethodRec[144].Inactive := false;
    xMethodRec[144].Options := '';

    xMethodRec[145].Valid := true;
    xMethodRec[145].name := xMethodName;
    xMethodRec[145].Seq := 1460;
    xMethodRec[145].Action := 'BLANK';
    xMethodRec[145].Inactive := false;
    xMethodRec[145].Options := '';

    xMethodRec[146].Valid := true;
    xMethodRec[146].name := xMethodName;
    xMethodRec[146].Seq := 1470;
    xMethodRec[146].Action := 'USQL';
    xMethodRec[146].Inactive := false;
    xMethodRec[146].Options :=
        'USQLFILE=CALIBUpdateStatus,USQLPARAMS={_0Status, _$0CountTries, _0VolcorrName, _0VolcorrPipDeviceName, _0Tip, _0VolcorrVolume}';

    xMethodRec[147].Valid := true;
    xMethodRec[147].name := xMethodName;
    xMethodRec[147].Seq := 1480;
    xMethodRec[147].Action := 'IFEND';
    xMethodRec[147].Inactive := false;
    xMethodRec[147].Options := '';

    xMethodRec[148].Valid := true;
    xMethodRec[148].name := xMethodName;
    xMethodRec[148].Seq := 1490;
    xMethodRec[148].Action := 'STORE';
    xMethodRec[148].Inactive := false;
    xMethodRec[148].Options := 'STOREKEY=_$0CountTries,STOREVALUE=_$0CountTries + 1';

    xMethodRec[149].Valid := true;
    xMethodRec[149].name := xMethodName;
    xMethodRec[149].Seq := 1500;
    xMethodRec[149].Action := 'BLANK';
    xMethodRec[149].Inactive := false;
    xMethodRec[149].Options := '';

    xMethodRec[150].Valid := true;
    xMethodRec[150].name := xMethodName;
    xMethodRec[150].Seq := 1510;
    xMethodRec[150].Action := 'WHEND';
    xMethodRec[150].Inactive := false;
    xMethodRec[150].Options := '';

    xMethodRec[151].Valid := true;
    xMethodRec[151].name := xMethodName;
    xMethodRec[151].Seq := 1520;
    xMethodRec[151].Action := 'STORE';
    xMethodRec[151].Inactive := false;
    xMethodRec[151].Options := 'STOREKEY=_$0CountLines,STOREVALUE=_$0CountLines + 1';

    xMethodRec[152].Valid := true;
    xMethodRec[152].name := xMethodName;
    xMethodRec[152].Seq := 1530;
    xMethodRec[152].Action := 'WHEND';
    xMethodRec[152].Inactive := false;
    xMethodRec[152].Options := '';

    xMethodRec[153].Valid := true;
    xMethodRec[153].name := xMethodName;
    xMethodRec[153].Seq := 1540;
    xMethodRec[153].Action := 'BLANK';
    xMethodRec[153].Inactive := false;
    xMethodRec[153].Options := '';

    xMethodRec[154].Valid := true;
    xMethodRec[154].name := xMethodName;
    xMethodRec[154].Seq := 1550;
    xMethodRec[154].Action := 'STORE';
    xMethodRec[154].Inactive := false;
    xMethodRec[154].Options := 'STOREKEY=_$0TipCounter,STOREVALUE=_$0TipCounter + 1';

    xMethodRec[155].Valid := true;
    xMethodRec[155].name := xMethodName;
    xMethodRec[155].Seq := 1560;
    xMethodRec[155].Action := 'WHEND';
    xMethodRec[155].Inactive := false;
    xMethodRec[155].Options := '';

    xMethodRec[156].Valid := true;
    xMethodRec[156].name := xMethodName;
    xMethodRec[156].Seq := 1570;
    xMethodRec[156].Action := 'BLANK';
    xMethodRec[156].Inactive := false;
    xMethodRec[156].Options := '';

    xMethodRec[157].Valid := true;
    xMethodRec[157].name := xMethodName;
    xMethodRec[157].Seq := 1580;
    xMethodRec[157].Action := 'WRSQL';
    xMethodRec[157].Inactive := false;
    xMethodRec[157].Options := 'WRSQLFILE=CALIBExportData,WRSQLSHOW=YES';

    xMethodRec[158].Valid := true;
    xMethodRec[158].name := xMethodName;
    xMethodRec[158].Seq := 1590;
    xMethodRec[158].Action := 'USQL';
    xMethodRec[158].Inactive := false;
    xMethodRec[158].Options := 'USQLFILE=CALIBSQLDelete';

    // Save the temporary method
    TMethodDataAdaptorExt.SaveTemporaryMethod(xMethodRec, TRunGlobals.Instance.LayoutName);

    // All the SQL statements to export
    xCALIBCreateTables.name := 'CALIBCreateTables';
    xCALIBCreateTables.Term := 'create table CALIBPresets (Name CHAR(40), SystemLiq CHAR(40),' +
        ' Density DOUBLE(4), Steps INTEGER, Tries INTEGER, MaxCV DOUBLE(4), MaxError DOUBLE(4)' +
        ', UseSySLiq BOOLEAN, ManualFill INTEGER, BalanceWait INTEGER, BalancePrecision INTEGER' +
        ', TipArray CHAR(40), LHP CHAR(40));' +
        'create table CALIBResults(Status CHAR(40), try INTEGER, Step INTEGER, VolCorrName CHAR(40),' +
        'PipDeviceName CHAR(40), Tip INTEGER, PipVol INTEGER, Error DOUBLE(4), Factor DOUBLE(4),' +
        'OldFactor DOUBLE(4), Weight DOUBLE(4), LQHP CHAR(40), DateTime TIMESTAMP)';
    xCALIBCreateTables.Defaultargs := '';
    xCALIBCreateTables.Comment := '';

    xCALIBExportData.name := 'CALIBExportData';
    xCALIBExportData.Term := 'Select * from CalibResults';
    xCALIBExportData.Defaultargs := '';
    xCALIBExportData.Comment := '';

    xCALIBGetAllWeights.name := 'CALIBGetAllWeights';
    xCALIBGetAllWeights.Term :=
        'Select Weight from Calibresults Where     Try = #P0# AND        VolcorrName = ' + chr(39) + '#P1#' +
        chr(39) + ' AND        Pipdevicename = ' + chr(39) + '#P2#' + chr(39) +
        ' AND        Tip = #P3# AND        PipVol = #P4#';
    xCALIBGetAllWeights.Defaultargs := '';
    xCALIBGetAllWeights.Comment := '';

    xCALIBGetAvgWeight.name := 'CALIBGetAvgWeight';
    xCALIBGetAvgWeight.Term :=
        'Select AVG(Weight) from Calibresults Where     Try = #P0# AND        VolcorrName = ' + chr(39) +
        '#P1#' + chr(39) + ' AND        Pipdevicename = ' + chr(39) + '#P2#' + chr(39) +
        ' AND        Tip = #P3# AND        PipVol = #P4#';
    xCALIBGetAvgWeight.Defaultargs := '';
    xCALIBGetAvgWeight.Comment := '';

    xCALIBGetErrorArray.name := 'CALIBGetErrorArray';
    xCALIBGetErrorArray.Term :=
        'Select Error from Calibresults Where     Try = #P0# AND        VolcorrName = ' + chr(39) + '#P1#' +
        chr(39) + ' AND        Pipdevicename = ' + chr(39) + '#P2#' + chr(39) +
        ' AND        Tip = #P3# AND        PipVol = #P4#';
    xCALIBGetErrorArray.Defaultargs := '';
    xCALIBGetErrorArray.Comment := '';

    xCALIBGetMaxError.name := 'CALIBGetMaxError';
    xCALIBGetMaxError.Term :=
        'Select Abs(Max(Error)) from Calibresults Where     Try = #P0# AND        VolcorrName = ' + chr(39) +
        '#P1#' + chr(39) + ' AND        Pipdevicename = ' + chr(39) + '#P2#' + chr(39) +
        ' AND        Tip = #P3# AND        PipVol = #P4#';
    xCALIBGetMaxError.Defaultargs := '';
    xCALIBGetMaxError.Comment := '';

    xCALIBGetMinError.name := 'CALIBGetMinError';
    xCALIBGetMinError.Term :=
        'Select Abs(Min(Error)) from Calibresults Where     Try = #P0# AND        VolcorrName = ' + chr(39) +
        '#P1#' + chr(39) + ' AND        Pipdevicename = ' + chr(39) + '#P2#' + chr(39) +
        ' AND        Tip = #P3# AND        PipVol = #P4#';
    xCALIBGetMinError.Defaultargs := '';
    xCALIBGetMinError.Comment := '';

    xCALIBGetVolcorrData.name := 'CALIBGetVolcorrData';
    xCALIBGetVolcorrData.Term := 'Select Pipdevicename, Volume, Factor  from Volcorr where Name = ' + chr(39)
        + '#P0#' + chr(39) + 'and Tip =#P1#';
    xCALIBGetVolcorrData.Defaultargs := '';
    xCALIBGetVolcorrData.Comment := '';

    xCALIBGetVolcurve.name := 'CALIBGetVolcurve';
    xCALIBGetVolcurve.Term := 'Select VolCorrCurve from LIQPARAM where PARAMNAME = ' + chr(39) + '#P0#'
        + chr(39);
    xCALIBGetVolcurve.Defaultargs := '';
    xCALIBGetVolcurve.Comment := '';

    xCALIBInsertPipetingSteps.name := 'CALIBInsertPipetingSteps';
    xCALIBInsertPipetingSteps.Term :=
        'Insert into CalibResults (Try, Step, VolCorrName, PipDeviceName, Tip, PipVol, Error, Factor, Weight, LQHP, DateTime)'
        + 'VALUES(#P0#,#P1#,' + chr(39) + '#P2#' + chr(39) + ',' + chr(39) + '#P3#' + chr(39) +
        ',#P4#,#P5#, Abs(#P6#),#P7#,#P8#,' + chr(39) + '#P9#' + chr(39) + ',Current_Timestamp) ';
    xCALIBInsertPipetingSteps.Defaultargs := '';
    xCALIBInsertPipetingSteps.Comment := '';

    xCALIBResultsDelete.name := 'CALIBResultsDelete';
    xCALIBResultsDelete.Term := 'Delete from CalibResults;';
    xCALIBResultsDelete.Defaultargs := '';
    xCALIBResultsDelete.Defaultargs := '';
    xCALIBResultsDelete.Comment := '';

    xCALIBSavePreset.name := 'CALIBSavePreset';
    xCALIBSavePreset.Term := 'Insert into CALIBPresets VALUES (' + chr(39) + '#P0#' + chr(39) + ',' + chr(39)
        + '#P1#' + chr(39) + ',' + chr(39) + '#P2#' + chr(39) + ',' + chr(39) + '#P3#' + chr(39) + ',' +
        chr(39) + '#P4#' + chr(39) + ',' + chr(39) + '#P5#' + chr(39) + ',' + chr(39) + '#P6#' + chr(39) + ','
        + chr(39) + '#P7#' + chr(39) + ',' + chr(39) + '#P8#' + chr(39) + ',' + chr(39) + '#P9#' + chr(39) +
        ',' + chr(39) + '#P10#' + chr(39) + ',' + chr(39) + '#P11#' + chr(39) + ',' + chr(39) + '#P12#' +
        chr(39) + ')';
    xCALIBSavePreset.Defaultargs := '';
    xCALIBSavePreset.Comment := '';

    xCALIBSQLDelete.name := 'CALIBSQLDelete';
    xCALIBSQLDelete.Term := 'Delete from SQLTERMS where LEFTSTR(Name,5) = ' + chr(39) + 'CALIB' + chr(39);
    xCALIBSQLDelete.Defaultargs := '';
    xCALIBSQLDelete.Comment := '';

    xCALIBUpdateVolcorr.name := 'CALIBUpdateVolcorr';
    xCALIBUpdateVolcorr.Term := 'Update Volcorr Set Factor = #P0# where Name = ' + chr(39) + '#P1#' + chr(39)
        + ' AND PipDeviceName = ' + chr(39) + '#P2#' + chr(39) + ' AND Tip = #P3# AND Volume = #P4#';
    xCALIBUpdateVolcorr.Defaultargs := '';
    xCALIBUpdateVolcorr.Comment := '';

    xCALIBTablesExist.name := 'CALIBTablesExist';
    xCALIBTablesExist.Term := 'Select * from SQLTERMS Where LEFTSTR(Name,5) = ' + chr(39) + 'CALIB' + chr(39);
    xCALIBTablesExist.Defaultargs := '';
    xCALIBTablesExist.Comment := '';

    xCALIBUpdateCV.name := 'CALIBUpdateCV';
    xCALIBUpdateCV.Term := 'Update  CalibResults Set CV= #P0# Where Try = #P1# AND        VolcorrName = ' +
        chr(39) + '#P2#' + chr(39) + ' AND        Pipdevicename = ' + chr(39) + '#P3#' + chr(39) +
        ' AND        Tip = #P4# AND        PipVol = #P5#';
    xCALIBUpdateCV.Defaultargs := '';
    xCALIBUpdateCV.Comment := '';

    xCALIBUpdateError.name := 'CALIBUpdateError';
    xCALIBUpdateError.Term :=
        'Update  CalibResults Set Error = #P0# Where Try = #P1# AND        VolcorrName = ' + chr(39) + '#P2#'
        + chr(39) + ' AND        Pipdevicename = ' + chr(39) + '#P3#' + chr(39) +
        ' AND        Tip = #P4# AND        PipVol = #P5#';
    xCALIBUpdateError.Defaultargs := '';
    xCALIBUpdateError.Comment := '';

    xCALIBUpdateResults.name := 'CALIBUpdateResults';
    xCALIBUpdateResults.Term :=
        'Update  CalibResults Set Factor = #P0# Where Try = #P1# AND        VolcorrName = ' + chr(39) + '#P2#'
        + chr(39) + ' AND        Pipdevicename = ' + chr(39) + '#P3#' + chr(39) +
        ' AND        Tip = #P4# AND        PipVol = #P5#';
    xCALIBUpdateResults.Defaultargs := '';
    xCALIBUpdateResults.Comment := '';

    xCALIBUpdateStatus.name := 'CALIBUpdateStatus';
    xCALIBUpdateStatus.Term := 'Update Calibresults set Status = ' + chr(39) + '#P0#' + chr(39) +
        ' Where     Try = #P1# AND        VolcorrName = ' + chr(39) + '#P2#' + chr(39) +
        ' AND        Pipdevicename = ' + chr(39) + '#P3#' + chr(39) +
        ' AND        Tip = #P4# AND        PipVol = #P5#';
    xCALIBUpdateStatus.Defaultargs := '';
    xCALIBUpdateStatus.Comment := '';

    xCALIBUpdateVolcorr.name := 'CALIBUpdateVolcorr';
    xCALIBUpdateVolcorr.Term := 'Update VOLCORR Set Factor = #P0# Where Name =' + chr(39) + '#P2#' + chr(39) +
        'AND Pipdevicename = ' + chr(39) + '#P3#' + chr(39) + ' AND Tip = #P4# AND Volume = #P5#';
    xCALIBUpdateVolcorr.Defaultargs := '';
    xCALIBUpdateVolcorr.Comment := '';

    xCALIBReadPreset.name := 'CALIBReadPreset';
    xCALIBReadPreset.Term := 'Select * from CALIBPresets Where Name = ' + chr(39) + '#P0#' + chr(39);
    xCALIBReadPreset.Defaultargs := '';
    xCALIBReadPreset.Comment := '';

    // SQL statements in array
    xSQLExport[0] := xCALIBCreateTables;
    xSQLExport[1] := xCALIBExportData;
    xSQLExport[2] := xCALIBGetAllWeights;
    xSQLExport[3] := xCALIBGetAvgWeight;
    xSQLExport[4] := xCALIBGetErrorArray;
    xSQLExport[5] := xCALIBGetMaxError;
    xSQLExport[6] := xCALIBGetMinError;
    xSQLExport[7] := xCALIBGetVolcorrData;
    xSQLExport[8] := xCALIBGetVolcurve;
    xSQLExport[9] := xCALIBInsertPipetingSteps;
    xSQLExport[10] := xCALIBResultsDelete;
    xSQLExport[11] := xCALIBSavePreset;
    xSQLExport[12] := xCALIBSQLDelete;
    xSQLExport[13] := xCALIBTablesExist;
    xSQLExport[14] := xCALIBUpdateCV;
    xSQLExport[15] := xCALIBUpdateError;
    xSQLExport[16] := xCALIBUpdateResults;
    xSQLExport[17] := xCALIBUpdateStatus;
    xSQLExport[18] := xCALIBUpdateVolcorr;
    xSQLExport[19] := xCALIBReadPreset;

    // Write the SQL queries to the table
    i := 0;
    try
        while i < xSQLTermCount do
        begin
            xSQLTerms.WriteSQLTermRec(xSQLExport[i]);
            i := i + 1;
        end
    finally
        xSQLTerms.Free;
        xQuery.Free;
    end;

    // Run the temporary method
    MethodStartIntern('~TemporaryMethod~', true, false, false);
end;

function TEdExternDirect.InterruptStart(const aInterruptText: string): boolean;
var
    xInterruptText: string;
begin
    // man könnte es auch damit probieren:
    // keybd_event(VK_ESCAPE, 0, 0, 0);
    // keybd_event(VK_ESCAPE, 0, KEYEVENTF_KEYUP, 0);

    xInterruptText := aInterruptText;
    if xInterruptText = '' then
    begin
        xInterruptText := 'Interrupt Start';
    end;

    result := TThreadManagerSetup.Instance.RequestInterruptStart(TThreadAPI.GetCurrentThreadID(),
        aInterruptText);
end;

function TEdExternDirect.MethodStart(const aMethodName: string;
    const aDontShowMessages, aSimulate: boolean): boolean;
begin
    EXIT(MethodStartIntern(aMethodName, aDontShowMessages, aSimulate, true));
end;

function TEdExternDirect.MethodStartIntern(const aMethodName: string;
    const aDontShowMessages, aSimulate, aInitAtEnd: boolean): boolean;
var
    xModes: TSamplerThreadModes;
begin
    result := false;
    xModes := [mdInitFirst];
    if aDontShowMessages then
        xModes := xModes + [mdNoMessages];
    if (aInitAtEnd and TRunGlobals.Instance.InitAtMethodEnd) then
        xModes := xModes + [mdInitAtEnd];

    // Method Start
    if (aMethodName <> '') then
    begin
        result := TMethodStarter.MethodStart(aMethodName, xModes, aSimulate);
    end;
end;

procedure TEdExternDirect.RegisterControllerForAllRunIDs;
begin
    // keine Funktion
end;

procedure TEdExternDirect.RegisterControllerIfNew(const aRunID: string);
begin
    // keine Funktion
end;

function TEdExternDirect.ReloadValues: boolean;
begin
    result := TRunGlobals.Instance.ReloadValues;
end;

procedure TEdExternDirect.RequestAndStateSetAbort;
begin
    TGUIManagerRunner.Instance.RequestAndStateSetAbort;
end;

procedure TEdExternDirect.RequestSafeAppClose(const aMaxWaitTime: cardinal);
begin
    TThreadManagerSetup.Instance.RequestSafeAppClose(aMaxWaitTime);
end;

procedure TEdExternDirect.ResetGlobalName;
begin
    TRunGlobals.Instance.ChangeGlobalName('', '', false, true);
end;

function TEdExternDirect.SetCurrentLayoutName(const aDialogText: string): boolean;
begin
    if (TRunGlobals.Instance.LayoutName = '') then
        self.ChangeLayout(false, aDialogText);

    result := (TRunGlobals.Instance.LayoutName <> '');
end;

procedure TEdExternDirect.SetGlobalError(const aErrorText: string);
begin
    gErrorManager.SetGlobalErr(ERR_USER, aErrorText);
end;

procedure TEdExternDirect.SetOnSafeAppClose(const aValue: TNotifyEvent);
begin
    TThreadManagerRun.Instance.SetOnSafeAppClose(aValue);
end;

procedure TEdExternDirect.StartCurrentMethod;
begin
    if TRunGlobals.Instance.PMethodName = '' then
        EXIT;
    self.MethodStart(TRunGlobals.Instance.PMethodName, false, false);
end;

procedure TEdExternDirect.StartMethod(aAlwaysSelect: boolean; const aDefaultMethod: string);
var
    xSelectName, xStartName: string;
begin
    if ThrMan.SamThreadRunning(false) then
        EXIT;

    Application.ProcessMessages; // Nötig, um metheditform.FormClose vollständig abzuschließen

    if (aAlwaysSelect) or (aDefaultMethod = '') then
    begin
        xSelectName := TRunGlobals.Instance.PMethodName;
        if (TGetMethDlg.Call(xSelectName) = actStart) then
            xStartName := xSelectName;
        Application.ProcessMessages;
    end
    else
        xStartName := aDefaultMethod;

    if (xStartName <> '') then
    begin
        TEdExtern.Instance.MethodStart(xStartName, false, false);
    end;
end;

function TEdExternDirect.ThreadIsRunning(aShowMsg: boolean): Boolean;
begin
    result := ThrMan.SamThreadRunning(aShowMsg);
end;

procedure TEdExternDirect.UserInterrupt;
begin
    ThrMan.UserInterrupt;
end;

procedure TEdExternDirect.ShowFlushDialog;
begin
    // Wenn noch kein Layout geladen ist dann zuerst Layout öffnen
    // - sonst wird eine Washstation die im Layout definiert ist nicht erkannt!
    if not SetCurrentLayoutName(TLanguageString.Read('Flush System', 'System spülen')) then
        EXIT;

    TdlgFlushSystem.ShowDialog(PeriPumpExists, GetPipDeviceNames, GetSystemLiquidNames,
        PipDeviceHasMultiPumps, PipDeviceGetLiquidIndices, PipDeviceGetTipCount, StartFlushMethod);
end;

procedure TEdExternDirect.ShowTableEditor(aTableName: string);
var
    xEditor: TfrmTableEditor;
    xDatabaseConfig: TDatabaseConfig;
    xAlias: string;
begin
    xAlias := TLocalDataProviderFactory.Instance.MainDBAlias;
    xDatabaseConfig := TLocalDataProviderFactory.Instance.DatabaseConfigGroup.List.FindByAlias(xAlias);
    xEditor := TfrmTableEditor.Create(xDatabaseConfig);
    try
        xEditor.LoadTableToDataGrid(aTableName);
        xEditor.ShowModal;
    finally
        FreeAndNil(xEditor);
    end;
end;

procedure TEdExternDirect.ShowCalibrateDialog;
var
    xQuery: TQueryDataAdaptor;
begin
    // Initialize Layout
    if not SetCurrentLayoutName(TLanguageString.Read('Calibrate system', 'System Kalibrierung')) then
        EXIT;

    // Initialize handles to write to SQL tables
    xQuery := TQueryDataAdaptor.Create('CALIBTemp');
    try
        try
            // Delete POSINFO data
            xQuery.ExecSQL('Delete from POSINFO;');
            // Create the tables
            xQuery.ExecSQL('create table CALIBPresets (Name CHAR(40), SystemLiq CHAR(40),' +
                ' Density DOUBLE(4), Steps INTEGER, Tries INTEGER, MaxCV DOUBLE(4), MaxError DOUBLE(4)' +
                ', UseSySLiq BOOLEAN, ManualFill INTEGER, BalanceWait INTEGER, BalancePrecision INTEGER' +
                ', TipArray CHAR(40), LHP CHAR(40));');
            xQuery.ExecSQL
                ('create table CALIBResults(Status CHAR(40), Try INTEGER, Step INTEGER, VolCorrName CHAR(40), PipDeviceName CHAR(40),'
                + ' Tip INTEGER, PipVol INTEGER, Error DOUBLE(4), Factor DOUBLE(4), ' +
                ' Weight DOUBLE(4), LQHP CHAR(40), DateTime TIMESTAMP)');
        except

        end;
    finally
        xQuery.Free;
    end;

    // Start the Dialog
    TdlgCalibSystem.ShowDialog(GetSystemLiquidNames, Calibrate);
end;

function TEdExternDirect.GetPipDeviceNames: TArray<string>;
begin
    EXIT(gPipDeviceManager.GetPipDeviceNames);
end;

function TEdExternDirect.GetSystemLiquidNames: TArray<string>;
begin
    EXIT(TLiquids.Instance.GetAllSystemLiquidNames());
end;

function TEdExternDirect.PeriPumpExists: boolean;
var
    xDev: TModule;
begin
    EXIT(gModules.Find(IPeriPumpDevice, xDev));
end;

function TEdExternDirect.PipDeviceGetLiquidIndices(const aPipDeviceName: string; aTips: TIPMAP;
    aPumpIndex1, aPumpIndex2: integer): TArray<integer>;
var
    xPipDevice: IPipDevice;
begin
    xPipDevice := gPipDeviceManager.FindPipDevice_ByName(aPipDeviceName);
    EXIT(gSysLiqManager.PipDeviceGetLiquidIndices(xPipDevice, aTips, aPumpIndex1, aPumpIndex2));
end;

function TEdExternDirect.PipDeviceGetTipCount(const aPipDeviceName: string): integer;
var
    xPipDevice: IPipDevice;
begin
    xPipDevice := gPipDeviceManager.FindPipDevice_ByName(aPipDeviceName);
    if not Assigned(xPipDevice) then
        EXIT(0);

    EXIT(xPipDevice.TipCount);
end;

function TEdExternDirect.PipDeviceHasMultiPumps(const aPipDeviceName: string): boolean;
var
    xPipDevice: IPipDevice;
    x: integer;
begin
    xPipDevice := gPipDeviceManager.FindPipDevice_ByName(aPipDeviceName);
    if not Assigned(xPipDevice) then
        EXIT(false);

    for x := 0 to xPipDevice.TipCount - 1 do
    begin
        if xPipDevice.Tips[x].NumberOfPumps > 1 then
            EXIT(true);
    end;

    EXIT(false);
end;


end.
