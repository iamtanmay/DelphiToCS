{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Loading DLL functions that uses the TDLLInterface (V 6.0 compatible)
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  03.09.03 wl                               TN1568   initial version
  03.09.03 pk  gmDLL_LoadEventFunction      TN1556.2 Code stripped out of this function and placed into new functions
  03.09.03 pk  gmDLL_LoadEventFunctionExt   TN1556.2 Does the same thing as gmLoadEventFunction, but has an extra pointer as parameter
  03.09.03 wl                               TN1568   alle DLL-Funktionen aus SamCmd hierher verschoben
  22.09.03 pk  gmDLL_LoadLibrary            TN1556.2 Put brackets around the dllname when writing log
  06.11.03 pk                               TN1649, TN1649.1 DLLInterface extended for GlobalErr and SimMode
  07.06.04 pk  all App_ functions           TN1964  now use c calling convention
  07.06.04 pk  all App_ functions           TN1964 Return types of functions changed from Widestring to TInterfaceStr
  29.10.04 pk  App_GetDefinedValue          TN2202.1 'GETTIPMAXVOL' returns max volume for the given tip
  01.11.04 pk  App_GetDefinedValue          TN2203.0 'GETRACKPOS' parameters: Rackname( or rackid or tubid),Pos. returns Rackname,Pos
  08.11.04 wl  TDllCall                     TN2213   Ersatz für LoadEvent-Funktionen (nur mit strings)
  11.11.04 pk  App_GetDefinedValue          TN2175.1 New: 'PREPARERUN' clears existing run or creates new run table
  08.12.04 pk  App_GetDefinedValue          TN2261   New: 'DLLFU' calls a dll funtion
  19.01.05 pk                               TN2281.0  TStringArray, TIntArray from AppTypes to GeneralTypes
  21.02.05 wl  App_ApplicationSleep         TN1960    gCommManager.AppSleep statt global_AppInterface-Methode
  08.03.05 pk  TDLLCall                     TN2337   Is now the base class for TDLLCallExt
  23.06.05 pk  gmDLL_LoadEventFunction      TN2471.1 pass Application.MainForm.Handle instead of Application.Handle
  11.07.05 thr App_ApplicationSleep         TN2498  GENERATEMULTIPIPSEQ external call to generate Multipipettingsequences
  11.07.05 wl  TFuncParam,TEventFunc,..     TN2498.1 von AppTypes hierher verschoben
  11.07.05 wl  TAppInterface                TN2498.1 kapselt die AppFunctions, Methoden können ganz normal überschrieben werden
  13.07.05 wl  TDllLoading                  TN2498.1 Kapselung für InitAppFunctions
  23.09.05 wl  TDLLCall.Execute             TN2627   ist jetzt Funktion - Rückgabewert der DLL wird nicht mehr "verschluckt"
  01.02.06 thr App_GefDefinedValue          TN2882 'DATAPATH' returns the path of the Data directory
  02.02.06 wl  TDllLoading.GetDefinedValue  TN2923   'GETCURRENTVOLUME' ermittelt den Füllstand einer Kavität
  20.03.06 wl  TDllLoading.GetDefinedValue  TN2967.1  neu: MODALDIALOGOPENED/-CLOSED, um User-Protection-Abfrage kurzfristig abzuschalten
  22.03.06 wl  TDllLoading.GetDefinedValue  TN2989.1  neu: PAINTRACKPOS färbt die entsprechende Position ein
  12.10.06 thr gmDLL_CallErrorHandling      TN3357 [ShowSamErrBox] wird aus dem Text der Fehlermeldung entfernt
  03.12.06 wl                               TN3243   alle ErrBox-Aufrufe durch ErrBoxSimple oder ErrBoxSerialIntf ersetzt
  07.12.06 wl  gmDLL_LoadAppFunctions       TN3409    bei Verwendung von Dll's, die nicht compliant sind -> Abbruch
  26.01.07 pk  gmDLL_LoadEventFunction      TN3525.3  WARNING, could be dangerous for old dlls. references to globalerrptr changed to nil
  27.04.07 wl  TDllLoading.GetDefinedValue  TN3669    neue Funktionen für TAppsettings.ItemConfirmDelete,Add,Edit, wenn ja ist result = YES
  07.05.07 wl  TDllLoading.GetDefinedValue  TN3669    neu: USERISADMIN: wenn user Systemadmin ist, ist result = YES
  25.07.07 pk  gmDLL_....                   TN3803    Converted to TDllHandle, TDllFunc, TDllFuncFactory. LoadLibrary was called everytime, and calling FreeLibrary once did not unload
  31.07.07 pk  TDLLCall                     TN3803    fMainAppHandle changed to GetMainAppHandle
  20.06.08 pk                               TN4139    WB global object replaced by LayoutManager
  13.07.09 pk  App_GetRunAlias              TN4585.4  RunAlias changed to RunPath
  31.08.09 pk  GetDefinedValue              TN4753    code moved to RunAppInterface
  02.03.11 wl                               TN5491    zahlreiche Änderungen für neues DllInterface Version 7
  13.04.11 wl                               TN5491   neues Konzept: statt IDataProvider wird ein Pointer übergeben
  29.06.11 wl  App_DPExecSQL                TN5613    result = RowsAffected
  15.12.11 wl  TAppInterface                TN5767   --> DllCall
  15.12.11 wl  TDllHandleV8                 TN5767   von TDllHandle abgeleitet
  28.09.12 wl  GetDefinedValue              TN5984   mit AutoRetryCount = 0
  -------------------------------------------------------------------------------------------------- }

unit DLLLoadingV8;


interface


uses
    Windows,
    AppTypes,
    DllCall,
    DllInterface;

type
    TDllLoadingV8 = class
    private
        fAppIntf: TAppInterface; // nur Zeiger, kein Member
        procedure InitAppFunctions();
        constructor Create(aAppIntf: TAppInterface);
    public
        class function Instance(aAppIntf: TAppInterface): TDllLoadingV8;
        //
        function GetDefinedValue(const aParameter: string): string;
        function GetCurrentMethodName(): string;
        function GetCurrentRunName(): string;
    end;

    // Function Types
    TSetAppFunctions = procedure(aAppFunctions: PDLLInterface; aVersion: TIntPtr); cdecl;

    TDllHandleV8 = class(TDllHandle)
    protected
        function LoadAppFuncs(): boolean; override;
    end;


implementation


uses
    SysUtils,
    Generics.Collections,
    LogManager,
    ErrorManager,
    GeneralTypes,
    StringUtilities,
    AppSettings,
    CommonTypes,
    RunFlow,
    DataProviderFactory,
    ConfigurationFile,
    DataProvider;

var
    uAppFunctions: TDLLInterface;

procedure gmSetAppFuncs(const aAppFuncs: TDLLInterface);
begin
    uAppFunctions := aAppFuncs;
end;

type
    // Function Types
    TOnGetDefinedValue = function(aParameter: WideString): PWideString; cdecl;

var
    uOnGetDefinedValue: TOnGetDefinedValue = nil;
    uDLLLoading: TDllLoadingV8;
    uDataProviders: TList<TDataProvider>;

function StrToInterfaceStr(aStr: string): PWideString;
begin
    result := PWideString(WideString(aStr));
end;

procedure App_SetGlobalErr(aValue: integer; aReason: WideString); cdecl;
begin
    gErrorManager.SetGlobalErr(aValue, aReason);
end;

function App_GetGlobalErr(): integer; cdecl;
begin
    result := gErrorManager.GlobalErr;
end;

function App_IsGlobalErr(): boolean; cdecl;
begin
    result := gErrorManager.IsGlobalErr();
end;

function App_IsSimulationMode(): boolean; cdecl;
begin
    result := gRunFlow.SimulationMode;
end;

procedure App_ApplicationSleep(aMSec: cardinal); cdecl;
begin
    gRunFlow.AppSleep(aMSec);
end;

procedure App_Log(aLogText: WideString; aLogType: Longint; aDisplay: boolean); cdecl;
begin
    gLogManager.Log(aLogText, aDisplay, aLogType);
end;

function App_ReadStringApp(aSection, aIdent: WideString): PWideString; cdecl;
var
    xIniAccess: IWinlissyIniAccess;
begin
    xIniAccess := gCommonDll.CreateAppIni;
    result := StrToInterfaceStr(xIniAccess.ReadString(aSection, aIdent));
end;

function App_ReadStringRobot(aSection, aIdent: WideString): PWideString; cdecl;
var
    xIniAccess: IWinlissyIniAccess;
begin
    xIniAccess := gCommonDll.CreateRobotIni;
    result := StrToInterfaceStr(xIniAccess.ReadString(aSection, aIdent));
end;

function App_ReadStringDLL(aSection, aIdent: WideString): PWideString; cdecl;
var
    xIniAccess: IWinlissyIniAccess;
begin
    xIniAccess := gCommonDll.CreateDLLIni;
    result := StrToInterfaceStr(xIniAccess.ReadString(aSection, aIdent));
end;

function App_ReadStringOther(aIniName, aSection, aIdent, aDefault: WideString): PWideString; cdecl;
var
    xIniAccess: ISimpleIniAccess;
begin
    xIniAccess := gCommonDll.CreateSimpleIni(aIniName);
    result := StrToInterfaceStr(xIniAccess.ReadString(aSection, aIdent, aDefault));
end;

function App_ReadIntApp(aSection, aIdent: WideString): Longint; cdecl;
var
    xIniAccess: IWinlissyIniAccess;
begin
    xIniAccess := gCommonDll.CreateAppIni;
    result := xIniAccess.ReadInteger(aSection, aIdent);
end;

function App_ReadIntRobot(aSection, aIdent: WideString): Longint; cdecl;
var
    xIniAccess: IWinlissyIniAccess;
begin
    xIniAccess := gCommonDll.CreateRobotIni;
    result := xIniAccess.ReadInteger(aSection, aIdent);
end;

function App_ReadIntDLL(aSection, aIdent: WideString): Longint; cdecl;
var
    xIniAccess: IWinlissyIniAccess;
begin
    xIniAccess := gCommonDll.CreateDLLIni;
    result := xIniAccess.ReadInteger(aSection, aIdent);
end;

function App_ReadIntOther(aIniName, aSection, aIdent: WideString; aDefault: Longint): Longint; cdecl;
var
    xIniAccess: ISimpleIniAccess;
begin
    xIniAccess := gCommonDll.CreateSimpleIni(aIniName);
    result := xIniAccess.ReadInteger(aSection, aIdent, aDefault);
end;

function App_GetAliasName: PWideString; cdecl;
begin
    result := StrToInterfaceStr(TDataProviderFactory.Instance.MainDBAlias);
end;

function App_GetPathFromAlias(aAlias: WideString): PWideString; cdecl;
begin
    result := StrToInterfaceStr(TDataProviderFactory.Instance.MainDBAlias);
end;

function App_GetDefinedValue(aParameter: WideString): PWideString; cdecl;
begin
    if Assigned(uOnGetDefinedValue) then
    begin
        result := uOnGetDefinedValue(aParameter);
        EXIT;
    end;

    result := StrToInterfaceStr(uDLLLoading.GetDefinedValue(aParameter));
end;

function App_GetCurrentMethodName(): PWideString; cdecl;
begin
    result := StrToInterfaceStr(uDLLLoading.GetCurrentMethodName());
end;

function App_GetCurrentRunName(): PWideString; cdecl;
begin
    result := StrToInterfaceStr(uDLLLoading.GetCurrentRunName());
end;

procedure gmDLL_SetOnGetDefinedValue(aOnGetDefinedValue: TOnGetDefinedValue);
begin
    uOnGetDefinedValue := aOnGetDefinedValue;
end;

function App_CreateMainDataProvider: PDllDataProvider; cdecl;
var
    xDP: TDataProvider;
begin
    xDP := TDataProviderFactory.Instance.CreateDataProvider(TDataProviderFactory.Instance.MainDBAlias);
    uDataProviders.Add(xDP);
    EXIT(xDP);
end;

function App_CreateDataProvider(aAlias: WideString): PDllDataProvider; cdecl;
var
    xDP: TDataProvider;
begin
    xDP := TDataProviderFactory.Instance.CreateDataProvider(aAlias);
    uDataProviders.Add(xDP);
    EXIT(xDP);
end;

procedure App_DestroyDataProvider(aDP: PDllDataProvider); cdecl;
var
    x: integer;
    xDP: TDataProvider;
begin
    for x := 0 to uDataProviders.Count - 1 do
    begin
        if (uDataProviders[x] = aDP) then
        begin
            xDP := uDataProviders[x];
            uDataProviders.Delete(x);
            xDP.Free;
            EXIT;
        end;
    end;
end;

function App_CallOtherDllFunction(aDllName, aDllFunction, aDllParams: WideString): PWideString; cdecl;
begin
    result := StrToInterfaceStr(TDllCall.DirectExecute(aDllName, aDllFunction, aDllParams,
        TDllInterfaceVersion.Version8, 0));
end;

function App_GetDataPath: PWideString; cdecl;
begin
    EXIT(StrToInterfaceStr(TAppSettings.DataPath));
end;

function App_UserIsAdmin: boolean; cdecl;
begin
    EXIT(gCommonDll.CurrentUser.HasLevel(usrSystemAdmin));
end;

function App_UserConfirmAdd(const aItemTypeName, aItemName, aCopiedFrom: WideString): boolean; cdecl;
begin
    result := TAppSettings.ItemConfirmAdd(aItemTypeName, aItemName, aCopiedFrom);
end;

function App_UserConfirmEdit(const aItemTypeName, aItemName: WideString): boolean; cdecl;
begin
    result := TAppSettings.ItemConfirmEdit(aItemTypeName, aItemName);
end;

function App_UserConfirmDelete(const aItemTypeName, aItemName: WideString): boolean; cdecl;
begin
    result := TAppSettings.ItemConfirmDelete(aItemTypeName, aItemName);
end;

function App_LocalIniReadString(aSection, aIdent, aDefault: WideString): PWideString; cdecl;
var
    xLocalIniFile: IConfigurationSet;
begin
    xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(true);
    try
        result := StrToInterfaceStr(xLocalIniFile.ReadString(aSection, aIdent, aDefault));
    finally
        xLocalIniFile.Close;
    end;
end;

procedure App_LocalIniWriteString(aSection, aIdent, aValue: WideString); cdecl;
var
    xLocalIniFile: IConfigurationSet;
begin
    xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(true);
    try
        xLocalIniFile.WriteString(aSection, aIdent, aValue);
    finally
        xLocalIniFile.Close;
    end;
end;

function App_DPGetActive(aDP: PDllDataProvider): boolean; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(xDataProvider.Active);
end;

function App_DPGetBof(aDP: PDllDataProvider): boolean; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(xDataProvider.Bof);
end;

function App_DPGetEof(aDP: PDllDataProvider): boolean; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(xDataProvider.Eof);
end;

function App_DPGetFieldCount(aDP: PDllDataProvider): integer; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(xDataProvider.FieldCount);
end;

function App_DPGetRecordCount(aDP: PDllDataProvider): integer; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(xDataProvider.RecordCount);
end;

function App_DPGetRecNo(aDP: PDllDataProvider): integer; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(xDataProvider.RecNo);
end;

procedure App_DPSelectAndOpen(aDP: PDllDataProvider; const aSQL: WideString; aReadOnly: boolean); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.SelectAndOpen(aSQL, aReadOnly);
end;

function App_DPExecSQL(aDP: PDllDataProvider; const aSQL: WideString): integer; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(xDataProvider.ExecSQL(aSQL));
end;

procedure App_DPAppend(aDP: PDllDataProvider); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.Append();
end;

procedure App_DPClose(aDP: PDllDataProvider); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.Close();
end;

procedure App_DPDelete(aDP: PDllDataProvider); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.Delete();
end;

procedure App_DPEdit(aDP: PDllDataProvider); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.Edit();
end;

procedure App_DPFirst(aDP: PDllDataProvider); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.First();
end;

procedure App_DPInsert(aDP: PDllDataProvider); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.Insert();
end;

function App_DPIsEmpty(aDP: PDllDataProvider): boolean; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(xDataProvider.IsEmpty);
end;

procedure App_DPLast(aDP: PDllDataProvider); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.Last();
end;

procedure App_DPNext(aDP: PDllDataProvider); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.Next();
end;

procedure App_DPPost(aDP: PDllDataProvider); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.Post();
end;

procedure App_DPPrior(aDP: PDllDataProvider); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.Prior();
end;

procedure App_DPMoveBy(aDP: PDllDataProvider; aOffset: integer); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.MoveBy(aOffset);
end;

procedure App_DPRefresh(aDP: PDllDataProvider); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.Refresh();
end;

function App_DPGetFieldByNameAsString(aDP: PDllDataProvider; const aFieldName: WideString)
    : PWideString; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(StrToInterfaceStr(xDataProvider.FieldByNameAsString(aFieldName)));
end;

procedure App_DPSetFieldByNameAsString(aDP: PDllDataProvider; const aFieldName: WideString;
    const aValue: WideString); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.FieldByNameAsString(aFieldName, aValue);
end;

function App_DPGetFieldByNameAsInt(aDP: PDllDataProvider; const aFieldName: WideString): integer; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(xDataProvider.FieldByNameAsInt(aFieldName));
end;

procedure App_DPSetFieldByNameAsInt(aDP: PDllDataProvider; const aFieldName: WideString;
    aValue: integer); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.FieldByNameAsInt(aFieldName, aValue);
end;

function App_DPGetFieldByNameAsFloat(aDP: PDllDataProvider; const aFieldName: WideString): double; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(xDataProvider.FieldByNameAsFloat(aFieldName));
end;

procedure App_DPSetFieldByNameAsFloat(aDP: PDllDataProvider; const aFieldName: WideString;
    aValue: double); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.FieldByNameAsFloat(aFieldName, aValue);
end;

function App_DPGetFieldByNameAsBoolean(aDP: PDllDataProvider; const aFieldName: WideString): boolean; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(xDataProvider.FieldByNameAsBoolean(aFieldName));
end;

procedure App_DPSetFieldByNameAsBoolean(aDP: PDllDataProvider; const aFieldName: WideString;
    aValue: boolean); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.FieldByNameAsBoolean(aFieldName, aValue);
end;

function App_DPGetFieldByNameAsDateTime(aDP: PDllDataProvider; const aFieldName: WideString)
    : TDateTime; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(xDataProvider.FieldByNameAsDateTime(aFieldName));
end;

procedure App_DPSetFieldByNameAsDateTime(aDP: PDllDataProvider; const aFieldName: WideString;
    aValue: TDateTime); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.FieldByNameAsDateTime(aFieldName, aValue);
end;

function App_DPGetFieldAsString(aDP: PDllDataProvider; const aIndex: integer): PWideString; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(StrToInterfaceStr(xDataProvider.FieldAsString(aIndex)));
end;

procedure App_DPSetFieldAsString(aDP: PDllDataProvider; const aIndex: integer;
    const aValue: WideString); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.FieldAsString(aIndex, aValue);
end;

function App_DPGetFieldAsInt(aDP: PDllDataProvider; const aIndex: integer): integer; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(xDataProvider.FieldAsInt(aIndex));
end;

procedure App_DPSetFieldAsInt(aDP: PDllDataProvider; const aIndex: integer; aValue: integer); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.FieldAsInt(aIndex, aValue);
end;

function App_DPGetFieldAsFloat(aDP: PDllDataProvider; const aIndex: integer): double; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(xDataProvider.FieldAsFloat(aIndex));
end;

procedure App_DPSetFieldAsFloat(aDP: PDllDataProvider; const aIndex: integer; aValue: double); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.FieldAsFloat(aIndex, aValue);
end;

function App_DPGetFieldAsBoolean(aDP: PDllDataProvider; const aIndex: integer): boolean; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(xDataProvider.FieldAsBoolean(aIndex));
end;

procedure App_DPSetFieldAsBoolean(aDP: PDllDataProvider; const aIndex: integer; aValue: boolean); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.FieldAsBoolean(aIndex, aValue);
end;

function App_DPGetFieldAsDateTime(aDP: PDllDataProvider; const aIndex: integer): TDateTime; cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    EXIT(xDataProvider.FieldAsDateTime(aIndex));
end;

procedure App_DPSetFieldAsDateTime(aDP: PDllDataProvider; const aIndex: integer; aValue: TDateTime); cdecl;
var
    xDataProvider: TDataProvider;
begin
    xDataProvider := aDP;
    xDataProvider.FieldAsDateTime(aIndex, aValue);
end;

{ TDllLoadingV8 }

constructor TDllLoadingV8.Create(aAppIntf: TAppInterface);
begin
    inherited Create();

    fAppIntf := aAppIntf;
    InitAppFunctions();
end;

class function TDllLoadingV8.Instance(aAppIntf: TAppInterface): TDllLoadingV8;
begin
    if not Assigned(uDLLLoading) then
        uDLLLoading := TDllLoadingV8.Create(aAppIntf);
    result := uDLLLoading;
end;

function TDllLoadingV8.GetCurrentMethodName(): string;
begin
    if Assigned(fAppIntf) then
        result := fAppIntf.GetCurrentMethodName();
end;

function TDllLoadingV8.GetCurrentRunName(): string;
begin
    if Assigned(fAppIntf) then
        result := fAppIntf.GetCurrentRunName();
end;

function TDllLoadingV8.GetDefinedValue(const aParameter: string): string;
const
    STR_DELIM = ',';
var
    xParamArray: TStringArray;
    xFuncName: string;
begin

    xParamArray := TStringUtilities.StringToStringArray(aParameter, STR_DELIM);
    try
        result := '';
        xFuncName := UpperCase(xParamArray[0]);

        if (xFuncName = STR_DLLINT_DEFVALUE_IDENT_MODALDIALOGOPENED) then
        begin
            if Assigned(fAppIntf) then
                fAppIntf.ModalDialogIsOpened();
            result := 'User protection paused';
        end
        else if (xFuncName = STR_DLLINT_DEFVALUE_IDENT_MODALDIALOGCLOSED) then
        begin
            if Assigned(fAppIntf) then
                fAppIntf.ModalDialogIsClosed();
            result := 'User protection unpaused';
        end
        else if (xFuncName = STR_DLLINT_DEFVALUE_IDENT_GETRACKPOS) then
        begin
            if Assigned(fAppIntf) then
                result := fAppIntf.GetRackPos(xParamArray);
        end
        else if (xFuncName = STR_DLLINT_DEFVALUE_IDENT_PAINTRACKPOS) then
        begin
            if Assigned(fAppIntf) then
                result := fAppIntf.PaintRackPos(xParamArray);
        end
        else if (xFuncName = STR_DLLINT_DEFVALUE_IDENT_GETCURRENTVOLUME) then
        begin
            if Assigned(fAppIntf) then
                result := fAppIntf.GetCurrentVol(xParamArray);
        end;
        // here you can define individual return values for each dll!!!
        // else if ( xFuncName = '' ) then begin
        // end
    finally
    end;
end;

procedure TDllLoadingV8.InitAppFunctions();
var
    xAppFuncs: TDLLInterface;
begin
    xAppFuncs.SetGlobalErr := App_SetGlobalErr;
    xAppFuncs.GetGlobalErr := App_GetGlobalErr;
    xAppFuncs.IsGlobalErr := App_IsGlobalErr;
    xAppFuncs.IsSimulationMode := App_IsSimulationMode;
    xAppFuncs.ApplicationSleep := App_ApplicationSleep;
    xAppFuncs.Log := App_Log;
    xAppFuncs.ReadStringApp := App_ReadStringApp;
    xAppFuncs.ReadStringRobot := App_ReadStringRobot;
    xAppFuncs.ReadStringDll := App_ReadStringDll;
    xAppFuncs.ReadStringOther := App_ReadStringOther;
    xAppFuncs.ReadIntApp := App_ReadIntApp;
    xAppFuncs.ReadIntRobot := App_ReadIntRobot;
    xAppFuncs.ReadIntDll := App_ReadIntDll;
    xAppFuncs.ReadIntOther := App_ReadIntOther;
    xAppFuncs.GetAliasName := App_GetAliasName;
    xAppFuncs.GetPathFromAlias := App_GetPathFromAlias;
    xAppFuncs.GetDataPath := App_GetDataPath;
    xAppFuncs.GetDefinedValue := App_GetDefinedValue;
    xAppFuncs.GetCurrentMethodName := App_GetCurrentMethodName;
    xAppFuncs.GetCurrentRunName := App_GetCurrentRunName;
    xAppFuncs.CallOtherDllFunction := App_CallOtherDllFunction;
    xAppFuncs.UserIsAdmin := App_UserIsAdmin;
    xAppFuncs.UserConfirmAdd := App_UserConfirmAdd;
    xAppFuncs.UserConfirmEdit := App_UserConfirmEdit;
    xAppFuncs.UserConfirmDelete := App_UserConfirmDelete;
    xAppFuncs.LocalIniReadString := App_LocalIniReadString;
    xAppFuncs.LocalIniWriteString := App_LocalIniWriteString;

    xAppFuncs.CreateMainDataProvider := App_CreateMainDataProvider;
    xAppFuncs.CreateDataProvider := App_CreateDataProvider;
    xAppFuncs.DestroyDataProvider := App_DestroyDataProvider;
    xAppFuncs.DPGetActive := App_DPGetActive;
    xAppFuncs.DPGetBof := App_DPGetBof;
    xAppFuncs.DPGetEof := App_DPGetEof;
    xAppFuncs.DPGetFieldCount := App_DPGetFieldCount;
    xAppFuncs.DPGetRecordCount := App_DPGetRecordCount;
    xAppFuncs.DPGetRecNo := App_DPGetRecNo;
    xAppFuncs.DPSelectAndOpen := App_DPSelectAndOpen;
    xAppFuncs.DPExecSQL := App_DPExecSQL;
    xAppFuncs.DPAppend := App_DPAppend;
    xAppFuncs.DPClose := App_DPClose;
    xAppFuncs.DPDelete := App_DPDelete;
    xAppFuncs.DPEdit := App_DPEdit;
    xAppFuncs.DPFirst := App_DPFirst;
    xAppFuncs.DPInsert := App_DPInsert;
    xAppFuncs.DPIsEmpty := App_DPIsEmpty;
    xAppFuncs.DPLast := App_DPLast;
    xAppFuncs.DPNext := App_DPNext;
    xAppFuncs.DPPost := App_DPPost;
    xAppFuncs.DPPrior := App_DPPrior;
    xAppFuncs.DPMoveBy := App_DPMoveBy;
    xAppFuncs.DPRefresh := App_DPRefresh;
    xAppFuncs.DPGetFieldByNameAsString := App_DPGetFieldByNameAsString;
    xAppFuncs.DPSetFieldByNameAsString := App_DPSetFieldByNameAsString;
    xAppFuncs.DPGetFieldByNameAsInt := App_DPGetFieldByNameAsInt;
    xAppFuncs.DPSetFieldByNameAsInt := App_DPSetFieldByNameAsInt;
    xAppFuncs.DPGetFieldByNameAsFloat := App_DPGetFieldByNameAsFloat;
    xAppFuncs.DPSetFieldByNameAsFloat := App_DPSetFieldByNameAsFloat;
    xAppFuncs.DPGetFieldByNameAsBoolean := App_DPGetFieldByNameAsBoolean;
    xAppFuncs.DPSetFieldByNameAsBoolean := App_DPSetFieldByNameAsBoolean;
    xAppFuncs.DPGetFieldByNameAsDateTime := App_DPGetFieldByNameAsDateTime;
    xAppFuncs.DPSetFieldByNameAsDateTime := App_DPSetFieldByNameAsDateTime;
    xAppFuncs.DPGetFieldAsString := App_DPGetFieldAsString;
    xAppFuncs.DPSetFieldAsString := App_DPSetFieldAsString;
    xAppFuncs.DPGetFieldAsInt := App_DPGetFieldAsInt;
    xAppFuncs.DPSetFieldAsInt := App_DPSetFieldAsInt;
    xAppFuncs.DPGetFieldAsFloat := App_DPGetFieldAsFloat;
    xAppFuncs.DPSetFieldAsFloat := App_DPSetFieldAsFloat;
    xAppFuncs.DPGetFieldAsBoolean := App_DPGetFieldAsBoolean;
    xAppFuncs.DPSetFieldAsBoolean := App_DPSetFieldAsBoolean;
    xAppFuncs.DPGetFieldAsDateTime := App_DPGetFieldAsDateTime;
    xAppFuncs.DPSetFieldAsDateTime := App_DPSetFieldAsDateTime;

    gmSetAppFuncs(xAppFuncs);
end;

{ TDllHandleV8 }

function TDllHandleV8.LoadAppFuncs: boolean;
var
    xProcAddress: TFarProc;
    xVersion: integer;
    xSetAppFunctions: TSetAppFunctions;
begin
    result := false;
    // ------------------------------------------------------------- SetAppFunctions()
    xProcAddress := GetProcAddress(fHandle, 'SetAppFunctions');
    if (xProcAddress = nil) then
    begin
        if (TAppSettings.CFR21ComplianceMode <> ccmNone) then
        begin
            gLogManager.Log('-->DLL Function SetAppFunctions does not exist!', true);
            gErrorManager.SetGlobalErr(ERR_USER,
                Format('FAILED loading library %s. SetAppFunctions does not exist', [fDllName]));
            Exit;
        end;
    end
    else
    begin
        xVersion := INT_DLLINTERFACE_VERSION;
        @xSetAppFunctions := xProcAddress;
        xSetAppFunctions(@(uAppFunctions), @xVersion);
        // bei version = 0 konnte SetAppFunctions nicht ausgeführt werden
        if (xVersion = 0) or (xVersion <> INT_DLLINTERFACE_VERSION) then
        begin
            gLogManager.LogF('Different versions of DLL interface. EXE: %d, DLL: %d',
                [INT_DLLINTERFACE_VERSION, xVersion], true);
            gLogManager.Log('-->FAILED loading Function SetAppFunctions', true);
            Exit;
        end;
    end;

    result := true;
end;


initialization


uDataProviders := TList<TDataProvider>.Create;


finalization


FreeAndNil(uDataProviders);


end.
