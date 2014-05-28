unit DLLLoadingV6;
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
  15.12.11 wl                               TN5767   von Version 7.4.5 hierher kopiert
  28.09.12 wl  GetDefinedValue              TN5984   mit AutoRetryCount = 0
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Windows,
    AppTypes,
    DllCall,
    DllInterfaceV6;

type
    TDllLoadingV6 = class
    private
        fAppIntf: TAppInterface; // nur Zeiger, kein Member
        procedure InitAppFunctions();
        constructor Create(aAppIntf: TAppInterface);
    public
        class function Instance(aAppIntf: TAppInterface): TDllLoadingV6;
        //
        function GetDefinedValue(const aParameter: string): string;
        function GetCurrentScriptName(): string;
        function GetCurrentMethodName(): string;
        function GetCurrentRunName(): string;
    end;

    // Function Types
    TSetAppFunctions = procedure(aAppFunctions: PDLLInterface; aVersion: TIntPtr); cdecl;

    TDllHandleV6 = class(TDllHandle)
    protected
        function LoadAppFuncs(): boolean; override;
    end;


implementation


uses
    SysUtils,
    LogManager,
    ErrorManager,
    StringUtilities,
    GeneralTypes,
    AppSettings,
    CommonTypes,
    RunFlow;

var
    uAppFunctions: TDLLInterface;

procedure gmSetAppFuncs(const aAppFuncs: TDLLInterface);
begin
    uAppFunctions := aAppFuncs;
end;

type
    // Function Types
    TOnGetDefinedValue = function(aParameter: WideString): TInterfaceStr; cdecl;

var
    uOnGetDefinedValue: TOnGetDefinedValue = nil;
    uDLLLoading: TDllLoadingV6;

procedure App_SetGlobalErr(aValue: integer; aReason: string); cdecl;
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

function App_ReadStringApp(aSection, aIdent: WideString): TInterfaceStr; cdecl;
var
    xIniAccess: IWinlissyIniAccess;
begin
    xIniAccess := gCommonDll.CreateAppIni;
    result := StrToInterfaceStr(xIniAccess.ReadString(aSection, aIdent));
end;

function App_ReadStringRobot(aSection, aIdent: WideString): TInterfaceStr; cdecl;
var
    xIniAccess: IWinlissyIniAccess;
begin
    xIniAccess := gCommonDll.CreateRobotIni;
    result := StrToInterfaceStr(xIniAccess.ReadString(aSection, aIdent));
end;

function App_ReadStringDLL(aSection, aIdent: WideString): TInterfaceStr; cdecl;
var
    xIniAccess: IWinlissyIniAccess;
begin
    xIniAccess := gCommonDll.CreateDLLIni;
    result := StrToInterfaceStr(xIniAccess.ReadString(aSection, aIdent));
end;

function App_ReadStringOther(aIniName, aSection, aIdent, aDefault: WideString): TInterfaceStr; cdecl;
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

function App_GetAlias: TInterfaceStr; cdecl;
begin
    result := StrToInterfaceStr(gCommonDll.Alias);
end;

function App_GetRunAlias: TInterfaceStr; cdecl;
begin
    result := StrToInterfaceStr(gCommonDll.RunPath);
end;

function App_GetDefinedValue(aParameter: WideString): TInterfaceStr; cdecl;
begin
    if Assigned(uOnGetDefinedValue) then
    begin
        result := uOnGetDefinedValue(aParameter);
        EXIT;
    end;

    result := StrToInterfaceStr(uDLLLoading.GetDefinedValue(aParameter));
end;

function App_GetCurrentScriptName(): TInterfaceStr; cdecl;
begin
    result := StrToInterfaceStr(uDLLLoading.GetCurrentScriptName());
end;

function App_GetCurrentMethodName(): TInterfaceStr; cdecl;
begin
    result := StrToInterfaceStr(uDLLLoading.GetCurrentMethodName());
end;

function App_GetCurrentRunName(): TInterfaceStr; cdecl;
begin
    result := StrToInterfaceStr(uDLLLoading.GetCurrentRunName());
end;

procedure gmDLL_SetOnGetDefinedValue(aOnGetDefinedValue: TOnGetDefinedValue);
begin
    uOnGetDefinedValue := aOnGetDefinedValue;
end;

{ TDllLoadingV6 }

constructor TDllLoadingV6.Create(aAppIntf: TAppInterface);
begin
    inherited Create();

    fAppIntf := aAppIntf;
    InitAppFunctions();
end;

class function TDllLoadingV6.Instance(aAppIntf: TAppInterface): TDllLoadingV6;
begin
    if not Assigned(uDLLLoading) then
        uDLLLoading := TDllLoadingV6.Create(aAppIntf);
    result := uDLLLoading;
end;

function TDllLoadingV6.GetCurrentMethodName(): string;
begin
    if Assigned(fAppIntf) then
        result := fAppIntf.GetCurrentMethodName();
end;

function TDllLoadingV6.GetCurrentRunName(): string;
begin
    if Assigned(fAppIntf) then
        result := fAppIntf.GetCurrentRunName();
end;

function TDllLoadingV6.GetCurrentScriptName(): string;
begin
    if Assigned(fAppIntf) then
        result := '';
end;

function TDllLoadingV6.GetDefinedValue(const aParameter: string): string;
const
    STR_DELIM = ',';
var
    xParamArray: TStringArray;
    xFuncName: string;
    xDllParams: string;
begin

    xParamArray := TStringUtilities.StringToStringArray(aParameter, STR_DELIM);
    try
        result := '';
        xFuncName := UpperCase(xParamArray[0]);

        if (xFuncName = STR_DLLINT_DEFVALUE_IDENT_GETTIPMAXVOL) then
        begin
            result := 'Not available'; // FloatToStr(GetMaxTipVolume(StrToInt(xParamArray[1])));
        end
        else if (xFuncName = STR_DLLINT_DEFVALUE_IDENT_PREPARERUN) then
        begin
            result := 'Not available';
        end
        else if (xFuncName = STR_DLLINT_DEFVALUE_IDENT_DLLFU) then
        begin
            xDllParams := TStringUtilities.StringArrayToString(xParamArray, STR_DELIM, 3, high(xParamArray));
            result := TDllCall.DirectExecute(xParamArray[1], xParamArray[2], xDllParams,
                TDllInterfaceVersion.Version6, 0);
        end
        else if (xFuncName = STR_DLLINT_DEFVALUE_IDENT_GENERATEMULTIPIPSEQ) then
        begin
            result := 'Not available';
        end
        else if (xFuncName = STR_DLLINT_DEFVALUE_IDENT_DATAPATH) then
        begin
            result := TAppSettings.Datapath;
        end
        else if (xFuncName = STR_DLLINT_DEFVALUE_IDENT_MODALDIALOGOPENED) then
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
        end
        else if (xFuncName = STR_DLLINT_DEFVALUE_IDENT_CONFIRMADD) then
        begin
            if TAppSettings.ItemConfirmAdd(xParamArray[1], xParamArray[2], xParamArray[3]) then
                result := 'YES';
        end
        else if (xFuncName = STR_DLLINT_DEFVALUE_IDENT_CONFIRMEDIT) then
        begin
            if TAppSettings.ItemConfirmEdit(xParamArray[1], xParamArray[2]) then
                result := 'YES';
        end
        else if (xFuncName = STR_DLLINT_DEFVALUE_IDENT_CONFIRMDELETE) then
        begin
            if TAppSettings.ItemConfirmDelete(xParamArray[1], xParamArray[2]) then
                result := 'YES';
        end
        else if (xFuncName = STR_DLLINT_DEFVALUE_IDENT_USERISADMIN) then
        begin
            if gCommonDll.CurrentUser.HasLevel(usrSystemAdmin) then
                result := 'YES';
        end;
        // here you can define individual return values for each dll!!!
        // else if ( xFuncName = '' ) then begin
        // end
    finally
    end;
end;

procedure TDllLoadingV6.InitAppFunctions();
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
    xAppFuncs.GetAlias := App_GetAlias;
    xAppFuncs.GetRunAlias := App_GetRunAlias;
    xAppFuncs.GetDefinedValue := App_GetDefinedValue;
    xAppFuncs.GetCurrentScriptName := App_GetCurrentScriptName;
    xAppFuncs.GetCurrentMethodName := App_GetCurrentMethodName;
    xAppFuncs.GetCurrentRunName := App_GetCurrentRunName;
    gmSetAppFuncs(xAppFuncs);
end;

{ TDllHandleV6 }

function TDllHandleV6.LoadAppFuncs: boolean;
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


end.
