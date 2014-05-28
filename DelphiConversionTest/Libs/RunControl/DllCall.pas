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
  09.11.07 pk                               TN3922   Dataset changed to DataProvider
  05.08.09 ts  TDllFunc.Prepare             TN4638   ErrorBox, falls Dll nicht gefunden wurde, Möglichkeit Retry, Ignore, Abort
  17.08.09 wl  TDllFunc.CallFunc            TN4227   nach dem Aufruf werden die lokalen Einstellungen zurückgesetzt, falls in einer DLL Schweinereien passieren
  26.08.09 wl  TFuncParam                   TN4752   AnsiChar statt Char (UnicodeChar), sonst kracht es in der Dll
  08.09.09 pk                               TN4753   uses ErrorMessage replaced by ErrorInfo
  06.10.09 pk  LoadLib                      TN4753   read settings into cache
  19.11.09 pk  TDllHandle.LoadLib           TN4818   load datacache settings using uppercase( dllname ) instead of just dllname
  15.11.10 pk  TDllFunc.Prepare             TN5346   if abort is pressed exit and return false
  15.12.11 wl  TAppInterface                TN5767   von DllLoading hierher
  15.12.11 wl  TDllHandle                   TN5767   enthält abstrakte Funktion
  28.09.12 wl  TDllFunc.ErrorHandling       TN5984   ein automatisches Retry ist möglich
  28.09.12 wl  TDllCall.DirectExecute       TN5984   AutoRetryCount neuer Parameter
  -------------------------------------------------------------------------------------------------- }

unit DllCall;


interface


uses
    Windows,
    AppTypes;

type
    PFuncParam = ^TFuncParam;
    TFuncParam = array [0 .. 255] of AnsiChar;

    TEventFunc = function(GlobalErrPtr: TintPtr; ParentWin: HWND; ParamPtr: PFuncParam): Boolean;
    TcEventFunc = function(GlobalErrPtr: TintPtr; ParentWin: HWND; ParamPtr: PFuncParam): Boolean; cdecl;

    TDllInterfaceVersion = (Version6, Version8);

    TDefFunctionType = (dftUnknown, dftGetRackPos, dftModalDialogOpened, dftModalDialogClosed,
        dftGetCurrentVol, dftPaintRackPos);

    TDllHandle = class
    private
        fOwnsHandle: boolean;
        function IsLoaded(): boolean;
    protected
        fDllName: string;
        fHandle: HModule;
        function LoadAppFuncs(): boolean; virtual; abstract;
    public
        constructor Create(const aDllName: string; aOwnsHandle: boolean);
        destructor Destroy(); override;

        function LoadLib(): boolean;
        property Handle: HModule read fHandle;
        property DllName: string read fDllName;
    end;

    TDllFunc = class
    private
        fFuncName: string;
        fOwnsHandle: boolean;
        fFuncAdr: TFarProc;
        fDllHandle: TDllHandle;
        fOrigParams: TFuncParam;
        fParamsAndAnswer: TFuncParam;
        fPrepared: boolean;
        fAutoRetryCount: integer;
        procedure DoCallFunc(); virtual; abstract;
        function ErrorHandling(var vNoOfAutoRetry: integer): boolean;
        procedure PrepareParams(const aParams: string);
        function ReadFuncAdr(): boolean;
        procedure ClearAnswer();
        function Prepare(): boolean;
        function GetMainAppHandle(): HWND;
    public
        constructor Create(aDllHandle: TDllHandle; aOwnsHandle: boolean; const aFuncName, aParams: string;
            aAutoRetryCount: integer);
        destructor Destroy(); override;
        function CallFunc(): string;
        function CallFuncWithParams(const aParams: string): string;
    end;

    TDllPascalFunc = class(TDllFunc)
    protected
        procedure DoCallFunc(); override;
    end;

    TDllCFunc = class(TDllFunc)
    protected
        procedure DoCallFunc(); override;
    end;

    TDllCall = class
    strict private
        FDllName: string;
        FDllFunction: string;
        FFuncParam: string;
        fVersion: TDllInterfaceVersion;
        fAutoRetryCount: integer;
        class function IsCDllName(const aDllName: string): boolean;
    public
        constructor Create(const aDLLName, aDLLFunction, aParam: string; aVersion: TDllInterfaceVersion;
            aAutoRetryCount: integer);
        function Execute(const aSituationLogText: string): string; virtual;
        // direct execution!
        class function CreateDllHandle(const aDllName: string; aOwnsHandle: boolean;
            aVersion: TDllInterfaceVersion): TDllHandle;
        class function CreateFuncWithHandle(aDllHandle: TDllHandle; aOwnsHandle: boolean;
            const aFuncName, aParams: string; aAutoRetryCount: integer): TDllFunc;
        class function DirectExecute(aDLLName, aDLLFunction, aDLLParameter: string;
            aVersion: TDllInterfaceVersion; aAutoRetryCount: integer = 0): string;
    end;

    TAppInterface = class
    public
        function GetRackPos(const aParams: TArray<string>): string; virtual;
        function PaintRackPos(const aParams: TArray<string>): string; virtual;
        function GetCurrentVol(const aParams: TArray<string>): string; virtual;
        function GetCurrentMethodName(): string; virtual;
        function GetCurrentRunName(): string; virtual;
        function GetMaxTipVolume(aTipIndex: integer): double; virtual;
        procedure ModalDialogIsOpened(); virtual;
        procedure ModalDialogIsClosed(); virtual;
    end;


implementation


uses
    SysUtils,
    Forms,
    LogManager,
    ErrorManager,
    ErrorMessageFactory,
    ErrorInfo,
    SamGlobe,
    DllLoadingV6,
    DllLoadingV8,
    AppInstanceIniAdaptor;

{ TDllHandle }

constructor TDllHandle.Create(const aDllName: string; aOwnsHandle: boolean);
begin
    fDllName := aDllName;
    fOwnsHandle := aOwnsHandle;
    fHandle := 0;
end;

destructor TDllHandle.Destroy;
begin
    if fOwnsHandle and IsLoaded() then
        FreeLibrary(fHandle);

    inherited;
end;

function TDllHandle.IsLoaded(): boolean;
begin
    result := (fHandle > 0);
end;

function TDllHandle.LoadLib(): boolean;
var
    xFirstLoad: boolean;
begin
    result := false;
    xFirstLoad := not IsLoaded();
    if xFirstLoad then
    begin
        fHandle := LoadLibrary(PChar(fDLLName));
    end;

    if not IsLoaded then
    begin
        gLogManager.LogF('-->FAILED loading DLL [%s]', [fDLLName], true);
        Exit;
    end;

    if xFirstLoad then
    begin
        // DLL-Name in Liste eintragen
        gCalledDLLs.Add(fDllName);
        // SetAppFunctions()
        if not LoadAppFuncs() then
            Exit;

        // 06.10.09 pk: this is new: Here we read the settings into the cache for eache dll separately
        // this is so that the DLL can read the settings using the DLLInterface functions.
        TAppInstanceIniAdaptor.Instance.Settings.DataCache.ReadAreaIntoCache(UpperCase(fDLLName));
    end;

    result := true;
end;

{ TDllFunc }

constructor TDllFunc.Create(aDllHandle: TDllHandle; aOwnsHandle: boolean; const aFuncName, aParams: string;
    aAutoRetryCount: integer);
begin
    inherited Create();
    fDllHandle := aDllHandle;
    fOwnsHandle := aOwnsHandle;
    fFuncName := aFuncName;
    fAutoRetryCount := aAutoRetryCount;
    ClearAnswer();
    PrepareParams(aParams);
end;

destructor TDllFunc.Destroy();
begin
    if fOwnsHandle then
        fDllHandle.Free;
    inherited;
end;

procedure TDllFunc.ClearAnswer();
begin
    fParamsAndAnswer[0] := #0;
end;

procedure TDllFunc.PrepareParams(const aParams: string);
const
    INT_MAX_PARAM_LEN = 255;
var
    xParams: AnsiString;
begin
    xParams := AnsiString(aParams);
    if Length(xParams) > INT_MAX_PARAM_LEN then
        xParams := Copy(xParams, 1, INT_MAX_PARAM_LEN);
    // TFuncParam can store 256 chars. The 256th char will hold the null character so
    // so just to be safe we copy only upto a maximum of 255 chars since the StrPCopy
    // does not do any bounds checking
    StrPCopy(fOrigParams, xParams);
end;

function TDllFunc.ReadFuncAdr: boolean;
begin
    result := false;
    fFuncAdr := GetProcAddress(fDLLHandle.Handle, PChar(fFuncName));
    if (fFuncAdr = nil) then
    begin
        gLogManager.Log('-->FAILED loading Function ' + fFuncName, true);
        Exit;
    end;
    result := true;
end;

function TDllFunc.ErrorHandling(var vNoOfAutoRetry: integer): boolean;
const
    STR_ERROR = '[ShowSamErrBox]';
var
    xPos: integer;
    xAnswerText: string;
begin
    result := true;
    xAnswerText := string(AnsiString(fParamsAndAnswer));

    xPos := Pos(STR_ERROR, xAnswerText);
    if xPos <= 0 then
        Exit;

    // sollte AutoRetryCount größer als 0 sein, gibt es keine Fehlermeldung und die Funktion wird noch mal aufgerufen
    if (vNoOfAutoRetry < fAutoRetryCount) then
    begin
        inc(vNoOfAutoRetry);
        TLogManager.Instance.Log('Dll Call - Auto retry number: ' + IntToStr(vNoOfAutoRetry), false);
        EXIT(false);
    end;
    vNoOfAutoRetry := 0;

    xAnswerText := Copy(xAnswerText, Length(STR_ERROR) + 1, Length(xAnswerText));
    case gErrorMessageFactory.ErrBoxSimple(xAnswerText, 'Dll Call', eibAbortRetryIgnore) of
        IDRetry:
            result := false;
        IDAbort:
            gErrorManager.SetGlobalErr(ERR_USER, '');
    end;

    // clear answer if error occured
    ClearAnswer();
end;

function TDllFunc.Prepare(): boolean;
var
    xErrMessageResult: integer;
begin
    result := false;
    if not fPrepared then
    begin
        xErrMessageResult := IDRetry;
        while xErrMessageResult = IDRetry do
        begin
            if not fDllHandle.LoadLib() then
            begin
                xErrMessageResult := gErrorMessageFactory.ErrBoxSimple(fDllHandle.fDllName + '.DLL not found',
                    'Dll Call', eibAbortRetryIgnore);
                case xErrMessageResult of
                    IDAbort:
                        begin
                            gErrorManager.SetGlobalErr(ERR_USER, '');
                            EXIT;
                        end;
                    IDIgnore:
                        EXIT;
                end;
            end
            else
            begin
                xErrMessageResult := IDIgnore;
                if not self.ReadFuncAdr() then
                    EXIT;
                fPrepared := true;
            end;
        end;
    end;

    result := true;
end;

function TDllFunc.CallFuncWithParams(const aParams: string): string;
begin
    PrepareParams(aParams);
    result := CallFunc();
end;

function TDllFunc.CallFunc(): string;
var
    xDone: boolean;
    xNoOfAutoRetry: integer;
begin
    result := '';

    if not Prepare() then
        EXIT;

    xNoOfAutoRetry := 0;
    repeat
        // Funktionsaufruf
        gLogManager.LogF('CallDll: %s,%s,%s', [fDllHandle.DllName, fFuncName, fOrigParams], true);
        fParamsAndAnswer := fOrigParams;
        DoCallFunc();
        gLogManager.Log('Result: ' + fParamsAndAnswer, true);
        // Fehlerbehandlung
        xDone := ErrorHandling(xNoOfAutoRetry);

    until xDone;
    result := string(AnsiString(fParamsAndAnswer));

    // FormatSettings auf lokale Settings setzen
    SysUtils.GetFormatSettings;
end;

function TDllFunc.GetMainAppHandle(): HWND;
begin
    if Assigned(Application.MainForm) then
        result := Application.MainForm.Handle
    else
        result := 0;
end;

{ TDllPascalFunc }

procedure TDllPascalFunc.DoCallFunc;
var
    xFunc: TEventFunc;
begin
    @xFunc := fFuncAdr;
    xFunc(nil, GetMainAppHandle(), @fParamsAndAnswer);
end;

{ TDllCFunc }

procedure TDllCFunc.DoCallFunc;
var
    xCFunc: TcEventFunc;
begin
    @xCFunc := fFuncAdr;
    xCFunc(nil, GetMainAppHandle(), @fParamsAndAnswer);
end;

{ TDllCall }

constructor TDllCall.Create(const aDLLName, aDLLFunction, aParam: string; aVersion: TDllInterfaceVersion;
    aAutoRetryCount: integer);
begin
    inherited Create;
    FDllName := aDLLName;
    FDllFunction := aDLLFunction;
    FFuncParam := aParam;
    fVersion := aVersion;
    fAutoRetryCount := aAutoRetryCount;
end;

function TDLLCall.Execute(const aSituationLogText: string): string;
begin
    EXIT(DirectExecute(fDLLName, fDLLFunction, fFuncParam, fVersion, fAutoRetryCount));
end;

class function TDllCall.IsCDllName(const aDllName: string): boolean;
begin
    result := (Pos(UpperCase(aDLLName), UpperCase(gCDllFiles)) > 0)
end;

class function TDllCall.CreateDllHandle(const aDllName: string; aOwnsHandle: boolean;
    aVersion: TDllInterfaceVersion): TDllHandle;
begin
    if (aVersion = TDllInterfaceVersion.Version6) then
        EXIT(TDllHandleV6.Create(aDllName, aOwnsHandle))
    else
        EXIT(TDllHandleV8.Create(aDllName, aOwnsHandle));
end;

class function TDllCall.CreateFuncWithHandle(aDllHandle: TDllHandle; aOwnsHandle: boolean;
    const aFuncName, aParams: string; aAutoRetryCount: integer): TDllFunc;
begin
    if IsCDllName(aDllHandle.DllName) then
        EXIT(TDllCFunc.Create(aDllHandle, aOwnsHandle, aFuncName, aParams, aAutoRetryCount))
    else
        EXIT(TDllPascalFunc.Create(aDllHandle, aOwnsHandle, aFuncName, aParams, aAutoRetryCount));
end;

class function TDllCall.DirectExecute(aDLLName, aDLLFunction, aDLLParameter: string;
    aVersion: TDllInterfaceVersion; aAutoRetryCount: integer): string;
var
    xDllFunc: TDllFunc;
begin
    xDllFunc := CreateFuncWithHandle(CreateDllHandle(aDllName, false, aVersion), true, aDllFunction,
        aDllParameter, aAutoRetryCount);
    try
        result := xDllFunc.CallFunc();
    finally
        xDllFunc.Free;
    end;
end;

{ TAppInterface }

function TAppInterface.GetCurrentMethodName: string;
begin
    result := ''; // Dummy
end;

function TAppInterface.GetCurrentRunName: string;
begin
    result := ''; // Dummy
end;

function TAppInterface.GetCurrentVol(const aParams: TArray<string>): string;
begin
    result := ''; // Dummy
end;

function TAppInterface.GetMaxTipVolume(aTipIndex: integer): double;
begin
    result := 0; // Dummy
end;

function TAppInterface.GetRackPos(const aParams: TArray<string>): string;
begin
    result := ''; // Dummy
end;

procedure TAppInterface.ModalDialogIsOpened;
begin
    // Dummy
end;

function TAppInterface.PaintRackPos(const aParams: TArray<string>): string;
begin
    result := ''; // Dummy
end;

procedure TAppInterface.ModalDialogIsClosed;
begin
    // Dummy
end;


end.
