{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Various functions for Multithreading assistance
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------   -------- -----------------------------------------------
  04.02.04 pk                                TN1719   New
  06.02.04 pk  TThreadedTimer                TN1714   Wrapper for TTimerThread
  06.02.04 pk  TTimerThread.BeforeDestr.     TN1714   Verify if calling thread is allowed to call destroy
  19.02.04 pk                                TN1719   Turn Debuginfo OFF
  24.06.04 wl                                TN2007   uses Variants (nur Delphi 6 und 7)
  18.01.05 pk  TExecThread, IExecutable      TN2281   New
  19.01.05 pk                                TN2281.0  TStringArray, TIntArray from AppTypes to GeneralTypes
  19.01.05 pk                                TN2281.0  IExecutable --> GeneralTypes
  11.03.05 pk  TExecThread                   TN2339.2  New field : fExceptionHandlerIntf
  11.03.05 pk  TSdtRoutine, TStdProc         TN2339.2  Removed
  17.03.05 pk  TExecThread                   TN2352.1  IExecptionHandler field removed
  17.03.05 pk  TExecThread                   TN2352.1  New events : OnStart, OnFinish, OnException
  07.11.05 pk                                TN2736    Various changes for safely suspending a thread
  21.11.06 pk  TExecThread.EnableSuspend     TN3424    returns true if the enable suspend caused a change
  07.12.06 pk  TExecThread                   TN3455    moved to ThreadClasses
  16.12.06 pk                                TN3458    for synchronous messages, the exception is now raised in
  the context of the calling thread instead of context of main thread
  16.12.06 pk                                          Debuginfo off
  29.05.12 wl  TPlatformSpecificOS.GetCurrentThreadID  TN5904   von ThreadClasses hierher
  -------------------------------------------------------------------------------------------------- }

unit ThreadUtils;
{$DEBUGINFO OFF}


interface


uses
    Windows,
    SysUtils,
    Messages;

type
    TMessageArg = variant;
    TMessageResult = variant;

    TFunc = function(const Args: TMessageArg): TMessageResult of object;

    TMessageType = (mtSynch, mtASynch);

    TStdFunc = class
    private
        fArgs: TMessageArg;
        fCallBack: TFunc;
        fReturnVal: TMessageResult;
        fAutoFree: boolean;
        fCatchException: boolean;
        fSavedException: Exception;
        function GetSavedException(): Exception;
    public
        constructor Create(aCallBack: TFunc; const aArgs: TMessageArg; aAutoFree: boolean;
            aCatchException: boolean);
        destructor Destroy(); override;
        procedure Call();
        procedure AutoFree();
        property ReturnVal: TMessageResult read fReturnVal;
        property SavedException: Exception read GetSavedException;
    end;

    TPlatformSpecificOS = class
        class function GetCurrentThreadID(): cardinal;
    end;

procedure gmMessageAndGo(aCallBack: TFunc; const aArgs: TMessageArg);
function gmMessageAndWait(aCallBack: TFunc; const aArgs: TMessageArg): TMessageResult;


implementation


uses
    Variants,
    Dialogs;

const
    SAM_EXECPROC = WM_USER + 1000;
    SAM_DESTROYWINDOW = WM_USER + 1001;

var
    uThreadUtilsWindow: HWND;

    // --------------------------------------------------------------------------------------------------
procedure gmMessageWithStdFunc(aStdFunc: TStdFunc; aType: TMessageType);
// --------------------------------------------------------------------------------------------------
begin
    case aType of
        mtASynch:
            PostMessage(uThreadUtilsWindow, SAM_EXECPROC, LongInt(aStdFunc), 0);
        mtSynch:
            SendMessage(uThreadUtilsWindow, SAM_EXECPROC, LongInt(aStdFunc), 0);
    end;
end;

// --------------------------------------------------------------------------------------------------
function gmMessage(aCallBack: TFunc; const aArgs: TMessageArg; aType: TMessageType): TMessageResult;
// --------------------------------------------------------------------------------------------------
var
    xStdFunc: TStdFunc;
    xException: Exception;
begin
    xStdFunc := TStdFunc.Create(aCallBack, aArgs, aType = mtASynch, aType <> mtASynch);
    result := xStdFunc.ReturnVal;

    gmMessageWithStdFunc(xStdFunc, aType);

    if aType = mtSynch then
    begin
        result := xStdFunc.ReturnVal;
        xException := xStdFunc.SavedException;
        xStdFunc.Free;
        if Assigned(xException) then
            raise xException;
    end;

end;

// --------------------------------------------------------------------------------------------------
procedure gmMessageAndGo(aCallBack: TFunc; const aArgs: TMessageArg);
// --------------------------------------------------------------------------------------------------
begin
    gmMessage(aCallBack, aArgs, mtASynch);
end;

// --------------------------------------------------------------------------------------------------
function gmMessageAndWait(aCallBack: TFunc; const aArgs: TMessageArg): TMessageResult;
// --------------------------------------------------------------------------------------------------
begin
    result := gmMessage(aCallBack, aArgs, mtSynch);
end;

// --------------------------------------------------------------------------------------------------
procedure FreeWindow(var aWindow: HWND);
// --------------------------------------------------------------------------------------------------
begin
    if aWindow <> 0 then
    begin
        DestroyWindow(aWindow);
        aWindow := 0;
    end;
end;

// --------------------------------------------------------------------------------------------------
function AllocateWindow(aWindowClass: TWndClass; aWndProc: TFNWndProc): HWND;
// --------------------------------------------------------------------------------------------------
var
    TempClass: TWndClass;
    ClassRegistered: Boolean;
begin
    aWindowClass.hInstance := HInstance;
    ClassRegistered := GetClassInfo(HInstance, aWindowClass.lpszClassName, TempClass);
    if not ClassRegistered or (TempClass.lpfnWndProc <> @aWndProc) then
    begin
        if ClassRegistered then
            Windows.UnregisterClass(aWindowClass.lpszClassName, HInstance);
        Windows.RegisterClass(aWindowClass);
    end;
    Result := CreateWindow(aWindowClass.lpszClassName, '', 0, 0, 0, 0, 0, 0, 0, HInstance, nil);
end;

// --------------------------------------------------------------------------------------------------
function ThreadUtilsWndProc(Window: HWND; message, wParam, lParam: Longint): Longint; stdcall;
// --------------------------------------------------------------------------------------------------
var
    xStdFunc: TStdFunc;
begin
    case message of
        SAM_EXECPROC:
            begin
                xStdFunc := TStdFunc(WParam);
                try
                    xStdFunc.Call();
                finally
                    xStdFunc.AutoFree;
                end;
                result := 0;
            end;
        SAM_DESTROYWINDOW:
            begin
                FreeWindow(uThreadUtilsWindow);
                result := 0;
            end;
        else
            result := DefWindowProc(Window, message, wParam, lParam);
    end;
end;

class function TPlatformSpecificOS.GetCurrentThreadID(): cardinal;
begin
    result := Windows.GetCurrentThreadID();
end;

// --------------------------------------------------------------------------------------------------
var
    uThreadUtilsWindowClass: TWndClass = (style: 0; lpfnWndProc: @ThreadUtilsWndProc; cbClsExtra: 0;
        cbWndExtra: 0; hInstance: 0; hIcon: 0; hCursor: 0; hbrBackground: 0; lpszMenuName: nil;
        lpszClassName: 'TThreadUtilsWindow');

    // --------------------------------------------------------------------------------------------------
constructor TStdFunc.Create(aCallBack: TFunc; const aArgs: TMessageArg; aAutoFree: boolean;
    aCatchException: boolean);
// --------------------------------------------------------------------------------------------------
begin
    inherited Create;
    fCallBack := aCallBack;
    fReturnVal := Unassigned;
    fArgs := aArgs;
    fAutoFree := aAutoFree;
    fCatchException := (not aAutoFree) and aCatchException;
    fSavedException := nil;
end;

destructor TStdFunc.Destroy();
begin
    inherited;
    // free the exception object if GetSavedException was not called
    fSavedException.Free;
end;

// --------------------------------------------------------------------------------------------------
procedure TStdFunc.Call();
// --------------------------------------------------------------------------------------------------
begin
    try
        fReturnVal := fCallBack(fArgs);
    except
        on E: Exception do
        begin
            if fCatchException then
                fSavedException := TObject(AcquireExceptionObject()) as Exception
            else
                raise
        end
        else
        begin
            raise;
        end;
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TStdFunc.AutoFree();
// --------------------------------------------------------------------------------------------------
begin
    if fAutoFree then
        self.Free();
end;

function TStdFunc.GetSavedException(): Exception;
begin
    result := nil;
    if fCatchException then
    begin
        result := fSavedException;
        fSavedException := nil;
    end;
end;


initialization


uThreadUtilsWindow := AllocateWindow(uThreadUtilsWindowClass, @ThreadUtilsWndProc);


finalization


PostMessage(uThreadUtilsWindow, SAM_DESTROYWINDOW, 0, 0);


end.
