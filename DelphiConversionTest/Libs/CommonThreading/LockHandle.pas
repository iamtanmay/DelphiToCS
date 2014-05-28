{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  13.04.10 wl                                        TN5044   uses geändert
  29.11.10 wl  TLock.WaitFor                         TN5370   umbenannt in TLock.WaitForLock
  14.12.10 wl  TLock.CreateEventExt                  TN5411   von UtilLib hierher
  15.08.13 wl                                        TN6223   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit LockHandle;


interface


uses
    Windows,
    SyncObjs;

type
    TLockHandleObject = class(TSynchroObject)
    protected
        FHandle: THandle;
        FLastError: Integer;
    public
        destructor Destroy; override;
        property LastError: Integer read FLastError;
        property Handle: THandle read FHandle;
    end;

    TLock = class(TLockHandleObject)
    private
        class function CreateEventExt(const aEventAttributes: TSecurityAttributes;
            aManualReset, aInitialState: BOOL; aName: string): THandle;
    public
        constructor Create(aEventAttributes: TSecurityAttributes; aManualReset, aIsUnlocked: boolean;
            const aName: string);
        function WaitForLock(Timeout: DWORD): TWaitResult; virtual;
        procedure Unlock; virtual;
        procedure Lock; virtual;

        class function ConvWaitResult(aWaitForRes: DWORD; out oErr: integer): TWaitResult;
    end;

    TGlobalLock = class(TLock)
    public
        constructor Create(aManualReset, aIsUnlocked: boolean; const aGlobalName: string);
    end;

    TSimpleLock = class(TLock)
    protected
        fCanSafeSuspend: boolean;
    public
        constructor Create(aManualReset, aIsUnlocked, aSafeSuspend: boolean); overload;
        function WaitForLock(aTimeout: DWORD): TWaitResult; override;
        property CanSafeSuspend: boolean read fCanSafeSuspend;
    end;


implementation


uses
    ShlObj,
    SysUtils,
    UtilLib,
    ThreadClasses,
    ThreadRegistry;

{ TLockHandleObject }

destructor TLockHandleObject.Destroy;
begin
    CloseHandle(FHandle);
    inherited Destroy;
end;

{ TLock }

constructor TLock.Create(aEventAttributes: TSecurityAttributes; aManualReset, aIsUnlocked: boolean;
    const aName: string);
begin
    inherited Create;

    FHandle := CreateEventExt(aEventAttributes, aManualReset, aIsUnlocked, aName);
end;

class function TLock.CreateEventExt(const aEventAttributes: TSecurityAttributes;
    aManualReset, aInitialState: BOOL; aName: string): THandle;
var
    xPName: PChar;
    xEventAttributes: PSecurityAttributes;
begin
    xPName := nil;
    xEventAttributes := nil;

    if aName <> '' then
        xPName := PChar(aName);
    if aEventAttributes.nLength > 0 then
        xEventAttributes := @aEventAttributes;

    result := CreateEvent(xEventAttributes, aManualReset, aInitialState, xPName);
end;

class function TLock.ConvWaitResult(aWaitForRes: DWORD; out oErr: integer): TWaitResult;
begin
    case aWaitForRes of
        WAIT_ABANDONED:
            Result := wrAbandoned;
        WAIT_OBJECT_0:
            Result := wrSignaled;
        WAIT_TIMEOUT:
            Result := wrTimeout;
        WAIT_FAILED:
            begin
                Result := wrError;
                oErr := GetLastError;
            end;
        else
            Result := wrError;
    end;
end;

function TLock.WaitForLock(Timeout: DWORD): TWaitResult;
begin
    if GetCurrentThreadID = MainThreadID then
        outputdebugstring(pchar(format('%d - Waifor - BEGIN', [GetCurrentThreadID])));
    result := ConvWaitResult(WaitForSingleObject(Handle, Timeout), FLastError);
    if GetCurrentThreadID = MainThreadID then
        outputdebugstring(pchar(format('%d - Waifor - END', [GetCurrentThreadID])));
end;

procedure TLock.UnLock;
begin
    Windows.SetEvent(Handle);
end;

procedure TLock.Lock;
begin
    Windows.ResetEvent(Handle);
end;

{ TGlobalLock }

constructor TGlobalLock.Create(aManualReset, aIsUnlocked: boolean; const aGlobalName: string);
var
    xSecurityAttributes: TSecurityAttributes;
begin
    xSecurityAttributes.nLength := 0;

    inherited Create(xSecurityAttributes, aManualReset, aIsUnlocked, aGlobalName);
end;

{ TSimpleLock }

constructor TSimpleLock.Create(aManualReset, aIsUnlocked, aSafeSuspend: boolean);
var
    xSecurityAttributes: TSecurityAttributes;
begin
    xSecurityAttributes.nLength := 0;
    inherited Create(xSecurityAttributes, aManualReset, aIsUnlocked, '');
    fCanSafeSuspend := aSafeSuspend;
end;

function TSimpleLock.WaitForLock(aTimeout: DWORD): TWaitResult;
var
    xWaitOK: boolean;
    xTID: cardinal;
    xThreadImage: TThreadImage;
begin
    // if GetCurrentThreadID = MainThreadID then outputdebugstring( pchar( format( '%d - Waifor - BEGIN', [ GetCurrentThreadID ]  ) ) );
    if fCanSafeSuspend then
    begin
        xTID := TThreadRegistry.Instance.FindCurrentThreadID();
        xThreadImage := TThreadRegistry.Instance.FindThreadImageByThreadID(xTID);
        if Assigned(xThreadImage) and Assigned(xThreadImage.Thread) then
        begin
            result := xThreadImage.Thread.WaitForLock(self, aTimeout, xWaitOK);
            if xWaitOK then
                EXIT;
        end;
    end;
    result := inherited WaitForLock(aTimeout);
    // if GetCurrentThreadID = MainThreadID then outputdebugstring( pchar( format( '%d - Waifor - END', [ GetCurrentThreadID ]  ) ) );
end;


end.
