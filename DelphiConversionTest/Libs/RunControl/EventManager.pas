{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  25.09.08 wl  TRunstCall                   TN4242    wird statt TDllCall verwendet
  06.10.08 pk                               TN4258    uses changed
  15.10.08 pk  ExecuteRunStartWithNameAndParams TN4258 Params changed from string to TKeyValueParamArray
  10.11.08 pk  StartThreadedMethod          TN4280    New
  24.11.08 pk  StartThreadedMethod          TN4280    return cardinal
  17.02.09 pk  PrepareCallMethod, JumpLocal TN4232    New
  20.02.09 pk                               TN4232    Changes for multithreaded trace
  25.02.09 pk  PrepareCallMethod            TN4279    New IsEvent Param
  16.03.09 pk  GetCurrentSubMethodName      TN4470    New
  06.04.09 pk  DisplayComponentSimpleEvent  TN4503    New
  04.02.10 pk                               TN4972    Changes for Restart
  09.03.10 pk                               TN5015    Massive Changes in ThreadClasses
  07.05.10 pk                               TN5092    various changes needed for new parser array type
  15.11.11 wl  CreateParamsForTipMethods    TN5738   von SubstanceHandling hierher; neuer Parameter: _$PipDevice
  01.03.12 wl  StoreParserIdentRWRawValue   TN5822   entfernt
  25.06.13 wl  PrepareCallMethod            TN6178   hier entfernt
  -------------------------------------------------------------------------------------------------- }

unit EventManager;


interface


uses
    AppTypes,
    MethodTypes,
    DisplayComponentIntf;

type
    TRunstCall = class
    private
        fRunName: string;
        fParams: TKeyArgValueList;
    public
        constructor Create(const aRunName: string; const aParams: TKeyArgValueList);

        procedure Execute(const aSituationLogText: string);
        property RunName: string read fRunName write fRunName;
        property Params: TKeyArgValueList read fParams write fParams;
    end;

    TEventManager = class
    private
        class var uInstance: TEventManager;
    protected
        constructor Create();
    public
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class property Instance: TEventManager read uInstance;
        class procedure SetInstance(const aValue: TEventManager);
        class function CreateEventIfNecessary(const aRunst: TRunStartOptions): TRunstCall;
        class function CreateParamsForTipMethods(const aPipDeviceName: string; const aTipmap: TIPMAP;
            const aTipTypeName: string): TKeyArgValueList;

        procedure ExecuteRunStartWithNameAndParams(const aSituationLogText, aRunName: string;
            const aParams: TKeyArgValueList); virtual;
        function RestartThreadByTraceName(const aThreadTraceName: string; const aCloseHandleOnFinish: boolean)
            : cardinal; virtual;
        procedure StartThreadedMethod(const aMethodName: string; const aParams: TKeyArgValueList;
            const aIsMainThread: boolean); virtual;
        function StartJoinableThreadedMethod(const aMethodName: string; const aParams: TKeyArgValueList)
            : cardinal; virtual;

        function JoinThread(const aThreadSysHandleID: cardinal; const aMaxWaitTime: cardinal)
            : boolean; virtual;
        procedure JumpLocal(const aAddress: integer); virtual;
        procedure StoreParserIdentRWValue(const aKey, aValue: string); virtual;

        function GetCurrentSubMethodName(): string; virtual;
        procedure DisplayComponentSimpleEvent(const aSender: IDisplayComponent;
            const aEventName: string); virtual;
        procedure AddStepsToPendingRunSteps(const aRunStepList: TObject; const aTrace: boolean); virtual;
        procedure PendingRunStepsMoveCursor(const aCursorPos: integer); virtual;

    end;


implementation


uses
    CommonTypes,
    ParserIdentDataType,
    TipTypeDataAdaptor;

{ TEventManager }

class function TEventManager.CreateParamsForTipMethods(const aPipDeviceName: string; const aTipmap: TIPMAP;
    const aTipTypeName: string): TKeyArgValueList;
const
    STR_HARDCODED_VARIABLE_PIPDEVICENAME = '_$PipDevice';
    STR_HARDCODED_VARIABLE_TIPMAP = '_$TipMap';
    STR_HARDCODED_VARIABLE_TIPTYPE = '_$TipType';
    STR_HARDCODED_VARIABLE_TOOLNAME = '_$ToolName';
var
    xTypeData: TTipType;
begin
    result := TKeyArgValueList.Create();

    TTipTypeDataAdaptor.TipTypeExists(aTipTypeName, xTypeData);

    result.Add(STR_HARDCODED_VARIABLE_PIPDEVICENAME, TStrArg.Create(aPipDeviceName));
    result.Add(STR_HARDCODED_VARIABLE_TIPMAP, TIntArg.Create(aTipmap));
    result.Add(STR_HARDCODED_VARIABLE_TIPTYPE, TStrArg.Create(aTipTypeName));
    result.Add(STR_HARDCODED_VARIABLE_TOOLNAME, TStrArg.Create(xTypeData.ToolName));
end;

procedure TEventManager.AddStepsToPendingRunSteps(const aRunStepList: TObject; const aTrace: boolean);
begin

end;

constructor TEventManager.Create;
begin
    inherited Create();
end;

class procedure TEventManager.CreateInstance;
begin
    if Assigned(uInstance) then
        EXIT;
    SetInstance(TEventManager.Create());
end;

class procedure TEventManager.DestroyInstance;
begin
    uInstance.Free;
end;

class procedure TEventManager.SetInstance(const aValue: TEventManager);
begin
    uInstance := aValue;
end;

class function TEventManager.CreateEventIfNecessary(const aRunst: TRunStartOptions): TRunstCall;
begin
    result := nil;
    if (aRunst.RunName = '') then
        EXIT;

    result := TRunstCall.Create(aRunst.RunName, aRunst.Params);
end;

procedure TEventManager.ExecuteRunStartWithNameAndParams(const aSituationLogText, aRunName: string;
    const aParams: TKeyArgValueList);
begin
    // wird in TRunEventManager mit Leben gefüllt
end;

procedure TEventManager.StoreParserIdentRWValue(const aKey: string; const aValue: string);
begin

end;

function TEventManager.JoinThread(const aThreadSysHandleID: cardinal; const aMaxWaitTime: cardinal): boolean;
begin
    result := true;
end;

function TEventManager.RestartThreadByTraceName(const aThreadTraceName: string;
    const aCloseHandleOnFinish: boolean): cardinal;
begin
    result := 0;
    // wird in TRunEventManager mit Leben gefüllt
end;

procedure TEventManager.StartThreadedMethod(const aMethodName: string; const aParams: TKeyArgValueList;
    const aIsMainThread: boolean);
begin
    // wird in TRunEventManager mit Leben gefüllt
end;

function TEventManager.StartJoinableThreadedMethod(const aMethodName: string; const aParams: TKeyArgValueList)
    : cardinal;
begin
    result := 0;
    // wird in TRunEventManager mit Leben gefüllt
end;

procedure TEventManager.PendingRunStepsMoveCursor(const aCursorPos: integer);
begin

end;

procedure TEventManager.JumpLocal(const aAddress: integer);
begin

end;

function TEventManager.GetCurrentSubMethodName: string;
begin
    result := '';
end;

procedure TEventManager.DisplayComponentSimpleEvent(const aSender: IDisplayComponent;
    const aEventName: string);
begin

end;

{ TRunstCall }

constructor TRunstCall.Create(const aRunName: string; const aParams: TKeyArgValueList);
begin
    inherited Create;
    fRunName := aRunName;
    fParams := aParams;
end;

procedure TRunstCall.Execute(const aSituationLogText: string);
begin
    TEventManager.Instance.ExecuteRunStartWithNameAndParams(aSituationLogText, fRunName, fParams);
end;


end.
