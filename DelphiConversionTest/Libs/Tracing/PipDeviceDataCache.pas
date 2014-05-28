unit PipDeviceDataCache;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  15.04.10 pk                                        TN5050    Initial Revision
  23.04.10 pk                                        TN5050    Now thread-safe
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    Streamable,
    GeneralTypes,
    ListDataCache;

type
    TTipData = class(TStreamable)
    private
        fTipIndex: integer;
        fTipType: string;
    public
        constructor Create(); overload; override;
        constructor Create(const aTipIndex: integer); reintroduce; overload;
        destructor Destroy(); override;
    published
        property TipIndex: integer read fTipIndex write fTipIndex;
        property TipType: string read fTipType write fTipType;
    end;

    TTipListData = class(TStreamableObjectList);

    TPipDeviceData = class(TStreamable)
    private
        fPipDeviceName: string;
        fTips: TTipListData;
    public
        constructor Create(); overload; override;
        constructor Create(const aPipDeviceName: string; const aTipCount: integer); reintroduce; overload;
        destructor Destroy(); override;
        procedure SetTipTypes(const aTipTypeNames: TStringArray);
        function GetTipTypes(): TStringArray;
    published
        property PipDeviceName: string read fPipDeviceName write fPipDeviceName;
        property Tips: TTipListData read fTips write fTips;
    end;

    TPipDeviceListDataCache = class(TListDataCache<TPipDeviceData>)
    private
        function FindPipDeviceByName(const aPipDeviceName: string): TPipDeviceData;
    public
        procedure AddPipDevice(const aPipDeviceName: string; const aTipCount: integer);
        procedure PipDeviceSetTipTypeName(const aPipDeviceName: string; const aTipIndex: integer;
            const aTipTypeName: string);
        procedure PipDeviceInitTipTypes(const aPipDeviceName: string; const aTipTypeNames: TStringArray);
        function PipDeviceGetTipTypeName(const aPipDeviceName: string; const aTipIndex: integer): string;
    end;


implementation


uses
    SysUtils;

{ TTipData }

constructor TTipData.Create;
begin
    inherited;

end;

constructor TTipData.Create(const aTipIndex: integer);
begin
    Create();
    fTipIndex := aTipIndex;
end;

destructor TTipData.Destroy;
begin

    inherited;
end;

{ TPipDeviceData }

constructor TPipDeviceData.Create;
begin
    inherited;
    fTips := TTipListData.Create();
end;

constructor TPipDeviceData.Create(const aPipDeviceName: string; const aTipCount: integer);
var
    x: integer;
begin
    Create();
    fPipDeviceName := aPipDeviceName;
    for x := 0 to aTipCount - 1 do
        fTips.Add(TTipData.Create(x));

end;

destructor TPipDeviceData.Destroy;
begin
    FreeAndNil(fTips);
    inherited;
end;

procedure TPipDeviceData.SetTipTypes(const aTipTypeNames: TStringArray);
var
    x: integer;
begin
    ASSERT(self.Tips.Count = Length(aTipTypeNames));
    for x := 0 to Length(aTipTypeNames) - 1 do
    begin
        (self.Tips[x] as TTipData).TipType := aTipTypeNames[x];
    end;
end;

function TPipDeviceData.GetTipTypes(): TStringArray;
var
    x: integer;
begin

    SetLength(result, self.Tips.Count);
    for x := 0 to Length(result) - 1 do
    begin
        result[x] := (self.Tips[x] as TTipData).TipType;
    end;
end;

{ TPipDeviceListDataCache }
function TPipDeviceListDataCache.FindPipDeviceByName(const aPipDeviceName: string): TPipDeviceData;
var
    x: integer;
    xPipDeviceData: TPipDeviceData;
begin
    result := nil;

    for x := 0 to self.Count - 1 do
    begin
        xPipDeviceData := self.ListData[x] as TPipDeviceData;
        if SameText(xPipDeviceData.PipDeviceName, aPipDeviceName) then
        begin
            result := xPipDeviceData;
            EXIT;
        end;
    end;

end;

procedure TPipDeviceListDataCache.AddPipDevice(const aPipDeviceName: string; const aTipCount: integer);
var
    xPipDeviceData: TPipDeviceData;
begin
    fCriticalSection.Enter();
    try
        xPipDeviceData := self.FindPipDeviceByName(aPipDeviceName);
        if Assigned(xPipDeviceData) then
        begin
            ASSERT(xPipDeviceData.Tips.Count = aTipCount);
            EXIT;
        end;

        self.Add(TPipDeviceData.Create(aPipDeviceName, aTipCount));
    finally
        fCriticalSection.Leave();
    end;
end;

function TPipDeviceListDataCache.PipDeviceGetTipTypeName(const aPipDeviceName: string;
    const aTipIndex: integer): string;
var
    xPipDeviceData: TPipDeviceData;
begin
    fCriticalSection.Enter();
    try
        xPipDeviceData := self.FindPipDeviceByName(aPipDeviceName);
        result := (xPipDeviceData.Tips[aTipIndex] as TTipData).TipType
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TPipDeviceListDataCache.PipDeviceInitTipTypes(const aPipDeviceName: string;
    const aTipTypeNames: TStringArray);
var
    xPipDeviceData: TPipDeviceData;
    x: integer;
begin
    fCriticalSection.Enter();
    try
        xPipDeviceData := self.FindPipDeviceByName(aPipDeviceName);
        ASSERT(xPipDeviceData.Tips.Count = Length(aTipTypeNames));
        for x := 0 to Length(aTipTypeNames) - 1 do
        begin
            (xPipDeviceData.Tips[x] as TTipData).TipType := aTipTypeNames[x];
            fReaderWriter.DataChanged();
        end;
    finally
        fCriticalSection.Leave();
    end;

end;

procedure TPipDeviceListDataCache.PipDeviceSetTipTypeName(const aPipDeviceName: string;
    const aTipIndex: integer; const aTipTypeName: string);
var
    xPipDeviceData: TPipDeviceData;
begin
    fCriticalSection.Enter();
    try
        xPipDeviceData := self.FindPipDeviceByName(aPipDeviceName);
        (xPipDeviceData.Tips[aTipIndex] as TTipData).TipType := aTipTypeName;
        fReaderWriter.DataChanged();
    finally
        fCriticalSection.Leave();
    end;
end;


end.
