{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : A device which manages the tips for an arm
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.11.07 wl  St_GetTipWasteVolArray       TN3897    entfernt
  20.06.08 pk  SetArmPanel                  TN4139    remove
  03.07.08 wl                                         TN4157
  11.07.08 wl  OnShowUseTips                TN4164   Event, wenn sich die Tip Types ändern
  17.09.08 wl  InitTipType                  TN4224   Parameter geändert
  12.09.09 wl                               TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  04.15.10 pk  LoadTips, StoreTips          TN5050   new
  25.06.10 pk  TTipUsage, TTipUsageRec      TN5163   New
  05.04.11 wl  ExecutePipPumps              TN5535   jetzt mit TipMap: Damit werden nicht mehr unbedingt alle execInterfaces angesprochen
  10.04.13 wl                               TN6045      uses geändert
  23.08.13 wl  St_InitTipStatus             TN6233.2 entfernt
  17.09.13 wl  InitSyringes                 TN6252.1 --> LiquidHandlingLow
  -------------------------------------------------------------------------------------------------- }

unit IntfPipDevice;


interface


uses
    Generics.Collections,
    AppTypes,
    CommonTypes,
    Driver,
    IntfPipPumpDevice,
    TipSystem,
    IntfDevice;

type
    TShowUseTipsEvent = procedure(aUsedArmIndex: integer; aArmUseTips: TIPMAP);
    TTipTypeChangedCallback = procedure(const aDeviceName: string; const aTipIndex: integer;
        const aTipTypeName: string) of object;

    TTipUsageRec = record
        TipMap: TIPMAP;
        TipTypeName: string;
        BasicType: TBasicTipType;
    end;

    TTipUsageRecArray = array of TTipUsageRec;

    TTipUsage = class
    private
        fTipmap: TIPMAP;
        fTipTypeName: string;
        fBasicType: TBasicTipType;
    public
        constructor Create(const aTipmap: TIPMAP; const aTipTypeName: string;
            const aBasicType: TBasicTipType);
        procedure AddTipToTipmap(const aTipIndex: integer);
        property Tipmap: TIPMAP read fTipmap;
        property TipTypeName: string read fTipTypeName;
        property BasicType: TBasicTipType read fBasicType;
    end;

    TTipUsageList = class(TObjectList<TTipUsage>)
    public
        function TipUsageByTypeName(const aTypeName: string): TTipUsage;
        function TipUsageByBasicType(const aBasicType: TBasicTipType): TTipUsage;
        function ToTipUsageArray(): TTipUsageRecArray;
    end;

    IPipDevice = interface(IDevice)
        ['{0DC3E36B-59D3-49A7-A14F-735963353E17}']
        function GetDropTipMap: TIPMAP;
        function GetOKTips: TIPMAP;
        function GetDirtyTips: TIPMAP;
        function GetDryAfterFlushTips: TIPMAP;

        // from customarm
        function GetTip(aIndex: Integer): TTipSystem;
        function GetFirstUsedTipIndex: integer;
        function GetFirstUsedTipName: string;
        function GetFirstUsedTipType: TBasicTipType;
        function GetFirstUsedTipDiTiType: string;
        function GetLastUsedTips: TIPMAP;
        function GetLastUsedTipType: string;
        function GetTipCount: integer;
        function GetUsedTipCount: Integer;
        function GetUsedTips: TIPMAP;

        function GetTipXOffset_mm(aIndex: Integer): TPosMM;
        function GetTipYOffset_mm(aIndex: Integer): TPosMM;
        function GetTipZOffset_mm(aIndex: Integer): TPosMM;
        function GetTipDryZOffset_mm(aIndex: Integer): TPosMM;
        function GetTipWashZOffset_mm(aIndex: Integer): TPosMM;
        function GetTipWasteZOffset_mm(aIndex: Integer): TPosMM;

        function GetNoOfUsedTips(aTipMap: TIPMAP): integer;
        function GetNoOfUnusedTips(aTipMap: TIPMAP): integer;

        function GetFirstUsedTip: TTipSystem;
        function GetInitTipMap(): TIPMAP;

        function GetHasTipWithMultiPumps(): boolean;
        function CreateTip(): TTipSystem; { TODO -oPK -cDEVICE : Create real tip }
        function GetRackNamePrefix: string;
        procedure SetRackNamePrefix(const aValue: string);

        function GetOnShowUseTips: TShowUseTipsEvent;
        procedure SetOnShowUseTips(const aValue: TShowUseTipsEvent);
        function GetOnTipTypeChanged: TTipTypeChangedCallback;
        procedure SetOnTipTypeChanged(const aValue: TTipTypeChangedCallback);

        procedure St_ClearTipStatus();

        procedure DropDispTips(aTips: TIPMAP);
        procedure FetchDispTips(aTips: TIPMAP);

        function GetUsedTipNo(aUsedTipCnt: integer): integer;
        procedure InitTipType(aIndex: integer; const aTipTypeName: string);
        procedure InitTipTypes(const aTipTypeNames: TArray<string>);
        procedure StoreTips();
        procedure LoadTips(const aTipTypeNames: TArray<string>);

        function SetUseTips(aTipName: string; aUseTips: TIPMAP; aOverwriteTipTypes: boolean): TIPMAP;
        function DetermineUsableTips(const aTipName: string; const aUseTips: TIPMAP): TIPMAP; overload;
        function DetermineUsableTips(const aUseTips: TIPMAP): TIPMAP; overload;
        function GetTipNames(aTipTypes: TBasicTipTypes): TArray<string>;
        function GetInitTipUsage(const aTipTypes: TBasicTipTypes): TTipUsageRecArray;
        function GetTipUsageForTipMap(const aTipMap: TipMap; const aTipTypes: TBasicTipTypes)
            : TTipUsageRecArray;

        function GetTip_XOffsets_mm(aTips: TIPMAP): TArray<double>;
        function GetTip_YOffsets_mm(aTips: TIPMAP): TArray<double>;
        function GetTip_ZOffsets_mm(): TArray<double>;

        function GetZRetractTips(aTipMap: TIPMAP; aDispense: boolean): TIPMAP;

        function GetNullDoubleArray: TArray<double>;
        function GetNullIntArray: TArray<integer>;
        function GetPosArrayForTips(): TArray<integer>;

        procedure ExecutePipPumps(aTipMap: integer);

        property TipCount: integer read GetTipCount;
        property Tips[index: Integer]: TTipSystem read GetTip;
        property UseTips: TIPMAP read GetUsedTips;
        property UseTipCount: Integer read GetUsedTipCount;
        property LastUseTips: TIPMAP read GetLastUsedTips;
        property LastUseTipType: string read GetLastUsedTipType;
        property FirstUsedTipType: TBasicTipType read GetFirstUsedTipType;
        property FirstUsedTipDiTiType: string read GetFirstUsedTipDiTiType;
        property FirstUsedTipName: string read GetFirstUsedTipName;
        property FirstUsedTipIndex: integer read GetFirstUsedTipIndex;

        property DropTipmap: TIPMAP read GetDropTipMap;
        property OKTips: TIPMAP read GetOKTips;
        property DirtyTips: TIPMAP read GetDirtyTips;
        property DryAfterFlushTips: TIPMAP read GetDryAfterFlushTips;
        property RackNamePrefix: string read GetRackNamePrefix write SetRackNamePrefix;
        property PosArrayForTips: TArray<integer>read GetPosArrayForTips;
        property HasTipWithMultiPumps: boolean read GetHasTipWithMultiPumps;
        property OnShowUseTips: TShowUseTipsEvent read GetOnShowUseTips write SetOnShowUseTips;
        property OnTipTypeChanged: TTipTypeChangedCallback read GetOnTipTypeChanged write SetOnTipTypeChanged;
    end;


implementation


uses
    SysUtils,
    TipmapUtils;

function TTipUsageList.TipUsageByTypeName(const aTypeName: string): TTipUsage;
var
    xTipUsage: TTipUsage;
begin
    result := nil;
    for xTipUsage in self do
    begin
        if SameText(xTipUsage.TipTypeName, aTypeName) then
        begin
            result := xTipUsage;
            EXIT;
        end;
    end;
end;

function TTipUsageList.ToTipUsageArray: TTipUsageRecArray;
var
    x: integer;
    xTipUsage: TTipUsage;
begin
    SetLength(result, self.Count);
    x := 0;
    for xTipUsage in self do
    begin
        result[x].TipMap := xTipUsage.Tipmap;
        result[x].TipTypeName := xTipUsage.TipTypeName;
        result[x].BasicType := xTipUsage.BasicType;
        Inc(x);
    end;
end;

function TTipUsageList.TipUsageByBasicType(const aBasicType: TBasicTipType): TTipUsage;
var
    xTipUsage: TTipUsage;
begin
    result := nil;
    for xTipUsage in self do
    begin
        if xTipUsage.BasicType = aBasicType then
        begin
            result := xTipUsage;
            EXIT;
        end;
    end;
end;

{ TTipUsage }

procedure TTipUsage.AddTipToTipmap(const aTipIndex: integer);
begin
    TTipmapUtils.SelectTip(fTipmap, aTipIndex);
end;

constructor TTipUsage.Create(const aTipmap: TIPMAP; const aTipTypeName: string;
    const aBasicType: TBasicTipType);
begin
    inherited Create();
    fTipmap := aTipmap;
    fTipTypeName := aTipTypeName;
    fBasicType := aBasicType;
end;


end.
