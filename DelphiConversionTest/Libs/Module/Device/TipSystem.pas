{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Abstract class for TipSystem. Implementation in TipSystemExt
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.11.07 wl  St_WasteVolume,St_ActWasteVol,St_DisableWaste  TN3897  Alte Verwaltung des AddWasteVol. entfernt
  09.01.08 wl  St_DPeriVol                   TN3972    entfernt
  20.06.08 pk                                TN4139    TArmPanel removed
  03.07.08 wl                                TN4157
  25.09.08 wl                                TN4242   es gibt nur noch WashMethod, nicht mehr WashMacro
  19.05.09 wl  Initialize                    TN4572   PipDeviceName als Parameter
  27.07.09 wl  Initialize                    TN4680   ArmTipIndex als Parameter
  31.07.09 wl  IsSampleAlreadyMixed          TN4049    vergleicht aktuelle Werte mit fLastAspMixJob
  12.09.09 wl  GetPipPump,GetSyrVolume       TN4740   Indizierte Properties durch Funktionen ersetzt
  08.04.10 pk  TTipSystemCalc                TN4996   code from TTipSystemExt
  04.15.10 pk  ResetData                     TN5050   new
  21.06.10 wl  St_CalculatePipVolume         TN5159   Parameter geändert
  29.06.10 pk                                TN5173   New GetChannelDefaultPumpIndices
  21.07.10 pk  GetDispSpeeds                 TN5185   New
  12.08.10 wl                                TN5227   PipPump-Methoden überarbeitet
  12.08.10 wl  GetPipPumpMaxVolume           TN5227   ersetzt die Methoden GetSyrvolume, GetMaxVolume, GetMaxChannelVolume
  12.08.10 wl  cChannel1Index,cChannel2Index TN5227   --> LiquidHandlingLow
  19.07.11 wl  St_DTransAirRetake            TN5630   St_DTransAirRetake boolean statt double
  20.09.11 wl  StoreDsp                      TN5723   neuer out-Parameter VolumeData
  28.09.11 wl  StoreAsp                      TN5725   neuer out-Parameter VolumeData
  31.10.11 wl  St_CalculatePipVolume         TN5731   UseVolControl entfernt
  03.11.11 wl  TLiqInfo                      TN5725   von TipSystemExt hierher
  03.11.11 wl  StoreDsp, StoreAsp            TN5725   --> SubstanceLoading
  17.11.11 wl  TLiqInfo.Clear                TN5725   entfernt
  26.01.12 wl  TLiqInfo.ReduceVolumes        TN5785   Berechnungsfehler korrigiert
  10.08.12 wl  Wm_UsePeriPump                TN5947   Neues Feld für "Wash after dispense"
  19.11.12 ts  GetPipPumpCurrentVolume       TN6019   Neu um komplettes Volumen abzugeben (ViscTool)
  11.02.13 ts  TipSystem                     TN6082   Neu: GetNumberOfPumps, Anzahl der Pumpen an Tip
  15.02.13 wl  TAspirateType,TDispenseType   TN5914   --> AppTypes
  15.02.13 wl  TLiqInfo                      TN5914   --> VolumeInfo
  15.02.13 wl  fLiqInfoList                  TN5914   --> IntfPipPumpDevice, IntfRediDevice
  25.04.13 wl  TWashJob                      TN6139.1 DestVol & DilRack entfernt
  -------------------------------------------------------------------------------------------------- }

unit TipSystem;


interface


uses
    CommonTypes,
    IntfPipPumpDevice,
    AppTypes,
    IntfRediDevice,
    LiqHTypes;

type
    TWashJob = record
        IsPipAction: boolean;
        SRack: string;
        SPos: integer;
        SourceVol, DilRackVol, DispMixVol, SampleAspMixVol: double;
        Wash: TLiqParWashRec;
        UsedTipType: string;
        DirtyBatchID: integer;
    end;

    TTipSystemCalc = class
    public
        class function CalcExtraGapAirVol(const aExtraGapCount: integer; const aExtraGapAirVol: double)
            : extended;
        class function CalcExtraGapWasteVol(const aExtraGapCount: integer; const aExtraGapWasteVol: double)
            : extended;
        class function CalcExtraGapVol(const aExtraGapCount: integer;
            const aExtraGapAirVol, aExtraGapWasteVol: double): extended;
        class function CalcMaxVolWithSampleAspWastePerCent(const aMaxVolume: extended;
            const aSampleAspWastePerCent: double): extended;

        class function CalcMaxSampleAndWasteVol(const aMaxVolume: extended;
            const aTransAirVol, aSysAirAspVol: double; const aExtraGapCount: integer;
            const aExtraGapAirVol, aExtraGapWasteVol: double; const aSampleAspWasteVol: double;
            const aSampleAspWastePerCent: double; const aSampleAspSpitBack: double;
            const aSpitBackCount: integer): extended;
    end;

    TTipSystem = class
    protected
        FRediDev: IRediDevice; // ist nicht Owner dieser Devices!
        FDitiLoaded: boolean;
        FArmTipIndex: integer;
        fRemovableTipPutMethod: string;
        fPipDeviceName: string;
        FPipPumps: TPipPumpDeviceArray;

        // Tip Status
        FSt_LhPtr: TLiqHandlingRec;
        FSt_IsClear: boolean;
        FSt_DVol, // Systemflüssigkeits - Volumen
        FSt_SVol, // Abgabevolumen (Dispense)
        FSt_DilVol, // Systemflüssigkeitsvolumen
        FSt_TotalVol, // Volumen auf das aufgefüllt werden soll -> Option REFILL_TUBE
        FSt_SaveSVol, // Zwischenspeicher
        FSt_SaveDVol, // Zwischenspeicher
        FSt_SaveDilVol, // Zwischenspeicher
        FSt_DAirVol: extended; // DispenseVolumen SysAir+TransAir
        FSt_DTransAirDisp: boolean; // Soll Transairvolumen beim Dispense abgegeben werden
        FSt_DTransAirRetake: boolean; // Soll Transairvolumen nach dem Dispense nochmals aufgenommen werden
        FSt_ActSysAirVol: extended;
        FSt_ActExtraGapTaken: boolean;
        FSt_DilVolWithCh2: Boolean;
        FSt_SVolSteps: extended;

        // Wash Management
        FWm_Dirty: boolean;

        FWm_WashVolCh1, FWm_WashVolCh2: extended;
        fWm_WashMethodName: string;
        fWm_DryAfterWash: boolean;
        fWm_UsePeripump: boolean;
        //
        FWm_WorkType: TWorkType;

        function GetBasicType: TBasicTipType; virtual; abstract;
        function GetDitiType: string; virtual; abstract;
        function GetMinVolume: double; virtual; abstract;
        function GetRelLength_mm: TPosMM; virtual; abstract;
        function GetToolName: string; virtual; abstract;
        function GetTypeName: string; virtual; abstract;
        function GetXOffset_mm: TPosMM; virtual; abstract;
        function GetYOffset_mm: TPosMM; virtual; abstract;

        function GetDryAfterFlush: boolean; virtual; abstract;
        function GetInitNeeded: boolean; virtual; abstract;
        function GetDryZOffset_mm: TPosMM; virtual; abstract;
        function GetWashZOffset_mm: TPosMM; virtual; abstract;
        function GetWasteZOffset_mm: TPosMM; virtual; abstract;

        function GetSpitBackCount(): integer; virtual; abstract;

        function GetMethodGetTip: string; virtual; abstract;
        function GetMethodPutTip: string; virtual; abstract;
        function GetHasMultiplePumps(): boolean; virtual; abstract;
        function GetNumberOfPumps: integer; virtual; abstract;

        function St_CalcExtraGapVol: extended; virtual; abstract;
    public
        constructor Create;
        // public methods
        procedure ResetData(); virtual; abstract;
        procedure Initialize(const aPipDeviceName: string; aArmTipIndex: integer; aTypeData: TTipType;
            aPipPumps: TPipPumpDeviceArray); virtual; abstract;

        // PipPump methods
        function GetPipPumpCurrentAspSpeed(const aIndex: integer): double; virtual; abstract;
        function GetPipPumpDispSpeed(const aIndex: integer): double; virtual; abstract;
        function GetPipPump(const aIndex: integer): IPipPumpDevice; virtual; abstract;
        function GetPipPumpMaxVolume(const aIndex: integer): double; virtual; abstract;
        function GetPipPumpCurrentVolume(const aIndex: integer): double; virtual; abstract;

        // Tip Status
        procedure St_InitTipStatus(); virtual; abstract;
        procedure St_ClearTipStatus(); virtual; abstract;
        procedure St_StorePipPosVolume(aSourceVol, aDestVol, aDilVol: extended;
            aDTransAirDisp, aDTransAirRetake: boolean; aSVolSteps: extended; aDilAspCh2: boolean);
            virtual; abstract;
        procedure St_CalculatePipVolume(aMultiPip: boolean; aAspSplitMinVolPercent: extended);
            virtual; abstract;
        function St_GetChannel1PumpIndex: integer; virtual; abstract;

        // Wash Management
        procedure Wm_SaveJobToTip(const aJob: TWashJob; aWorkType: TWorkType); virtual; abstract;
        procedure Wm_Clean; virtual; abstract;
        function Wm_TipNeedsWash(const aUpcomingJob: TWashJob): boolean; virtual; abstract;
        function Wm_IsDirtyNeedingAutoWash(aDirtyBatchID: integer): boolean; virtual; abstract;
        procedure Wm_DisableWash(); virtual; abstract;
        // First-Asp-Mix-Management
        procedure ResetLastAspMix(); virtual; abstract;
        procedure SaveLastAspMix(const aSourceRack: string; aSourcePos: integer); virtual; abstract;
        function IsSampleAlreadyMixed(const aSourceRack: string; aSourcePos: integer): boolean;
            virtual; abstract;

        // read-only properties
        property RediDev: IRediDevice read FRediDev;
        property TypeName: string read GetTypeName;
        property XOffset_mm: TPosMM read GetXOffset_mm;
        property YOffset_mm: TPosMM read GetYOffset_mm;
        property RelLength_mm: TPosMM read GetRelLength_mm;
        property MinVolume: double read GetMinVolume;
        property BasicType: TBasicTipType read GetBasicType;
        property DitiType: string read GetDitiType;
        property ToolName: string read GetToolName;
        property DryZOffset_mm: TPosMM read GetDryZOffset_mm;
        property WashZOffset_mm: TPosMM read GetWashZOffset_mm;
        property WasteZOffset_mm: TPosMM read GetWasteZOffset_mm;
        property DryAfterFlush: boolean read GetDryAfterFlush;
        property InitNeeded: boolean read GetInitNeeded;
        property MethodGetTip: string read GetMethodGetTip;
        property MethodPutTip: string read GetMethodPutTip;
        property HasMultiplePumps: boolean read GetHasMultiplePumps;
        property NumberOfPumps: integer read GetNumberOfPumps;

        // Tip Status
        property St_LhPtr: TLiqHandlingRec read FSt_LhPtr write FSt_LhPtr;
        property St_DVol: extended read FSt_DVol write FSt_DVol;
        property St_SVol: extended read FSt_SVol write FSt_SVol;
        property St_DilVol: extended read FSt_DilVol write FSt_DilVol;
        property St_TotalVol: extended read FSt_TotalVol;
        property St_SaveSVol: extended read FSt_SaveSVol write FSt_SaveSVol;
        property St_SaveDVol: extended read FSt_SaveDVol;
        property St_SaveDilVol: extended read FSt_SaveDilVol write FSt_SaveDilVol;
        property St_DAirVol: extended read FSt_DAirVol;
        property St_DTransAirDisp: boolean read FSt_DTransAirDisp;
        property St_DTransAirRetake: boolean read FSt_DTransAirRetake;
        property St_ActSysAirVol: extended read FSt_ActSysAirVol write FSt_ActSysAirVol;
        property St_SVolSteps: extended read FSt_SVolSteps;
        property St_ActExtraGapTaken: boolean read FSt_ActExtraGapTaken write FSt_ActExtraGapTaken;
        property St_DilVolWithCh2: boolean read FSt_DilVolWithCh2;
        property St_SpitBackCount: integer read GetSpitBackCount;
        property St_IsClear: boolean read FSt_IsClear;

        // Wash Management
        property Wm_Dirty: boolean read FWm_Dirty;
        property Wm_WashVolCh1: extended read fWm_WashVolCh1;
        property Wm_WashVolCh2: extended read fWm_WashVolCh2;
        property Wm_WashMethodName: string read fWm_WashMethodName;
        property Wm_DryAfterWash: boolean read fWm_DryAfterWash;
        property Wm_UsePeripump: boolean read fWm_UsePeripump;
        property Wm_WorkType: TWorkType read FWm_WorkType;

        // read-write properties
        property DiTiLoaded: boolean read FDiTiLoaded write FDitiLoaded;
        property RemovableTipPutMethod: string read FRemovableTipPutMethod write FRemovableTipPutMethod;
        property ArmTipIndex: integer read FArmTipIndex;

        class function PipPumpNumberToPipPumpIndex(aPipPumpNumber: integer): integer;
    end;


implementation


uses
    SysUtils;

{ TTipSystemCalc }

class function TTipSystemCalc.CalcExtraGapAirVol(const aExtraGapCount: integer; const aExtraGapAirVol: double)
    : extended;
begin
    result := aExtraGapCount * aExtraGapAirVol;
end;

class function TTipSystemCalc.CalcExtraGapWasteVol(const aExtraGapCount: integer;
    const aExtraGapWasteVol: double): extended;
begin
    result := aExtraGapCount * aExtraGapWasteVol;
end;

class function TTipSystemCalc.CalcExtraGapVol(const aExtraGapCount: integer;
    const aExtraGapAirVol, aExtraGapWasteVol: double): extended;
begin
    result := CalcExtraGapAirVol(aExtraGapCount, aExtraGapAirVol) + CalcExtraGapWasteVol(aExtraGapCount,
        aExtraGapWasteVol);
end;

class function TTipSystemCalc.CalcMaxVolWithSampleAspWastePerCent(const aMaxVolume: extended;
    const aSampleAspWastePerCent: double): extended;
var
    xMaxVolume: extended;
    x: integer;
begin
    result := aMaxVolume;

    if (aSampleAspWastePerCent > 0) then
    begin
        // Maximalvolumen ist rekursiv, wenn Waste in Prozent angegeben
        // Das errechnete Maximalvolumen wird selbst zur Grundlage der Berechnung
        xMaxVolume := result;

        // 04.08.10 pk :  was soll das 0 bis 4?
        for x := 0 to 4 do
            xMaxVolume := result - (aSampleAspWastePerCent * xMaxVolume / 100);

        result := xMaxVolume;
    end;
end;

class function TTipSystemCalc.CalcMaxSampleAndWasteVol(const aMaxVolume: extended;
    const aTransAirVol, aSysAirAspVol: double; const aExtraGapCount: integer;
    const aExtraGapAirVol, aExtraGapWasteVol: double; const aSampleAspWasteVol: double;
    const aSampleAspWastePerCent: double; const aSampleAspSpitBack: double; const aSpitBackCount: integer)
    : extended;
begin
    result := aMaxVolume;

    result := result - aTransAirVol - aSysAirAspVol - CalcExtraGapVol(aExtraGapCount, aExtraGapAirVol,
        aExtraGapWasteVol) - aSampleAspWasteVol - aSampleAspSpitBack * aSpitBackCount;

    result := CalcMaxVolWithSampleAspWastePerCent(result, aSampleAspWastePerCent);
end;

{ TTipSystem }

constructor TTipSystem.Create;
begin
    inherited;
end;

class function TTipSystem.PipPumpNumberToPipPumpIndex(aPipPumpNumber: integer): integer;
begin
    // PipPumpNumber ist 1-terminiert, kann aber auch 0 sein, dann ist die erste PipPump mit Index 0 gemeint
    if aPipPumpNumber = 0 then
        EXIT(0);

    EXIT(aPipPumpNumber - 1);
end;


end.
