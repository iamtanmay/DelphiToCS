{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Data adaptor for Tipset.db
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  30.08.07 pk                                TN3840.2 New
  03.07.08 wl                                TN4157
  25.09.08 wl                                TN4242   es gibt nur noch WashMethod, nicht mehr WashMacro
  18.11.08 wl  TLiqHandlingRec               TN4312   Begrenzte strings durch string ersetzt
  31.07.09 wl  TLiqHandlingRec               TN3950   3 neue Felder: SampleAspInsertMoveType, DilAspInsertMoveType, DispInsertMoveType
  31.07.09 wl  TLiqHandlingRec               TN4024   neu: DispSpeZMoveFrequency (Typ: double)
  31.07.09 wl  TLiqHandlingMixRec            TN4049   (SampleAspMix)FirstOnly ist jetzt dem MixRec zugeordnet
  31.07.09 wl  TLiqParWashRec,TLiqHandlingRec TN4693   WashFlag und ErrFlag (3 mal) sind jetzt integer
  31.07.09 wl  TLiqHandlingRec               TN4693   Konsistente Datentypen: Alle Volumen,Volspeeds,Delays,Faktoren sind double; single,smallint gibt es nicht mehr
  29.06.10 pk  TLiqHandlingRec               TN5173   New Ch1PumpNumber
  26.08.10 ts  TLiqHandlingRec               TN5248   new: MoveToSysAirPosSpeed
  22.09.10 wl  TLiqHandlingRec               TN5275   new: ExtraGapAirPos
  19.07.11 wl  TLiqHandlingRec               TN5630   new: TransAirRetakeBetweenDisp, TransAirRetakeAfterLastDisp
  27.09.11 wl  TLiqHandlingData              TN5698   ähnlich wie TLiqHandlingRec, aber als TStreamable
  02.12.11 wl  TLiqHandlingMixData           TN5758   Neue Felder: 'DispMixUseCalculatedVol', 'SampleAspMixUseCalculatedVol'
  23.02.12 wl  TLiqHandlingRec               TN5818   Neue Felder: SampleAspTipTouchScan,-Submerge,DispTipTouchScan,-Submerge = 'SampleAspTipTouchScan';
  10.08.12 wl  TLiqHandlingRec               TN5947   Neues Feld: WashUsePeripump
  04.09.12 wl  TLiqHandlingRec               TN5972   Neues Feld: SampleAspSpitBackAtAspPos (DilAspSpitBack entfernt)
  26.11.12 wl  TMixModes                     TN6027   neu: cUseRetract
  02.01.13 wl  TLiqHandlingRec               TN6064   SampleAspMixFirstOnly ist jetzt TLiqHandlingRec zugeordnet
  02.01.13 wl  TLiqHandlingRec               TN6064   Neues Feld: DispMixAspSubmerge
  07.05.13 ts  TLiqHandlingRec               TN6118   Neues Feld: DispTipTouchScanStoreVol
  21.10.13 wl  TLiqHandlingParamComparer     TN6276   'UsedTips' und 'UsedTipType' können jetzt unterschiedlich sein
  -------------------------------------------------------------------------------------------------- }

unit LiqHTypes;


interface


uses
    CommonTypes,
    Streamable;

const
    INT_LIQPARAM_USEWASHMETHOD_NO = 0;
    INT_LIQPARAM_USEWASHMETHOD_YES = 1;

type
    // Mix Modes
    TMixModes = record
    public const
        cDispenseAtZOffset = $0001; // Dispense at Z-Max - Z-Offset
        cMixWithAir = $0002; // Aspirate air and mix with it
        cAspirateTracking = $0004; // Aspirate: Use tracking
        cAspirateWithScan = $0008; // Aspirate: Scan for Liquid position
        cUseRetract = $0010; // Use retract speed when moving up z between the cycles
    end;

    TLiqParWashRec = record
        IsForced: boolean;
        UseMethod: boolean;
        WashMethodName: string;
        VolMin: double;
        VolMax: double;
        Flag: integer;
        VolFactor: double;
        VolChannel2: double;
        DryAfterWash: boolean;
        UsePeripump: boolean;
    end;

    TLiqHandlingMixRec = record
        Cycles: integer;
        Method: integer;
        Vol: double;
        MinRestVol: double;
        UseCalculatedVol: boolean;
        AspSpeed: double; // Pumpspeed: double
        DispSpeed: double; // Pumpspeed: double
        ZOffset_mm: double;
    end;

    TLiqHandlingRec = record
    public
        Valid: boolean;
        PARAMNAME: string;
        Description: string;
        LiqClass: string;
        VolCorrCurve: string;

        SampleAspCalc: boolean;
        SampleAspSpeed: double; // Pumpspeed: double
        SampleAspDelay: double;
        SampleAspLiqDet: integer;
        SampleAspSubmerge: double;
        SampleAspRetractPos_mm: double;
        SampleAspWasteVol: double;
        SampleAspErrFlag: integer;
        AspSwitchPos: integer;
        AspSwitchModule: string;
        RecordDetectionVolume: boolean;
        AspirationMethodRetractZ: boolean;
        SampleAspScanMode: integer;
        SampleAspScanSpeed: integer; // Motorspeed: integer
        SampleAspRetrSpeed: integer; // Motorspeed: integer
        SampleAspRetrDistance: double;
        SampleAspInsertMoveType: integer;

        SampleAspMix: TLiqHandlingMixRec;
        SampleAspMixFirstOnly: boolean;

        SampleAspMultipip: boolean;
        SampleAspMultiMaxVol: double;
        SampleAspMultiMinVol: double;
        SampleAspMultiSplitVol: double;
        SampleAspWastePerCent: double;
        SampleAspMultiWash: Boolean;
        SampleAspTipSingleRetract: Boolean;

        SampleAspSpitBack: double; // Volumen
        SampleAspSpitBackCount: integer;
        SampleAspSpitBackCalc: boolean;
        SampleAspSpitBackSpeed: double; // Pumpspeed: double
        SampleAspSpitBackAtAspPos: boolean;

        SampleAspCh2WashVol: double;
        SampleAspCh2WashCalc: Boolean;
        SampleAspCh2WashSpeed: double; // Pumpspeed: double
        SampleAspCh2WashDelay: double;

        SampleAspTipTouch: boolean;
        SampleAspTipTouchDelay: double;
        SampleAspTipTouchScan: boolean;
        SampleAspTipTouchScanMode: integer;
        SampleAspTipTouchSingle: boolean;
        SampleAspTipTouchSubmerge: double;

        DilAspCalc: Boolean;
        DilAspSpeed: double; // Pumpspeed: double
        DilAspDelay: double;
        DilAspSubmerge: double;
        DilAspWasteVol: double;
        DilAspLiqDet: integer;
        DilAspErrFlag: integer;
        DilAspTipSingleRetract: Boolean;
        DilAspSwitchPos: integer;
        DilAspSwitchModule: string;
        DilAspChannel2: boolean;
        DilAspScanMode: integer;
        DilAspScanSpeed: integer; // Motorspeed: integer
        DilAspRetrSpeed: integer; // Motorspeed: integer
        DilAspRetrDistance: double;
        DilAspInsertMoveType: integer;

        DispCalc: Boolean;
        DispSpeed: double; // Pumpspeed: double
        DispDelay: double;
        DispLiqDet: integer;
        DispErrFlag: integer;
        DispTipSingleRetract: Boolean;
        DispStepVolume: double;
        DispStepDelay: double;
        DispSubmerge: double;
        DspSwitchPos: integer;
        DspSwitchModule: string;
        DispScanMode: integer;
        DispScanSpeed: integer; // Motorspeed: integer
        DispRetrSpeed: integer; // Motorspeed: integer
        DispRetrDistance: double;
        DispInsertMoveType: integer;

        DispMix: TLiqHandlingMixRec;
        DispMixAspSubmerge: double;

        DispTipTouch: boolean;
        DispTipTouchDelay: double;
        DispTipTouchScan: boolean;
        DispTipTouchScanMode: integer;
        DispTipTouchSingle: boolean;
        DispTipTouchSubmerge: double;
        DispTipTouchScanStoreVol: boolean;

        Wash: TLiqParWashRec;

        SysAirAspPos: integer;
        SysAirAspDelay: double;
        SysAirAspCalc: boolean;
        SysAirAspVol: double;
        SysAirAspSpeed: double; // Pumpspeed: double
        SysAirDispVol: double;

        TransAirPos: integer;
        TransAirAspDelay: double;
        TransAirAspCalc: boolean;
        TransAirVol: double;
        TransAirSpeed: double; // Pumpspeed: double
        TransAirRetakeBetweenDisp: boolean;
        TransAirRetakeAfterLastDisp: boolean;

        ExtraGapCount: integer;
        ExtraGapAirVol: double;
        ExtraGapAirCalc: boolean;
        ExtraGapAirSpeed: double; // Pumpspeed: double
        ExtraGapAirDelay: double;
        ExtraGapAirPos: integer;
        ExtraGapWasteVol: double;
        ExtraGapWasteCalc: boolean;
        ExtraGapWasteSpeed: double; // Pumpspeed: double
        ExtraGapWasteDelay: double;

        GetTipFlag: integer;
        UsedTips: integer;
        UsedTipType: string;
        UsedPipDevice: string;
        UseDispTipWithoutTip: boolean;

        AspSplitMinVolPercent: double;
        DispSpeZMoveFrequency: double;

        // Powder Handling
        DispEmptyVarRedi: boolean;
        SampleAspXYShifting: boolean;
        DispXYShifting: boolean;

        Ch1PumpNumber: integer;
        MoveToSysAirPosSpeed: integer;
    end;

    TLiqHandlingWashData = class(TStreamable)
    private
        fIsForced: boolean;
        fUseMethod: boolean;
        fWashMethodName: string;
        fVolMin: double;
        fVolMax: double;
        fFlag: integer;
        fVolFactor: double;
        fVolChannel2: double;
        fDryAfterWash: boolean;
        fUsePeripump: boolean;
    public
        procedure WriteData(const aRec: TLiqParWashRec);
        function ReadData: TLiqParWashRec;
    published
        property IsForced: boolean read fIsForced write fIsForced;
        property UseMethod: boolean read fUseMethod write fUseMethod;
        property WashMethodName: string read fWashMethodName write fWashMethodName;
        property VolMin: double read fVolMin write fVolMin;
        property VolMax: double read fVolMax write fVolMax;
        property Flag: integer read fFlag write fFlag;
        property VolFactor: double read fVolFactor write fVolFactor;
        property VolChannel2: double read fVolChannel2 write fVolChannel2;
        property DryAfterWash: boolean read fDryAfterWash write fDryAfterWash;
        property UsePeripump: boolean read fUsePeripump write fUsePeripump;
    end;

    TLiqHandlingMixData = class(TStreamable)
    private
        fCycles: integer;
        fMethod: integer;
        fVol: double;
        fMinRestVol: double;
        fUseCalculatedVol: boolean;
        fAspSpeed: double; // Pumpspeed: double
        fDispSpeed: double; // Pumpspeed: double
        fZOffset_mm: double;
    public
        procedure WriteData(const aRec: TLiqHandlingMixRec);
        function ReadData: TLiqHandlingMixRec;
    published
        property Cycles: integer read fCycles write fCycles;
        property Method: integer read fMethod write fMethod;
        property Vol: double read fVol write fVol;
        property MinRestVol: double read fMinRestVol write fMinRestVol;
        property UseCalculatedVol: boolean read fUseCalculatedVol write fUseCalculatedVol;
        property AspSpeed: double read fAspSpeed write fAspSpeed; // Pumpspeed: double
        property DispSpeed: double read fDispSpeed write fDispSpeed; // Pumpspeed: double
        property ZOffset_mm: double read fZOffset_mm write fZOffset_mm;
    end;

    TLiqHandlingData = class(TStreamable)
    private
        fValid: boolean;
        fPARAMNAME: string;
        fDescription: string;
        fLiqClass: string;
        fVolCorrCurve: string;

        fSampleAspCalc: boolean;
        fSampleAspSpeed: double; // Pumpspeed: double
        fSampleAspDelay: double;
        fSampleAspLiqDet: integer;
        fSampleAspSubmerge: double;
        fSampleAspRetractPos_mm: double;
        fSampleAspWasteVol: double;
        fSampleAspErrFlag: integer;
        fAspSwitchPos: integer;
        fAspSwitchModule: string;
        fRecordDetectionVolume: boolean;
        fAspirationMethodRetractZ: boolean;
        fSampleAspScanMode: integer;
        fSampleAspScanSpeed: integer; // Motorspeed: integer
        fSampleAspRetrSpeed: integer; // Motorspeed: integer
        fSampleAspRetrDistance: double;
        fSampleAspInsertMoveType: integer;

        fSampleAspMix: TLiqHandlingMixData;
        fSampleAspMixFirstOnly: boolean;

        fSampleAspMultipip: boolean;
        fSampleAspMultiMaxVol: double;
        fSampleAspMultiMinVol: double;
        fSampleAspMultiSplitVol: double;
        fSampleAspWastePerCent: double;
        fSampleAspMultiWash: Boolean;
        fSampleAspTipSingleRetract: Boolean;

        fSampleAspSpitBack: double; // Volumen
        fSampleAspSpitBackCount: integer;
        fSampleAspSpitBackCalc: boolean;
        fSampleAspSpitBackSpeed: double; // Pumpspeed: double
        fSampleAspSpitBackAtAspPos: boolean;

        fSampleAspCh2WashVol: double;
        fSampleAspCh2WashCalc: Boolean;
        fSampleAspCh2WashSpeed: double; // Pumpspeed: double
        fSampleAspCh2WashDelay: double;

        fSampleAspTipTouch: boolean;
        fSampleAspTipTouchDelay: double;
        fSampleAspTipTouchScan: boolean;
        fSampleAspTipTouchScanMode: integer;
        fSampleAspTipTouchSingle: boolean;
        fSampleAspTipTouchSubmerge: double;

        fDilAspCalc: Boolean;
        fDilAspSpeed: double; // Pumpspeed: double
        fDilAspDelay: double;
        fDilAspSubmerge: double;
        fDilAspWasteVol: double;
        fDilAspLiqDet: integer;
        fDilAspErrFlag: integer;
        fDilAspTipSingleRetract: Boolean;
        fDilAspSwitchPos: integer;
        fDilAspSwitchModule: string;
        fDilAspChannel2: boolean;
        fDilAspScanMode: integer;
        fDilAspScanSpeed: integer; // Motorspeed: integer
        fDilAspRetrSpeed: integer; // Motorspeed: integer
        fDilAspRetrDistance: double;
        fDilAspInsertMoveType: integer;

        fDispCalc: Boolean;
        fDispSpeed: double; // Pumpspeed: double
        fDispDelay: double;
        fDispLiqDet: integer;
        fDispErrFlag: integer;
        fDispTipSingleRetract: Boolean;
        fDispStepVolume: double;
        fDispStepDelay: double;
        fDispSubmerge: double;
        fDspSwitchPos: integer;
        fDspSwitchModule: string;
        fDispScanMode: integer;
        fDispScanSpeed: integer; // Motorspeed: integer
        fDispRetrSpeed: integer; // Motorspeed: integer
        fDispRetrDistance: double;
        fDispInsertMoveType: integer;

        fDispMix: TLiqHandlingMixData;
        fDispMixAspSubmerge: double;

        fDispTipTouch: boolean;
        fDispTipTouchDelay: double;
        fDispTipTouchScan: boolean;
        fDispTipTouchScanMode: integer;
        fDispTipTouchSingle: boolean;
        fDispTipTouchSubmerge: double;
        fDispTipTouchScanStoreVol: boolean;

        fWash: TLiqHandlingWashData;

        fSysAirAspPos: integer;
        fSysAirAspDelay: double;
        fSysAirAspCalc: boolean;
        fSysAirAspVol: double;
        fSysAirAspSpeed: double; // Pumpspeed: double
        fSysAirDispVol: double;

        fTransAirPos: integer;
        fTransAirAspDelay: double;
        fTransAirAspCalc: boolean;
        fTransAirVol: double;
        fTransAirSpeed: double; // Pumpspeed: double
        fTransAirRetakeBetweenDisp: boolean;
        fTransAirRetakeAfterLastDisp: boolean;

        fExtraGapCount: integer;
        fExtraGapAirVol: double;
        fExtraGapAirCalc: boolean;
        fExtraGapAirSpeed: double; // Pumpspeed: double
        fExtraGapAirDelay: double;
        fExtraGapAirPos: integer;
        fExtraGapWasteVol: double;
        fExtraGapWasteCalc: boolean;
        fExtraGapWasteSpeed: double; // Pumpspeed: double
        fExtraGapWasteDelay: double;

        fGetTipFlag: integer;
        fUsedTips: integer;
        fUsedTipType: string;
        fUsedPipDevice: string;
        fUseDispTipWithoutTip: boolean;

        fAspSplitMinVolPercent: double;
        fDispSpeZMoveFrequency: double;

        // Powder Handling
        fDispEmptyVarRedi: boolean;
        fSampleAspXYShifting: boolean;
        fDispXYShifting: boolean;

        fCh1PumpNumber: integer;
        fMoveToSysAirPosSpeed: integer;
    public
        constructor Create(); override;
        destructor Destroy(); override;
        procedure WriteData(const aRec: TLiqHandlingRec);
        function ReadData: TLiqHandlingRec;
    published
        property Valid: boolean read fValid write fValid;
        property PARAMNAME: string read fPARAMNAME write fPARAMNAME;
        property Description: string read fDescription write fDescription;
        property LiqClass: string read fLiqClass write fLiqClass;
        property VolCorrCurve: string read fVolCorrCurve write fVolCorrCurve;

        property SampleAspCalc: boolean read fSampleAspCalc write fSampleAspCalc;
        property SampleAspSpeed: double read fSampleAspSpeed write fSampleAspSpeed;
        property SampleAspDelay: double read fSampleAspDelay write fSampleAspDelay;
        property SampleAspLiqDet: integer read fSampleAspLiqDet write fSampleAspLiqDet;
        property SampleAspSubmerge: double read fSampleAspSubmerge write fSampleAspSubmerge;
        property SampleAspRetractPos_mm: double read fSampleAspRetractPos_mm write fSampleAspRetractPos_mm;
        property SampleAspWasteVol: double read fSampleAspWasteVol write fSampleAspWasteVol;
        property SampleAspErrFlag: integer read fSampleAspErrFlag write fSampleAspErrFlag;
        property AspSwitchPos: integer read fAspSwitchPos write fAspSwitchPos;
        property AspSwitchModule: string read fAspSwitchModule write fAspSwitchModule;
        property RecordDetectionVolume: boolean read fRecordDetectionVolume write fRecordDetectionVolume;
        property AspirationMethodRetractZ: boolean read fAspirationMethodRetractZ
            write fAspirationMethodRetractZ;
        property SampleAspScanMode: integer read fSampleAspScanMode write fSampleAspScanMode;
        property SampleAspScanSpeed: integer read fSampleAspScanSpeed write fSampleAspScanSpeed;
        property SampleAspRetrSpeed: integer read fSampleAspRetrSpeed write fSampleAspRetrSpeed;
        property SampleAspRetrDistance: double read fSampleAspRetrDistance write fSampleAspRetrDistance;
        property SampleAspInsertMoveType: integer read fSampleAspInsertMoveType
            write fSampleAspInsertMoveType;

        property SampleAspMix: TLiqHandlingMixData read fSampleAspMix write fSampleAspMix;
        property SampleAspMixFirstOnly: boolean read fSampleAspMixFirstOnly write fSampleAspMixFirstOnly;

        property SampleAspMultipip: boolean read fSampleAspMultipip write fSampleAspMultipip;
        property SampleAspMultiMaxVol: double read fSampleAspMultiMaxVol write fSampleAspMultiMaxVol;
        property SampleAspMultiMinVol: double read fSampleAspMultiMinVol write fSampleAspMultiMinVol;
        property SampleAspMultiSplitVol: double read fSampleAspMultiSplitVol write fSampleAspMultiSplitVol;
        property SampleAspWastePerCent: double read fSampleAspWastePerCent write fSampleAspWastePerCent;
        property SampleAspMultiWash: Boolean read fSampleAspMultiWash write fSampleAspMultiWash;
        property SampleAspTipSingleRetract: Boolean read fSampleAspTipSingleRetract
            write fSampleAspTipSingleRetract;

        property SampleAspSpitBack: double read fSampleAspSpitBack write fSampleAspSpitBack;
        property SampleAspSpitBackCount: integer read fSampleAspSpitBackCount write fSampleAspSpitBackCount;
        property SampleAspSpitBackCalc: boolean read fSampleAspSpitBackCalc write fSampleAspSpitBackCalc;
        property SampleAspSpitBackSpeed: double read fSampleAspSpitBackSpeed write fSampleAspSpitBackSpeed;
        property SampleAspSpitBackAtAspPos: boolean read fSampleAspSpitBackAtAspPos
            write fSampleAspSpitBackAtAspPos;

        property SampleAspCh2WashVol: double read fSampleAspCh2WashVol write fSampleAspCh2WashVol;
        property SampleAspCh2WashCalc: Boolean read fSampleAspCh2WashCalc write fSampleAspCh2WashCalc;
        property SampleAspCh2WashSpeed: double read fSampleAspCh2WashSpeed write fSampleAspCh2WashSpeed;
        property SampleAspCh2WashDelay: double read fSampleAspCh2WashDelay write fSampleAspCh2WashDelay;

        property SampleAspTipTouch: boolean read fSampleAspTipTouch write fSampleAspTipTouch;
        property SampleAspTipTouchDelay: double read fSampleAspTipTouchDelay write fSampleAspTipTouchDelay;
        property SampleAspTipTouchScan: boolean read fSampleAspTipTouchScan write fSampleAspTipTouchScan;
        property SampleAspTipTouchScanMode: integer read fSampleAspTipTouchScanMode
            write fSampleAspTipTouchScanMode;
        property SampleAspTipTouchSingle: boolean read fSampleAspTipTouchSingle
            write fSampleAspTipTouchSingle;
        property SampleAspTipTouchSubmerge: double read fSampleAspTipTouchSubmerge
            write fSampleAspTipTouchSubmerge;

        property DilAspCalc: Boolean read fDilAspCalc write fDilAspCalc;
        property DilAspSpeed: double read fDilAspSpeed write fDilAspSpeed;
        property DilAspDelay: double read fDilAspDelay write fDilAspDelay;
        property DilAspSubmerge: double read fDilAspSubmerge write fDilAspSubmerge;
        property DilAspWasteVol: double read fDilAspWasteVol write fDilAspWasteVol;
        property DilAspLiqDet: integer read fDilAspLiqDet write fDilAspLiqDet;
        property DilAspErrFlag: integer read fDilAspErrFlag write fDilAspErrFlag;
        property DilAspTipSingleRetract: Boolean read fDilAspTipSingleRetract write fDilAspTipSingleRetract;
        property DilAspSwitchPos: integer read fDilAspSwitchPos write fDilAspSwitchPos;
        property DilAspSwitchModule: string read fDilAspSwitchModule write fDilAspSwitchModule;
        property DilAspChannel2: boolean read fDilAspChannel2 write fDilAspChannel2;
        property DilAspScanMode: integer read fDilAspScanMode write fDilAspScanMode;
        property DilAspScanSpeed: integer read fDilAspScanSpeed write fDilAspScanSpeed;
        property DilAspRetrSpeed: integer read fDilAspRetrSpeed write fDilAspRetrSpeed;
        property DilAspRetrDistance: double read fDilAspRetrDistance write fDilAspRetrDistance;
        property DilAspInsertMoveType: integer read fDilAspInsertMoveType write fDilAspInsertMoveType;

        property DispCalc: Boolean read fDispCalc write fDispCalc;
        property DispSpeed: double read fDispSpeed write fDispSpeed;
        property DispDelay: double read fDispDelay write fDispDelay;
        property DispLiqDet: integer read fDispLiqDet write fDispLiqDet;
        property DispErrFlag: integer read fDispErrFlag write fDispErrFlag;
        property DispTipSingleRetract: Boolean read fDispTipSingleRetract write fDispTipSingleRetract;
        property DispStepVolume: double read fDispStepVolume write fDispStepVolume;
        property DispStepDelay: double read fDispStepDelay write fDispStepDelay;
        property DispSubmerge: double read fDispSubmerge write fDispSubmerge;
        property DspSwitchPos: integer read fDspSwitchPos write fDspSwitchPos;
        property DspSwitchModule: string read fDspSwitchModule write fDspSwitchModule;
        property DispScanMode: integer read fDispScanMode write fDispScanMode;
        property DispScanSpeed: integer read fDispScanSpeed write fDispScanSpeed;
        property DispRetrSpeed: integer read fDispRetrSpeed write fDispRetrSpeed;
        property DispRetrDistance: double read fDispRetrDistance write fDispRetrDistance;
        property DispInsertMoveType: integer read fDispInsertMoveType write fDispInsertMoveType;

        property DispMix: TLiqHandlingMixData read fDispMix write fDispMix;
        property DispMixAspSubmerge: double read fDispMixAspSubmerge write fDispMixAspSubmerge;

        property DispTipTouch: boolean read fDispTipTouch write fDispTipTouch;
        property DispTipTouchDelay: double read fDispTipTouchDelay write fDispTipTouchDelay;
        property DispTipTouchScanMode: integer read fDispTipTouchScanMode write fDispTipTouchScanMode;
        property DispTipTouchScan: boolean read fDispTipTouchScan write fDispTipTouchScan;
        property DispTipTouchSingle: boolean read fDispTipTouchSingle write fDispTipTouchSingle;
        property DispTipTouchSubmerge: double read fDispTipTouchSubmerge write fDispTipTouchSubmerge;
        property DispTipTouchScanStoreVol: boolean read fDispTipTouchScanStoreVol
            write fDispTipTouchScanStoreVol;

        property Wash: TLiqHandlingWashData read fWash write fWash;

        property SysAirAspPos: integer read fSysAirAspPos write fSysAirAspPos;
        property SysAirAspDelay: double read fSysAirAspDelay write fSysAirAspDelay;
        property SysAirAspCalc: boolean read fSysAirAspCalc write fSysAirAspCalc;
        property SysAirAspVol: double read fSysAirAspVol write fSysAirAspVol;
        property SysAirAspSpeed: double read fSysAirAspSpeed write fSysAirAspSpeed;
        property SysAirDispVol: double read fSysAirDispVol write fSysAirDispVol;

        property TransAirPos: integer read fTransAirPos write fTransAirPos;
        property TransAirAspDelay: double read fTransAirAspDelay write fTransAirAspDelay;
        property TransAirAspCalc: boolean read fTransAirAspCalc write fTransAirAspCalc;
        property TransAirVol: double read fTransAirVol write fTransAirVol;
        property TransAirSpeed: double read fTransAirSpeed write fTransAirSpeed;
        property TransAirRetakeBetweenDisp: boolean read fTransAirRetakeBetweenDisp
            write fTransAirRetakeBetweenDisp;
        property TransAirRetakeAfterLastDisp: boolean read fTransAirRetakeAfterLastDisp
            write fTransAirRetakeAfterLastDisp;

        property ExtraGapCount: integer read fExtraGapCount write fExtraGapCount;
        property ExtraGapAirVol: double read fExtraGapAirVol write fExtraGapAirVol;
        property ExtraGapAirCalc: boolean read fExtraGapAirCalc write fExtraGapAirCalc;
        property ExtraGapAirSpeed: double read fExtraGapAirSpeed write fExtraGapAirSpeed;
        property ExtraGapAirDelay: double read fExtraGapAirDelay write fExtraGapAirDelay;
        property ExtraGapAirPos: integer read fExtraGapAirPos write fExtraGapAirPos;
        property ExtraGapWasteVol: double read fExtraGapWasteVol write fExtraGapWasteVol;
        property ExtraGapWasteCalc: boolean read fExtraGapWasteCalc write fExtraGapWasteCalc;
        property ExtraGapWasteSpeed: double read fExtraGapWasteSpeed write fExtraGapWasteSpeed;
        property ExtraGapWasteDelay: double read fExtraGapWasteDelay write fExtraGapWasteDelay;

        property GetTipFlag: integer read fGetTipFlag write fGetTipFlag;
        property UsedTips: integer read fUsedTips write fUsedTips;
        property UsedTipType: string read fUsedTipType write fUsedTipType;
        property UsedPipDevice: string read fUsedPipDevice write fUsedPipDevice;
        property UseDispTipWithoutTip: boolean read fUseDispTipWithoutTip write fUseDispTipWithoutTip;

        property AspSplitMinVolPercent: double read fAspSplitMinVolPercent write fAspSplitMinVolPercent;
        property DispSpeZMoveFrequency: double read fDispSpeZMoveFrequency write fDispSpeZMoveFrequency;

        // Powder Handling
        property DispEmptyVarRedi: boolean read fDispEmptyVarRedi write fDispEmptyVarRedi;
        property SampleAspXYShifting: boolean read fSampleAspXYShifting write fSampleAspXYShifting;
        property DispXYShifting: boolean read fDispXYShifting write fDispXYShifting;

        property Ch1PumpNumber: integer read fCh1PumpNumber write fCh1PumpNumber;
        property MoveToSysAirPosSpeed: integer read fMoveToSysAirPosSpeed write fMoveToSysAirPosSpeed;
    end;

    TLiqHandlingParamComparer = class
    private
        class function AreValuesCompatible(const aFieldName: string; const aV1, aV2: string;
            out oIncompatibleFieldName: string): boolean; overload;
        class function AreValuesCompatible(const aFieldName: string; const aV1, aV2: integer;
            out oIncompatibleFieldName: string): boolean; overload;
        class function AreValuesCompatible(const aFieldName: string; const aV1, aV2: extended;
            out oIncompatibleFieldName: string): boolean; overload;
        class function AreValuesCompatible(const aFieldName: string; const aV1, aV2: boolean;
            out oIncompatibleFieldName: string): boolean; overload;
        class function AreMixValuesCompatible(const aFieldName: string; const aV1, aV2: TLiqHandlingMixData;
            out oIncompatibleFieldName: string): boolean;
        class function AreWashValuesCompatible(const aFieldName: string; const aV1, aV2: TLiqHandlingWashData;
            out oIncompatibleFieldName: string): boolean;
    public
        class function AreCompatible(const aV1, aV2: TLiqHandlingData;
            out oIncompatibleFieldName: string): boolean;
    end;


implementation


uses
    SysUtils;

{ TLiqHandlingWashData }

function TLiqHandlingWashData.ReadData: TLiqParWashRec;
begin
    result.IsForced := fIsForced;
    result.UseMethod := fUseMethod;
    result.WashMethodName := fWashMethodName;
    result.VolMin := fVolMin;
    result.VolMax := fVolMax;
    result.Flag := fFlag;
    result.VolFactor := fVolFactor;
    result.VolChannel2 := fVolChannel2;
    result.DryAfterWash := fDryAfterWash;
    result.UsePeripump := fUsePeripump;
end;

procedure TLiqHandlingWashData.WriteData(const aRec: TLiqParWashRec);
begin
    fIsForced := aRec.IsForced;
    fUseMethod := aRec.UseMethod;
    fWashMethodName := aRec.WashMethodName;
    fVolMin := aRec.VolMin;
    fVolMax := aRec.VolMax;
    fFlag := aRec.Flag;
    fVolFactor := aRec.VolFactor;
    fVolChannel2 := aRec.VolChannel2;
    fDryAfterWash := aRec.DryAfterWash;
    fUsePeripump := aRec.UsePeripump;
end;

{ TLiqHandlingMixData }

function TLiqHandlingMixData.ReadData: TLiqHandlingMixRec;
begin
    result.Cycles := fCycles;
    result.Method := fMethod;
    result.Vol := fVol;
    result.MinRestVol := fMinRestVol;
    result.UseCalculatedVol := fUseCalculatedVol;
    result.AspSpeed := fAspSpeed;
    result.DispSpeed := fDispSpeed;
    result.ZOffset_mm := fZOffset_mm;
end;

procedure TLiqHandlingMixData.WriteData(const aRec: TLiqHandlingMixRec);
begin
    fCycles := aRec.Cycles;
    fMethod := aRec.Method;
    fVol := aRec.Vol;
    fMinRestVol := aRec.MinRestVol;
    fUseCalculatedVol := aRec.UseCalculatedVol;
    fAspSpeed := aRec.AspSpeed;
    fDispSpeed := aRec.DispSpeed;
    fZOffset_mm := aRec.ZOffset_mm;
end;

{ TLiqHandlingData }

constructor TLiqHandlingData.Create;
begin
    inherited;
    fSampleAspMix := TLiqHandlingMixData.Create;
    fDispMix := TLiqHandlingMixData.Create;
    fWash := TLiqHandlingWashData.Create;
end;

destructor TLiqHandlingData.Destroy;
begin
    FreeAndNil(fWash);
    FreeAndNil(fDispMix);
    FreeAndNil(fSampleAspMix);
    inherited;
end;

function TLiqHandlingData.ReadData: TLiqHandlingRec;
begin
    result.Valid := fValid;
    result.PARAMNAME := fPARAMNAME;
    result.Description := fDescription;
    result.LiqClass := fLiqClass;
    result.VolCorrCurve := fVolCorrCurve;

    result.SampleAspCalc := fSampleAspCalc;
    result.SampleAspSpeed := fSampleAspSpeed;
    result.SampleAspDelay := fSampleAspDelay;
    result.SampleAspLiqDet := fSampleAspLiqDet;
    result.SampleAspSubmerge := fSampleAspSubmerge;
    result.SampleAspRetractPos_mm := fSampleAspRetractPos_mm;
    result.SampleAspWasteVol := fSampleAspWasteVol;
    result.SampleAspErrFlag := fSampleAspErrFlag;
    result.AspSwitchPos := fAspSwitchPos;
    result.AspSwitchModule := fAspSwitchModule;
    result.RecordDetectionVolume := fRecordDetectionVolume;
    result.AspirationMethodRetractZ := fAspirationMethodRetractZ;
    result.SampleAspScanMode := fSampleAspScanMode;
    result.SampleAspScanSpeed := fSampleAspScanSpeed;
    result.SampleAspRetrSpeed := fSampleAspRetrSpeed;
    result.SampleAspRetrDistance := fSampleAspRetrDistance;
    result.SampleAspInsertMoveType := fSampleAspInsertMoveType;

    result.SampleAspMix := fSampleAspMix.ReadData;
    result.SampleAspMixFirstOnly := fSampleAspMixFirstOnly;

    result.SampleAspMultipip := fSampleAspMultipip;
    result.SampleAspMultiMaxVol := fSampleAspMultiMaxVol;
    result.SampleAspMultiMinVol := fSampleAspMultiMinVol;
    result.SampleAspMultiSplitVol := fSampleAspMultiSplitVol;
    result.SampleAspWastePerCent := fSampleAspWastePerCent;
    result.SampleAspMultiWash := fSampleAspMultiWash;
    result.SampleAspTipSingleRetract := fSampleAspTipSingleRetract;

    result.SampleAspSpitBack := fSampleAspSpitBack;
    result.SampleAspSpitBackCount := fSampleAspSpitBackCount;
    result.SampleAspSpitBackCalc := fSampleAspSpitBackCalc;
    result.SampleAspSpitBackSpeed := fSampleAspSpitBackSpeed;
    result.SampleAspSpitBackAtAspPos := fSampleAspSpitBackAtAspPos;

    result.SampleAspCh2WashVol := fSampleAspCh2WashVol;
    result.SampleAspCh2WashCalc := fSampleAspCh2WashCalc;
    result.SampleAspCh2WashSpeed := fSampleAspCh2WashSpeed;
    result.SampleAspCh2WashDelay := fSampleAspCh2WashDelay;

    result.SampleAspTipTouch := fSampleAspTipTouch;
    result.SampleAspTipTouchDelay := fSampleAspTipTouchDelay;
    result.SampleAspTipTouchScan := fSampleAspTipTouchScan;
    result.SampleAspTipTouchScanMode := fSampleAspTipTouchScanMode;
    result.SampleAspTipTouchSingle := fSampleAspTipTouchSingle;
    result.SampleAspTipTouchSubmerge := fSampleAspTipTouchSubmerge;

    result.DilAspCalc := fDilAspCalc;
    result.DilAspSpeed := fDilAspSpeed;
    result.DilAspDelay := fDilAspDelay;
    result.DilAspSubmerge := fDilAspSubmerge;
    result.DilAspWasteVol := fDilAspWasteVol;
    result.DilAspLiqDet := fDilAspLiqDet;
    result.DilAspErrFlag := fDilAspErrFlag;
    result.DilAspTipSingleRetract := fDilAspTipSingleRetract;
    result.DilAspSwitchPos := fDilAspSwitchPos;
    result.DilAspSwitchModule := fDilAspSwitchModule;
    result.DilAspChannel2 := fDilAspChannel2;
    result.DilAspScanMode := fDilAspScanMode;
    result.DilAspScanSpeed := fDilAspScanSpeed;
    result.DilAspRetrSpeed := fDilAspRetrSpeed;
    result.DilAspRetrDistance := fDilAspRetrDistance;
    result.DilAspInsertMoveType := fDilAspInsertMoveType;

    result.DispCalc := fDispCalc;
    result.DispSpeed := fDispSpeed;
    result.DispDelay := fDispDelay;
    result.DispLiqDet := fDispLiqDet;
    result.DispErrFlag := fDispErrFlag;
    result.DispTipSingleRetract := fDispTipSingleRetract;
    result.DispStepVolume := fDispStepVolume;
    result.DispStepDelay := fDispStepDelay;
    result.DispSubmerge := fDispSubmerge;
    result.DspSwitchPos := fDspSwitchPos;
    result.DspSwitchModule := fDspSwitchModule;
    result.DispScanMode := fDispScanMode;
    result.DispScanSpeed := fDispScanSpeed;
    result.DispRetrSpeed := fDispRetrSpeed;
    result.DispRetrDistance := fDispRetrDistance;
    result.DispInsertMoveType := fDispInsertMoveType;

    result.DispMix := fDispMix.ReadData;
    result.DispMixAspSubmerge := fDispMixAspSubmerge;

    result.DispTipTouch := fDispTipTouch;
    result.DispTipTouchDelay := fDispTipTouchDelay;
    result.DispTipTouchScan := fDispTipTouchScan;
    result.DispTipTouchScanMode := fDispTipTouchScanMode;
    result.DispTipTouchSingle := fDispTipTouchSingle;
    result.DispTipTouchSubmerge := fDispTipTouchSubmerge;
    result.DispTipTouchScanStoreVol := fDispTipTouchScanStoreVol;

    result.Wash := fWash.ReadData;

    result.SysAirAspPos := fSysAirAspPos;
    result.SysAirAspDelay := fSysAirAspDelay;
    result.SysAirAspCalc := fSysAirAspCalc;
    result.SysAirAspVol := fSysAirAspVol;
    result.SysAirAspSpeed := fSysAirAspSpeed;
    result.SysAirDispVol := fSysAirDispVol;

    result.TransAirPos := fTransAirPos;
    result.TransAirAspDelay := fTransAirAspDelay;
    result.TransAirAspCalc := fTransAirAspCalc;
    result.TransAirVol := fTransAirVol;
    result.TransAirSpeed := fTransAirSpeed;
    result.TransAirRetakeBetweenDisp := fTransAirRetakeBetweenDisp;
    result.TransAirRetakeAfterLastDisp := fTransAirRetakeAfterLastDisp;

    result.ExtraGapCount := fExtraGapCount;
    result.ExtraGapAirVol := fExtraGapAirVol;
    result.ExtraGapAirCalc := fExtraGapAirCalc;
    result.ExtraGapAirSpeed := fExtraGapAirSpeed;
    result.ExtraGapAirDelay := fExtraGapAirDelay;
    result.ExtraGapAirPos := fExtraGapAirPos;
    result.ExtraGapWasteVol := fExtraGapWasteVol;
    result.ExtraGapWasteCalc := fExtraGapWasteCalc;
    result.ExtraGapWasteSpeed := fExtraGapWasteSpeed;
    result.ExtraGapWasteDelay := fExtraGapWasteDelay;

    result.GetTipFlag := fGetTipFlag;
    result.UsedTips := fUsedTips;
    result.UsedTipType := fUsedTipType;
    result.UsedPipDevice := fUsedPipDevice;
    result.UseDispTipWithoutTip := fUseDispTipWithoutTip;

    result.AspSplitMinVolPercent := fAspSplitMinVolPercent;
    result.DispSpeZMoveFrequency := fDispSpeZMoveFrequency;

    // Powder Handling
    result.DispEmptyVarRedi := fDispEmptyVarRedi;
    result.SampleAspXYShifting := fSampleAspXYShifting;
    result.DispXYShifting := fDispXYShifting;

    result.Ch1PumpNumber := fCh1PumpNumber;
    result.MoveToSysAirPosSpeed := fMoveToSysAirPosSpeed;
end;

procedure TLiqHandlingData.WriteData(const aRec: TLiqHandlingRec);
begin
    fValid := aRec.Valid;
    fPARAMNAME := aRec.PARAMNAME;
    fDescription := aRec.Description;
    fLiqClass := aRec.LiqClass;
    fVolCorrCurve := aRec.VolCorrCurve;

    fSampleAspCalc := aRec.SampleAspCalc;
    fSampleAspSpeed := aRec.SampleAspSpeed;
    fSampleAspDelay := aRec.SampleAspDelay;
    fSampleAspLiqDet := aRec.SampleAspLiqDet;
    fSampleAspSubmerge := aRec.SampleAspSubmerge;
    fSampleAspRetractPos_mm := aRec.SampleAspRetractPos_mm;
    fSampleAspWasteVol := aRec.SampleAspWasteVol;
    fSampleAspErrFlag := aRec.SampleAspErrFlag;
    fAspSwitchPos := aRec.AspSwitchPos;
    fAspSwitchModule := aRec.AspSwitchModule;
    fRecordDetectionVolume := aRec.RecordDetectionVolume;
    fAspirationMethodRetractZ := aRec.AspirationMethodRetractZ;
    fSampleAspScanMode := aRec.SampleAspScanMode;
    fSampleAspScanSpeed := aRec.SampleAspScanSpeed;
    fSampleAspRetrSpeed := aRec.SampleAspRetrSpeed;
    fSampleAspRetrDistance := aRec.SampleAspRetrDistance;
    fSampleAspInsertMoveType := aRec.SampleAspInsertMoveType;

    fSampleAspMix.WriteData(aRec.SampleAspMix);
    fSampleAspMixFirstOnly := aRec.SampleAspMixFirstOnly;

    fSampleAspMultipip := aRec.SampleAspMultipip;
    fSampleAspMultiMaxVol := aRec.SampleAspMultiMaxVol;
    fSampleAspMultiMinVol := aRec.SampleAspMultiMinVol;
    fSampleAspMultiSplitVol := aRec.SampleAspMultiSplitVol;
    fSampleAspWastePerCent := aRec.SampleAspWastePerCent;
    fSampleAspMultiWash := aRec.SampleAspMultiWash;
    fSampleAspTipSingleRetract := aRec.SampleAspTipSingleRetract;

    fSampleAspSpitBack := aRec.SampleAspSpitBack;
    fSampleAspSpitBackCount := aRec.SampleAspSpitBackCount;
    fSampleAspSpitBackCalc := aRec.SampleAspSpitBackCalc;
    fSampleAspSpitBackSpeed := aRec.SampleAspSpitBackSpeed;
    fSampleAspSpitBackAtAspPos := aRec.SampleAspSpitBackAtAspPos;

    fSampleAspCh2WashVol := aRec.SampleAspCh2WashVol;
    fSampleAspCh2WashCalc := aRec.SampleAspCh2WashCalc;
    fSampleAspCh2WashSpeed := aRec.SampleAspCh2WashSpeed;
    fSampleAspCh2WashDelay := aRec.SampleAspCh2WashDelay;

    fSampleAspTipTouch := aRec.SampleAspTipTouch;
    fSampleAspTipTouchDelay := aRec.SampleAspTipTouchDelay;
    fSampleAspTipTouchScan := aRec.SampleAspTipTouchScan;
    fSampleAspTipTouchScanMode := aRec.SampleAspTipTouchScanMode;
    fSampleAspTipTouchSingle := aRec.SampleAspTipTouchSingle;
    fSampleAspTipTouchSubmerge := aRec.SampleAspTipTouchSubmerge;

    fDilAspCalc := aRec.DilAspCalc;
    fDilAspSpeed := aRec.DilAspSpeed;
    fDilAspDelay := aRec.DilAspDelay;
    fDilAspSubmerge := aRec.DilAspSubmerge;
    fDilAspWasteVol := aRec.DilAspWasteVol;
    fDilAspLiqDet := aRec.DilAspLiqDet;
    fDilAspErrFlag := aRec.DilAspErrFlag;
    fDilAspTipSingleRetract := aRec.DilAspTipSingleRetract;
    fDilAspSwitchPos := aRec.DilAspSwitchPos;
    fDilAspSwitchModule := aRec.DilAspSwitchModule;
    fDilAspChannel2 := aRec.DilAspChannel2;
    fDilAspScanMode := aRec.DilAspScanMode;
    fDilAspScanSpeed := aRec.DilAspScanSpeed;
    fDilAspRetrSpeed := aRec.DilAspRetrSpeed;
    fDilAspRetrDistance := aRec.DilAspRetrDistance;
    fDilAspInsertMoveType := aRec.DilAspInsertMoveType;

    fDispCalc := aRec.DispCalc;
    fDispSpeed := aRec.DispSpeed;
    fDispDelay := aRec.DispDelay;
    fDispLiqDet := aRec.DispLiqDet;
    fDispErrFlag := aRec.DispErrFlag;
    fDispTipSingleRetract := aRec.DispTipSingleRetract;
    fDispStepVolume := aRec.DispStepVolume;
    fDispStepDelay := aRec.DispStepDelay;
    fDispSubmerge := aRec.DispSubmerge;
    fDspSwitchPos := aRec.DspSwitchPos;
    fDspSwitchModule := aRec.DspSwitchModule;
    fDispScanMode := aRec.DispScanMode;
    fDispScanSpeed := aRec.DispScanSpeed;
    fDispRetrSpeed := aRec.DispRetrSpeed;
    fDispRetrDistance := aRec.DispRetrDistance;
    fDispInsertMoveType := aRec.DispInsertMoveType;

    fDispMix.WriteData(aRec.DispMix);
    fDispMixAspSubmerge := aRec.DispMixAspSubmerge;

    fDispTipTouch := aRec.DispTipTouch;
    fDispTipTouchDelay := aRec.DispTipTouchDelay;
    fDispTipTouchScan := aRec.DispTipTouchScan;
    fDispTipTouchScanMode := aRec.DispTipTouchScanMode;
    fDispTipTouchSingle := aRec.DispTipTouchSingle;
    fDispTipTouchSubmerge := aRec.DispTipTouchSubmerge;
    fDispTipTouchScanStoreVol := aRec.DispTipTouchScanStoreVol;

    fWash.WriteData(aRec.Wash);

    fSysAirAspPos := aRec.SysAirAspPos;
    fSysAirAspDelay := aRec.SysAirAspDelay;
    fSysAirAspCalc := aRec.SysAirAspCalc;
    fSysAirAspVol := aRec.SysAirAspVol;
    fSysAirAspSpeed := aRec.SysAirAspSpeed;
    fSysAirDispVol := aRec.SysAirDispVol;

    fTransAirPos := aRec.TransAirPos;
    fTransAirAspDelay := aRec.TransAirAspDelay;
    fTransAirAspCalc := aRec.TransAirAspCalc;
    fTransAirVol := aRec.TransAirVol;
    fTransAirSpeed := aRec.TransAirSpeed;
    fTransAirRetakeBetweenDisp := aRec.TransAirRetakeBetweenDisp;
    fTransAirRetakeAfterLastDisp := aRec.TransAirRetakeAfterLastDisp;

    fExtraGapCount := aRec.ExtraGapCount;
    fExtraGapAirVol := aRec.ExtraGapAirVol;
    fExtraGapAirCalc := aRec.ExtraGapAirCalc;
    fExtraGapAirSpeed := aRec.ExtraGapAirSpeed;
    fExtraGapAirDelay := aRec.ExtraGapAirDelay;
    fExtraGapAirPos := aRec.ExtraGapAirPos;
    fExtraGapWasteVol := aRec.ExtraGapWasteVol;
    fExtraGapWasteCalc := aRec.ExtraGapWasteCalc;
    fExtraGapWasteSpeed := aRec.ExtraGapWasteSpeed;
    fExtraGapWasteDelay := aRec.ExtraGapWasteDelay;

    fGetTipFlag := aRec.GetTipFlag;
    fUsedTips := aRec.UsedTips;
    fUsedTipType := aRec.UsedTipType;
    fUsedPipDevice := aRec.UsedPipDevice;
    fUseDispTipWithoutTip := aRec.UseDispTipWithoutTip;

    fAspSplitMinVolPercent := aRec.AspSplitMinVolPercent;
    fDispSpeZMoveFrequency := aRec.DispSpeZMoveFrequency;

    // Powder Handling
    fDispEmptyVarRedi := aRec.DispEmptyVarRedi;
    fSampleAspXYShifting := aRec.SampleAspXYShifting;
    fDispXYShifting := aRec.DispXYShifting;

    fCh1PumpNumber := aRec.Ch1PumpNumber;
    fMoveToSysAirPosSpeed := aRec.MoveToSysAirPosSpeed;
end;

{ TLiqHandlingParamComparer }

class function TLiqHandlingParamComparer.AreValuesCompatible(const aFieldName: string; const aV1, aV2: string;
    out oIncompatibleFieldName: string): boolean;
begin
    oIncompatibleFieldName := '';
    result := SameText(aV1, aV2);
    if not result then
        oIncompatibleFieldName := aFieldName;
end;

class function TLiqHandlingParamComparer.AreValuesCompatible(const aFieldName: string;
    const aV1, aV2: integer; out oIncompatibleFieldName: string): boolean;
begin
    oIncompatibleFieldName := '';
    result := aV1 = aV2;
    if not result then
        oIncompatibleFieldName := aFieldName;
end;

class function TLiqHandlingParamComparer.AreValuesCompatible(const aFieldName: string;
    const aV1, aV2: extended; out oIncompatibleFieldName: string): boolean;
begin
    oIncompatibleFieldName := '';
    result := Abs(aV1 - aV2) < 0.00001;
    if not result then
        oIncompatibleFieldName := aFieldName;
end;

class function TLiqHandlingParamComparer.AreValuesCompatible(const aFieldName: string;
    const aV1, aV2: boolean; out oIncompatibleFieldName: string): boolean;
begin
    oIncompatibleFieldName := '';
    result := aV1 = aV2;
    if not result then
        oIncompatibleFieldName := aFieldName;
end;

class function TLiqHandlingParamComparer.AreMixValuesCompatible(const aFieldName: string;
    const aV1, aV2: TLiqHandlingMixData; out oIncompatibleFieldName: string): boolean;
begin
    result := AreValuesCompatible('Cycles', aV1.Cycles, aV2.Cycles, oIncompatibleFieldName) and
        AreValuesCompatible('Method', aV1.Method, aV2.Method, oIncompatibleFieldName) and
        AreValuesCompatible('Vol', aV1.Vol, aV2.Vol, oIncompatibleFieldName) and
        AreValuesCompatible('MinRestVol', aV1.MinRestVol, aV2.MinRestVol, oIncompatibleFieldName) and
        AreValuesCompatible('UseCalculatedVol', aV1.UseCalculatedVol, aV2.UseCalculatedVol,
        oIncompatibleFieldName) and AreValuesCompatible('AspSpeed', aV1.AspSpeed, aV2.AspSpeed,
        oIncompatibleFieldName) and AreValuesCompatible('DispSpeed', aV1.DispSpeed, aV2.DispSpeed,
        oIncompatibleFieldName) and AreValuesCompatible('ZOffset_mm', aV1.ZOffset_mm, aV2.ZOffset_mm,
        oIncompatibleFieldName);

    if not result then
        oIncompatibleFieldName := aFieldName + '.' + oIncompatibleFieldName;
end;

class function TLiqHandlingParamComparer.AreWashValuesCompatible(const aFieldName: string;
    const aV1, aV2: TLiqHandlingWashData; out oIncompatibleFieldName: string): boolean;
begin
    result := AreValuesCompatible('IsForced', aV1.IsForced, aV2.IsForced, oIncompatibleFieldName) and
        AreValuesCompatible('UseMethod', aV1.UseMethod, aV2.UseMethod, oIncompatibleFieldName) and
        AreValuesCompatible('WashMethodName', aV1.WashMethodName, aV2.WashMethodName, oIncompatibleFieldName)
    // and AreValuesCompatible( 'VolMin',             aV1.VolMin        , aV2.VolMin           , oIncompatibleFieldName )
    // and AreValuesCompatible( 'VolMax',             aV1.VolMax        , aV2.VolMax           , oIncompatibleFieldName )
        and AreValuesCompatible('Flag', aV1.Flag, aV2.Flag, oIncompatibleFieldName)
    // and AreValuesCompatible( 'VolFactor',          aV1.VolFactor     , aV2.VolFactor        , oIncompatibleFieldName )
    // and AreValuesCompatible( 'VolChannel2',        aV1.VolChannel2   , aV2.VolChannel2      , oIncompatibleFieldName )
        and AreValuesCompatible('DryAfterWash', aV1.DryAfterWash, aV2.DryAfterWash, oIncompatibleFieldName)
        and AreValuesCompatible('UsePeripump', aV1.UsePeripump, aV2.UsePeripump, oIncompatibleFieldName);

    if not result then
        oIncompatibleFieldName := aFieldName + '.' + oIncompatibleFieldName;
end;

class function TLiqHandlingParamComparer.AreCompatible(const aV1, aV2: TLiqHandlingData;
    out oIncompatibleFieldName: string): boolean;
begin
    result := AreValuesCompatible('LiqClass', aV1.LiqClass, aV2.LiqClass, oIncompatibleFieldName)
    // and AreValuesCompatible( 'VolCorrCurve',             aV1.VolCorrCurve                , aV2.VolCorrCurve                 , oIncompatibleFieldName )

    // OK        and AreValuesCompatible( 'SampleAspCalc',            aV1.SampleAspCalc               ,  aV2.SampleAspCalc               , oIncompatibleFieldName )
    // OK        and AreValuesCompatible( 'SampleAspSpeed',           aV1.SampleAspSpeed              ,  aV2.SampleAspSpeed              , oIncompatibleFieldName )
    // OK        and AreValuesCompatible( 'SampleAspDelay',           aV1.SampleAspDelay              ,  aV2.SampleAspDelay              , oIncompatibleFieldName )
        and AreValuesCompatible('SampleAspLiqDet', aV1.SampleAspLiqDet, aV2.SampleAspLiqDet,
        oIncompatibleFieldName) and AreValuesCompatible('SampleAspSubmerge', aV1.SampleAspSubmerge,
        aV2.SampleAspSubmerge, oIncompatibleFieldName) and AreValuesCompatible('SampleAspRetractPos_mm',
        aV1.SampleAspRetractPos_mm, aV2.SampleAspRetractPos_mm, oIncompatibleFieldName) and
        AreValuesCompatible('SampleAspErrFlag', aV1.SampleAspErrFlag, aV2.SampleAspErrFlag,
        oIncompatibleFieldName) and AreValuesCompatible('AspSwitchPos', aV1.AspSwitchPos, aV2.AspSwitchPos,
        oIncompatibleFieldName) and AreValuesCompatible('AspSwitchModule', aV1.AspSwitchModule,
        aV2.AspSwitchModule, oIncompatibleFieldName) and AreValuesCompatible('RecordDetectionVolume',
        aV1.RecordDetectionVolume, aV2.RecordDetectionVolume, oIncompatibleFieldName) and
        AreValuesCompatible('AspirationMethodRetractZ', aV1.AspirationMethodRetractZ,
        aV2.AspirationMethodRetractZ, oIncompatibleFieldName) and AreValuesCompatible('SampleAspScanMode',
        aV1.SampleAspScanMode, aV2.SampleAspScanMode, oIncompatibleFieldName) and
        AreValuesCompatible('SampleAspScanSpeed', aV1.SampleAspScanSpeed, aV2.SampleAspScanSpeed,
        oIncompatibleFieldName) and AreValuesCompatible('SampleAspRetrSpeed', aV1.SampleAspRetrSpeed,
        aV2.SampleAspRetrSpeed, oIncompatibleFieldName) and AreValuesCompatible('SampleAspRetrDistance',
        aV1.SampleAspRetrDistance, aV2.SampleAspRetrDistance, oIncompatibleFieldName) and
        AreValuesCompatible('SampleAspInsertMoveType', aV1.SampleAspInsertMoveType,
        aV2.SampleAspInsertMoveType, oIncompatibleFieldName)

        and AreMixValuesCompatible('SampleAspMix', aV1.SampleAspMix, aV2.SampleAspMix, oIncompatibleFieldName)
        and AreValuesCompatible('SampleAspMixFirstOnly', aV1.SampleAspMixFirstOnly, aV2.SampleAspMixFirstOnly,
        oIncompatibleFieldName) and AreValuesCompatible('SampleAspMultipip', aV1.SampleAspMultipip,
        aV2.SampleAspMultipip, oIncompatibleFieldName) and AreValuesCompatible('SampleAspMultiMaxVol',
        aV1.SampleAspMultiMaxVol, aV2.SampleAspMultiMaxVol, oIncompatibleFieldName) and
        AreValuesCompatible('SampleAspMultiMinVol', aV1.SampleAspMultiMinVol, aV2.SampleAspMultiMinVol,
        oIncompatibleFieldName) and AreValuesCompatible('SampleAspMultiSplitVol', aV1.SampleAspMultiSplitVol,
        aV2.SampleAspMultiSplitVol, oIncompatibleFieldName) and AreValuesCompatible('SampleAspMultiWash',
        aV1.SampleAspMultiWash, aV2.SampleAspMultiWash, oIncompatibleFieldName) and
        AreValuesCompatible('SampleAspTipSingleRetract', aV1.SampleAspTipSingleRetract,
        aV2.SampleAspTipSingleRetract, oIncompatibleFieldName)

    // OK        and AreValuesCompatible( 'SampleAspWasteVol',        aV1.SampleAspWasteVol           ,  aV2.SampleAspWasteVol           , oIncompatibleFieldName )
    // OK        and AreValuesCompatible( 'SampleAspWastePerCent',    aV1.SampleAspWastePerCent       ,  aV2.SampleAspWastePerCent       , oIncompatibleFieldName )

    // OK        and AreValuesCompatible( 'SampleAspSpitBack',        aV1.SampleAspSpitBack           ,  aV2.SampleAspSpitBack           , oIncompatibleFieldName )
        and AreValuesCompatible('SampleAspSpitBackCount', aV1.SampleAspSpitBackCount,
        aV2.SampleAspSpitBackCount, oIncompatibleFieldName)
    // OK        and AreValuesCompatible( 'SampleAspSpitBackCalc',    aV1.SampleAspSpitBackCalc       ,  aV2.SampleAspSpitBackCalc       , oIncompatibleFieldName )
    // OK        and AreValuesCompatible( 'SampleAspSpitBackSpeed',   aV1.SampleAspSpitBackSpeed      ,  aV2.SampleAspSpitBackSpeed      , oIncompatibleFieldName )
        and AreValuesCompatible('SampleAspSpitBackAtAspPos', aV1.SampleAspSpitBackAtAspPos,
        aV2.SampleAspSpitBackAtAspPos, oIncompatibleFieldName)

    // OK        and AreValuesCompatible( 'SampleAspCh2WashVol',      aV1.SampleAspCh2WashVol         ,  aV2.SampleAspCh2WashVol         , oIncompatibleFieldName )
    // OK        and AreValuesCompatible( 'SampleAspCh2WashCalc',     aV1.SampleAspCh2WashCalc        ,  aV2.SampleAspCh2WashCalc        , oIncompatibleFieldName )
    // OK        and AreValuesCompatible( 'SampleAspCh2WashSpeed',    aV1.SampleAspCh2WashSpeed       ,  aV2.SampleAspCh2WashSpeed       , oIncompatibleFieldName )
    // OK        and AreValuesCompatible( 'SampleAspCh2WashDelay',    aV1.SampleAspCh2WashDelay       ,  aV2.SampleAspCh2WashDelay       , oIncompatibleFieldName )

        and AreValuesCompatible('SampleAspTipTouch', aV1.SampleAspTipTouch, aV2.SampleAspTipTouch,
        oIncompatibleFieldName) and ((not aV1.SampleAspTipTouch)
        // wenn TipTouch nicht aktiv ist, die Details nicht prüfen:
        or (AreValuesCompatible('SampleAspTipTouchDelay', aV1.SampleAspTipTouchDelay,
        aV2.SampleAspTipTouchDelay, oIncompatibleFieldName) and AreValuesCompatible('SampleAspTipTouchScan',
        aV1.SampleAspTipTouchScan, aV2.SampleAspTipTouchScan, oIncompatibleFieldName) and
        AreValuesCompatible('SampleAspTipTouchScanMode', aV1.SampleAspTipTouchScanMode,
        aV2.SampleAspTipTouchScanMode, oIncompatibleFieldName) and
        AreValuesCompatible('SampleAspTipTouchSingle', aV1.SampleAspTipTouchSingle,
        aV2.SampleAspTipTouchSingle, oIncompatibleFieldName)))

    // OK        and AreValuesCompatible( 'DilAspCalc',               aV1.DilAspCalc                  ,  aV2.DilAspCalc                  , oIncompatibleFieldName )
    // OK        and AreValuesCompatible( 'DilAspSpeed',              aV1.DilAspSpeed                 ,  aV2.DilAspSpeed                 , oIncompatibleFieldName )
    // OK        and AreValuesCompatible( 'DilAspDelay',              aV1.DilAspDelay                 ,  aV2.DilAspDelay                 , oIncompatibleFieldName )
        and AreValuesCompatible('DilAspSubmerge', aV1.DilAspSubmerge, aV2.DilAspSubmerge,
        oIncompatibleFieldName) and AreValuesCompatible('DilAspWasteVol', aV1.DilAspWasteVol,
        aV2.DilAspWasteVol, oIncompatibleFieldName) and AreValuesCompatible('DilAspLiqDet', aV1.DilAspLiqDet,
        aV2.DilAspLiqDet, oIncompatibleFieldName) and AreValuesCompatible('DilAspErrFlag', aV1.DilAspErrFlag,
        aV2.DilAspErrFlag, oIncompatibleFieldName) and AreValuesCompatible('DilAspTipSingleRetract',
        aV1.DilAspTipSingleRetract, aV2.DilAspTipSingleRetract, oIncompatibleFieldName) and
        AreValuesCompatible('DilAspSwitchPos', aV1.DilAspSwitchPos, aV2.DilAspSwitchPos,
        oIncompatibleFieldName) and AreValuesCompatible('DilAspSwitchModule', aV1.DilAspSwitchModule,
        aV2.DilAspSwitchModule, oIncompatibleFieldName)
    // OK        and AreValuesCompatible( 'DilAspChannel2',           aV1.DilAspChannel2              ,  aV2.DilAspChannel2              , oIncompatibleFieldName )
        and AreValuesCompatible('DilAspScanMode', aV1.DilAspScanMode, aV2.DilAspScanMode,
        oIncompatibleFieldName) and AreValuesCompatible('DilAspScanSpeed', aV1.DilAspScanSpeed,
        aV2.DilAspScanSpeed, oIncompatibleFieldName) and AreValuesCompatible('DilAspRetrSpeed',
        aV1.DilAspRetrSpeed, aV2.DilAspRetrSpeed, oIncompatibleFieldName) and
        AreValuesCompatible('DilAspRetrDistance', aV1.DilAspRetrDistance, aV2.DilAspRetrDistance,
        oIncompatibleFieldName) and AreValuesCompatible('DilAspInsertMoveType', aV1.DilAspInsertMoveType,
        aV2.DilAspInsertMoveType, oIncompatibleFieldName)

    // OK        and AreValuesCompatible( 'DispCalc',                 aV1.DispCalc                    ,  aV2.DispCalc                    , oIncompatibleFieldName )
    // OK        and AreValuesCompatible( 'DispSpeed',                aV1.DispSpeed                   ,  aV2.DispSpeed                   , oIncompatibleFieldName )
    // OK        and AreValuesCompatible( 'DispDelay',                aV1.DispDelay                   ,  aV2.DispDelay                   , oIncompatibleFieldName )
        and AreValuesCompatible('DispLiqDet', aV1.DispLiqDet, aV2.DispLiqDet, oIncompatibleFieldName) and
        AreValuesCompatible('DispErrFlag', aV1.DispErrFlag, aV2.DispErrFlag, oIncompatibleFieldName) and
        AreValuesCompatible('DispTipSingleRetract', aV1.DispTipSingleRetract, aV2.DispTipSingleRetract,
        oIncompatibleFieldName) and AreValuesCompatible('DispStepVolume', aV1.DispStepVolume,
        aV2.DispStepVolume, oIncompatibleFieldName) and AreValuesCompatible('DispStepDelay',
        aV1.DispStepDelay, aV2.DispStepDelay, oIncompatibleFieldName) and
        AreValuesCompatible('DispSubmerge', aV1.DispSubmerge, aV2.DispSubmerge, oIncompatibleFieldName) and
        AreValuesCompatible('DspSwitchPos', aV1.DspSwitchPos, aV2.DspSwitchPos, oIncompatibleFieldName) and
        AreValuesCompatible('DspSwitchModule', aV1.DspSwitchModule, aV2.DspSwitchModule,
        oIncompatibleFieldName) and AreValuesCompatible('DispScanMode', aV1.DispScanMode, aV2.DispScanMode,
        oIncompatibleFieldName) and AreValuesCompatible('DispScanSpeed', aV1.DispScanSpeed, aV2.DispScanSpeed,
        oIncompatibleFieldName) and AreValuesCompatible('DispRetrSpeed', aV1.DispRetrSpeed, aV2.DispRetrSpeed,
        oIncompatibleFieldName) and AreValuesCompatible('DispRetrDistance', aV1.DispRetrDistance,
        aV2.DispRetrDistance, oIncompatibleFieldName) and AreValuesCompatible('DispInsertMoveType',
        aV1.DispInsertMoveType, aV2.DispInsertMoveType, oIncompatibleFieldName)

        and AreMixValuesCompatible('DispMix', aV1.DispMix, aV2.DispMix, oIncompatibleFieldName) and
        AreValuesCompatible('DispMixAspSubmerge', aV1.DispMixAspSubmerge, aV2.DispMixAspSubmerge,
        oIncompatibleFieldName)

        and AreValuesCompatible('DispTipTouch', aV1.DispTipTouch, aV2.DispTipTouch, oIncompatibleFieldName)
        and ((not aV1.DispTipTouch) // wenn TipTouch nicht aktiv ist, die Details nicht prüfen:
        or (AreValuesCompatible('DispTipTouchDelay', aV1.DispTipTouchDelay, aV2.DispTipTouchDelay,
        oIncompatibleFieldName) and AreValuesCompatible('DispTipTouchScan', aV1.DispTipTouchScan,
        aV2.DispTipTouchScan, oIncompatibleFieldName) and AreValuesCompatible('DispTipTouchScanMode',
        aV1.DispTipTouchScanMode, aV2.DispTipTouchScanMode, oIncompatibleFieldName) and
        AreValuesCompatible('DispTipTouchSingle', aV1.DispTipTouchSingle, aV2.DispTipTouchSingle,
        oIncompatibleFieldName) and AreValuesCompatible('DispTipTouchScanStoreVol',
        aV1.DispTipTouchScanStoreVol, aV2.DispTipTouchScanStoreVol, oIncompatibleFieldName)))

        and AreWashValuesCompatible('Wash', aV1.Wash, aV2.Wash, oIncompatibleFieldName)

        and AreValuesCompatible('SysAirAspPos', aV1.SysAirAspPos, aV2.SysAirAspPos, oIncompatibleFieldName)
    // OK        and AreValuesCompatible( 'SysAirAspDelay',           aV1.SysAirAspDelay              ,  aV2.SysAirAspDelay              , oIncompatibleFieldName )
    // OK        and AreValuesCompatible( 'SysAirAspCalc',            aV1.SysAirAspCalc               ,  aV2.SysAirAspCalc               , oIncompatibleFieldName )
    // OK        and AreValuesCompatible( 'SysAirAspVol',             aV1.SysAirAspVol                ,  aV2.SysAirAspVol                , oIncompatibleFieldName )
    // OK        and AreValuesCompatible( 'SysAirAspSpeed',           aV1.SysAirAspSpeed              ,  aV2.SysAirAspSpeed              , oIncompatibleFieldName )
        and AreValuesCompatible('SysAirDispVol', aV1.SysAirDispVol, aV2.SysAirDispVol, oIncompatibleFieldName)
        and AreValuesCompatible('MoveToSysAirPosSpeed', aV1.MoveToSysAirPosSpeed, aV2.MoveToSysAirPosSpeed,
        oIncompatibleFieldName)

        and AreValuesCompatible('TransAirPos', aV1.TransAirPos, aV2.TransAirPos, oIncompatibleFieldName)
    // OK        and AreValuesCompatible( 'TransAirAspDelay',         aV1.TransAirAspDelay            ,  aV2.TransAirAspDelay            , oIncompatibleFieldName )
    // OK        and AreValuesCompatible( 'TransAirAspCalc',          aV1.TransAirAspCalc             ,  aV2.TransAirAspCalc             , oIncompatibleFieldName )
    // OK        and AreValuesCompatible( 'TransAirVol',              aV1.TransAirVol                 ,  aV2.TransAirVol                 , oIncompatibleFieldName )
    // OK        and AreValuesCompatible( 'TransAirSpeed',            aV1.TransAirSpeed               ,  aV2.TransAirSpeed               , oIncompatibleFieldName )

        and AreValuesCompatible('ExtraGapCount', aV1.ExtraGapCount, aV2.ExtraGapCount, oIncompatibleFieldName)
        and AreValuesCompatible('ExtraGapAirVol', aV1.ExtraGapAirVol, aV2.ExtraGapAirVol,
        oIncompatibleFieldName) and AreValuesCompatible('ExtraGapAirCalc', aV1.ExtraGapAirCalc,
        aV2.ExtraGapAirCalc, oIncompatibleFieldName) and AreValuesCompatible('ExtraGapAirSpeed',
        aV1.ExtraGapAirSpeed, aV2.ExtraGapAirSpeed, oIncompatibleFieldName) and
        AreValuesCompatible('ExtraGapAirDelay', aV1.ExtraGapAirDelay, aV2.ExtraGapAirDelay,
        oIncompatibleFieldName) and AreValuesCompatible('ExtraGapAirPos', aV1.ExtraGapAirPos,
        aV2.ExtraGapAirPos, oIncompatibleFieldName) and AreValuesCompatible('ExtraGapWasteVol',
        aV1.ExtraGapWasteVol, aV2.ExtraGapWasteVol, oIncompatibleFieldName) and
        AreValuesCompatible('ExtraGapWasteCalc', aV1.ExtraGapWasteCalc, aV2.ExtraGapWasteCalc,
        oIncompatibleFieldName) and AreValuesCompatible('ExtraGapWasteSpeed', aV1.ExtraGapWasteSpeed,
        aV2.ExtraGapWasteSpeed, oIncompatibleFieldName) and AreValuesCompatible('ExtraGapWasteDelay',
        aV1.ExtraGapWasteDelay, aV2.ExtraGapWasteDelay, oIncompatibleFieldName)

        and AreValuesCompatible('GetTipFlag', aV1.GetTipFlag, aV2.GetTipFlag, oIncompatibleFieldName) and
    // AreValuesCompatible('UsedTips', aV1.UsedTips, aV2.UsedTips, oIncompatibleFieldName) and
    // AreValuesCompatible('UsedTipType', aV1.UsedTipType, aV2.UsedTipType, oIncompatibleFieldName) and
        AreValuesCompatible('UsedPipDevice', aV1.UsedPipDevice, aV2.UsedPipDevice, oIncompatibleFieldName) and
        AreValuesCompatible('UseDispTipWithoutTip', aV1.UseDispTipWithoutTip, aV2.UseDispTipWithoutTip,
        oIncompatibleFieldName)

        and AreValuesCompatible('AspSplitMinVolPercent', aV1.AspSplitMinVolPercent, aV2.AspSplitMinVolPercent,
        oIncompatibleFieldName) and AreValuesCompatible('DispSpeZMoveFrequency', aV1.DispSpeZMoveFrequency,
        aV2.DispSpeZMoveFrequency, oIncompatibleFieldName)

    // Powder Handling                 // Powder Handling          // Powder Handling
        and AreValuesCompatible('DispEmptyVarRedi', aV1.DispEmptyVarRedi, aV2.DispEmptyVarRedi,
        oIncompatibleFieldName) and AreValuesCompatible('SampleAspXYShifting', aV1.SampleAspXYShifting,
        aV2.SampleAspXYShifting, oIncompatibleFieldName) and AreValuesCompatible('DispXYShifting',
        aV1.DispXYShifting, aV2.DispXYShifting, oIncompatibleFieldName)
    // OK        and AreValuesCompatible( 'Ch1PumpNumber', aV1.Ch1PumpNumber    ,  aV2.Ch1PumpNumber         , oIncompatibleFieldName )
        ;
end;


end.
