{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                 track-no improvement/change
  -------- --  -------------------------------------  -------- -------------------------------------
  10.07.08 wl  TLiqHTableStructDefV2                  TN4157    new field UsedPipDevice
  10.07.08 wl  TLiqHTableUpdateV2.ReplaceTotalTipMap  TN4157    reads Pip Devices and change Tips
  25.09.08 wl  TLiqHTableStructDefV3                  TN4242    WashMacroName hat jetzt 50 chars
  31.07.09 wl  TLiqHTableStructDefV1                  TN3950   3 neue Felder: SampleAspInsertMoveType, DilAspInsertMoveType, DispInsertMoveType
  31.07.09 wl  TLiqHTableStructDefV1                  TN4024   neu: DispSpeZMoveFrequency (Typ: double)
  31.07.09 wl  TLiqHTableStructDefV1                  TN4693   WashFlag und ErrFlag (3 mal) sind jetzt integer
  31.07.09 wl  TLiqHTableStructDefV1                  TN4693   Konsistente Datentypen: Alle Volumen,Volspeeds,Delays,Faktoren sind Float; smallint -> integer
  29.06.10 pk  TLiqHTableStructDefV4                  TN5173   new Ch1PipNumber field
  22.07.10 wl  TLiqHTableUpdateV4_1                   TN5184   Update für "Mix with air"
  26.08.10 ts  TLiqHTableStructDefV5                  TN5248   new: MoveToSysAirPosSpeed
  22.09.10 wl  TLiqHTableStructDefV5_1                TN5275   new: ExtraGapAirPos
  22.09.10 wl  TLiqHTableStructDefV4,TLiqHTableStructDefV5   TN5275   entfernt, damit alle (Branch-)Versionen ohne Datenverlust updaten können
  15.02.11 pk                               		    TN4780   changes needed to make UpdateManager compatible with TurboDB
  19.07.11 wl  TLiqHTableStructDefV5_2                TN5630   new: TransAirRetakeBetweenDisp, TransAirRetakeAfterLastDisp
  02.12.11 wl  TLiqHTableStructDefV6                  TN5758   Neue Felder: 'DispMixUseCalculatedVol', 'SampleAspMixUseCalculatedVol'
  23.02.12 wl  TLiqHTableStructDefV7                  TN5818   Neue Felder: SampleAspTipTouchScan,-Submerge,DispTipTouchScan,-Submerge = 'SampleAspTipTouchScan';
  10.08.12 wl  TLiqHTableStructDefV8                  TN5947   Neues Feld: WashUsePeripump
  04.09.12 wl  TLiqHTableStructDefV9                  TN5972   Neues Feld: SampleAspSpitBackAtAspPos
  02.01.13 wl  TLiqHTableStructDefV10                 TN6064   Neues Feld: DispMixAspSubmerge
  07.05.13 ts  TLiqHTableStructDefV11                 TN6118   Neues Feld: DispTipTouchScanStoreVol
  -------------------------------------------------------------------------------------------------- }

unit LiqHTableUpdate;


interface


uses
    Update,
    TableStructDef,
    TableUpdate,
    SettingsTableUpdate;

type
    // table structure definitions
    TLiqHTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TLiqHTableStructDefV1 = class(TLiqHTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TLiqHTableStructDefV2 = class(TLiqHTableStructDefV1)
    protected
        procedure DoDefineStruct(); override;
    end;

    TLiqHTableStructDefV3 = class(TLiqHTableStructDefV2)
    protected
        procedure DoDefineStruct(); override;
    end;

    TLiqHTableStructDefV5_1 = class(TLiqHTableStructDefV3) // Version 4.0 und 5.0 wurden entfernt
    protected
        procedure DoDefineStruct(); override;
    end;

    TLiqHTableStructDefV5_2 = class(TLiqHTableStructDefV5_1)
    protected
        procedure DoDefineStruct(); override;
    end;

    TLiqHTableStructDefV6 = class(TLiqHTableStructDefV5_2)
    protected
        procedure DoDefineStruct(); override;
    public const
        cFieldNameDISPMIXUseCalculatedVol = 'DispMixUseCalculatedVol';
        cFieldNameSAMPLEASPMIXUseCalculatedVol = 'SampleAspMixUseCalculatedVol';
    end;

    TLiqHTableStructDefV7 = class(TLiqHTableStructDefV6)
    protected
        procedure DoDefineStruct(); override;
    public const
        cFieldNameSampleAspTipTouchScan = 'SampleAspTipTouchScan';
        cFieldNameSampleAspTipTouchSubmerge = 'SampleAspTipTouchSubmerge';
        cFieldNameDispTipTouchScan = 'DispTipTouchScan';
        cFieldNameDispTipTouchSubmerge = 'DispTipTouchSubmerge';
    end;

    TLiqHTableStructDefV8 = class(TLiqHTableStructDefV7)
    protected
        procedure DoDefineStruct(); override;
    end;

    TLiqHTableStructDefV9 = class(TLiqHTableStructDefV8)
    protected
        procedure DoDefineStruct(); override;
    end;

    TLiqHTableStructDefV10 = class(TLiqHTableStructDefV9)
    protected
        procedure DoDefineStruct(); override;
    end;

    TLiqHTableStructDefV11 = class(TLiqHTableStructDefV10)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TLiqHTableUpdateV1 = class(TTableUpdate)
    private
        procedure SetLiqParamDefaultsIfNull();
        procedure UpdatePortFieldVersion1(aSwitchPosFieldName, aSwitchPortNumberFieldName,
            aSwithModuleFieldName: string);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TLiqHTableUpdateV2 = class(TTableUpdate)
    private
        procedure LiqHUpdateV2CustomFunc(aSender: TObject);
        procedure ReplaceTotalTipMap(aPipDevLoader: TSettingsTablePipDeviceLoader);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TLiqHTableUpdateV3 = class(TTableUpdate)
    private
        procedure LiqHUpdateV3CustomFunc(aSender: TObject);
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TLiqHTableUpdateV3_1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TLiqHTableUpdateV4 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TLiqHTableUpdateV4_1 = class(TTableUpdate)
    private
        procedure LiqHUpdateV4_1CustomFunc(aSender: TObject);
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TLiqHTableUpdateV5 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TLiqHTableUpdateV5_1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TLiqHTableUpdateV5_2 = class(TTableUpdate)
    strict private
        procedure LiqHUpdateV5_2CustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TLiqHTableUpdateV6 = class(TTableUpdate)
    strict private
        procedure CustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TLiqHTableUpdateV7 = class(TTableUpdate)
    strict private
        procedure CustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TLiqHTableUpdateV8 = class(TTableUpdate)
    strict private
        procedure CustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TLiqHTableUpdateV9 = class(TTableUpdate)
    strict private
        procedure CustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TLiqHTableUpdateV10 = class(TTableUpdate)
    strict private
        procedure CustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TLiqHTableUpdateV11 = class(TTableUpdate)
    strict private
        procedure CustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils,
    DataProvider,
    MethodTableUpdate;

const
    INT_REVISION_1 = 1;
    INT_REVISION_2 = 2;
    INT_REVISION_3 = 3;
    INT_REVISION_4 = 4;
    INT_REVISION_5 = 5;
    INT_REVISION_6 = 6;
    INT_REVISION_7 = 7;
    INT_REVISION_8 = 8;
    INT_REVISION_9 = 9;
    INT_REVISION_10 = 10;
    INT_REVISION_11 = 11;
    INT_MINORREVISION_1 = 1;
    INT_MINORREVISION_2 = 2;

const
    STR_LIQPARAM_TBL = 'LIQPARAM';

    STR_LIQPARAM_FLD_PARAMNAME = 'PARAMNAME';
    STR_LIQPARAM_FLD_DESCRIPTION = 'Description';
    STR_LIQPARAM_FLD_LIQCLASS = 'LIQCLASS';
    STR_LIQPARAM_FLD_VOLCORRCURVE = 'VolCorrCurve';

    STR_LIQPARAM_FLD_SAMPLEASPCALC = 'SampleAspCalc';
    STR_LIQPARAM_FLD_SAMPLEASPSPEED = 'SampleAspSpeed';
    STR_LIQPARAM_FLD_SAMPLEASPDELAY = 'SampleAspDelay';
    STR_LIQPARAM_FLD_SAMPLEASPLIQDET = 'SampleAspLiqDet';
    STR_LIQPARAM_FLD_SAMPLEASPSUBMERGE = 'SampleAspSubmerge';
    STR_LIQPARAM_FLD_SAMPLEASPRETRACTPOS = 'SampleAspRetractPos';
    STR_LIQPARAM_FLD_SAMPLEASPWASTEVOL = 'SampleAspWasteVol';
    STR_LIQPARAM_FLD_SAMPLEASPERRFLAG = 'SampleAspErrFlag';
    STR_LIQPARAM_FLD_ASPSWITCHPOS = 'AspSwitchPos';
    STR_LIQPARAM_FLD_ASPSWITCHMODULE = 'AspSwitchModule';
    STR_LIQPARAM_FLD_RECORDDETECTIONVOLUME = 'RecordDetectionVolume';
    STR_LIQPARAM_FLD_ASPIRATIONMETHODRETRACTZ = 'AspirationMethodRetractZ';
    STR_LIQPARAM_FLD_SAMPLEASPSCANMODE = 'SampleAspScanMode';
    STR_LIQPARAM_FLD_SAMPLEASPSCANSPEED = 'SampleAspScanSpeed';
    STR_LIQPARAM_FLD_SAMPLEASPRETRSPEED = 'SampleAspRetrSpeed';
    STR_LIQPARAM_FLD_SAMPLEASPRETRDISTANCE = 'SampleAspRetrDistance';
    STR_LIQPARAM_FLD_SAMPLEASPINSERTMOVETYPE = 'SampleAspInsertMoveType';

    STR_LIQPARAM_FLD_SAMPLEASPMIXCYCL = 'SampleAspMixCycl';
    STR_LIQPARAM_FLD_SAMPLEASPMIXMETHOD = 'SampleAspMixMethod';
    STR_LIQPARAM_FLD_SAMPLEASPMIXVOL = 'SampleAspMixVol';
    STR_LIQPARAM_FLD_SAMPLEASPMIXMINRESTVOL = 'SampleAspMixMinRestVol';
    STR_LIQPARAM_FLD_SAMPLEASPMIXSPEED = 'SampleAspMixSpeed';
    STR_LIQPARAM_FLD_SAMPLEASPMIXDISPSPEED = 'SampleAspMixDispSpeed';
    STR_LIQPARAM_FLD_SAMPLEASPMIXZOFFSET = 'SampleAspMixZOffset';
    STR_LIQPARAM_FLD_SAMPLEASPMIXFIRSTONLY = 'SampleAspMixFirstOnly';

    STR_LIQPARAM_FLD_SAMPLEASPMULTIPIP = 'SampleAspMultipip';
    STR_LIQPARAM_FLD_SAMPLEASPMULTIMAXVOL = 'SampleAspMultiMaxVol';
    STR_LIQPARAM_FLD_SAMPLEASPMULTIMINVOL = 'SampleAspMultiMinVol';
    STR_LIQPARAM_FLD_SAMPLEASPMULTISPLITVOL = 'SampleAspMultiSplitVol';
    STR_LIQPARAM_FLD_SAMPLEASPWASTEPERCENT = 'SampleAspWastePerCent';
    STR_LIQPARAM_FLD_SAMPLEASPMULTIWASH = 'SampleAspMultiWash';
    STR_LIQPARAM_FLD_SAMPLEASPTIPSINGLERETRACT = 'SampleAspTipSingleRetract';

    STR_LIQPARAM_FLD_SAMPLEASPSPITBACK = 'SampleAspSpitBack';
    STR_LIQPARAM_FLD_SAMPLEASPSPITBACKCOUNT = 'SampleAspSpitBackCount';
    STR_LIQPARAM_FLD_SAMPLEASPSPITBACKCALC = 'SampleAspSpitBackCalc';
    STR_LIQPARAM_FLD_SAMPLEASPSPITBACKSPEED = 'SampleAspSpitBackSpeed';
    STR_LIQPARAM_FLD_SampleAspSpitBackAtAspPos = 'SampleAspSpitBackAtAspPos';

    STR_LIQPARAM_FLD_SAMPLEASPCH2WASHVOL = 'SampleAspCh2WashVol';
    STR_LIQPARAM_FLD_SAMPLEASPCH2WASHCALC = 'SampleAspCh2WashCalc';
    STR_LIQPARAM_FLD_SAMPLEASPCH2WASHSPEED = 'SampleAspCh2WashSpeed';
    STR_LIQPARAM_FLD_SAMPLEASPCH2WASHDELAY = 'SampleAspCh2WashDelay';

    STR_LIQPARAM_FLD_SAMPLEASPTIPTOUCH = 'SampleAspTipTouch';
    STR_LIQPARAM_FLD_SAMPLEASPTIPTOUCHDELAY = 'SampleAspTipTouchDelay';
    STR_LIQPARAM_FLD_SAMPLEASPTIPTOUCHSCANMODE = 'SampleAspTipTouchScanMode';
    STR_LIQPARAM_FLD_SAMPLEASPTIPTOUCHSINGLE = 'SampleAspTipTouchSingle';

    STR_LIQPARAM_FLD_DILASPCALC = 'DilAspCalc';
    STR_LIQPARAM_FLD_DILASPSPEED = 'DilAspSpeed';
    STR_LIQPARAM_FLD_DILASPDELAY = 'DilAspDelay';
    STR_LIQPARAM_FLD_DILASPSUBMERGE = 'DilAspSubmerge';
    STR_LIQPARAM_FLD_DILASPWASTEVOL = 'DilAspWasteVol';
    STR_LIQPARAM_FLD_DILASPSPITBACK = 'DilAspSpitBack';
    STR_LIQPARAM_FLD_DILASPLIQDET = 'DilAspLiqDet';
    STR_LIQPARAM_FLD_DILASPERRFLAG = 'DilAspErrFlag';
    STR_LIQPARAM_FLD_DILASPTIPSINGLERETRACT = 'DilAspTipSingleRetract';
    STR_LIQPARAM_FLD_DILASPSWITCHPOS = 'DilAspSwitchPos';
    STR_LIQPARAM_FLD_DILASPSWITCHMODULE = 'DilAspSwitchModule';
    STR_LIQPARAM_FLD_DILASPCHANNEL2 = 'DilAspChannel2';
    STR_LIQPARAM_FLD_DILASPSCANMODE = 'DilAspScanMode';
    STR_LIQPARAM_FLD_DILASPSCANSPEED = 'DilAspScanSpeed';
    STR_LIQPARAM_FLD_DILASPRETRSPEED = 'DilAspRetrSpeed';
    STR_LIQPARAM_FLD_DILASPRETRDISTANCE = 'DilAspRetrDistance';
    STR_LIQPARAM_FLD_DILASPINSERTMOVETYPE = 'DilAspInsertMoveType';

    STR_LIQPARAM_FLD_DISPCALC = 'DispCalc';
    STR_LIQPARAM_FLD_DISPSPEED = 'DispSpeed';
    STR_LIQPARAM_FLD_DISPDELAY = 'DispDelay';
    STR_LIQPARAM_FLD_DISPLIQDET = 'DispLiqDet';
    STR_LIQPARAM_FLD_DISPERRFLAG = 'DispErrFlag';
    STR_LIQPARAM_FLD_DISPTIPSINGLERETRACT = 'DispTipSingleRetract';
    STR_LIQPARAM_FLD_DISPSTEPVOLUME = 'DispStepVolume';
    STR_LIQPARAM_FLD_DISPSTEPDELAY = 'DispStepDelay';
    STR_LIQPARAM_FLD_DISPSUBMERGE = 'DispSubmerge';
    STR_LIQPARAM_FLD_DSPSWITCHPOS = 'DspSwitchPos';
    STR_LIQPARAM_FLD_DSPSWITCHMODULE = 'DspSwitchModule';
    STR_LIQPARAM_FLD_DISPSCANMODE = 'DispScanMode';
    STR_LIQPARAM_FLD_DISPSCANSPEED = 'DispScanSpeed';
    STR_LIQPARAM_FLD_DISPRETRSPEED = 'DispRetrSpeed';
    STR_LIQPARAM_FLD_DISPRETRDISTANCE = 'DispRetrDistance';
    STR_LIQPARAM_FLD_DISPINSERTMOVETYPE = 'DispInsertMoveType';

    STR_LIQPARAM_FLD_DISPMIXCYCL = 'DispMixCycl';
    STR_LIQPARAM_FLD_DISPMIXMETHOD = 'DispMixMethod';
    STR_LIQPARAM_FLD_DISPMIXVOL = 'DispMixVol';
    STR_LIQPARAM_FLD_DISPMIXMINRESTVOL = 'DispMixMinRestVol';
    STR_LIQPARAM_FLD_DISPMIXSPEED = 'DispMixSpeed';
    STR_LIQPARAM_FLD_DISPMIXDISPSPEED = 'DispMixDispSpeed';
    STR_LIQPARAM_FLD_DISPMIXZOFFSET = 'DispMixZOffset';
    STR_LIQPARAM_FLD_DISPMIXAspSubmerge = 'DispMixAspSubmerge';

    STR_LIQPARAM_FLD_DISPTIPTOUCH = 'DispTipTouch';
    STR_LIQPARAM_FLD_DISPTIPTOUCHDELAY = 'DispTipTouchDelay';
    STR_LIQPARAM_FLD_DISPTIPTOUCHSCANMODE = 'DispTipTouchScanMode';
    STR_LIQPARAM_FLD_DISPTIPTOUCHSINGLE = 'DispTipTouchSingle';
    STR_LIQPARAM_FLD_DISPTIPTOUCHSCANSTOREVOL = 'DispTipTouchScanStoreVol';

    STR_LIQPARAM_FLD_WASHISFORCED = 'WashIsForced';
    STR_LIQPARAM_FLD_USEWASHMACRO = 'UseWashMacro';
    STR_LIQPARAM_FLD_WASHMACRONAME = 'WashMacroName';
    STR_LIQPARAM_FLD_WASHVOLMIN = 'WashVolMin';
    STR_LIQPARAM_FLD_WASHVOLMAX = 'WashVolMax';
    STR_LIQPARAM_FLD_WASHFLAG = 'WashFlag';
    STR_LIQPARAM_FLD_WASHVOLFACTOR = 'WashVolFactor';
    STR_LIQPARAM_FLD_WASHVOLCHANNEL2 = 'WashVolChannel2';
    STR_LIQPARAM_FLD_DRYAFTERWASH = 'DryAfterWash';
    STR_LIQPARAM_FLD_WashUsePeripump = 'WashUsePeripump';

    STR_LIQPARAM_FLD_SYSAIRASPPOS = 'SysAirAspPos';
    STR_LIQPARAM_FLD_SYSAIRASPDELAY = 'SysAirAspDelay';
    STR_LIQPARAM_FLD_SYSAIRASPCALC = 'SysAirAspCalc';
    STR_LIQPARAM_FLD_SYSAIRASPVOL = 'SysAirAspVol';
    STR_LIQPARAM_FLD_SYSAIRASPSPEED = 'SysAirAspSpeed';
    STR_LIQPARAM_FLD_SYSAIRDISPVOL = 'SysAirDispVol';
    STR_LIQPARAM_FLD_MOVETOSYSAIRPOSSPEED = 'MoveToSysAirPosSpeed';

    STR_LIQPARAM_FLD_TRANSAIRPOS = 'TransAirPos';
    STR_LIQPARAM_FLD_TRANSAIRASPDELAY = 'TransAirAspDelay';
    STR_LIQPARAM_FLD_TRANSAIRASPCALC = 'TransAirAspCalc';
    STR_LIQPARAM_FLD_TRANSAIRVOL = 'TransAirVol';
    STR_LIQPARAM_FLD_TRANSAIRSPEED = 'TransAirSpeed';

    STR_LIQPARAM_FLD_EXTRAGAPCOUNT = 'ExtraGapCount';
    STR_LIQPARAM_FLD_EXTRAGAPAIRVOL = 'ExtraGapAirVol';
    STR_LIQPARAM_FLD_EXTRAGAPAIRCALC = 'ExtraGapAirCalc';
    STR_LIQPARAM_FLD_EXTRAGAPAIRSPEED = 'ExtraGapAirSpeed';
    STR_LIQPARAM_FLD_EXTRAGAPAIRDELAY = 'ExtraGapAirDelay';
    STR_LIQPARAM_FLD_EXTRAGAPAIRPOS = 'ExtraGapAirPos';
    STR_LIQPARAM_FLD_EXTRAGAPWASTEVOL = 'ExtraGapWasteVol';
    STR_LIQPARAM_FLD_EXTRAGAPWASTECALC = 'ExtraGapWasteCalc';
    STR_LIQPARAM_FLD_EXTRAGAPWASTESPEED = 'ExtraGapWasteSpeed';
    STR_LIQPARAM_FLD_EXTRAGAPWASTEDELAY = 'ExtraGapWasteDelay';

    STR_LIQPARAM_FLD_GETTIPFLAG = 'GetTipFlag';
    STR_LIQPARAM_FLD_USEDTIPS = 'UsedTips';
    STR_LIQPARAM_FLD_USEDTIPTYPE = 'UsedTipType';
    STR_LIQPARAM_FLD_USEDPIPDEVICE = 'UsedPipDevice';
    STR_LIQPARAM_FLD_USEDISPTIPWITHOUTTIP = 'UseDispTipWithoutTip';

    STR_LIQPARAM_FLD_DISPEMPTYVARREDI = 'DispEmptyVarRedi';
    STR_LIQPARAM_FLD_SAMPLEASPXYSHIFTING = 'SampleAspXYShifting';
    STR_LIQPARAM_FLD_DISPXYSHIFTING = 'DispXYShifting';

    STR_LIQPARAM_FLD_ASPSPLITMINVOLPERCENT = 'AspSplitMinVolPercent';
    STR_LIQPARAM_FLD_DispSpeZMoveFrequency = 'DispSpeZMoveFrequency';

    // alte Felder: raus
    STR_LIQPARAM_FLD_ASPSWITCHPORTNUMBER = 'AspSwitchPortNumber';
    STR_LIQPARAM_FLD_DSPSWITCHPORTNUMBER = 'DspSwitchPortNumber';
    STR_LIQPARAM_FLD_DILASPSWITCHPORTNUMBER = 'DilAspSwitchPortNumber';

    STR_LIQPARAM_FLD_CH1PUMPPUMPNUMBER = 'Ch1PumpNumber';
    STR_LIQPARAM_FLD_TransAirRetakeBetweenDisp = 'TransAirRetakeBetweenDisp';
    STR_LIQPARAM_FLD_TransAirRetakeAfterLastDisp = 'TransAirRetakeAfterLastDisp';

    INT_LIQPARAM_FLDLEN_PARAMNAME = 20;
    INT_LIQPARAM_FLDLEN_DESCRIPTION = 120;
    INT_LIQPARAM_FLDLEN_USEDTIPTYPE = 20;
    INT_LIQPARAM_FLDLEN_ASPSWITCHMODULE = 25;
    INT_LIQPARAM_FLDLEN_DILASPSWITCHMODULE = 25;
    INT_LIQPARAM_FLDLEN_DSPSWITCHMODULE = 25;
    INT_LIQPARAM_FLDLEN_WASHMACRONAME = 20;
    INT_LIQPARAM_FLDLEN_LIQCLASS = 20;
    INT_LIQPARAM_FLDLEN_VOLCORRCURVE = 20;

    INT_LIQPARAM_USEWASHMACRO_DEFAULT = 0;
    INT_LIQPARAM_USEWASHMACRO_MACRO = -1;
    INT_LIQPARAM_USEWASHMACRO_METHOD = 1;

    STR_LIQPARAM_INDEX_FIELDS = STR_LIQPARAM_FLD_PARAMNAME;

    { TLiqHTableStructDefV0 }

procedure TLiqHTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := STR_LIQPARAM_TBL;
end;

{ TLiqHTableStructDefV1 }

procedure TLiqHTableStructDefV1.DoDefineStruct();
begin
    inherited;

    self.AddField(STR_LIQPARAM_FLD_PARAMNAME, tftString, INT_LIQPARAM_FLDLEN_PARAMNAME);
    self.AddField(STR_LIQPARAM_FLD_DESCRIPTION, tftString, INT_LIQPARAM_FLDLEN_DESCRIPTION);
    self.AddField(STR_LIQPARAM_FLD_LIQCLASS, tftString, INT_LIQPARAM_FLDLEN_LIQCLASS);
    self.AddField(STR_LIQPARAM_FLD_VOLCORRCURVE, tftString, INT_LIQPARAM_FLDLEN_VOLCORRCURVE);

    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPCALC, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPSPEED, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPDELAY, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPLIQDET, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPSUBMERGE, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPRETRACTPOS, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPWASTEVOL, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPERRFLAG, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_ASPSWITCHPOS, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_ASPSWITCHMODULE, tftString, INT_LIQPARAM_FLDLEN_ASPSWITCHMODULE);
    self.AddField(STR_LIQPARAM_FLD_RECORDDETECTIONVOLUME, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_ASPIRATIONMETHODRETRACTZ, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPSCANMODE, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPSCANSPEED, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPRETRSPEED, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPRETRDISTANCE, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPINSERTMOVETYPE, tftInteger, 0);

    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPMIXCYCL, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPMIXMETHOD, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPMIXVOL, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPMIXMINRESTVOL, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPMIXSPEED, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPMIXDISPSPEED, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPMIXZOFFSET, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPMIXFIRSTONLY, tftBoolean, 0);

    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPMULTIPIP, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPMULTIMAXVOL, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPMULTIMINVOL, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPMULTISPLITVOL, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPWASTEPERCENT, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPMULTIWASH, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPTIPSINGLERETRACT, tftBoolean, 0);

    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPSPITBACK, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPSPITBACKCOUNT, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPSPITBACKCALC, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPSPITBACKSPEED, tftFloat, 0);

    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPCH2WASHVOL, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPCH2WASHCALC, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPCH2WASHSPEED, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPCH2WASHDELAY, tftFloat, 0);

    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPTIPTOUCH, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPTIPTOUCHDELAY, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPTIPTOUCHSCANMODE, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPTIPTOUCHSINGLE, tftBoolean, 0);

    self.AddField(STR_LIQPARAM_FLD_DILASPCALC, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_DILASPSPEED, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DILASPDELAY, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DILASPSUBMERGE, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DILASPWASTEVOL, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DILASPSPITBACK, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DILASPLIQDET, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_DILASPERRFLAG, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_DILASPTIPSINGLERETRACT, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_DILASPSWITCHPOS, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_DILASPSWITCHMODULE, tftString, INT_LIQPARAM_FLDLEN_DILASPSWITCHMODULE);
    self.AddField(STR_LIQPARAM_FLD_DILASPCHANNEL2, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_DILASPSCANMODE, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_DILASPSCANSPEED, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_DILASPRETRSPEED, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_DILASPRETRDISTANCE, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DILASPINSERTMOVETYPE, tftInteger, 0);

    self.AddField(STR_LIQPARAM_FLD_DISPCALC, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPSPEED, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPDELAY, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPLIQDET, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPERRFLAG, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPTIPSINGLERETRACT, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPSTEPVOLUME, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPSTEPDELAY, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPSUBMERGE, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DSPSWITCHPOS, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_DSPSWITCHMODULE, tftString, INT_LIQPARAM_FLDLEN_DSPSWITCHMODULE);
    self.AddField(STR_LIQPARAM_FLD_DISPSCANMODE, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPSCANSPEED, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPRETRSPEED, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPRETRDISTANCE, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPINSERTMOVETYPE, tftInteger, 0);

    self.AddField(STR_LIQPARAM_FLD_DISPMIXCYCL, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPMIXMETHOD, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPMIXVOL, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPMIXMINRESTVOL, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPMIXSPEED, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPMIXDISPSPEED, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPMIXZOFFSET, tftFloat, 0);

    self.AddField(STR_LIQPARAM_FLD_DISPTIPTOUCH, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPTIPTOUCHDELAY, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPTIPTOUCHSCANMODE, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPTIPTOUCHSINGLE, tftBoolean, 0);

    self.AddField(STR_LIQPARAM_FLD_WASHISFORCED, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_USEWASHMACRO, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_WASHMACRONAME, tftString, INT_LIQPARAM_FLDLEN_WASHMACRONAME);
    self.AddField(STR_LIQPARAM_FLD_WASHVOLMIN, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_WASHVOLMAX, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_WASHFLAG, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_WASHVOLFACTOR, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_WASHVOLCHANNEL2, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DRYAFTERWASH, tftBoolean, 0);

    self.AddField(STR_LIQPARAM_FLD_SYSAIRASPPOS, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_SYSAIRASPDELAY, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SYSAIRASPCALC, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_SYSAIRASPVOL, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SYSAIRASPSPEED, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_SYSAIRDISPVOL, tftFloat, 0);

    self.AddField(STR_LIQPARAM_FLD_TRANSAIRPOS, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_TRANSAIRASPDELAY, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_TRANSAIRASPCALC, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_TRANSAIRVOL, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_TRANSAIRSPEED, tftFloat, 0);

    self.AddField(STR_LIQPARAM_FLD_EXTRAGAPCOUNT, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_EXTRAGAPAIRVOL, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_EXTRAGAPAIRCALC, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_EXTRAGAPAIRSPEED, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_EXTRAGAPAIRDELAY, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_EXTRAGAPWASTEVOL, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_EXTRAGAPWASTECALC, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_EXTRAGAPWASTESPEED, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_EXTRAGAPWASTEDELAY, tftFloat, 0);

    self.AddField(STR_LIQPARAM_FLD_GETTIPFLAG, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_USEDTIPS, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_USEDTIPTYPE, tftString, INT_LIQPARAM_FLDLEN_USEDTIPTYPE);
    self.AddField(STR_LIQPARAM_FLD_USEDISPTIPWITHOUTTIP, tftBoolean, 0);

    self.AddField(STR_LIQPARAM_FLD_DISPEMPTYVARREDI, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_SAMPLEASPXYSHIFTING, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_DISPXYSHIFTING, tftBoolean, 0);

    self.AddField(STR_LIQPARAM_FLD_ASPSPLITMINVOLPERCENT, tftFloat, 0);
    self.AddField(STR_LIQPARAM_FLD_DispSpeZMoveFrequency, tftFloat, 0);

    // werden später gelöscht
    self.AddField(STR_LIQPARAM_FLD_ASPSWITCHPORTNUMBER, tftSmallint, 0);
    self.AddField(STR_LIQPARAM_FLD_DSPSWITCHPORTNUMBER, tftSmallint, 0);
    self.AddField(STR_LIQPARAM_FLD_DILASPSWITCHPORTNUMBER, tftSmallint, 0);

    self.AddIndex(STR_LIQPARAM_INDEX_FIELDS);
end;

{ TLiqHTableUpdateV1 }

procedure TLiqHTableUpdateV1.SetLiqParamDefaultsIfNull();
// var
// xDefaults : TLiqHDefaultArray;
// x : integer;
begin
    { xDefaults := TLiqHDataAdaptor.GetDefaultVals();
      for x := 0 to High( xDefaults ) do
      aVersionUpdate.SetConstField( xDefaults[x].F, xDefaults[x].V, true ); }
end;

{ class procedure TLiqHTableUpdateV1.UpdatePortFieldV1( const aSourceVal: variant; var vDestVal: variant );
  //var
  //    xSwitchPos, xSwithchPort : integer;
  begin
  {    if aSourceVal[2] <> null then EXIT;
  if ( aSourceVal[0] = null ) or ( aSourceVal[1] = null ) then EXIT;
  xSwitchPos := aSourceVal[0];
  if xSwitchPos <= 0 then EXIT;
  xSwithchPort := aSourceVal[1];
  if xSwithchPort < 0 then EXIT;
  vDestVal := Format( 'Port %d', [xSwithchPort] );

  end;
}
procedure TLiqHTableUpdateV1.UpdatePortFieldVersion1(aSwitchPosFieldName, aSwitchPortNumberFieldName,
    aSwithModuleFieldName: string);
begin
    {
      aVersionUpdate.AddDataItem( TTableUpdateExecuteDataMapItem.Create(
      TTableUpdateCopyFieldWithFuncDataMap.Create(
      StringArrayOf( [ aSwitchPosFieldName, aSwitchPortNumberFieldName, aSwithModuleFieldName ] ),
      aSwithModuleFieldName, UpdatePortFieldV1 ) ) );
    }
end;

constructor TLiqHTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLiqHTableStructDefV0, INT_REVISION_1);
    AlterStructure(TLiqHTableStructDefV1);

    // this has to be included for every update
    self.CopyMatchingFields([]);

    // update Port von alten Format
    UpdatePortFieldVersion1(STR_LIQPARAM_FLD_ASPSWITCHPOS, STR_LIQPARAM_FLD_ASPSWITCHPORTNUMBER,
        STR_LIQPARAM_FLD_ASPSWITCHMODULE);
    UpdatePortFieldVersion1(STR_LIQPARAM_FLD_DILASPSWITCHPOS, STR_LIQPARAM_FLD_DILASPSWITCHPORTNUMBER,
        STR_LIQPARAM_FLD_DILASPSWITCHMODULE);
    UpdatePortFieldVersion1(STR_LIQPARAM_FLD_DSPSWITCHPOS, STR_LIQPARAM_FLD_DSPSWITCHPORTNUMBER,
        STR_LIQPARAM_FLD_DSPSWITCHMODULE);

    // update default values
    SetLiqParamDefaultsIfNull();
    CopyMatchingFields([]);
end;

{ TLiqHTableStructDefV2 }

procedure TLiqHTableStructDefV2.DoDefineStruct;
begin
    inherited;

    // alte Felder löschen
    self.DelField(STR_LIQPARAM_FLD_ASPSWITCHPORTNUMBER);
    self.DelField(STR_LIQPARAM_FLD_DSPSWITCHPORTNUMBER);
    self.DelField(STR_LIQPARAM_FLD_DILASPSWITCHPORTNUMBER);

    self.AddFieldAt(STR_LIQPARAM_FLD_USEDPIPDEVICE, tftString, 40, 122); // V7.3.0
end;

{ TLiqHTableStructDefV3 }

procedure TLiqHTableStructDefV3.DoDefineStruct;
begin
    inherited;

    self.ResizeField(STR_LIQPARAM_FLD_WASHMACRONAME, 50);
    { TODO -owl : Eigentlich müssten die Felder USEMACRO, WASHMACRONAME umbenannt werden }
end;

{ TLiqHTableStructDefV5_1 }

procedure TLiqHTableStructDefV5_1.DoDefineStruct;
begin
    inherited;

    // Die Felder der Versionen 4.0, 5.0 und 5.1 wurden hier zusammengefasst,
    // damit alle (Branch-)Versionen ohne Datenverlust updaten können

    self.AddField(STR_LIQPARAM_FLD_CH1PUMPPUMPNUMBER, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_MOVETOSYSAIRPOSSPEED, tftInteger, 0);
    self.AddField(STR_LIQPARAM_FLD_EXTRAGAPAIRPOS, tftInteger, 0);
end;

{ TLiqHTableStructDefV5_2 }

procedure TLiqHTableStructDefV5_2.DoDefineStruct;
begin
    inherited;

    self.AddField(STR_LIQPARAM_FLD_TransAirRetakeBetweenDisp, tftBoolean, 0);
    self.AddField(STR_LIQPARAM_FLD_TransAirRetakeAfterLastDisp, tftBoolean, 0);
end;

{ TLiqHTableStructDefV6 }

procedure TLiqHTableStructDefV6.DoDefineStruct;
begin
    inherited;

    self.AddField(cFieldNameSAMPLEASPMIXUseCalculatedVol, tftBoolean, 0);
    self.AddField(cFieldNameDISPMIXUseCalculatedVol, tftBoolean, 0);
end;

{ TLiqHTableStructDefV7 }

procedure TLiqHTableStructDefV7.DoDefineStruct;
begin
    inherited;

    self.AddField('SampleAspTipTouchScan', tftBoolean, 0);
    self.AddField('SampleAspTipTouchSubmerge', tftFloat, 0);
    self.AddField('DispTipTouchScan', tftBoolean, 0);
    self.AddField('DispTipTouchSubmerge', tftFloat, 0);
end;

{ TLiqHTableStructDefV8 }

procedure TLiqHTableStructDefV8.DoDefineStruct;
begin
    inherited;

    self.AddField(STR_LIQPARAM_FLD_WashUsePeripump, tftBoolean, 0);
end;

{ TLiqHTableStructDefV9 }

procedure TLiqHTableStructDefV9.DoDefineStruct;
begin
    inherited;

    self.DelField(STR_LIQPARAM_FLD_DILASPSPITBACK);
    self.AddField(STR_LIQPARAM_FLD_SampleAspSpitBackAtAspPos, tftBoolean, 0);
end;

{ TLiqHTableStructDefV10 }

procedure TLiqHTableStructDefV10.DoDefineStruct;
begin
    inherited;

    self.AddFieldAt(STR_LIQPARAM_FLD_DISPMIXAspSubmerge, tftFloat, 0, 87);
end;

{ TLiqHTableStructDefV11 }

procedure TLiqHTableStructDefV11.DoDefineStruct;
begin
    inherited;
    self.AddField(STR_LIQPARAM_FLD_DISPTIPTOUCHSCANSTOREVOL, tftBoolean, 0);
end;

{ TLiqHTableUpdateV2 }

constructor TLiqHTableUpdateV2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLiqHTableStructDefV2, INT_REVISION_2);

    AlterStructure(TLiqHTableStructDefV2);
    CopyMatchingFields([]);

    self.CustomDataFunc(LiqHUpdateV2CustomFunc);
end;

procedure TLiqHTableUpdateV2.ReplaceTotalTipMap(aPipDevLoader: TSettingsTablePipDeviceLoader);
var
    xDP: TDataProvider;
    xPipDeviceName: string;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM ' + STR_LIQPARAM_TBL, false);
        try
            while not xDP.Eof do
            begin

                xDP.Edit;
                xDP.FieldByName(STR_LIQPARAM_FLD_USEDTIPS).AsInteger :=
                    aPipDevLoader.GetArmTipmapFromTotalTipmap(xDP.FieldByName(STR_LIQPARAM_FLD_USEDTIPS)
                    .AsInteger, xPipDeviceName);
                xDP.FieldByName(STR_LIQPARAM_FLD_USEDPIPDEVICE).AsString := xPipDeviceName;
                xDP.Post;

                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

procedure TLiqHTableUpdateV2.LiqHUpdateV2CustomFunc(aSender: TObject);
var
    xPipDevLoader: TSettingsTablePipDeviceLoader;
begin
    xPipDevLoader := TSettingsTablePipDeviceLoader.Create(fTableChangeAdaptor);
    try
        self.ReplaceTotalTipMap(xPipDevLoader);
    finally
        xPipDevLoader.Free;
    end;
end;

{ TLiqHTableUpdateV3 }

constructor TLiqHTableUpdateV3.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLiqHTableStructDefV2, INT_REVISION_3);

    AlterStructure(TLiqHTableStructDefV3);
    CopyMatchingFields([]);

    self.CustomDataFunc(LiqHUpdateV3CustomFunc);
end;

function TLiqHTableUpdateV3.GetUpdateDescription: string;
begin
    result := 'Replace Wash Macros by Methods';
end;

procedure TLiqHTableUpdateV3.LiqHUpdateV3CustomFunc(aSender: TObject);
const
    INT_LIQPARAM_USEWASHMACRO_MACRO = -1;
    INT_LIQPARAM_USEWASHMACRO_METHOD = 1;
var
    xDP: TDataProvider;
    xUseMacro: integer;
    xMacroName: string;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM ' + STR_LIQPARAM_TBL, false);
        try
            while not xDP.Eof do
            begin
                xUseMacro := xDP.FieldByName(STR_LIQPARAM_FLD_USEWASHMACRO).AsInteger;
                xMacroName := xDP.FieldByName(STR_LIQPARAM_FLD_WASHMACRONAME).AsString;

                if (xUseMacro = INT_LIQPARAM_USEWASHMACRO_MACRO) and (xMacroName <> '') then
                begin

                    xDP.Edit;
                    xDP.FieldByName(STR_LIQPARAM_FLD_USEWASHMACRO).AsInteger :=
                        INT_LIQPARAM_USEWASHMACRO_METHOD;
                    xDP.FieldByName(STR_LIQPARAM_FLD_WASHMACRONAME).AsString := cMethodNamePrefixExCommand +
                        xMacroName;
                    xDP.Post;
                end;

                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

{ TLiqHTableUpdateV4 }

constructor TLiqHTableUpdateV3_1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLiqHTableStructDefV3, INT_REVISION_3, INT_MINORREVISION_1);

    AlterStructure(TLiqHTableStructDefV3);
    CopyMatchingFields([]);
end;

{ TLiqHTableUpdateV4 }

constructor TLiqHTableUpdateV4.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLiqHTableStructDefV3, INT_REVISION_4);

    AlterStructure(TLiqHTableStructDefV5_1);
    CopyMatchingFields([]);
end;

{ TLiqHTableUpdateV4_1 }

constructor TLiqHTableUpdateV4_1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLiqHTableStructDefV5_1, INT_REVISION_4, INT_MINORREVISION_1);

    AlterStructure(TLiqHTableStructDefV5_1);
    CopyMatchingFields([]);

    self.CustomDataFunc(LiqHUpdateV4_1CustomFunc);
end;

function TLiqHTableUpdateV4_1.GetUpdateDescription: string;
begin
    result := 'Update the "Mix with air" parameters';
end;

procedure TLiqHTableUpdateV4_1.LiqHUpdateV4_1CustomFunc(aSender: TObject);
const
    INT_MIXMODES_DISP_AT_ZOFFSET = $0001; // Dispense at Z-Max - Z-Offset
    INT_MIXMODES_MIX_WITH_AIR = $0002; // Aspirate air and mix with it
var
    xDP: TDataProvider;
    xMap: integer;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM ' + STR_LIQPARAM_TBL, false);
        try
            while not xDP.Eof do
            begin

                // Bisher wurde bei "Mix with air" immer automatisch auch "Disp at Z-Offset" ausgeführt.
                // Dieser Automatismus wurde entfernt. Um sicherzugehen, dass nach dem Update das gleiche
                // passiert wie vorher, wird "Disp at Z-Offset" jetzt immer gesetzt:

                // SampleAspMixMethod
                xMap := xDP.FieldByName(STR_LIQPARAM_FLD_SAMPLEASPMIXMETHOD).AsInteger;
                if ((xMap and INT_MIXMODES_MIX_WITH_AIR) <> 0) // Mix with air
                    and ((xMap and INT_MIXMODES_DISP_AT_ZOFFSET) = 0) // aber nicht "Disp at Z-Max - Z-Offset"
                then
                begin
                    xDP.Edit;
                    xDP.FieldByName(STR_LIQPARAM_FLD_SAMPLEASPMIXMETHOD).AsInteger := xMap or
                        INT_MIXMODES_DISP_AT_ZOFFSET;
                    xDP.Post;
                end;

                // DispMixMethod
                xMap := xDP.FieldByName(STR_LIQPARAM_FLD_DISPMIXMETHOD).AsInteger;
                if ((xMap and INT_MIXMODES_MIX_WITH_AIR) <> 0) // Mix with air
                    and ((xMap and INT_MIXMODES_DISP_AT_ZOFFSET) = 0) // aber nicht "Disp at Z-Max - Z-Offset"
                then
                begin
                    xDP.Edit;
                    xDP.FieldByName(STR_LIQPARAM_FLD_DISPMIXMETHOD).AsInteger := xMap or
                        INT_MIXMODES_DISP_AT_ZOFFSET;
                    xDP.Post;
                end;

                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

{ TLiqHTableUpdateV5 }

constructor TLiqHTableUpdateV5.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLiqHTableStructDefV5_1, INT_REVISION_5);

    AlterStructure(TLiqHTableStructDefV5_1);
    // Update soll sicherstellen, dass von allen (Branch-)versionen richtig upgedated wird
    CopyMatchingFields([]);
end;

{ TLiqHTableUpdateV5_1 }

constructor TLiqHTableUpdateV5_1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLiqHTableStructDefV5_1, INT_REVISION_5, INT_MINORREVISION_1);

    AlterStructure(TLiqHTableStructDefV5_1);
    // Update soll sicherstellen, dass von allen (Branch-)versionen richtig upgedated wird
    CopyMatchingFields([]);
end;

{ TLiqHTableUpdateV5_2 }

constructor TLiqHTableUpdateV5_2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLiqHTableStructDefV5_2, INT_REVISION_5, INT_MINORREVISION_2);

    AlterStructure(TLiqHTableStructDefV5_2);
    CopyMatchingFields([]);

    self.CustomDataFunc(LiqHUpdateV5_2CustomFunc);
end;

procedure TLiqHTableUpdateV5_2.LiqHUpdateV5_2CustomFunc(aSender: TObject);
var
    xDP: TDataProvider;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM ' + STR_LIQPARAM_TBL, false);
        try
            while not xDP.Eof do
            begin
                xDP.Edit;
                xDP.FieldByName(STR_LIQPARAM_FLD_TransAirRetakeBetweenDisp).AsBoolean := true;
                xDP.FieldByName(STR_LIQPARAM_FLD_TransAirRetakeAfterLastDisp).AsBoolean := false;
                xDP.Post;

                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

{ TLiqHTableUpdateV6 }

constructor TLiqHTableUpdateV6.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLiqHTableStructDefV5_2, INT_REVISION_6);

    AlterStructure(TLiqHTableStructDefV6);
    CopyMatchingFields([]);

    self.CustomDataFunc(self.CustomFunc);
end;

procedure TLiqHTableUpdateV6.CustomFunc(aSender: TObject);
var
    xDP: TDataProvider;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM ' + STR_LIQPARAM_TBL, false);
        try
            while not xDP.Eof do
            begin
                xDP.Edit;
                xDP.FieldByName(TLiqHTableStructDefV6.cFieldNameSAMPLEASPMIXUseCalculatedVol)
                    .AsBoolean := false;
                xDP.FieldByName(TLiqHTableStructDefV6.cFieldNameDISPMIXUseCalculatedVol).AsBoolean := false;
                xDP.Post;

                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

{ TLiqHTableUpdateV7 }

constructor TLiqHTableUpdateV7.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLiqHTableStructDefV6, INT_REVISION_7);

    AlterStructure(TLiqHTableStructDefV7);
    CopyMatchingFields([]);

    self.CustomDataFunc(self.CustomFunc);
end;

procedure TLiqHTableUpdateV7.CustomFunc(aSender: TObject);
var
    xDP: TDataProvider;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM ' + STR_LIQPARAM_TBL, false);
        try
            while not xDP.Eof do
            begin
                xDP.Edit;
                xDP.FieldByName(TLiqHTableStructDefV7.cFieldNameSampleAspTipTouchScan).AsBoolean := true;
                xDP.FieldByName(TLiqHTableStructDefV7.cFieldNameDispTipTouchScan).AsBoolean := true;
                xDP.Post;

                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

{ TLiqHTableUpdateV8 }

constructor TLiqHTableUpdateV8.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLiqHTableStructDefV7, INT_REVISION_8);

    AlterStructure(TLiqHTableStructDefV8);
    CopyMatchingFields([]);

    self.CustomDataFunc(self.CustomFunc);
end;

procedure TLiqHTableUpdateV8.CustomFunc(aSender: TObject);
var
    xDP: TDataProvider;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM ' + STR_LIQPARAM_TBL, false);
        try
            while not xDP.Eof do
            begin
                xDP.Edit;
                xDP.FieldByName(STR_LIQPARAM_FLD_WashUsePeripump).AsBoolean := true;
                xDP.Post;

                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

{ TLiqHTableUpdateV9 }

constructor TLiqHTableUpdateV9.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLiqHTableStructDefV8, INT_REVISION_9);

    AlterStructure(TLiqHTableStructDefV9);
    CopyMatchingFields([]);

    self.CustomDataFunc(self.CustomFunc);
end;

procedure TLiqHTableUpdateV9.CustomFunc(aSender: TObject);
var
    xDP: TDataProvider;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM ' + STR_LIQPARAM_TBL, false);
        try
            while not xDP.Eof do
            begin
                xDP.Edit;
                xDP.FieldByName(STR_LIQPARAM_FLD_SampleAspSpitBackAtAspPos).AsBoolean := false;
                xDP.Post;

                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

{ TLiqHTableUpdateV10 }

constructor TLiqHTableUpdateV10.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLiqHTableStructDefV9, INT_REVISION_10);

    AlterStructure(TLiqHTableStructDefV10);
    CopyMatchingFields([]);

    self.CustomDataFunc(self.CustomFunc);
end;

procedure TLiqHTableUpdateV10.CustomFunc(aSender: TObject);
var
    xDP: TDataProvider;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM ' + STR_LIQPARAM_TBL, false);
        try
            while not xDP.Eof do
            begin
                xDP.Edit;
                // Vorher wurde DispSubmerge auch für Mix after disp verwendet:
                xDP.FieldByName(STR_LIQPARAM_FLD_DispMixAspSubmerge).AsFloat :=
                    xDP.FieldByName(STR_LIQPARAM_FLD_DispSubmerge).AsFloat;
                xDP.Post;

                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

{ TLiqHTableUpdateV11 }

constructor TLiqHTableUpdateV11.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLiqHTableStructDefV10, INT_REVISION_11);

    AlterStructure(TLiqHTableStructDefV11);
    CopyMatchingFields([]);

    self.CustomDataFunc(self.CustomFunc);
end;

procedure TLiqHTableUpdateV11.CustomFunc(aSender: TObject);
var
    xDP: TDataProvider;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM ' + STR_LIQPARAM_TBL, false);
        try
            while not xDP.Eof do
            begin
                xDP.Edit;
                xDP.FieldByName(STR_LIQPARAM_FLD_DISPTIPTOUCHSCANSTOREVOL).AsBoolean := false;
                xDP.Post;

                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;


end.
