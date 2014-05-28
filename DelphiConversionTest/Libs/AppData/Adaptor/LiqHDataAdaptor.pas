{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  : Data Adaptor for Liqparam table
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  04.03.05 pk                               TN2330.1 Initial revision ( code from DBRack.pas )
  18.03.05 pk  ReadRecWhenOpen              TN2355   Reads record with opening/closing dataset
  07.04.05 pk  GetAspMultiPip               TN2355   New
  21.04.05 tbh TLiqHandlingRec              TN2386   new parameter 'LiqClass'
  21.04.05 tbh ReadRecFromDataset           TN2386   new parameter 'LiqClass'
  25.05.05 tbh ReadRecForVolWhenOpen        TN2386   New: Reads Speeds, Volumes and Delays from LiquidClass
  31.05.05 tbh TLiqParWashRec               TN2430   new parameter 'VolFactor'
  31.05.05 tbh WriteWashRec                 TN2430   new parameter 'VolFactor'
  31.05.05 tbh ReadRecFromDataset           TN2430   reads 'VolFactor'
  20.06.05 tbh TLiqHandlingRec              TN2385   new parameter 'VolCorrCurve'
  20.06.05 tbh ReadRecFromDataset           TN2385   reads 'VolCorrCurve'
  06.07.05 pk  TLiqHandlingRec              TN2492   new : SampleAspSpitBackCount
  14.07.05 wl  ReadRecForVolWhenOpen        TN2506   6 neue Felder für Spitback
  15.07.05 wl                               TN2506   3 Methoden aus PipetteParamDataAdaptor hierher verschoben
  15.07.05 wl  ReadRecForVolWhenOpen        TN2506   übergibt auch Options
  08.08.05 pk  ReadRecForVolWhenOpen        TN2523   new : SampleAspWasteVol, SampleAspWastePerCent
  08.08.05 pk                               TN2531   New : SampleAspMultMaxVol, SampleAspMultiSplitVol
  08.08.05 pk                               TN2527   New : SampleAspTipTouch, SampleAspTipTouchDely, DispTipTouch, DispTipTouchDelay
  10.08.05 pk  ReadRecForVolWhenOpen        TN2551   parameter aOption changed from var to const
  01.09.05 thr ReadRecForVolWhenOpen        TN2587   wash record read from liquid class
  24.11.05 pk  TLiquid\PowderParDataAdaptor TN2765   New
  24.11.05 pk  GetPipetteParamNames         TN2765   use Dataset instead of TTable
  11.01.06 pk  InstallTable                 TN2871.1 New functions for update management
  11.01.06 pk  VerifyTable                  TN2871.1 This function is a replacement for what was done in DMRack
  11.01.06 pk  ExtraGap                     TN2871.0 New fields
  02.02.06 pk  NewPipetteParameter          TN2920   write aNewName to db instead of xNewName
  25.04.06 wl                               TN3053   Neu: AspSplitMinVolPercent
  24.05.06 wl  gmDispensePowder             TN3119    new field "DispEmptyVarRedi"
  06.09.06 wl  InstallTable                 TN3256   UseWashMacro ist jetzt Integer statt Boolean: Aus TRUE wird -1, aus FALSE wird 0
  06.09.06 wl                               TN3257   Neu: MethodNameGetTip, MethodNamePutTip
  12.09.06 wl  TLiqParWashRec               TN3285   --> LiqClassDataAdaptor
  12.09.06 wl  TLiqHandlingRec              TN3286   enthält MethodNameGetTip
  23.09.06 wl                               TN3326   MethodNameGetTip, MethodNamePutTip wieder entfernt
  04.10.06 wl                               TN3317   neue Felder: SampleAspXYShifting, DispXYShifting
  04.10.06 wl  SetLiqParamDefaultsIfNull    TN3317   Es werden jetzt alle Boolean-Werte gesetzt
  05.10.06 wl  NewPipetteParameter          TN3317   Auch alle Radio-Buttons haben von Beginn an definierte Werte
  05.02.07 pk                               TN3544   Changes for updatemanager
  07.02.07 pk  UpdatePortFieldV1            TN3544   Bug: variant(string) cannot be converted to variant(double) fixed
  16.02.07 pk  UpdatePortFieldV1            TN3544   Bug: when switchpos is null variant(null) cannot be converted to variant(integer)
  18.04.07 wl                               TN3658   neu: SampleAspMixMinRestVol, DispMixMinRestVol
  18.04.07 wl  TLiqHandlingMixRec           TN3658   alle Mix-Parameter überarbeitet: VolPercent entfernt, Vol und Speed als float
  22.06.07 wl  SelectLiqParamAndOpen        TN3740   Neu: Kann Datensatz ( not CaseSensitive ) öffnen
  22.06.07 wl  LiqParamNameExists           TN3740   Neu: Wird von TPipetteParamViewItem verwendet
  30.08.07 pk                               TN3840.2 types moved to LiqHTypes
  03.09.07 pk                               TN3847   Application-specific funcs/classes moved to LiqHDataAdaptorExt
  02.10.07 wl  GetKeyFields                 TN3811.5 neu: damit DeleteName & SaveNameAs benutzt werden können
  09.11.07 pk                               TN3921   Changes for updatemanager
  09.11.07 pk                               TN3922   Dataset changed to DataProvider
  03.07.08 wl                                         TN4157
  23.09.08 wl                               TN4236   Vereinigung mit ..Fieldnames
  25.09.08 wl                               TN4242   es gibt nur noch WashMethod, nicht mehr WashMacro
  29.09.08 wl                               TN4242    WashMacroName hat jetzt 50 chars
  07.10.08 pk SQLAllDistinctNames           TN4265   New
  16.01.09 wl                               TN4362   an Änderungen in TQueryDataAdaptor angepasst
  09.07.09 pk                               TN4585.4 DataProvider.Locate removed, functionality replaced by SelectAndOpen
  31.07.09 wl                               TN3950   3 neue Felder: SampleAspInsertMoveType, DilAspInsertMoveType, DispInsertMoveType
  31.07.09 wl                               TN4024   neu: DispSpeZMoveFrequency (Typ: double)
  31.07.09 wl  ReadRecFromDataset           TN4693   WashFlag und ErrFlag (3 mal) sind jetzt integer
  31.07.09 wl  ReadRecFromDataset           TN4693   Konsistente Datentypen: Alle Volumen,Volspeeds,Delays,Faktoren sind Float; smallint -> integer
  02.10.09 pk  GetDefaultVals               TN4801   array length corrected
  29.06.10 pk  Ch1PumpNumber                TN5173   New field
  29.06.10 pk  TLiqHandlingParamComparer    TN5143   New
  13.07.10 pk  AreCompatible                TN5143   Some fields removed from comparison
  21.07.10 wl                               TN5202   Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  04.08.10 wl  TLiqHandlingParamComparer    TN5217   TipTouch-Details werden nur geprüft, wenn TipTouch = true (SampleAsp, Disp)
  04.08.10 wl  TLiqHandlingParamComparer    TN5217   Unterschiedliche Waste-Einstellungen erlaubt (SampleAsp)
  04.08.10 wl  TLiqHandlingParamComparer    TN5217   Unterschiedliche SpitBack-Einstellungen erlaubt, bis auf SampleAspSpitBackCount! (SampleAsp)
  04.08.10 wl  TLiqHandlingParamComparer    TN5217   Unterschiedliche Ch2Wash-Einstellungen erlaubt (SampleAsp)
  25.08.10 ts  ReadRecFromDataset           TN5248   new: MoveToSysAirPosSpeed
  07.09.10 wl  TLiqHandlingParamComparer    TN5265   Unterschiedliche Volumen beim Waschen erlaubt
  09.09.10 wl  TLiqHandlingParamComparer    TN5266.1 Unterschiedliche VolCorrCurve erlaubt
  22.09.10 wl  ReadRecFromDataset           TN5275   new: ExtraGapAirPos
  20.05.11 ts  WriteMixRec                  TN5588   FirstOnly hat gefehlt
  19.07.11 wl  ReadRecFromDataset           TN5630   new: TransAirRetakeBetweenDisp, TransAirRetakeAfterLastDisp
  27.09.11 wl                               TN5698   LiqClass --> RunStepBuilderHelper
  02.12.11 wl                               TN5758   Neue Felder: 'DispMixUseCalculatedVol', 'SampleAspMixUseCalculatedVol'
  23.02.12 wl                               TN5818   Neue Felder: SampleAspTipTouchScan,-Submerge,DispTipTouchScan,-Submerge = 'SampleAspTipTouchScan';
  18.04.12 wl  CreateDefaultValList         TN5870   Statt Array wird eine Liste erzeugt (das Array war zu klein dimensioniert)
  10.08.12 wl  TLiqHandlingRec              TN5947   Neues Feld: WashUsePeripump
  04.09.12 wl  TLiqHandlingRec              TN5972   Neues Feld: SampleAspSpitBackAtAspPos (DilAspSpitBack entfernt)
  02.01.13 wl  TLiqHandlingRec              TN6064   SampleAspMixFirstOnly ist jetzt TLiqHandlingRec zugeordnet
  02.01.13 wl  TLiqHandlingRec              TN6064   Neues Feld: DispMixAspSubmerge
  07.05.13 ts  TLiqHandlingRec              TN6118   Neues Feld: DispTipTouchScanStoreVol
  15.08.13 wl                               TN6217   Instance-Variablen gelöscht
  -------------------------------------------------------------------------------------------------- }

unit LiqHDataAdaptor;


interface


uses
    Generics.Collections,
    DataProvider,
    GeneralTypes,
    QueryDataAdaptor,
    LiqHTypes;

const
    cLiqHNameDefault = 'DEFAULT';

    STR_LIQPARAM_TBL = 'LIQPARAM';

    STR_LIQPARAM_FLD_PARAMNAME = 'PARAMNAME';
    STR_LIQPARAM_FLD_DESCRIPTION = 'Description';
    STR_LIQPARAM_FLD_LIQCLASS = 'LIQCLASS';
    STR_LIQPARAM_FLD_VOLCORRCURVE = 'VolCorrCurve';

    STR_LIQPARAM_FLD_SAMPLEASPCALC = 'SampleAspCalc';
    STR_LIQPARAM_FLD_SAMPLEASPSPEED = 'SampleAspSpeed';
    STR_LIQPARAM_FLD_SAMPLEASPDELAY = 'SampleAspDelay';
    STR_LIQPARAM_FLD_SAMPLEASPLIQDET = 'SampleAspLiqDet';
    STR_LIQPARAM_FLD_SAMPLEASPZPOS = 'SampleAspZPos';
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
    STR_LIQPARAM_FLD_SAMPLEASPMIXUseCalculatedVol = 'SampleAspMixUseCalculatedVol';
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
    STR_LIQPARAM_FLD_SampleAspTipTouchScan = 'SampleAspTipTouchScan';
    STR_LIQPARAM_FLD_SAMPLEASPTIPTOUCHSCANMODE = 'SampleAspTipTouchScanMode';
    STR_LIQPARAM_FLD_SAMPLEASPTIPTOUCHSINGLE = 'SampleAspTipTouchSingle';
    STR_LIQPARAM_FLD_SampleAspTipTouchSubmerge = 'SampleAspTipTouchSubmerge';

    STR_LIQPARAM_FLD_DILASPCALC = 'DilAspCalc';
    STR_LIQPARAM_FLD_DILASPSPEED = 'DilAspSpeed';
    STR_LIQPARAM_FLD_DILASPDELAY = 'DilAspDelay';
    STR_LIQPARAM_FLD_DILASPSUBMERGE = 'DilAspSubmerge';
    STR_LIQPARAM_FLD_DILASPWASTEVOL = 'DilAspWasteVol';
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
    STR_LIQPARAM_FLD_DISPMIXUseCalculatedVol = 'DispMixUseCalculatedVol';
    STR_LIQPARAM_FLD_DISPMIXSPEED = 'DispMixSpeed';
    STR_LIQPARAM_FLD_DISPMIXDISPSPEED = 'DispMixDispSpeed';
    STR_LIQPARAM_FLD_DISPMIXZOFFSET = 'DispMixZOffset';
    STR_LIQPARAM_FLD_DISPMIXAspSubmerge = 'DispMixAspSubmerge';

    STR_LIQPARAM_FLD_DISPTIPTOUCH = 'DispTipTouch';
    STR_LIQPARAM_FLD_DISPTIPTOUCHDELAY = 'DispTipTouchDelay';
    STR_LIQPARAM_FLD_DISPTIPTOUCHSCAN = 'DispTipTouchScan';
    STR_LIQPARAM_FLD_DISPTIPTOUCHSCANMODE = 'DispTipTouchScanMode';
    STR_LIQPARAM_FLD_DISPTIPTOUCHSINGLE = 'DispTipTouchSingle';
    STR_LIQPARAM_FLD_DISPTipTouchSubmerge = 'DispTipTouchSubmerge';
    STR_LIQPARAM_FLD_DISPTipTouchScanStoreVol = 'DispTipTouchScanStoreVol';

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
    STR_LIQPARAM_FLD_MoveToSysAirPosSpeed = 'MoveToSysAirPosSpeed';

    STR_LIQPARAM_FLD_TRANSAIRPOS = 'TransAirPos';
    STR_LIQPARAM_FLD_TRANSAIRASPDELAY = 'TransAirAspDelay';
    STR_LIQPARAM_FLD_TRANSAIRASPCALC = 'TransAirAspCalc';
    STR_LIQPARAM_FLD_TRANSAIRVOL = 'TransAirVol';
    STR_LIQPARAM_FLD_TRANSAIRSPEED = 'TransAirSpeed';
    STR_LIQPARAM_FLD_TransAirRetakeBetweenDisp = 'TransAirRetakeBetweenDisp';
    STR_LIQPARAM_FLD_TransAirRetakeAfterLastDisp = 'TransAirRetakeAfterLastDisp';

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

    STR_LIQPARAM_FLD_CH1PUMPNUMBER = 'Ch1PumpNumber';

type
    TLiqHDefault = TKeyValueItem<string, variant>;

    TLiqHDataAdaptor = class(TQueryDataAdaptor)
    private const
        STR_SQL_LIQPARAM_FROM = ' FROM ' + STR_LIQPARAM_TBL;
        STR_LIQPARAM_SELECT = 'SELECT * ' + STR_SQL_LIQPARAM_FROM;
    private
        procedure SelectAndOpenLiqParamTryDef(const aName: string; aReadOnly, aCaseSensitive: boolean);
    protected
        class function CreateDefaultValList(): TObjectList<TLiqHDefault>;
        function GetNameField(): string; override;
    public
        constructor Create();

        procedure SelectAndOpenLiqParam(const aName: string; aReadOnly: boolean; aCaseSensitive: boolean);
        function ReadRec(const aName: string; var vRec: TLiqHandlingRec): boolean;
        function ReadUsedTipType(const aName: string; out oUsedTipType: string): boolean;

        function AnyWithMultipip: boolean;
        function LiqParamNameExists(var vName: string; aCaseSensitive: boolean): boolean;

        class function ReadRecFromDataset(aDataset: TDataProvider; var vRec: TLiqHandlingRec): boolean;
        class function WriteWashRec(aWashIsForced: boolean; aUseWashMethod: boolean;
            const aWashMethodName: string; aWashVolMin, aWashVolMax: double; aWashFlag: integer;
            aWashVolFactor, aWashVolChannel2: double; aDryAfterWash, aUsePeripump: boolean): TLiqParWashRec;
        class function WriteMixRec(aCycles, aMethod: integer; aVol, aMinRestVol: double;
            aUseCalculatedVol: boolean; aAspSpeed, aDispSpeed: double; aZOffset_mm: double)
            : TLiqHandlingMixRec;
        class function GetAspMultiPip(aDataset: TDataProvider): boolean;
        class function GetUsedTipType(aDataset: TDataProvider): string;
        class function GetUsedTips(aDataset: TDataProvider; out oPipDeviceName: string): integer;
    end;


implementation


uses
    SysUtils;

{ TLiqHDataAdaptor }

constructor TLiqHDataAdaptor.Create;
begin
    inherited Create(STR_LIQPARAM_TBL);
end;

function TLiqHDataAdaptor.GetNameField(): string;
begin
    result := STR_LIQPARAM_FLD_PARAMNAME;
end;

class function TLiqHDataAdaptor.GetAspMultiPip(aDataset: TDataProvider): boolean;
begin
    result := aDataset.FieldByName('SampleAspMultipip').AsBoolean;
end;

class function TLiqHDataAdaptor.GetUsedTipType(aDataset: TDataProvider): string;
begin
    result := aDataset.FieldByName(STR_LIQPARAM_FLD_USEDTIPTYPE).AsString;
end;

class function TLiqHDataAdaptor.GetUsedTips(aDataset: TDataProvider; out oPipDeviceName: string): integer;
begin
    oPipDeviceName := aDataset.FieldByName(STR_LIQPARAM_FLD_USEDPIPDEVICE).AsString;
    result := aDataset.FieldByName(STR_LIQPARAM_FLD_USEDTIPS).AsInteger;
end;

function TLiqHDataAdaptor.ReadUsedTipType(const aName: string; out oUsedTipType: string): boolean;
begin
    SelectAndOpenLiqParam(aName, true, false);
    try
        result := false;
        if self.DataProvider.IsEmpty then
            EXIT;
        oUsedTipType := self.GetUsedTipType(self.DataProvider);
        result := true;
    finally
        Close();
    end;
end;

class function TLiqHDataAdaptor.WriteWashRec(aWashIsForced: boolean; aUseWashMethod: boolean;
    const aWashMethodName: string; aWashVolMin, aWashVolMax: double; aWashFlag: integer;
    aWashVolFactor, aWashVolChannel2: double; aDryAfterWash, aUsePeripump: boolean): TLiqParWashRec;
begin
    with result do
    begin
        IsForced := aWashIsForced;
        UseMethod := aUseWashMethod;
        WashMethodName := aWashMethodName;
        VolMin := aWashVolMin;
        VolMax := aWashVolMax;
        Flag := aWashFlag;
        if (aWashVolFactor < 1) then
            VolFactor := 1
        else
            VolFactor := aWashVolFactor;
        VolChannel2 := aWashVolChannel2;
        DryAfterWash := aDryAfterWash;
        UsePeripump := aUsePeripump;
    end;
end;

function TLiqHDataAdaptor.AnyWithMultipip: boolean;
begin
    result := false;
    SelectAndOpenAll(true);
    try
        while not self.DataProvider.Eof do
        begin
            if GetAspMultiPip(self.DataProvider) then
            begin
                result := true;
                EXIT;
            end;
            self.DataProvider.Next;
        end;
    finally
        Close();
    end;
end;

class function TLiqHDataAdaptor.WriteMixRec(aCycles, aMethod: integer; aVol, aMinRestVol: double;
    aUseCalculatedVol: boolean; aAspSpeed, aDispSpeed: double; aZOffset_mm: double): TLiqHandlingMixRec;
begin
    result.Cycles := aCycles;
    result.Method := aMethod;
    result.Vol := aVol;
    result.MinRestVol := aMinRestVol;
    result.UseCalculatedVol := aUseCalculatedVol;
    result.AspSpeed := aAspSpeed;
    result.DispSpeed := aDispSpeed;
    result.ZOffset_mm := aZOffset_mm;
end;

class function TLiqHDataAdaptor.ReadRecFromDataset(aDataset: TDataProvider;
    var vRec: TLiqHandlingRec): boolean;
begin
    result := not aDataset.Eof;
    with vRec do
    begin
        // ----------------------------------------------------------------------------- Parameter-Übergabe
        Paramname := aDataset.FieldByName(STR_LIQPARAM_FLD_PARAMNAME).AsString;
        Description := aDataset.FieldByName('Description').AsString;
        LiqClass := aDataset.FieldByName('LIQCLASS').AsString;
        VolCorrCurve := aDataset.FieldByName('VolCorrCurve').AsString;
        // ---------------------------------------------------------------------------- Aspirate Sample
        SampleAspCalc := aDataset.FieldByName('SampleAspCalc').AsBoolean;
        SampleAspSpeed := aDataset.FieldByName('SampleAspSpeed').AsFloat;
        SampleAspDelay := aDataset.FieldByName('SampleAspDelay').AsFloat;
        SampleAspLiqDet := aDataset.FieldByName('SampleAspLiqDet').AsInteger;
        SampleAspSubmerge := aDataset.FieldByName('SampleAspSubmerge').AsFloat;
        SampleAspRetractPos_mm := aDataset.FieldByName('SampleAspRetractPos').AsFloat;
        SampleAspWasteVol := aDataset.FieldByName('SampleAspWasteVol').AsFloat;
        SampleAspErrFlag := aDataset.FieldByName('SampleAspErrFlag').AsInteger;
        AspSwitchPos := aDataset.FieldByName('AspSwitchPos').AsInteger;
        AspSwitchModule := aDataset.FieldByName('AspSwitchModule').AsString;
        RecordDetectionVolume := aDataset.FieldByName('RecordDetectionVolume').AsBoolean;
        AspirationMethodRetractZ := aDataset.FieldByName('AspirationMethodRetractZ').AsBoolean;
        SampleAspScanMode := aDataset.FieldByName('SampleAspScanMode').AsInteger;
        SampleAspScanSpeed := aDataset.FieldByName('SampleAspScanSpeed').AsInteger;
        SampleAspRetrSpeed := aDataset.FieldByName('SampleAspRetrSpeed').AsInteger;
        SampleAspRetrDistance := aDataset.FieldByName('SampleAspRetrDistance').AsFloat;
        SampleAspInsertMoveType := aDataset.FieldByName(STR_LIQPARAM_FLD_SAMPLEASPINSERTMOVETYPE).AsInteger;

        // ---------------------------------------------------------------------------- Aspirate Sample Mixen
        SampleAspMix := self.WriteMixRec(aDataset.FieldByName(STR_LIQPARAM_FLD_SAMPLEASPMIXCYCL).AsInteger,
            aDataset.FieldByName(STR_LIQPARAM_FLD_SAMPLEASPMIXMETHOD).AsInteger,
            aDataset.FieldByName(STR_LIQPARAM_FLD_SAMPLEASPMIXVOL).AsFloat,
            aDataset.FieldByName(STR_LIQPARAM_FLD_SAMPLEASPMIXMINRESTVOL).AsFloat,
            aDataset.FieldByName(STR_LIQPARAM_FLD_SAMPLEASPMIXUseCalculatedVol).AsBoolean,
            aDataset.FieldByName(STR_LIQPARAM_FLD_SAMPLEASPMIXSPEED).AsFloat,
            aDataset.FieldByName(STR_LIQPARAM_FLD_SAMPLEASPMIXDISPSPEED).AsFloat,
            aDataset.FieldByName(STR_LIQPARAM_FLD_SAMPLEASPMIXZOFFSET).AsFloat);
        SampleAspMixFirstOnly := aDataset.FieldByName(STR_LIQPARAM_FLD_SAMPLEASPMIXFIRSTONLY).AsBoolean;
        // ---------------------------------------------------------------------------- Aspirate Sample Multipip
        SampleAspMultipip := GetAspMultiPip(aDataset);
        SampleAspMultiMaxVol := aDataset.FieldByName('SampleAspMultiMaxVol').AsFloat;
        SampleAspMultiMinVol := aDataset.FieldByName('SampleAspMultiMinVol').AsFloat;
        SampleAspMultiSplitVol := aDataset.FieldByName('SampleAspMultiSplitVol').AsFloat;
        SampleAspWastePerCent := aDataset.FieldByName('SampleAspWastePerCent').AsFloat;
        SampleAspMultiWash := aDataset.FieldByName('SampleAspMultiWash').AsBoolean;
        SampleAspTipSingleRetract := aDataset.FieldByName('SampleAspTipSingleRetract').ASBoolean;
        // ---------------------------------------------------------------------------- Aspirate Sample Spitback Kanal 1 nach Aspirate
        SampleAspSpitBack := aDataset.FieldByName('SampleAspSpitBack').AsFloat;
        SampleAspSpitBackCount := aDataset.FieldByName('SampleAspSpitBackCount').AsInteger;
        SampleAspSpitBackCalc := aDataset.FieldByName('SampleAspSpitBackCalc').AsBoolean;
        SampleAspSpitBackSpeed := aDataset.FieldByName('SampleAspSpitBackSpeed').AsFloat;
        SampleAspSpitBackAtAspPos := aDataset.FieldByName('SampleAspSpitBackAtAspPos').AsBoolean;

        // ---------------------------------------------------------------------------- Aspirate Sample Säubern der Nadel nach Aspirate mit Kanal 2
        SampleAspCh2WashVol := aDataset.FieldByName('SampleAspCh2WashVol').AsFloat;
        SampleAspCh2WashCalc := aDataset.FieldByName('SampleAspCh2WashCalc').AsBoolean;
        SampleAspCh2WashSpeed := aDataset.FieldByName('SampleAspCh2WashSpeed').AsFloat;
        SampleAspCh2WashDelay := aDataset.FieldByName('SampleAspCh2WashDelay').AsFloat;
        // ----------------------------------------------------------------------------- Aspirate Sample Tip Touch
        SampleAspTipTouch := aDataset.FieldByName('SampleAspTipTouch').AsBoolean;
        SampleAspTipTouchDelay := aDataset.FieldByName('SampleAspTipTouchDelay').AsFloat;
        SampleAspTipTouchScan := aDataset.FieldByName('SampleAspTipTouchScan').AsBoolean;
        SampleAspTipTouchScanMode := aDataset.FieldByName('SampleAspTipTouchScanMode').AsInteger;
        SampleAspTipTouchSingle := aDataset.FieldByName('SampleAspTipTouchSingle').AsBoolean;
        SampleAspTipTouchSubmerge := aDataset.FieldByName('SampleAspTipTouchSubmerge').AsFloat;
        // ---------------------------------------------------------------------------- Aspirate Diluent
        DilAspCalc := aDataset.FieldByName('DilAspCalc').AsBoolean;
        DilAspSpeed := aDataset.FieldByName('DilAspSpeed').AsFloat;
        DilAspDelay := aDataset.FieldByName('DilAspDelay').AsFloat;
        DilAspSubmerge := aDataset.FieldByName('DilAspSubmerge').AsFloat;
        DilAspWasteVol := aDataset.FieldByName('DilAspWasteVol').AsFloat;
        DilAspLiqDet := aDataset.FieldByName('DilAspLiqDet').AsInteger;
        DilAspErrFlag := aDataset.FieldByName('DilAspErrFlag').AsInteger;
        DilAspTipSingleRetract := aDataset.FieldByName('DilAspTipSingleRetract').AsBoolean;
        DilAspSwitchPos := aDataset.FieldByName('DilAspSwitchPos').AsInteger;
        DilAspSwitchModule := aDataset.FieldByName('DilAspSwitchModule').AsString;
        DilAspChannel2 := aDataset.FieldByName('DilAspChannel2').AsBoolean;
        DilAspScanMode := aDataset.FieldByName('DilAspScanMode').AsInteger;
        DilAspScanSpeed := aDataset.FieldByName('DilAspScanSpeed').AsInteger;
        DilAspRetrSpeed := aDataset.FieldByName('DilAspRetrSpeed').AsInteger;
        DilAspRetrDistance := aDataset.FieldByName('DilAspRetrDistance').AsFloat;
        DilAspInsertMoveType := aDataset.FieldByName(STR_LIQPARAM_FLD_DILASPINSERTMOVETYPE).AsInteger;
        // ---------------------------------------------------------------------------- Dispense
        DispCalc := aDataset.FieldByName('DispCalc').AsBoolean;
        DispSpeed := aDataset.FieldByName('DispSpeed').AsFloat;
        DispDelay := aDataset.FieldByName('DispDelay').AsFloat;
        DispLiqDet := aDataset.FieldByName('DispLiqDet').AsInteger;
        DispErrFlag := aDataset.FieldByName('DispErrFlag').AsInteger;
        DispTipSingleRetract := aDataset.FieldByName('DispTipSingleRetract').AsBoolean;
        DispStepVolume := aDataset.FieldByName('DispStepVolume').AsFloat;
        DispStepDelay := aDataset.FieldByName('DispStepDelay').AsFloat;
        DispSubmerge := aDataset.FieldByName('DispSubmerge').AsFloat;
        DspSwitchPos := aDataset.FieldByName('DspSwitchPos').AsInteger;
        DspSwitchModule := aDataset.FieldByName('DspSwitchModule').AsString;
        DispScanMode := aDataset.FieldByName('DispScanMode').AsInteger;
        DispScanSpeed := aDataset.FieldByName('DispScanSpeed').AsInteger;
        DispRetrSpeed := aDataset.FieldByName('DispRetrSpeed').AsInteger;
        DispRetrDistance := aDataset.FieldByName('DispRetrDistance').AsFloat;
        DispInsertMoveType := aDataset.FieldByName(STR_LIQPARAM_FLD_DISPINSERTMOVETYPE).AsInteger;
        // ---------------------------------------------------------------------------- Dispense Mixen
        DispMix := self.WriteMixRec(aDataset.FieldByName(STR_LIQPARAM_FLD_DISPMIXCYCL).AsInteger,
            aDataset.FieldByName(STR_LIQPARAM_FLD_DISPMIXMETHOD).AsInteger,
            aDataset.FieldByName(STR_LIQPARAM_FLD_DISPMIXVOL).AsFloat,
            aDataset.FieldByName(STR_LIQPARAM_FLD_DISPMIXMINRESTVOL).AsFloat,
            aDataset.FieldByName(STR_LIQPARAM_FLD_DISPMIXUseCalculatedVol).AsBoolean,
            aDataset.FieldByName(STR_LIQPARAM_FLD_DISPMIXSPEED).AsFloat,
            aDataset.FieldByName(STR_LIQPARAM_FLD_DISPMIXDISPSPEED).AsFloat,
            aDataset.FieldByName(STR_LIQPARAM_FLD_DISPMIXZOFFSET).AsFloat);
        DispMixAspSubmerge := aDataset.FieldByName(STR_LIQPARAM_FLD_DISPMIXAspSubmerge).AsFloat;
        // ----------------------------------------------------------------------------- Dispense Tip Touch
        DispTipTouch := aDataset.FieldByName('DispTipTouch').AsBoolean;
        DispTipTouchDelay := aDataset.FieldByName('DispTipTouchDelay').AsFloat;
        DispTipTouchScan := aDataset.FieldByName('DispTipTouchScan').AsBoolean;
        DispTipTouchScanMode := aDataset.FieldByName('DispTipTouchScanMode').AsInteger;
        DispTipTouchSingle := aDataset.FieldByName('DispTipTouchSingle').AsBoolean;
        DispTipTouchSubmerge := aDataset.FieldByName('DispTipTouchSubmerge').AsFloat;
        DispTipTouchScanStoreVol := aDataset.FieldByName('DispTipTouchScanStoreVol').AsBoolean;
        // ---------------------------------------------------------------------------- Wash
        Wash := WriteWashRec(aDataset.FieldByName('WashIsForced').AsBoolean,
            (aDataset.FieldByName(STR_LIQPARAM_FLD_USEWASHMACRO).AsInteger = INT_LIQPARAM_USEWASHMETHOD_YES),
            aDataset.FieldByName(STR_LIQPARAM_FLD_WASHMACRONAME).AsString, aDataset.FieldByName('WashVolMin')
            .AsFloat, aDataset.FieldByName('WashVolMax').AsFloat, aDataset.FieldByName('WashFlag').AsInteger,
            aDataset.FieldByName('WashVolFactor').AsFloat, aDataset.FieldByName('WashVolChannel2').AsFloat,
            aDataset.FieldByName('DryAfterWash').AsBoolean,
            aDataset.FieldByName(STR_LIQPARAM_FLD_WashUsePeripump).AsBoolean);
        // ---------------------------------------------------------------------------- AirGaps
        SysAirAspPos := aDataset.FieldByName('SysAirAspPos').AsInteger;
        SysAirAspDelay := aDataset.FieldByName('SysAirAspDelay').AsFloat;
        SysAirAspCalc := aDataset.FieldByName('SysAirAspCalc').AsBoolean;
        SysAirAspVol := aDataset.FieldByName('SysAirAspVol').AsFloat;
        SysAirAspSpeed := aDataset.FieldByName('SysAirAspSpeed').AsFloat;
        SysAirDispVol := aDataset.FieldByName('SysAirDispVol').AsFloat;
        MoveToSysAirPosSpeed := aDataset.FieldByName('MoveToSysAirPosSpeed').AsInteger;

        TransAirPos := aDataset.FieldByName('TransAirPos').AsInteger;
        TransAirAspDelay := aDataset.FieldByName('TransAirAspDelay').AsFloat;
        TransAirAspCalc := aDataset.FieldByName('TransAirAspCalc').AsBoolean;
        TransAirVol := aDataset.FieldByName('TransAirVol').AsFloat;
        TransAirSpeed := aDataset.FieldByName('TransAirSpeed').AsFloat;
        TransAirRetakeBetweenDisp := aDataset.FieldByName(STR_LIQPARAM_FLD_TransAirRetakeBetweenDisp)
            .AsBoolean;
        TransAirRetakeAfterLastDisp := aDataset.FieldByName(STR_LIQPARAM_FLD_TransAirRetakeAfterLastDisp)
            .AsBoolean;
        // ----------------------------------------------------------------------------- ExtraGap
        ExtraGapCount := aDataset.FieldByName(STR_LIQPARAM_FLD_EXTRAGAPCOUNT).AsInteger;
        ExtraGapAirVol := aDataset.FieldByName(STR_LIQPARAM_FLD_EXTRAGAPAIRVOL).AsFloat;
        ExtraGapAirCalc := aDataset.FieldByName(STR_LIQPARAM_FLD_EXTRAGAPAIRCALC).AsBoolean;
        ExtraGapAirSpeed := aDataset.FieldByName(STR_LIQPARAM_FLD_EXTRAGAPAIRSPEED).AsFloat;
        ExtraGapAirDelay := aDataset.FieldByName(STR_LIQPARAM_FLD_EXTRAGAPAIRDELAY).AsFloat;
        ExtraGapAirPos := aDataset.FieldByName(STR_LIQPARAM_FLD_EXTRAGAPAIRPOS).AsInteger;
        ExtraGapWasteVol := aDataset.FieldByName(STR_LIQPARAM_FLD_EXTRAGAPWASTEVOL).AsFloat;
        ExtraGapWasteCalc := aDataset.FieldByName(STR_LIQPARAM_FLD_EXTRAGAPWASTECALC).AsBoolean;
        ExtraGapWasteSpeed := aDataset.FieldByName(STR_LIQPARAM_FLD_EXTRAGAPWASTESPEED).AsFloat;
        ExtraGapWasteDelay := aDataset.FieldByName(STR_LIQPARAM_FLD_EXTRAGAPWASTEDELAY).AsFloat;
        // ---------------------------------------------------------------------------- Powder
        DispEmptyVarRedi := aDataset.FieldByName(STR_LIQPARAM_FLD_DISPEMPTYVARREDI).AsBoolean;
        SampleAspXYShifting := aDataset.FieldByName(STR_LIQPARAM_FLD_SAMPLEASPXYSHIFTING).AsBoolean;
        DispXYShifting := aDataset.FieldByName(STR_LIQPARAM_FLD_DISPXYSHIFTING).AsBoolean;
        // ---------------------------------------------------------------------------- Tips
        GetTipFlag := aDataset.FieldByName('GetTipFlag').ASInteger;
        UsedTips := GetUsedTips(aDataset, UsedPipDevice);
        UsedTipType := GetUsedTipType(aDataset);
        UseDispTipWithoutTip := aDataset.FieldByName('UseDispTipWithoutTip').AsBoolean;
        // ---------------------------------------------------------------------------- Aspirate splitting
        AspSplitMinVolPercent := aDataset.FieldByName(STR_LIQPARAM_FLD_ASPSPLITMINVOLPERCENT).AsFloat;
        // ---------------------------------------------------------------------------- SPE Mode
        DispSpeZMoveFrequency := aDataset.FieldByName(STR_LIQPARAM_FLD_DispSpeZMoveFrequency).AsFloat;
        // ------------------------------------------------------------------------------------------------
        Ch1PumpNumber := aDataset.FieldByName(STR_LIQPARAM_FLD_CH1PUMPNUMBER).AsInteger;
        Valid := true;
    end;
end;

procedure TLiqHDataAdaptor.SelectAndOpenLiqParam(const aName: string; aReadOnly, aCaseSensitive: boolean);
begin
    if (aCaseSensitive) then
        self.SelectAndOpen(Format(STR_LIQPARAM_SELECT + ' WHERE %s = ''%s''', [STR_LIQPARAM_FLD_PARAMNAME,
            aName]), aReadOnly)
    else
        self.SelectAndOpen(Format(STR_LIQPARAM_SELECT + ' WHERE UPPER(%s) = ''%s''',
            [STR_LIQPARAM_FLD_PARAMNAME, UpperCase(aName)]), aReadOnly);
end;

procedure TLiqHDataAdaptor.SelectAndOpenLiqParamTryDef(const aName: string;
    aReadOnly, aCaseSensitive: boolean);
begin
    self.SelectAndOpenLiqParam(aName, aReadOnly, aCaseSensitive);
    if self.DataProvider.IsEmpty then
    begin
        self.Close();
        SelectAndOpenLiqParam(cLiqHNameDefault, aReadOnly, aCaseSensitive);
        ASSERT(not self.DataProvider.IsEmpty, Format('LiqHParam [%s] not found', [aName]));
    end;
end;

function TLiqHDataAdaptor.LiqParamNameExists(var vName: string; aCaseSensitive: boolean): boolean;
begin
    SelectAndOpenLiqParam(vName, true, aCaseSensitive);
    try
        if (not self.DataProvider.Eof) then
            vName := self.DataProvider.FieldByName(STR_LIQPARAM_FLD_PARAMNAME).AsString;

        result := not self.DataProvider.Eof;
    finally
        Close();
    end;
end;

function TLiqHDataAdaptor.ReadRec(const aName: string; var vRec: TLiqHandlingRec): boolean;
begin
    self.SelectAndOpenLiqParamTryDef(aName, true, false);
    try
        result := self.ReadRecFromDataset(self.DataProvider, vRec);
    finally
        Close();
    end;
end;

class function TLiqHDataAdaptor.CreateDefaultValList(): TObjectList<TLiqHDefault>;
begin
    result := TObjectList<TLiqHDefault>.Create;

    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_SYSAIRASPPOS, 0));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_TRANSAIRPOS, 0));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_SAMPLEASPMIXFIRSTONLY, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_RECORDDETECTIONVOLUME, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_ASPIRATIONMETHODRETRACTZ, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_USEDISPTIPWITHOUTTIP, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_DILASPCHANNEL2, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_SAMPLEASPSPITBACKCALC, true));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_SAMPLEASPSPITBACKAtAspPos, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_SAMPLEASPCH2WASHCALC, true));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_WASHISFORCED, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_USEWASHMACRO, 0));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_DRYAFTERWASH, true));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_WashUsePeripump, true));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_SAMPLEASPTIPTOUCH, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_SAMPLEASPTIPTOUCHSINGLE, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_SAMPLEASPTIPTOUCHSCAN, true));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_DISPTIPTOUCH, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_DISPTIPTOUCHSINGLE, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_DISPTIPTOUCHSCAN, true));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_SAMPLEASPTIPSINGLERETRACT, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_DILASPTIPSINGLERETRACT, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_DISPTIPSINGLERETRACT, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_SYSAIRASPCALC, true));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_SAMPLEASPCALC, true));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_TRANSAIRASPCALC, true));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_DILASPCALC, true));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_DISPCALC, true));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_SAMPLEASPMULTIPIP, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_EXTRAGAPAIRCALC, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_EXTRAGAPWASTECALC, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_DISPEMPTYVARREDI, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_SAMPLEASPXYSHIFTING, true));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_DISPXYSHIFTING, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_SAMPLEASPINSERTMOVETYPE, 0));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_DILASPINSERTMOVETYPE, 0));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_DISPINSERTMOVETYPE, 0));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_TransAirRetakeBetweenDisp, true));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_TransAirRetakeAfterLastDisp, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_DispMixUseCalculatedVol, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_SampleAspMixUseCalculatedVol, false));
    result.Add(TLiqHDefault.Create(STR_LIQPARAM_FLD_DISPTipTouchScanStoreVol, false));

end;


end.
