{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Common types (for all Zinsser Applications)
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  20.12.02 wl                               TN1293.5 initial version
  20.12.02 wl                               TN1293.5 alle const aus CommonConstants hierher (z.T. deaktiviert)
  27.12.02 wl  all const values             TN1293.5 zum Teil aktiviert, zum Teil entfernt
  27.12.02 wl  TModuleType, TComDevType     TN1293.5 --> Device
  27.12.02 wl  TDevicePart,TDevicePartArray TN1293.5 --> Device (TDevicePart -> TDevice)
  27.12.02 wl  TDeviceDisplay,TRediDevices  TN1293.5 --> Device
  28.01.03 wl  MAX_ADR, TAInfo              TN1293.5 --> RoboticInterface
  28.01.03 wl  TDeviceType(s)               TN1293.5 aus Device hierher verschoben
  28.01.03 wl  TRackStruct                  TN1293.5 'PosShifted' eingefügt (ohne Gewähr) - Tobias, bitte melden!
  12.01.03 wl  STR_TEMP_INIFILENAME         TN1293.5 --> ZACommon.Dll
  18.02.03 wl  PSamplerEvent, TSamplerEvent TN1345 --> AppInterface
  20.02.03 wl  type TEventRec               TN1334.3 entfernt
  20.02.03 wl  type TSamplerThreadMode      TN1334.3 neu: mdUserLogRun - Should the user actions be logged for this run?
  05.03.03 tbh STR_SCRIPT_DESCR_STATUS_ID        TN1306 von ObjSampl hierher verschoben
  05.03.03 tbh STR_SCRIPT_DESCR_ENDTIME_ID       TN1306 von ObjSampl hierher verschoben
  05.03.03 tbh STR_SCRIPT_DESCR_ATTRIBUTE_DELIM  TN1306 von ObjSampl hierher verschoben
  05.03.03 tbh STR_SCRIPT_DESCR_ATTRIBUTE_DELIM  TN1306 von ';' zu '/' geändert
  11.03.03 mo  cCPRunName                   TN1442 Von SAMI nach 'CALLIP' geändert
  14.03.03 wl  type TSamParsStr             TN1293.7 entfernt
  09.04.03 wl  type TTipPositions           TN1464   für die geänderte Funktion GetTipPositions
  05.05.03 tbh STR_METHOD_UNIVERSAL_LAYOUT  TN1481   neu: Layoutname für universelle Methoden
  02.06.03 wl                               TN1485.4 neu: TRoboticArmType
  12.06.03 tbh TRackStruct                  TN1501   neu: TubeY2_mm,ZPos...,Color,Cap-Settings
  13.06.03 wl  TDeviceType                  TN1501.1 neu: mSamPhotometer, dptBCReader, dptTurntable, dptDecapper
  17.06.03 wl  TRackStruct                  TN1501   PosShifted entfernt
  17.06.03 wl  CPosinfo-Konstanten          TN1501.7 --> PosinfoDataAdaptor
  25.06.03 wl  TTubeOptions                 TN1501.7 statt integer-Wert -> übersichtlicher als vorher
  03.07.03 wl  TDeviceType                  TN1501.2 neue Device types: dptBCReader, dptTurntable, dptDecapper
  08.07.03 wl  BARCODESTR                   TN1501.1 Umstellung BCReader: Barcodestr entfernt
  17.07.03 wl  TTubeParameters              TN1501.7 LastPos und FirstPos durch eine pos ersetzt
  25.07.03 wl  TMoveTubeModes               TN1501.7 set of TMoveTubeMode: für mehrere Angaben
  25.07.03 wl  TMoveTubeMode                TN1501.7 neu: mtmNoStartPosAfterGetTube, mtmNoStartPosBeforePutTube
  29.07.03 wl  TDeviceTypes                 TN1501.3 neu: dptSensor
  03.08.03 wl  TTubeOptions                 TN1501.3 neue Optionen optTubeNoCapSensor (256) und optTubeNoTubeSensor (512)
  14.08.03 wl                               TN1526   viele Sampler.dll-Konstanten aktiviert (für PowderHandling)
  30.08.03 wl  cBCError, TReadBarcodeModes  TN1543   von BasicThr hierher verschoben
  02.09.03 wl  RACKNAME,SLOTNAME,..         TN1559   Char-Arrays durch string ersetzt
  02.09.03 pk  TEventFuncExt,TcEventFuncExt TN1556.2 New function types that contain a pointer as a parameter
  12.09.03 wl  TSlotStruct,TLayoutStruct    TN1581.7 mit Rotation
  12.09.03 wl  TRackStruct,TSlotGroupStruct TN1581.7 ohne H_Rotation
  17.09.03 wl  TSlotGroupStruct             TN1526   ungenutzte Parameter entfernt
  17.09.03 wl  RACKDATA                     TN1526   Zusatz "_ZP01" entfernt, da von beiden Plattformen genutzt
  17.09.03 wl  THandlerTip                  TN1526   --> SamGlobe
  19.09.03 wl  TDeviceType                  TN1598   neu: dptShakerIKAOld
  25.09.03 wl                               TN1580   const-Werte für Liquid Handling aktiviert
  29.09.03 wl  TRackStruct                  TN1501.4 neu: SlopeType und Z_LastPos_mm
  15.10.03 wl  TValvePos                    TN1624   --> InterfaceComm.pas
  15.10.03 wl  dptPipPump                   TN1624   neuer Devicetyp
  23.10.03 wl  SLOTGROUPDATA                TN1631   neu: XPreStartOfs
  23.10.03 wl  TSlotGroupStruct             TN1631   neu: H_XPreStart_mm
  06.11.03 pk  TEventFuncExt,TcEventFuncExt TN1649   GlobalErrPtr no longer passed into functions
  12.11.03 wl  TPipStep                     TN1659.1 entspricht PIP_STEP_IndividualY mit X- und YOffset -> kann allgemein verwendet werden
  14.11.03 wl  TSlotGroupStruct             TN1664   um Rotation-Werte erweitert
  18.11.03 wl  TSlotGroupStruct             TN1667   neu: H_RStart_degree
  18.11.03 wl  TMoveTubeData                TN1667   --> TubeHandling.pas
  01.12.03 wl  ACTION_TYPE                  TN1656.5 bereinigt
  10.12.03 wl  TDeviceType                  TN1672   new: dptPipMotor, dptGrpMotor, dptPipArm, dptGrpArm
  10.12.03 wl  PIPStepArray_WithVarispan    TN1672   entfernt
  10.12.03 wl  SLOTGROUPDATA                TN1672   hRReTakeOfs entfernt, neu: SlotNr
  15.12.03 wl  TSlotGroupType               TN1672   neu: für Carrier-Typen (Corridor, ..)
  17.12.03 wl  TDeviceType                  TN1672   new: dptVYPipMotor
  18.12.03 wl  TArmMotorType                TN1672   neu zur Kennzeichnung von Motoren
  18.12.03 wl  ACTION_TYPE                  TN1672   --> InterfaceComm
  19.12.03 wl  TDeviceType                  TN1672   entfernt: dptVYPipMotor
  20.01.03 wl  STR_ZP01_VMOTOR/YMOTOR       TN1672   entfernt
  22.01.04 wl  STR_REVERSE_XMOTOR_NAME      TN1672   Name als const
  04.02.04 pk  TAuftRec                     TN1719   Changed for TAction
  05.02.04 wl                               TN1734   Sampler errors: Fehlermeldungen aufgeräumt
  12.02.04 mo                               TN1743   Max_Racks auf 600 erweitert
  19.02.04 mo  TRackStruct                  TN1754   Neue Werte für Tube Greifen
  03.03.04 wl  TAuftRec                     TN1786   Beschränkung bei Option auf 120 Zeichen aufgehoben
  08.03.04 pk  EStartupInvalid              TN1646   --> RoboticInterface
  11.03.04 mo  TTubeOption                  TN1807  neu: optUseRackidPos
  12.03.04 wl  RACKDATA                     TN1812   nicht benötigte Teile enfernt
  12.03.04 wl  INT_LAST_MOTOR_POS           TN1812   entfernt: durch Änderungen bei MoveXY-Funktion nicht mehr möglich
  16.03.04 wl  TLiqHandlingRec              TN1708   10 neue Felder in Liquid paramters (ScanMode,ScanSpeed,RetractSpeed)
  18.03.04 wl  TArmMotorType                TN1765.1 neu: amtCombinedZMotor: im Movement-Panel wird ZP01-HZ-Motor dem Pipettierarm zugerechenet!
  02.04.04 wl  SLOTGROUPDATA                TN1788   Plate..-Werte aus RACKDATA übernommen
  02.04.04 wl  RACKDATA                     TN1788   auf Z-Positionen reduziert
  02.04.04 wl  LIQPAR_MIXREC                TN1788   ZOffset: Steps-Wert in mm geändert
  19.04.04 wl  LQ_PIP_ISV0L                 TN1788   vereinheitlicht für ZP01 und ZP02
  19.04.04 wl  TipWordArray                 TN1788   abgeschafft: ist jetzt immer TipIntegerArray
  19.04.04 wl  TMinimalRack                 TN1788   entfernt (ist nicht mehr nötig)
  19.04.04 wl  TTipPositions                TN1788   --> RackTypes
  19.04.04 wl  TAuftRec                     TN1788   Tip heißt jetzt TotalTip (im Gegensatz zu ArmTip)
  27.04.04 pk  TImportOptionsRec            TN1880   New : Import Method Option
  27.04.04 wl  TPipStep                     TN1881   XOffset,YOffset umbenannt: X-,YAspShift
  27.04.04 wl  RACKTUBEDATA                 TN1881   XOffset_ZP01,YOffset_ZP01,NumLiquid entfernt
  28.04.04 wl                               TN1788   unbenutzte typen entfernt
  05.05.04 pk  TCoordType                   TN1901   Type of coordinate for a rack position
  05.05.04 wl  TTubeStruct                  TN1788   entfernt
  05.05.04 wl  TMoveTubeData                TN1788   von TubeHandlingLow hierher
  10.05.04 pk                               TN1889   New Constants: INT_RACKPOS_RACKNAME_IS_TUBEID, etc..
  15.05.04 pk                               TN1889   New constants: STR_RACKNAME_SYSTEM, STR_RACKNAME_MANUAL, etc.
  07.06.04 wl  dptEntireGripper             TN1963   neue Bezeichnung für Roboterarm wie z.B. von CRS
  08.06.04 wl  SLOTGROUPDATA,TMoveTubeData  TN1963   --> DevicesXBasedMotorSystem
  08.06.04 pk  TImportOptionsRec            TN1976   --> MethodGUIParsing
  09.06.04 pk                               TN1978   STR_RACKNAME_SYSTEM, STR_RACKNAME_MANUAL --> MethodGUIParsing
  15.06.04 pk  TStringArray                 TN1980.0 new
  17.06.04 wl  TTubeOption                  TN1981   new: optTubeDecapToCapPark (1024)
  17.06.04 wl  TLiqHandlingRec              TN1975   DispRetrSpeed und WashRetractSpeed entfernt
  01.07.04 wl  TPipStep                     TN1963   neu: mit XY-Move-Offset
  05.07.04 pk  TTubeOption                  TN2018   Documentation of TTubeOption from PosTools
  07.07.04 pk  ARR_POWER_OF_TWO             TN2018   used to parse bitmap options
  21.07.04 pk  TLiqParWashRec               TN2049   New: Wash-related fields from TLiqHandlingRec
  21.07.04 pk  TAuftRec                     TN2049   moved to TRunDataAdaptor
  21.07.04 pk  TSchedDatasourceType         TN2049   New: is the schedule based on a script or a method
  30.07.04 pk  MAX_CARRIER, MAX_RACKS       TN2068   Removed
  04.08.04 pk  TRackOption                  TN2080   New
  11.09.04 wl  TPipZPosition                TN2123   neuer Name für RACKDATA
  11.09.04 wl  RACKTUBEDATA                 TN2123   enthält jetzt auch Z:TPipZPosition
  30.09.04 wl  TDeviceType                  TN2157   Benennung einheitlich mit dpt am Anfang und Cmp für Composite
  30.09.04 wl  TDeviceType                  TN2157.1 neu: dptStateSignal,dptStateSwitch
  30.09.04 wl  TSystemState                 TN2157.1 von ObjSystemState hierher
  13.10.04 wl  TLiqHandlingRec              TN2151   neu: SampleAsp-,DilAsp-,DispRetractDisance, DispRetractSpeed
  13.10.04 wl  TSwitchPlace                 TN2151   neu: für SwitchModulePort (Liquid/Powder Handling)
  14.10.04 wl  INT_LQMODES_NO_CALCULATION   TN2178   neu: wenn trotz Rack-ID nicht berechnet werden soll
  27.10.04 wl  TSlotGroupStruct             TN2071   SlotDirection: TRotationValue statt SlotRotation_degree
  27.10.04 wl  TRackStruct                  TN2071   H_RTake_degree statt H_Direction
  27.10.04 wl  TSlotTurnType                TN2071   neu: für TSlotGroupStruct.SlotTurnType enum statt integer
  04.11.04 wl  TEnAction                    TN2213   von ActionLow hierher
  04.11.04 wl  TSlotStructStr               TN2213   von postools hierher
  04.11.04 wl  const STR_EV_...             TN2213   von SamplerEvent hierher
  04.11.04 wl  TTubeParameters              TN2213   ersetzt durch TTubeActionRunRec (MethodTypes)
  08.11.04 wl  const STR_EV_...             TN2213   --> MethodGUIParsing
  08.11.04 wl  TRackIDPosition              TN2213   RecCnt entfernt
  22.11.04 pk  TEnAction                    TN2237   New : acWashTips 22.11.04 pk  TipsNeedingWash                  TN2237   New param : IgnoreWashFlag
  08.12.04 wl  TTubeOption                  TN2258   New option: optTubeJustDecap
  14.12.04 wl                               TN2265   New option 4096 (optTubeNoPosinfoChange)
  19.01.05 pk                               TN2281.0  TStringArray, TIntArray from AppTypes to GeneralTypes
  25.01.05 tbh TRackStruct                  TN2293    New: BlockBalanceDoor
  31.01.05 wl  TLiqParWashRec               TN2297.3 neu: DryAfterWash
  15.02.05 wl  TipStringArray               TN2269   neu
  18.02.05 pk  TCallDevFuncResultType       TN2322   For TDevice.CallFunc
  28.02.05 pk  TEnAction                    TN2314.1 acGroup
  04.03.05 pk  TLiqHandlingRec              TN2330.1 ---> TLiqHDataAdaptor
  08.03.05 pk  TDllCallStruct               TN2337   New Field : DLLType
  11.03.05 wl  TSlotGroupStruct             TN2342   neue Werte für Tube-Bewegungen
  29.03.05 pk  Log class constants          TN2364   Add 00 to each LogType as Lowbyte
  04.05.05 wl  wtVirtual                    TN2410   entfernt
  25.05.05 wl  TFlushThrChecks              TN2427   --> DlgFlush
  07.06.05 pk  TRunCreateParserType         TN2449  New
  15.06.05 pk  TWorkType                    TN2464.2 wtCalli added
  24.06.05 wl  acMessageWithBeeper          TN2459   entfernt
  11.07.05 wl  TFuncParam,TEventFunc,..     TN2498.1 --> DllLoading
  25.08.05 wl  TSchedDataSourceType         TN2558.8  entfernt
  07.09.05 wl                               TN2585   TLoginModes -> ExternDllFunc
  08.09.05 wl  TSamplerThreadMode           TN2585   mdStartedFromDesigner: soll am Schluß des Laufs wieder zum Designer zurückkehren
  23.09.05 wl  TCallDllStruct               TN2627   neu: StoreKeyName
  10.10.05 wl  STR_DDE_STATUS_..            TN2637.2 neue Konstanten für DDEStatus
  11.10.05 wl  acGripperToolGet             TN2658  new action that gets a rack with the gripper as tool
  15.10.05 wl  STR_RACKNAME_..              TN2672   --> RackTypes
  20.10.05 wl  TEnAction                    TN2659   neu: acTipsGet
  20.10.05 wl  TEnAction                    TN2660   neu: acTipsReturn
  08.11.05 wl  TSwitchState                 TN2435   von DevicesOther hierher
  08.11.05 wl  TWashprogRec                 TN2745   --> WashprogDataAdaptor
  08.11.05 wl                               TN2745   Script-bezogene Typen entfernt
  09.11.05 wl  TPipStepArray                TN2728   --> RackTypes
  18.11.05 wl  TSwitchState                 TN2780   neu: swsBothOn
  20.11.05 wl  acChangeTips                 TN2784   für TChangeTipsAction
  22.12.05 pk  dptCmpUserProtect            TN2875   new for UserProtectionDevice
  18.01.06 wl  STR_RUNVAR_VALUE_EMPTY       TN2885   SLSQL schreibt 'EMPTY' wenn kein Datensatz gefunden wurde
  18.01.06 wl  acSQLSelect                  TN2885   neu für SLSQL
  25.03.06 pk  TEnAction                    TN2997   New acInitDevice
  31.03.06 pk  TDeviceType                  TN2958   removed dptGripMotor/PipMotor
  31.03.06 pk  TDeviceType                  TN3009   New types for PneumaticGripper
  18.04.06 pk  TDeviceType                  TN2958   New type for Removable Pipette device
  19.04.06 wl  TipFloatArray                TN3051  ersetzt TipSingleArray; = array[0..MAX_TIPS-1] of extended;
  18.07.06 pk  TTubeOption                  TN3205  new 8192 for entering  barcode manually
  24.08.06 wl  TRackStruct                  TN3269   neu: StackHeightZ_mm
  24.08.06 wl  TSlotGroupStruct             TN3269   neu: SlotZ_Calculate
  24.08.06 wl  TSlotGroupStruct             TN3269.1 korrigiert: SlotZ_Last_mm -> SlotZ_ReallyFirst_mm, SlotZ_First_mm -> SlotZ_ReallyLast_mm
  07.09.06 wl  TWorkType                    TN3288   wtCalli,wtSimpleCalli entfernt
  15.09.06 pk  TRackStruct                  TN3073   New: H_VIsUndefined
  20.09.06 wl  TEnAction                    TN3318   neu: enMotorMove
  27.10.06 pk  TTubeOption                  TN3386   New: tube option optTubeMoveToDestDuringBC
  06.11.06 wl  TEnAction                    TN3394   neu: enSensorCheck
  04.12.06 wl                               TN3445   viele CONST-Werte gelöscht
  03.01.07 pk  TSystemState                 TN3479   New member: sStateUnknown
  26.01.07 pk  TDeviceType                  TN3503   dptLocationAllAxes replaces dptEntireGripper
  31.01.07 wl  TSlotGroupStruct             TN3532   entfernt -> s. Carrier.pas, TTubeMoveStruct,TRackMoveStruct
  19.02.07 wl  TEnAction                    TN3585   neu: acVirtualRackMove
  20.02.07 wl  TRunCreateParserType         TN3016   entfernt
  02.03.07 pk  TDeviceType                  TN3613   Removed dptRediMotor, Added dptcmpRedi
  07.03.07 wl  TDeviceType                  TN3620   Removed dptGripper, Added  dptPeriPump
  12.03.07 pk  TRackMoveOptions             TN3629   New
  12.03.07 pk  TDeviceType                  TN3631   New: dptSimpleWatchDog
  18.04.07 wl  INT_MIXMODES_..              TN3658   Mix-Modes statt Mix-Methoden
  23.07.07 pk  TDeviceType                  TN3726   New: dptPHMeter, dptThermometer
  23.07.07 pk  TDeviceType                  TN3796   New: dptCmpErrorLink
  18.09.07 pk  TDilRackType                 TN3864   Moved here from RackTypes
  09.11.07 pk  TPipZPositions               TN3924   MPosArray changed to TPosMMArray
  09.01.08 wl  TEnAction                    TN3972   entfernt
  09.01.08 wl  TDilRackType                 TN3972   drManual entfernt
  09.01.08 wl  TWorkType                    TN3972   kann nur noch Liquid oder Powder sein
  29.01.08 wl  TSwitchState                 TN3980   --> ISwitchDriver
  04.04.08 wl  TZTipMoveErrorMessage        TN4057   new for ZTIPM action
  14.04.08 wl  LOG_...                      TN4060   --> LogManager
  24.04.08 wl  TPumpAspirateType            TN4050    neu
  24.06.08 wl  TDeviceType                  TN4143   entfernt
  03.07.08 wl                               TN4157
  16.07.08 pk  TTipLiqErrorTypeArray        TN4157   New
  25.09.08 wl  TCallDllStruct               TN4242   entfernt
  15.10.08 pk  TKeyValueParam               TN4258   New
  29.10.08 wl  TRackStruct                  TN4290   --> RackDataAdaptor
  13.01.09 wl  TRackPositions               TN4312   ist jetzt dynamisches Array
  16.01.09 wl  TMethodEditorAttribute,TDLLMenuItems  TN4362   vorübergehend hier untergebracht (Ex-ViewItems)
  04.03.09 pk  TKeyValueParamArray          TN4232   --> GeneralTypes
  13.03.09 wl  TMoveTo/FromPositionRoute    TN4460   new
  17.04.09 pk  crCustZoom, etc              TN4532   constants for screen cursors
  21.07.09 pk  INT_LQMODES_USE_DETECTED_VAL TN4667   new
  31.07.09 ts  TInfoGroupBehaviour          TN4666   new (Info-Action setting)
  12.08.09 wl  cToolErrResult..             TN4712   Konstanten für ToolErrDialog
  12.09.09 wl  TipInteger/Float/StringArray TN4740   entfernt: werden durch TDoubleArray,TIntArray und TStringArray ersetzt
  12.09.09 wl  TPipStepZPosArray            TN4740   ist jetzt dynamisch
  04.11.09 pk                               TN4843 Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  08.12.09 ts  TPumpDispenseType            TN4921   new
  15.12.09 ts  TRackOption                  TN4945   new Rackoption: roReadBCManually
  07.05.10 pk  TRunStartOptions             TN5092   Is now a streamable class
  27.07.10 wl  TImport                      TN5123   entfernt
  27.07.10 wl  TExportDefs                  TN5123   weniger Member
  23.08.10 wl                               TN5214   LOLI, NOLI --> PosinfoDataAdaptor
  22.03.11 wl  TArmMotorType                TN5401   entfernt
  03.11.11 wl  TTubeColors                  TN5725   Tube-Standardfarben
  03.11.11 wl  TSubstIDAndVol               TN5725   für TLiqInfo: Enthält einen Volumen-Anteil
  21.11.11 wl  TRackPositionWithVol         TN5730   neu für Volumenkontrolle
  12.12.11 wl  TMultiPipStrategy            TN5764   von PipetteTypes hierher
  02.02.11 wl  TInternMovementData          TN5791   neu zum Merken der zuletzt angefharenen Racks/Carrier
  01.03.12 wl  TRunStartOptions             TN5822   --> MethodTypes
  20.04.12 wl  TRunMainState                TN5858   neu
  25.04.12 wl  TMultiPipStrategy            TN5878   --> BasicPipetteTypes
  10.05.12 wl  TPumpDispenseType            TN5893   wieder abgeschafft
  29.05.12 wl  TRunMainState                TN5894   neu: rmsRequestState
  25.07.12 ts  TExportDefs                  TN5944   EndOfLineChar for each line can be set in the settings (CR+LF)
  07.08.12 wl                               TN5946   einige überflüssige Konstanten entfernt
  26.11.12 wl  INT_MIX_MODES...             TN6027   --> LiqHTypes
  11.02.13 wl  TRackIDPositionWithVol       TN6078   jetzt mit SubstID
  15.02.13 wl  TAspirateType,TDispenseType  TN5914   neu
  23.05.13 wl  TInternMovementData          TN6153   entfernt
  15.08.13 wl                               TN6217   cStandardTipName entfernt
  30.08.13 wl  TVisibilityAttribute         TN6236   allgemein formuliert statt nur auf Method bezogen
  27.11.13 wl  RTF_SINGLE_TUBE              TN6313   entfernt
  -------------------------------------------------------------------------------------------------- }

unit AppTypes;


interface


uses
    GeneralTypes,
    CommonTypes;

const
    // cursors in SamintfBitmap.res
    crCustInfo = 5;
    crCustZoom = 6;
    crCustOpenHand = 7;
    crCustCloseHand = 8;
    crCustMagnifyGlass = 9;

    ERR_NONE = $0000; // No error

    // gängige Fehlertypen (werden nicht ausgewertet - es ist also völlig egal, was man nimmt)
    ERR_MODULE = $1300;
    ERR_APPLICATION = $2100;
    ERR_USER = $FF00;

    SIZE_SLOT_NAME = 19;
    SIZE_RACK_ID = 19;
    SIZE_RACK_NAME = 39;

    // Liquid and Clot detector Modes
    INT_LQMODES_LQ_SCAN = $0001; // Scan for Liquid position default = 1
    INT_LQMODES_LQ_DISP_ERROR = $0002; // Display Liquid detection Error
    INT_LQMODES_LQ_ERROR_GOTO = $0004; // Error recover: GotoZMax else Skip
    INT_LQMODES_TRACKING = $0008; // use tracking
    INT_LQMODES_LQ_CHECK_EMPTY = $0010; // Liquid detection for empty check
    INT_LQMODES_LQ_SINGLE_TIP = $0020; // Liquid detection one tip after the other (single detection)
    INT_LQMODES_LQ_PIP_ISVOL = $0080; // Pipet detected volume  // noch nicht implementiert
    INT_LQMODES_CD_ON = $0100; // Use Clot Detection
    // INT_LQMODES_CD_DISP_ERROR	   = $0200;	    // Display Liquid detection Error (not used)
    // INT_LQMODES_CD_ERROR_MODE	   = $0400;	    // Recover Mode: (not used)
    // INT_LQMODES_CD_RETRACT	       = $0800;	    // Display Liquid detection Error
    INT_LQMODES_NO_CALCULATION = $1000; // No volume calculation (even if rack ID is set)
    INT_LQMODES_USE_DETECTED_VAL = $2000;
    // Use a liquid detection value that was previously stored for a certain rack and position

    // Definition for Adress Flag Bits CAdress.Flags
    // AF_EXECUTE          =   $01;  // Module has to be executed
    AF_ERROR = $04; // Module got an error at last Exec.
    // AF_WARN	      =	  $08;	// Module got an recovered Error
    // AF_NOT_INIT         =   $20;  // Module is not initialized
    // AF_DETECTED	      =	  $40;	// Detection Flag is on

    // Definition for Module Flag Bits CAdress.Flags
    // AC_NO_WAIT	      =   $01;	// Do not wait for this action when active
    // AC_NO_QUERY	      =	  $02;	// Do not Query Module State when active
    AC_ERR_DIS = $04; // Disable Error Handling for this action

    // Diti Options
    DT_CHECK_BEFORE = $1;
    DT_DISP_BEFORE = $2;
    DT_DISP_ERROR = $4;
    DT_CHECK_AFTER = $8;
    DT_DISP_AFTER = $10;
    DT_REMOVE_LIQ = $20;

    GT_DISP_ERROR = $4;
    GT_SKIP_ERROR = $100;
    GT_RETRY_ERROR = $200;
    GT_CHECK_AFTER = $8;
    GT_DISP_AFTER = $10;
    GT_SKIP_AFTER = $400;
    GT_RETRY_AFTER = $800;
    GT_SINGLE_TIP = $1000;

    cPosinfoWeight = -899;
    cPosinfoNetWeight = -900;

    cBCError = 'BCERROR';
    cBCNoRack = 'NOCODE';
    cBCRandom = 'RANDOMBC';

    STR_DDE_STATUS_RUNNING = 'Running';
    STR_DDE_STATUS_STOPPED = 'StopButtonPressed';
    STR_DDE_STATUS_READY = 'Ready';
    STR_DDE_STATUS_ERROR = 'Error';
    STR_DDE_STATUS_DONE = 'Method done';

    cToolErrResultCancel = 0;
    cToolErrResultBringBack = 1;
    cToolErrResultOpenGripper = 2;

type
    // --------------------------------------------------------------------------------------------------
    // ---- SAMPLER.DLL C++ und Delphi - Typen ----------------------------------------------------------
    // --------------------------------------------------------------------------------------------------

    TIPMAP = integer;

    EXEC_MODE = (m_NO_EXEC, m_EXECUTE);

    TPipZPositions = record // Absolute Z-Positionen
        // ----------------------------------------------------------- Beschreibung der Aktionspositionen
        ZTravel: TDoubleArray; // Z-Travel	(absolut aus Racktyp und aktuellem Slot)
        ZScan: TDoubleArray; // Z-Scan	(absolut aus Racktyp und aktuellem Slot)
        ZDisp: TDoubleArray; // Z-Disp	(absolut aus Racktyp und aktuellem Slot)
        ZMax: TDoubleArray; // Z-Max	(absolut aus Racktyp und aktuellem Slot)
        ZTube: TDoubleArray; // Tube-Höhe    (absolut)
        // ---------------------------------------------------------------------------- Move-Offset Daten
        MOffset: TDoubleArray; // Z-Move-Offsetposition (absolut aus Racktyp und aktuellem Slot)
    end;

    TPipStepZPos = record
        SyrMap: TIPMAP;
        Z: TPipZPositions;
    end;

    TPipStepZPosArray = array of TPipStepZPos;

    TMotorMapArray = array of TIPMAP;

    AinfoAction = record
        New: SmallInt; // New Position
    end;

    AINFO = record
        QReq: Byte; // Last Answer from Query Request
        EReq: Word; // Last Answer from Error Request
        Error: Word; // Last Error Id
        Action: AinfoAction; // Actual Action
    end;

    TTipLiqErrorType = (letOK, letLowLiquid, letNoLiquid);
    TTipLiqErrorTypeArray = array of TTipLiqErrorType;

    // Rack Related records and types
const
    STR_MATRIXCOORD_OFFSET_DELIMITER = '_';
    STR_RACKPOS_MIN_ALLOWED = '1';
    INT_RACKPOS_RACKNAME_IS_TUBEID = -1;

type
    TRotationValue = (rotation_0, rotation_90, rotation_180, rotation_270);

    TCoordType = (ctNone, ctMatrix, ctInteger);

const
    STR_CARRIER_SAME = '_CARRIER_SAME_';

type

    TSlotStruct = record
        CarrierName: string;
        SlotNr: integer;
        Rotation: TRotationValue;
    end;

    TLayoutStruct = record
        Run: string;
        name: string;
        _RackName: string;
        CarrierName: string;
        Slot: integer;
        RackID: string;
        RackTyp: string;
        Rotation: integer;
    end;

    TintPtr = ^integer;

    TSchedProperty = (ufpParallelAct, // Paralleler Thread
        ufpRackBased, // Action bezieht sich nur auf ein Rack (Reaction Block)
        ufpComment, // Kommentar
        ufpFirstInit, // festes Init am Anfang
        ufpLastInit, // festes Init am Ende
        ufpAllRacks // Kann sich auf alle Racks beziehen, deshalb müssen alle Threads gestoppt werden
        );
    TSchedProperties = set of TSchedProperty;

    TWBBorder = record
        Top: integer;
        Left: integer;
        Right: integer;
        Bottom: integer;
    end;

    TMaxNameLen = record
        RunName, LayoutName, RackName, RackType, RackID, CarrierName, CarrierType: integer;
    end;

    TWorkType = (wtLiquid, wtPowder);

    TRackPosition = record
        Rack: string;
        Pos: integer;
    end;

    TRackPositions = array of TRackPosition;

    TRackPositionWithVol = record
        Rack: string;
        Pos: integer;
        Vol: extended;
    end;

    TRackIDPosition = record
        Rack: string;
        RackID: string;
        Pos: integer;
    end;

    TRackIDPositionWithVol = record
        Rack: string;
        RackID: string;
        Pos: integer;
        Vol: extended;
        SubstID: string;
    end;

    TExportDefs = record
        Numbering: boolean;
        DateTimeStamp: boolean;
        DateTimeStampStr: string;
        Separator: string;
        SeprAtEndOfLine: boolean;
        Path: string;
        Extension: string;
        xlsDelCSV: boolean;
        EndOfLineChar: string;
    end;

const
    // this is a general array of power of two, and NOT just used for Tube Options
    ARR_POWER_OF_TWO: array [0 .. 15] of integer = (0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048,
        4096, 8192, 16384);

const
    INT_NUM_TUBEOPTIONS = 16;
    INT_NUM_RACKOPTIONS = 2;

type
    TTubeOption = (optUseRackidPos, { -1 }
        optTubeShaking, { 1 }     // Tubes werden geschüttelt
        optTubeNotReadIntoWaste, { 2 }     // nicht gelesene Tubes wandern in den Waste
        optTubeNoDoubleWeighing, { 4 }     // Tubes werden kein zweites Mal gewogen
        optTubeSafeMovement, { 8 }     // Tubes werden im SafeMove-Modus bewegt
        optTubeDoNotWait, { 16 }    // Sleeps nach dem Aufnehmen und Ablegen der Tubes werden weggelassen
        optTubeDecapToWaste, { 32 }
        // Decapping beim/nach Barcodelesen und (wenn vorhanden) Deckel in Waste bringen
        optTubePutOnNewCap, { 64 }
        // Nach dem Lesen & Wiegen einen neuen Deckel aufsetzen (aus Cap-Park-Rack, wenn vorhanden)
        optTubeDecapAndPutBack, { 128 }
        // Decapping (Kappe auf CapPosition lassen), danach Wiegen, Cap zurück auf Tube
        optTubeNoCapSensor, { 256 }   // vorhandener Sensor zum Prüfen des Decappers wird nicht benutzt
        optTubeNoTubeSensor, { 512 }   // vorhandener Sensor zum Prüfen des Tube-Transports wird nicht benutzt
        optTubeDecapToCapPark, { 1024 }
        // Decapping und Kappe auf nächste Position eines Cap-Park-Racks bringen (statt in Cap-Waste)
        optTubeJustDecap, { 2048 }  // Decapping und Kappe im Greifer bzw. auf der CapPosition belassen
        optTubeNoPosinfoChange, { 4096 }  // Tube infos (Barcode, Weight, ..) are not moved in the Posinfo.db
        optTubeManualBarcode, { 8192 }  // Enter barcode manually, even if a barcode reader is available
        optTubeMoveToDestDuringBC { 16384 }
        // Move to decapper, balance, or to destination during barcode reading
        );
    TTubeOptions = set of TTubeOption;

    // --------------------------------------------------------------- CalliW Types
    TCWstring = string[40];

    TCWParamSet = record
        name: TCWstring;
    end;

    TCWGetToolResult = record
        Tool: TCWstring;
        Step: integer;
        StepArray: array of integer;
        ParamSet: TCWParamSet;
        DoWeigh: boolean;
        CorrectVol: Double;
        CorrectVolArray: array of Double;
    end;

    // --------------------------------------------------------------------------- Sampler-Thread-Modes
    TSamplerThreadMode = (mdAlwaysRestart, mdCreateWorkedUpRun, // für RunStart
        mdInitFirst, mdInitAtEnd, // für Execute
        mdDeleteAfterRun, // für ExecLast
        mdNoDDEReady, // für Schedule: kein 'Ready' am Schluß des Threads senden
        mdNoMessages, // beinhaltet: AskCreateWorkedUpRun, AskRacksWellPlaced, DoneFormAfterRun
        mdUpdateBasicLayout, // Layout übernimmt Änderungen des RunLayouts
        mdAdministrator, // For Zinsser internal testing purposes
        mdStartedFromDesigner); // soll am Schluß des Laufs wieder zum Designer zurückkehren

    TSamplerThreadModes = set of TSamplerThreadMode;

    TOrderItemType = (oitNoItem, oitMethod);

    // ----------------------------------------------------------------------------- Read-Barcode-Modes
    TReadBarcodeMode = (mdShowErrMsg, mdInsertOnly, mdExternBCReader, mdNoRackMove, mdCheckPlate, mdCheckBC);
    TReadBarcodeModes = set of TReadBarcodeMode;

    TRackOption = (roAllowNonUniqueBC, { 0 }
        roReadBCManually); { 1 }
    TRackOptions = set of TRackOption;

    TLayoutPropertyKind = (lpkRack, lpkCarrier, lpkWorkbench);
    TLayoutProperty = integer;

    TSwitchPlace = (spDoNotSwitch, spSwitchInside, spSwitchOutside);

    TSlotStructStr = record
        Carrier, Slot, Rotation: string;
    end;

    TTubeActionMode = (mdtInsertBC, // zeige Eingabedialog, wenn Tube-ID nicht gelesen werden konnte
        mdtReadBC, // Barcode wird gelesen
        mdtWeighTare, // Tara des Tube wird (nach dem Lesen) gewogen
        mdtFreeBalanceOnly); // nur leztes Tube von Waage nehmen
    TTubeActionModes = set of TTubeActionMode;

    TDatasourceSchedStatus = (skUnknown, skNormal, skSchedulable, skScheduled, skLive);

    TDDEStatus = (ddeRunning, ddeStopButton, ddeReady, ddeError, ddeMethodDone);

    TRackZPositionType = (zptOther, zptGlobalZTravel, zptRackZTravel, zptZScan, zptZDispense, zptZMaximum,
        zptCalculated, zptZMoveOffset);

    TMoveXYMovementOption = (xyoUseCorridor, xyoNeverUseRackZTravel);
    TMoveXYMovementOptions = set of TMoveXYMovementOption;

    TRackMoveOptions = record
        GetMoveToDoXBeforeZ, GetMoveFromDoXBeforeZ, PutMoveToDoXBeforeZ, PutMoveFromDoXBeforeZ: boolean;
    end;

    TDilRackType = (drSystem, drNormal);

    TZTipMoveErrorMessage = (ztmErrorOnResistance, ztmErrorOnNoResistance);

    TPumpAspirateType = (patSample, patSystem, patAir);

    TVisibilityAttribute = (meaDefault, meaReadOnly, meaHidden);

    TDLLMenuItems = array of TDLLMenuItem;

    TMoveToPositionRoute = (mtpComplete, mtpSkipStartPos, mtpToStartPosOnly, mtpFromStartPosToDestPos);
    TMoveFromPositionRoute = (mfpComplete, mfpSkipStartPos, mfpToStartPosOnly, mfpFromStartPosToApproach);

    TInfoGroupBehaviour = (igbShow, igbHide, igbNoChange);

    TTubeColors = record
        Aspirate: integer;
        Dispense: integer;
        TubeMove: integer;
        Other: integer;
    end;

    TSubstIDAndVol = record
        SubstID: string;
        Volume: extended;
    end;

    TRunMainState = (rmsReady, rmsRunning, rmsPaused, rmsRequestState);

    TAspirateType = (asptLiquid, asptPowder, asptSystemLiquid, asptSystemRackLiquid, asptAir);
    TDispenseType = (dsptLiquid, dsptPowder);


implementation


end.
