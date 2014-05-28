{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : TWinlissyIniAccess: Application-specific Wrapper for ini file access,
  including WinLissy-specific ini file information.
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.09.02 wl                               TN1293.2 initial version
  09.10.02 wl  TWashrecEntry.Create         TN1293.2 Last string (InitStr) is read with delimiters
  TBarcoderecEntry.Create      TN1293.2 Last string (InitStr) is read with delimiters
  10.10.02 wl                               TN1293.2 IniEntry-Derivates --> WinLissyIniEntry
  11.10.02 wl                               TN1293.2 viele neue Werte aus machine.ini
  15.10.02 wl                               TN1293.1 neu Section 'TipTypes'
  16.10.02 wl  ReadTipType                  TN1293.1 neu
  18.10.02 wl                               TN1293.1 viele neue Einträge
  22.10.02 wl  Read-,WriteControlBounds     TN1293.1 neue Methoden
  23.10.02 wl                               TN1293.1 viele neue Einträge
  25.10.02 wl                               TN1293.1 diverse Änderungen
  06.11.02 wl  CreateFreeType               TN1293.1 'XYZStepsPerMillimeter','DistY_H_Tip1' = Integer
  06.11.02 wl                               TN1293.1 uses AppSettings
  06.11.02 wl  CreateFreeType               TN1293.1 {$ifdef .. durch TAppSettings... ersetzt
  07.11.02 wl  ReadControlBounds            TN1293.1 exit if there is no stored value
  14.11.02 wl  CreateFreeType               TN1293.1 zusätzliche Einträge aus Sampler.dll hinzugefügt
  22.11.02 wl  CreateFreeType               TN1293.4 neu: Section [TubeToolData]
  26.11.02 wl  CreateFreeType               TN1293.1 'XYZStepsPerMillimeter','DistY_H_Tip1' entfernt
  16.12.02 wl  CreateFreeType               TN1371   Section 'MethodParameters' hinzugefügt
  28.12.02 wl  CreateFreeType               TN1394   'WritePosinfo' ->  Default = true
  20.12.02 wl  Create, CreateFileAccess     TN1293.5 geänderte constructors, in Abhängigkeit von TIniAccess
  20.12.02 wl  Read-,WriteControlBounds     TN1293.5 Zugriff auf AppSettings entfernt
  20.12.02 wl                               TN1293.5 alle const values --> CommonTypes
  30.12.02 wl  CreateAllowedEntries         TN1293.1 RelayPort5,MaxPorts,DilutorType --> Section:ZP01Cfg
  30.12.02 wl                               TN1293.5 jetzt auch von IIniAccess abgeleitet
  04.01.03 wl  Create                       TN1334.1 mit 'LastUserName'
  15.01.03 wl  CreateAllowedEntries         TN1295.5 drei mal für die bekannten Ini-Dateien
  17.01.03 wl                               TN1293.5 zum Teil Ressource-Nummern geändert
  22.01.03 tbh                              TN1193 neu: [Redi] ShakeTimeAdjustable = Boolean
  29.01.03 hn                               TN1429 einige neue Beschreibungen, Defaults f. WeighingWizard geändert
  30.01.03 wl                               TN1429 Default-Werte wiederhergestellt (DatabaseToolDll, WeighLogicDbName)
  31.01.03 wl  ReadControlBounds            TN1416 Fehler beseitigt: BoundsRect statt SetBounds
  12.02.03 wl  Read-,WriteControlBounds     TN1293.5 --> LocalIniFile
  12.02.03 wl  TZPAppIniAccess.CreateAllowedEntries TN1293.5 LastUserName entfernt
  03.03.03 wl  TZP01CfgIniAccess.CreateAllowedEntries  TN1345 neu: BarXMot, BarYMot, BarYScan
  22.11.02 wl  TZPRobotIniAccess.CreateAllowedEntries  TN1293.4 bei TubeToolData jetzt richtiger Section-Name
  07.03.03 tbh TZPRobotIniAccess.CreateAllowedEntries  TN1443 neu: ShakeHeight, ShakeZOffset, ShakeYOffset
  11.03.03 wl  TWinlissyIniAccess.ReadLiquidPortData   TN1293.5 neue Funktion
  13.03.03 hn                               TN1293.3 Einheiten bearbeitet
  19.03.03 wl  TZPAppIniAccess.CreateAllowedEntries    TN1332.3 neu ('Documentation','ZipArchivePath'...)
  26.03.03 wl  TZPAppIniAccess.CreateAllowedEntries    TN1439.5 neu: 'Backup', 'Cycle' und 'Path'
  31.03.03 mo  TZPAppIniAccess.CreateAllowedEntries    TN1383.8 neu: [UserProtection] Active und DllName
  31.03.03 mo  TZPAppIniAccess.CreateAllowedEntries    TN1383.8  UserProtection nach TZPRobotIniAccess verschoben
  03.04.03 tbh TZPRobotIniAccess.CreateAllowedEntries  TN1383.7 neu: [StateMonitor] DllName
  03.04.03 tbh TZPRobotIniAccess.CreateAllowedEntries  TN1383.6 [StateMonitor] ComPort, StateActivePort,
  03.04.03 tbh TZPRobotIniAccess.CreateAllowedEntries  TN1383.6 [StateMonitor] StateSetupPort, StateErrorPort
  11.04.03 tbh TZPRobotIniAccess.CreateAllowedEntries  TN1383.6 [StateMonitor] AddActivePort
  11.04.03 tbh TZPRobotIniAccess.CreateAllowedEntries  TN1383.8 [UserProtection] RequestComPort
  16.04.03 tbh TZPDLLIniAccess                         TN1468   neu: für Einträge für Kunden-Dlls
  05.05.03 tbh TZPAppIniAccess.CreateAllowedEntries    TN1482.1 [ScriptReset] Default-Pfad korrigiert
  21.05.03 tbh TZPAppIniAccess.CreateAllowedEntries    TN1487   neu: [Display] ProcessScriptsRAckBased
  13.06.03 wl  TZPRobotIniAccess.CreateAllowedEntries  TN1501.1 Default für ReadE_DLLName = '' (nicht 'STACKER')
  25.07.03 tbh TZPRobotIniAccess.CreateAllowedEntries  TN1448   Defaults für VDrop/ZDrop angepasst
  04.09.03 wl  TZPRobotIniAccess.CreateAllowedEntries  TN1570   neu: 'TUBE_BCR_Filter.' und 'MTP_BCR_CodeFilter'
  18.09.03 wl  TZPAppIniAccess.CreateAllowedEntries    TN1597   24h-Mode als Integer mit dem Range 0..2
  18.09.03 wl  TZPRobotIniAccess.CreateAllowedEntries  TN1526   WiperSpeedX,-Y von "ZP01CONFIG in "ROBOT" verschoben
  18.09.03 wl  TZPRobotIniAccess.CreateAllowedEntries  TN1526   'HZTravel' aus "ROBOT" entfernt (war doppelt definiert)
  25.09.03 wl  TZPRobotIniAccess.CreateAllowedEntries  TN1580   'DispCh1and2together' von "ZP01CONFIG in "ROBOT" verschoben
  07.10.03 wl  TZP01CfgIniAccess.CreateAllowedEntries  TN1526   DroZOfs wurde und wird nie benutzt
  30.10.03 tbh TZPRobotIniAccess.CreateAllowedEntries  TN1635   neu: 'UseCorrectCarrierOffsets'
  12.11.03 wl  TZPRobotIniAccess.CreateAllowedEntries  TN1660   TBcrpositionEntry: Default-Werte entfernt
  02.12.03 wl  TZPAppIniAccess.CreateAllowedEntries    TN1598.1 Sektion 'VORTEXER' wird nicht mehr gebraucht
  02.12.03 wl  TZPRobotIniAccess.CreateAllowedEntries  TN1429   einige neue Einheiten
  17.12.03 wl  TZPRobotIniAccess.CreateAllowedEntries  TN1672   neu: Gripper/pip arm security margin Section: 'Carrier_Crash_Avoidance'
  17.12.03 wl  TWinlissyIniAccess.ReadXYRangeData      TN1672   liest XYRangeData ein
  23.12.03 wl  TZPRobotIniAccess.CreateAllowedEntries  TN1712   neu: 'TubeRetakeRotation'
  23.02.04 pk  TZPAppIniAccess.CreateAllowedEntries    TN1760   neu: 'HideVolumesWindow'
  04.03.04 pk  TZPAppIniAccess.CreateAllowedEntries    TN1789   neu: 'DontPutBackTool'
  08.03.04 pk  TZP01CfgIniAccess.CreateAllowedEntries  TN1647   MaxPorts: default changed from 0 to 13
  02.04.04 wl  TZP01CfgIniAccess.CreateAllowedEntries  TN1788   entfernt: WashPos,WastePos,HomePos,DeepPos#
  28.04.04 wl  TZP02CfgIniAccess.CreateAllowedEntries  TN1788.9 liest primitive Daten aus SiasCfg.ini
  29.04.04 wl  TZPRobotIniAccess.CreateAllowedEntries  TN1887   neu: 'MTP_ShowNoRackButton' - Default: false
  05.05.04 pk  TZPAppIniAccess.CreateAllowedEntries    TN1889.0 New 'RunCreateRackNameRequired' - Default: true
  28.04.04 wl  TZP02CfgIniAccess.CreateAllowedEntries  TN1788.9 Daten korrigiert und erweitert
  14.05.04 pk  TZPRobotIniAccess.CreateAllowedEntries  TN1920   TubeBarcodesAreUnique
  25.05.04 pk  TZPAppIniAccess.CreateAllowedEntries    TN1909.1 STR_ISEC_METHODPARAMS
  07.08.04 pk  TZPAppIniAccess.CreateAllowedEntries    TN1974.1 RunStatusFlagEditMode - Default: 0
  09.06.04 wl  TZPRobotIniAccess.CreateAllowedEntries  TN1915   New: MoveZTravelIfNoXYMove
  17.06.04 wl  TZPAppIniAccess.CreateAllowedEntries    TN1951.1 New: 'Pipetting','FlushWithRediTips'
  17.06.04 wl  TZPAppIniAccess.CreateAllowedEntries    TN1987.1 New: 'Redi','MoveToRealPosBeforeWiper'
  21.07.04 pk  TZPAppIniAccess.CreateAllowedEntries    New: 'Scheduling', 'DatasourceType', 0
  29.07.04 tbh TZPAppIniAccess.CreateAllowedEntries    TN1982   New: SkipDelaysOnSimulation
  05.08.04 wl  TWinlissyIniAccess.ReadAPOS             TN2074   entfernt
  05.08.04 wl  TZP01CfgIniAccess.CreateAllowedEntries  TN2074   entfernt: CheckPos,DropPos,DropXOfs
  18.08.04 wl  TZPRobotIniAccess.CreateAllowedEntries  TN2079   neu: WashAtWasteVol in area ROBOT
  18.08.04 wl  TZP01CfgIniAccess.CreateAllowedEntries  TN2099   entfert: SamTrack,MTP_Reader,Incu1,Incu2,SystemSensor,WasteSensor
  19.08.04 wl  TZPAppIniAccess.CreateAllowedEntries    TN2104   neu: NeverWashIfWashDisabled - wenn WashFlag nicht gesetzt, wird wirklich nicht gewaschan
  16.11.04 pk  TWinlissyIniAccess.CreateAllowedEntries TN2231   New: Run,ArchiveRunSteps
  02.12.04 wl  TZPAppIniAccess.CreateAllowedEntries    TN2254.1 neu: 'SubMethods','AcceptOnlyUniversalLayout'
  04.02.05 wl  TZPAppIniAccess.CreateAllowedEntries    TN2307   doppeltes VolumeControl entfernt
  16.02.05 wl  TZPAppIniAccess.CreateAllowedEntries    TN2269   SystemLiquidValve 'Adr' und 'NoValveRead' entfernt
  23.03.05 pk  TZPRobotIniAccess.CreateAllowedEntries  TN2360   TBarcoderecEntry.Create : pass InitStr resource id as argument
  23.03.05 pk  TZPRobotIniAccess.CreateAllowedEntries  TN2358.1 New : XConflictResolve section
  20.04.05 pk  TZPAppIniAccess.CreateAllowedEntries    TN2395   New : 'Scheduling', 'MinTime', 5
  19.05.05 wl  TZPAppIniAccess.CreateAllowedEntries    TN2414   'CapParkRack','CapParkStrategy'
  20.05.05 wl  TZPAppIniAccess.CreateAllowedEntries    TN2377.2 New: 'Calli','RestAtRWash'
  25.05.05 wl  TZPAppIniAccess.CreateAllowedEntries    TN2427   'Liquids', 'FlushCycles' entfernt
  02.06.05 wl  TZP02CfgIniAccess.CreateAllowedEntries  TN2436   Einträge für 'TipArm2' hinzugefügt
  02.06.05 pk  TZPAppIniAccess.CreateAllowedEntries    TN2449   New : 'Run', 'CreateRunParserType', 1
  08.06.05 wl  TZPRobotIniAccess.CreateAllowedEntries  TN2468   neu: 'Options','ChangeXYAtRotation'
  15.06.05 pk  TZPRobotIniAccess.CreateAllowedEntries  TN2464   New Pip2_ReferenceTip_Distances_Steps','DistX_Pip2Tip1_Tip1',-1,'X-Steps'
  17.06.05 wl  TMethodIniAccess.CreateAllowedEntries   TN2440   Section 'EditorGrid' zum Speichern verschiedener Editor-Ansichten
  20.06.05 wl  TMethodIniAccess.CreateAllowedEntries   TN2441   Section 'Comment', Short und Long
  21.06.05 pk  TZPRobotIniAccess.CreateAllowedEntries  TN2464.3 Removed : Pip2_ReferenceTip_Distances_Steps
  29.06.05 wl  TZPAppIniAccess.CreateAllowedEntries    TN2480   'Documentation','UseTubeIDIfNoSubstIDExists'
  01.08.05 pk  TZPAppIniAccess.CreateAllowedEntries    TN2509   New: 'Run','ShowDialogForDelayLongerThan', 3
  08.08.05 pk  TZPAppIniAccess.CreateAllowedEntries    TN2524   New: 'Pipetting','PipDifferentOptionsTogether',false
  25.08.05 wl  TZPAppIniAccess.CreateAllowedEntries    TN2558.8  'SchedDataSourceType' entfernt
  05.09.05 wl  TZPAppIniAccess.CreateAllowedEntries    TN2541.4  new: 'ReagentList' section
  23.09.05 wl  TZPRobotIniAccess.CreateAllowedEntries  TN2624    STR_ISEC_STATEMONITOR, Ports jetzt string statt integer
  07.11.05 pk  TZPAppIniAccess.CreateAllowedEntries    TN2737    New: 'Scheduling','ShiftTime',10
  08.11.05 wl  TZPAppIniAccess.CreateAllowedEntries    TN2745    Script-bezogene Einträge entfernt (UseScript,ScriptsAreRackBased,..)
  30.11.05 wl  TZPAppIniAccess.CreateAllowedEntries    TN2818    entfernt: PR_TipTypes, WashRemRediTips
  21.12.05 wl  TZPAppIniAccess.CreateAllowedEntries    TN2541.0  'CompleteMethodImport', 'ImportButton'
  06.01.06 wl  TZPRobotIniAccess.CreateAllowedEntries  TN2718    neu: 'Stacker','UseCorrectLevelHeights'
  26.01.06 pk  TZPAppIniAccess.CreateAllowedEntries    TN2902    'Run','RunCreateInsertFreeB',true,
  06.02.06 wl  TZPRobotIniAccess.CreateAllowedEntries  TN2928    'MoveRack','NoMoveIfRackAtDestination'
  04.03.06 wl  TZPAppIniAccess.CreateAllowedEntries    TN2541.4  'ReagentList': Mehr Flexibilität
  04.03.06 wl  TWinlissyIniAccess.ReadDBFieldDefinition TN2541.4 für individuelle Felddefinitionen
  04.03.06 wl  TZPAppIniAccess.CreateAllowedEntries    TN2953    'Display', 'UseWashPrograms','UseSequences','UseReagentRacks',
  14.03.06 wl  TZPAppIniAccess.CreateAllowedEntries    TN2964   'Washprogram','SwitchOffN2C3BeforeWashInnerCh'
  15.03.06 wl  TZPAppIniAccess.CreateAllowedEntries    TN2965   'Washprogram','DelayBefore2ndBlowN2'
  10.04.06 pk  TMethodIniAccess.CreateAllowedEntries   TN3031   STR_ISEC_METHOD_INITOPTIONS, 'Init', 0
  27.04.06 pk  TZPRobotIniAccess.CreateAllowedEntries  TN2958   'DISPOSAL_TIPS','GT_ScanSpeed',500
  27.04.06 pk  TZPRobotIniAccess.CreateAllowedEntries  TN2958   'DISPOSAL_TIPS','GT_ScanRamp',300
  19.05.06 wl  TMethodIniAccess.CreateAllowedEntries   TN3109    neu: 'Attributes'
  13.06.06 pk  TZPAppIniAccess.CreateAllowedEntries    TN3154    Run, RunVarUsePriority, false
  05.07.06 pk  TZPAppIniAccess.CreateAllowedEntries    TN3181    Run, RunCreateUseMethVars,true
  06.09.06 wl  ReadAppIni                              TN3288   Nicht benötigte Calli-Options entfernt
  13.09.06 pk  TZP01CfgIniAccess.CreateAllowedEntries  TN3298   New: 'Module', 'TubeHandTrk', 0
  21.09.06 wl  TWinlissyIniAccess.ReadTipType          TN3326   entfernt
  21.09.06 wl  TZPRobotIniAccess.CreateAllowedEntries  TN3326   'DISPOSAL_TIPS': 2 unnötige Einträge entfernt
  21.09.06 wl  TZPRobotIniAccess.CreateAllowedEntries  TN3326   'TipTypes'-Section entfernt
  25.10.06 wl  TZPRobotIniAccess.CreateAllowedEntries  TN3375TEST.1  neu: 'EDOSTracking','Delay'
  29.11.06 wl  TZPAppIniAccess.CreateAllowedEntries    TN3225.1  entfernt: 'Pipetting','WashRetractSpeed'
  12.12.06 wl  TZPRobotIniAccess.CreateAllowedEntries  TN3468   BorderFactor abgeschafft
  20.12.06 wl  TZP01CfgIniAccess.CreateAllowedEntries  TN3426   neu: 'Module', 'MinMotorSpeed', Default = 6
  16.01.07 pk  TMethodIniAccess.CreateAllowedEntries   TN3481   STR_ISEC_METHOD_AUTOSEQOPTIONS, STR_IDENT_METHOD_AUTOSEQON, true
  02.02.07 wl  TZPRobotIniAccess.CreateAllowedEntries  TN3537   neu: 'MoveRack', 'MoveXYBeforeRotation'
  20.02.07 wl  TZPAppIniAccess.CreateAllowedEntries    TN3016   entfernt: 'Run','RunCreateParserType'
  21.02.07 wl  TZP01CfgIniAccess.CreateAllowedEntries  TN3590   'Precision', 'WashAtWaste' nur noch unter ROBOT
  21.02.07 wl  TZPRobotIniAccess.CreateAllowedEntries  TN3590   Default für 'WashAtWaste' unter ROBOT = 10
  06.02.07 wl  TZPRobotIniAccess.CreateAllowedEntries  TN3622   einheitlich für ZP01/ZP02: Default = 100%
  24.07.07 pk  TZPAppIniAccess.CreateAllowedEntries    TN3785   'Versions', 'Parser', 2
  25.07.07 wl  TZPAppIniAccess.CreateAllowedEntries    TN3792   entfernt: 'SaveSolvent' und alle 'Washprogram'-Einträge
  30.07.07 wl  TZPRobotIniAccess.CreateAllowedEntries  TN3782   neu: 'MoveRack','HXYIndependetFromRotation' - entfernt: 'Options','ChangeXYAtRotation'
  08.08.07 wl  TZPAppIniAccess.CreateAllowedEntries    TN3802   wieder eingeführt: 'Washprogram','SwitchOffN2C3BeforeWashInnerCh'
  31.08.07 wl                                          TN3811.4  an Änderungen in IniAccess angepasst
  09.11.07 pk                                          TN3922    Steps changed to mm
  27.11.07 wl  TZPRobotIniAccess.CreateAllowedEntries  TN3898   neu: 'Mixing', 'DispLessThanAsp'
  27.11.07 wl  TZPRobotIniAccess.CreateAllowedEntries  TN3844   neue Einträge für 'PowderDetection'
  28.11.07 wl  TZPAppIniAccess.CreateAllowedEntries    TN3915   Neu: 'View','VisiblemenuItems'
  09.01.08 wl  TZPAppIniAccess.CreateAllowedEntries    TN3972   RunCreateSeqAuto  entfernt
  09.01.08 wl  TZPAppIniAccess.CreateAllowedEntries    TN3972   SwitchPortsOnRestart  entfernt
  09.01.08 wl  TZPAppIniAccess.CreateAllowedEntries    TN3972   Master/Slave-Krempel  entfernt
  07.01.08 pk  TZPRobotIniAccess.CreateAllowedEntries  TN3971   New: WorkbenchZ in mm
  30.01.08 wl  TZPAppIniAccess.CreateAllowedEntries    TN4003   'Schedule','OpenFixationAtInit' --> CatMixConnection
  30.01.08 wl  TZPAppIniAccess.CreateAllowedEntries    TN4003   'Schedule','FixAfterVortexing' --> ShakerDevice
  30.01.08 wl  TZPAppIniAccess.CreateAllowedEntries    TN4003   'Schedule','NoVortexerStopAtInit' --> CatMixConnection
  13.02.08 wl                                          TN4009   alle Module, die früher über Settings definiert wurden, auskommentiert
  29.02.08 wl  TZPRobotIniAccess.CreateAllowedEntries  TN4032    New value RackMoveZTravel
  11.06.08 wl  TZP01CfgIniAccess, TZP02CfgIniAccess    TN4143    entfernt
  20.06.08 pk  TZPAppIniAccess.CreateAllowedEntries    TN4139    New: 'Graphics', 'GraphicsType', '2D'
  27.06.08 pk  TZPRobotIniAccess.CreateAllowedEntries  TN4139    New: 'Carrier', 'ReverseY', false
  30.06.08 pk  TZPRobotIniAccess.CreateAllowedEntries  TN4151    ZTravel settings removed from Robot, Module
  04.08.08 pk  TZPRobotIniAccess.CreateAllowedEntries  TN4139    Remove VisualFactor Zoom, BorderLeft, etc.
  06.08.08 pk  TMethodIniAccess                        TN4165.1  Removed
  11.09.08 wl  TZPRobotIniAccess.CreateAllowedEntries  TN4176   New: 'WGHP','UseLastSubstID'
  26.11.08 wl  TZPRobotIniAccess.CreateAllowedEntries  TN4329    Leichen entfernt
  26.11.08 wl  TZPAppIniAccess.CreateAllowedEntries    TN4329    Leichen entfernt
  08.12.08 wl  TZPAppIniAccess.CreateAllowedEntries    TN4343    neue 'Display' Settings: UseLiquids und DefaultMethod
  24.02.09 pk  TZPAppIniAccess.CreateAllowedEntries    TN4232    New: 'Restart','Enabled',true,''
  09.04.09 pk  TZPAppIniAccess.CreateAllowedEntries    TN4520    New 'Logging', 'LogMaxLinesPerFile', 0
  14.04.09 pk  TZPAppIniAccess.CreateAllowedEntries    TN4512    New 'Display', 'CanSimulateInRealMode', false
  06.08.09 wl                                          TN4702   Strings werden direkt geladen
  30.04.10 wl  TZPAppIniAccess.CreateAllowedEntries    TN5052   3 neue Einträge in 'Display': 'UseDisplayComponents', 'OneTipTypeMode', 'OneWorkspaceMode'
  06.05.10 wl  TZPAppIniAccess.CreateAllowedEntries    TN5052   'OneWorkspaceMode': Default ist jetzt true!!
  19.05.10 pk  TZPAppIniAccess.CreateAllowedEntries    TN5113   New: 'Restart', 'OnlyAtRestartMarkers', false
  23.06.10 pk  TZPAppIniAccess.CreateAllowedEntries    TN5163   Pipetting','UseDifferentTipTypesTogether',false
  29.06.10 pk  TZPAppIniAccess.CreateAllowedEntries    TN5143   Pipetting','UseDifferentLiqParamsTogether',false
  15.07.10 pk  TZPRobotIniAccess.CreateAllowedEntries  TN5196   WaitAfterPickSys removed
  27.07.10 wl                                          TN5086    SimBeforeStart entfernt
  27.07.10 wl                                          TN5123   unbenutzte Settings entfernt
  27.07.10 wl                                          TN5123   'Options', 'O' entfernt
  27.10.10 wl  TZPAppIniAccess.CreateAllowedEntries    TN5300   6 neue Einträge unter 'MailSender'
  23.11.10 wl  TZPAppIniAccess.CreateAllowedEntries    TN5300   'MailSender', 'Password' ist jetzt TStringEntry
  28.11.10 wl  TZPAppIniAccess.CreateAllowedEntries    TN5355   Delay-Settings haben jetzt section 'Delay'
  30.11.10 wl  TZPAppIniAccess.CreateAllowedEntries    TN5300    'MailSender' 'Recipients Ident geändert
  26.01.11 wl  TWinLissyIniAccess.ReadXYRangeData      TN5448   entfernt
  06.04.11 wl  TZPAppIniAccess.CreateAllowedEntries    TN5501   neu: 'Pipetting','ReloadLiqHParamsForEachStep'
  27.04.11 wl  TZPAppIniAccess.CreateAllowedEntries    TN5562   neu: 'Pipetting', 'DropTipsBeforewashMethod'
  02.05.11 wl  TZPRobotIniAccess.CreateAllowedEntries  TN5565   bessere Description zu 'Precision', 'WashAtWaste'
  20.07.11 wl  TZPAppIniAccess.CreateAllowedEntries    TN5614.1 neu: 'Display', 'MethodEditorBigImages'
  15.09.11 wl  TZPAppIniAccess.CreateAllowedEntries    TN5694   ShowTempRequestTime,LogVortInfo entfernt
  20.09.11 wl  TZPAppIniAccess.CreateAllowedEntries    TN5723   RunCreatePaint entfernt
  28.10.11 wl  TZPAppIniAccess.CreateAllowedEntries    TN5725   TubeColors : 4 Werte, die TubeDispColor ersetzen
  17.11.11 wl  TZPAppIniAccess.CreateAllowedEntries    TN5725   RunCreateRackNameRequired raus (ist jetzt überflüssig)
  21.11.11 wl  TZPAppIniAccess.CreateAllowedEntries    TN5741   'ReloadLiqHParamsForEachStep': Default = false
  03.02.12 wl  TZPAppIniAccess.CreateAllowedEntries    TN5792   'ReagentList'-Einträge entfernt
  07.02.12 wl  TZPAppIniAccess.CreateAllowedEntries    TN5796   'VolumeControl'-Einträge entfernt
  09.02.12 wl  TZPAppIniAccess.CreateAllowedEntries    TN5792   neu: 'UseSubstances'
  27.03.12 wl  TZPAppIniAccess.CreateAllowedEntries    TN5833   neu: 'Display', 'EditVariablesInRunner'
  23.04.12 ts  TZPAppIniAccess.CreateAllowedEntries    TN5860   neu: LayoutZoomBarPosition
  02.05.12 wl  TZPRobotIniAccess.CreateAllowedEntries  TN5882   neu: 'PipDevice', 'MaxPumpCount'
  18.06.12 wl  TZPAppIniAccess.CreateAllowedEntries    TN5899   EnableVolumeControl entfernt
  25.07.12 ts  TZPAppIniAccess.CreateAllowedEntries    TN5944   EndOfLineChar for each line can be set in the settings (CR+LF)
  07.08.12 wl  TZPAppIniAccess.CreateAllowedEntries    TN5946   Irrelevantes Setting AcceptOnlyUniversalLayout entfernt
  10.08.12 wl  TZPAppIniAccess.CreateAllowedEntries    TN5947   UsePeriAtWash entfernt
  13.12.12 wl  TZPAppIniAccess.CreateAllowedEntries    TN6054   Section 'Actions' entfernt
  27.01.13 wl  TZPAppIniAccess.CreateAllowedEntries    TN6069   neue 'Backup'-Einträge
  20.02.13 wl  TZPAppIniAccess.CreateAllowedEntries    TN6055   Section 'MethodParams' entfernt
  13.03.13 wl  TZPAppIniAccess.CreateAllowedEntries    TN5960   neu: BuildingBlockEditorMode
  18.03.13 ts  TZPAppIniAccess.CreateAllowedEntries    TN6109   neu: RemoteControl - UseTCPIPInterface
  28.03.13 wl  TZPAppIniAccess.CreateAllowedEntries    TN6120   'SubstanceSet', 'AutomaticLoadSet' ersetzt festen Namen 'AUTOLOAD'
  28.03.13 wl  TZPAppIniAccess.CreateAllowedEntries    TN6120   neu: 'SubstanceSet', 'AutomaticRefreshSet'
  18.09.13 wl                                          TN6045   unbenutzte Settings entfernt
  10.12.13 wl  TZPRobotIniAccess.CreateAllowedEntries  TN6326   entfernt: 'Carrier', 'ReverseY'
  08.04.14 ts                                          TN6392   new EnableCalibration for Calibration
  08.04.14 ts                                          TN6393   new TablesEditableInRunner to edit tables from ZARunner
  ------------------------------------------------------------------------------------------------------------ }

unit WinlissyIniAccess;


interface


uses
    Controls,
    IniAccess,
    CommonTypes;

type
    TWinlissyIniAccess = class(TCheckingIniAccess, IWinLissyIniAccess)
    public
        // Public Methods
        function ReadBcscanrange(aSection, aIdent: string): BCSCANRANGE;
        function ReadBcrposition(aSection, aIdent: string): BCRPOSITION;
        function ReadDLLMenuItem(aSection, aIdent: string): TDLLMenuItem;
        function ReadTubeToolData(aSection, aIdent: string): TTubeToolData;
        function ReadLiquidPortData(aSection, aIdent: string): TLiquidPortData;
        function ReadDBFieldDefinition(aSection, aIdent: string): TDBFieldDefinition;
        function ReadBackupPathData(aSection, aIdent: string): TBackupPathData;
    end;

    TZPAppIniAccess = class(TWinlissyIniAccess)
    protected
        procedure CreateAllowedEntries; override;
    end;

    TZPRobotIniAccess = class(TWinlissyIniAccess)
    protected
        procedure CreateAllowedEntries; override;
    end;

    TZPDLLIniAccess = class(TWinlissyIniAccess)
    protected
        procedure CreateAllowedEntries; override;
    end;


implementation


uses
    SysUtils,
    Graphics,
    Windows,
    GeneralTypes,
    IniEntry,
    WinLissyIniEntry,
    IniSection;

{ TZPRobotIniAccess }

procedure TZPRobotIniAccess.CreateAllowedEntries;
begin
    inherited CreateAllowedEntries;

    fItems.Add(TIntegerEntry.Create('OPTIONS', 'HToolYCorrect', 1, '', TLanguageString.Read('HToolYCorrect',
        'HToolYCorrect')));

    fItems.Add(TIntegerEntry.Create('LiquidSpeed', 'DefaultAspSpeed', 0, 'µl/sec',
        TLanguageString.Read('DefaultAspSpeed', 'DefaultAspSpeed')));
    fItems.Add(TIntegerEntry.Create('LiquidSpeed', 'DefaultDispSpeed', 0, 'µl/sec',
        TLanguageString.Read('DefaultDispSpeed', 'DefaultDispSpeed')));
    fItems.Add(TIntegerEntry.Create('LiquidSpeed', 'MaxAspSpeed', 0, 'µl/sec',
        TLanguageString.Read('MaxAspSpeed', 'MaxAspSpeed')));
    fItems.Add(TIntegerEntry.Create('LiquidSpeed', 'AspSpeedCalcFactor', 100, '',
        TLanguageString.Read('AspSpeedCalcFactor', 'AspSpeedCalcFactor')));
    fItems.Add(TIntegerEntry.Create('LiquidSpeed', 'DispSpeedCalcFactor', 100, '',
        TLanguageString.Read('DispSpeedCalcFactor', 'DispSpeedCalcFactor')));

    fItems.Add(TIntegerEntry.Create('Module', 'DilutorAspSpeedPerCent', 0, '',
        TLanguageString.Read('DilutorAspSpeedPerCent',
        'DilutorAspSpeedPerCent - wenn >0 dann wird der Dilutor.AspirationSpeed als Wert gewertet')));

    fItems.Add(TIntegerEntry.Create('WriteLogs', 'WRITE_LOG_INFO', 1, '',
        TLanguageString.Read('WRITE_LOG_INFO', 'WRITE_LOG_INFO')));
    fItems.Add(TIntegerEntry.Create('WriteLogs', 'WRITE_LOG_ERROR', 1, '',
        TLanguageString.Read('WRITE_LOG_ERROR', 'WRITE_LOG_ERROR')));
    fItems.Add(TIntegerEntry.Create('WriteLogs', 'WRITE_LOG_DEBUG', 0, '',
        TLanguageString.Read('WRITE_LOG_DEBUG', 'WRITE_LOG_DEBUG')));
    fItems.Add(TIntegerEntry.Create('WriteLogs', 'WRITE_LOG_SEND', 1, '',
        TLanguageString.Read('WRITE_LOG_SEND', 'WRITE_LOG_SEND')));
    fItems.Add(TIntegerEntry.Create('WriteLogs', 'WRITE_LOG_NO_NEW', 1, '',
        TLanguageString.Read('WRITE_LOG_NO_NEW', 'WRITE_LOG_NO_NEW')));

    fItems.Add(TBoolEntry.Create('DISPOSAL_TIPS', 'DT_CHECK_BEFORE', false, '',
        TLanguageString.Read('Check for all tips available before dropping',
        'Check for all tips available before dropping')));
    fItems.Add(TBoolEntry.Create('DISPOSAL_TIPS', 'DT_DISP_BEFORE', false, '',
        TLanguageString.Read('Display Error when not all tips available',
        'Display Error when not all tips available')));
    fItems.Add(TBoolEntry.Create('DISPOSAL_TIPS', 'DT_DISP_ERROR', false, '',
        TLanguageString.Read('Diplay error when unable to remove tips',
        'Diplay error when unable to remove tips')));
    fItems.Add(TBoolEntry.Create('DISPOSAL_TIPS', 'DT_CHECK_AFTER', false, '',
        TLanguageString.Read('Check for all tips have succesfully removed',
        'Check for all tips have succesfully removed')));
    fItems.Add(TBoolEntry.Create('DISPOSAL_TIPS', 'DT_DISP_AFTER', false, '',
        TLanguageString.Read('Diplay error when unable to remove tips',
        'Diplay error when unable to remove tips')));
    fItems.Add(TBoolEntry.Create('DISPOSAL_TIPS', 'DT_REMOVE_LIQ', false, '',
        TLanguageString.Read('Remove Liquid before drop tips', 'Remove Liquid before drop tips')));

    fItems.Add(TBoolEntry.Create('DISPOSAL_TIPS', 'GT_DISP_ERROR', false, '',
        TLanguageString.Read('Display error when unable to remove tips',
        'Display error when unable to remove tips')));
    fItems.Add(TBoolEntry.Create('DISPOSAL_TIPS', 'GT_SKIP_ERROR', false, '',
        TLanguageString.Read('Auto Skip  at Error else Auto Ignore',
        'Auto Skip  at Error else Auto Ignore')));
    fItems.Add(TBoolEntry.Create('DISPOSAL_TIPS', 'GT_RETRY_ERROR', false, '',
        TLanguageString.Read('Auto RETRY at Error else Auto Ignore',
        'Auto RETRY at Error else Auto Ignore')));
    fItems.Add(TBoolEntry.Create('DISPOSAL_TIPS', 'GT_CHECK_AFTER', false, '',
        TLanguageString.Read('Check for all tips have succesfully removed',
        'Check for all tips have succesfully removed')));
    fItems.Add(TBoolEntry.Create('DISPOSAL_TIPS', 'GT_DISP_AFTER', false, '',
        TLanguageString.Read('Diplay error when unable to remove tips',
        'Diplay error when unable to remove tips')));
    fItems.Add(TBoolEntry.Create('DISPOSAL_TIPS', 'GT_SKIP_AFTER', false, '',
        TLanguageString.Read('Auto Skip  at Error else Auto Ignore',
        'Auto Skip  at Error else Auto Ignore')));
    fItems.Add(TBoolEntry.Create('DISPOSAL_TIPS', 'GT_RETRY_AFTER', false, '',
        TLanguageString.Read('Auto RETRY at Error else Auto Ignore',
        'Auto RETRY at Error else Auto Ignore')));
    fItems.Add(TBoolEntry.Create('DISPOSAL_TIPS', 'GT_SINGLE_TIP', false, '',
        TLanguageString.Read('Tips one by one', 'Tips one by one')));

    fItems.Add(TBoolEntry.Create('DISPOSAL_TIPS', 'IgnoreRestTips', true, '',
        TLanguageString.Read('DISPOSAL_TIPS IgnoreRestTips', 'DISPOSAL_TIPS IgnoreRestTips')));
    fItems.Add(TIntegerEntry.Create('DISPOSAL_TIPS', 'GT_ScanSpeed', 500, '',
        TLanguageString.Read('Speed used when fetching tips',
        'Geschwindigkeit verwendet bei der Tipaufnahme')));
    fItems.Add(TIntegerEntry.Create('DISPOSAL_TIPS', 'GT_ScanRamp', 300, '',
        TLanguageString.Read('Ramping used when fetching tips', 'Ramping verwendet bei der Tipaufnahme')));

    fItems.Add(TIntegerEntry.Create('PlateCheck', 'PutPlate', 0, '', TLanguageString.
        Read('Checks during each plate moving action if a gripped plate can be brought back by the handler',
        'Überprüft bei jeder Plattenbewegungs-Action, ob die Platte zurückgebracht werden konnte')));
    fItems.Add(TIntegerEntry.Create('PlateCheck', 'GetPlate', 0, '', TLanguageString.
        Read('Checks during each plate moving action if the plate was really gripped',
        'Überprüft bei jeder Plattenbewegungs-Action, ob die Platte gegriffen wurde')));

    fItems.Add(TFloatEntry.Create('TakeTubes', 'XOffset', 0, 'mm', TLanguageString.
        Read('X-offset when gripping tubes with a fixed tool',
        'X-Offset beim Greifen von Tubes mit einem permanenten Greifertool')));
    fItems.Add(TFloatEntry.Create('TakeTubes', 'YOffset', 0, 'mm', TLanguageString.
        Read('Y-offset when gripping tubes with a fixed tool',
        'Y-Offset beim Greifen von Tubes mit einem permanenten Greifertool')));
    fItems.Add(TFloatEntry.Create('TakeTubes', 'ZOffset', 0, 'mm', TLanguageString.
        Read('Z-offset when gripping tubes with a fixed tool',
        'Z-Offset beim Greifen von Tubes mit einem permanenten Greifertool')));
    fItems.Add(TFloatEntry.Create('TakeTubes', 'VOpen', 0, 'mm', TLanguageString.
        Read('varispan value when picking up tubes with a fixed tool',
        'Varispan-Wert beim Greifen von Tubes mit einem permanenten Greifertool')));
    fItems.Add(TFloatEntry.Create('TakeTubes', 'VDrop', 0, 'mm', TLanguageString.
        Read('varispan value when dropping tubes with a fixed tool',
        'Varispan-Wert beim Absetzen von Tubes mit einem permanenten Greifertool')));
    fItems.Add(TFloatEntry.Create('TakeTubes', 'ZDrop', 0, '-mm', TLanguageString.
        Read('dropping height [mm] for tubes picked up with a fixed OR PICKUP tool (negative value)',
        'Absetzhöhe [mm] für Tubes, die mit einem permanenten ODER AUFGENOMMENEN Greifertool transportiert wurden (negativer Wert)')
        ));
    fItems.Add(TFloatEntry.Create('TakeTubes', 'TubeDY', 0, 'mm', TLanguageString.
        Read('virtual diameter of the tube to adjust gripping pressure: higher value means gripping harder',
        'virtueller Tube-Durchmesser, um den Greiferdruck zu regulieren: ein größerer Wert bedeutet festeres Zugreifen')
        ));
    fItems.Add(TFloatEntry.Create('TakeTubes', 'SaveMoveOffset', 0, 'mm',
        TLanguageString.Read('offset [steps] to reduce the speed to save move speed to put a tube',
        'Offset [Schritte], um mit der >save move speed< beim Absetzen von Tubes zu beginnen')));
    fItems.Add(TIntegerEntry.Create('TakeTubes', 'SaveMoveSpeed', 0, 'Steps/sec',
        TLanguageString.Read('save move speed [steps/sec] to put a tube',
        'save move speed [Schritte/sec], mit der Tubes abgesetzt werden')));
    fItems.Add(TFloatEntry.Create('TakeTubes', 'ToolBringBackVOffset', 0, 'mm',
        TLanguageString.
        Read('varispan value will be reduced by this amount of steps when bringing back tools (attention: when tools which have a fixed diameter are used this can cause an error!)',
        'Beim Zurückbringen von Tools wird der Varispan-Wert um diesen Wert verkleinert (Achtung: bei Benutzung von Tools mit einem festen Durchmesser kann diese Einstellung zu Fehlern führen!)')
        ));
    fItems.Add(TFloatEntry.Create('TakeTubes', 'ShakeHeight', 1600, 'mm',
        TLanguageString.Read('TakeTubes ShakeHeight', 'TakeTubes ShakeHeight')));
    fItems.Add(TFloatEntry.Create('TakeTubes', 'ShakeZOffset', 700, 'mm',
        TLanguageString.Read('TakeTubes ShakeZOffset', 'TakeTubes ShakeZOffset')));
    fItems.Add(TFloatEntry.Create('TakeTubes', 'ShakeYOffset', 30, 'mm',
        TLanguageString.Read('TakeTubes ShakeYOffset', 'TakeTubes ShakeYOffset')));
    fItems.Add(TBoolEntry.Create('TakeTubes', 'UseCorrectCarrierOffsets', true, '',
        TLanguageString.Read('TakeTubes UseCorrectCarrierOffsets', 'TakeTubes UseCorrectCarrierOffsets')));

    fItems.Add(TIntegerEntry.Create('Module', 'SetupSpeedFactor', 100, '',
        TLanguageString.Read('Reduced speeds for Layouter', 'Reduced speeds for Layouter'), 2, 100));

    fItems.Add(TIntegerEntry.Create(STR_ISEC_BARCODE, 'TubeRetakeRotation', 0, '° (Degrees)',
        TLanguageString.
        Read('If first tube reading fails - Read again with other rotation (0 = do not read again)',
        'Wenn das erste Lesen mislingt - Nochmal lesen mit anderem Winkel (0 = nicht nochmal lesen)')));
    fItems.Add(TBoolEntry.Create(STR_ISEC_BARCODE, 'TubeBarcodesAreUnique', true, '',
        TLanguageString.
        Read('1= allow a Barcode to be entered for ONLY one tube; 0= allow the same Barcode to be entered for one or MORE tubes',
        '1= Dasselbe Barcode darft nicht für zwei verschiedene Röhrchen eingetragen werden; 0= Dasselbe Barcode darft für mehrere Röhrchen eingetragen werden')
        ));

    FAllowedSections.Add(TTubeToolDataSection.Create(STR_ISEC_TUBETOOLDATA, '',
        TLanguageString.Read('Tube Tool Data', 'Tube Tool Data')));

    fItems.Add(TStringEntry.Create(STR_ISEC_STATEMONITOR, 'DLLName', '', '',
        TLanguageString.Read('Dll name for to report state change', 'Dll Name um Statuswechsel zu melden')));

    fItems.Add(TIntegerEntry.Create('Module', 'WiperSpeedX', 0, '', TLanguageString.
        Read('X-Speed for MoveOffsetX', 'X-Speed for MoveOffsetX')));
    fItems.Add(TIntegerEntry.Create('Module', 'WiperSpeedY', 0, '', TLanguageString.
        Read('Y-Speed for MoveOffsetY', 'Y-Speed for MoveOffsetY')));

    fItems.Add(TIntegerEntry.Create('Precision', 'WashAtWaste', 10, '',
        TLanguageString.
        Read('Percent of Washvolume to wash at the waste station (0 = empty pumps at waste, -1 = go directly to wash)',
        'Prozent des Waschvolumens an Waste-Station abgeben (0 = Spritzen entleeren in Waste, -1 = gehe dierekt zu Wash-Rack)')
        ));

    fItems.Add(TIntegerEntry.Create('XConflictResolve', 'HardMode', 0, '',
        TLanguageString.
        Read('X Motor Conflict Resolve Mode - (0=Let software decide, 1=Always move other arm away, 2=Always wait until other arm done)',
        'X Motor Konfliktlösungsmodus - (0=Software soll entscheiden, 1=Bewege immer den anderen Arm, 2=Warte immer bis der anderer Arm fertig ist.')
        ));
    fItems.Add(TFloatEntry.Create('XConflictResolve', 'ConflictBuffer', 200, '',
        TLanguageString.
        Read('X Motor Conflict Buffer Distance - Minimum distance (in mm) allowed between arms',
        'X Motor Konfliktpufferabstand - Min. Abstand (in mm) erlaubt zwischen Arme')));

    fItems.Add(TBoolEntry.Create('Stacker', 'UseCorrectLevelHeights', false, '',
        TLanguageString.
        Read('For Racks placed in Stackers use for pipetting and tube handling .. 1: real heights (recommended), 0: height of level 1',
        'Für Racks platziert in Stackern benutze zum Pipettieren und Tube-bewegen .. 1: die jeweilige Level-Höhe (empfohlen), 0: dei Höhe von Level 1')
        ));
    fItems.Add(TBoolEntry.Create('MoveRack', 'NoMoveIfRackAtDestination', false, '',
        TLanguageString.
        Read('Action MOVER: 1 = Do nothing if the rack is already placed on the destination carrier',
        'Aktion MOVER: 1 = Keine Rackbewegung wenn das Rack schon auf dem Ziel platziert ist.')));

    fItems.Add(TBoolEntry.Create('MoveRack', 'MoveXYBeforeRotation', false, '',
        TLanguageString.
        Read('Rack movement: 0: Rotate gripper before moving to XY-Startposition, 1: Move to XY-Startposition before rotation',
        'Racks bewegen: 0: Drehe Greifer vor der Bewegung zur XY-Startposition, 1: Gehe zur XY-Startposition vor der Rotation')
        ));
    fItems.Add(TIntegerEntry.Create('Mixing', 'DispLessThanAsp', 0, 'uL', ''));
    // Dangerous Hack for Bayer France: Please do not use this!

    fItems.Add(TIntegerEntry.Create('PowderDetection', 'AbsoluteWeight', 100, 'mg',
        TLanguageString.
        Read('Powder Detection: Stop detection if absolute weigh value is higher than this value.',
        'Powder Detection: Anhalten, wenn die absolute Einwaage höher als dieser Wert ist.')));
    fItems.Add(TIntegerEntry.Create('PowderDetection', 'RelativeWeight', 50, 'mg',
        TLanguageString.
        Read('Powder Detection: Stop detection if difference between the current and the last weight is more than this value.',
        'Powder Detection: Anhalten, wenn der Unterschied zwischen der aktuellen und der letzten Einwaage höher als dieser Wert ist.')
        ));

    fItems.Add(TBoolEntry.Create('WGHP', 'UseLastSubstID', false, '',
        TLanguageString.
        Read('0: Always use SubstID >> Destination <<, 1: Use SubstID of last dispensed substance',
        '0: Benutze immer SubstID >> Destination <<, 1: Benutze SubstID der letzten zugegebenen Substanz')));

    fItems.Add(TFloatEntry.Create('Module', 'WorkbenchZ', 0, 'mm', TLanguageString.Read('Workbench Z Height',
        'Workbench Z Höhe')));

    fItems.Add(TIntegerEntry.Create('PipDevice', 'MaxPumpCount', 16, '',
        TLanguageString.Read('The maximum number of PipPump devices for a PipDevice',
        'Die maximale Anzahl von PipPumps für ein PipDevice')));
end;

{ TZPAppIniAccess }

procedure TZPAppIniAccess.CreateAllowedEntries;
begin
    inherited CreateAllowedEntries;

    // Part 3: All entries needed in sampler.ini
    fItems.Add(TStringEntry.Create('Info', 'SerialNo', '', '', TLanguageString.Read('Serial Number',
        'Seriennummer')));

    fItems.Add(TStringEntry.Create('Oem', 'AppTitle', '', '', TLanguageString.Read('OEM application title',
        'OEM Applikationstitel')));
    fItems.Add(TStringEntry.Create('Oem', 'OemTitle', '', '', TLanguageString.Read('OEM title',
        'OEM Titel')));
    fItems.Add(TStringEntry.Create('Oem', 'JobTitle', '', '', TLanguageString.Read('OEM job title',
        'OEM Jobtitel')));
    fItems.Add(TStringEntry.Create('Oem', 'Support', '', '', TLanguageString.Read('OEM support',
        'OEM Support')));
    fItems.Add(TStringEntry.Create('Oem', 'Copyr1', '', '', TLanguageString.Read('OEM copyright text',
        'OEM Copyright Text')));
    fItems.Add(TStringEntry.Create('Oem', 'Copyr2', '', '', TLanguageString.Read('OEM copyright text',
        'OEM Copyright Text')));
    fItems.Add(TStringEntry.Create('Oem', 'www', '', '', TLanguageString.Read('OEM www address',
        'OEM WWW-Adresse')));

    fItems.Add(TIntegerEntry.Create('Display', 'Language', 0, '', TLanguageString.
        Read('Language: 0 = English, 1 = German', 'Sprache: 0 = Englisch, 1 = Deutsch')));
    fItems.Add(TBoolEntry.Create('Display', 'ConfirmIgnore', false, '',
        TLanguageString.Read('Ignoring an error message must be extra confirmed',
        'Ignorieren einer Fehlermeldung muss extra bestätigt werden')));
    fItems.Add(TIntegerEntry.Create('Display', '24h-Mode', 0, '', TLanguageString.
        Read('If barcodes cannot be scanned there will be no message (1 = Request at the end, 2 = no request)',
        'Wenn Barcodes nicht gelesen werden kommt keine Meldung (1 = Abfrage am Schluß, 2 = keine Abfrage)'),
        0, 2));
    fItems.Add(TBoolEntry.Create('Display', 'ChangePosinfo', false, '', TLanguageString.Read('ChangePosinfo',
        'ChangePosinfo')));
    fItems.Add(TIntegerEntry.Create('Display', 'AskForRackPlacement', 0, '',
        TLanguageString.Read('AskForRackPlacement', 'AskForRackPlacement')));
    fItems.Add(TIntegerEntry.Create('Display', 'TubeAspColor', $00B75B00, '',
        TLanguageString.Read('Rack well color at aspiration',
        'Farbe der Rackposition bei der Flüssigkeits-Aufnahme')));
    fItems.Add(TIntegerEntry.Create('Display', 'TubeDispColor', integer(clRed), '',
        TLanguageString.Read('Rack well color at dispense',
        'Farbe der Rackposition bei der Flüssigkeits-Abgabe')));
    fItems.Add(TIntegerEntry.Create('Display', 'TubeMoveColor', integer(clGreen), '',
        TLanguageString.Read('Rack well color at tube movement',
        'Farbe der Rackposition bei der Tube-Bewegung')));
    fItems.Add(TIntegerEntry.Create('Display', 'TubeOtherColor', 0, '',
        TLanguageString.Read('Rack well color at other actions (e.g. liquid detection)',
        'Farbe der Rackposition bei anderen Aktionen (z.B. Liquid-Detektion')));
    fItems.Add(TIntegerEntry.Create('Display', 'UseScheduler', 0, '', TLanguageString.Read('UseScheduler',
        'UseScheduler')));
    fItems.Add(TBoolEntry.Create('Display', 'HideVolumesWindow', false, '',
        TLanguageString.Read('Hide volumes window', 'Voluminafenster verstecken')));
    fItems.Add(TBoolEntry.Create('Display', 'UseWashPrograms', true, '',
        TLanguageString.Read('Show list of wash programs in ZADesign (if SOPHAS licence exists)',
        'Liste der Waschprogramme in ZADesign zeigen (wenn eine SOPHAS-Lizenz besteht)')));
    fItems.Add(TBoolEntry.Create('Display', 'UseSequences', true, '',
        TLanguageString.Read('Show list of sequences in ZADesign (if SOPHAS licence exists)',
        'Liste der Sequenzen in ZADesign zeigen (wenn eine SOPHAS-Lizenz besteht)')));
    fItems.Add(TBoolEntry.Create('Display', 'UseReagentRacks', true, '',
        TLanguageString.Read('Show list of substance sets in ZADesign (if SOPHAS licence exists)',
        'Liste der Substanz-Sets in ZADesign zeigen (wenn eine SOPHAS-Lizenz besteht)')));
    fItems.Add(TBoolEntry.Create('Display', 'UseSubstances', false, '',
        TLanguageString.Read('Show list of substances in ZADesign',
        'Liste der Substanzen in ZADesign zeigen')));
    fItems.Add(TBoolEntry.Create('Display', 'MethodEditorBigImages', false, '',
        TLanguageString.Read('Use 24x24 Images in method editor',
        '24x24-Bilder im Methodeneditor verwenden')));
    fItems.Add(TIntegerEntry.Create('Display', 'BuildingBlockEditorMode', 0, '',
        TLanguageString.Read('0=No Building Block Editor, 1=Simple Editor',
        '0=Kein Building Block Editor, 1=Einfacher Editor')));

    fItems.Add(TBoolEntry.Create('Display', 'UseDisplayComponents', true, '',
        TLanguageString.Read('Show List of Display Components in ZADesign',
        'Liste der Display-Komponenten in ZADesign zeigen')));
    fItems.Add(TBoolEntry.Create('Display', 'OneTipTypeMode', false, '',
        TLanguageString.Read('There is only one Tip Type', 'Es gibt nur einen Tip-Typ')));
    fItems.Add(TBoolEntry.Create('Display', 'OneWorkspaceMode', true, '',
        TLanguageString.Read('There is only one Workspace', 'Es gibt nur einen Workspace')));
    fItems.Add(TBoolEntry.Create('Display', 'EditVariablesInRunner', true, '',
        TLanguageString.Read('The user can edit variable descriptions in the ZARunner',
        'Anwender können Variablen-Beschreibungen im ZARunner ändern')));
    fItems.Add(TIntegerEntry.Create('Display', 'LayoutZoomBarPosition', 1, '',
        TLanguageString.Read('Position: 0 = Top, 1 = Left, 2 = Right',
        'Position: 0 = Oben, 1 = Links, 2 = Rechts')));

    fItems.Add(TStringEntry.Create('Logfiles', 'Path', '', '', TLanguageString.
        Read('Path were log files are stored', 'Speicherpfad für Logfiles')));
    fItems.Add(TIntegerEntry.Create('Logfiles', 'StoreDays', 28, 'Days',
        TLanguageString.Read('Number of days after which logfiles are deleted automatically',
        'Anzahl Tage, nach denen Logfiles automatisch gelöscht werden')));
    fItems.Add(TStringEntry.Create('Logfiles', 'Archives', '', '', TLanguageString.
        Read('Logfile Archive Path', 'Archivpfad für Logfiles')));
    fItems.Add(TIntegerEntry.Create('Logfiles', 'MaxLinesPerFile', 0, '',
        TLanguageString.
        Read('Number of lines after which a new log file is created. (0 = do not create a new log file). (10000 Lines ~ 1 MB)',
        'Anzahl Logzeilen nach denen eine neue Logdatei erstellt wird (0 = keine neue Logdatei erstellen). (10000 Zeilen ~ 1 MB)')
        ));

    fItems.Add(TBoolEntry.Create('Documentation', 'WritePosinfo', true, '',
        TLanguageString.Read('WritePosinfo', 'WritePosinfo')));
    fItems.Add(TBoolEntry.Create('Documentation', 'UseTubeIDIfNoSubstIDExists', true, '',
        TLanguageString.
        Read('Posinfo logs for dispense: The Tube-ID of the source position should be entered in the field SUBSTID if no Subst-ID exists.',
        'Posinfo-Log für Abgabe: Die Tube-ID der Quellposition soll in das Feld SUBSTID eingetragen werden, wenn keine Subst-ID existiert.')
        ));

    fItems.Add(TIntegerEntry.Create('Pipetting', 'WashDryDelay', 1000, '',
        TLanguageString.Read('WashDryDelay', 'WashDryDelay')));
    fItems.Add(TBoolEntry.Create('Pipetting', 'WashOnlyUsedTips', false, '',
        TLanguageString.Read('Unused tips are not washed to save system liquid',
        'Unbenutzte Spitzen werden nicht gewaschen, um Systemflüssigkeit zu sparen')));
    fItems.Add(TBoolEntry.Create('Pipetting', 'DropTipsBeforewashMethod', true, '',
        TLanguageString.Read('Drop disposable tips before executing a wash method',
        'Einwegspitzen werden von der Ausführung einer Waschmethode abgeworfen')));
    fItems.Add(TBoolEntry.Create('Pipetting', 'DispSubmergeActive', false, '',
        TLanguageString.
        Read('If not enabled a dispense submerge setting in the liquid parameter window is ignored for compatibility reasons',
        'Wenn nicht aktiviert wird die Submerge-Einstellung beim Dispense im Liquid-Parameter Fenster aus Kompatibilitätsgründen ignoriert')
        ));
    fItems.Add(TBoolEntry.Create('Pipetting', 'ChangeTipTypes', false, '',
        TLanguageString.
        Read('Allows selection of tips in a method with the liquid parameter that are not defined in the currently selected layout',
        'Allows selection of tips in a method with the liquid parameter that are not defined in the currently selected layout')
        ));
    fItems.Add(TBoolEntry.Create('Pipetting', 'UseDifferentTipTypesTogether', false, '',
        TLanguageString.Read('Allows simultaneous pipetting with different tiptypes',
        'Ermöglicht gemeinsames Pipettieren mit unterschiedlichen Tiptypen')));
    fItems.Add(TBoolEntry.Create('Pipetting', 'UseDifferentLiqParamsTogether', false, '',
        TLanguageString.Read('Allows simultaneous pipetting with different liquid parameters',
        'Ermöglicht gemeinsames Pipettieren mit unterschiedlichen Liquid-Parameter')));

    fItems.Add(TBoolEntry.Create('Pipetting', 'NeverWashIfWashDisabled', false, '',
        TLanguageString.Read('Never wash after a pipetting step without <Wash After Dispense>.',
        'Niemals nach einem Pipettierschritt waschen, wenn Waschen nicht angekreuzt ist.')));
    fItems.Add(TIntegerEntry.Create('Pipetting', 'FlushVolAfterInit', 0, 'µl',
        TLanguageString.Read('Forced flushing with the volume entered after each init (0 = no forced flush)',
        'Zwangsspülung mit dem eingetragenen Volumen nach jedem Init (0 = keine Zwangsspülung)')));
    fItems.Add(TIntegerEntry.Create('Pipetting', 'AirVolAfterInit', 0, 'µl',
        TLanguageString.
        Read('Aspiration and dispense of air in waste after init to remove excess liquid from the outside of the tip',
        'Aspirieren und Dispergieren von Luft in der Waste-Station, um überschüssige Flüssigkeit von der Nadel zu entfernen')
        ));
    fItems.Add(TIntegerEntry.Create('Pipetting', 'RetractStepsAfterInit', 0, '',
        TLanguageString.Read('RetractStepsAfterInit', 'RetractStepsAfterInit')));
    fItems.Add(TBoolEntry.Create('Pipetting', 'FlushWithRediTips', true, '',
        TLanguageString.Read('Should the REDI-Tips be washed at the flush-window or the Flush action?',
        'Sollen die REDI-Tips im Flush-Fenster oder bei der Flush Action mitgewaschen werden?')));

    fItems.Add(TBoolEntry.Create('Pipetting', 'ReloadLiqHParamsForEachStep', false, '',
        TLanguageString.
        Read('Should the Liquid handling parameters be reloaded for every step? (not Thread-Safe)',
        'Sollen die Liquid Handling Parameter für jeden Schritt neu geladen werden? (nicht Thread-sicher)')));

    FAllowedSections.Add(TIntegerIniSection.Create('Liquids', 'Port%dVol',
        TLanguageString.
        Read('Actual content of the container connected to this port (value entered and tracked by program)',
        'Aktueller Inhalt des Behälters, der an diesen Port angeschlossen ist (Wert wird vom Programm eingetragen und nachgeführt)')
        ));
    FAllowedSections.Add(TIntegerIniSection.Create('Liquids', 'Port%dMinVol',
        TLanguageString.Read('If this value is reached pipetting stops with an error message',
        'Wenn dieser Wert erreicht wird, stoppt die Pipettierung mit einer Fehlermeldung')));
    fItems.Add(TIntegerEntry.Create('Liquids', 'WasteVol', 0, 'ml', TLanguageString.
        Read('Actual content of the waste container (value stored by program)',
        'Actueller Inhalt des Abfallbehälters (Wert wird vom Programm eingetragen)')));
    fItems.Add(TIntegerEntry.Create('Liquids', 'WasteMaxVol', 1000, 'ml',
        TLanguageString.Read('Maximum volume of waste container', 'Maximalvolumen des Abfallbehälters')));
    fItems.Add(TIntegerEntry.Create('Liquids', 'SysLiqMinVol', 100, 'ml',
        TLanguageString.
        Read('If this value is reached the display on the volume control windows changes to yellow',
        'Wenn dieser Wert erreicht wird, schaltet die Volumenkontrollanzeige auf gelb um')));

    fItems.Add(TIntegerEntry.Create('Balance', 'MoveTubeToBalance', 0, '',
        TLanguageString.
        Read('Options for tube movement to balance rack (see table Tube options for ReadT action in WinLissy manual)',
        'Optionen für den Röhrchentransport zum Waagenrack (siehe Tabelle Tube options bei der ReadT-Action im WinLissy Handbuch)')
        ));
    fItems.Add(TIntegerEntry.Create('Balance', 'MoveTubeFromBalance', 0, '',
        TLanguageString.
        Read('Options for tube movement from balance rack (see table Tube options for ReadT action in WinLissy manual)',
        'Optionen für den Röhrchentransport vom Waagenrack (siehe Tabelle Tube options bei der ReadT-Action im WinLissy Handbuch)')
        ));
    fItems.Add(TIntegerEntry.Create('Balance', 'MoveTubesAfterRestart', 0, '',
        TLanguageString.Read('Removes tubes left on the balance when program is restarted',
        'Entfernt tubes, die auf der Waage verblieben sind beim Neustart')));
    fItems.Add(TBoolEntry.Create('Balance', 'UseSeveralBalanceRacks', true, '',
        TLanguageString.Read('UseSeveralBalanceRacks', 'UseSeveralBalanceRacks')));

    fItems.Add(TStringEntry.Create('Calli', 'WeighLogicDbName', '', '',
        TLanguageString.Read('Path and name of WeighingWizard database',
        'Pfad und Name der WeighingWizard Datenbank')));
    fItems.Add(TStringEntry.Create('Calli', 'WeighLogicDbUser', 'CALLIW', '',
        TLanguageString.
        Read('Login name for the WeighingWizard database (has to correspond with the login name set up in IBConsole)',
        'Loginname für die WeighingWizard Datenbank (muss mit dem Loginnamen, der in der IBConsole registriert wurde, übereinstimmen)')
        ));
    fItems.Add(TStringEntry.Create('Calli', 'WeighLogicDbPassword', '', '',
        TLanguageString.
        Read('Password for the WeighingWizard database (has to correspond with the password set up in IBConsole)',
        'Passwort für die WeighingWizard Datenbank (muss mit dem Passwort, das in der IBConsole registriert wurde, übereinstimmen)')
        ));
    fItems.Add(TBoolEntry.Create('Calli', 'UseRackIDPos', false, '', TLanguageString.
        Read('RackID + position of tube are used as identifier in Posinfo.DB instead of barcode',
        'RackID + Position des Tubes werden in der Posinfo.DB statt eines Barcodes als Identifizierer benutzt')
        ));
    fItems.Add(TIntegerEntry.Create('Calli', 'RestAtRWash', 0, '', TLanguageString.
        Read('During Weighing: 0= Nothing; 1= Move to Redi-Wash; 2= move into RWash and switch off pump; 9= move in x (Move-X-Offset)',
        'Während der Einwaage: 0= Nichts;1 = Zur Redi-Waschstation fahren; 2= in RWash und Pumpe abstellen; 9= in x bewegen (Move-X-Offset)')
        ));

    fItems.Add(TBoolEntry.Create('Redi', 'ShakeTimeAdjustable', false, '',
        TLanguageString.Read('Redi ShakeTimeAdjustable', 'Redi ShakeTimeAdjustable')));
    fItems.Add(TBoolEntry.Create('Redi', 'MoveToRealPosBeforeWiper', false, '',
        TLanguageString.
        Read('After aspirate powder (with Shifting): Move the tip to the middle of the source pos (with wiper speed) before moving to wiper',
        'Nach der Pulveraufnahme (mit Aspiration Shifting): Die Mitte der Position mit Wiper Speed anfahren (vor dem Anfahren des Wipers)')
        ));

    fItems.Add(TStringEntry.Create('DatabaseToolDll', 'Name', '', '',
        TLanguageString.
        Read('Name of the DLL containing functions of the WeighingWizard database (IBTools.DLL - check correct version)',
        'Name der DLL, die Zusatzfunktionen für die WeighingWizard Datenbank enthält (IBTools.DLL - achten Sie auf die passende Version)')
        ));

    fItems.Add(TStringEntry.Create('DLLFiles', 'cDLLFiles', 'CRSRobot', '', TLanguageString.Read('cDLLFiles',
        'cDLLFiles')));
    fItems.Add(TStringEntry.Create('DLLFiles', 'DetachDLL', 'CRSRobot', '', TLanguageString.Read('DetachDLL',
        'DetachDLL')));

    fItems.Add(TStringEntry.Create('Device', 'Name', 'Sampler', '', TLanguageString.
        Read('Device,Name,Sampler', 'Device,Name,Sampler')));

    fItems.Add(TStringEntry.Create('Run', 'MoveRackSlotDelimiter', ';', '',
        TLanguageString.
        Read('Delimiter to seperate a slot definition from a rack definition for a move rack action',
        'Trennzeichen zwischen Slot- und Rackdefinition bei einer MoveR Action')));

    fItems.Add(TBoolEntry.Create('Delay', 'SkipDelaysOnSimulation', false, '',
        TLanguageString.Read('Delays are skipped (=1) or executed (=0) when running in simulation mode',
        'Delays werden im Simulationsmodus übersprungen (=1) oder ausgeführt (=0)')));
    fItems.Add(TIntegerEntry.Create('Delay', 'ShowDelayLongerThanXMilliSeconds', 950, 'msec',
        TLanguageString.
        Read('Show the delay information only for delays which are longer than the given number of milli seconds',
        'Zeige Delay-Informationen nur, wenn das Delay länger als diese Anzahl an Millisekunden ist')));
    fItems.Add(TIntegerEntry.Create('Delay', 'SkipXMilliSeconds', 0, 'msec',
        TLanguageString.Read('The given number of milli seconds will be skipped for every delay action',
        'Diese Anzahl an Millisekunden wird von jeder Delay Action abgezogen')));

    fItems.Add(TStringEntry.Create('ExportRunFile', 'Path', '.\ExportRn', '',
        TLanguageString.Read('Path for export files', 'Pfad für Exportdateien')));
    fItems.Add(TStringEntry.Create('ExportRunFile', 'Separator', ',', '',
        TLanguageString.Read('Field separator for export files (, or ; or TAB)',
        'Feldtrennzeichen für Exportdateien (, oder ; oder TAB)')));
    fItems.Add(TBoolEntry.Create('ExportRunFile', 'Numbering', true, '',
        TLanguageString.Read('If not set the ending is *.txt or *.xls, etc. - otherwise *.1, etc',
        'Wenn nichts angegeben, wird eine Endung wie *.txt oder *.xls verwendet, sonst *.1, etc.')));
    fItems.Add(TBoolEntry.Create('ExportRunFile', 'DateTimeStamp', false, '',
        TLanguageString.Read('Attach the timestamp defined with key DateTimeStampStr to export file names',
        'Anfügen des Zeitstempels an die Bezeichnung von Exportdateien, der durch den Schlüssel DateTimeStampStr definiert wird')
        ));
    fItems.Add(TBoolEntry.Create('ExportRunFile', 'SeprAtEndOfLine', true, '',
        TLanguageString.Read('If =1 a separator will be exported at the end of each line',
        'Wenn =1, dann wird an jedem Zeilenende noch ein Trennzeichen exportiert')));
    fItems.Add(TStringEntry.Create('ExportRunFile', 'Extension', 'txt', '',
        TLanguageString.Read('Extension for all export files (also for WrSQL actions)',
        'Endung, die für alle Exportdateien verwendet wird (auch für WrSQL-Actions)')));
    fItems.Add(TBoolEntry.Create('ExportRunFile', 'xlsDelCSV', true, '',
        TLanguageString.
        Read('After export of *.xls file the *.csv file will be deleted if this key is set to 1',
        'Nach dem Export einer *.xls-Datei wird die *.csv-Datei, aus der die Excel-Datei hervorgeht gelöscht, wenn dieser Schlüssel auf 1 gesetzt ist')
        ));
    fItems.Add(TStringEntry.Create('ExportRunFile', 'DateTimeStampStr', 'yyyymmdd-hhnnsszzz', '',
        TLanguageString.
        Read('Definition of a timestamp for the exportfile: yy=year, m=months, d=day, h=hours, n=minutes, s=seconds, z=10th of seconds; apart from yy every value can have 1 or 2 figures, year can be yy or yyyy',
        'Definition eines Zeitstempels für Exportdateien: yy=Jahr, m=Monat, d=Tag, h=Stunden, n=Minuten, s=Sekunden, z=Zehntelsekunden; jeder Wert außer yy kann 1- oder 2-stellig sein, das Jahr kann als yy oder yyyy angegeben werden')
        ));
    fItems.Add(TStringEntry.Create('ExportRunFile', 'EndOfLineChar', 'CRLF', '',
        TLanguageString.Read('End character for all lines of export file (WrSQL actions)(CRLF,CR,LF)',
        'Endung, die für alle Zeilen der Exportdatei verwendet wird (WrSQL-Actions)(CRLF,CR,LF)')));

    fItems.Add(TBoolEntry.Create('ScriptReset', 'AskForKeepContents', false, '',
        TLanguageString.Read('ScriptReset AskForKeepContents', 'ScriptReset AskForKeepContents')));
    fItems.Add(TStringEntry.Create('ScriptReset', 'ArchiveScriptPath', '.\ScriptArchive', '',
        TLanguageString.Read('Path for script archives',
        'Pfad in den die fertigen Scripte archiviert werden')));

    FAllowedSections.Add(TStringIniSection.Create('ReservedNames', 'N',
        TLanguageString.Read('Reserved names that should be prevented from being used as method names',
        'Reservierte Bezeichner, die man nicht als Methodennamen verwenden können soll')));

    FAllowedSections.Add(TDLLMenuItemSection.Create('MenuTool', 'M', TLanguageString.
        Read('Menu items from DLL', 'Menu items from DLL')));

    fItems.Add(TBoolEntry.Create('Washprogram', 'SwitchOffN2C3BeforeWashInnerCh', false, '',
        TLanguageString.Read('Wash program: 1 = Switch off valve N2CANNEL3 to wash inner channel',
        'Waschprogramm: 1 = Beim Waschen des inneren Kanals das Ventil N2CANNEL3 abschalten')));

    fItems.Add(TBoolEntry.Create('Display', 'ShowAnimation', false, '',
        TLanguageString.Read('Show animation in main window', 'Zeigt eine Animation im Hauptfenster')));
    fItems.Add(TBoolEntry.Create('Display', 'UseLiquids', true, '', TLanguageString.
        Read('If set to 0, liquid handling is disabled: The Volumes and Flush Window are not displayed',
        'Wenn es auf 0 gesetzt ist, ist das Liquid Handling nicht möglich. Es wird kein Volumen- oder Spülen-Fenster gezeigt.')
        ));
    fItems.Add(TStringEntry.Create('Display', 'DefaultMethod', '', '',
        TLanguageString.Read('This method will always be started in the Runner',
        'Diese Methode wird im Runner immer gestartet')));
    fItems.Add(TBoolEntry.Create('Display', 'CanSimulateInRealMode', false, '',
        TLanguageString.
        Read('Can a method be started in simulation mode even if the Runner was started in real mode?  0= Simulation not possible; 1= Simulation is possible.',
        'Ist es möglich eine Methode im Simulationsmodus zu starten obwhol der Runner in Realmodus gestartet wurde?  0= Simulation ist nicht möglich; 1= Simulation ist möglich.')
        ));

    fItems.Add(TStringEntry.Create('Beep', 'DLLName', '', '', TLanguageString.Read('Beep DLLName',
        'Beep DLLName')));
    fItems.Add(TStringEntry.Create('Beep', 'DLLFunction', '', '', TLanguageString.Read('Beep DLLFunction',
        'Beep DLLFunction')));
    fItems.Add(TStringEntry.Create('Beep', 'DLLOffFunction', '', '', TLanguageString.
        Read('Beep DLLOffFunction', 'Beep DLLOffFunction')));
    fItems.Add(TIntegerEntry.Create('Beep', 'Time', 50, '', TLanguageString.Read('Beep Time', 'Beep Time')));
    fItems.Add(TIntegerEntry.Create('Beep', 'Sound', 16, '', TLanguageString.Read('Beep Sound',
        'Beep Sound')));

    FAllowedSections.Add(TStringIniSection.Create('Wiper', '', TLanguageString.Read('Wiper racks section',
        'Wiper racks section')));

    fItems.Add(TBoolEntry.Create('Display', 'InitAtMethodEnd', false, '',
        TLanguageString.Read('Forces init each time a run was completed',
        'Zwangsinit nach dem Ende eines jeden Laufs')));

    fItems.Add(TIntegerEntry.Create('PosPressCtrl', 'RequestTime', 500, '',
        TLanguageString.Read('PosPressCtrl RequestTime', 'PosPressCtrl RequestTime')));
    fItems.Add(TIntegerEntry.Create('PosPressCtrl', 'StartDelay', 500, '',
        TLanguageString.Read('PosPressCtrl StartDelay', 'PosPressCtrl StartDelay')));
    fItems.Add(TIntegerEntry.Create('PosPressCtrl', 'MoveZDelay', 500, '',
        TLanguageString.Read('PosPressCtrl MoveZDelay', 'PosPressCtrl MoveZDelay')));
    fItems.Add(TIntegerEntry.Create('PosPressCtrl', 'EndDelay', 0, '',
        TLanguageString.Read('PosPressCtrl EndDelay', 'PosPressCtrl EndDelay')));
    fItems.Add(TIntegerEntry.Create('PosPressCtrl', 'ScanSpeed', 0, '',
        TLanguageString.Read('PosPressCtrl ScanSpeed', 'PosPressCtrl ScanSpeed')));
    fItems.Add(TIntegerEntry.Create('PosPressCtrl', 'RetractSpeed', 0, '',
        TLanguageString.Read('PosPressCtrl RetractSpeed', 'PosPressCtrl RetractSpeed')));

    fItems.Add(TIntegerEntry.Create(STR_ISEC_LASTRBLOCK, 'Columns', 12, '',
        TLanguageString.Read('Editor: Last reaction block columns', 'Editor: Last reaction block columns')));
    fItems.Add(TIntegerEntry.Create(STR_ISEC_LASTRBLOCK, 'Rows', 8, '',
        TLanguageString.Read('Editor: Last reaction block Rows', 'Editor: Last reaction block Rows')));
    fItems.Add(TIntegerEntry.Create(STR_ISEC_LASTRBLOCK, 'VolumeF1000', 100000, '',
        TLanguageString.Read('Editor: Last reaction block VolumeF1000',
        'Editor: Last reaction block VolumeF1000')));

    fItems.Add(TIntegerEntry.Create(STR_ISEC_SYSLIQVALVE, 'UsePeriAtWash', 1, '',
        TLanguageString.Read('UsePeriAtWash', 'UsePeriAtWash')));
    fItems.Add(TIntegerEntry.Create(STR_ISEC_SYSLIQVALVE, 'FlushVol', 1000, 'µl',
        TLanguageString.Read('Volume of liquid with which is washed each time the system liquid is changed',
        'Waschvolumen beim Wechseln der Systemflüssigkeiten')));
    FAllowedSections.Add(TLiquidPortDataSection.Create(STR_ISEC_SYSLIQVALVE, 'Port%d',
        TLanguageString.Read('Data of the system liquid', 'Daten der Systemflüssigkeit')));

    fItems.Add(TStringEntry.Create('Backup', 'Path', '.\Backup', '', TLanguageString.Read('Backup path',
        'Backup-Pfad')));
    FAllowedSections.Add(TBackupPathDataSection.Create('BackupAddFiles', '',
        TLanguageString.Read('Additional Backup files', 'Zusätzliche Backup-Verzeichnisse')));
    fItems.Add(TBoolEntry.Create('Backup', 'MailEnabled', false, '', TLanguageString.
        Read('Activate mail function', 'Mail-Funktion aktivieren')));
    fItems.Add(TStringEntry.Create('Backup', 'MailAddresses', 'Support@zinsser-analytic.com', '',
        TLanguageString.Read('Send to ...', 'Senden an ..')));

    fItems.Add(TStringEntry.Create(STR_ISEC_ACTIONDEPENDENT, 'DontPutBackTool', '', '',
        TLanguageString.
        Read('Do not put back tool for Actions that appear in this list (eg. DLLFu,Devic,Flush)',
        'Tool nicht zurückstellen für die Actions die in dieser Liste erscheinen (zB. DLLFu,Devic,Flush)')));

    fItems.Add(TStringEntry.Create('CapParkRack', 'CapParkStrategy', 'FIFO', '',
        TLanguageString.
        Read('FIFO = Put cap to first position, take from first pos. FILO = Put on first, take from last pos.',
        'FIFO = Stell Kappe auf erste Position, nimm von der ersten Pos. FILO = Stell auf erste, nimm von letzter Pos.')
        ));

    fItems.Add(TDLLMenuItemEntry.Create('CompleteMethodImport', 'ImportButton',
        TLanguageString.Read('Complete Method Import: Button', 'Complete Method Import: Button')));
    fItems.Add(TStringEntry.Create('View', 'VisiblemenuItems', '', '',
        TLanguageString.Read('Visible menu items in All-Items-View',
        'Sichtbare Menüpunkte in Übersicht >Alle Einträge<')));

    fItems.Add(TStringEntry.Create('Graphics', 'GraphicsType', '2D', '', TLanguageString.Read('Graphics Type',
        'Grafiktyp')));

    fItems.Add(TBoolEntry.Create('Restart', 'Enabled', true, '', TLanguageString.
        Read('Is Restart Allowed: 1= Allowed; 0= Not allowed',
        'Ist Restart erlaubt: 1= Erlaubt; 0= Nicht erlaubt')));

    fItems.Add(TBoolEntry.Create('Restart', 'OnlyAtRestartMarkers', false, '',
        TLanguageString.
        Read('Restart is only allowed at Restart Marker (RESTA) actions: 1= only at Restart Marker (RESTA) actions; 0= at any steps',
        'Restart ist nur bei Restart Marker (RESTA) Aktionen erlaubt: 1= nur bei Restart Marker (RESTA) Aktionen, 0= bei jedem Schritt')
        ));

    fItems.Add(TStringEntry.Create('MailSender', 'SendHostIP', '', '', TLanguageString.Read('', '')));
    fItems.Add(TIntegerEntry.Create('MailSender', 'SendPort', 25, '', TLanguageString.Read('', '')));
    fItems.Add(TStringEntry.Create('MailSender', 'Address', '', '', TLanguageString.Read('', '')));
    fItems.Add(TStringEntry.Create('MailSender', 'Password', '', TLanguageString.Read('', ''), true));
    fItems.Add(TStringEntry.Create('MailSender', 'ErrorAutomaticRecipients', '', '',
        TLanguageString.Read('Names of recipients (comma separated)',
        'Namen der Empfänger (mit Komma getrennt)')));
    fItems.Add(TStringEntry.Create('MailSender', 'ErrorAutomaticCCRecipients', '', '',
        TLanguageString.Read('Names of CC recipients (comma separated)',
        'Namen der CC-Empfänger (mit Komma getrennt)')));

    fItems.Add(TBoolEntry.Create('RemoteControl', 'UseTCPIPInterface', false, '',
        TLanguageString.Read('Use TCPIP interface in ZARunner for remote control (1=Enabled,0=Disabled)',
        'TCPIP-Schnittstelle in ZARunner zur Fernsteuerung nutzen (1=eingeschaltet,0=ausgeschaltet')));

    fItems.Add(TStringEntry.Create('SubstanceSet', 'AutomaticLoadSet', 'AUTOLOAD', '',
        TLanguageString.Read('Substance-Set that is automatically loaded when starting any method',
        'Substanz-Set, dass automatisch bei jedem Methoden-Start geladen wird')));
    fItems.Add(TStringEntry.Create('SubstanceSet', 'AutomaticRefreshSet', '', '',
        TLanguageString.
        Read('The volumes of this Set will automatically be changed in the SubstanceSet table (without SubstSave)',
        'Die Volumen dieses Sets werden automatisch in der SubstanceSet-Tablle geändert (ohne SubstSave)')));
    fItems.Add(TStringEntry.Create('Display', 'TablesEditableInRunner', '', '',
        TLanguageString.
        Read('Tables in that list can be edited via the Tools menue of ZARunner (eg. CARRIER,RACKS)',
        'Tabellen aus der Liste können im ZARunner durch Aufruf im Tools Menü editiert werden (z.B. CARRIER,RACKS)')
        ));
    fItems.Add(TIntegerEntry.Create('Display', 'EnableCalibration', 0, '',
        TLanguageString.
        Read('0=No Calibration available in ZARunner, 1=Calibration can be executed in ZARunner',
        '0=Keine Kalibrierung aktiviert im ZARunner, 1=Kalibrierung in ZARunner möglich')));
end;

{ TZPDLLIniAccess }

procedure TZPDLLIniAccess.CreateAllowedEntries;
begin
    inherited CreateAllowedEntries;

    // ----------------------------------------------------------------------------------- H+P Rührer
    fItems.Add(TIntegerEntry.Create('MagStir40CT.dll', 'ComPort', -1, '', ''));
    fItems.Add(TIntegerEntry.Create('MagStir40CT.dll', 'Baud', 9600, '', ''));
    fItems.Add(TIntegerEntry.Create('MagStir40CT.dll', 'Timeout', 2, '', ''));
    fItems.Add(TIntegerEntry.Create('MagStir40CT.dll', 'SleepTime', 10, '', ''));
end;

{ TWinlissyIniAccess }

function TWinlissyIniAccess.ReadBcscanrange(aSection, aIdent: string): BCSCANRANGE;
var
    xIniEntry: TBcscanrangeEntry;
begin
    xIniEntry := TBcscanrangeEntry.CreateWithoutDefault(aSection, aIdent);
    ReadAllowedValue(xIniEntry);
    result := xIniEntry.BcscanrangeValue;
    xIniEntry.Free;
end;

function TWinlissyIniAccess.ReadBcrposition(aSection, aIdent: string): BCRPOSITION;
var
    xIniEntry: TBcrpositionEntry;
begin
    xIniEntry := TBcrpositionEntry.CreateWithoutDefault(aSection, aIdent);
    ReadAllowedValue(xIniEntry);
    result := xIniEntry.BcrpositionValue;
    xIniEntry.Free;
end;

function TWinlissyIniAccess.ReadDLLMenuItem(aSection, aIdent: string): TDLLMenuItem;
var
    xIniEntry: TDLLMenuItemEntry;
begin
    xIniEntry := TDLLMenuItemEntry.CreateWithoutDefault(aSection, aIdent);
    ReadAllowedValue(xIniEntry);
    result := xIniEntry.DLLMenuItemValue;
    xIniEntry.Free;
end;

function TWinlissyIniAccess.ReadTubeToolData(aSection, aIdent: string): TTubeToolData;
var
    xIniEntry: TTubeToolDataEntry;
begin
    xIniEntry := TTubeToolDataEntry.CreateWithoutDefault(aSection, aIdent);
    ReadAllowedValue(xIniEntry);
    result := xIniEntry.TubeToolDataValue;
    xIniEntry.Free;
end;

function TWinlissyIniAccess.ReadLiquidPortData(aSection, aIdent: string): TLiquidPortData;
var
    xIniEntry: TLiquidPortDataEntry;
begin
    xIniEntry := TLiquidPortDataEntry.CreateWithoutDefault(aSection, aIdent);
    ReadAllowedValue(xIniEntry);
    result := xIniEntry.LiquidPortDataValue;
    xIniEntry.Free;
end;

function TWinlissyIniAccess.ReadDBFieldDefinition(aSection, aIdent: string): TDBFieldDefinition;
var
    xIniEntry: TDBFieldDefinitionEntry;
begin
    xIniEntry := TDBFieldDefinitionEntry.CreateWithoutDefault(aSection, aIdent);
    ReadAllowedValue(xIniEntry);
    result := xIniEntry.DBFieldDefinitionValue;
    xIniEntry.Free;
end;

function TWinlissyIniAccess.ReadBackupPathData(aSection, aIdent: string): TBackupPathData;
var
    xIniEntry: TBackupPathDataEntry;
begin
    xIniEntry := TBackupPathDataEntry.CreateWithoutDefault(aSection, aIdent);
    ReadAllowedValue(xIniEntry);
    result := xIniEntry.BackupPathDataValue;
    xIniEntry.Free;
end;


end.
