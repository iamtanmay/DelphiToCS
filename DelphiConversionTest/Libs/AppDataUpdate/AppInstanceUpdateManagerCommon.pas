{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Initializes UpdateManager
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                               TN4139   Workspace Table Update
  24.06.08 pk                               TN4148   uses UpdateManagerCommonTypes instead of CommonTypes
  24.06.08 pk  AddUpdates                   TN4139   New Updates: TLayoutWorkspaceTableUpdateV1, etc
  03.07.08 wl                               TN4157   5 new Updates of Run, Method, Volcorr, LiqH and Tipset table
  10.07.08 wl                               TN4164   new: WorkspaceDevicesTable
  20.09.08 pk                               TN4215   new TMethodTableUpdateV3_1
  25.09.08 wl                               TN4242   neue Updates für Method und LiqH
  29.09.08 wl                               TN4242   4 weitere Updates: MethodName hat jetzt 50 chars
  29.10.08 wl                               TN4290   Update-Nummern geändert
  29.10.08 wl  AddUpdates                   TN4290   TRacksTableUpdateV3
  02.12.08 pk  AddUpdates                   TN4337   Convert RUNST to ADDM
  20.01.09 wl  AddUpdates                   TN4358   TSettingsTableUpdateV1_3 für ZP04/LinearMotor01-Trennung
  19.02.09 pk  AddUpdates                   TN4232   TMethodTableUpdateV4_2
  19.02.09 pk  AddUpdates                   TN4346   TImportDataTypesTableUpdateV1
  13.03.09 ts  AddUpdates                   TN4464   TMethodSettingsTableUpdateV2_1
  16.03.09 ts  AddUpdates                   TN4471   TMethodSettingsTableUpdateV2_2 Methodnames in METHODSETTINGS wie in METHOD
  16.04.09 ts  AddUpdates                   TN4477   TDataHistoryTableUpdateV1
  27.05.09 wl  AddUpdates                   TN4476   TImportDataTypesTableStructDefV2: Feld IMPORTFILENAME entfernt
  09.06.09 pk  AddUpdates                   TN3979   TMethodTableUpdateV2
  19.06.09 ts  AddUpdates                   TN4618   TSettingsTableUpdateV2
  31.07.09 wl  AddUpdates                   TN4693   TLiqHTableUpdateV3_1 und TLiqClassTableUpdateV2
  05.08.09 wl  AddUpdates                   TN4705   TSettingsTableUpdateV2_1
  23.02.10 ts  AddUpdates                   TN4992   TSettingsTableUpdateV2_2
  31.05.10 wl  AddUpdates                   TN5120   TMethodTableUpdateV4_2
  22.06.10 pk  AddUpdates                   TN5088   TMethodTableUpdateV4_4
  29.06.10 pk  AddUpdates                   TN5173   TLiqHTableUpdateV4
  22.07.10 wl  AddUpdates                   TN5184   TLiqHTableUpdateV4_1
  16.08.10 wl  Achtung: TSettingsTableUpdate V2_3, V2_4, V2_5 gibt es nicht!
  16.08.10 wl  AddUpdates                   TN5222.1 TSettingsTableUpdateV2_6: FixAfterVortexing anders, CATMix wird zu Mixer01
  26.08.10 ts  AddUpdates                   TN5248   TLiqHTableUpdateV5
  22.09.10 wl  AddUpdates                   TN5275   TLiqHTableUpdateV5_1
  29.09.10 pk  AddUpdates                   TN5283   TMethodTableUpdateV4_5 Short and Long method step comments combined
  30.09.10 pk  AddUpdates                   TN5287   TMethodTableUpdateV4_6
  25.11.10 wl  AddUpdates                   TN5355   TSettingsTableUpdateV2_6: Delay-Settings haben jetzt section 'Delay'
  30.11.10 wl  AddUpdates                   TN5373   TSQLTermsTableUpdateV1
  20.01.11 wl  AddUpdates                   TN5439   TWashprogTableUpdateV1
  31.01.11 wl  AddUpdates                   TN5440   Reihenfolge geändert: erst TSettingsTableUpdateV1_2, dann TLayoutTableUpdateV2 und TRacksTableUpdateV2
  15.02.11 pk                               TN4780   changes needed to make UpdateManager compatible with TurboDB
  21.02.11 wl  AddUpdates                   TN5455   TMethodTableUpdateV2_1
  24.02.11 wl  AddUpdates                   TN5431   TRacksTableUpdateV4
  06.03.11 pk  Create                       TN4780   comments removed
  12.04.11 ts  AddUpdates                   TN5548   INT_IMPORTDEF_TABLE_UPDATE_V2
  05.05.11 ts  AddUpdates                   TN5552   TRacksTableUpdateV5
  30.06.11 wl  AddUpdates                   TN5618   TMethodTableUpdateV4_7
  30.06.11 wl  AddUpdates                   TN5603   TSequenceTableUpdateV2
  19.07.11 wl  AddUpdates                   TN5630   new: TLiqHTableStructDefV5_2
  22.07.11 wl  AddUpdates                   TN5619   TImportFileDefTableUpdateV2_1
  05.09.11 ts  AddUpdates                   TN5683   TRacksTableUpdateV6
  11.09.11 wl                               TN5672   Instanz von fAppInstanceLocalDataProvider wird hier verwaltet
  20.10.11 wl  AddUpdates                   TN5723   TPosinfoTableUpdateV1
  28.10.11 wl  AddUpdates                   TN5728   TDisposableTipTableUpdateV1
  28.10.11 wl  AddUpdates                   TN5729   TSubstanceSetTableUpdateV1
  28.10.11 wl  AddUpdates                   TN5730   TPipetteListTableUpdateV1
  31.10.11 wl  AddUpdates                   TN5725   TSubstanceDataTableUpdateV1
  17.11.11 ts  AddUpdates                   TN5702   TRacksTableUpdateV7, TDiscreteRacksTableUpdateV1 (DiscretePositions)
  30.11.11 wl  AddUpdates                   TN5755   TPipetteListTableUpdateV2
  02.12.11 wl  AddUpdates                   TN5758   TLiqHTableUpdateV6
  14.12.11 wl                               TN5765   Run- und Sesion-Updates deaktiviert
  19.12.11 wl  AddUpdates                   TN5771   TImportColDefTableUpdateV1 wird jetzt erst später verwendet
  27.12.11 wl  AddUpdates                   TN5773   TPipetteListTableStructDefV3
  15.02.12 wl  AddUpdates                   TN5803   TSettingsTableUpdateV2_8: Updater für BCTurntable-Device
  23.02.12 wl  AddUpdates                   TN5818   TLiqHTableStructDefV7: SampleAspTipTouchScan,-Submerge,DispTipTouchScan,-Submerge = 'SampleAspTipTouchScan';
  05.07.12 wl  AddUpdates                   TN5931   TPipetteListTableStructDefV4, neue Felder: RunSourcePosX, ...
  05.07.12 wl  AddUpdates                   TN5927   TPipetteListTableStructDefV5: DILNAME ist jetzt ein string-Feld
  10.08.12 wl  AddUpdates                   TN5947   TLiqHTableStructDefV8: Neues Feld: WashUsePeripump
  04.09.12 wl  AddUpdates                   TN5972   TLiqHTableStructDefV9: Neues Feld: SampleAspSpitBackAtAspPos
  10.09.12 wl  AddUpdates                   TN5979   TSubstanceSetTableUpdateV2: neu: MinVolume2
  10.09.12 ts  AddUpdates                   TN5977   TRacksTableUpdateV8: neues Feld: RackMoveZTravelOffset
  13.11.12 wl  AddUpdates                   TN6015   TDisplayComponentsTableUpdateV1: DisplayComponents von Settings trennen
  13.11.12 wl  AddUpdates                   TN6015   TSettingsTableUpdateV2_9: DisplayComponents von Settings trennen
  14.12.12 wl  AddUpdates                   TN6054   neu: TRunVarTableUpdateV3
  14.12.12 wl  AddUpdates                   TN6055   neu: TMethodVariablesTableUpdateV1
  02.01.13 wl  AddUpdates                   TN6064   TLiqHTableStructDefV10: Neues Feld: DispMixAspSubmerge
  07.01.13 ts  AddUpdates                   TN6065   TWashProgramTableUpdateV2: neues Feld: CleanHeight
  20.02.13 wl  AddUpdates                   TN6055   TSettingsTableUpdateV2_10: MethodParams von Settings trennen
  08.03.13 wl  AddUpdates                   TN6095   TMethodVarPagesTableUpdateV1
  16.04.13 pp  AddUpdates                   TN6131   TDiscreteCarriersTableUpdateV1
  07.05.13 ts  AddUpdates                   TN6118   TLiqHTableUpdateV11: neues Feld: DispTipTouchScanStoreVol
  12.06.13 wl  AddUpdates                   TN6172   TPipetteListTableUpdateV6: neue Felder für den aktuellen Status
  30.08.13 wl  AddUpdates                   TN6236   TMethodSettingsTableUpdateV3: Komplett neue Struktur für MethodSettings
  02.09.13 wl  AddUpdates                   TN6239   TPipetteListTableUpdateV7: NAME hat jetzt 50 Character
  30.09.13 wl  AddUpdates                   TN6260   TMethodTableUpdateV4_8: eventuell vorhandene Volumen löschen, wenn Cycles > 0
  21.10.13 wl  AddUpdates                   TN6276   TMethodTableUpdateV4_9: Geänderte Multi-Pipetting-Parameter
  07.11.13 wl  AddUpdates                   TN6274   TSettingsTableUpdateV2_11: REMPIPETTE wird durch ATTACHEDPIPETTE ersetzt
  -------------------------------------------------------------------------------------------------- }

unit AppInstanceUpdateManagerCommon;


interface


uses
    Windows,
    Controls,
    AppInstanceLocalDataProviderLib,
    DatabaseProvider;

type
    TAppInstanceUpdateManagerCommon = class
    private const
        // Updates vor Version 7.3: >= 90
        INT_UPDATESTATUS_TABLE_UPDATE_V1 = 90;

        INT_TIPTYPE_TABLE_UPDATE_V1 = 99;
        INT_METHODSETTINGS_TABLE_UPDATE_V1 = 100;

        INT_SETTINGS_TABLE_UPDATE_V1 = 101;
        INT_METHOD_TABLE_UPDATE_V1 = 102;
        // INT_SESSION_TABLE_UPDATE_V1 = 103;
        INT_LAYOUT_TABLE_UPDATE_V1 = 104;
        // INT_RUN_TABLE_UPDATE_V1 = 105;
        INT_CARRIER_TABLE_UPDATE_V1 = 106;
        INT_RACKS_TABLE_UPDATE_V1 = 107;
        INT_IMPORTCOLDEF_TABLE_UPDATE_V1 = 108;
        INT_IMPORTDEF_TABLE_UPDATE_V1 = 109;
        INT_LIQCLASS_TABLE_UPDATE_V1 = 110;
        INT_RESOURCES_TABLE_UPDATE_V1 = 111;
        INT_RESSCHEME_TABLE_UPDATE_V1 = 112;
        INT_RUNVAR_TABLE_UPDATE_V1 = 113;
        INT_SEQUENCE_TABLE_UPDATE_V1 = 114;
        INT_IMPORTDATATYPES_TABLE_UPDATE_V1 = 115;
        INT_DATAHISTORY_TABLE_UPDATE_V1 = 116;
        INT_SQLTERMS_TABLE_UPDATE_V1 = 117;
        INT_WASHPROGRAM_TABLE_UPDATE_V1 = 118;

        INT_METHODSETTINGS_TABLE_UPDATE_V1_5 = 119;

        // INT_SESSION_TABLE_UPDATE_V2 = 120;
        INT_CARRIER_TABLE_UPDATE_V2 = 121;
        INT_METHOD_TABLE_UPDATE_V2 = 122;

        INT_TIPTYPE_TABLE_UPDATE_V2 = 123;

        INT_SETTINGS_DATA_UPDATE_V1_1 = 129;
        INT_SETTINGS_DATA_UPDATE_V1_2 = 130;
        INT_METHOD_TABLE_UPDATE_V2_1 = 131;
        INT_LAYOUT_TABLE_UPDATE_V2 = 132;
        INT_RACKS_TABLE_UPDATE_V2 = 133;

        INT_WORKSPACE_TABLE_UPDATE_V1 = 134;
        INT_WORKSPACE_DATA_UPDATE_V1_1 = 135;
        INT_LAYOUT_TABLE_UPDATE_V3 = 136;
        INT_LAYOUTWORKSPACE_TABLE_UPDATE_V1 = 137;
        INT_LAYOUTWORKSPACE_DATA_UPDATE_V1_1 = 138;
        INT_LAYOUT_DATA_UPDATE_V3_1 = 139;

        // INT_RUN_TABLE_UPDATE_V2 = 140;
        INT_TIPSET_TABLE_UPDATE_V2 = 141;
        INT_LIQH_TABLE_UPDATE_V2 = 142;
        INT_VOLCORR_TABLE_UPDATE_V2 = 143;
        INT_METHOD_TABLE_UPDATE_V3 = 144;

        INT_WORKSPACEDEV_TABLE_UPDATE_V1 = 150;
        INT_WORKSPACE_TABLE_UPDATE_V2 = 151;
        INT_WORKSPACE_TABLE_UPDATE_V2_1 = 152;

        INT_METHOD_TABLE_UPDATE_V3_1 = 158;
        INT_RACKS_TABLE_UPDATE_V3 = 159;

        INT_METHOD_TABLE_UPDATE_V4 = 161;
        INT_LIQH_TABLE_UPDATE_V3 = 162;
        INT_METHODSETTINGS_TABLE_UPDATE_V2 = 163;
        INT_LAYOUT_TABLE_UPDATE_V4 = 164;
        INT_RUNVAR_TABLE_UPDATE_V2 = 165;
        // INT_SESSION_TABLE_UPDATE_V3 = 166;
        INT_METHOD_TABLE_UPDATE_V4_1 = 167;
        INT_METHOD_TABLE_UPDATE_V4_2 = 168;

        INT_SETTINGS_DATA_UPDATE_V1_3 = 170;

        INT_METHODSETTINGS_TABLE_UPDATE_V2_1 = 171;
        INT_METHODSETTINGS_TABLE_UPDATE_V2_2 = 172;

        INT_IMPORTDATATYPES_TABLE_UPDATE_V2 = 173;

        INT_SETTINGS_TABLE_UPDATE_V2 = 174;

        INT_LIQH_TABLE_UPDATE_V3_1 = 175;
        INT_LIQCLASS_TABLE_UPDATE_V2 = 176;
        INT_SETTINGS_TABLE_UPDATE_V2_1 = 177;
        INT_SETTINGS_TABLE_UPDATE_V2_2 = 178;

        INT_METHOD_TABLE_UPDATE_V4_3 = 180;
        INT_METHOD_TABLE_UPDATE_V4_4 = 181;

        INT_LIQH_TABLE_UPDATE_V4 = 182;
        INT_LIQH_TABLE_UPDATE_V4_1 = 183;
        INT_SETTINGS_TABLE_UPDATE_V2_6 = 184;
        INT_LIQH_TABLE_UPDATE_V5 = 185;
        INT_LIQH_TABLE_UPDATE_V5_1 = 186;

        INT_SETTINGS_TABLE_UPDATE_V2_7 = 188;

        INT_METHOD_TABLE_UPDATE_V4_5 = 190;
        INT_METHOD_TABLE_UPDATE_V4_6 = 191;

        INT_RACKS_TABLE_UPDATE_V4 = 193;
        INT_RACKS_TABLE_UPDATE_V5 = 194;

        INT_IMPORTDEF_TABLE_UPDATE_V2 = 195;
        INT_LIQH_TABLE_UPDATE_V5_2 = 196;
        INT_METHOD_TABLE_UPDATE_V4_7 = 197;

        INT_SEQUENCE_TABLE_UPDATE_V2 = 200;
        INT_IMPORTDEF_TABLE_UPDATE_V2_1 = 201;
        INT_RACKS_TABLE_UPDATE_V6 = 202;

        INT_RACKS_TABLE_UPDATE_V7 = 203;
        INT_DISCRETERACKS_TABLE_UPDATE_V1 = 204;
        INT_POSINFO_TABLE_UPDATE_V1 = 205;
        INT_SUBSTANCESET_TABLE_UPDATE_V1 = 207;
        INT_DISPOSABLETIP_TABLE_UPDATE_V1 = 208;
        INT_PIPETTELIST_TABLE_UPDATE_V1 = 209;
        INT_POSINFO_TABLE_UPDATE_V1_1 = 210;
        INT_SUBSTANCEDATA_TABLE_UPDATE_V1 = 211;
        INT_PIPETTELIST_TABLE_UPDATE_V2 = 214;
        INT_LIQH_TABLE_UPDATE_V6 = 215;
        INT_PIPETTELIST_TABLE_UPDATE_V3 = 216;

        INT_SETTINGS_TABLE_UPDATE_V2_8 = 218;
        INT_LIQH_TABLE_UPDATE_V7 = 219;
        INT_PIPETTELIST_TABLE_UPDATE_V4 = 220;
        INT_PIPETTELIST_TABLE_UPDATE_V5 = 221;

        INT_LIQH_TABLE_UPDATE_V8 = 223;
        INT_LIQH_TABLE_UPDATE_V9 = 224;

        INT_SUBSTANCESET_TABLE_UPDATE_V2 = 226;
        INT_LIQH_TABLE_UPDATE_V10 = 227;

        INT_RACKS_TABLE_UPDATE_V8 = 228;
        INT_DISPLAYCOMPONENTS_TABLE_UPDATE_V1 = 231;
        INT_SETTINGS_TABLE_UPDATE_V2_9 = 232;
        INT_WASHPROGRAM_TABLE_UPDATE_V2 = 235;
        INT_PIPETTELIST_TABLE_UPDATE_V6 = 237;
        INT_LIQH_TABLE_UPDATE_V11 = 238;
        INT_METHOD_TABLE_UPDATE_V4_8 = 239;

        INT_RUNVAR_TABLE_UPDATE_V3 = 241;
        INT_METHODVARIABLES_TABLE_UPDATE_V1 = 242;
        INT_SETTINGS_TABLE_UPDATE_V2_10 = 243;
        INT_METHODVARPAGES_TABLE_UPDATE_V1 = 244;

        INT_DISCRETECARRIERS_TABLE_UPDATE_V1 = 247;
        INT_CARRIER_TABLE_UPDATE_V3 = 248;
        INT_METHODSETTINGS_TABLE_UPDATE_V3 = 250;
        INT_PIPETTELIST_TABLE_UPDATE_V7 = 251;
        INT_METHOD_TABLE_UPDATE_V4_9 = 253;
        INT_SETTINGS_TABLE_UPDATE_V2_11 = 255;
        // Mut zur Lücke! Ab und zu mal ein paar Zahlen auslassen..
    private
        fDatabaseProvider: TDatabaseProvider;
        fTempDatabaseProvider: TDatabaseProvider;
        fAppInstanceLocalDataProvider: TAppInstanceLocalDataProvider;
        class var uInstance: TAppInstanceUpdateManagerCommon; // Single Instance
        constructor Create(const aDataPath, aDBPath, aTempDBPath, aUpdatesPath: string);
    public
        destructor Destroy(); override;
        procedure AddUpdates();
        class function CreateInstance(const aDataPath, aDBPath, aTempDBPath, aUpdatesPath: string)
            : TAppInstanceUpdateManagerCommon;
        class function Instance(): TAppInstanceUpdateManagerCommon;
    end;


implementation


uses
    SysUtils,
    UpdateManager,
    Update,
    UpdateManagerCommonTypes,
    DataProviderFactory,
    UpdaterDataProviderFactory,
    DatabaseProviderManagerTypeDictionary,

    TableUpdate,
    // table updates:
    SettingsTableUpdate,
    ImportColDefTableUpdate,
    ImportDefTableUpdate,
    ImportDataTypesTableUpdate,
    DataHistoryTableUpdate,
    CarrierTableUpdate,
    LayoutTableUpdate,
    RackTableUpdate,
    PosinfoTableUpdate,
    LiqClassTableUpdate,
    ResourcesTableUpdate,
    ResSchemeTableUpdate,
    MethodTableUpdate,
    RunVarTableUpdate,
    SequenceTableUpdate,
    TipsetTableUpdate,
    LiqHTableUpdate,
    WorkspaceTableUpdate,
    LayoutWorkspaceTableUpdate,
    VolcorrTableUpdate,
    StepsToMMUpdate,
    IniToSettingsDB,
    WashprogTableUpdate,
    WorkspaceDevicesTableUpdate,
    MethodSettingsTableUpdate,
    SQLTermsTableUpdate,
    UpdateStatusTableUpdate,
    TipTypeTableUpdate,
    ZP04Update,
    DisplayComponentsTableUpdate,
    ImportFileDefTableUpdate,
    MethodVariablesTableUpdate,
    MethodVarPagesTableUpdate,
    DisposableTipTableUpdate,
    SubstanceSetTableUpdate,
    SubstanceDataTableUpdate,
    PipetteListTableUpdate,
    DiscreteRacksTableUpdate,
    DiscreteCarriersTableUpdate;

{ TAppInstanceUpdateManagerCommon }

constructor TAppInstanceUpdateManagerCommon.Create(const aDataPath, aDBPath, aTempDBPath,
    aUpdatesPath: string);
begin
    inherited Create();
    gUpdateManager := TUpdateManager.Create();
    TUpdatePaths.CreateInstance(aDataPath, aDBPath, aTempDBPath, aUpdatesPath);
    TLogManager.CreateInstance();
    fAppInstanceLocalDataProvider := TAppInstanceLocalDataProvider.Create;
    fAppInstanceLocalDataProvider.SetTypeDictionary(TDatabaseProviderManagerTypeDictionary.Create(true));

    TDataProviderFactory.Instance.Initialize('');

    fDatabaseProvider := TUpdaterDataProviderFactory.CreateStandardDatabaseProvider(aDBPath);

    fTempDatabaseProvider := TUpdaterDataProviderFactory.CreateStandardDatabaseProvider(aTempDBPath);
end;

destructor TAppInstanceUpdateManagerCommon.Destroy;
begin
    FreeAndNil(fTempDatabaseProvider);
    FreeAndNil(fDatabaseProvider);
    FreeAndNil(fAppInstanceLocalDataProvider);
    TLogManager.DestroyInstance();
    gUpdateManager.Free;
    inherited;
end;

procedure TAppInstanceUpdateManagerCommon.AddUpdates();
begin
    if gUpdateManager.Updates.Count > 0 then
        EXIT;
    gUpdateManager.AddUpdate(TUpdateStatusTableUpdateV1.Create(INT_UPDATESTATUS_TABLE_UPDATE_V1));

    gUpdateManager.AddUpdate(TMethodSettingsTableUpdateV1.Create(INT_METHODSETTINGS_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TSettingsTableUpdateV1.Create(INT_SETTINGS_TABLE_UPDATE_V1));
    // gUpdateManager.AddUpdate(TSessionTableUpdateV1.Create(INT_SESSION_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TMethodTableUpdateV1.Create(INT_METHOD_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TLayoutTableUpdateV1.Create(INT_LAYOUT_TABLE_UPDATE_V1));
    // gUpdateManager.AddUpdate(TRunTableUpdateV1.Create(INT_RUN_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TCarrierTableUpdateV1.Create(INT_CARRIER_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TRacksTableUpdateV1.Create(INT_RACKS_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TImportDefTableUpdateV1.Create(INT_IMPORTDEF_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TLiqClassTableUpdateV1.Create(INT_LIQCLASS_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TResourcesTableUpdateV1.Create(INT_RESOURCES_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TResSchemeTableUpdateV1.Create(INT_RESSCHEME_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TRunVarTableUpdateV1.Create(INT_RUNVAR_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TSequenceTableUpdateV1.Create(INT_SEQUENCE_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TImportDataTypesTableUpdateV1.Create(INT_IMPORTDATATYPES_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TDataHistoryTableUpdateV1.Create(INT_DATAHISTORY_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TSQLTermsTableUpdateV1.Create(INT_SQLTERMS_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TWashprogTableUpdateV1.Create(INT_WASHPROGRAM_TABLE_UPDATE_V1));

    // gUpdateManager.AddUpdate(TSessionTableUpdateV2.Create(INT_SESSION_TABLE_UPDATE_V2));
    gUpdateManager.AddUpdate(TCarrierTableUpdateV2.Create(INT_CARRIER_TABLE_UPDATE_V2));

    gUpdateManager.AddUpdate(TMethodSettingsTableUpdateV1_5.Create(INT_METHODSETTINGS_TABLE_UPDATE_V1_5));
    gUpdateManager.AddUpdate(TMethodTableUpdateV2.Create(INT_METHOD_TABLE_UPDATE_V2));

    gUpdateManager.AddUpdate(TSettingsTableUpdateV1_1.Create(INT_SETTINGS_DATA_UPDATE_V1_1));
    gUpdateManager.AddUpdate(TSettingsTableUpdateV1_2.Create(INT_SETTINGS_DATA_UPDATE_V1_2));
    gUpdateManager.AddUpdate(TMethodTableUpdateV2_1.Create(INT_METHOD_TABLE_UPDATE_V2_1));
    gUpdateManager.AddUpdate(TLayoutTableUpdateV2.Create(INT_LAYOUT_TABLE_UPDATE_V2));
    gUpdateManager.AddUpdate(TRacksTableUpdateV2.Create(INT_RACKS_TABLE_UPDATE_V2));

    gUpdateManager.AddUpdate(TTipTypeTableUpdateV2.Create(INT_TIPTYPE_TABLE_UPDATE_V2));

    gUpdateManager.AddUpdate(TWorkspaceTableUpdateV1.Create(INT_WORKSPACE_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TWorkspaceTableUpdateV1_1.Create(INT_WORKSPACE_DATA_UPDATE_V1_1));
    gUpdateManager.AddUpdate(TLayoutTableUpdateV3.Create(INT_LAYOUT_TABLE_UPDATE_V3));

    gUpdateManager.AddUpdate(TLayoutWorkspaceTableUpdateV1.Create(INT_LAYOUTWORKSPACE_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TLayoutWorkspaceTableUpdateV1_1.Create(INT_LAYOUTWORKSPACE_DATA_UPDATE_V1_1));
    gUpdateManager.AddUpdate(TLayoutTableUpdateV3_1.Create(INT_LAYOUT_DATA_UPDATE_V3_1));

    // gUpdateManager.AddUpdate(TRunTableUpdateV2.Create(INT_RUN_TABLE_UPDATE_V2));
    gUpdateManager.AddUpdate(TTipsetTableUpdateV2.Create(INT_TIPSET_TABLE_UPDATE_V2));
    gUpdateManager.AddUpdate(TLiqHTableUpdateV2.Create(INT_LIQH_TABLE_UPDATE_V2));
    gUpdateManager.AddUpdate(TVolcorrTableUpdateV2.Create(INT_VOLCORR_TABLE_UPDATE_V2));
    gUpdateManager.AddUpdate(TMethodTableUpdateV3.Create(INT_METHOD_TABLE_UPDATE_V3));

    gUpdateManager.AddUpdate(TWorkspaceDevicesTableUpdateV1.Create(INT_WORKSPACEDEV_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TWorkspaceTableUpdateV2.Create(INT_WORKSPACE_TABLE_UPDATE_V2));
    gUpdateManager.AddUpdate(TWorkspaceTableUpdateV2_1.Create(INT_WORKSPACE_TABLE_UPDATE_V2_1));

    gUpdateManager.AddUpdate(TMethodTableUpdateV3_1.Create(INT_METHOD_TABLE_UPDATE_V3_1));

    gUpdateManager.AddUpdate(TMethodTableUpdateV4.Create(INT_METHOD_TABLE_UPDATE_V4));
    gUpdateManager.AddUpdate(TLiqHTableUpdateV3.Create(INT_LIQH_TABLE_UPDATE_V3));
    gUpdateManager.AddUpdate(TMethodSettingsTableUpdateV2.Create(INT_METHODSETTINGS_TABLE_UPDATE_V2));
    gUpdateManager.AddUpdate(TLayoutTableUpdateV4.Create(INT_LAYOUT_TABLE_UPDATE_V4));
    gUpdateManager.AddUpdate(TRunVarTableUpdateV2.Create(INT_RUNVAR_TABLE_UPDATE_V2));
    // gUpdateManager.AddUpdate(TSessionTableUpdateV3.Create(INT_SESSION_TABLE_UPDATE_V3));
    gUpdateManager.AddUpdate(TMethodTableUpdateV4_1.Create(INT_METHOD_TABLE_UPDATE_V4_1));
    gUpdateManager.AddUpdate(TMethodTableUpdateV4_2.Create(INT_METHOD_TABLE_UPDATE_V4_2));

    { TODO -owl : Session.db könnte noch gelöscht werden }
    { TODO -owl : Command.db könnte noch gelöscht werden }

    gUpdateManager.AddUpdate(TRacksTableUpdateV3.Create(INT_RACKS_TABLE_UPDATE_V3));

    gUpdateManager.AddUpdate(TSettingsTableUpdateV1_3.Create(INT_SETTINGS_DATA_UPDATE_V1_3));

    gUpdateManager.AddUpdate(TMethodSettingsTableUpdateV2_1.Create(INT_METHODSETTINGS_TABLE_UPDATE_V2_1));
    gUpdateManager.AddUpdate(TMethodSettingsTableUpdateV2_2.Create(INT_METHODSETTINGS_TABLE_UPDATE_V2_2));

    gUpdateManager.AddUpdate(TImportDataTypesTableUpdateV2.Create(INT_IMPORTDATATYPES_TABLE_UPDATE_V2));

    gUpdateManager.AddUpdate(TSettingsTableUpdateV2.Create(INT_SETTINGS_TABLE_UPDATE_V2));

    gUpdateManager.AddUpdate(TLiqHTableUpdateV3_1.Create(INT_LIQH_TABLE_UPDATE_V3_1));
    gUpdateManager.AddUpdate(TLiqClassTableUpdateV2.Create(INT_LIQCLASS_TABLE_UPDATE_V2));
    gUpdateManager.AddUpdate(TSettingsTableUpdateV2_1.Create(INT_SETTINGS_TABLE_UPDATE_V2_1));
    gUpdateManager.AddUpdate(TSettingsTableUpdateV2_2.Create(INT_SETTINGS_TABLE_UPDATE_V2_2));

    gUpdateManager.AddUpdate(TMethodTableUpdateV4_3.Create(INT_METHOD_TABLE_UPDATE_V4_3));
    // nach hinten verschoben:
    gUpdateManager.AddUpdate(TImportColDefTableUpdateV1.Create(INT_IMPORTCOLDEF_TABLE_UPDATE_V1));

    gUpdateManager.AddUpdate(TMethodTableUpdateV4_4.Create(INT_METHOD_TABLE_UPDATE_V4_4));
    gUpdateManager.AddUpdate(TLiqHTableUpdateV4.Create(INT_LIQH_TABLE_UPDATE_V4));
    gUpdateManager.AddUpdate(TLiqHTableUpdateV4_1.Create(INT_LIQH_TABLE_UPDATE_V4_1));
    gUpdateManager.AddUpdate(TSettingsTableUpdateV2_6.Create(INT_SETTINGS_TABLE_UPDATE_V2_6));
    gUpdateManager.AddUpdate(TLiqHTableUpdateV5.Create(INT_LIQH_TABLE_UPDATE_V5));
    gUpdateManager.AddUpdate(TLiqHTableUpdateV5_1.Create(INT_LIQH_TABLE_UPDATE_V5_1));

    gUpdateManager.AddUpdate(TSettingsTableUpdateV2_7.Create(INT_SETTINGS_TABLE_UPDATE_V2_7));

    gUpdateManager.AddUpdate(TMethodTableUpdateV4_5.Create(INT_METHOD_TABLE_UPDATE_V4_5));
    gUpdateManager.AddUpdate(TMethodTableUpdateV4_6.Create(INT_METHOD_TABLE_UPDATE_V4_6));
    gUpdateManager.AddUpdate(TRacksTableUpdateV4.Create(INT_RACKS_TABLE_UPDATE_V4));
    gUpdateManager.AddUpdate(TImportFileDefTableUpdateV2.Create(INT_IMPORTDEF_TABLE_UPDATE_V2));
    gUpdateManager.AddUpdate(TRacksTableUpdateV5.Create(INT_RACKS_TABLE_UPDATE_V5));
    gUpdateManager.AddUpdate(TLiqHTableUpdateV5_2.Create(INT_LIQH_TABLE_UPDATE_V5_2));

    gUpdateManager.AddUpdate(TMethodTableUpdateV4_7.Create(INT_METHOD_TABLE_UPDATE_V4_7));
    gUpdateManager.AddUpdate(TSequenceTableUpdateV2.Create(INT_SEQUENCE_TABLE_UPDATE_V2));
    gUpdateManager.AddUpdate(TImportFileDefTableUpdateV2_1.Create(INT_IMPORTDEF_TABLE_UPDATE_V2_1));
    gUpdateManager.AddUpdate(TRacksTableUpdateV6.Create(INT_RACKS_TABLE_UPDATE_V6));

    // Version 8.0.3:
    gUpdateManager.AddUpdate(TRacksTableUpdateV7.Create(INT_RACKS_TABLE_UPDATE_V7));
    gUpdateManager.AddUpdate(TDiscreteRacksTableUpdateV1.Create(INT_DISCRETERACKS_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TPosinfoTableUpdateV1.Create(INT_POSINFO_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TSubstanceSetTableUpdateV1.Create(INT_SUBSTANCESET_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TDisposableTipTableUpdateV1.Create(INT_DISPOSABLETIP_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TPipetteListTableUpdateV1.Create(INT_PIPETTELIST_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TPosinfoTableUpdateV1_1.Create(INT_POSINFO_TABLE_UPDATE_V1_1));
    gUpdateManager.AddUpdate(TSubstanceDataTableUpdateV1.Create(INT_SUBSTANCEDATA_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TPipetteListTableUpdateV2.Create(INT_PIPETTELIST_TABLE_UPDATE_V2));
    gUpdateManager.AddUpdate(TLiqHTableUpdateV6.Create(INT_LIQH_TABLE_UPDATE_V6));
    gUpdateManager.AddUpdate(TPipetteListTableUpdateV3.Create(INT_PIPETTELIST_TABLE_UPDATE_V3));

    gUpdateManager.AddUpdate(TSettingsTableUpdateV2_8.Create(INT_SETTINGS_TABLE_UPDATE_V2_8));
    gUpdateManager.AddUpdate(TLiqHTableUpdateV7.Create(INT_LIQH_TABLE_UPDATE_V7));
    gUpdateManager.AddUpdate(TPipetteListTableUpdateV4.Create(INT_PIPETTELIST_TABLE_UPDATE_V4));
    gUpdateManager.AddUpdate(TPipetteListTableUpdateV5.Create(INT_PIPETTELIST_TABLE_UPDATE_V5));
    gUpdateManager.AddUpdate(TLiqHTableUpdateV8.Create(INT_LIQH_TABLE_UPDATE_V8));
    gUpdateManager.AddUpdate(TLiqHTableUpdateV9.Create(INT_LIQH_TABLE_UPDATE_V9));
    gUpdateManager.AddUpdate(TSubstanceSetTableUpdateV2.Create(INT_SUBSTANCESET_TABLE_UPDATE_V2));
    gUpdateManager.AddUpdate(TLiqHTableUpdateV10.Create(INT_LIQH_TABLE_UPDATE_V10));

    // Version 8.0.4:
    gUpdateManager.AddUpdate(TRacksTableUpdateV8.Create(INT_RACKS_TABLE_UPDATE_V8));
    gUpdateManager.AddUpdate(TDisplayComponentsTableUpdateV1.Create(INT_DISPLAYCOMPONENTS_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TSettingsTableUpdateV2_9.Create(INT_SETTINGS_TABLE_UPDATE_V2_9));
    gUpdateManager.AddUpdate(TWashprogTableUpdateV2.Create(INT_WASHPROGRAM_TABLE_UPDATE_V2));
    gUpdateManager.AddUpdate(TLiqHTableUpdateV11.Create(INT_LIQH_TABLE_UPDATE_V11));
    gUpdateManager.AddUpdate(TMethodTableUpdateV4_8.Create(INT_METHOD_TABLE_UPDATE_V4_8));

    // Version 8.1.0
    gUpdateManager.AddUpdate(TPipetteListTableUpdateV6.Create(INT_PIPETTELIST_TABLE_UPDATE_V6));
    gUpdateManager.AddUpdate(TRunVarTableUpdateV3.Create(INT_RUNVAR_TABLE_UPDATE_V3));
    gUpdateManager.AddUpdate(TMethodVariablesTableUpdateV1.Create(INT_METHODVARIABLES_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TSettingsTableUpdateV2_10.Create(INT_SETTINGS_TABLE_UPDATE_V2_10));
    gUpdateManager.AddUpdate(TMethodVarPagesTableUpdateV1.Create(INT_METHODVARPAGES_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TDiscreteCarriersTableUpdateV1.Create(INT_DISCRETECARRIERS_TABLE_UPDATE_V1));
    gUpdateManager.AddUpdate(TCarrierTableUpdateV3.Create(INT_CARRIER_TABLE_UPDATE_V3));
    gUpdateManager.AddUpdate(TMethodSettingsTableUpdateV3.Create(INT_METHODSETTINGS_TABLE_UPDATE_V3));
    gUpdateManager.AddUpdate(TPipetteListTableUpdateV7.Create(INT_PIPETTELIST_TABLE_UPDATE_V7));
    gUpdateManager.AddUpdate(TMethodTableUpdateV4_9.Create(INT_METHOD_TABLE_UPDATE_V4_9));
    gUpdateManager.AddUpdate(TSettingsTableUpdateV2_11.Create(INT_SETTINGS_TABLE_UPDATE_V2_11));
end;

class function TAppInstanceUpdateManagerCommon.CreateInstance(const aDataPath, aDBPath, aTempDBPath,
    aUpdatesPath: string): TAppInstanceUpdateManagerCommon;
begin
    if not Assigned(uInstance) then
        uInstance := TAppInstanceUpdateManagerCommon.Create(aDataPath, aDBPath, aTempDBPath, aUpdatesPath);
    result := Instance();
end;

class function TAppInstanceUpdateManagerCommon.Instance: TAppInstanceUpdateManagerCommon;
begin
    result := uInstance;
end;


end.
