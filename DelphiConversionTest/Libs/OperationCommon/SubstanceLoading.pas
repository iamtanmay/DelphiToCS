{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Central instance for subtance management and volume control
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  28.10.11 wl                                      TN5729   Initial Revision
  03.11.11 wl  ReadDetectedVol, RecordVolumes      TN5725   von MotionSystemPipMove hierher
  03.11.11 wl  StoreDsp, StoreAsp                  TN5725   von PosinfoDataAdaptor, TipSystemExt und SamHigh hierher
  03.11.11 wl  fSubstanceDataCache                 TN5725   wird von hier aus verwaltet
  03.11.11 wl  StoreDsp, StoreAsp                  TN5725   alles wird zuerst in das Well geschrieben und dann in Posinfo (wenn WritePosinfo=true)
  17.11.11 wl  FindCapEntry                        TN5725   von PosinfoDataAdaptorExt hierher
  17.11.11 wl  FindXRackPosByTubeID                TN5725   nach TubeID wird (noch) in der Posinfo gesucht
  17.11.11 wl  FindXRackPosBySubstID               TN5725   nach SubstID wird im Layout gesucht
  21.11.11 wl  FindXRackPosBySubstID               TN5730   --> RunStepBuilderHelper
  27.11.11 wl  LoadSubstanceSet                    TN5730   LoadColors: Farben können auch nicht geladen werden
  28.11.11 wl  ManualFill                          TN5730   das Totvolumen wird in Posinfo mit -3 gespeichert
  09.12.11 wl  ManualFill                          TN5761   neue Parameter ClearVolumeData (AbsoluteVol) und SubstanceSetName
  09.12.11 wl  SaveVolumesToSubstanceSet           TN5761   liest Daten aus Layout, nicht aus Posinfo
  09.12.11 wl  SaveVolumesToSubstanceSet           TN5761   DeleteEmptyPos: Positionen mit Vol <= MinVol werden gelöscht
  27.12.11 wl  LoadPosinfoDataToLayout             TN5768   lädt die Posinfo-Daten in das aktuelle Layout
  26.01.12 wl  WriteAnyStep                        TN5785   ersetzt WriteAspStep und WriteDispStep
  02.02.11 wl  RecordVolumes                       TN5791   verwendet TArray<TXRackPosition> statt TRack und Pos-Array
  07.02.12 wl  InternStoreAsp                      TN5796   Fehlerfenster entfernt: Ist durch PipList-Funktionen überflüssig geworden
  13.03.12 wl  LoadPosinfoDataToWell               TN5797   aus LoadPosinfoDataToLayout herausgetrennt
  14.08.12 wl  ManualFill                          TN5957   ClearVolumeData funktioniert jetzt auch ohne LoadAsStorage
  10.09.12 wl  SaveVolumesToSubstanceSet           TN5979   neuer Parameter DeleteEmptyPosDeadVolume: Bestimmt, welches Min-Volumen als Totvolumen verwendet wird
  10.09.12 wl  ManualFill                          TN5979   neuer Parameter MinVolume2
  11.09.12 wl  LoadPosinfoDataToWell               TN5979.2 eine leere StorageSubstID wird auch ins Layout übertragen
  11.09.12 wl  ManualFill                          TN5979   aufgeteilt in LoadAsStorage & ManualAddVolume
  21.11.12 wl  LoadSubstanceSet                    TN6021   vor dem Laden eines Substance sets wird SubstanceData neu in den Cache geladen
  30.01.13 wl  SetWellStorageID                    TN6076   neu
  30.01.13 wl  MoveTubeAndRefreshColor             TN6076   die Storage-Werte werden jetzt auch mitverschoben
  11.02.13 wl  DeleteStoragePosition               TN6078   neu
  11.02.13 wl  FindCorrectedStorageVolumesAll      TN6078   neu
  15.02.13 wl  StoreLiquidAsp/StoreLiquidDsp       TN5914   Suchen das verwendete PipPumpDevice und speichern das Volumen dort
  28.03.13 wl  LoadSubstanceSet                    TN6119   Neu: ReplaceWildcardBy ersetzt ein * in der Rack-ID durch beliebigen Text
  28.03.13 wl  Create                              TN6120   Setting 'AutomaticLoadSet' ersetzt festen Namen 'AUTOLOAD'
  28.03.13 wl  WriteAnyStep                        TN6120   Funktion prüft, ob es sich um eine Position aus dem AutomaticLoadSet handelt und speichert dann das Volumen
  07.05.13 ts  RecordVolumes                       TN6118   MaxMotorIndex entfernt, durch High(aRP) ersetzt
  29.05.13 ts  TSubstanceLoading.ReadDetectedVol   TN6162   xPosinfoDA.DataProvider.Next hinzugefügt, sonst Endlosschleife möglich
  27.11.13 wl                                      TN6313   Spezialfall MultiTip-Rack: Gespeicherte Position ist immer 1
  ----------------------------------------------------------------------------------------------------------- }

unit SubstanceLoading;


interface


uses
    Generics.Collections,
    Rack,
    MethodTypes,
    RackTypes,
    Layout,
    IntfPipDevice,
    AppTypes,
    VolumeInfo,
    TipSystem,
    IntfPipPumpDevice,
    PosinfoDataAdaptor,
    RackWell,
    SubstanceDataDataCache,
    GeneralTypes;

type
    TSubstanceLoading = class
    strict private
        fSubstanceDataCache: TSubstanceDataDataCompleteCache;
        fUseTubeIDIfNoSubstIDExists: boolean;
        fWritePosinfo: boolean;
        fAutomaticLoadSet: string;
        fAutomaticRefreshSet: string;
        class var uInstance: TSubstanceLoading;
        function ReadColorBySubstID(const aSubstID: string): integer;
        class function GetLiquidFlagText(aLiqFlag: TTipLiqErrorType): string; static;
        class procedure InternRecordVolumes(xDataAdaptor: TPosinfoDataAdaptor; aRackID: string;
            const aPos: integer; const aVol: double; const aLiqFlag: string);
        class function GetOriginFromVolumeInfo(aVolumeInfo: TVolumeInfo): string; static;
        class function GetUnitFromAspType(aAspType: TAspirateType): integer; static;

        procedure WriteAnyStep(aPosinfoDA: TPosinfoDataAdaptor; aRack: TRack; aPos: integer; aVol: extended;
            aUnit: integer; const aSubstID, aOrigin: string);
        procedure AddDispVolumes(aRack: TRack; aPos: integer; aVolumes: TArray<TSubstIDAndVol>;
            aUnit: integer; const aOrigin: string);
        procedure LoadAsStorage(const aRack: TRack; aPosition: integer; aRackWell: TRackWell;
            const aSubstID: string; aLoadColors: boolean; aMinVolume1, aMinVolume2, aMaxVolume: double;
            const aSubstanceSetName: string);
        procedure DeleteStoragePosition(const aRack: TRack; aPosition: integer; aRackWell: TRackWell);
        procedure ManualAddVolume(const aRack: TRack; aPosition: integer; aRackWell: TRackWell;
            const aSubstID: string; aVolume: double; aLoadColors: boolean; aClearVolumeData: boolean);
    public
        constructor Create;
        destructor Destroy; override;
        class property Instance: TSubstanceLoading read uInstance;

        class procedure CreateInstance;
        class procedure DestroyInstance;

        procedure ManualFill(const aRack: TRack; aPosition: integer; const aSubstID: string; aVolume: double;
            aLoadAsStorage: TStoreOrDelete; aLoadColors: boolean; aClearVolumeData: boolean;
            aMinVolume1, aMinVolume2, aMaxVolume: double; const aSubstanceSetName: string);
        procedure LoadSubstanceSet(const aSetName: string; aLoadAsStorage: TStoreOrDelete;
            aLoadColors: boolean; aReplaceWildcardBy: string = '');
        procedure SaveVolumesToSubstanceSet(const aName: string; aSaveAtSimulation: boolean;
            aDeleteEmptyPos: boolean; aDeleteEmptyPosDeadVolume: TUseDeadVolume);
        procedure AutoLoadSubstanceSet;
        procedure MoveTubeAndRefreshColor(xDataAdaptor: TPosinfoDataAdaptor; const aSRack: TRack;
            aSPos: integer; const aDRack: TRack; aDPos: integer);

        procedure StorePowderOrSampleAsp(aVolumes: TObjectList<TVolumeInfo>; aRack: TRack; aPos: integer;
            aVol: extended; aAspType: TAspirateType);
        procedure StoreLiquidAsp(aPipDevice: IPipDevice; aRack: TRack; aPos, aTip: integer; aVol: extended;
            aAspType: TAspirateType; aPumpIndex: integer);
        procedure StoreLiquidPumpAsp(aPipPump: IPipPumpDevice; aRack: TRack; aPos: integer; aVol: extended;
            aAspType: TAspirateType);

        procedure StoreDsp(aVolumes: TObjectList<TVolumeInfo>; aRack: TRack; aPos: integer; aVol: extended;
            aDspType: TDispenseType);
        procedure StoreLiquidDsp(aPipDevice: IPipDevice; aRack: TRack; aPos, aTip: Integer; aVol: extended;
            aDspType: TDispenseType; aPumpIndex: integer);

        procedure RecordVolumes(const aRP: TArray<TXRackPosition>; aMotorMap: TIPMAP;
            aVolumes, aCalcVolumes: TDoubleArray; const aStoreDifferenceAsVolume: boolean;
            const aLiqFlags: TTipLiqErrorTypeArray);
        class function ReadDetectedVol(const aRack: TRack; const aPos: integer; out oVol: extended): boolean;
        class function GetNextStep(aDA: TPosinfoDataAdaptor; aRackID: string; aPos: integer): integer;
        function FindRackPosByTubeID(aLayout: TLayout; aTubeID: string): TRackPosition;
        function FindCapEntry(aLayout: TLayout; aTubeID, aRunName: string): TXRackPosition;
        function FindCorrectedStorageVolumesByID(aLayout: TLayout; aSubstID: string;
            aUseDeadVolume: TUseDeadVolume): TArray<TRackPositionWithVol>;
        function FindCorrectedStorageVolumesAll(aLayout: TLayout; aUseDeadVolume: TUseDeadVolume)
            : TArray<TRackIDPositionWithVol>;
        procedure LoadPosinfoDataToWell(aDA: TPosinfoDataAdaptor; aRack: TRack; aRackWell: TRackWell);
        procedure LoadPosinfoDataToLayout();
        procedure RefreshSubstanceDatabase();
        procedure SetWellStorageID(aRackWell: TRackWell; const aSubstID: string; aLoadColor: boolean;
            aMinVolume1, aMinVolume2, aMaxVolume: double; const aSubstanceSetName: string);
    end;


implementation


uses
    Controls,
    StrUtils,
    SysUtils,
    LogManager,
    RunFlow,
    SubstanceSetDataAdaptor,
    LiquidManager,
    ErrorInfoExt,
    LayoutManager,
    TipMapUtils,
    ErrorMessageFactory,
    ErrorManager,
    AppSettings,
    CommonTypes,
    SamGlobe;

{ TSubstanceLoading }

constructor TSubstanceLoading.Create;
var
    xIniAccess: IWinLissyIniAccess;
begin
    inherited;
    fSubstanceDataCache := TSubstanceDataDataCompleteCache.Create;

    xIniAccess := TAppSettings.CreateAppIni;
    fUseTubeIDIfNoSubstIDExists := xIniAccess.ReadBool('Documentation', 'UseTubeIDIfNoSubstIDExists');
    fWritePosinfo := xIniAccess.ReadBool('Documentation', 'WritePosinfo');
    fAutomaticLoadSet := xIniAccess.ReadString('SubstanceSet', 'AutomaticLoadSet');
    fAutomaticRefreshSet := xIniAccess.ReadString('SubstanceSet', 'AutomaticRefreshSet');
end;

destructor TSubstanceLoading.Destroy;
begin
    FreeAndNil(fSubstanceDataCache);
    inherited;
end;

class procedure TSubstanceLoading.CreateInstance;
begin
    uInstance := TSubstanceLoading.Create;
end;

class procedure TSubstanceLoading.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;

procedure TSubstanceLoading.AutoLoadSubstanceSet;
begin
    if (fAutomaticLoadSet <> '') then
        LoadSubstanceSet(fAutomaticLoadSet, sdStore, true);
end;

procedure TSubstanceLoading.LoadPosinfoDataToWell(aDA: TPosinfoDataAdaptor; aRack: TRack;
    aRackWell: TRackWell);
var
    xData: TRackWellStorageData;
    // xIsStorage: boolean;
    xSubstID: string;
    // xMinVolume1, xMinVolume2, xMaxVolume: double;
    xColor: integer;
begin
    xData := TRackWell.GetResetStorageData();

    aDA.SelectAndOpen('select * from posinfo where rackid = ''' + aRack.RackID + ''' and [pos] = ' +
        IntToStr(aRackWell.WellNr), true);
    while not aDA.DataProvider.Eof do
    begin
        if (aDA.ReadStep > INT_POSINFO_STEP_MIN_SUBSTID) then
        begin
            xSubstID := aDA.ReadSubstID;
            xColor := self.ReadColorBySubstID(xSubstID);
            aRackWell.AddVolume(aDA.ReadAmount, xSubstID, xColor, 0, false);
        end;

        if (aDA.ReadStep = INT_POSINFO_STEP_MIN_SUBSTID) then
        begin
            xData.IsStorage := true;
            xData.SubstID := aDA.ReadSubstID;
            xData.SetName := aDA.ReadOrigin;
        end;
        if (aDA.ReadStep = INT_POSINFO_STEP_MAXVOLUME) then
            xData.MaxVolume := aDA.ReadAmount;
        if (aDA.ReadStep = INT_POSINFO_STEP_MINVOLUME1) then
            xData.MinVolume1 := aDA.ReadAmount;
        if (aDA.ReadStep = INT_POSINFO_STEP_MINVOLUME2) then
            xData.MinVolume2 := aDA.ReadAmount;

        aDA.DataProvider.Next;
    end;
    aDA.Close;

    aRackWell.StorageData := xData;
end;

procedure TSubstanceLoading.LoadPosinfoDataToLayout;
var
    xDA: TPosinfoDataAdaptor;
    xRackIndex, x: integer;
    xLayout: TLayout;
    xRack: TRack;
begin
    xDA := TPosinfoDataAdaptor.Create;
    try
        xLayout := TLayoutManager.Instance.CurrentLayout;
        for xRackIndex := 0 to xLayout.Racks.Count - 1 do
        begin
            xRack := xLayout.Racks[xRackIndex];
            if xRack.RackID = '' then
                CONTINUE;

            // Gibt es überhaupt Einträge?
            xDA.SelectAndOpen('select * from posinfo where rackid = ''' + xRack.RackID + '''', true);
            try
                if xDA.DataProvider.Eof then
                    CONTINUE;
            finally
                xDA.Close;
            end;

            // alles laden und ins Layout eintragen
            for x := 0 to xRack.Wells.Count - 1 do
            begin
                LoadPosinfoDataToWell(xDA, xRack, xRack.Wells[x]);
            end;
        end;
    finally
        FreeAndNil(xDA);
    end;
end;

procedure TSubstanceLoading.RefreshSubstanceDatabase();
begin
    // Substanzdaten neu lesen
    fSubstanceDataCache.ReadAll();
end;

procedure TSubstanceLoading.LoadSubstanceSet(const aSetName: string; aLoadAsStorage: TStoreOrDelete;
    aLoadColors: boolean; aReplaceWildcardBy: string);
const
    cWildCard = '*';
var
    xDA: TSubstanceSetDataAdaptor;
    xRecs: TArray<TSubstanceSetRec>;
    x: integer;
    xRack: TRack;
    xLastRackID: string;
begin
    // Substanzdaten neu lesen
    RefreshSubstanceDatabase();

    xDA := TSubstanceSetDataAdaptor.Create;
    try
        xRecs := xDA.ReadSubstanceSetRecsBySetName(aSetName);
    finally
        FreeAndNil(xDA);
    end;

    xRack := nil;
    xLastRackID := '';

    for x := 0 to high(xRecs) do
    begin
        // Replace Wildcard: *-Symbol in der Rack-ID durch einen Text ersetzen
        if (aReplaceWildcardBy <> '') then
        begin
            xRecs[x].RackID := StringReplace(xRecs[x].RackID, cWildCard, aReplaceWildcardBy, [rfReplaceAll]);
        end;

        if (xLastRackID <> xRecs[x].RackID) then
        begin
            xRack := TLayoutManager.Instance.CurrentLayout.FindRackByRackID(xRecs[x].RackID);
            xLastRackID := xRecs[x].RackID;
        end;
        ManualFill(xRack, xRecs[x].Pos, xRecs[x].SubstID, xRecs[x].Amount, aLoadAsStorage, aLoadColors, true,
            xRecs[x].MinVolume1, xRecs[x].MinVolume2, xRecs[x].MaxVolume, aSetName);
    end;
end;

class function TSubstanceLoading.GetNextStep(aDA: TPosinfoDataAdaptor; aRackID: string;
    aPos: integer): integer;
begin
    aDA.SelectAndOpenByRackIDAndPos(aRackID, aPos, true);
    try
        result := aDA.ReadNextStep(aDA.DataProvider, aRackID, aPos);
    finally
        aDA.Close();
    end;
end;

function TSubstanceLoading.ReadColorBySubstID(const aSubstID: string): integer;
var
    xCacheElement: TSubstanceDataCacheElement;
begin
    xCacheElement := fSubstanceDataCache.FindByName(aSubstID);
    if Assigned(xCacheElement) then
        EXIT(xCacheElement.Rec.SubstColor)
    else
        EXIT(0);
end;

procedure TSubstanceLoading.WriteAnyStep(aPosinfoDA: TPosinfoDataAdaptor; aRack: TRack; aPos: integer;
    aVol: extended; aUnit: integer; const aSubstID, aOrigin: string);
var
    xSubstSetDA: TSubstanceSetDataAdaptor;
    xRec: TSubstanceSetRec;
    xNextStep: integer;
    xIsStorage: boolean;
begin
    aPosinfoDA.SelectAndOpenByRackIDAndPos(aRack.RackID, aPos, false);
    try
        xNextStep := aPosinfoDA.ReadNextStep(aPosinfoDA.DataProvider, aRack.RackID, aPos);

        aPosinfoDA.WritePosinfoDataToDataset(aPosinfoDA.DataProvider, true, true, aRack.RackID, aPos,
            xNextStep, aSubstID, aOrigin, Now, aVol, aUnit);
    finally
        aPosinfoDA.Close();
    end;

    // Diese Funktion sollte nicht in der Simulation ausgeführt werden!
    if (fAutomaticRefreshSet <> '') and not gRunFlow.SimulationMode then
    begin
        // Automatic Refresh: Ist die Position eine Storage-Position und hat sie einen Set-Namen?
        xIsStorage := aRack.WellIsStoragePosition(aPos, xRec);
        if xIsStorage and (xRec.SetName = fAutomaticRefreshSet) then
        begin
            xSubstSetDA := TSubstanceSetDataAdaptor.Create;
            try
                xSubstSetDA.EditOrAppendRec(xRec);
            finally
                FreeAndNil(xSubstSetDA);
            end;
        end;
    end;
end;

procedure TSubstanceLoading.DeleteStoragePosition(const aRack: TRack; aPosition: integer;
    aRackWell: TRackWell);
var
    xPosinfoDA: TPosinfoDataAdaptor;
begin
    gLogManager.LogF('Delete storage data: Rack-[%s] Pos-[%d]', [aRack.Name, aPosition], true);

    // Vorratsgefäß: SubstanceID, Minimum und Maximum eintragen
    aRackWell.ResetStorageData();

    if (fWritePosinfo) then
    begin
        xPosinfoDA := TPosinfoDataAdaptor.Create();
        try
            // Storage-Daten löschen (wenn SubstID leer, wird nur gelöscht)
            xPosinfoDA.DeleteStorageData(aRack.RackID, aPosition);
        finally
            FreeAndNil(xPosinfoDA);
        end;
    end;
end;

procedure TSubstanceLoading.LoadAsStorage(const aRack: TRack; aPosition: integer; aRackWell: TRackWell;
    const aSubstID: string; aLoadColors: boolean; aMinVolume1, aMinVolume2, aMaxVolume: double;
    const aSubstanceSetName: string);
var
    xPosinfoDA: TPosinfoDataAdaptor;
begin
    gLogManager.LogF('Load as Storage: - Rack-[%s] Pos-[%d] SubstID-[%s]',
        [aRack.Name, aPosition, aSubstID], true);

    // Vorratsgefäß: SubstanceID, Minimum und Maximum eintragen
    SetWellStorageID(aRackWell, aSubstID, aLoadColors, aMinVolume1, aMinVolume2, aMaxVolume,
        aSubstanceSetName);

    if (fWritePosinfo) then
    begin
        xPosinfoDA := TPosinfoDataAdaptor.Create();
        try
            // Storage-Daten löschen (wenn SubstID leer, wird nur gelöscht)
            xPosinfoDA.DeleteStorageData(aRack.RackID, aPosition);
            if (aSubstID <> '') then
                xPosinfoDA.WriteStorageData(aRack.RackID, aPosition, aSubstID, aMinVolume1, aMinVolume2,
                    aMaxVolume, aSubstanceSetName);
        finally
            FreeAndNil(xPosinfoDA);
        end;
    end;
end;

procedure TSubstanceLoading.ManualAddVolume(const aRack: TRack; aPosition: integer; aRackWell: TRackWell;
    const aSubstID: string; aVolume: double; aLoadColors: boolean; aClearVolumeData: boolean);
var
    xPosinfoDA: TPosinfoDataAdaptor;
    xColor: integer;
    xText: string;
begin
    xText := Format('Store amount - Rack-[%s] Pos-[%d] Vol-[%g]', [aRack.Name, aPosition, aVolume]);
    if (aClearVolumeData) then
        xText := xText + ' (Clear other volume data)';
    gLogManager.Log(xText, true);

    if (aLoadColors) then
        xColor := self.ReadColorBySubstID(aSubstID)
    else
        xColor := 0;

    // ClearVolumeData: Alle alten Daten löschen
    if (aClearVolumeData) then
        aRackWell.VolumeData.Clear;

    aRackWell.AddVolume(aVolume, aSubstID, xColor, 0, false);

    if (fWritePosinfo) then
    begin
        xPosinfoDA := TPosinfoDataAdaptor.Create();
        try
            // ClearVolumeData: Alle alten Daten löschen
            if (aClearVolumeData) then
                xPosinfoDA.DeleteVolumeData(aRack.RackID, aPosition);

            WriteAnyStep(xPosinfoDA, aRack, aPosition, aVolume, INT_POSINFO_UNIT_MANUAL, aSubstID, 'MANUAL');
        finally
            FreeAndNil(xPosinfoDA);
        end;
    end;
end;

procedure TSubstanceLoading.ManualFill(const aRack: TRack; aPosition: integer; const aSubstID: string;
    aVolume: double; aLoadAsStorage: TStoreOrDelete; aLoadColors: boolean; aClearVolumeData: boolean;
    aMinVolume1, aMinVolume2, aMaxVolume: double; const aSubstanceSetName: string);
var
    xRackWell: TRackWell;
begin
    if (aRack = nil) or (aRack.RackId = '') then
        EXIT;

    // Spezialfall MultiTip-Rack: Gespeicherte Position ist immer 1
    if aRack.IsMultiTipRack() then
        aPosition := 1;

    xRackWell := aRack.GetWellFromWellNr(aPosition);
    if Assigned(xRackWell) then
    begin
        if (aLoadAsStorage = sdStore) then
            LoadAsStorage(aRack, aPosition, xRackWell, aSubstID, aLoadColors, aMinVolume1, aMinVolume2,
                aMaxVolume, aSubstanceSetName);

        if (aLoadAsStorage = sdDelete) then
            DeleteStoragePosition(aRack, aPosition, xRackWell);

        ManualAddVolume(aRack, aPosition, xRackWell, aSubstID, aVolume, aLoadColors, aClearVolumeData);
    end;
end;

procedure TSubstanceLoading.MoveTubeAndRefreshColor(xDataAdaptor: TPosinfoDataAdaptor; const aSRack: TRack;
    aSPos: integer; const aDRack: TRack; aDPos: integer);
var
    xDRackWell, xSRackWell: TRackWell;
    x: integer;
    xStorageData: TRackWellStorageData;
begin
    xDRackWell := aDRack.GetWellFromWellNr(aDPos);
    xSRackWell := aSRack.GetWellFromWellNr(aSPos);

    if Assigned(xDRackWell) then
    begin
        // im Ziel-Gefäß kann ja nichts sein
        xDRackWell.VolumeData.Clear;
        xDRackWell.ResetStorageData;

        // Volumen und Storage-Daten übertragen
        for x := 0 to xSRackWell.VolumeData.Count - 1 do
            xDRackWell.AddVolume(xSRackWell.VolumeData[x].Amount, xSRackWell.VolumeData[x].ID,
                xSRackWell.VolumeData[x].Color, 0, false);
        xStorageData := xSRackWell.StorageData;
        xDRackWell.StorageData := xStorageData;
        xDRackWell.ShowVolumeData(0);

        xSRackWell.VolumeData.Clear;
        xSRackWell.ResetStorageData();
        xSRackWell.ShowVolumeData(0);
    end;

    // Posinfo-Entries werden umgeschrieben
    if (fWritePosinfo) then
        xDataAdaptor.MoveTubeProperties(aSRack.RackID, aSPos, aDRack.RackID, aDPos);
end;

procedure TSubstanceLoading.SaveVolumesToSubstanceSet(const aName: string; aSaveAtSimulation: boolean;
    aDeleteEmptyPos: boolean; aDeleteEmptyPosDeadVolume: TUseDeadVolume);
var
    xSubstSetDA: TSubstanceSetDataAdaptor;
    x: integer;
    xRecs: TArray<TSubstanceSetRec>;
    xDeadVolume: double;
begin
    // Diese Funktion sollte nicht in der Simulation ausgeführt werden!
    if gRunFlow.SimulationMode and not aSaveAtSimulation then
        EXIT;

    xSubstSetDA := TSubstanceSetDataAdaptor.Create;
    try
        xRecs := TLayoutManager.Instance.CurrentLayout.FindSetStoragePositions(aName);
        for x := 0 to high(xRecs) do
        begin
            xDeadVolume := TDeadVolume.Calculate(xRecs[x].MinVolume1, xRecs[x].MinVolume2,
                aDeleteEmptyPosDeadVolume);
            if aDeleteEmptyPos and (xRecs[x].Amount <= xDeadVolume) then
                xSubstSetDA.DeletePos(xRecs[x].SetName, xRecs[x].RackID, xRecs[x].Pos)
            else
                xSubstSetDA.EditOrAppendRec(xRecs[x]);
        end;
    finally
        FreeAndNil(xSubstSetDA);
    end;
end;

class function TSubstanceLoading.GetUnitFromAspType(aAspType: TAspirateType): integer;
begin
    case (aAspType) of
        // asptSystemLiquid:
        // asptAir:
        asptSystemRackLiquid, asptLiquid:
            result := INT_POSINFO_UNIT_LIQUID;
        asptPowder:
            result := INT_POSINFO_UNIT_POWDER;
        else
            result := INT_POSINFO_UNIT_LIQUID;
    end;
end;

procedure TSubstanceLoading.StorePowderOrSampleAsp(aVolumes: TObjectList<TVolumeInfo>; aRack: TRack;
    aPos: integer; aVol: extended; aAspType: TAspirateType);
var
    xUnit, x: integer;
    xRackWell: TRackWell;
    xAspVolumeData: TArray<TSubstIDAndVol>;
    xPosinfoDA: TPosinfoDataAdaptor;
begin
    if not Assigned(aRack) or (aRack.RackID = '') then
        EXIT;

    // Spezialfall MultiTip-Rack: Gespeicherte Position ist immer 1
    if aRack.IsMultiTipRack() then
        aPos := 1;

    // Volumen im Layout speichern
    xRackWell := aRack.GetWellFromWellNr(aPos);
    if Assigned(xRackWell) then
    begin
        xAspVolumeData := xRackWell.AddVolume(-aVol, '', 0, gTubeColors.Aspirate,
            fUseTubeIDIfNoSubstIDExists);
    end;

    // Volumen in POSINFO-Tabelle speichern
    if (fWritePosinfo) then
    begin
        xUnit := GetUnitFromAspType(aAspType);

        xPosinfoDA := TPosinfoDataAdaptor.Create();
        try
            for x := 0 to high(xAspVolumeData) do
            begin
                WriteAnyStep(xPosinfoDA, aRack, aPos, -(xAspVolumeData[x].Volume), xUnit,
                    xAspVolumeData[x].SubstID, '');
            end;
        finally
            FreeAndNil(xPosinfoDA);
        end;
    end;

    // Herkunft und Volumen für die Abgabe speichern
    aVolumes.Add(TVolumeInfo.Create(xAspVolumeData, aRack.RackID, aPos, aAspType));
end;

procedure TSubstanceLoading.StoreLiquidPumpAsp(aPipPump: IPipPumpDevice; aRack: TRack; aPos: integer;
    aVol: extended; aAspType: TAspirateType);
var
    xSubstID: string;
    xAspVolumeData: TArray<TSubstIDAndVol>;
begin
    if aAspType = asptAir then
    begin
        SetLength(xAspVolumeData, 1);
        xAspVolumeData[0].SubstID := '';
        xAspVolumeData[0].Volume := aVol;
        aPipPump.Volumes.Add(TVolumeInfo.Create(xAspVolumeData, aAspType));
        EXIT;
    end
    else if aAspType = asptSystemLiquid then
    begin
        xSubstID := gSysLiqManager.PumpGetCurrentSysName(aPipPump);

        SetLength(xAspVolumeData, 1);
        xAspVolumeData[0].SubstID := xSubstID;
        xAspVolumeData[0].Volume := aVol;
        aPipPump.Volumes.Add(TVolumeInfo.Create(xAspVolumeData, aAspType));
        EXIT;
    end;

    StorePowderOrSampleAsp(aPipPump.Volumes, aRack, aPos, aVol, aAspType);
end;

procedure TSubstanceLoading.StoreLiquidAsp(aPipDevice: IPipDevice; aRack: TRack; aPos, aTip: integer;
    aVol: extended; aAspType: TAspirateType; aPumpIndex: integer);
var
    xPipPump: IPipPumpDevice;
begin
    if aVol <= 0 then
        EXIT;

    // PipPump definieren
    xPipPump := aPipDevice.Tips[aTip].GetPipPump(aPumpIndex);
    if not Assigned(xPipPump) then
        EXIT;

    StoreLiquidPumpAsp(xPipPump, aRack, aPos, aVol, aAspType);
end;

class function TSubstanceLoading.GetOriginFromVolumeInfo(aVolumeInfo: TVolumeInfo): string;
begin
    case aVolumeInfo.AspType of
        asptSystemLiquid:
            EXIT('Dispense !   Source (SYSTEM) --> [' + aVolumeInfo.Volumes[0].SubstID + ']');
        asptSystemRackLiquid:
            EXIT('Dispense !   Source (SYSTEM) --> [' + aVolumeInfo.RackID + ' / Position: ' +
                IntToStr(aVolumeInfo.Pos) + ']');
        asptAir:
            EXIT('Dispense !   Source (AIR)')
        else
            EXIT('Dispense !   Source --> ' + '[' + aVolumeInfo.RackID + '/ Position: ' +
                IntToStr(aVolumeInfo.Pos) + ']');
    end;
end;

procedure TSubstanceLoading.AddDispVolumes(aRack: TRack; aPos: integer; aVolumes: TArray<TSubstIDAndVol>;
    aUnit: integer; const aOrigin: string);
var
    xRackWell: TRackWell;
    x: integer;
    xPosinfoDA: TPosinfoDataAdaptor;
begin
    // Spezialfall MultiTip-Rack: Gespeicherte Position ist immer 1
    if aRack.IsMultiTipRack() then
        aPos := 1;

    // Volumen im Layout speichern
    xRackWell := aRack.GetWellFromWellNr(aPos);
    if Assigned(xRackWell) then
    begin
        for x := 0 to high(aVolumes) do
        begin
            xRackWell.AddVolume(aVolumes[x].Volume, aVolumes[x].SubstID,
                ReadColorBySubstID(aVolumes[x].SubstID), gTubeColors.Dispense, false);
        end;
    end;

    // Volumen in POSINFO-Tabelle speichern
    if (fWritePosinfo) then
    begin
        xPosinfoDA := TPosinfoDataAdaptor.Create();
        try
            for x := 0 to high(aVolumes) do
            begin
                WriteAnyStep(xPosinfoDA, aRack, aPos, aVolumes[x].Volume, aUnit, aVolumes[x].SubstID,
                    aOrigin);
            end;
        finally
            FreeAndNil(xPosinfoDA);
        end;
    end;
end;

procedure TSubstanceLoading.StoreDsp(aVolumes: TObjectList<TVolumeInfo>; aRack: TRack; aPos: integer;
    aVol: extended; aDspType: TDispenseType);
var
    xOrigin: string;
    xTopIndex: integer;
    xVolumeInfo: TVolumeInfo;
    xVolRemaining: extended;
    xCurrentVol, xVolumeInfoVol: extended;
    xAspType: TAspirateType;
    xCurrentVolumes: TArray<TSubstIDAndVol>;
begin
    xVolRemaining := aVol;
    while (xVolRemaining > 0) do
    begin
        xTopIndex := aVolumes.Count - 1;
        if xTopIndex >= 0 then
            xVolumeInfo := aVolumes[xTopIndex]
        else
            xVolumeInfo := nil;

        xCurrentVol := xVolRemaining;

        if Assigned(xVolumeInfo) then
        begin
            xAspType := xVolumeInfo.AspType;
            xOrigin := GetOriginFromVolumeInfo(xVolumeInfo);

            xVolumeInfoVol := xVolumeInfo.TotalVolume;
            if (xCurrentVol >= xVolumeInfoVol) then
            begin
                // aktuelles Volumen reduzieren
                xCurrentVol := xVolumeInfoVol;

                // komplettes Volumen übernehmen
                xCurrentVolumes := xVolumeInfo.Volumes;

                // VolumeInfo löschen
                aVolumes.Delete(xTopIndex)
            end
            else
            begin
                // VolumeInfo um das Teilvolumen reduzieren
                xCurrentVolumes := xVolumeInfo.ReduceVolumes(xCurrentVol);
            end;
        end
        else
        begin
            // Es gibt keine VolumeInfo (sollte gar nicht passieren)
            xAspType := asptLiquid;
            xOrigin := 'Undefined origin';
            SetLength(xCurrentVolumes, 1);
            xCurrentVolumes[0].SubstID := '';
            xCurrentVolumes[0].Volume := xCurrentVol;
        end;

        // Dispense-Daten in RackWell und Posinfo schreiben
        if Assigned(aRack) and (aRack.RackID <> '') and (xAspType <> asptAir) then
        begin
            AddDispVolumes(aRack, aPos, xCurrentVolumes, GetUnitFromAspType(xAspType), xOrigin);
        end;

        // verbleibendes Volumen reduzieren
        xVolRemaining := xVolRemaining - xCurrentVol;
    end;
end;

procedure TSubstanceLoading.StoreLiquidDsp(aPipDevice: IPipDevice; aRack: TRack; aPos, aTip: Integer;
    aVol: extended; aDspType: TDispenseType; aPumpIndex: integer);
var
    xPipPump: IPipPumpDevice;
begin
    if aVol <= 0 then
        EXIT;

    // PipPump definieren
    xPipPump := aPipDevice.Tips[aTip].GetPipPump(aPumpIndex);
    if not Assigned(xPipPump) then
        EXIT;

    StoreDsp(xPipPump.Volumes, aRack, aPos, aVol, aDspType);
end;

class procedure TSubstanceLoading.InternRecordVolumes(xDataAdaptor: TPosinfoDataAdaptor; aRackID: string;
    const aPos: integer; const aVol: double; const aLiqFlag: string);
begin

    xDataAdaptor.SelectAndOpenByRackIDAndPosAndStep(aRackID, aPos, INT_POSINFO_STEP_DETECTED, false);
    try
        if not xDataAdaptor.DataProvider.IsEmpty then
        begin
            xDataAdaptor.DataProvider.Edit;
            xDataAdaptor.WriteDate(now);
            xDataAdaptor.WriteAmount(aVol);

            // alter Eintrag bleibt erhalten, wenn keine neuen Daten geleifert werden
            if (aLiqFlag <> '') then
                xDataAdaptor.WriteOrigin(aLiqFlag);

            xDataAdaptor.DataProvider.Post;
        end
        else
        begin
            xDataAdaptor.WritePosinfoDataToDataset(xDataAdaptor.DataProvider, true, true, aRackID, aPos,
                INT_POSINFO_STEP_DETECTED, '', aLiqFlag, Now, aVol, 0);
        end;
    finally
        xDataAdaptor.Close();
    end;
end;

procedure TSubstanceLoading.RecordVolumes(const aRP: TArray<TXRackPosition>; aMotorMap: TIPMAP;
    aVolumes, aCalcVolumes: TDoubleArray; const aStoreDifferenceAsVolume: boolean;
    const aLiqFlags: TTipLiqErrorTypeArray);
var
    x, xNextStep: integer;
    xPosinfoDA: TPosinfoDataAdaptor;
    xLiqFlag: string;
    xRackWell: TRackWell;
begin
    xPosinfoDA := TPosinfoDataAdaptor.Create();
    try
        for x := 0 to high(aRP) do
        begin
            if (not Assigned(aRP[x].Rack) or (aRP[x].Rack.RackID = '')) then
                EXIT;

            if not TTipMapUtils.TipSelected(aMotorMap, x) then
                CONTINUE;

            if (x <= high(aLiqFlags)) then
                xLiqFlag := GetLiquidFlagText(aLiqFlags[x])
            else
                xLiqFlag := '';

            InternRecordVolumes(xPosinfoDA, aRP[x].Rack.RackID, aRP[x].Pos, aVolumes[x], xLiqFlag);

            if (aStoreDifferenceAsVolume) then
            begin
                xRackWell := aRP[x].Rack.GetWellFromWellNr(aRP[x].Pos);
                if Assigned(xRackWell) then
                    xRackWell.AddVolume(aVolumes[x] - aCalcVolumes[x], TRackWell.cSubstIDCalculated, 0,
                        0, false);

                if fWritePosinfo then
                begin
                    xNextStep := GetNextStep(xPosinfoDA, aRP[x].Rack.RackID, aRP[x].Pos);
                    xPosinfoDA.AppendRec(aRP[x].Rack.RackID, aRP[x].Pos, xNextStep, '',
                        'Detected volume difference to sum up right', aVolumes[x] - aCalcVolumes[x],
                        INT_POSINFO_UNIT_DETECTED);
                end;
            end;
        end;
    finally
        FreeAndNil(xPosinfoDA);
    end;
end;

class function TSubstanceLoading.ReadDetectedVol(const aRack: TRack; const aPos: integer;
    out oVol: extended): boolean;
var
    xPosinfoDA: TPosinfoDataAdaptor;
begin
    result := false;
    oVol := 0;
    xPosinfoDA := TPosinfoDataAdaptor.Create();
    try
        xPosinfoDA.SelectAndOpenByDetectedRackID(aRack.RackID, true);
        while not xPosinfoDA.DataProvider.Eof do
        begin
            if xPosinfoDA.ReadPos = aPos then
            begin
                result := true;
                oVol := xPosinfoDA.ReadAmount;
                EXIT;
            end;
            xPosinfoDA.DataProvider.Next;
        end;
    finally
        FreeAndNil(xPosinfoDA);
    end;
end;

class function TSubstanceLoading.GetLiquidFlagText(aLiqFlag: TTipLiqErrorType): string;
const
    STR_LIQFLAG_OK = 'Detected OK';
    STR_LIQFLAG_NOLIQUID = 'NOLI';
    STR_LIQFLAG_LOWLIQUID = 'LOLI';
begin
    case (aLiqFlag) of
        letLowLiquid:
            result := STR_LIQFLAG_LOWLIQUID;
        letNoLiquid:
            result := STR_LIQFLAG_NOLIQUID;
        else
            result := STR_LIQFLAG_OK;
    end;
end;

function TSubstanceLoading.FindRackPosByTubeID(aLayout: TLayout; aTubeID: string): TRackPosition;
var
    xRackIndex: integer;
    xPosinfoRecs: TPosinfoRecs;
    i: integer;
    xRackID: string;
    xPosinfoDA: TPosinfoDataAdaptor;
begin
    result := gmMakeRackPos('', 0);
    xPosinfoDA := TPosinfoDataAdaptor.Create();
    try
        xPosinfoRecs := xPosinfoDA.ReadPosinfoRecsByTubeID(aTubeID);
    finally
        FreeAndNil(xPosinfoDA);
    end;

    for i := 0 to high(xPosinfoRecs) do
    begin
        xRackID := xPosInfoRecs[i].RackID;
        xRackIndex := aLayout.GetRackNoForRackID(xRackID);
        if (xRackIndex < 0) then
            CONTINUE;
        result := gmMakeRackPos(aLayout.Racks[xRackIndex].name, xPosInfoRecs[i].Pos);
        EXIT;
    end;
end;

function TSubstanceLoading.FindCapEntry(aLayout: TLayout; aTubeID, aRunName: string): TXRackPosition;
var
    xRackID: string;
    xRackNo: integer;
    xPosinfoDA: TPosinfoDataAdaptor;
begin
    result.Rack := nil;
    result.Pos := 0;

    xPosinfoDA := TPosinfoDataAdaptor.Create();
    try
        xPosinfoDA.SelectAndOpenTubeID(aTubeID, [tiCapID]);
        try
            if (xPosinfoDA.DataProvider.IsEmpty) then
                EXIT;
            result.Pos := xPosinfoDA.ReadPos();
            xRackID := xPosinfoDA.ReadRackID();
        finally
            xPosinfoDA.Close();
        end;
    finally
        FreeAndNil(xPosinfoDA);
    end;

    xRackNo := aLayout.GetRackNoForRackID(xRackID);
    if (result.Pos > 0) and (xRackNo >= 0) then
    begin
        result.Rack := aLayout.Racks[xRackNo]
    end;
end;

function TSubstanceLoading.FindCorrectedStorageVolumesByID(aLayout: TLayout; aSubstID: string;
    aUseDeadVolume: TUseDeadVolume): TArray<TRackPositionWithVol>;
var
    x: integer;
    xStorages: TArray<TRackIDPositionWithVol>;
begin
    xStorages := aLayout.FindStoragePositionsByID(aSubstID, aUseDeadVolume);

    SetLength(result, Length(xStorages));
    for x := 0 to high(xStorages) do
    begin
        result[x].Rack := xStorages[x].Rack;
        result[x].Pos := xStorages[x].Pos;

        // Volume um VolLimit kürzen
        result[x].Vol := xStorages[x].Vol;
        if (result[x].Vol < 0) then
            result[x].Vol := 0;
    end;
end;

function TSubstanceLoading.FindCorrectedStorageVolumesAll(aLayout: TLayout; aUseDeadVolume: TUseDeadVolume)
    : TArray<TRackIDPositionWithVol>;
var
    x: integer;
begin
    result := aLayout.FindStoragePositionsAll(aUseDeadVolume);

    for x := 0 to high(result) do
    begin
        // Volume um VolLimit kürzen
        result[x].Vol := result[x].Vol;
        if (result[x].Vol < 0) then
            result[x].Vol := 0;
    end;
end;

procedure TSubstanceLoading.SetWellStorageID(aRackWell: TRackWell; const aSubstID: string;
    aLoadColor: boolean; aMinVolume1, aMinVolume2, aMaxVolume: double; const aSubstanceSetName: string);
var
    xData: TRackWellStorageData;
begin
    xData.SubstID := aSubstID;
    if (aLoadColor) then
        xData.SubstColor := self.ReadColorBySubstID(aSubstID)
    else
        xData.SubstColor := 0;
    xData.MinVolume1 := aMinVolume1;
    xData.MinVolume2 := aMinVolume2;
    xData.MaxVolume := aMaxVolume;
    xData.SetName := aSubstanceSetName;

    aRackWell.StorageData := xData;
end;


end.
