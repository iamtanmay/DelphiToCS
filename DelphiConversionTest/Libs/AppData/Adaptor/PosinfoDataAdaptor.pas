{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Posinfo Data Adaptor encapsulates the access to Posinfo.db
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  16.06.03 wl                               TN1501.5 initial version
  26.06.03 wl  CreateCapEntry               TN1501.8 Schreibt für eine Position einen Deckel-Eintrag
  26.06.03 wl  FindRandomTubeBC             TN1501.6 Sucht alle Random-Barcodes für das Rack
  26.06.03 wl  MoveTubeProperties           TN1501   von dbTools hierher verschoben
  17.07.03 wl  FindFirstCap                 TN1501   neu: für Cap-Park-Racks
  22.07.03 wl  CapExists                    TN1501   gibt es ein Cap auf dieser Position
  05.08.03 wl  GetTubeOrigin                TN1536   2.Hälfte der Methode von ObjWorkbExt hierher
  03.09.03 wl  GetTubeOrigin                TN1568   benutzt DLLLoading.pas
  19.04.04 wl  RecordVolumes                TN1788   von AppInterface hierher
  27.04.04 wl  GetLastPipStepNumber         TN1881   ersetzt gmGetNoOfTubeAccess (dbRack)
  10.05.04 pk                               TN1889   ReadRackIDPosByTubeID, ReadPosinfoRecFromDataset, FindPosInfoRecByTubeID, etc.
  10.05.04 pk  TubeIDExists                 TN1889   call FindPosInfoRecByTubeID
  13.05.04 pk  FindPosinfoRecByTubeID       TN1889   allow type of tube ID to be found to be specified as parameter
  13.05.04 wl  GetPipVolume                 TN1788   Teil aus AppInterface.GetAPosition
  14.05.04 pk  TubeIDExists                 TN1920   from private to public
  17.06.04 pk  ReadPosinfoRecsByTubeID      TN1994   returns array of all records that match tube IDs
  17.06.04 pk  PrepareTransfer              TN1993   from ObjThreadFactory.CreateTransfer
  22.06.04 wl  FindFirstNewCap              TN1981   Findet nur noch Caps OHNE Tube-ID
  22.06.04 wl  FindFirstCapFreePos          TN1981   Findet die erste "freie" Position in einem Rack (ohne Cap)
  22.06.04 wl  FindCapEntry                 TN1981   Findet eine Kappe über die Tube-ID
  08.09.04 wl  GetNextStep                  TN2121   entspricht getLastStepNumber
  08.09.04 wl  AspGetNextStep               TN2121   GetNextStep + Volumenkontrolle aus AppInterface.AspLiquid
  08.09.04 wl  AppendRec                    TN2121   aus AppInterface hierher
  11.09.04 wl  RecordVolumes                TN2123   überflüssige Parameter entfernt
  08.11.04 wl  GetTubeOrigin                TN2213   OriginRecCnt wird separat (nicht Teil von TRackIDPos) behandelt
  08.11.04 wl  GetTubeOriginWithRecCnt      TN2213   OriginRecCnt als out-Parameter
  08.11.04 wl  DeleteTubeOrigin             TN2213   ruft WriteTubeOrigin mit leerer Position auf
  22.04.05 pk                               TN2398   Massive Changes
  22.04.05 wl                               TN2398   uses Variants
  04.05.05 wl  INT_POSINFO_UNIT_..          TN2410   von DevicesTips hierher
  19.05.05 wl  FindFirstNewCap              TN2414   wenn aLastOut = true werden die Positionen in umgekehrter Reihenfolge durchsucht
  29.06.05 wl  ReplaceSubstID               TN2480   ersetzt ChangeIDofCap -> ersetzt die Random-Tube-ID's auch, wenn sie in Asp/Disp-Steps stehen
  18.07.05 thr GetTotalPipvolume            TN2411   Gesamtvolumen eines Racks für MultitipRacks
  16.09.05 wl  INT_POSINFO_STEP_...         TN2574   von implementation nach interface
  23.11.05 thr Get-/ReadLastSubstID         TN2799   Neue Funktionen um die zuletzt pipettierte Substanz zu lesen
  28.11.05 thr GetLastTareWeightAndUnitBySubst TN2810 Auch Tarawert unter Beachtung der Substanz lesen, benutzt eigenes SQL-Statement
  02.02.06 wl  GetTotalPipVolume,GetPipVolume  TN2923 Geben jetzt auch Subst-ID zurück
  23.02.06 thr GetLastSubstID               TN2941   Vorbesetzung war inkorrekt
  19.04.06 wl  RecordVolumes                TN3051   benutzt TipFloatArray
  07.09.06 pk                               TN3292   new class functions to avoid the need to use the Instance
  03.08.07 wl                               TN3811.2 uses IntMap statt ObjStructures
  07.08.07 wl                               TN3811.3  TPosinfoAdapter.Create ohne Parameter, Instance entfernt
  04.09.07 pk  fTable                       TN3847   removed
  04.09.07 pk                               TN3847   Application-specific functions move to PosinfoDataAdaptorExt
  06.09.07 wl  GetTubeTareWeight            TN3828   entfernt
  06.09.07 wl  WriteSubstWeight             TN3828   jetzt mit Rückgabewert: Step-Nummer
  06.09.07 wl  GetAnyWeightAndUnit          TN3828   neu: Damit erhält man ein bestimmtes Gewicht, dessen Step-Nummer man sich gemerkt hat
  06.09.07 pk
  02.10.07 wl  TableName                    TN3811.5 neu
  09.11.07 pk                               TN3921   Changes for updatemanager
  09.11.07 pk                               TN3922   Dataset changed to DataProvider
  13.11.07 wl  WriteSubstWeight             TN3828   Compiler-Fehler korrigiert
  11.09.08 wl  Get-/ReadLastSubstID         TN4176   SubstID will only be changed it entry exists
  23.09.08 wl                               TN4236   Vereinigung mit ..Fieldnames
  13.01.09 wl  FindRandomTubeBC             TN4312    TRackPositions ist jetzt dynamisches Array
  16.01.09 wl                               TN4362   an Änderungen in TQueryDataAdaptor angepasst
  06.07.09 pk  WriteSubstWeight             TN4585.4 AppendRecord Removed.
  09.07.09 pk                               TN4585.4 DataProvider.Locate removed, functionality replaced by SelectAndOpen
  27.08.09 wl  INT_POSINFO_UNIT_DETECTED    TN4757   neu für LDDET Action
  12.09.09 wl  RecordVolumes                TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.04.10 wl                               TN5044   uses StringUtilities
  17.06.10 pk                               TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  23.08.10 wl  RecordVolumes                TN5214   unter Origin wird LOLI oder NOLI gespeichert!
  24.08.10 wl  RecordVolumes                TN5214   bei ordnungsgemäßer Detektion wird "Detected OK" eingetragen
  04.11.10 ts  MoveTubeProperties           TN5329   STR_SQL_WHERE_POS -> Posinfo."POS" instead of POS (reserved in TurboDB)
  07.03.11 wl  DeletePositions              TN5329   Posinfo."POS" instead of POS (reserved in TurboDB)
  02.05.11 wl  DeletePositions              TN5329   Posinfo."POS" instead of POS (reserved in TurboDB)
  05.05.11 wl  ReplaceSubstID               TN5572   für TurboDB geändert
  20.09.11 wl                               TN5723   neues Feld SubstColor (integer)
  20.09.11 wl  WriteAspStep,WriteDispStep   TN5723   neu, verwenden auch SubstColor
  20.09.11 wl  ReadSubstColor               TN5723   neu
  28.10.11 wl                               TN5728   Disposables-Einträge (Step is null) --> DisposableTipDataAdaptor
  28.10.11 wl                               TN5729   Substances-Einträge (Step = 0) --> SubstanceSetDataAdaptor
  31.10.11 wl                               TN5725   SUBSTCOLOR -> SubstanceData
  17.11.11 wl                               TN5725   Vieles im Zusammenhang mit Step=0 entfernt
  28.11.11 wl  INT_POSINFO_STEP_MINVOLUME   TN5730   neue Konstante für Totvolumen
  14.12.11 wl  DeletePositions              TN5765   entfernt
  27.12.11 wl  DeleteVolumeData             TN5768   löscht jetzt wirklich
  22.02.12 wl  MoveTubeProperties           TN5817   Einträge, die kleiner als -4 sind, werden nicht gelöscht (z.B. Einwaagen aus WGHP)
  27.03.12 wl  ReadTubeID                   TN5842   Tube-ID wurde nicht mehr bei Step=-1 gesucht, sondern irgendwo (seit V 7.4.0!!)
  25.04.12 ts  MoveTubeProperties           TN5876   Wiegewerte (Step<=-1000) werden nicht mit verschoben (sonst keine Mehrfacheinwaagen möglich)
  10.09.12 wl  INT_POSINFO_STEP_MINVOLUME2  TN5979   neu = -6
  10.09.12 wl  INT_POSINFO_STEP_LOWEST_DELETED_AT_MOVE   TN5979.1   alte Cap-Einträge werden bei TubeMove jetzt auch gelöscht
  11.09.12 wl  DeleteVolumeData             TN5979   löscht bei dem Eintrag mit Step 0 nur das Volumen
  12.09.12 wl  WriteStorageData             TN5979   neu von SubstanceLoading hierher
  27.03.13 wl                               TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit PosinfoDataAdaptor;


interface


uses
    Classes,
    DataProvider,
    CommonTypes,
    DataAdaptor,
    AppTypes,
    QueryDataAdaptor,
    GeneralTypes;

const
    INT_POSINFO_UNIT_LIQUID = 1;
    INT_POSINFO_UNIT_POWDER = 2;
    INT_POSINFO_UNIT_MANUAL = 3;
    INT_POSINFO_UNIT_DETECTED = 4;

    INT_POSINFO_STEP_MIN_SUBSTID = 0;
    INT_POSINFO_STEP_TUBEID = -1;
    INT_POSINFO_STEP_DETECTED = -2;
    INT_POSINFO_STEP_MINVOLUME1 = -3;
    INT_POSINFO_STEP_MAXVOLUME = -4;
    INT_POSINFO_STEP_CAP = -5;
    INT_POSINFO_STEP_MINVOLUME2 = -6;

    // Bei Tube-Moves werden vorhandene Einträge der Dest-Position gekillt, die großer und gleich dieser Zahl sind:
    INT_POSINFO_STEP_LOWEST_DELETED_AT_MOVE = -6;
    // Selbst-definiertes und Einwaagen bleiben also erhalten (stellen aber ein Indexfehler-Risiko dar)

    INT_POSINFO_STEP_WEIGHT = -899; // cPosinfoWeight
    INT_POSINFO_STEP_NET_WEIGHT = -900; // cPosinfoNetWeight
    INT_POSINFO_STEP_ORIGIN = -999;
    INT_POSINFO_STEP_TARE_WEIGHT = -1000;
    // cPosinfoTaraWeight.  This is the first tare weight, then comes -1001, -1002, etc

type
    TTubeIDType = (tiTubeID, tiCapID);
    TTubeIDTypes = set of TTubeIDType;

    TPosInfoRec = record
        Valid: boolean;
        RackID: string;
        Pos: integer;
        Step: integer;
        SubstID: string;
        DateAndTime: TDateTime;
        Origin: string;
        Amount: double;
        UnitID: integer;
    end;

    TPosinfoRecs = array of TPosinfoRec;

    TPosinfoDataAdaptor = class(TQueryDataAdaptor)
    private
        class procedure ReadLastSubstID(aDataset: TDataProvider; var vSubstID: string; const aRackID: string;
            aPos: integer);
        class function ReadRackIDFromDataset(aDataset: TDataProvider): string;
        class function ReadPosFromDataset(aDataset: TDataProvider): integer;
        class function ReadAmountFromDataset(aDataset: TDataProvider): double;
        class function ReadUnitFromDataset(aDataset: TDataProvider): integer;
        class function ReadSubstIDFromDataset(aDataset: TDataProvider): string;
        class function ReadStepFromDataset(aDataset: TDataProvider): integer;
        class function ReadOriginFromDataset(aDataset: TDataProvider): string;
        class function ReadDateFromDataset(aDataset: TDataProvider): TDateTime;

        class procedure WriteRackIDToDataset(aDataset: TDataProvider; const aRackID: string);
        class procedure WritePosToDataset(aDataset: TDataProvider; aPos: integer);
        class procedure WriteStepToDataset(aDataset: TDataProvider; aStep: integer);
        class procedure WriteSubstIDToDataset(aDataset: TDataProvider; const aSubstID: string);
        class procedure WriteAmountToDataset(aDataset: TDataProvider; aAmount: double);
        class procedure WriteOriginToDataset(aDataset: TDataProvider; const aOrigin: string);
        class procedure WriteUnitToDataset(aDataset: TDataProvider; aUnit: integer);
        class procedure WriteDateToDataset(aDataset: TDataProvider; aDate: TDateTime);

    private
        // private methods
        procedure ReplaceSubstID(aOldTubeID, aNewTubeID: string);

        function InternGetTareWeightAndUnit(out oTareWeight: double; out oUnit: integer): boolean;
        function InternGetBalanceTareVal(out oTareWeight: double; out oUnit: integer): boolean;
        procedure DeleteByRackIDAndPosAndStep(const aRackID: string; aPos, aStep: integer);
        procedure SelectAndOpenByBalanceAndPos(const aBalName: string; aBalPos: integer; aReadOnly: boolean);
        procedure AppendEntry(const aRackID: string; aPos, aStep: integer; const aSubstID: string);
    public
        constructor Create();
        // public methods
        class function IsRandomTubeID(aTubeID: string): boolean;
        class function ReadNextStep(aDataset: TDataProvider; const aRackID: string; aPos: integer): integer;
        class procedure WritePosinfoDataToDataset(aDataset: TDataProvider; aAppend: boolean;
            aWriteAmount: boolean; const aRackID: string; aPos, aStep: integer;
            const aSubstID, aOrigin: string; aDateAndTime: TDateTime; aAmount: double; aUnitID: integer);

        // TQuery-Based
        procedure SelectAndOpenAll(aReadOnly: boolean);
        procedure SelectAndOpenByRackID(const aRackID: string; aReadOnly: boolean = true);
        procedure SelectByRackIDAndSubstID(const aRackID, aSubstID: string; aReadOnly: boolean = true);
        procedure SelectAndOpenByRackIDAndPos(const aRackID: string; aPos: integer; aReadOnly: boolean);
        procedure SelectAndOpenByRackIDAndPosAndSubstID(const aRackID: string; aPos: integer;
            const aSubstID: string; aReadOnly: boolean);
        procedure SelectAndOpenByRackIDAndStep(const aRackID: string; aStep: integer; aReadOnly: boolean);
        procedure SelectAndOpenByRackIDAndPosAndStep(const aRackID: string; aPos: integer; aStep: integer;
            aReadOnly: boolean);
        procedure SelectAndOpenByDetectedRackID(const aRackID: string; aReadOnly: boolean);
        function GetTubeOrigin(aCurrentPos: TRackIDPosition; aRunName: string): TRackIDPosition;

        function ReadRackID: string;
        function ReadPos(): integer;
        function ReadStep(): integer;
        function ReadSubstID(): string;
        function ReadAmount(): double;
        function ReadOrigin(): string;
        function ReadUnit(): integer;

        procedure WriteRackID(const aRackID: string);
        procedure WritePos(aPos: integer);
        procedure WriteStep(aStep: integer);
        procedure WriteSubstID(const aSubstID: string);
        procedure WriteAmount(aAmount: double);
        procedure WriteUnit(aUnit: integer);
        procedure WriteOrigin(aOrigin: string);
        procedure WriteDate(aDate: TDateTime);

        procedure InternInsertIfNotFound(const aRackID: string; aPos: integer);
        procedure ChangeRackID(const aOldRackID, aNewRackID: string);

        function EditOrAppendRec(const aKeyRackID: string; aKeyPos, aKeyStep: integer; const aSubstID: string;
            const aOrigin: string; aAmount: double; aUnit: integer): boolean;

        procedure AppendRec(aRackID: string; aPos, aStep: integer; aSubstID, aOrigin: string; aAmount: double;
            aUnitID: integer);

        function GetPosFromRackIDAndSubstID(const aRackID, aSubstID: string; out oPos: integer): boolean;
        function GetPosFromRackID(const aRackID: string; out oPos: integer): boolean;

        // Balance
        function GetBalanceTareVal(const aBalName: string; out oTareVal: double): boolean;
        function GetBalanceTareValByPos(const aBalName: string; aBalPos: integer; out oTareWeight: double;
            out oUnit: integer): boolean;
        function GetTareWeightAndUnit(const aRackID: string; aPos: integer; out oTareWeight: double;
            out oUnit: integer): boolean;
        function GetLastTareWeightAndUnit(const aRackID: string; aPos: integer; out oTareWeight: double;
            out oUnit: integer): boolean;
        function GetLastTareWeightAndUnitBySubst(const aRackID: string; aPos: integer; const cSubst: string;
            out oTareWeight: double; out oUnit: integer): boolean;
        function GetTareWeightFromPos(aTubePos: TRackIDPosition; out oTareWeight: double): boolean;
        function GetAnyWeightAndUnit(const aRackID: string; aPos: integer; aStep: integer;
            out oTareWeight: double; out oUnit: integer): boolean;

        // public methods
        function CreateRandomTubeID: string;
        procedure WriteTubeID(aTubeID: string; aTubePos: TRackIDPosition);
        function ReadTubeID(aTubePos: TRackIDPosition): string;
        procedure WriteCapEntry(aTubePos: TRackIDPosition);
        function CapExists(aTubePos: TRackIDPosition; var vTubeID: string): boolean;
        procedure MoveCapEntry(aSourcePos, aDestPos: TRackIDPosition);

        procedure WriteTubeOrigin(aCurrentPos, aOrigin: TRackIDPosition);
        procedure DeleteTubeOrigin(aCurrentPos: TRackIDPosition);
        function FindRandomTubeBC(const aRackName, aRackID: string): TRackPositions;
        procedure MoveTubeProperties(const aSRackID: string; aSPos: integer; const aDRackID: string;
            aDPos: integer);
        function FindFirstNewCap(aRackID: string; aLastOut: boolean): integer;
        function FindFirstCapFreePos(aRackID: string; aNoOfPositions: integer): integer;
        procedure DeleteVolumeData(const aRackID: string; aPos: integer);
        procedure DeleteStorageData(const aRackID: string; aPos: integer);
        procedure WriteStorageData(const aRackID: string; aPos: integer; const aSubstID: string;
            aMinVolume1, aMinVolume2, aMaxVolume: double; const aSubstanceSetName: string);

        // Balance
        function WriteSubstWeight(aBalName: string; aWeight: double; aRackID: string; aPos: Integer;
            aSubstID: string; out oStepNo: integer): boolean;
        procedure WriteTareWeight(aBalName: string; aWeight: double);

        // WeighPosition
        procedure GetLastSubstID(var vSubstID: string; aRackID: string; aPos: integer);
        //
        function ReadPosInfoRecsByTubeID(aTubeID: string): TPosinfoRecs;
        function TubeIDExists(aTubeID: string): boolean;
        // Sophas Transfer
        procedure PrepareTransfer(aRackID: string; aFirstSource, aLastSource, aStartVolume: integer);

        function ReadPosInfoRecsByRackIDAndSubstID(aRackID, aSubstID: string): TPosinfoRecs;

        procedure SelectAndOpenTubeID(aTubeID: string; aTubeIDTypes: TTubeIDTypes);
        procedure SelectAndOpenTareWeightByRackIDAndPos(const aRackID: string; aPos: integer;
            aStepOffset: integer; const aReadOnly: boolean);

        class function MakePosInfoRec(aValid: boolean; aRackID: string; aPos, aStep: integer;
            aSubstID: string; aDateAndTime: TDateTime; aOrigin: string; aAmount: double; aUnitID: integer)
            : TPosinfoRec;
        class procedure WritePosinfoRecToDataset(aDataset: TDataProvider; aPosInfoRec: TPosInfoRec;
            aAppend: boolean);
        class function ReadPosinfoRecFromDataset(aDataset: TDataProvider): TPosInfoRec;
        class function ReadPosinfoRecsFromDataset(aDataset: TDataProvider): TPosinfoRecs;
        class function MakeSQLStepIsIn(aTubeIDTypes: TTubeIDTypes): string;

        class function GetTubeFirstWeight(aTubePos: TRackIDPosition; var vUnit: integer): double;
        class function GetTubeID(aTubePos: TRackIDPosition): string;
        class procedure WriteWeight(const aRackID, aSubstID, aOrigin: string; aPos, aStep, aUnit: integer;
            aWeight: double);
        class procedure InstChangeRackID(const aOldID, aNewID: string);
    end;


implementation


uses
    Variants,
    Windows,
    Generics.Collections,
    SysUtils,
    LogManager,
    Utility2,
    StringUtilities;

const
    // Posinfo.DB:
    STR_TABLE_POSINFO = 'Posinfo';
    STR_POSINFO_FLD_RACKID = 'RACKID'; // ALPHA(20)
    STR_POSINFO_FLD_POS = 'POS'; // SHORT
    STR_POSINFO_FLD_STEP = 'STEP'; // LONG
    STR_POSINFO_FLD_ID = 'SUBSTID'; // ALPHA(30 oder mehr)
    STR_POSINFO_FLD_DATETIME = 'DateTime'; // TIMESTAMP
    STR_POSINFO_FLD_ORIGIN = 'Origin'; // ALPHA(80)
    STR_POSINFO_FLD_AMOUNT = 'Amount'; // NUMBER
    STR_POSINFO_FLD_UNIT = 'Unit'; // LONG

    STR_DESTINATION = 'Destination';
    INT_POSINFO_POS_INVALID = 0;

    STR_POSINFO_RANDOM_BARCODE = 'RANDOMBC';

    STR_ORIGIN_NONE = '';
    STR_ORIGIN_LIQ_DETECTED = 'Liquid Detection';

    INT_BALANCE_FIRST_POS = 0;

    INT_POSINFO_WEIGHT_UNIT = 2;

    STR_SUBSTID_NONE = '';
    STR_SUBSTID_DETECTED_FIRST_TIME = '1';

    STR_INDEX_RACKID = STR_POSINFO_FLD_RACKID + ';';

    STR_INDEX_RACKID_POS = STR_INDEX_RACKID + STR_POSINFO_FLD_POS + ';';

    STR_INDEX_RACKID_POS_STEP = STR_INDEX_RACKID_POS + STR_POSINFO_FLD_STEP + ';';

    STR_SQL_TBL = STR_TABLE_POSINFO;

    STR_SQL_TBL_DOT = STR_SQL_TBL + '.';

    STR_SQL_SELECT = 'SELECT * FROM ' + STR_SQL_TBL;
    STR_SQL_DELETE = 'DELETE FROM ' + STR_SQL_TBL;

    STR_SQL_WHERE_POS = STR_SQL_TBL_DOT + '"' + STR_POSINFO_FLD_POS + '"';

    STR_SQL_WHERE_RACKID_FMT = STR_POSINFO_FLD_RACKID + ' = ''%s''';
    STR_SQL_WHERE_SUBSTID_FMT = STR_POSINFO_FLD_ID + ' = ''%s''';
    STR_SQL_WHERE_POS_FMT = STR_SQL_WHERE_POS + ' = %d';
    STR_SQL_WHERE_STEP_FMT = STR_SQL_TBL_DOT + '"' + STR_POSINFO_FLD_STEP + '"' + ' = %d';

    STR_SQL_WHERE_RACKID_POS_STEP = STR_SQL_WHERE_RACKID_FMT + ' AND ' + STR_SQL_WHERE_POS_FMT + ' AND ' +
        STR_SQL_WHERE_STEP_FMT;

    STR_SQL_UPDATE_RACKID_FMT = 'UPDATE ' + STR_SQL_TBL + ' SET ' + STR_POSINFO_FLD_RACKID +
        ' = ''%s'' WHERE ' + STR_SQL_WHERE_RACKID_FMT;

    { TPosinfoDataAdaptor }

constructor TPosinfoDataAdaptor.Create();
begin
    inherited Create(STR_TABLE_POSINFO);
end;

procedure TPosinfoDataAdaptor.SelectAndOpenAll(aReadOnly: boolean);
begin
    SelectAndOpen(STR_SQL_SELECT, aReadOnly);
end;

procedure TPosinfoDataAdaptor.SelectAndOpenByRackID(const aRackID: string; aReadOnly: boolean = true);
begin
    SelectAndOpen(Format(STR_SQL_SELECT + ' WHERE ' + STR_SQL_WHERE_RACKID_FMT, [aRackID]), aReadOnly);

end;

procedure TPosinfoDataAdaptor.SelectByRackIDAndSubstID(const aRackID, aSubstID: string;
    aReadOnly: boolean = true);
begin
    SelectAndOpen(Format(STR_SQL_SELECT + ' WHERE ' + STR_SQL_WHERE_RACKID_FMT + ' AND ' +
        STR_SQL_WHERE_SUBSTID_FMT, [aRackID, aSubstID]), aReadOnly);

end;

procedure TPosinfoDataAdaptor.SelectAndOpenByRackIDAndPosAndSubstID(const aRackID: string; aPos: integer;
    const aSubstID: string; aReadOnly: boolean);
begin
    SelectAndOpen(Format(STR_SQL_SELECT + ' WHERE ' + STR_SQL_WHERE_RACKID_FMT + ' AND ' +
        STR_SQL_WHERE_POS_FMT + ' AND ' + STR_SQL_WHERE_SUBSTID_FMT, [aRackID, aPos, aSubstID]), aReadOnly);
end;

procedure TPosinfoDataAdaptor.SelectAndOpenByRackIDAndPos(const aRackID: string; aPos: integer;
    aReadOnly: boolean);
begin
    SelectAndOpen(Format(STR_SQL_SELECT + ' WHERE ' + STR_SQL_WHERE_RACKID_FMT + ' AND ' +
        STR_SQL_WHERE_POS_FMT, [aRackID, aPos]), aReadOnly);
end;

procedure TPosinfoDataAdaptor.SelectAndOpenByRackIDAndStep(const aRackID: string; aStep: integer;
    aReadOnly: boolean);
begin
    SelectAndOpen(Format(STR_SQL_SELECT + ' WHERE ' + STR_SQL_WHERE_RACKID_FMT + ' AND ' +
        STR_SQL_WHERE_STEP_FMT, [aRackID, aStep]), aReadOnly);
end;

procedure TPosinfoDataAdaptor.SelectAndOpenByRackIDAndPosAndStep(const aRackID: string; aPos: integer;
    aStep: integer; aReadOnly: boolean);
begin
    SelectAndOpen(Format(STR_SQL_SELECT + ' WHERE ' + STR_SQL_WHERE_RACKID_POS_STEP, [aRackID, aPos, aStep]),
        aReadOnly);
end;

procedure TPosinfoDataAdaptor.DeleteByRackIDAndPosAndStep(const aRackID: string; aPos: integer;
    aStep: integer);
begin
    self.ExecSQLFmt(STR_SQL_DELETE + ' WHERE ' + STR_SQL_WHERE_RACKID_POS_STEP, [aRackID, aPos, aStep]);
end;

procedure TPosinfoDataAdaptor.SelectAndOpenByDetectedRackID(const aRackID: string; aReadOnly: boolean);
begin
    SelectAndOpenByRackIDAndStep(aRackID, INT_POSINFO_STEP_DETECTED, aReadOnly);
end;

procedure TPosinfoDataAdaptor.SelectAndOpenTubeID(aTubeID: string; aTubeIDTypes: TTubeIDTypes);
var
    xStepIsIn: string;
begin
    xStepIsIn := MakeSQLStepIsIn(aTubeIDTypes);
    if xStepIsIn <> '' then
        xStepIsIn := Format(' AND %s %s', [STR_POSINFO_FLD_STEP, xStepIsIn]);

    SelectAndOpen(STR_SQL_SELECT + ' WHERE ' + STR_POSINFO_FLD_ID + '=''' + aTubeID + '''' + xStepIsIn, true);
end;

procedure TPosinfoDataAdaptor.SelectAndOpenTareWeightByRackIDAndPos(const aRackID: string; aPos: integer;
    aStepOffset: integer; const aReadOnly: boolean);
begin
    ASSERT(aStepOffset >= 0);
    SelectAndOpenByRackIDAndPosAndStep(aRackID, aPos, INT_POSINFO_STEP_TARE_WEIGHT - aStepOffset, aReadOnly);
end;

procedure TPosinfoDataAdaptor.SelectAndOpenByBalanceAndPos(const aBalName: string; aBalPos: integer;
    aReadOnly: boolean);
begin
    SelectAndOpenByRackIDAndPosAndStep(aBalName, aBalPos, INT_POSINFO_STEP_WEIGHT, aReadOnly);
end;

function TPosinfoDataAdaptor.ReadRackID(): string;
begin
    result := ReadRackIDFromDataset(self.DataProvider);
end;

function TPosinfoDataAdaptor.ReadPos(): integer;
begin
    result := ReadPosFromDataset(self.DataProvider);
end;

function TPosinfoDataAdaptor.ReadStep(): integer;
begin
    result := ReadStepFromDataset(self.DataProvider);
end;

function TPosinfoDataAdaptor.ReadSubstID(): string;
begin
    result := ReadSubstIDFromDataset(self.DataProvider);
end;

function TPosinfoDataAdaptor.ReadAmount(): double;
begin
    result := ReadAmountFromDataset(self.DataProvider);
end;

function TPosinfoDataAdaptor.ReadOrigin(): string;
begin
    result := ReadOriginFromDataset(self.DataProvider);
end;

function TPosinfoDataAdaptor.ReadUnit(): integer;
begin
    result := ReadUnitFromDataset(self.DataProvider);
end;

procedure TPosinfoDataAdaptor.WriteRackID(const aRackID: string);
begin
    WriteRackIDToDataset(self.DataProvider, aRackID);
end;

procedure TPosinfoDataAdaptor.WritePos(aPos: integer);
begin
    WritePosToDataset(self.DataProvider, aPos);
end;

procedure TPosinfoDataAdaptor.WriteStep(aStep: integer);
begin
    WriteStepToDataset(self.DataProvider, aStep);
end;

procedure TPosinfoDataAdaptor.WriteSubstID(const aSubstID: string);
begin
    WriteSubstIDToDataset(self.DataProvider, aSubstID);
end;

procedure TPosinfoDataAdaptor.WriteAmount(aAmount: double);
begin
    WriteAmountToDataset(self.DataProvider, aAmount);
end;

procedure TPosinfoDataAdaptor.WriteUnit(aUnit: integer);
begin
    WriteUnitToDataset(self.DataProvider, aUnit);
end;

procedure TPosinfoDataAdaptor.WriteOrigin(aOrigin: string);
begin
    WriteOriginToDataset(self.DataProvider, aOrigin);
end;

procedure TPosinfoDataAdaptor.WriteDate(aDate: TDateTime);
begin
    WriteDateToDataset(self.DataProvider, aDate);
end;

procedure TPosinfoDataAdaptor.AppendEntry(const aRackID: string; aPos: integer; aStep: integer;
    const aSubstID: string);
begin
    self.DataProvider.Append;
    WriteRackID(aRackID);
    WritePos(aPos);
    WriteStep(aStep);
    WriteSubstID(aSubstID);
    WriteDate(Now());
    self.DataProvider.Post;
end;

procedure TPosinfoDataAdaptor.InternInsertIfNotFound(const aRackID: string; aPos: integer);
begin
    self.SelectAndOpenByRackIDAndPos(aRackID, aPos, false);
    try
        if not self.DataProvider.IsEmpty then
            EXIT;
        self.DataProvider.Append;
        WriteRackID(aRackID);
        WritePos(aPos);
        self.DataProvider.Post;
    finally
        self.Close();
    end;
end;

procedure TPosinfoDataAdaptor.ChangeRackID(const aOldRackID, aNewRackID: string);
// change every occurence of aOldRackID to aNewRackID
begin
    self.ExecSQLFmt(STR_SQL_UPDATE_RACKID_FMT, [aNewRackID, aOldRackID]);
end;

procedure TPosinfoDataAdaptor.WriteTubeID(aTubeID: string; aTubePos: TRackIDPosition);
var
    xOldTubeID: string;
    xTubeIDExists: boolean;
begin
    if trim(aTubePos.RackID) = '' then
        exit;
    if trim(aTubeID) = '' then
        exit;

    xOldTubeID := '';
    SelectAndOpenByRackIDAndPosAndStep(aTubePos.RackID, aTubePos.Pos, INT_POSINFO_STEP_TUBEID, false);
    try
        xTubeIDExists := not self.DataProvider.IsEmpty;
        if xTubeIDExists then
        begin
            // edit tube id entry
            xOldTubeID := ReadSubstID();
            self.DataProvider.Edit;
            WriteSubstID(aTubeID);
            self.DataProvider.Post;
        end;
    finally
        Close();
    end;

    if xTubeIDExists then
    begin
        // find cap with the same tube-ID and change its tube ID
        ReplaceSubstID(xOldTubeID, aTubeID);
    end
    else
    begin
        SelectAndOpenAll(false);
        try
            AppendEntry(aTubePos.RackID, aTubePos.Pos, INT_POSINFO_STEP_TUBEID, aTubeID);
        finally
            Close();
        end;
    end;

    gLogManager.LogF('Rack [%s] Position [%d] - TubeID set to: [%s]',
        [aTubePos.Rack, aTubePos.Pos, aTubeID], true);

end;

function TPosinfoDataAdaptor.ReadTubeID(aTubePos: TRackIDPosition): string;
begin
    self.SelectAndOpenByRackIDAndPosAndStep(aTubePos.RackID, aTubePos.Pos, INT_POSINFO_STEP_TUBEID, true);
    try
        if self.DataProvider.IsEmpty then
            EXIT('');
        EXIT(ReadSubstID());
    finally
        Close();
    end;
end;

procedure TPosinfoDataAdaptor.WriteTubeOrigin(aCurrentPos, aOrigin: TRackIDPosition);
begin
    if (trim(aCurrentPos.RackID) = '') or (aCurrentPos.Pos = 0) then
        exit;

    if (aOrigin.Pos = 0) then
    begin
        // ------------------------------------------------------------------- Herkunfts-Angabe löschen
        self.DeleteByRackIDAndPosAndStep(aCurrentPos.RackID, aCurrentPos.Pos, INT_POSINFO_STEP_ORIGIN);
        EXIT;
    end;

    SelectAndOpenByRackIDAndPosAndStep(aCurrentPos.RackID, aCurrentPos.Pos, INT_POSINFO_STEP_ORIGIN, false);
    try
        // ------------------------------------------------------ Herkunfts-Angabe einfügen oder ändern
        if not self.DataProvider.IsEmpty then
            self.DataProvider.Edit
        else
            self.DataProvider.Append;

        WriteRackID(aCurrentPos.RackID);
        WritePos(aCurrentPos.Pos);
        WriteStep(INT_POSINFO_STEP_ORIGIN);
        WriteOrigin(aOrigin.Rack + ';' + aOrigin.RackID + ';' + IntToStr(aOrigin.Pos) + ';0');

        self.DataProvider.Post;
    finally
        Close();
    end;
end;

procedure TPosinfoDataAdaptor.DeleteTubeOrigin(aCurrentPos: TRackIDPosition);
var
    xDummyPos: TRackIDPosition;
begin
    xDummyPos.Pos := 0; // xDummyPos.Pos=0 -> Eintrag wird gelöscht

    WriteTubeOrigin(aCurrentPos, xDummyPos);
end;

procedure TPosinfoDataAdaptor.WriteCapEntry(aTubePos: TRackIDPosition);
var
    xTubeID: string;
begin
    if trim(aTubePos.RackID) = '' then
        exit;

    xTubeID := ReadTubeID(aTubePos);

    SelectAndOpenByRackIDAndPosAndStep(aTubePos.RackID, aTubePos.Pos, INT_POSINFO_STEP_CAP, false);
    try
        if self.DataProvider.IsEmpty then
        begin
            AppendEntry(aTubePos.RackID, aTubePos.Pos, INT_POSINFO_STEP_CAP, xTubeID);
        end;
    finally
        Close();
    end;
    gLogManager.LogF('Rack [%s] Position [%d] - Cap entry created (Tube ID: %s)',
        [aTubePos.Rack, aTubePos.Pos, xTubeID], false);
end;

function TPosinfoDataAdaptor.CapExists(aTubePos: TRackIDPosition; var vTubeID: string): boolean;
begin
    result := false;
    if trim(aTubePos.RackID) = '' then
        exit;

    SelectAndOpenByRackIDAndPosAndStep(aTubePos.RackID, aTubePos.Pos, INT_POSINFO_STEP_CAP, true);
    try
        if not self.DataProvider.IsEmpty then
        begin
            vTubeID := ReadSubstID();
            result := true;
        end;
    finally
        Close();
    end;
end;

procedure TPosinfoDataAdaptor.MoveCapEntry(aSourcePos, aDestPos: TRackIDPosition);
begin
    SelectAndOpenByRackIDAndPosAndStep(aSourcePos.RackID, aSourcePos.Pos, INT_POSINFO_STEP_CAP, false);
    try
        if self.DataProvider.IsEmpty then
            EXIT;

        if trim(aDestPos.RackID) = '' then
        begin
            // Cap Waste: delete cap entry
            self.DataProvider.Delete;
            gLogManager.LogF('Rack [%s] Position [%d] - Cap entry deleted (Cap Waste!)',
                [aSourcePos.Rack, aSourcePos.Pos], true);
        end
        else
        begin
            self.DataProvider.Edit;
            WriteRackID(aDestPos.RackID);
            WritePos(aDestPos.Pos);
            self.DataProvider.Post;
            gLogManager.LogF('Cap moved from Rack [%s] Position [%d] to Rack [%s] Position [%d]',
                [aSourcePos.Rack, aSourcePos.Pos, aDestPos.Rack, aDestPos.Pos], true);
        end;
    finally
        Close();
    end;
end;

procedure TPosinfoDataAdaptor.DeleteVolumeData(const aRackID: string; aPos: integer);
var
    xChangedRecs: integer;
begin
    if (aRackID <> '') then
    begin
        // bei Step = 0 nur das Volumen auf 0 setzen
        xChangedRecs := self.DataProvider.ExecSQL('update ' + STR_TABLE_POSINFO + ' set ' +
            STR_POSINFO_FLD_AMOUNT + '=0 where ' + STR_POSINFO_FLD_RACKID + '=''' + aRackID + '''' +
            ' and (step = 0) and ' + STR_SQL_WHERE_POS + '=' + IntToStr(aPos));

        // Alle Steps höher als 0 löschen
        xChangedRecs := xChangedRecs + self.DataProvider.ExecSQL('delete from ' + STR_TABLE_POSINFO +
            ' where ' + STR_POSINFO_FLD_RACKID + '=''' + aRackID + '''' + ' and (step > 0) and ' +
            STR_SQL_WHERE_POS + '=' + IntToStr(aPos));

        if (xChangedRecs > 0) then
            gLogManager.LogF('RackID [%s] Position [%d] - Volume information deleted in posinfo',
                [aRackID, aPos], false);
    end
end;

procedure TPosinfoDataAdaptor.MoveTubeProperties(const aSRackID: string; aSPos: integer;
    const aDRackID: string; aDPos: integer);
begin
    if (aSRackID = '') then
        exit;

    if (aDRackID = '') then
    begin
        // Source-Daten löschen
        if (self.DataProvider.ExecSQL('delete from ' + STR_TABLE_POSINFO + ' where ' + STR_POSINFO_FLD_RACKID
            + '=''' + aSRackID + '''' + ' and ' + STR_SQL_WHERE_POS + '=' + IntToStr(aSPos)) > 0) then
            gLogManager.LogF('RackID [%s] Position [%d] - Tube information deleted in posinfo',
                [aSRackID, aSPos], false);
    end
    else
    begin
        // Dest-Daten löschen
        if (self.DataProvider.ExecSQL('delete from ' + STR_TABLE_POSINFO + ' where ' + STR_POSINFO_FLD_RACKID
            + '=''' + aDRackID + '''' + ' and ' + STR_SQL_WHERE_POS + '=' + IntToStr(aDPos) + ' and ' +
            STR_POSINFO_FLD_STEP + '>= ' + IntToStr(INT_POSINFO_STEP_LOWEST_DELETED_AT_MOVE)) > 0) then
            gLogManager.LogF('RackID [%s] Position [%d] - Tube information deleted in posinfo',
                [aDRackID, aDPos], false);

        // Source-Daten zur Dest-Position verschieben
        if (self.DataProvider.ExecSQL('update ' + STR_TABLE_POSINFO + ' set ' + STR_POSINFO_FLD_RACKID + '='''
            + aDRackID + '''' + ', ' + STR_SQL_WHERE_POS + '=' + IntToStr(aDPos) + ' where ' +
            STR_POSINFO_FLD_RACKID + '=''' + aSRackID + '''' + ' and ' + STR_SQL_WHERE_POS + '=' +
            IntToStr(aSPos) + ' and ' + STR_POSINFO_FLD_STEP + '> ' + IntToStr(INT_POSINFO_STEP_TARE_WEIGHT)
            ) > 0) then
            gLogManager.LogF
                ('Tube information moved from RackID [%s] Position [%d] to RackID [%s] Position [%d]',
                [aSRackID, aSPos, aDRackID, aDPos], false);
    end;
end;

function TPosinfoDataAdaptor.CreateRandomTubeID: string;
var
    xTubeID: string;
begin
    Randomize;
    repeat
        xTubeID := STR_POSINFO_RANDOM_BARCODE + IntToStr(Random( high(Integer)));
    until (not TubeIDExists(xTubeID));

    result := xTubeID;
end;

function TPosinfoDataAdaptor.ReadPosInfoRecsByRackIDAndSubstID(aRackID, aSubstID: string): TPosinfoRecs;
begin
    self.SelectByRackIDAndSubstID(aRackID, aSubstID);
    try
        result := ReadPosInfoRecsFromDataset(self.DataProvider);
    finally
        Close();
    end;
end;

function TPosinfoDataAdaptor.ReadPosInfoRecsByTubeID(aTubeID: string): TPosinfoRecs;
begin
    SelectAndOpenTubeID(aTubeID, [tiTubeID]); // nur noch Tube ID, SubstID hat sich erledigt
    try
        if self.DataProvider.IsEmpty() then
            Exit;
        result := ReadPosInfoRecsFromDataset(self.DataProvider);
    finally
        self.DataProvider.Close;
    end;
end;

function TPosinfoDataAdaptor.TubeIDExists(aTubeID: string): boolean;
begin
    SelectAndOpenTubeID(aTubeID, [tiTubeID]);
    try
        result := not self.DataProvider.IsEmpty()
    finally
        self.DataProvider.Close;
    end;
end;

function TPosinfoDataAdaptor.FindRandomTubeBC(const aRackName, aRackID: string): TRackPositions;
var
    xNoOfTubes: integer;
begin
    xNoOfTubes := 0;
    SetLength(result, 0);
    SelectAndOpenByRackIDAndStep(aRackID, INT_POSINFO_STEP_TUBEID, true);
    try
        while (not self.DataProvider.EOF) do
        begin
            // does this position have a random barcode?
            if IsRandomTubeID(ReadSubstID()) then
            begin
                SetLength(result, xNoOfTubes + 1);
                result[xNoOfTubes].Rack := aRackName;
                result[xNoOfTubes].Pos := ReadPos();
                inc(xNoOfTubes);
            end;

            self.DataProvider.Next;
        end;
    finally
        Close();
    end;
end;

class function TPosinfoDataAdaptor.IsRandomTubeID(aTubeID: string): boolean;
begin
    result := (Pos(STR_POSINFO_RANDOM_BARCODE, aTubeID) = 1);
end;

procedure TPosinfoDataAdaptor.ReplaceSubstID(aOldTubeID, aNewTubeID: string);
begin
    // Cap- und Asp/Disp-Einträge mit OldTube-ID finden und durch neue ID ersetzen
    SelectAndOpen('select * from ' + STR_TABLE_POSINFO + ' where ' + STR_POSINFO_FLD_ID + '=''' + aOldTubeID +
        '''' + ' and (' + STR_POSINFO_FLD_STEP + '=' + IntToStr(INT_POSINFO_STEP_CAP) + ' or ' +
        STR_POSINFO_FLD_STEP + '>0)', false);
    try
        while not self.DataProvider.Eof do
        begin
            self.DataProvider.Edit;
            self.DataProvider.FieldByName(STR_POSINFO_FLD_ID).AsString := aNewTubeID;
            self.DataProvider.Post;

            self.DataProvider.Next;
        end;
    finally
        Close();
    end;
end;

function TPosinfoDataAdaptor.FindFirstNewCap(aRackID: string; aLastOut: boolean): integer;
begin
    result := 0;

    // select all tube barcodes of this rack
    SelectAndOpen('select * from ' + STR_TABLE_POSINFO + ' where ' + STR_POSINFO_FLD_RACKID + '=''' + aRackID
        + '''' + ' and ' + STR_POSINFO_FLD_STEP + '=' + IntToStr(INT_POSINFO_STEP_CAP) + ' and ' +
        STR_POSINFO_FLD_ID + '=''''', true); // neu = keine TubeID eingetragen
    try
        if (not self.DataProvider.EOF) then
        begin
            if (aLastOut) then
                self.DataProvider.Last; // Letzte Position suchen
            result := self.DataProvider.FieldByName(STR_POSINFO_FLD_POS).AsInteger;
        end;
    finally
        Close();
    end;
end;

function TPosinfoDataAdaptor.FindFirstCapFreePos(aRackID: string; aNoOfPositions: integer): integer;
var
    xExpectedPos, xCurrentPos: integer;
begin
    result := 0;

    // select all tube barcodes of this rack
    SelectAndOpen('select * from ' + STR_TABLE_POSINFO + ' where ' + STR_POSINFO_FLD_RACKID + '=''' + aRackID
        + '''' + ' and ' + STR_POSINFO_FLD_STEP + '=' + IntToStr(INT_POSINFO_STEP_CAP) + ' order by ' +
        STR_SQL_WHERE_POS, true);
    try
        // Suchen nach der ersten Position die nicht belegt ist
        xExpectedPos := 1;
        while (not self.DataProvider.EOF) do
        begin
            xCurrentPos := self.DataProvider.FieldByName(STR_POSINFO_FLD_POS).AsInteger;
            if (xCurrentPos > xExpectedPos) then
                break
            else
                xExpectedPos := xCurrentPos + 1;
            self.DataProvider.Next;
        end;

        if (xExpectedPos <= aNoOfPositions) then
            result := xExpectedPos;
    finally
        self.DataProvider.Close;
    end;
end;

function TPosinfoDataAdaptor.InternGetTareWeightAndUnit(out oTareWeight: double; out oUnit: integer): boolean;
// aStepOffset must be positive
begin
    result := not self.DataProvider.IsEmpty;
    if not result then
        EXIT;
    oTareWeight := ReadAmount();
    oUnit := ReadUnit();
end;

function TPosinfoDataAdaptor.GetLastTareWeightAndUnit(const aRackID: string; aPos: integer;
    out oTareWeight: double; out oUnit: integer): boolean;
// Get the last Tare weight.
// Example if Tare weights with step = -1000, -1001, -1002 exist for aRackID and aPos then return the
// TareWeight for step -1002
var
    i: integer;
begin

    i := 0;
    while true do
    begin
        SelectAndOpenTareWeightByRackIDAndPos(aRackID, aPos, i, true);
        try
            result := InternGetTareWeightAndUnit(oTareWeight, oUnit);
            if not result then
                EXIT;
            Inc(i);
        finally
            Close();
        end;
    end;

end;

function TPosinfoDataAdaptor.GetLastTareWeightAndUnitBySubst(const aRackID: string; aPos: integer;
    const cSubst: string; out oTareWeight: double; out oUnit: integer): boolean;
// Get the last Tare weight, taking account of the substance
var
    xStep: integer;
begin
    result := False;
    oTareWeight := 0;
    oUnit := 0;
    SelectAndOpenByRackIDAndPosAndSubstID(aRackID, aPos, cSubst, true);
    try
        if self.DataProvider.IsEmpty then
            exit;
        self.DataProvider.First;
        xStep := ReadStep;
        if xStep > INT_POSINFO_STEP_TARE_WEIGHT then
            exit;
        oTareWeight := ReadAmount;
        oUnit := ReadUnit;
        result := true;
    finally
        Close();
    end;
end;

function TPosinfoDataAdaptor.GetAnyWeightAndUnit(const aRackID: string; aPos: integer; aStep: integer;
    out oTareWeight: double; out oUnit: integer): boolean;
var
    xStepOffset: integer;
begin
    xStepOffset := INT_POSINFO_STEP_TARE_WEIGHT - aStep;
    SelectAndOpenTareWeightByRackIDAndPos(aRackID, aPos, xStepOffset, true);
    try
        result := InternGetTareWeightAndUnit(oTareWeight, oUnit);
    finally
        Close();
    end;
end;

function TPosinfoDataAdaptor.GetTareWeightAndUnit(const aRackID: string; aPos: integer;
    out oTareWeight: double; out oUnit: integer): boolean;
// Get the Tare weight @Step = -1000
begin
    SelectAndOpenTareWeightByRackIDAndPos(aRackID, aPos, 0, true);
    try
        result := InternGetTareWeightAndUnit(oTareWeight, oUnit);
    finally
        Close();
    end;
end;

function TPosinfoDataAdaptor.GetTareWeightFromPos(aTubePos: TRackIDPosition; out oTareWeight: double)
    : boolean;
var
    xDummy: integer;
begin
    result := GetTareWeightAndUnit(aTubePos.RackID, aTubePos.Pos, oTareWeight, xDummy);
end;

function TPosinfoDataAdaptor.InternGetBalanceTareVal(out oTareWeight: double; out oUnit: integer): boolean;
begin
    result := not self.DataProvider.IsEmpty; // FindBalanceTare( aBalName, aBalPos );
    if not result then
        EXIT;
    oTareWeight := ReadAmount();
    oUnit := ReadUnit();

end;

function TPosinfoDataAdaptor.GetBalanceTareValByPos(const aBalName: string; aBalPos: integer;
    out oTareWeight: double; out oUnit: integer): boolean;
begin
    SelectAndOpenByBalanceAndPos(aBalName, aBalPos, true);
    try
        result := InternGetBalanceTareVal(oTareWeight, oUnit);
    finally
        Close();
    end;
end;

function TPosinfoDataAdaptor.GetBalanceTareVal(const aBalName: string; out oTareVal: double): boolean;
var
    xDummy: integer;
begin

    SelectAndOpenByBalanceAndPos(aBalName, INT_BALANCE_FIRST_POS, true);
    try
        result := InternGetBalanceTareVal(oTareVal, xDummy);
        if not result then
            EXIT;
        gLogManager.LogF('Tare weight: %.6f - Unit %d ', [oTareVal, INT_POSINFO_WEIGHT_UNIT], true);
    finally
        Close();
    end;

end;

// --------------------------------------------------------------------------------------------------
function TPosinfoDataAdaptor.EditOrAppendRec(const aKeyRackID: string; aKeyPos, aKeyStep: integer;
    const aSubstID: string; const aOrigin: string; aAmount: double; aUnit: integer): boolean;
// --------------------------------------------------------------------------------------------------
// if entry for RackId, Pos and Step already exists then edit it
// otherwise append a new entry
var
    xAppend: boolean;
begin
    result := false;
    if trim(aKeyRackID) = '' then
        EXIT;

    SelectAndOpenByRackIDAndPosAndStep(aKeyRackID, aKeyPos, aKeyStep, false);
    try
        xAppend := self.DataProvider.IsEmpty;

        WritePosinfoDataToDataset(self.DataProvider, xAppend, true, aKeyRackID, aKeyPos, aKeyStep, aSubstID,
            aOrigin, Now(), aAmount, aUnit);

        result := true;
    finally
        Close();
    end;

end;

procedure TPosinfoDataAdaptor.WriteTareWeight(aBalName: string; aWeight: double);
begin
    gLogManager.LogF('Rack %s - Weight: %.6f - Unit %d - (%d)', [aBalName, aWeight, INT_POSINFO_WEIGHT_UNIT,
        INT_POSINFO_STEP_WEIGHT], true);

    EditOrAppendRec(aBalName, INT_BALANCE_FIRST_POS, INT_POSINFO_STEP_WEIGHT, STR_SUBSTID_NONE,
        STR_ORIGIN_NONE, aWeight, INT_POSINFO_WEIGHT_UNIT);
end;

function TPosinfoDataAdaptor.WriteSubstWeight(aBalName: string; aWeight: double; aRackID: string;
    aPos: Integer; aSubstID: string; out oStepNo: integer): boolean;
// return false if no Tare Weight was found for aBalName

var
    xTareWeight: double;
begin
    result := true;
    oStepNo := -1000;
    if (aRackID = '_DoNotStore') then
    begin
        gLogManager.Log('Do not store Substance Weight!', false);
        exit;
    end;
    gLogManager.Log('Store Substance Weight!', false);

    // substract Tare from current weight to get substance weight
    if not GetBalanceTareVal(aBalName, xTareWeight) then
    begin
        result := false;
        EXIT;
    end;
    aWeight := aWeight - xTareWeight;

    self.SelectAndOpenByRackIDAndPosAndSubstID(aRackID, aPos, aSubstID, false);
    try
        if (not self.DataProvider.EOF) and (self.DataProvider.FieldByName('STEP').AsInteger < -999) then
            oStepNo := self.DataProvider.FieldByName('STEP').AsInteger - 1;

        // self.DataProvider.AppendRecord([aRackID,aPos, oStepNo,aSubstID,now,'['+aBalName+']',aWeight,INT_POSINFO_WEIGHT_UNIT]);
        WritePosinfoDataToDataset(self.DataProvider, true, true, aRackID, aPos, oStepNo, aSubstID,
            '[' + aBalName + ']', Now, aWeight, INT_POSINFO_WEIGHT_UNIT);
    finally
        self.DataProvider.Close;
    end;

    gLogManager.LogF('Rack %s Position %d - Substance ID %s - Weight: %.6f - Unit %d - (%d)',
        [aRackID, aPos, aSubstID, aWeight, INT_POSINFO_WEIGHT_UNIT, oStepNo], true);

end;

class function TPosinfoDataAdaptor.ReadNextStep(aDataset: TDataProvider; const aRackID: string;
    aPos: integer): integer;
begin
    result := 0;
    if (aRackID = '') then
        exit;

    aDataset.Last;
    result := ReadStepFromDataset(aDataset) + 1;

    if (result <= 0) then
        result := 1;
end;

class procedure TPosinfoDataAdaptor.ReadLastSubstID(aDataset: TDataProvider; var vSubstID: string;
    const aRackID: string; aPos: integer);
var
    xStep: Integer;
begin
    if (aRackID = '') then
        exit;
    if (aPos = 0) then
        exit;

    aDataset.Last;

    xStep := ReadStepFromDataset(aDataset);
    if xStep = NULL then
        exit;
    if xStep < 0 then
        exit;

    vSubstID := ReadSubstIDFromDataset(aDataset);
end;

procedure TPosinfoDataAdaptor.GetLastSubstID(var vSubstID: string; aRackID: string; aPos: integer);
begin
    SelectAndOpenByRackIDAndPos(aRackID, aPos, true);
    try
        if self.DataProvider.RecordCount > 0 then
            ReadLastSubstID(self.DataProvider, vSubstID, aRackID, aPos);
    finally
        Close();
    end;
end;

procedure TPosinfoDataAdaptor.AppendRec(aRackID: string; aPos, aStep: integer; aSubstID, aOrigin: string;
    aAmount: double; aUnitID: integer);
begin
    SelectAndOpenAll(false);
    try
        WritePosinfoDataToDataset(self.DataProvider, true, true, aRackID, aPos, aStep, aSubstID, aOrigin, Now,
            aAmount, aUnitID);
    finally
        Close();
    end;
end;

function TPosinfoDataAdaptor.GetPosFromRackIDAndSubstID(const aRackID, aSubstID: string;
    out oPos: integer): boolean;
begin
    oPos := -1;
    SelectByRackIDAndSubstID(aRackID, aSubstID, true);
    try
        result := not self.DataProvider.IsEmpty;
        if not result then
            EXIT;
        oPos := ReadPos();
    finally
        Close();
    end;
end;

function TPosinfoDataAdaptor.GetPosFromRackID(const aRackID: string; out oPos: integer): boolean;
begin
    oPos := -1;
    SelectAndOpenByRackID(aRackID, true);
    try
        result := not self.DataProvider.IsEmpty;
        if not result then
            EXIT;
        oPos := ReadPos();
    finally
        Close();
    end;
end;

function TPosinfoDataAdaptor.GetTubeOrigin(aCurrentPos: TRackIDPosition; aRunName: string): TRackIDPosition;
var
    xOriginStr: string;
    x: integer;
begin
    result.Rack := '';
    result.RackID := '';
    result.Pos := 0;

    SelectAndOpenByRackIDAndPosAndStep(aCurrentPos.RackID, aCurrentPos.Pos, INT_POSINFO_STEP_ORIGIN, true);
    try
        if self.DataProvider.IsEmpty then
            EXIT;
        xOriginStr := ReadOrigin();
        x := 1;
        result.Rack := gmReadSubString(x, xOriginStr, ';');
        result.RackID := gmReadSubString(x, xOriginStr, ';');
        result.Pos := StrToInt(gmReadSubString(x, xOriginStr, ';'));
    finally
        Close();
    end;
end;

procedure TPosinfoDataAdaptor.PrepareTransfer(aRackID: string;
    aFirstSource, aLastSource, aStartVolume: integer);
var
    i, xStep: integer;
    xVolume: double;
begin
    if aStartVolume = 0 then
        EXIT; // ---- Posinfo mittels Correcturfactor auf das StartVolume bringen--- !
    for i := aFirstSource to aLastSource do
    begin
        xVolume := 0;
        self.SelectAndOpenByRackIDAndPos(aRackID, i, true);
        try
            xStep := 1;
            while not self.DataProvider.EOF do
            begin
                xVolume := xVolume + ReadAmount();
                xStep := ReadStep();
                self.DataProvider.Next;
            end;
        finally
            Close();
        end;

        SelectAndOpenAll(false);
        try
            self.DataProvider.Append;
            WriteRackID(aRackID);
            WritePos(i);
            WriteStep(xStep + 1);
            WriteSubstID('TransferCorection');
            WriteAmount(-(xVolume - aStartVolume));
            self.DataProvider.Post;
        finally
            Close();
        end;
    end;
end;

class procedure TPosinfoDataAdaptor.WritePosinfoDataToDataset(aDataset: TDataProvider; aAppend: boolean;
    aWriteAmount: boolean; const aRackID: string; aPos, aStep: integer; const aSubstID, aOrigin: string;
    aDateAndTime: TDateTime; aAmount: double; aUnitID: integer);
begin
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    WriteRackIDToDataset(aDataset, aRackID);
    WritePosToDataset(aDataset, aPos);
    WriteStepToDataset(aDataset, aStep);
    WriteSubstIDToDataset(aDataset, aSubstID);
    WriteDateToDataset(aDataset, aDateAndTime);
    WriteOriginToDataset(aDataset, aOrigin);
    WriteAmountToDataset(aDataset, aAmount);
    WriteUnitToDataset(aDataset, aUnitID);

    aDataset.Post;
end;

class procedure TPosinfoDataAdaptor.WritePosinfoRecToDataset(aDataset: TDataProvider;
    aPosInfoRec: TPosInfoRec; aAppend: boolean);
begin
    WritePosinfoDataToDataset(aDataset, aAppend, true, aPosInfoRec.RackID, aPosInfoRec.Pos, aPosInfoRec.Step,
        aPosInfoRec.SubstID, aPosInfoRec.Origin, aPosInfoRec.DateAndTime, aPosInfoRec.Amount,
        aPosInfoRec.UnitID)
end;

class function TPosinfoDataAdaptor.ReadPosinfoRecFromDataset(aDataset: TDataProvider): TPosInfoRec;
begin
    result.Valid := false;
    if aDataset.Eof then
        Exit;

    result := MakePosInfoRec(true, ReadRackIDFromDataset(aDataset), ReadPosFromDataset(aDataset),
        ReadStepFromDataset(aDataset), ReadSubstIDFromDataset(aDataset), ReadDateFromDataset(aDataset),
        ReadOriginFromDataset(aDataset), ReadAmountFromDataset(aDataset), ReadUnitFromDataset(aDataset));
end;

class function TPosinfoDataAdaptor.ReadPosinfoRecsFromDataset(aDataset: TDataProvider): TPosinfoRecs;
var
    i: integer;
begin
    if aDataset.IsEmpty then
        EXIT;
    SetLength(result, aDataset.RecordCount);
    i := 0;
    while not aDataset.Eof do
    begin
        result[i] := ReadPosinfoRecFromDataset(aDataset);
        Inc(i);
        aDataset.Next;
    end;
end;

class function TPosinfoDataAdaptor.MakePosInfoRec(aValid: boolean; aRackID: string; aPos, aStep: integer;
    aSubstID: string; aDateAndTime: TDateTime; aOrigin: string; aAmount: double; aUnitID: integer)
    : TPosinfoRec;
begin
    result.Valid := aValid;
    result.RackID := aRackID;
    result.Pos := aPos;
    result.Step := aStep;
    result.SubstID := aSubstID;
    result.DateAndTime := aDateAndTime;
    result.Origin := aOrigin;
    result.Amount := aAmount;
    result.UnitID := aUnitID;
end;

class function TPosinfoDataAdaptor.MakeSQLStepIsIn(aTubeIDTypes: TTubeIDTypes): string;
var
    xList: TList<string>;
begin
    result := '';
    xList := TList<string>.Create();
    try
        if tiTubeID in aTubeIDTypes then
            xList.Add(IntToStr(INT_POSINFO_STEP_TUBEID));
        if tiCapID in aTubeIDTypes then
            xList.Add(IntToStr(INT_POSINFO_STEP_CAP));

        result := TStringUtilities.ListToString(xList, ',');
        if result <> '' then
            result := Format(' IN (%s) ', [result]);
    finally
        FreeAndNil(xList);
    end;

end;

class function TPosinfoDataAdaptor.ReadRackIDFromDataset(aDataset: TDataProvider): string;
begin
    result := aDataset.FieldByName(STR_POSINFO_FLD_RACKID).AsString;
end;

class function TPosinfoDataAdaptor.ReadAmountFromDataset(aDataset: TDataProvider): double;
begin
    result := aDataset.FieldByName(STR_POSINFO_FLD_AMOUNT).AsFloat;
end;

class function TPosinfoDataAdaptor.ReadPosFromDataset(aDataset: TDataProvider): integer;
begin
    result := aDataset.FieldByName(STR_POSINFO_FLD_POS).AsInteger;
end;

class function TPosinfoDataAdaptor.ReadUnitFromDataset(aDataset: TDataProvider): integer;
begin
    result := aDataset.FieldByName(STR_POSINFO_FLD_UNIT).AsInteger;
end;

class function TPosinfoDataAdaptor.ReadSubstIDFromDataset(aDataset: TDataProvider): string;
begin
    result := aDataset.FieldByName(STR_POSINFO_FLD_ID).AsString;
end;

class function TPosinfoDataAdaptor.ReadStepFromDataset(aDataset: TDataProvider): integer;
begin
    result := aDataset.FieldByName(STR_POSINFO_FLD_STEP).AsInteger;
end;

class function TPosinfoDataAdaptor.ReadOriginFromDataset(aDataset: TDataProvider): string;
begin
    result := aDataset.FieldByName(STR_POSINFO_FLD_ORIGIN).AsString;
end;

class function TPosinfoDataAdaptor.ReadDateFromDataset(aDataset: TDataProvider): TDateTime;
begin
    result := aDataset.FieldByName(STR_POSINFO_FLD_DATETIME).AsDateTime;
end;

class procedure TPosinfoDataAdaptor.WriteRackIDToDataset(aDataset: TDataProvider; const aRackID: string);
begin
    aDataset.FieldByName(STR_POSINFO_FLD_RACKID).AsString := aRackID;
end;

class procedure TPosinfoDataAdaptor.WritePosToDataset(aDataset: TDataProvider; aPos: integer);
begin
    aDataset.FieldByName(STR_POSINFO_FLD_POS).AsInteger := aPos;
end;

class procedure TPosinfoDataAdaptor.WriteStepToDataset(aDataset: TDataProvider; aStep: integer);
begin
    aDataset.FieldByName(STR_POSINFO_FLD_STEP).AsInteger := aStep;
end;

procedure TPosinfoDataAdaptor.DeleteStorageData(const aRackID: string; aPos: integer);
begin
    // SubstanceID, Minimum und Maximum löschen
    self.DataProvider.ExecSQL('delete from ' + STR_TABLE_POSINFO + ' where ' + STR_POSINFO_FLD_RACKID + '='''
        + aRackID + ''' and ' + STR_SQL_WHERE_POS + '=' + IntToStr(aPos) + ' and (step = ' +
        IntToStr(INT_POSINFO_STEP_MIN_SUBSTID) + ' or step = ' + IntToStr(INT_POSINFO_STEP_MAXVOLUME) +
        ' or step = ' + IntToStr(INT_POSINFO_STEP_MINVOLUME1) + ' or step = ' +
        IntToStr(INT_POSINFO_STEP_MINVOLUME2) + ')');
end;

procedure TPosinfoDataAdaptor.WriteStorageData(const aRackID: string; aPos: integer; const aSubstID: string;
    aMinVolume1, aMinVolume2, aMaxVolume: double; const aSubstanceSetName: string);
begin
    SelectAndOpenAll(false);
    try
        // Vorratsgefäß: SubstanceID, Minimum und Maximum eintragen
        WritePosinfoDataToDataset(self.DataProvider, true, true, aRackID, aPos, INT_POSINFO_STEP_MIN_SUBSTID,
            aSubstID, aSubstanceSetName, Now(), 0, 0);
        if (aMinVolume1 > 0) then
            WritePosinfoDataToDataset(self.DataProvider, true, true, aRackID, aPos,
                INT_POSINFO_STEP_MINVOLUME1, '', aSubstanceSetName, Now(), aMinVolume1, 0);
        if (aMinVolume2 > 0) then
            WritePosinfoDataToDataset(self.DataProvider, true, true, aRackID, aPos,
                INT_POSINFO_STEP_MINVOLUME2, '', aSubstanceSetName, Now(), aMinVolume2, 0);
        if (aMaxVolume > 0) then
            WritePosinfoDataToDataset(self.DataProvider, true, true, aRackID, aPos,
                INT_POSINFO_STEP_MAXVOLUME, '', aSubstanceSetName, Now(), aMaxVolume, 0);
    finally
        Close();
    end;
end;

class procedure TPosinfoDataAdaptor.WriteSubstIDToDataset(aDataset: TDataProvider; const aSubstID: string);
begin
    aDataset.FieldByName(STR_POSINFO_FLD_ID).AsString := aSubstID;
end;

class procedure TPosinfoDataAdaptor.WriteAmountToDataset(aDataset: TDataProvider; aAmount: double);
begin
    aDataset.FieldByName(STR_POSINFO_FLD_AMOUNT).AsFloat := aAmount;
end;

class procedure TPosinfoDataAdaptor.WriteOriginToDataset(aDataset: TDataProvider; const aOrigin: string);
begin
    aDataset.FieldByName(STR_POSINFO_FLD_ORIGIN).AsString := aOrigin;
end;

class procedure TPosinfoDataAdaptor.WriteUnitToDataset(aDataset: TDataProvider; aUnit: integer);
begin
    aDataset.FieldByName(STR_POSINFO_FLD_UNIT).AsInteger := aUnit;
end;

class procedure TPosinfoDataAdaptor.WriteDateToDataset(aDataset: TDataProvider; aDate: TDateTime);
begin
    aDataset.FieldByName(STR_POSINFO_FLD_DATETIME).AsDateTime := aDate;
end;

class procedure TPosinfoDataAdaptor.WriteWeight(const aRackID, aSubstID, aOrigin: string;
    aPos, aStep, aUnit: integer; aWeight: double);
var
    xDataAdaptor: TPosinfoDataAdaptor;
begin
    xDataAdaptor := TPosinfoDataAdaptor.Create();
    xDataAdaptor.EditOrAppendRec(aRackID, aPos, aStep, aSubstID, aOrigin, aWeight, aUnit);
    FreeAndNil(xDataAdaptor);
end;

class function TPosinfoDataAdaptor.GetTubeID(aTubePos: TRackIDPosition): string;
var
    xDataAdaptor: TPosinfoDataAdaptor;
begin
    xDataAdaptor := TPosinfoDataAdaptor.Create();
    result := xDataAdaptor.ReadTubeID(aTubePos);
    FreeAndNil(xDataAdaptor);
end;

class function TPosinfoDataAdaptor.GetTubeFirstWeight(aTubePos: TRackIDPosition; var vUnit: integer): double;
var
    xDataAdaptor: TPosinfoDataAdaptor;
begin
    xDataAdaptor := TPosinfoDataAdaptor.Create();
    xDataAdaptor.GetTareWeightAndUnit(aTubePos.RackID, aTubePos.Pos, result, vUnit);
    FreeAndNil(xDataAdaptor);
end;

class procedure TPosinfoDataAdaptor.InstChangeRackID(const aOldID, aNewID: string);
var
    xDataAdaptor: TPosinfoDataAdaptor;
begin
    xDataAdaptor := TPosinfoDataAdaptor.Create();
    xDataAdaptor.ChangeRackID(aOldID, aNewID);
    FreeAndNil(xDataAdaptor);
end;


end.
