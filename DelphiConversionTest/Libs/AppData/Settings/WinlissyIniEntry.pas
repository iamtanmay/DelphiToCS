{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : TMotorEntry, TBswitchEntry, ...: Application-specific composed ini entry classes
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  10.10.02 wl                               TN1293.2 initial version
  11.10.02 wl                               TN1293.2 neu: TTipTypeEntry
  15.10.02 wl                               TN1293.2 neu: TTipTypeIniSection
  16.10.02 wl                               TN1293.2 neu: TDilutorIniSection
  18.10.02 wl                               TN1293.1 neu: TBcscanrangeEntry, TBcpositionEntry
  18.10.02 wl                               TN1293.1 neu: TDLLMenuItemEntry
  22.10.02 wl                               TN1293.1 neu: TRectEntry, TRectIniSection
  22.10.02 wl                               TN1293.1 TStringIniSection --> IniEntryList
  23.10.02 wl                               TN1293.1 neu: TSlaveDataEntry
  25.10.02 wl                               TN1293.1 neu: TPRTipDataEntry
  21.11.02 wl                               TN1293.4 neu: TTubeToolDataEntry
  20.12.02 wl                               TN1293.5 neu: Zugriff auf TResLoader
  15.01.03 wl                               TN1295.5 uses geändert
  17.01.03 wl                               TN1293.5 Kommmentare zu jedem Sub-Entry
  12.02.03 wl  TRectEntry                   TN1293.5 --> LocalIniFile  (TRectIniSection entfernt)
  07.03.03 tbh TTubeToolDataEntry.GetTubeToolDataValue  TN1443 Schüttelparameter ergänzt
  11.03.03 wl                               TN1293.4 neu: TLiquidPortDataEntry, TLiquidPortDataSection
  13.03.03 hn                               TN1293.3 Einheiten bearbeitet
  09.05.03 wl  TTipTypeEntry                TN1490   alle Funktionen um DitiType erweitert
  09.07.03 wl  TTubeToolDataEntry.CreateWithoutDefault  TN1443 Schüttelparameter auch hier ergänzt !!!
  09.07.03 wl  TTubeToolDataEntry.Create    TN1501   Default-Werte geändert
  25.07.03 tbh TTubeToolDataEntry           TN1448   ToolBringBackVOffset ergänzt
  25.07.03 tbh TTubeToolDataEntry.Create    TN1448   Default-Werte geändert
  12.11.03 wl  TBcrpositionEntry            TN1660   alle Funktionen um RPos erweitert
  10.12.03 wl  TAPOSEntry                   TN1672   Y wird als MPOSArray gespeichert - Varispan fällt weg
  17.12.03 wl  TXYRangeDataEntry            TN1672   neu: TXYRangeDataEntry
  24.06.04 wl                               TN2007   FileCtrl-Aufrufe durch Utility-Aufrufe ersetzt
  26.07.04 pk  GetBcrpositionValue          TN2053   Read PosName value.  Raise exception if Posname exists and X,Y,Z, or RSteps arent 0
  05.08.04 wl  TAPOSEntry                   TN2074   entfernt
  18.08.04 wl  TIncubatorrecEntry           TN2099   entfernt
  23.11.04 wl  TMotorEntry                  TN2239.1 neu: InitSpeed
  23.11.04 wl  TDilutorEntry                TN2239.2 neu: ScalingFactor
  28.01.05 wl  TTipTypeEntry                TN2297.4 neue Parameter: WashZOffset_mm, WasteZOffset_mm, DryZOffset_mm, DryAtInit
  31.01.05 wl  TTipTypeEntry                TN2297.4  DryAtInit muß wegen systematischen Fehlers beim Einlesen in DoNotDryAtInit umbenannt werden
  22.02.05 wl  TTipTypeEntry                TN2323    DoNotDryAtInit wird umbenannt in DoNotDryAfterFlush
  23.03.05 pk  TBarcoderecEntry.Create      TN2360   Pass InitStr resource as argument
  04.10.05 wl  TTipTypeEntry                TN2550    DilutorName statt DilutorAdr
  30.11.05 wl  TPRTipIniEntry               TN2818    entfernt
  04.03.06 wl  TDBFieldDefinitionEntry      TN2541.4 für individuelle Felddefinitionen
  23.09.06 wl  TTipTypeEntry                TN3326    entfernt
  23.09.06 wl  TTipTypeEntrySection         TN3326    entfernt
  31.08.07 wl                               TN3811.4  an Änderungen in IniAccess angepasst
  09.11.07 pk  TTubeToolDataEntry           TN3922    Steps changed to mm
  09.01.08 wl  TSlaveDataEntry              TN3972    entfernt
  13.02.08 wl                               TN4009   alle Module, die früher über Settings definiert wurden, entfernt
  06.08.09 wl                               TN4702   Strings werden direkt geladen
  13.08.09 wl  TTubeToolDataEntry           TN4723   Typen korrigiert
  13.04.10 wl                               TN5044   uses geändert
  26.01.11 wl  TXYRangeDataEntry            TN5448   entfernt
  03.02.12 wl  TDBFieldDefinitionEntry      TN5792   'ReagentList'-Einträge entfernt
  27.01.13 wl  TBackupPathDataSection       TN6069   neu
  06.06.13 wl  TTubeToolDataEntry           TN6154   Neu: GetTube-Parameter
  -------------------------------------------------------------------------------------------------- }

unit WinlissyIniEntry;


interface


uses
    CommonTypes,
    CompositeIniEntry,
    IniEntry,
    Windows,
    IniSection;

type
    TBcscanrangeEntry = class(TCompositeIniEntry)
    private
        function GetBcscanrangeValue: BCSCANRANGE;
    public
        // Constructor/Destructor
        constructor Create(aSection, aIdent: string; aXRange, aXSteps, aYRange, aYSteps, aZRange,
            aZSteps: integer; const aDescription: string);
        constructor CreateWithoutDefault(aSection, aIdent: string);
        // Properties
        property BcscanrangeValue: BCSCANRANGE read GetBcscanrangeValue;
    end;

    TBcrpositionEntry = class(TCompositeIniEntry)
    private
        function GetBcrpositionValue: BCRPOSITION;
    public
        // Constructor/Destructor
        constructor Create(aSection, aIdent, aDescription: string);
        constructor CreateWithoutDefault(aSection, aIdent: string);
        // Properties
        property BcrpositionValue: BCRPOSITION read GetBcrpositionValue;
    end;

    TDLLMenuItemEntry = class(TCompositeIniEntry)
    private
        function GetDLLMenuItemValue: TDLLMenuItem;
    public
        // Constructor/Destructor
        constructor Create(aSection, aIdent, aDescription: string);
        constructor CreateWithoutDefault(aSection, aIdent: string);
        // Properties
        property DLLMenuItemValue: TDLLMenuItem read GetDLLMenuItemValue;
    end;

    TTubeToolDataEntry = class(TCompositeIniEntry)
    private
        function GetTubeToolDataValue: TTubeToolData;
    public
        // Constructor/Destructor
        constructor Create(aSection, aIdent, aDescription: string);
        constructor CreateWithoutDefault(aSection, aIdent: string);
        // Properties
        property TubeToolDataValue: TTubeToolData read GetTubeToolDataValue;
    end;

    TLiquidPortDataEntry = class(TCompositeIniEntry)
    private
        function GetLiquidPortDataValue: TLiquidPortData;
    public
        // Constructor/Destructor
        constructor Create(aSection, aIdent, aDescription: string);
        constructor CreateWithoutDefault(aSection, aIdent: string);
        // Properties
        property LiquidPortDataValue: TLiquidPortData read GetLiquidPortDataValue;
    end;

    TDBFieldDefinitionEntry = class(TCompositeIniEntry)
    private
        function GetDBFieldDefinitionValue: TDBFieldDefinition;
    public
        // Constructor/Destructor
        constructor Create(aSection, aIdent, aDescription: string);
        constructor CreateWithoutDefault(aSection, aIdent: string);
        // Properties
        property DBFieldDefinitionValue: TDBFieldDefinition read GetDBFieldDefinitionValue;
    end;

    TBackupPathDataEntry = class(TCompositeIniEntry)
    private
        function GetBackupPathDataValue: TBackupPathData;
    public
        // Constructor/Destructor
        constructor Create(aSection, aIdent, aDescription: string);
        constructor CreateWithoutDefault(aSection, aIdent: string);
        // Properties
        property BackupPathDataValue: TBackupPathData read GetBackupPathDataValue;
    end;

    TDLLMenuItemSection = class(TTypeIniSection)
    public
        function CreatePrototypeEntry(aIdent: string): TIniEntry; override;
    end;

    TTubeToolDataSection = class(TTypeIniSection)
    public
        function CreatePrototypeEntry(aIdent: string): TIniEntry; override;
    end;

    TBcrpositionSection = class(TTypeIniSection)
    public
        function CreatePrototypeEntry(aIdent: string): TIniEntry; override;
    end;

    TLiquidPortDataSection = class(TTypeIniSection)
    public
        function CreatePrototypeEntry(aIdent: string): TIniEntry; override;
    end;

    TBackupPathDataSection = class(TTypeIniSection)
    public
        function CreatePrototypeEntry(aIdent: string): TIniEntry; override;
    end;


implementation


uses
    SysUtils,
    GeneralTypes;

// --------------------------------------------------------------------------------------------------
// TBcscanrangeEntry -> TCompositeIniEntry -> TIniEntry
// --------------------------------------------------------------------------------------------------
const
    STR_BCSCANRANGE_DELIMITER = ',';

function TBcscanrangeEntry.GetBcscanrangeValue: BCSCANRANGE;
begin
    result.XScanRange := (FSubEntries[0] as TIntegerEntry).IntegerValue;
    result.XScanSteps := (FSubEntries[1] as TIntegerEntry).IntegerValue;
    result.YScanRange := (FSubEntries[2] as TIntegerEntry).IntegerValue;
    result.YScanSteps := (FSubEntries[3] as TIntegerEntry).IntegerValue;
    result.ZScanRange := (FSubEntries[4] as TIntegerEntry).IntegerValue;
    result.ZScanSteps := (FSubEntries[5] as TIntegerEntry).IntegerValue;
end;

constructor TBcscanrangeEntry.Create(aSection, aIdent: string; aXRange, aXSteps, aYRange, aYSteps, aZRange,
    aZSteps: integer; const aDescription: string);
begin
    inherited Create(aSection, aIdent, STR_BCSCANRANGE_DELIMITER, aDescription);
    AddInteger(aXRange, 'X-Steps', TLanguageString.Read('Scan range X-Max', 'Scan range X-Max'));
    AddInteger(aXSteps, 'X-Steps', TLanguageString.Read('Scan range X-Shift', 'Scan range X-Shift'));
    AddInteger(aYRange, 'Y-Steps', TLanguageString.Read('Scan range Y-Max', 'Scan range Y-Max'));
    AddInteger(aYSteps, 'Y-Steps', TLanguageString.Read('Scan range Y-Shift', 'Scan range Y-Shift'));
    AddInteger(aZRange, 'Z-Steps', TLanguageString.Read('Scan range Z-Max', 'Scan range Z-Max'));
    AddInteger(aZSteps, 'Z-Steps', TLanguageString.Read('Scan range Z-Shift', 'Scan range Z-Shift'));
end;

constructor TBcscanrangeEntry.CreateWithoutDefault(aSection, aIdent: string);
var
    x: integer;
begin
    inherited CreateWithoutDefault(aSection, aIdent, STR_BCSCANRANGE_DELIMITER);
    for x := 0 to 5 do
        AddIntegerWithoutDefault;
end;

// --------------------------------------------------------------------------------------------------
// TBcscanrangeEntry -> TCompositeIniEntry -> TIniEntry
// --------------------------------------------------------------------------------------------------
const
    STR_BCRPOSITION_DELIMITER = ',';

function TBcrpositionEntry.GetBcrpositionValue: BCRPOSITION;
begin
    result.XPos := (FSubEntries[0] as TIntegerEntry).IntegerValue;
    result.YPos := (FSubEntries[1] as TIntegerEntry).IntegerValue;
    result.ZPos := (FSubEntries[2] as TIntegerEntry).IntegerValue;
    result.RPos := (FSubEntries[3] as TIntegerEntry).IntegerValue;
    if FSubEntries.Count > 4 then
    begin
        result.PosName := (FSubEntries[4] as TStringEntry).Value;
        if (result.PosName <> '') and ((result.XPos <> 0) or (result.YPos <> 0) or (result.ZPos <> 0) or
            (result.RPos <> 0)) then
            raise EInvalidIniValue.Create(TLanguageString.
                Read('Entry [{0}] {1}: A position name can only be defined when the X,Y,Z, and R steps are set to 0',
                'Eintrag [{0}] {1}: Positionname kann nur dann denifiert werden wenn X-,Y-,Z-, und R-Steps auf 0 gesetzt sind',
                [GetSection, GetIdent]));
    end;
end;

constructor TBcrpositionEntry.Create(aSection, aIdent, aDescription: string);
begin
    inherited Create(aSection, aIdent, STR_BCRPOSITION_DELIMITER, aDescription);

    AddInteger(0, 'X-Steps', TLanguageString.Read('Barcode reader X-Position', 'X-Position Barcodeleser'));
    AddInteger(0, 'Y-Steps', TLanguageString.Read('Barcode reader Y-Position', 'Y-Position Barcodeleser'));
    AddInteger(0, 'Z-Steps', TLanguageString.Read('Barcode reader Z-Position', 'Z-Position Barcodeleser'));
    AddInteger(0, 'R-Steps', TLanguageString.Read('Barcode reader rotation position',
        'Rotations-Position Barcodeleser'));
    AddString('', 'Name', TLanguageString.Read('Name of taught position (5-Axis Arm)',
        'Name der vordefinierten Position (5-Achsen Arm)'));
end;

constructor TBcrpositionEntry.CreateWithoutDefault(aSection, aIdent: string);
var
    x: integer;
begin
    inherited CreateWithoutDefault(aSection, aIdent, STR_BCRPOSITION_DELIMITER);
    for x := 0 to 3 do
        AddIntegerWithoutDefault;
    AddStringWithoutDefault;
end;

// --------------------------------------------------------------------------------------------------
// TDLLMenuItemEntry -> TCompositeIniEntry -> TIniEntry
// --------------------------------------------------------------------------------------------------
const
    STR_DLLMENUITEMENTRY_DELIMITER = ',';

function TDLLMenuItemEntry.GetDLLMenuItemValue: TDLLMenuItem;
begin
    result.DLLName := (FSubEntries[0] as TStringEntry).Value;
    result.DLLFunction := (FSubEntries[1] as TStringEntry).Value;
    result.Parameter := (FSubEntries[2] as TStringEntry).Value;
    result.MenuCaption := (FSubEntries[3] as TStringEntry).Value;
end;

constructor TDLLMenuItemEntry.Create(aSection, aIdent, aDescription: string);
begin
    inherited Create(aSection, aIdent, STR_DLLMENUITEMENTRY_DELIMITER, aDescription);

    AddString('', '', TLanguageString.Read('DLL name (without extension)',
        'DLL-Name (ohne Dateinamenerweiterung)'));
    AddString('', '', TLanguageString.Read('DLL function', 'DLL-Funktion'));
    AddString('', '', TLanguageString.Read('DLL function parameter', 'DLL-Funktionsparameter'));
    AddString('', '', TLanguageString.Read('Text of control item', 'Text der Schaltfläche'));
end;

constructor TDLLMenuItemEntry.CreateWithoutDefault(aSection, aIdent: string);
var
    x: integer;
begin
    inherited CreateWithoutDefault(aSection, aIdent, STR_DLLMENUITEMENTRY_DELIMITER);
    for x := 0 to 3 do
        AddStringWithoutDefault;
end;

// --------------------------------------------------------------------------------------------------
// TTubeToolDataEntry -> TCompositeIniEntry -> TIniEntry
// --------------------------------------------------------------------------------------------------
const
    STR_TUBETOOLDATAENTRY_DELIMITER = ',';

function TTubeToolDataEntry.GetTubeToolDataValue: TTubeToolData;
begin
    result.XOffset := (FSubEntries[0] as TFloatEntry).FloatValue;
    result.YOffset := (FSubEntries[1] as TFloatEntry).FloatValue;
    result.ZOffset := (FSubEntries[2] as TFloatEntry).FloatValue;
    result.ZDrop := (FSubEntries[3] as TFloatEntry).FloatValue;
    result.VOpen := (FSubEntries[4] as TFloatEntry).FloatValue;
    result.VDrop := (FSubEntries[5] as TFloatEntry).FloatValue;
    result.TubeDY := (FSubEntries[6] as TFloatEntry).FloatValue;
    result.SafeMoveOffset := (FSubEntries[7] as TFloatEntry).FloatValue;
    result.SafeMoveSpeed := (FSubEntries[8] as TIntegerEntry).IntegerValue;
    result.ShakeHeight := (FSubEntries[9] as TFloatEntry).FloatValue;
    result.ShakeZOffset := (FSubEntries[10] as TFloatEntry).FloatValue;
    result.ShakeYOffset := (FSubEntries[11] as TFloatEntry).FloatValue;
    result.BringBackVOffset := (FSubEntries[12] as TFloatEntry).FloatValue;

    result.GetTubeXYShiftStepCount := (FSubEntries[13] as TIntegerEntry).IntegerValue;
    result.GetTubeXYShiftRadius1 := (FSubEntries[14] as TFloatEntry).FloatValue;
    result.GetTubeXYShiftRadius2 := (FSubEntries[15] as TFloatEntry).FloatValue;
end;

constructor TTubeToolDataEntry.Create(aSection, aIdent, aDescription: string);
begin
    inherited Create(aSection, aIdent, STR_TUBETOOLDATAENTRY_DELIMITER, aDescription);

    AddFloat(0, 'x-mm', TLanguageString.Read('Tube tool X-Offset', 'Tube tool X-Offset'));
    AddFloat(0, 'y-mm', TLanguageString.Read('Tube tool Y-Offset', 'Tube tool Y-Offset'));
    AddFloat(0, 'z-mm', TLanguageString.Read('Tube tool Z-Offset', 'Tube tool Z-Offset'));
    AddFloat(0, 'z-mm', TLanguageString.Read('Tube tool Z-drop', 'Tube tool Z-drop'));
    AddFloat(0, 'v-mm', TLanguageString.Read('Tube tool V-open', 'Tube tool V-open'));
    AddFloat(0, 'v-mm', TLanguageString.Read('Tube tool V-drop', 'Tube tool V-drop'));
    AddFloat(0, 'y-mm', TLanguageString.Read('Tube tool TubeDY', 'Tube tool TubeDY'));
    AddFloat(0, 'z-mm', TLanguageString.Read('Tube tool SafeMoveOffset', 'Tube tool SafeMoveOffset'));
    AddInteger(0, 'z-Steps/s', TLanguageString.Read('Tube tool SafeMoveSpeed', 'Tube tool SafeMoveSpeed'));
    AddFloat(0, 'z-mm', TLanguageString.Read('Tube tool ShakeHeight', 'Tube tool ShakeHeight'));
    AddFloat(0, 'z-mm', TLanguageString.Read('Tube tool ShakeZOffset', 'Tube tool ShakeZOffset'));
    AddFloat(0, 'y-mm', TLanguageString.Read('Tube tool ShakeYOffset', 'Tube tool ShakeYOffset'));
    AddFloat(0, 'y-mm', TLanguageString.Read('Tube tool BringBackVOffset', 'Tube tool BringBackVOffset'));

    AddInteger(0, '', TLanguageString.Read('Get Tube: XY-Shift, Number of tries',
        'Get Tube: XY-Shift, Number of tries'));
    AddFloat(0, 'mm', TLanguageString.Read('Get Tube: XY-Shift, Radius 1', 'Get Tube: XY-Shift, Radius 1'));
    AddFloat(0, 'mm', TLanguageString.Read('Get Tube: XY-Shift, Radius 2', 'Get Tube: XY-Shift, Radius 2'));
end;

constructor TTubeToolDataEntry.CreateWithoutDefault(aSection, aIdent: string);
var
    x: integer;
begin
    inherited CreateWithoutDefault(aSection, aIdent, STR_TUBETOOLDATAENTRY_DELIMITER);

    for x := 0 to 7 do
        AddFloatWithoutDefault;
    AddIntegerWithoutDefault;
    for x := 9 to 12 do
        AddFloatWithoutDefault;

    AddIntegerWithoutDefault;
    AddFloatWithoutDefault;
    AddFloatWithoutDefault;
end;

// --------------------------------------------------------------------------------------------------
// TLiquidPortDataEntry -> TCompositeIniEntry -> TIniEntry
// --------------------------------------------------------------------------------------------------
const
    STR_LIQUIDPORTDATAENTRY_DELIMITER = ',';

function TLiquidPortDataEntry.GetLiquidPortDataValue: TLiquidPortData;
begin
    result.DilName := (FSubEntries[0] as TStringEntry).Value;
    result.AspSpeed := (FSubEntries[1] as TIntegerEntry).IntegerValue;
end;

constructor TLiquidPortDataEntry.Create(aSection, aIdent, aDescription: string);
begin
    inherited Create(aSection, aIdent, STR_LIQUIDPORTDATAENTRY_DELIMITER, aDescription);
    AddString('', '', TLanguageString.Read('Diluent name', 'Name des Lösungsmittels'));
    AddInteger(400, '', TLanguageString.Read('Aspiration speed', 'Aspirationsgeschwindigkeit'));
end;

constructor TLiquidPortDataEntry.CreateWithoutDefault(aSection, aIdent: string);
begin
    inherited CreateWithoutDefault(aSection, aIdent, STR_LIQUIDPORTDATAENTRY_DELIMITER);
    AddStringWithoutDefault;
    AddIntegerWithoutDefault;
end;

{ TDBFieldDefinitionEntry }

const
    STR_DBFIELDDEFINITION_DELIMITER = ';';

constructor TDBFieldDefinitionEntry.Create(aSection, aIdent, aDescription: string);
begin
    inherited Create(aSection, aIdent, STR_DBFIELDDEFINITION_DELIMITER, aDescription);

    AddString('', '', TLanguageString.Read('Field name in database', 'Feldname in der Datenbank'));
    AddString('', '', TLanguageString.Read('Field name in user interface',
        'Feldname in der Software-Oberfläche'));
    AddString('', '', TLanguageString.Read('Definition of the pick list', 'Definition der Auswahlliste'));
end;

constructor TDBFieldDefinitionEntry.CreateWithoutDefault(aSection, aIdent: string);
var
    x: integer;
begin
    inherited CreateWithoutDefault(aSection, aIdent, STR_DBFIELDDEFINITION_DELIMITER);

    for x := 0 to 2 do
        AddStringWithoutDefault;
end;

function TDBFieldDefinitionEntry.GetDBFieldDefinitionValue: TDBFieldDefinition;
begin
    result.FieldName := (FSubEntries[0] as TStringEntry).Value;
    result.Caption := (FSubEntries[1] as TStringEntry).Value;
    result.PickList := (FSubEntries[2] as TStringEntry).Value;
end;

{ TBackupPathDataEntry }

const
    cBackupPathDataDelimiter = ',';

constructor TBackupPathDataEntry.Create(aSection, aIdent, aDescription: string);
begin
    inherited Create(aSection, aIdent, cBackupPathDataDelimiter, aDescription);

    AddBool(false, TLanguageString.Read('0=File, 1=Folder', '0=Datei, 1=Ordner'));
    AddString('', '', TLanguageString.Read('Archive name', 'Archivname'));
    AddString('', '', TLanguageString.Read('Path', 'Pfad'));
    AddBool(false, TLanguageString.Read('Copy subfolders?', 'Unterverzeichnisse mitkopieren?'));
    AddBool(false, TLanguageString.Read('Copy only files with current date?',
        'Nur Daten mit aktuellen Datum kopieren?'));
end;

constructor TBackupPathDataEntry.CreateWithoutDefault(aSection, aIdent: string);
begin
    inherited CreateWithoutDefault(aSection, aIdent, cBackupPathDataDelimiter);

    AddBoolWithoutDefault;
    AddStringWithoutDefault;
    AddStringWithoutDefault;
    AddBoolWithoutDefault;
    AddBoolWithoutDefault;
end;

function TBackupPathDataEntry.GetBackupPathDataValue: TBackupPathData;
begin
    result.IsFolder := (FSubEntries[0] as TBoolEntry).BoolValue;
    result.ArchiveName := (FSubEntries[1] as TStringEntry).Value;
    result.PathName := (FSubEntries[2] as TStringEntry).Value;
    result.Subfolders := (FSubEntries[3] as TBoolEntry).BoolValue;
    result.CheckDate := (FSubEntries[4] as TBoolEntry).BoolValue;
end;

{ TDLLMenuItemSection }

function TDLLMenuItemSection.CreatePrototypeEntry(aIdent: string): TIniEntry;
begin
    result := TDLLMenuItemEntry.Create(FSection, aIdent, FDescription);
end;

{ TTubeToolDataSection }

function TTubeToolDataSection.CreatePrototypeEntry(aIdent: string): TIniEntry;
begin
    result := TTubeToolDataEntry.Create(FSection, aIdent, FDescription);
end;

{ TBcrpositionSection }

function TBcrpositionSection.CreatePrototypeEntry(aIdent: string): TIniEntry;
begin
    result := TBcrpositionEntry.Create(FSection, aIdent, FDescription);
end;

{ TLiquidPortDataSection }

function TLiquidPortDataSection.CreatePrototypeEntry(aIdent: string): TIniEntry;
begin
    result := TLiquidPortDataEntry.Create(FSection, aIdent, FDescription);
end;

{ TBackupPathDataSection }

function TBackupPathDataSection.CreatePrototypeEntry(aIdent: string): TIniEntry;
begin
    result := TBackupPathDataEntry.Create(FSection, aIdent, FDescription);
end;


end.
