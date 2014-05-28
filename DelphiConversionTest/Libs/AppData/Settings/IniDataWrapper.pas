{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : TIniDataAdapter: Abstract class for ini data access
  TIniFileDataAdapter: owns a TIniFile for data access
  TIniDBDataAdapter: owns a TTable of "Settings.db" for data access
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.09.02 wl                               TN1293.1 initial version
  15.10.02 wl                               TN1293.2 neu: ReadSection, DeleteKey
  21.10.02 wl  TIniFileDataAdapter          TN1293.1 Open vor allen Aktionen
  25.10.02 wl                               TN1293.1 diverse Änderungen
  25.10.02 wl  TIniFileDataAdapter          TN1293.1 ermittelt ini-Filenamen jetzt selbst
  06.11.02 wl  TIniFileDataAdapter          TN1293.1 'ZP02App.ini' und 'ZP02Robot wieder entfernt
  06.11.02 wl                               TN1293.1 uses AppSettings
  06.11.02 wl  GetAccessType                TN1293.1 CLASS Methode, um IniAccessType zu bekommen
  06.11.02 wl  GetFileName, GetAreaName     TN1293.1 jetzt als CLASS Methoden
  20.12.02 wl                               TN1293.5 Zugriff auf AppSettings abgeschafft
  20.12.02 wl  Create                       TN1293.5 notwendige Settings werden übergeben
  20.12.02 wl                               TN1293.5 neue Properties Area & FileName
  20.12.02 wl  SettingsTableExists          TN1293.5 neu
  08.01.03 wl  Database constants           TN1293.5 von CommonTypes hierher
  08.01.03 wl  CreateInifileTable           TN1293.5 Feld Value muß keinen Wert haben
  09.01.03 wl  GetAnyFileName               TN1293.5 Platform appSIAS entfernt
  15.01.03 wl                               TN1295.5 Änderungen durch Wegfall von TIniAccessSection
  15.01.03 wl                               TN1295.5 Const-Werte STR_SETTINGS_AREA... -> CommonTypes
  15.01.03 wl  GetAnyFileName               TN1295.5 Zugriff auch auf beliebige Dateien
  16.01.03 wl  IniDBDataAdapter.ReadSections TN1295.5 Bug korrigiert (doppelte Einträge)
  16.01.03 wl  IniDBDataAdapter.ReadSection TN1295.5 Bug korrigiert (doppelte Einträge)
  23.01.03 wl  TIniFileDataAdapter.GetAreaNames TN1293.6 für Konvertierung implementiert
  05.02.03 wl  IsEmpty                          TN1293.3 jetz für beide Access-Typen
  05.02.03 wl  TIniFileDataAdapter.GetAreaNames TN1293.3 überarbeitet
  06.02.03 wl  WriteString                  TN1334.3 Änderungen werden geloggt (mit User & Reason)
  06.02.03 wl  DeleteKey                    TN1334.3 Änderungen werden geloggt (mit User & Reason)
  10.02.03 wl  GetFieldLength_...           TN1295.3 neu: alle Feldlängen können erfragt werden
  11.02.03 wl                               TN1334.3 INT_SETTINGS_FLDLEN_AREA     = 16
  11.02.03 wl  GetAreaFromAnyName           TN1334.3 neu: wird auch außerhalb des Adaptors benötigt
  03.06.03 wl  DefinePlatform               TN1485.1 definiert die Platform anhand der vorhandenen Daten (Ist ZP01CFG vorhanden?)
  08.12.05 wl  TIniDBDataAdapter            TN2841   The Settings.db table will be closed after any transaction
  28.06.06 wl                               TN3171    INT_SETTINGS_FLDLEN_AREA = 40 (statt 16)
  28.06.06 wl  TIniDBDataAdapter.Open       TN3171    Prüft, ob Feld "AREA" schon 40 Zeichen hat (evtl. Update)
  05.12.06 wl  TIniDBDataAdapter.StandardUpdate       Kein Compiler-Hint mehr
  03.09.07 wl                               TN3811.4  --> Alle Datenbankzugriffe --> SettingsDataAdaptor
  09.11.07 pk                               TN3922   uses QryTools2 removed
  03.06.08 wl                               TN4128   Field constants of Settings.db removed (already defined in SettingsFieldNames)
  11.06.08 wl                               TN4143   alles ZinsserPlatform-spezifische entfernt
  06.08.08 pk                               TN4165.1 various changes need for new SettingsDataCache
  23.09.08 wl                               TN4236   uses SettingsFieldnames entfernt
  07.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  26.10.09 wl  TIniFileDataWrapper          TN4831   IConfigurationSet replaces TIniFile
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  15.11.10 pk                               TN5340   Changes to prevent memory leak
  27.03.13 wl                               TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit IniDataWrapper;


interface


uses
    ConfigurationFile,
    SysUtils,
    GeneralTypes,
    CommonTypes,
    SettingsDataAdaptor,
    SettingsDataCache;

type
    EInvalidIniAccess = Exception;

    TIniDataWrapper = class // könnte auch als interface deklariert werden
    private
        fArea: string;
    public
        constructor Create(const aAnyName: string);

        // Public class methods
        class function GetFieldLength_Area: integer;
        class function GetFieldLength_Ident: integer;
        class function GetFieldLength_Section: integer;
        class function GetFieldLength_Value: integer;
        class function GetFileNameFromArea(aAreaName, aDataPath: string): string;
        class function CutAreaName(aAreaName: string): string;
        class function GetAreaFromAnyName(aAnyName: string): string;
        // Public Methods
        procedure ReadIntoCache(); virtual;
        procedure WriteFromCache(); virtual;
        procedure ReadSectionIntoCache(const aSection: string); virtual;
        procedure WriteSectionFromCache(const aSection: string); virtual;

        function ReadString(aSection, aIdent: string; var aValue: string): boolean; virtual; abstract;
        procedure WriteString(aCurrentUser: IUser; aReason, aSection, aIdent, aValue: string);
            virtual; abstract;
        function ReadSection(aSection: string): TStringArray; virtual; abstract;
        procedure DeleteKey(aCurrentUser: IUser; aReason, aSection, aIdent: string); virtual; abstract;
        function SectionExists(aSection: string): Boolean; virtual; abstract;
        function ReadSections(): TStringArray; virtual; abstract;
        function IsEmpty: boolean; virtual; abstract;
        // properties
        property Area: string read FArea;
    end;

    TIniFileDataWrapper = class(TIniDataWrapper)
    private
        FFileName: string;
        FIniFile: IConfigurationSet;
        procedure Open;
    public
        // Constructor/Destructor
        constructor Create(const aAnyName, aDataPath: string);
        destructor Destroy; override;
        // Public class methods
        class function GetAreaNames(const aDataPath: string): TStringArray;
        // Public Methods
        function ReadString(aSection, aIdent: string; var aValue: string): boolean; override;
        procedure WriteString(aCurrentUser: IUser; aReason, aSection, aIdent, aValue: string); override;
        function ReadSection(aSection: string): TStringArray; override;
        procedure DeleteKey(aCurrentUser: IUser; aReason, aSection, aIdent: string); override;
        function SectionExists(aSection: string): Boolean; override;
        function ReadSections(): TStringArray; override;
        function IsEmpty: boolean; override;
        // property
        property FileName: string read FFileName;
    end;

    TIniDBDataWrapper = class(TIniDataWrapper)
    private
        fDataAdaptor: TSettingsDataCacheArea;
    public
        // Constructor/Destructor
        constructor Create(aDataAdaptor: TSettingsDataCacheArea);
        destructor Destroy; override;
        // Cache Methods
        procedure ReadIntoCache(); override;
        procedure WriteFromCache(); override;
        procedure ReadSectionIntoCache(const aSection: string); override;
        procedure WriteSectionFromCache(const aSection: string); override;
        // Public Methods
        function ReadString(aSection, aIdent: string; var aValue: string): boolean; override;
        procedure WriteString(aCurrentUser: IUser; aReason, aSection, aIdent, aValue: string); override;
        function ReadSection(aSection: string): TStringArray; override;
        procedure DeleteKey(aCurrentUser: IUser; aReason, aSection, aIdent: string); override;
        function SectionExists(aSection: string): Boolean; override;
        function ReadSections(): TStringArray; override;
        function IsEmpty: boolean; override;
    end;


implementation


uses
    Windows,
    DB,
    Forms,
    Generics.Collections;

const
    STR_ZP01_ROBOTINI = 'machine.ini';
    STR_ZP01_APPINI = 'Sampler.ini';
    STR_DEVICEINI = 'Device.ini';

    // --------------------------------------------------------------------------------------------------
    // TIniDataWrapper
    // --------------------------------------------------------------------------------------------------
constructor TIniDataWrapper.Create(const aAnyName: string);
begin
    inherited Create;

    // aAreaName can be an area name or a file name
    fArea := GetAreaFromAnyName(aAnyName);

    // fTableType := aTableType;
end;

procedure TIniDataWrapper.ReadIntoCache;
begin

end;

procedure TIniDataWrapper.ReadSectionIntoCache(const aSection: string);
begin

end;

procedure TIniDataWrapper.WriteFromCache;
begin

end;

procedure TIniDataWrapper.WriteSectionFromCache(const aSection: string);
begin

end;

class function TIniDataWrapper.GetFieldLength_Area: integer;
begin
    result := INT_SETTINGS_FLDLEN_AREA;
end;

class function TIniDataWrapper.GetFieldLength_Ident: integer;
begin
    result := INT_SETTINGS_FLDLEN_IDENT;
end;

class function TIniDataWrapper.GetFieldLength_Section: integer;
begin
    result := INT_SETTINGS_FLDLEN_SECTION;
end;

class function TIniDataWrapper.GetFieldLength_Value: integer;
begin
    result := INT_SETTINGS_FLDLEN_VALUE;
end;

class function TIniDataWrapper.GetFileNameFromArea(aAreaName, aDataPath: string): string;
begin
    if (aAreaName = STR_SETTINGS_AREA_ROBOT) then
    begin
        result := aDataPath + STR_ZP01_ROBOTINI;
    end
    else if (aAreaName = STR_SETTINGS_AREA_APP) then
    begin
        result := aDataPath + STR_ZP01_APPINI;
    end
    else if (aAreaName = STR_SETTINGS_AREA_DEVICES) then
        result := aDataPath + STR_DEVICEINI
    else
    begin
        // individual file name
        result := aAreaName;

        // no path
        if (ExtractFilePath(aAreaName) = '') then
            result := aDataPath + result;

        // no extension
        if (ExtractFileExt(aAreaName) = '') then
            result := result + '.ini';
    end;
end;

class function TIniDataWrapper.GetAreaFromAnyName(aAnyName: string): string;
begin
    result := '';

    // Sonderfälle (aAnyName = machine.ini, sampler.ini, ...):
    if (CutAreaName(aAnyName) = CutAreaName(GetFileNameFromArea(STR_SETTINGS_AREA_APP, ''))) then
    begin
        result := STR_SETTINGS_AREA_APP;
    end
    else if (CutAreaName(aAnyName) = CutAreaName(GetFileNameFromArea(STR_SETTINGS_AREA_ROBOT, ''))) then
    begin
        result := STR_SETTINGS_AREA_ROBOT;
    end
    else
    begin
        result := CutAreaName(aAnyName);
    end;
end;

class function TIniDataWrapper.CutAreaName(aAreaName: string): string;
var
    xExt: string;
begin
    // define FArea:
    aAreaName := UpperCase(aAreaName); // upper case
    aAreaName := ExtractFileName(aAreaName); // no path

    // area: no extension
    xExt := ExtractFileExt(aAreaName);
    Delete(aAreaName, 1 + Length(aAreaName) - Length(xExt), Length(xExt));

    // shorten the term to field length
    result := Copy(aAreaName, 1, GetFieldLength_Area)
end;

// --------------------------------------------------------------------------------------------------
// TIniFileDataWrapper -> TIniDataWrapper
// --------------------------------------------------------------------------------------------------
class function TIniFileDataWrapper.GetAreaNames(const aDataPath: string): TStringArray;
var
    x: integer;
    xSearchRec: TSearchRec;
    xFileList: TList<string>;
    xAreas: TList<string>;
begin
    xFileList := TList<string>.Create;
    try
        // find all ini files
        x := FindFirst(aDataPath + '*.ini', 0, xSearchRec);
        while (x = 0) do
        begin
            xFileList.Add(aDataPath + xSearchRec.Name);
            x := FindNext(xSearchRec);
        end;
        SysUtils.FindClose(xSearchRec);

        xAreas := TList<string>.Create();
        try
            // edit list platform-specific!
            for x := 0 to xFileList.Count - 1 do
            begin

                if (Uppercase(xFileList[x]) = Uppercase(GetFileNameFromArea(STR_SETTINGS_AREA_APP,
                    aDataPath))) then
                begin
                    xAreas.Add(STR_SETTINGS_AREA_APP);
                end
                else if (Uppercase(xFileList[x]) = Uppercase(GetFileNameFromArea(STR_SETTINGS_AREA_ROBOT,
                    aDataPath))) then
                begin
                    xAreas.Add(STR_SETTINGS_AREA_ROBOT);
                end
                else
                begin
                    xAreas.Add(UpperCase(xFileList[x]));
                end;
            end;

            result := xAreas.ToArray;

        finally
            FreeAndNil(xAreas);
        end;
    finally
        FreeAndNil(xFileList);
    end;
end;

constructor TIniFileDataWrapper.Create(const aAnyName, aDataPath: string);
begin
    inherited Create(aAnyName);

    if (ExtractFileExt(aAnyName) <> '') or (ExtractFilePath(aAnyName) <> '') then
        // 1. aAreaName is the file name
        FFileName := aAnyName
    else
        // 2. Define file name from its area name:
        FFileName := GetFileNameFromArea(FArea, aDataPath);

    FIniFile := TConfigurationFile.Create(FFileName);
end;

destructor TIniFileDataWrapper.Destroy;
begin
    // schlechtes Design: hier darf Close nicht ausgeführt werden
    FIniFile.Close();
    inherited;
end;

procedure TIniFileDataWrapper.Open;
begin
    FIniFile.Open(false);
end;

function TIniFileDataWrapper.ReadString(aSection, aIdent: string; var aValue: string): boolean;
begin
    Open;
    result := FIniFile.ValueExists(aSection, aIdent);
    aValue := FIniFile.ReadString(aSection, aIdent, '');
end;

procedure TIniFileDataWrapper.WriteString(aCurrentUser: IUser; aReason, aSection, aIdent, aValue: string);
var
    xFileHandle: integer;
begin
    if not FileExists(FFileName) then
    begin
        xFileHandle := FileCreate(FFileName);
        FileClose(xFileHandle);
    end;
    Open;
    FIniFile.WriteString(aSection, aIdent, aValue);
end;

function TIniFileDataWrapper.ReadSection(aSection: string): TStringArray;
begin
    Open;
    result := FIniFile.ReadSection(aSection);
end;

procedure TIniFileDataWrapper.DeleteKey(aCurrentUser: IUser; aReason, aSection, aIdent: string);
begin
    Open;
    if FIniFile.ValueExists(aSection, aIdent) then // TBD_WL kracht, wenn der erste Wert nicht vorhanden ist!
        FIniFile.DeleteKey(aSection, aIdent);
end;

function TIniFileDataWrapper.SectionExists(aSection: string): Boolean;
begin
    Open;
    result := FIniFile.SectionExists(aSection);
end;

function TIniFileDataWrapper.ReadSections(): TStringArray;
begin
    Open;
    result := FIniFile.ReadSections();
end;

function TIniFileDataWrapper.IsEmpty: boolean;
var
    xSections, xIdents: TStringArray;
    x: integer;
begin
    Open;
    result := true;
    // find any ident in the ini file
    xSections := FIniFile.ReadSections();
    for x := 0 to high(xSections) do
    begin
        xIdents := FIniFile.ReadSection(xSections[x]);
        if (Length(xIdents) > 0) then
        begin
            result := false;
            exit;
        end;
    end;

end;

{ TIniDBDataWrapper }

constructor TIniDBDataWrapper.Create(aDataAdaptor: TSettingsDataCacheArea);
begin
    inherited Create(aDataAdaptor.AreaName);
    fDataAdaptor := aDataAdaptor;
end;

procedure TIniDBDataWrapper.DeleteKey(aCurrentUser: IUser; aReason, aSection, aIdent: string);
begin
    fDataAdaptor.RemoveItem(aCurrentUser, aReason, aSection, aIdent);
end;

destructor TIniDBDataWrapper.Destroy;
begin
    // not the owner of fDataAdaptor so dont free
    // FreeAndNil( fDataAdaptor );
    inherited;
end;

function TIniDBDataWrapper.IsEmpty: boolean;
begin
    result := fDataAdaptor.IsEmpty();
end;

function TIniDBDataWrapper.ReadSection(aSection: string): TStringArray;
begin
    result := fDataAdaptor.GetIdentNamesBySection(aSection);
end;

function TIniDBDataWrapper.ReadSections(): TStringArray;
begin
    result := fDataAdaptor.GetSectionNames();
end;

function TIniDBDataWrapper.ReadString(aSection, aIdent: string; var aValue: string): boolean;
begin
    result := fDataAdaptor.GetValue(aSection, aIdent, aValue);
end;

function TIniDBDataWrapper.SectionExists(aSection: string): Boolean;
begin
    result := fDataAdaptor.SectionExists(aSection);
end;

procedure TIniDBDataWrapper.WriteString(aCurrentUser: IUser; aReason, aSection, aIdent, aValue: string);
begin
    fDataAdaptor.SetValue(aCurrentUser, aReason, aSection, aIdent, aValue);
end;

procedure TIniDBDataWrapper.ReadIntoCache;
begin
    fDataAdaptor.ReadIntoCache();
end;

procedure TIniDBDataWrapper.WriteFromCache;
begin
    fDataAdaptor.WriteFromCache();
end;

procedure TIniDBDataWrapper.ReadSectionIntoCache(const aSection: string);
begin
    fDataAdaptor.ReadSectionIntoCache(aSection);

end;

procedure TIniDBDataWrapper.WriteSectionFromCache(const aSection: string);
begin
    fDataAdaptor.WriteSectionFromCache(aSection);
end;


end.
