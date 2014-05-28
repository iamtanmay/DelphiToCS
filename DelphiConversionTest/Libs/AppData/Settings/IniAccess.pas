{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : TIniAccess: Wrapper for ini file access,
  TCheckingIniAccess: owns a list of all allowed entries - values can be checked for integrity
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.09.02 wl                               TN1293.1 initial version
  10.10.02 wl  Read-Methods                 TN1293.2 Try..except-Verhalten geändert
  10.10.02 wl                               einfache Read/Write-Methoden --> SimpleIniAccess
  15.10.02 wl                               TN1293.2 neu: FAllowedSection-Konzept
  16.10.02 wl  ReadAllowedSection           TN1293.2 neu
  21.10.02 wl  Write-Methoden               TN1293.1 neu
  21.10.02 wl  GetItemByIdent               TN1293.1 erzeugt bei Bedarf neuen Item in erlaubten Sektionen
  22.10.02 wl  WriteAllowedValue            TN1293.1 Bugfix
  23.10.02 wl  DeleteKey                    TN1293.1 mit direkter Ausführung (aExecute)
  23.10.02 wl  ValueExists                  TN1293.1 neu
  25.10.02 wl                               TN1293.1 erheblich verändert für SimpleIniAccess
  06.11.02 wl                               TN1293.1 uses AppSettings
  06.11.02 wl  GetName, GetFileName         TN1293.1 Methode, um file/section name von DataAdapter zu bekommen
  20.12.02 wl  Create, CreateFileAccess     TN1293.5 2 constructors for DB and File Access
  20.12.02 wl  GetName                      TN1293.5 class method abgeschafft
  20.12.02 wl  ChangeAccessType             TN1293.5 aufgeteilt in ChangeToFileAccess, ChangeToDBAccess
  20.12.02 wl  CreateAllowedEntries         TN1293.5 in TIniAccess Dummy-Methode, in TCheckingIniAccess wird FAllowed erzeugt
  20.12.02 wl  ErrorDialog                  TN1293.5 Logging deaktiviert (nicht aus DLL heraus)
  20.12.02 wl  DeleteKey                    TN1371.1 aAddIfPossible=true: erzeugt einen IniItem in erlaubten Sektionen
  20.12.02 wl  IsCheckingAccess             TN1293.5 neu für IIniAccess
  08.01.03 wl  LoadAllValues, SaveAllValues TN1293.5 geht auch im Fehlerfall weiter
  15.01.03 wl  Create                       TN1295.5 IniDataAdapter-Instanz
  15.01.03 wl                               TN1295.5 TIniAccess ist von IIniAccess abgeleitet
  15.01.03 wl  AddDialog                    TN1295.5 neu: für Settings Editor
  16.01.03 wl  AddDialog                    TN1295.3 überarbeitet
  16.01.03 wl  EditDialog                   TN1295.3 neu: zum Eingeben von Werten
  17.01.03 wl  AddForeignIniEntry           TN1293.3 neue Funktion für Settings Editor
  29.01.03 wl  TCheckingIniAccess.ValueExists TN1420  iniEntry.Free entfernt (führt zu schlimmen Situationen)
  05.02.03 wl  DeleteAllValues              TN1293.3 neu: löscht alle Werte
  05.02.03 wl  ConvertAndSve                TN1293.3 neu: konvertiert alle Daten in anderen Access-Typ
  05.02.03 wl  AddForeignIniEntry           TN1293.3 überarbeitet
  06.02.03 wl  GetReason                    TN1334.3 neu: Grund für Änderungen muß z.T. angegeben werden
  06.02.03 wl  WriteValue                   TN1334.3 User und Reason werden für Logging übergeben
  06.02.03 wl  SaveAllValues                TN1334.3 Integration von GetReason
  06.02.03 wl  ConvertAndSave               TN1334.3 User wird übergeben
  11.02.03 wl  ConvertAndSave               TN1334.3 IniDataAdaptor wird anders erzeugt
  14.02.03 wl  EditDialog                   TN1293.5 Calls wizard with spanner bitmap (Res-no.499)
  12.03.03 wl                               TN1293.5 uses geändert
  10.05.04 wl  ConvertAndSave               TN1788.10 Dateien können jetzt ohne Meldung überschrieben werden
  16.08.05 wl  DeleteAllAndSave             TN2558.4  neu: löscht alle Keys in einer Area
  16.08.05 wl  DeleteAllValues              TN2558.4  jetzt auch für TCheckingIniAccess möglich
  09.06.06 pk                               TN3142    Improved errorhandling
  29.07.07 wl  EditDialog                   TN3811   Wizard wird mit Create, nicht mit CreateWithBitmap aufgerufen
  31.08.07 wl                               TN3811.4 ist nicht mehr von IIniAccess abgeleitet
  04.09.07 wl  this                         TN3811.4 Items property = default
  11.06.08 wl  ConvertAndSave               TN4143   ohne Platform-property
  06.08.08 pk                               TN4165.1 various changes need for new SettingsDataCache
  19.05.09 wl  ConvertAndSave                        Compiler-Fehler entfernt
  06.08.09 wl                               TN4702   Strings werden direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  11.11.09 pk  GetSection                   TN4843   Reimplemented
  15.11.10 pk                               TN5340   Changes to prevent memory leak
  09.05.12 wl  ConvertAndSave               TN5889   Erzeugen von TIniDBDataWrapper wieder implementiert
  21.03.13 wl                               TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit IniAccess;


interface


uses
    SysUtils,
    SettingsDataCache,
    IniDataWrapper,
    IniEntry,
    CommonTypes,
    Generics.Collections,
    IniSection;

type

    EReadIniEntry = class(Exception);
    EWriteIniEntry = class(Exception);

    // Main Class dealing with TIniEntry
    TIniAccess = class(TInterfacedObject)
    protected
        fDeleteAllAllowed: boolean;
        FIniDataWrapper: TIniDataWrapper;
        fItems: TObjectList<TIniEntry>;

        function GetAreaEmpty: boolean; virtual; abstract;
        function GetCount: integer; virtual; abstract;
        function GetItem(aIndex: Integer): TIniEntry; virtual; abstract;

        function GetFileName: string; virtual;
        function GetArea: string; virtual;
        function GetAccessType: TIniAccessType; virtual;
        procedure ErrorDialog(aMessage: string);
        // low-level read & write methods (no exception handling)
        procedure ReadValue(aIniEntry: TIniEntry);
        procedure WriteValue(aCurrentUser: IUser; aReason: string; const aIniEntry: TIniEntry);
        function ReadSectionItems(aIniSection: TIniSection): TArray<string>;
        function GetReason(aCurrentUser: IUser; var aReason: string): boolean;
        function DeleteAll(): boolean; virtual; abstract;
    public
        // Constructor/Destructor
        constructor Create(aIniDataWrapper: TIniDataWrapper; aDeleteAllAllowed: boolean);
        destructor Destroy; override;

        // Cache
        procedure ReadIntoCache;
        procedure ReadSectionIntoCache(const aSection: string);
        procedure WriteFromCache;
        procedure WriteSectionFromCache(const aSection: string);

        // Load all / save all
        procedure LoadAllValues; virtual; abstract;
        function SaveAllValues(aCurrentUser: IUser): boolean; virtual; abstract;
        function GetItemByIdent(aSection, aIdent: string; aAddIfPossible: boolean): TIniEntry;
            virtual; abstract;
        function IsCheckingAccess: boolean; virtual; abstract;

        function SectionExists(aSection: string): Boolean;
        function DeleteKey(aSection, aIdent: string; aExecute: boolean): boolean; virtual;
        function AddDialog: TIniEntry; virtual;
        function EditDialog(aIniEntry: TIniEntry): boolean; virtual;
        function AddForeignIniEntry(aForeignEntry: TIniEntry): TIniEntry; virtual;
        function ConvertAndSave(aCurrentUser: IUser; aDataCache: TSettingsDataCache; aDataPath: string)
            : boolean; virtual;
        function DeleteAllValues(): boolean; virtual;

        // Properties
        property IniDataWrapper: TIniDataWrapper read FIniDataWrapper write FIniDataWrapper;
        property Area: string read GetArea;
        property FileName: string read GetFileName;
        property AreaEmpty: boolean read GetAreaEmpty;
        property Count: integer read GetCount;
        property this[index: Integer]: TIniEntry read GetItem; default;
        property AccessType: TIniAccessType read GetAccessType;
    end;

    TCheckingIniAccess = class(TIniAccess, ICheckingIniAccess)
    protected
        FAllowedSections: TObjectList<TIniSection>;
    protected
        function GetAllowedIniEntry(aSection, aIdent: string; aClassRef: TClass): TIniEntry;
        procedure ReadAllowedValue(aIniEntry: TIniEntry);
        procedure WriteAllowedValue(aIniEntry: TIniEntry);
        procedure CreateAllowedEntries; virtual;
        function DeleteAll(): boolean; override;

        function GetAreaEmpty: boolean; override;
        function GetCount: integer; override;
        function GetItem(aIndex: Integer): TIniEntry; override;
    public
        // Constructor/Destructor
        constructor Create(aIniDataWrapper: TIniDataWrapper; aDeleteAllAllowed: boolean = false);
        destructor Destroy; override;

        // Read Methods
        function ReadBool(aSection, aIdent: string): Boolean;
        function ReadString(aSection, aIdent: string): string;
        function ReadInteger(aSection, aIdent: string): Longint;
        function ReadFloat(aSection, aIdent: string): double;
        // Write Methods
        procedure WriteBool(aSection, aIdent: string; aValue: Boolean);
        procedure WriteString(aSection, aIdent, aValue: string);
        procedure WriteInteger(aSection, aIdent: string; aValue: Longint);
        procedure WriteFloat(aSection, aIdent: string; aValue: double);
        // Load all / save all
        procedure LoadAllValues; override;
        function SaveAllValues(aCurrentUser: IUser): boolean; override;

        function IsCheckingAccess: boolean; override;
        function ValueExists(aSection, aIdent: string): boolean;
        function GetItemByIdent(aSection, aIdent: string; aAddIfPossible: boolean): TIniEntry; override;
        procedure DeleteAllAndSave(aCurrentUser: IUser; const aReason: string);

        function GetAllowedSections(): TArray<string>;
        function GetSection(aCompleteIdent: string): TIniSection;
        // function GetISection(aCompleteIdent: string): IIniSection;
        function ReadAllowedSection(aSection, aDefaultIdent: string): TArray<string>;
    end;


implementation


uses
    Windows,
    Controls,
    DialogUtils,
    GeneralTypes,
    Wizard,
    IniAddEntry,
    IniEditValue,
    CompositeIniEntry;

{ TIniAccess }

constructor TIniAccess.Create(aIniDataWrapper: TIniDataWrapper; aDeleteAllAllowed: boolean);
begin
    inherited Create;

    fDeleteAllAllowed := aDeleteAllAllowed;
    FIniDataWrapper := aIniDataWrapper;

    FItems := TObjectList<TIniEntry>.Create;
end;

destructor TIniAccess.Destroy;
begin
    FreeAndNil(FIniDataWrapper);
    FreeAndNil(FItems);

    inherited Destroy;
end;

function TIniAccess.GetFileName: string;
begin
    if (FIniDataWrapper is TIniFileDataWrapper) then
        result := (FIniDataWrapper as TIniFileDataWrapper).FileName;

    if (FIniDataWrapper is TIniDBDataWrapper) then
        result := (FIniDataWrapper as TIniDBDataWrapper).Area;
end;

function TIniAccess.GetArea: string;
begin
    if (FIniDataWrapper <> nil) then
        result := FIniDataWrapper.Area;
end;

function TIniAccess.GetAccessType: TIniAccessType;

begin
    result := iatNothing;

    if (FIniDataWrapper is TIniFileDataWrapper) then
        result := iatIniFileAccess;

    if (FIniDataWrapper is TIniDBDataWrapper) then
        result := iatDatabaseAccess;
end;

procedure TIniAccess.ErrorDialog(aMessage: string);
begin
    // ShowMessage(aMessage);
    raise Exception.Create('IniAccess Error - ' + aMessage);
end;

function TIniAccess.GetReason(aCurrentUser: IUser; var aReason: string): boolean;
begin
    result := true;
    if (aCurrentUser <> nil) and (aCurrentUser.MustGiveReason) then // User must give reasons for every change
        if (FIniDataWrapper is TIniDBDataWrapper) then // IniAccess must be database access
            if not TDialogUtils.InputQuery(TLanguageString.Read('Please enter a reason for your changes:',
                'Bitte geben Sie einen Grund für die Änderungen an:'),
                TLanguageString.Read('Save changed values', 'Geänderte Werte speichern'), aReason) then
                result := false;
end;

procedure TIniAccess.ReadValue(aIniEntry: TIniEntry);
var
    xValue: string;
begin
    aIniEntry.ValueExists := FIniDataWrapper.ReadString(aIniEntry.Section, aIniEntry.Ident, xValue);
    if (aIniEntry.ValueExists) then
        aIniEntry.Value := xValue
    else
        aIniEntry.Value := aIniEntry.DefaultValue; // Set default value if value could not be read
end;

procedure TIniAccess.WriteValue(aCurrentUser: IUser; aReason: string; const aIniEntry: TIniEntry);
begin
    if (aIniEntry.ValueExists) then
        FIniDataWrapper.WriteString(aCurrentUser, aReason, aIniEntry.Section, aIniEntry.Ident,
            aIniEntry.Value)
    else
        FIniDataWrapper.DeleteKey(aCurrentUser, aReason, aIniEntry.Section, aIniEntry.Ident);
end;

function TIniAccess.ReadSectionItems(aIniSection: TIniSection): TArray<string>;
var
    x: integer;
    xIniEntry: TIniEntry;
begin
    try
        // read all section idents
        result := FIniDataWrapper.ReadSection(aIniSection.Section);

        //
        result := aIniSection.PrepareIdentList(result);

        // clear IniSection
        aIniSection.Clear;

        // load all values
        for x := 0 to high(result) do
        begin
            xIniEntry := aIniSection.Add(result[x]);
            ReadValue(xIniEntry);
        end;
    except
        on E: Exception do
            ErrorDialog(E.Message);
    end
end;

function TIniAccess.SectionExists(aSection: string): Boolean;
begin
    result := FIniDataWrapper.SectionExists(aSection);
end;

function TIniAccess.DeleteKey(aSection, aIdent: string; aExecute: boolean): boolean;
var
    xIniEntry: TIniEntry;
begin
    // get item from FAllowed
    xIniEntry := GetItemByIdent(aSection, aIdent, true);
    if (xIniEntry <> nil) then
    begin
        result := true;

        xIniEntry.ValueExists := false;

        // if aExecute is set the key will be deleted in the ini file
        // (otherwise it will be deleted by SaveAllValues)
        if (aExecute) then
            WriteValue(nil, '', xIniEntry);
    end
    else
        result := false;

end;

function TIniAccess.AddDialog: TIniEntry;
var
    xfrmWizard: TfrmWizard;
    xfraPage1: TfraIniAddEntry;
    xIniEntry: TIniEntry;
begin
    result := nil;
    // Create a "Add new user" wizard
    xfrmWizard := TfrmWizard.Create(nil);
    xfraPage1 := TfraIniAddEntry.Create(xfrmWizard);
    xfraPage1.IniAccess := self;
    xfrmWizard.Add(xfraPage1);

    if (xfrmWizard.ShowModal = mrOK) then
    begin
        if (xfrmWizard.Items[0] is TfraIniAddEntry) then
        begin

            // Get Ini Entry from Add-Entry-Wizard
            xIniEntry := (xfrmWizard.Items[0] as TfraIniAddEntry).IniEntry;

            // call Edit-Value-Wizard
            if EditDialog(xIniEntry) then
                result := xIniEntry;
        end;
    end;

    FreeAndNil(xfrmWizard);
end;

function TIniAccess.EditDialog(aIniEntry: TIniEntry): boolean;
var
    xfrmWizard: TfrmWizard;
    xfraPage1: TfraIniEditValue;
    x: integer;
begin
    result := false;

    // Create a "Edit Value" wizard
    xfrmWizard := TfrmWizard.Create(nil); // WithBitmap( Application, 499{spanner bitmap});

    if (aIniEntry is TCompositeIniEntry) then
    begin
        // Create x pages
        for x := 0 to (aIniEntry as TCompositeIniEntry).SubEntries.Count - 1 do
        begin
            xfraPage1 := TfraIniEditValue.Create(xfrmWizard);
            xfraPage1.Area := FileName;
            xfraPage1.IniEntry := (aIniEntry as TCompositeIniEntry).SubEntries[x];
            xfrmWizard.Add(xfraPage1);
        end;
    end
    else
    begin
        // Create Page
        xfraPage1 := TfraIniEditValue.Create(xfrmWizard);
        xfraPage1.Area := FileName;
        xfraPage1.IniEntry := aIniEntry;
        xfrmWizard.Add(xfraPage1);
    end;

    // Get result
    if (xfrmWizard.ShowModal = mrOK) then
    begin
        aIniEntry.ValueExists := true;
        result := true;
    end;

    FreeAndNil(xfrmWizard);
end;

function TIniAccess.AddForeignIniEntry(aForeignEntry: TIniEntry): TIniEntry;
var
    xIniEntry: TIniEntry;
begin
    result := nil;

    // new value must exist
    if (aForeignEntry = nil) then
        exit;
    if (not aForeignEntry.ValueExists) then
        exit;

    // Check if value exists in this List view
    xIniEntry := GetItemByIdent(aForeignEntry.Section, aForeignEntry.Ident, false);
    if (xIniEntry <> nil) then
    begin
        if (xIniEntry.ValueExists) then
        begin
            if (xIniEntry.Value <> aForeignEntry.Value) then
                if (TDialogUtils.MessageBox(TLanguageString.
                    Read('Identifier [{0}] {1} already exists. Overwrite Value {2} with {3}?',
                    'Identifizierer [{0}] {1} existiert schon. Soll der Wert {2} mit {3} überschrieben werden?',
                    [xIniEntry.Section, xIniEntry.Ident, xIniEntry.Value, aForeignEntry.Value]), '',
                    MB_ICONQUESTION + MB_YESNO) = IDYES) then
                    result := xIniEntry;
        end
        else
            result := xIniEntry;
    end;

    // Check if value exists in this List view
    xIniEntry := GetItemByIdent(aForeignEntry.Section, aForeignEntry.Ident, true);
    if (xIniEntry <> nil) then
        result := xIniEntry;

    if (result <> nil) then
    begin
        xIniEntry.Value := aForeignEntry.Value;
        xIniEntry.ValueExists := true;
    end;
end;

function TIniAccess.ConvertAndSave(aCurrentUser: IUser; aDataCache: TSettingsDataCache;
    aDataPath: string): boolean;
var
    xIniDataWrapper1, xIniDataWrapper2: TIniDataWrapper;
begin
    result := false;
    xIniDataWrapper1 := FIniDataWrapper;
    xIniDataWrapper2 := nil;

    // convert file to database / database to file
    if (xIniDataWrapper1 is TIniFileDataWrapper) then
        xIniDataWrapper2 := TIniDBDataWrapper.Create(aDataCache.FindOrCreateArea(GetArea))
    else
        xIniDataWrapper2 := TIniFileDataWrapper.Create(GetArea, aDataPath);

    // database area must be empty!
    if (xIniDataWrapper2 is TIniDBDataWrapper) and (not xIniDataWrapper2.IsEmpty) then
    begin
        TDialogUtils.MessageBox(TLanguageString.
            Read('Ini file {0} can not be stored as area {1} because the area already exists.',
            'Die INI-Datei {0} kann nicht als Bereich {1} gespeichert werden, da der Bereich schon exisitiert.',
            [(xIniDataWrapper1 as TIniFileDataWrapper).FileName, GetArea]),
            TLanguageString.Read('Convert ini file to settings area',
            'Konvertieren der INI-Datei zu einem Einstellungs-Bereich'), 16);
    end
    else
    begin
        FIniDataWrapper := xIniDataWrapper2; // change data adaptor
        if SaveAllValues(aCurrentUser) then // save all values
            result := true;
        FIniDataWrapper := xIniDataWrapper1; // restore data adaptor
    end;

    FreeAndNil(xIniDataWrapper2);
end;

function TIniAccess.DeleteAllValues(): boolean;
var
    xButton: integer;
begin
    result := false;

    if (not fDeleteAllAllowed) then
        EXIT;

    xButton := TDialogUtils.MessageBox(TLanguageString.
        Read('Do you realy want to delete all values of area [{0}]?',
        'Möchten Sie tatsächlich alle Werte des Bereichs [{0}] löschen ?', [GetArea]),
        TLanguageString.Read('Remove All Values', 'Entferne alle Werte'),
        MB_ICONQUESTION + MB_YESNO + MB_DEFBUTTON2);

    if (xButton = IDYES) then
        result := DeleteAll();
end;

procedure TIniAccess.ReadIntoCache;
begin
    fIniDataWrapper.ReadIntoCache();
end;

procedure TIniAccess.ReadSectionIntoCache(const aSection: string);
begin
    fIniDataWrapper.ReadSectionIntoCache(aSection);

end;

procedure TIniAccess.WriteFromCache;
begin
    fIniDataWrapper.WriteFromCache();

end;

procedure TIniAccess.WriteSectionFromCache(const aSection: string);
begin
    fIniDataWrapper.WriteSectionFromCache(aSection);
end;

{ TCheckingIniAccess }

constructor TCheckingIniAccess.Create(aIniDataWrapper: TIniDataWrapper; aDeleteAllAllowed: boolean);
begin
    inherited Create(aIniDataWrapper, aDeleteAllAllowed);

    fDeleteAllAllowed := aDeleteAllAllowed;
    FAllowedSections := TObjectList<TIniSection>.Create;
    CreateAllowedEntries;
end;

procedure TCheckingIniAccess.CreateAllowedEntries;
begin
    //
end;

destructor TCheckingIniAccess.Destroy;
begin
    FreeAndNil(FAllowedSections);
    inherited Destroy;
end;

function TCheckingIniAccess.IsCheckingAccess: boolean;
begin
    result := true;
end;

procedure TCheckingIniAccess.LoadAllValues;
var
    x: integer;
    xErrorMsg: string;
begin
    xErrorMsg := '';
    ReadIntoCache();
    // Read allowed single values
    for x := 0 to FItems.Count - 1 do
        try
            ReadValue(FItems[x]);
        except
            on E: Exception do
                xErrorMsg := xErrorMsg + ' - ' + E.Message;
        end;

    // Read allowed sections
    for x := 0 to FAllowedSections.Count - 1 do
        try
            ReadSectionItems(FAllowedSections[x]);
        except
            on E: Exception do
                xErrorMsg := xErrorMsg + ' - ' + E.Message;
        end;

    if (xErrorMsg <> '') then
        ErrorDialog(xErrorMsg);
end;

function TCheckingIniAccess.SaveAllValues(aCurrentUser: IUser): boolean;
var
    x, x1, x2: integer;
    xReason, xErrorMsg: string;
begin
    result := GetReason(aCurrentUser, xReason);

    if not(result) then
        EXIT;
    xErrorMsg := '';

    // Write allowed single values
    for x := 0 to FItems.Count - 1 do
    begin
        try
            WriteValue(aCurrentUser, xReason, FItems[x]);
        except
            on E: Exception do
                xErrorMsg := xErrorMsg + ' - ' + E.Message;
        end;
    end;

    // Write allowed sections
    for x1 := 0 to FAllowedSections.Count - 1 do
    begin
        try
            for x2 := 0 to FAllowedSections[x1].Count - 1 do
                WriteValue(aCurrentUser, xReason, FAllowedSections[x1][x2]);
        except
            on E: Exception do
                xErrorMsg := xErrorMsg + ' - ' + E.Message;
        end;
    end;

    if (xErrorMsg <> '') then
        ErrorDialog(xErrorMsg);

    WriteFromCache();

end;

function TCheckingIniAccess.GetAllowedIniEntry(aSection, aIdent: string; aClassRef: TClass): TIniEntry;
begin
    // find value in FAllowed - xIniEntry may be NIL !!
    result := GetItemByIdent(aSection, aIdent, true);

    // Message if item can not be found or added
    if (result = nil) then
        raise EInvalidIniValue.Create(TLanguageString.Read('Ini entry [{0}] {1} is not allowed!',
            'INI-Eintrag [{0}] {1} ist nicht erlaubt!', [aSection, aIdent]));

    // check if ini entry type is the expected type
    if (result.ClassType <> aClassRef) then
        raise EInvalidIniValue.Create(TLanguageString.
            Read('Identifier [{0}] {1}: Allowed Value is not type {2}!',
            'Identifizierer [{0}] {1}: Erlaubter Wert ist nicht Typ {2}!',
            [aSection, aIdent, aClassRef.ClassName]));
end;

procedure TCheckingIniAccess.ReadAllowedValue(aIniEntry: TIniEntry);
var
    xIniEntry: TIniEntry;
begin
    try
        xIniEntry := GetAllowedIniEntry(aIniEntry.Section, aIniEntry.Ident, aIniEntry.ClassType);

        // Read value from ini data Wrapper: Exception EInvalidIniValue, if not valid.
        ReadValue(xIniEntry);

        aIniEntry.Value := xIniEntry.Value;
        aIniEntry.ValueExists := xIniEntry.ValueExists;
    except
        on E: Exception do
            ErrorDialog(E.Message);
    end
end;

procedure TCheckingIniAccess.WriteAllowedValue(aIniEntry: TIniEntry);
var
    xIniEntry: TIniEntry;
begin
    try
        xIniEntry := GetAllowedIniEntry(aIniEntry.Section, aIniEntry.Ident, aIniEntry.ClassType);

        xIniEntry.Value := aIniEntry.Value;
        xIniEntry.ValueExists := true;

        // Write value to ini data Wrapper
        WriteValue(nil, '', xIniEntry);
    except
        on E: Exception do
            ErrorDialog(E.Message);
    end
end;

function TCheckingIniAccess.ReadBool(aSection, aIdent: string): Boolean;
var
    xIniEntry: TBoolEntry;
begin
    xIniEntry := TBoolEntry.CreateWithoutDefault(aSection, aIdent);
    ReadAllowedValue(xIniEntry);
    result := xIniEntry.BoolValue;
    FreeAndNil(xIniEntry);
end;

function TCheckingIniAccess.ReadString(aSection, aIdent: string): string;
var
    xIniEntry: TStringEntry;
begin
    try
        xIniEntry := TStringEntry.CreateWithoutDefault(aSection, aIdent);
        ReadAllowedValue(xIniEntry);
        result := xIniEntry.Value;
        FreeAndNil(xIniEntry);
    except
        on E: Exception do
            raise Exception.CreateFmt('TCheckingIniAccess.ReadString: Section[%s], Ident[%s], - %s ',
                [aSection, aIdent, E.Message]);
    end;
end;

function TCheckingIniAccess.ReadInteger(aSection, aIdent: string): Longint;
var
    xIniEntry: TIntegerEntry;
begin
    xIniEntry := TIntegerEntry.CreateWithoutDefault(aSection, aIdent);
    ReadAllowedValue(xIniEntry);
    result := xIniEntry.IntegerValue;
    FreeAndNil(xIniEntry);
end;

function TCheckingIniAccess.ReadFloat(aSection, aIdent: string): double;
var
    xIniEntry: TFloatEntry;
begin
    xIniEntry := TFloatEntry.CreateWithoutDefault(aSection, aIdent);
    ReadAllowedValue(xIniEntry);
    result := xIniEntry.FloatValue;
    FreeAndNil(xIniEntry);
end;

procedure TCheckingIniAccess.WriteBool(aSection, aIdent: string; aValue: Boolean);
var
    xIniEntry: TBoolEntry;
begin
    xIniEntry := TBoolEntry.CreateWithoutDefault(aSection, aIdent);
    xIniEntry.BoolValue := aValue;
    WriteAllowedValue(xIniEntry);
    FreeAndNil(xIniEntry);
end;

procedure TCheckingIniAccess.WriteInteger(aSection, aIdent: string; aValue: Longint);
var
    xIniEntry: TIntegerEntry;
begin
    xIniEntry := TIntegerEntry.CreateWithoutDefault(aSection, aIdent);
    xIniEntry.IntegerValue := aValue;
    WriteAllowedValue(xIniEntry);
    FreeAndNil(xIniEntry);
end;

procedure TCheckingIniAccess.WriteString(aSection, aIdent, aValue: string);
var
    xIniEntry: TStringEntry;
begin
    xIniEntry := TStringEntry.CreateWithoutDefault(aSection, aIdent);
    xIniEntry.Value := aValue;
    WriteAllowedValue(xIniEntry);
    FreeAndNil(xIniEntry);
end;

procedure TCheckingIniAccess.WriteFloat(aSection, aIdent: string; aValue: double);
var
    xIniEntry: TFloatEntry;
begin
    xIniEntry := TFloatEntry.CreateWithoutDefault(aSection, aIdent);
    xIniEntry.FloatValue := aValue; // hier kann's krachen!
    WriteAllowedValue(xIniEntry);
    FreeAndNil(xIniEntry);
end;

function TCheckingIniAccess.GetCount: integer;
var
    x: integer;
begin
    result := fItems.Count;
    for x := 0 to FAllowedSections.Count - 1 do
        result := result + FAllowedSections[x].Count;
end;

function TCheckingIniAccess.GetItem(aIndex: Integer): TIniEntry;
var
    x: integer;
begin
    result := nil;
    if (aIndex < fItems.Count) then
    begin
        // get item from FAllowed
        result := fItems[aIndex];
    end
    else
    begin
        // get item from FAllowedSections
        aIndex := aIndex - fItems.Count;
        for x := 0 to FAllowedSections.Count - 1 do
            if (aIndex < FAllowedSections[x].Count) then
            begin
                result := FAllowedSections[x][aIndex];
                exit;
            end
            else
                aIndex := aIndex - FAllowedSections[x].Count;
    end;
end;

function TCheckingIniAccess.GetItemByIdent(aSection, aIdent: string; aAddIfPossible: boolean): TIniEntry;
var
    x, x2: integer;
begin
    // serch in the list for the right IniEntry
    for x := 0 to fItems.Count - 1 do
    begin
        if (fItems[x].Ident = aIdent) and (fItems[x].Section = aSection) then
            EXIT(fItems[x]);
    end;

    // get item from FAllowedSections
    for x := 0 to FAllowedSections.Count - 1 do
    begin
        if not FAllowedSections[x].IsAllowed(aSection, aIdent) then
            CONTINUE;

        for x2 := 0 to FAllowedSections[x].Count - 1 do
        begin
            if (FAllowedSections[x][x2].Ident = aIdent) and (FAllowedSections[x][x2].Section = aSection) then
                EXIT(FAllowedSections[x][x2]);
        end;

        if (aAddIfPossible) then
            EXIT(FAllowedSections[x].Add(aIdent));
    end;

    EXIT(nil);
end;

function TCheckingIniAccess.GetAllowedSections(): TArray<string>;
var
    x: integer;
begin
    SetLength(result, FAllowedSections.Count);
    for x := 0 to FAllowedSections.Count - 1 do
        result[x] := FAllowedSections[x].CompleteIdent;
end;

function TCheckingIniAccess.GetSection(aCompleteIdent: string): TIniSection;
var
    x: integer;
begin
    result := nil;
    for x := 0 to FAllowedSections.Count - 1 do
        if (aCompleteIdent = FAllowedSections[x].CompleteIdent) then
            result := FAllowedSections[x];
end;

function TCheckingIniAccess.ReadAllowedSection(aSection, aDefaultIdent: string): TArray<string>;
var
    x: integer;
    xIniSection: TIniSection;
begin
    xIniSection := nil;

    // serch in the list for the right Ini section
    for x := 0 to FAllowedSections.Count - 1 do
    begin
        if (FAllowedSections[x].Section = aSection) and
            (FAllowedSections[x].DefaultIdent = aDefaultIdent) then
        begin
            xIniSection := FAllowedSections[x];
            BREAK;
        end;
    end;

    // Message if section can not be found
    if (xIniSection = nil) then
        TDialogUtils.MessageBox(TLanguageString.Read('Ini section [{0}] {1} is not allowed!',
            'INI-Sektion [{0}] {1} ist nicht erlaubt!', [aSection, aDefaultIdent]), '', 16);

    result := ReadSectionItems(xIniSection);
end;

function TCheckingIniAccess.ValueExists(aSection, aIdent: string): boolean;
var
    xIniEntry: TIniEntry;
begin
    result := false;
    xIniEntry := GetItemByIdent(aSection, aIdent, true);
    if (xIniEntry <> nil) then
    begin
        ReadValue(xIniEntry);
        result := xIniEntry.ValueExists;
    end;
end;

function TCheckingIniAccess.GetAreaEmpty: boolean;
begin
    result := false;
end;

procedure TCheckingIniAccess.DeleteAllAndSave(aCurrentUser: IUser; const aReason: string);
var
    x, x1, x2: integer;
begin
    LoadAllValues;

    for x := 0 to fItems.Count - 1 do
    begin

        FIniDataWrapper.DeleteKey(aCurrentUser, aReason, fItems[x].Section, fItems[x].Ident);
    end;

    for x1 := 0 to FAllowedSections.Count - 1 do
    begin
        for x2 := 0 to FAllowedSections[x1].Count - 1 do
        begin

            FIniDataWrapper.DeleteKey(aCurrentUser, aReason, FAllowedSections[x1][x2].Section,
                FAllowedSections[x1][x2].Ident);
        end;
    end;
end;

function TCheckingIniAccess.DeleteAll(): boolean;
var
    x, x1, x2: integer;
begin
    result := false;

    for x := 0 to fItems.Count - 1 do
    begin

        if (not fItems[x].ValueExists) then
            CONTINUE;
        fItems[x].ValueExists := false;
        result := true;
    end;

    for x1 := 0 to FAllowedSections.Count - 1 do
    begin
        for x2 := 0 to FAllowedSections[x1].Count - 1 do
        begin

            if (not FAllowedSections[x1][x2].ValueExists) then
                CONTINUE;
            FAllowedSections[x1][x2].ValueExists := false;
            result := true;
        end;
    end;
end;


end.
