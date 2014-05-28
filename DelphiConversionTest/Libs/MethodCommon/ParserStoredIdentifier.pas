{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.08.07 pk                               TN3788   Code moved here from ParserIdentifier.pas
  06.08.08 pk  ReadWriter                   TN4165.1 New property
  06.11.08 pk                               TN4279   uses ParserIdentDataType
  26.05.09 pk  TParserStoredIdent.EditValue TN4348   add ofNoChangeDir to xOpenDialog.Options so that current system directory is not changed
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  09.02.10 pk                               TN4973   use TDialogUtils.SelectDirectory instead of FileCtrl.SelectDirectory
  11.03.10 ts  CreatePickList               TN5023   Except-Block -> Länge von result muss 1 sein, sonst Access Violation
  13.04.10 wl                               TN5044   uses FileUtilities
  07.05.10 pk  fDefValue                    TN5092   Now Object instead of a simple string
  18.06.10 pk  CreatePickList               TN5152.1 uses DataProvider instead of TQuery
  01.03.12 wl                               TN5822   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit ParserStoredIdentifier;


interface


uses
    SysUtils,
    Classes,
    GeneralTypes,
    ListClasses,
    ParserIdentifier,
    ParserIdentReaderWriter,
    ParserIdentDataType;

type
    ESetParserStoredIdentException = class(Exception);

    TParserStoredIdent = class(TParserIdentifier)
    private
        // Properties:
        fDescription, fMinValue, fMaxValue, fPickList: string;
        fDefValue: TArg;
        fValueFormat: integer;
        fReaderWriter: TParserIdentReaderWriter;
        fOwnsReaderWriter: boolean;
        fOnChange: TNotifyEvent;
        function GetMaxLength: integer;
        function GetCaption: string;
        function GetVisible: boolean;
        procedure CheckEmpty();
        procedure CheckMinMaxValue();
        procedure CheckPickList();
        class function GetNumberValue(const aValue: TArg; out oValue: extended): boolean;
        function GetDefValueAsStr: string;
        procedure SetDefValueAsStr(const aValue: string);
    public
        constructor Create(const aName: string; aReaderWriter: TParserIdentReaderWriter;
            aOwnsReaderWriter: boolean = false);
        constructor CreateWithReaderWriterSettings(const aName: string);
        destructor Destroy(); override;
        procedure ReadValue();
        procedure WriteValue();
        procedure DeleteValue();

        // abgeleitet von IEditableParameter
        function GetPickList(): TStringArray;
        function HasEditFunction(): boolean;
        function EditValue(const aValue: string): string;
        procedure CheckValue();

        procedure ReadMinMaxValues();
        procedure ReadOtherValues();
        procedure ReadDescription();
        class function CreatePickList(aSourceString: string): TStringArray;
        function GetAllExistingVariables(): TStringArray;
        function DeleteIdentifier(): boolean;

        procedure Changed();

        procedure LoadProperties;
        procedure SaveProperties;

        function CanUseValue(): boolean; virtual;

        property Description: string read fDescription write fDescription;
        property MinValue: string read fMinValue write fMinValue;
        property MaxValue: string read fMaxValue write fMaxValue;
        property DefValue: TArg read fDefValue write fDefValue;
        property DefValueAsStr: string read GetDefValueAsStr write SetDefValueAsStr;
        property PickList: string read fPickList write fPickList;
        property ValueFormat: integer read fValueFormat write fValueFormat;
        // neu:
        property MaxLength: integer read GetMaxLength;
        property Visible: boolean read GetVisible;
        property Caption: string read GetCaption;
        property OnChange: TNotifyEvent read fOnChange write fOnChange;
        property ReaderWriter: TParserIdentReaderWriter read fReaderWriter;
    end;

    TParserStoredIdentRun = class(TParserStoredIdent)
    public
        function CanUseValue(): boolean; override;
    end;


implementation


uses
    Windows,
    Dialogs,
    CommonTypes,
    AppSettings,
    StringUtilities,
    FileUtilities,
    DialogUtils,
    GUIManager,
    ParserEvalNode,
    DataProvider,
    DataProviderFactory;

{ TParserStoredIdent }

function TParserStoredIdent.CanUseValue(): boolean;
begin
    result := self.HasValue;
end;

constructor TParserStoredIdent.CreateWithReaderWriterSettings(const aName: string);
begin
    Create(aName, TParserIdentReaderWriterSettings.Create(), true);
end;

constructor TParserStoredIdent.Create(const aName: string; aReaderWriter: TParserIdentReaderWriter;
    aOwnsReaderWriter: boolean = false);
begin
    inherited Create(aName);
    fReaderWriter := aReaderWriter;
    fOwnsReaderWriter := aOwnsReaderWriter;
end;

destructor TParserStoredIdent.Destroy();
begin
    if fOwnsReaderWriter then
        fReaderWriter.Free;

    inherited;
end;

procedure TParserStoredIdent.ReadValue();
begin
    fValue := self.StrToValue(fReaderWriter.ReadValue(fName));
end;

procedure TParserStoredIdent.DeleteValue();
begin
    fReaderWriter.DeleteValue(fName);
end;

procedure TParserStoredIdent.WriteValue();
begin
    fReaderWriter.WriteValue(fName, self.ValueToStr(fValue));
end;

procedure TParserStoredIdent.ReadMinMaxValues();
begin
    fReaderWriter.ReadMinMaxValues(fName, fMinValue, fMaxValue);
end;

procedure TParserStoredIdent.ReadOtherValues();
var
    xDefValue: string;
begin
    fReaderWriter.ReadOtherValues(fName, fValueFormat, fDescription, xDefValue, fPickList);
    fDefValue := self.StrToValue(xDefValue);
end;

procedure TParserStoredIdent.ReadDescription();
begin
    fDescription := fReaderWriter.ReadDescription(fName);
end;

function TParserStoredIdent.GetAllExistingVariables(): TStringArray;
begin
    result := fReaderWriter.GetAllExistingVariables();
end;

function TParserStoredIdent.DeleteIdentifier(): boolean;
begin
    result := false;
    if (fName = '') then
        EXIT;

    if gGUIManager.MessageBox(TLanguageString.Read('Do you really want to delete {0} {1}?',
        'Wollen Sie wirklich {0} {1} löschen?', ['Variable', fName]), TLanguageString.Read('Delete variable',
        'Variable löschen'), MB_ICONWARNING + MB_YESNO + MB_DEFBUTTON2) = IDNO then
        EXIT;

    fReaderWriter.DeleteIdentifier(fName);
    result := true;
end;

procedure TParserStoredIdent.LoadProperties;
begin
    ReadMinMaxValues();
    ReadOtherValues();
end;

procedure TParserStoredIdent.SaveProperties;
begin
    fReaderWriter.SaveProperties(fName, fDescription, fMinValue, fMaxValue, self.ValueToStr(fDefValue),
        fPickList, fValueFormat);
end;

procedure TParserStoredIdent.SetDefValueAsStr(const aValue: string);
begin
    self.DefValue := self.StrToValue(aValue);
end;

function TParserStoredIdent.GetDefValueAsStr: string;
begin
    result := self.ValueToStr(self.DefValue);
end;

class function TParserStoredIdent.CreatePickList(aSourceString: string): TStringArray;
var
    x: integer;
    xQuery: TDataProvider;
begin
    try
        // 4 Möglichkeiten:
        if (Pos(STR_IDENT_PICKLIST_PATH, Uppercase(aSourceString)) = 1) then
        begin

            // a) Zeilen aus Datei lesen
            result := TFileUtilities.TakeAllStringsFromFile(Copy(aSourceString, 7, Length(aSourceString)));

            // b) SQL-Datei
            if (Length(result) > 0) and (Pos(STR_IDENT_PICKLIST_SQL, UpperCase(result[0])) = 1) then
            begin
                for x := 0 to Length(result) - 1 do
                    aSourceString := result[x] + ' ';
                SetLength(result, 0);
            end
            else
                EXIT; // a) Zeilen sind schon in der Picklist
        end;

        // b/c) SQl-String auswerten
        if (Pos(STR_IDENT_PICKLIST_SQL, UpperCase(aSourceString)) = 1) then
        begin
            xQuery := TDataProviderFactory.Instance.CreateDataProvider();
            try
                xQuery.SelectAndOpen(aSourceString, true);
                try
                    SetLength(result, xQuery.RecordCount);
                    x := 0;
                    while not xQuery.EOF do
                    begin
                        result[x] := xQuery.Fields[0].AsString;
                        Inc(x);
                        xQuery.Next;
                    end;
                finally
                    xQuery.Close;
                end;
            finally
                FreeAndNil(xQuery);
            end;

            EXIT;
        end;

        // d) durch Komma getrennte strings
        result := TStringUtilities.StringToStringArray(aSourceString, ',');

    except
        SetLength(result, 1);
        result[0] := 'Error reading list items';
    end;
end;

function TParserStoredIdent.GetPickList(): TStringArray;
begin
    result := self.CreatePickList(self.PickList);
end;

function TParserStoredIdent.HasEditFunction(): boolean;
begin
    result := (self.ValueFormat = INT_VALUE_IS_PATHNAME) or (self.ValueFormat = INT_VALUE_IS_FILENAME);
end;

function TParserStoredIdent.GetMaxLength: integer;
begin
    result := 0;
end;

function TParserStoredIdent.GetCaption: string;
begin
    // Define Label text
    if (self.Description <> '') then
        result := self.Description
    else
        result := TLanguageString.Read('Parameter: {0}', 'Parameter: {0}', [self.Name]);

    if (self.MinValue <> '') or (self.MaxValue <> '') then
        result := result + '   [' + self.MinValue + ' .. ' + self.MaxValue + ']';
end;

function TParserStoredIdent.GetVisible: boolean;
begin
    result := true;
end;

function TParserStoredIdent.EditValue(const aValue: string): string;
var
    xOpenDialog: TOpenDialog;
begin
    result := aValue;

    if (self.ValueFormat = INT_VALUE_IS_PATHNAME) then
    begin
        TDialogUtils.SelectDirectory(TLanguageString.Read('Please choose directory for parameter {0}:',
            'Bitte wählen Sie einen Pfadnamen für Parameter {0}:', [self.Caption]), aValue, result);
    end;

    if (self.ValueFormat = INT_VALUE_IS_FILENAME) then
    begin
        xOpenDialog := TOpenDialog.Create(nil);
        try
            xOpenDialog.Options := xOpenDialog.Options + [ofNoChangeDir];
            xOpenDialog.FileName := aValue;
            xOpenDialog.Execute;
            result := xOpenDialog.FileName;
        finally
            xOpenDialog.Free;
        end;
    end;
end;

procedure TParserStoredIdent.Changed();
begin
    if Assigned(fOnChange) then
        fOnChange(self);
end;

class function TParserStoredIdent.GetNumberValue(const aValue: TArg; out oValue: extended): boolean;
begin
    result := false;
    if aValue is TDblArg then
    begin
        result := true;
        oValue := (aValue as TDblArg).AsFloat;
    end
    else if aValue is TIntArg then
    begin
        result := true;
        oValue := (aValue as TIntArg).AsInt;
    end;
end;

procedure TParserStoredIdent.CheckMinMaxValue();
var

    xValue, xMinVal, xMaxVal: extended;
begin

    if not GetNumberValue(self.Value, xValue) then
        EXIT;

    if (self.MinValue <> '') then
    begin
        if TryStrToFloat(self.MinValue, xMinVal) then
        begin

            if (xValue < xMinVal) then
                raise ESetParserStoredIdentException.Create(TLanguageString.
                    Read('Parameter {0} value [{1}] is smaller than minimum value [{2}].',
                    'Der Wert für Parameter {0} [{1}] ist kleiner als der Minimalwert [{2}].',
                    [self.Name, xValue, xMinVal]));
        end;
    end;

    if (self.MaxValue <> '') then
    begin
        if TryStrToFloat(self.MaxValue, xMaxVal) then
        begin

            if (xValue > xMaxVal) then
                raise ESetParserStoredIdentException.Create(TLanguageString.
                    Read('Parameter {0} value [{1}] is bigger than maximum value [{2}].',
                    'Der Wert für Parameter {0} [{1}] ist größer als der Maximalwert [{2}].',
                    [self.Name, xValue, xMaxVal]));
        end;
    end;
end;

procedure TParserStoredIdent.CheckPickList();
var
    x: integer;
    xStrings: TStringValueList;
    xStrValue: string;
begin
    // if self.GetIdentType( self.Name ) <> itNone then EXIT;

    xStrings := TStringValueList.Create;
    try
        xStrings.AddRange(self.GetPickList());

        if (xStrings.Count <= 0) then
            EXIT;

        for x := 0 to xStrings.Count - 1 do
        begin
            xStrValue := self.ValueToStr(self.Value);
            if (xStrValue = xStrings[x]) then
                EXIT;
        end;

        raise ESetParserStoredIdentException.Create(TLanguageString.
            Read('Parameter {0} value [{1}] is not member of the pick list.',
            'Der Wert für Parameter {0} [{1}] ist nicht in der Auswahlliste enthalten.',
            [self.Name, xStrValue]));
    finally
        FreeAndNil(xStrings);
    end;
end;

procedure TParserStoredIdent.CheckEmpty();
begin
    if (self.Value = nil) then
        raise ESetParserStoredIdentException.Create(TLanguageString.Read('Parameter {0} value is empty.',
            'Der Wert für Parameter {0} ist leer.', [self.Name]));
end;

procedure TParserStoredIdent.CheckValue();
begin
    CheckEmpty();
    CheckMinMaxValue();
    CheckPickList();
end;

{ TParserStoredIdentRun }

function TParserStoredIdentRun.CanUseValue(): boolean;
// The variable can always use the value of the StoredIdent, even if the value of the StoredIdent is empty(null)
begin
    result := true;
end;


end.
