{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  31.08.07 wl                               TN3811.4 initial revision
  31.08.07 wl                               TN3811.4  STR_SETTINGS_TABLE_SETTINGS von CommonTypes hierher
  09.11.07 pk                               TN3921   Changes for updatemanager
  09.11.07 pk                               TN3922   Dataset changed to DataProvider
  11.06.08 wl  DefinePlatform               TN4143   entfernt
  06.08.08 pk                               TN4165.1 new functions needed for SettingsDataCache
  13.08.08 pk                               TN4165.1 New ReadRecs
  23.09.08 wl                               TN4236   Vereinigung mit SettingsFieldnames
  13.10.08 pk SelectAndOpenSection/Area     TN4272.2 New
  16.01.09 wl                               TN4362   an Änderungen in TQueryDataAdaptor angepasst
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  17.06.10 pk                               TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  05.08.10 pk                               TN5221   Changes needed for Component Ace Absolute Database
  -------------------------------------------------------------------------------------------------- }

unit SettingsDataAdaptor;


interface


uses
    GeneralTypes,
    CommonTypes,
    DataProvider,
    QueryDataAdaptor;

const
    STR_SETTINGS_TBL = 'SETTINGS';

    STR_SETTINGS_FLD_AREA = 'AREA';
    STR_SETTINGS_FLD_SECTION = 'SECTION';
    STR_SETTINGS_FLD_IDENT = 'IDENT';
    STR_SETTINGS_FLD_VALUE = 'VALUE';

    INT_SETTINGS_FLDLEN_AREA = 40;
    INT_SETTINGS_FLDLEN_SECTION = 40;
    INT_SETTINGS_FLDLEN_IDENT = 60;
    INT_SETTINGS_FLDLEN_VALUE = 200;

type
    TSettingsRec = record
        Area, Section, Ident, Value: string;
    end;

    TSettingsRecArray = array of TSettingsRec;

    TSettingsDataAdaptor = class(TQueryDataAdaptor)
    private
        procedure DeleteRecsByArea(const aArea: string);
        class procedure ReadRecFromDataset(aDataset: TDataProvider; var vRec: TSettingsRec);
        class procedure WriteRecToDataset(aDataset: TDataProvider; const aRec: TSettingsRec;
            aAppend: boolean);
        class procedure ReadRecsFromDataset(aDataset: TDataProvider; var vRecs: TSettingsRecArray);
        class procedure WriteRecsToDataset(aDataset: TDataProvider; const aRecs: TSettingsRecArray;
            aAppend: boolean);
        procedure DeleteRecsBySection(const aArea, aSection: string);
    public
        constructor Create();

        function GetAreaNames(): TStringArray;
        //
        procedure ReadRecs(var vRecs: TSettingsRecArray);
        procedure ReadRecsByArea(const aArea: string; var vRecs: TSettingsRecArray);
        procedure WriteRecsByArea(const aArea: string; const aRecs: TSettingsRecArray);
        procedure ReadRecsBySection(const aArea, aSection: string; var vRecs: TSettingsRecArray);
        procedure WriteRecsBySection(const aArea, aSection: string; const aRecs: TSettingsRecArray);
        procedure SelectAndOpenIdent(const aArea, aSection, aIdent: string; const aReadOnly: boolean);
        procedure SelectAndOpenSection(const aArea, aSection: string; aReadOnly: boolean);
        procedure SelectAndOpenArea(const aArea: string; aReadOnly: boolean);
        function ReadString(aArea, aSection, aIdent: string; var aValue: string): boolean;
        procedure WriteString(aCurrentUser: IUser; aArea, aReason, aSection, aIdent, aValue: string);
        function ReadSection(aArea, aSection: string): TStringArray;
        procedure DeleteKey(aCurrentUser: IUser; aArea, aReason, aSection, aIdent: string);
        function SectionExists(aArea, aSection: string): Boolean;
        function ReadAreaField(): string;
        function ReadSectionField(): string;
        function ReadValueField(): string;
        function ReadIdentField(): string;
        procedure WriteAreaField(const aValue: string);
        procedure WriteIdentField(const aValue: string);
        procedure WriteSectionField(const aValue: string);
        procedure WriteValueField(const aValue: string);
        function ReadSections(const aArea: string): TStringArray;
        function IsEmpty(const aArea: string): boolean;
    end;


implementation


uses
    SysUtils,
    DataAdaptor;

const
    STR_SETTINGS_TABLE_SETTINGS = 'SETTINGS';
    STR_SETTINGS_TABLE_METHSETTINGS = 'METHODSETTINGS';

    STR_SQL_TBL = STR_SETTINGS_TABLE_SETTINGS;

    STR_SQL_TBL_DOT = STR_SQL_TBL + '.';

    STR_SQL_FROM = ' FROM ' + STR_SQL_TBL;
    STR_SQL_SELECT = 'SELECT *' + STR_SQL_FROM;
    STR_SQL_DELETE = 'DELETE' + STR_SQL_FROM;

    STR_SQL_FLD_AREA = STR_SETTINGS_FLD_AREA;
    STR_SQL_FLD_SECTION = STR_SQL_TBL_DOT + '"' + STR_SETTINGS_FLD_SECTION + '"';
    STR_SQL_FLD_IDENT = STR_SETTINGS_FLD_IDENT;

    STR_SQL_SELECTDISTINCT_AREA = 'SELECT DISTINCT ' + STR_SQL_FLD_AREA + STR_SQL_FROM;
    STR_SQL_SELECTDISTINCT_SECTION = 'SELECT DISTINCT ' + STR_SQL_FLD_SECTION + STR_SQL_FROM;

    STR_SQL_WHERE_AREA_FMT = STR_SQL_FLD_AREA + ' = ''%s''';
    STR_SQL_WHERE_SECTION_FMT = STR_SQL_FLD_SECTION + ' = ''%s''';
    STR_SQL_WHERE_IDENT_FMT = STR_SQL_FLD_IDENT + ' = ''%s''';

    STR_SQL_WHERE_AREA_AND_SECTION_FMT = STR_SQL_WHERE_AREA_FMT + ' AND ' + STR_SQL_WHERE_SECTION_FMT;
    STR_SQL_WHERE_AREA_AND_SECTION_AND_IDENT_FMT = STR_SQL_WHERE_AREA_AND_SECTION_FMT + ' AND ' +
        STR_SQL_WHERE_IDENT_FMT;

    { TSettingsDataAdaptor }

constructor TSettingsDataAdaptor.Create;
begin
    inherited Create(STR_SETTINGS_TABLE_SETTINGS);
end;

function TSettingsDataAdaptor.GetAreaNames(): TStringArray;
var
    xName: string;
    x: integer;
begin
    self.SelectAndOpen(STR_SQL_SELECTDISTINCT_AREA + ' WHERE ' + STR_SQL_FLD_AREA + ' IS NOT NULL', true);
    try
        SetLength(result, self.DataProvider.RecordCount);
        x := 0;
        while not self.DataProvider.EOF do
        begin
            xName := self.DataProvider.FieldbyName(STR_SQL_FLD_AREA).AsString;

            result[x] := xName;
            Inc(x);

            self.DataProvider.Next;
        end;
    finally
        self.Close();
    end;
end;

procedure TSettingsDataAdaptor.SelectAndOpenIdent(const aArea, aSection, aIdent: string;
    const aReadOnly: boolean);
begin
    self.SelectAndOpenFmt(STR_SQL_SELECT + ' WHERE ' + STR_SQL_WHERE_AREA_AND_SECTION_AND_IDENT_FMT,
        [aArea, aSection, aIdent], aReadOnly);
end;

procedure TSettingsDataAdaptor.SelectAndOpenSection(const aArea, aSection: string; aReadOnly: boolean);
begin
    self.SelectAndOpenFmt(STR_SQL_SELECT + ' WHERE ' + STR_SQL_WHERE_AREA_AND_SECTION_FMT, [aArea, aSection],
        aReadOnly);
end;

procedure TSettingsDataAdaptor.SelectAndOpenArea(const aArea: string; aReadOnly: boolean);
begin
    self.SelectAndOpenFmt(STR_SQL_SELECT + ' WHERE ' + STR_SQL_WHERE_AREA_FMT, [aArea], aReadOnly);
end;

function TSettingsDataAdaptor.ReadString(aArea, aSection, aIdent: string; var aValue: string): boolean;
begin
    result := false;

    self.SelectAndOpenIdent(aArea, aSection, aIdent, true);
    try
        if not self.DataProvider.Eof then
        begin
            aValue := self.DataProvider.FieldbyName(STR_SETTINGS_FLD_VALUE).AsString;
            result := true;
        end;
    finally
        self.Close;
    end;
end;

procedure TSettingsDataAdaptor.WriteString(aCurrentUser: IUser;
    aArea, aReason, aSection, aIdent, aValue: string);
begin
    self.SelectAndOpenIdent(aArea, aSection, aIdent, false);
    try
        if not self.DataProvider.Eof then
        begin
            if (self.DataProvider.FieldbyName(STR_SETTINGS_FLD_VALUE).AsString <> aValue) then
            begin
                self.DataProvider.Edit;
                self.DataProvider.FieldbyName(STR_SETTINGS_FLD_VALUE).AsString := aValue;
                self.DataProvider.Post;
                if (aCurrentUser <> nil) then
                    aCurrentUser.LogDataChanged(self.TableName, aArea + ', ' + aSection + ', ' + aIdent +
                        ', new value:' + aValue, aReason, lctEdit);
            end;
        end
        else
        begin
            self.DataProvider.Append;
            self.DataProvider.FieldbyName(STR_SETTINGS_FLD_AREA).AsString := aArea;
            self.DataProvider.FieldbyName(STR_SETTINGS_FLD_SECTION).AsString := aSection;
            self.DataProvider.FieldbyName(STR_SETTINGS_FLD_IDENT).AsString := aIdent;
            self.DataProvider.FieldbyName(STR_SETTINGS_FLD_VALUE).AsString := aValue;
            self.DataProvider.Post;
            if (aCurrentUser <> nil) then
                aCurrentUser.LogDataChanged(self.TableName, aArea + ', ' + aSection + ', ' + aIdent + ', ' +
                    aValue + ' added', aReason, lctAdd);
        end;
    finally
        self.Close;
    end;
end;

function TSettingsDataAdaptor.ReadSection(aArea, aSection: string): TStringArray;
var
    x: integer;
begin
    SelectAndOpenSection(aArea, aSection, true);
    try
        SetLength(result, self.DataProvider.RecordCount);
        x := 0;
        while not self.DataProvider.Eof do
        begin
            result[x] := self.DataProvider.FieldbyName(STR_SETTINGS_FLD_IDENT).AsString;
            Inc(x);

            self.DataProvider.Next;
        end;
    finally
        self.Close;
    end;
end;

procedure TSettingsDataAdaptor.DeleteKey(aCurrentUser: IUser; aArea, aReason, aSection, aIdent: string);
begin
    self.SelectAndOpenIdent(aArea, aSection, aIdent, false);
    try
        if not self.DataProvider.Eof then
        begin
            self.DataProvider.Delete;
            if (aCurrentUser <> nil) then
                aCurrentUser.LogDataChanged(self.TableName, aArea + ', ' + aSection + ', ' + aIdent +
                    ' deleted', aReason, lctRemove);
        end;
    finally
        self.Close;
    end;
end;

function TSettingsDataAdaptor.SectionExists(aArea, aSection: string): Boolean;
begin
    result := false;
    self.SelectAndOpenSection(aArea, aSection, true);
    try
        if not self.DataProvider.Eof then
        begin
            result := true;
        end;
    finally
        self.Close;
    end;
end;

function TSettingsDataAdaptor.ReadSections(const aArea: string): TStringArray;
var
    x: integer;
begin

    self.SelectAndOpenFmt(STR_SQL_SELECTDISTINCT_SECTION + ' WHERE ' + STR_SQL_WHERE_AREA_FMT, [aArea], true);
    try
        SetLength(result, self.DataProvider.RecordCount);
        x := 0;
        while not self.DataProvider.Eof do
        begin
            result[x] := self.DataProvider.FieldbyName(STR_SETTINGS_FLD_SECTION).AsString;
            Inc(x);
            self.DataProvider.Next;
        end;
    finally
        self.Close;
    end;
end;

function TSettingsDataAdaptor.IsEmpty(const aArea: string): boolean;
begin
    self.SelectAndOpenFmt(STR_SQL_SELECTDISTINCT_SECTION + ' WHERE ' + STR_SQL_WHERE_AREA_FMT, [aArea], true);
    try
        result := self.DataProvider.Eof;
    finally
        self.Close;
    end;
end;

class procedure TSettingsDataAdaptor.ReadRecFromDataset(aDataset: TDataProvider; var vRec: TSettingsRec);
begin
    vRec.Area := aDataset.FieldbyName(STR_SETTINGS_FLD_AREA).AsString;
    vRec.Section := aDataset.FieldbyName(STR_SETTINGS_FLD_SECTION).AsString;
    vRec.Ident := aDataset.FieldbyName(STR_SETTINGS_FLD_IDENT).AsString;
    vRec.Value := aDataset.FieldbyName(STR_SETTINGS_FLD_VALUE).AsString;
end;

class procedure TSettingsDataAdaptor.ReadRecsFromDataset(aDataset: TDataProvider;
    var vRecs: TSettingsRecArray);
var
    x: integer;
begin
    x := 0;
    SetLength(vRecs, aDataset.RecordCount);
    while not aDataset.Eof do
    begin
        ReadRecFromDataset(aDataset, vRecs[x]);
        aDataset.Next;
        Inc(x);
    end;
end;

class procedure TSettingsDataAdaptor.WriteRecToDataset(aDataset: TDataProvider; const aRec: TSettingsRec;
    aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;
    aDataset.FieldbyName(STR_SETTINGS_FLD_AREA).AsString := aRec.Area;
    aDataset.FieldbyName(STR_SETTINGS_FLD_SECTION).AsString := aRec.Section;
    aDataset.FieldbyName(STR_SETTINGS_FLD_IDENT).AsString := aRec.Ident;
    aDataset.FieldbyName(STR_SETTINGS_FLD_VALUE).AsString := aRec.Value;
    aDataset.Post;
end;

class procedure TSettingsDataAdaptor.WriteRecsToDataset(aDataset: TDataProvider;
    const aRecs: TSettingsRecArray; aAppend: boolean);
var
    x: integer;
begin
    for x := 0 to high(aRecs) do
    begin
        WriteRecToDataset(aDataset, aRecs[x], true);
    end;
end;

procedure TSettingsDataAdaptor.ReadRecsBySection(const aArea, aSection: string; var vRecs: TSettingsRecArray);
begin
    self.SelectAndOpenSection(aArea, aSection, true);
    try
        ReadRecsFromDataset(self.DataProvider, vRecs);
    finally
        self.Close();
    end;
end;

procedure TSettingsDataAdaptor.DeleteRecsBySection(const aArea, aSection: string);
begin
    self.ExecSQLFmt(STR_SQL_DELETE + ' WHERE ' + STR_SQL_WHERE_AREA_AND_SECTION_FMT, [aArea, aSection]);
end;

procedure TSettingsDataAdaptor.WriteRecsBySection(const aArea, aSection: string;
    const aRecs: TSettingsRecArray);
begin
    DeleteRecsBySection(aArea, aSection);
    self.SelectAndOpenSection(aArea, aSection, false);
    try
        WriteRecsToDataset(self.DataProvider, aRecs, true);
    finally
        self.Close();
    end;
end;

procedure TSettingsDataAdaptor.ReadRecsByArea(const aArea: string; var vRecs: TSettingsRecArray);
begin
    SelectAndOpenArea(aArea, true);
    try
        ReadRecsFromDataset(self.DataProvider, vRecs);
    finally
        self.Close();
    end;
end;

procedure TSettingsDataAdaptor.DeleteRecsByArea(const aArea: string);
begin
    self.ExecSQLFmt(STR_SQL_DELETE + ' WHERE ' + STR_SQL_WHERE_AREA_FMT, [aArea]);
end;

procedure TSettingsDataAdaptor.WriteRecsByArea(const aArea: string; const aRecs: TSettingsRecArray);
begin
    DeleteRecsByArea(aArea);
    self.SelectAndOpenArea(aArea, false);
    try
        WriteRecsToDataset(self.DataProvider, aRecs, true);
    finally
        self.Close();
    end;
end;

procedure TSettingsDataAdaptor.ReadRecs(var vRecs: TSettingsRecArray);
begin
    self.SelectAndOpen(STR_SQL_SELECT, true);
    try
        ReadRecsFromDataset(self.DataProvider, vRecs);
    finally
        self.Close();
    end;
end;

procedure TSettingsDataAdaptor.WriteAreaField(const aValue: string);
begin
    self.DataProvider.FieldbyName(STR_SETTINGS_FLD_AREA).AsString := aValue;
end;

procedure TSettingsDataAdaptor.WriteSectionField(const aValue: string);
begin
    self.DataProvider.FieldbyName(STR_SETTINGS_FLD_SECTION).AsString := aValue;
end;

procedure TSettingsDataAdaptor.WriteIdentField(const aValue: string);
begin
    self.DataProvider.FieldbyName(STR_SETTINGS_FLD_IDENT).AsString := aValue;
end;

procedure TSettingsDataAdaptor.WriteValueField(const aValue: string);
begin
    self.DataProvider.FieldbyName(STR_SETTINGS_FLD_VALUE).AsString := aValue;
end;

function TSettingsDataAdaptor.ReadAreaField: string;
begin
    result := self.DataProvider.FieldbyName(STR_SETTINGS_FLD_AREA).AsString;
end;

function TSettingsDataAdaptor.ReadSectionField: string;
begin
    result := self.DataProvider.FieldbyName(STR_SETTINGS_FLD_SECTION).AsString;
end;

function TSettingsDataAdaptor.ReadIdentField: string;
begin
    result := self.DataProvider.FieldbyName(STR_SETTINGS_FLD_IDENT).AsString;
end;

function TSettingsDataAdaptor.ReadValueField: string;
begin
    result := self.DataProvider.FieldbyName(STR_SETTINGS_FLD_VALUE).AsString;
end;


end.
