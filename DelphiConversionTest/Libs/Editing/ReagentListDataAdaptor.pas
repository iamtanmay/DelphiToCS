unit ReagentListDataAdaptor;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Modal Form to insert reagent data
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------- ----------------------------------------------
  03.09.05 wl                               TN2541.4  initial version
  04.03.06 wl                               TN2541.4  Datenbankaufbau ist jetzt viel flexibler
  05.03.06 wl                               TN2541.4  neue Funktionen zum Editieren von Reagenzien
  13.03.06 wl                               TN2541.4  Bugfix
  08.06.06 wl                               TN2541.4  Meldung beim Aufstarten, wenn falsche Einstellungen
  07.08.07 wl                               TN3811.3  TPosinfoAdapter.Create override
  02.10.07 wl                               TN3811.5 benutzt self.Query statt fQuery
  09.11.07 pk                               TN3922    Dataset changed to DataProvider
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  06.08.09 wl  TSettingsMainStringLoader    TN4702   Strings werden direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  -------------------------------------------------------------------------------------------------- }


interface


uses
    ComCtrls,
    DataProvider,
    QueryDataAdaptor,
    CommonTypes,
    GeneralTypes;

type
    TReagentListRec = record
        Data: array of string;
    end;

    TReagentListRecArray = array of TReagentListRec;

    TReagentListTableDefinitions = record
        AliasName: string;
        TableName: string;
        Fields: array of TDBFieldDefinition;
        readonly: boolean;
    end;

    TReagentListDataAdaptor = class(TQueryDataAdaptor)
    private
        fDefinitions: TReagentListTableDefinitions;
        //
        class function ReadDefinitions(): TReagentListTableDefinitions;
        //
        function ReadRecAtCursor(aDataset: TDataProvider): TReagentListRec;
        procedure WriteRecAtCursor(aDataset: TDataProvider; aReagentRec: TReagentListRec; aAppend: boolean);
        //
        procedure SelectAllAndOpen(aReadOnly: boolean);
        procedure SelectByName(const aReagentName: string; aReadOnly: boolean);
        function GetField(aIndex: Integer): TDBFieldDefinition;
        function GetFieldCount: integer;
        function GetReagentField: TDBFieldDefinition;
    public
        constructor Create();

        class function Instance(): TReagentListDataAdaptor;
        //
        class function TableIsDefined(): boolean;
        class function GetEmptyRec(aFieldCount: integer): TReagentListRec;

        function GetAllReagentNames(): TStringArray;
        function ReadReagentDataByName(const aReagentName: string): TReagentListRec;
        procedure DeleteReagent(const aReagentName: string);
        procedure ChangeOrAddData(aReagentRec: TReagentListRec);
        procedure SaveAs(const aSourceName, aTargetName: string);

        property FieldCount: integer read GetFieldCount;
        property Fields[index: Integer]: TDBFieldDefinition read GetField;
        property ReagentField: TDBFieldDefinition read GetReagentField;
    end;


implementation


uses
    Windows,
    AppSettings,
    SysUtils,
    GUIManager;

var
    uInstance: TReagentListDataAdaptor;

    { TReagentListDataAdaptor }

constructor TReagentListDataAdaptor.Create;
begin
    fDefinitions := ReadDefinitions;
    inherited Create(fDefinitions.TableName);
end;

class function TReagentListDataAdaptor.Instance: TReagentListDataAdaptor;
begin
    if not Assigned(uInstance) then
        uInstance := TReagentListDataAdaptor.Create();

    result := uInstance;
end;

class function TReagentListDataAdaptor.ReadDefinitions: TReagentListTableDefinitions;
var
    xIdentNames: TStringArray;
    x: integer;
    xIniAccess: IWinLissyIniAccess;
begin
    xIniAccess := gCommonDll.CreateAppIni;
    result.AliasName := xIniAccess.ReadString('ReagentList', 'AliasName');
    result.TableName := xIniAccess.ReadString('ReagentList', 'TableName');
    result.ReadOnly := xIniAccess.ReadBool('ReagentList', 'ReadOnly');

    xIdentNames := xIniAccess.ReadAllowedSection('ReagentList', 'Fields_');
    SetLength(result.Fields, Length(xIdentNames) + 1);

    // Das erste Feld ist immer "Reagent name"
    result.Fields[0] := xIniAccess.ReadDBFieldDefinition('ReagentList', 'Field_ReagentName');

    for x := 0 to Length(xIdentNames) - 1 do
        result.Fields[x + 1] := xIniAccess.ReadDBFieldDefinition('ReagentList', xIdentNames[x]);

end;

class function TReagentListDataAdaptor.GetEmptyRec(aFieldCount: integer): TReagentListRec;
var
    x: integer;
begin
    SetLength(result.Data, aFieldCount);
    for x := 0 to aFieldCount - 1 do
        result.Data[x] := '';
end;

function TReagentListDataAdaptor.ReadReagentDataByName(const aReagentName: string): TReagentListRec;
begin
    result := self.GetEmptyRec(self.FieldCount);
    self.SelectByName(aReagentName, true);
    try
        if not self.DataProvider.Eof then
        begin
            result := ReadRecAtCursor(self.DataProvider);
        end;
    finally
        Close();
    end;
end;

procedure TReagentListDataAdaptor.SelectAllAndOpen(aReadOnly: boolean);
var
    xSQL: string;
begin
    xSQL := 'SELECT * FROM ":' + fDefinitions.AliasName + ':' + fDefinitions.TableName + '" ORDER BY ' +
        fDefinitions.Fields[0].FieldName;

    SelectAndOpen(xSQL, aReadOnly);
end;

function TReagentListDataAdaptor.ReadRecAtCursor(aDataset: TDataProvider): TReagentListRec;
var
    x: integer;
begin
    result := GetEmptyRec(self.FieldCount);

    for x := 0 to high(fDefinitions.Fields) do
        result.Data[x] := aDataset.FieldByName(fDefinitions.Fields[x].FieldName).AsString;
end;

class function TReagentListDataAdaptor.TableIsDefined(): boolean;
var
    xDefinitions: TReagentListTableDefinitions;
begin
    xDefinitions := ReadDefinitions();

    // für alle 3 Idents müssen Werte gesetzt sein, sonst geht es nicht
    result := (xDefinitions.AliasName <> '') and (xDefinitions.TableName <> '') and
        (xDefinitions.Fields[0].FieldName <> '');
end;

procedure TReagentListDataAdaptor.DeleteReagent(const aReagentName: string);
begin
    try
        ExecSQL('delete from ":' + fDefinitions.AliasName + ':' + fDefinitions.TableName + '" where ' +
            self.ReagentField.FieldName + ' = "' + aReagentName + '"');
    finally
        Close();
    end;
end;

procedure TReagentListDataAdaptor.WriteRecAtCursor(aDataset: TDataProvider; aReagentRec: TReagentListRec;
    aAppend: boolean);
var
    x: integer;
begin
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    for x := 0 to high(fDefinitions.Fields) do
        aDataset.FieldByName(fDefinitions.Fields[x].FieldName).AsString := aReagentRec.Data[x];

    aDataset.Post;
end;

procedure TReagentListDataAdaptor.SelectByName(const aReagentName: string; aReadOnly: boolean);
begin
    self.SelectAndOpen('SELECT * FROM ":' + fDefinitions.AliasName + ':' + fDefinitions.TableName +
        '"  where ' + self.ReagentField.FieldName + ' = "' + aReagentName + '"', aReadOnly);
end;

procedure TReagentListDataAdaptor.ChangeOrAddData(aReagentRec: TReagentListRec);
begin
    if (aReagentRec.Data[0] = '') then
        EXIT;

    self.SelectByName(aReagentRec.Data[0], false);
    try
        if not self.DataProvider.Eof then
            self.WriteRecAtCursor(self.DataProvider, aReagentRec, false)
        else
            self.WriteRecAtCursor(self.DataProvider, aReagentRec, true);
    finally
        Close();
    end;
end;

function TReagentListDataAdaptor.GetField(aIndex: Integer): TDBFieldDefinition;
begin
    result := fDefinitions.Fields[aIndex];
end;

function TReagentListDataAdaptor.GetFieldCount: integer;
begin
    result := high(fDefinitions.Fields) + 1;
end;

function TReagentListDataAdaptor.GetReagentField: TDBFieldDefinition;
begin
    result := fDefinitions.Fields[0];
end;

function TReagentListDataAdaptor.GetAllReagentNames(): TStringArray;
var
    x: integer;
begin
    try
        self.SelectAllAndOpen(true);
        try
            SetLength(result, self.DataProvider.RecordCount);
            x := 0;
            while not self.DataProvider.Eof do
            begin
                result[x] := self.DataProvider.FieldByName(fDefinitions.Fields[0].FieldName).AsString;
                Inc(x);
                self.DataProvider.Next;
            end;
        finally
            Close();
        end;
    except
        gGUIManager.MessageBox('Reagent List Settings are wrong.', '', 0);
    end;
end;

procedure TReagentListDataAdaptor.SaveAs(const aSourceName, aTargetName: string);
var
    xReagRec: TReagentListRec;
    xTypeName: string;
begin
    // Target testen
    xReagRec := self.ReadReagentDataByName(aTargetName);
    if (xReagRec.Data[0] <> '') then
    begin
        xTypeName := TLanguageString.Read('Reagent', 'Reagenz');
        gGUIManager.MessageBox(TLanguageString.Read('The {0} {1} already exist!',
            '{0} {1} existiert bereits!', [xTypeName, aTargetName]), TLanguageString.Read('Save {0} as ..',
            '{0} speichern unter ..', [xTypeName]), MB_ICONSTOP);
        EXIT;
    end;

    xReagRec := self.ReadReagentDataByName(aSourceName);
    xReagRec.Data[0] := aTargetName;
    self.ChangeOrAddData(xReagRec)
end;


end.
