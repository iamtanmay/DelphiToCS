{ --------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  13.11.12 wl                               TN6015   initial revision
  23.11.12 wl  SelectAndOpenIdents          TN6015.1 jetzt nach Namen sortiert
  28.11.12 wl  ReadDisplayIDs               TN6032   gibt alle Display-IDs zurück (nach Namen sortiert)
  -------------------------------------------------------------------------------------------------- }

unit DisplayComponentsDataAdaptor;


interface


uses
    DataProvider,
    QueryDataAdaptor;

type
    TDisplayComponentRec = record
        Ident: string;
        Value: string;
    end;

    TDisplayComponentsRec = record
        ComponentName: string;
        Ident: string;
        Value: string;
    end;

    TDisplayComponentsDataAdaptor = class(TQueryDataAdaptor)
    private const
        cDisplayComponentsTable = 'DisplayComponents';
        cDisplayComponentsFieldName = 'NAME';
        cDisplayComponentsFieldIdent = 'IDENT';
        cDisplayComponentsFieldValue = 'VALUE';
    private
        class procedure ReadRecFromDataset(aDataset: TDataProvider; var vRec: TDisplayComponentsRec);
        class procedure WriteRecToDataset(aDataset: TDataProvider; const aRec: TDisplayComponentsRec;
            aAppend: boolean);
        class function ReadRecsFromDataset(aDataset: TDataProvider): TArray<TDisplayComponentsRec>;
        class procedure WriteRecsToDataset(aDataset: TDataProvider;
            const aRecs: TArray<TDisplayComponentsRec>; aAppend: boolean);
        procedure SelectAndOpenNameAndIdent(const aComponentName, aIdent: string; const aReadOnly: boolean);
        procedure SelectAndOpenComponentName(const aComponentName: string; aReadOnly: boolean);
        procedure SelectAndOpenIdents(const aIdent, aOrderBy: string; aReadOnly: boolean);

        class function ConvertComponentToComponents(const aComponentName: string;
            const aRecs: TArray<TDisplayComponentRec>): TArray<TDisplayComponentsRec>;
        class function ConvertComponentsToComponent(const aRecs: TArray<TDisplayComponentsRec>)
            : TArray<TDisplayComponentRec>;
    protected
        function GetNameField(): string; override;
    public const
        cDisplayComponentSettingType = 'Type';
        cDisplayComponentSettingDisplayID = 'DisplayID';
    public
        constructor Create();

        function ReadRecs(): TArray<TDisplayComponentsRec>;
        function ReadRecsByComponentName(const aComponentName: string): TArray<TDisplayComponentRec>;
        procedure WriteRecsByComponentName(const aComponentName: string;
            const aRecs: TArray<TDisplayComponentRec>);
        procedure DeleteByComponentName(const aComponentName: string);

        class function ReadNamesForTypes(const aTypes: TArray<string>): TArray<string>;
        class function ReadDisplayIDs(): TArray<string>;
        class function ReadDisplayComponentType(const aDisplayComponentName: string): string;
    end;


implementation


uses
    SysUtils,
    Generics.Collections;

{ TDisplayComponentsDataAdaptor }

class function TDisplayComponentsDataAdaptor.ConvertComponentsToComponent
    (const aRecs: TArray<TDisplayComponentsRec>): TArray<TDisplayComponentRec>;
var
    x: integer;
    xComponentName: string;
begin
    SetLength(result, Length(aRecs));
    if Length(aRecs) <= 0 then
        EXIT;

    xComponentName := aRecs[0].ComponentName;
    for x := 0 to high(aRecs) do
    begin
        ASSERT(aRecs[x].ComponentName = xComponentName, 'DisplayCompont: Data mixed with other component');
        result[x].Ident := aRecs[x].Ident;
        result[x].Value := aRecs[x].Value;
    end;
end;

class function TDisplayComponentsDataAdaptor.ConvertComponentToComponents(const aComponentName: string;
    const aRecs: TArray<TDisplayComponentRec>): TArray<TDisplayComponentsRec>;
var
    x: integer;
begin
    SetLength(result, Length(aRecs));
    for x := 0 to high(aRecs) do
    begin
        result[x].ComponentName := aComponentName;
        result[x].Ident := aRecs[x].Ident;
        result[x].Value := aRecs[x].Value;
    end;
end;

constructor TDisplayComponentsDataAdaptor.Create;
begin
    inherited Create(cDisplayComponentsTable);
end;

procedure TDisplayComponentsDataAdaptor.SelectAndOpenNameAndIdent(const aComponentName, aIdent: string;
    const aReadOnly: boolean);
begin
    self.SelectAndOpenFmt('SELECT * FROM ' + cDisplayComponentsTable + ' WHERE ' + cDisplayComponentsFieldName
        + ' = ''%s''' + ' AND ' + cDisplayComponentsFieldIdent + ' = ''%s''', [aComponentName, aIdent],
        aReadOnly);
end;

procedure TDisplayComponentsDataAdaptor.SelectAndOpenComponentName(const aComponentName: string;
    aReadOnly: boolean);
begin
    self.SelectAndOpenFmt('SELECT * FROM ' + cDisplayComponentsTable + ' WHERE ' + cDisplayComponentsFieldName
        + ' = ''%s''', [aComponentName], aReadOnly);
end;

procedure TDisplayComponentsDataAdaptor.SelectAndOpenIdents(const aIdent, aOrderBy: string;
    aReadOnly: boolean);
begin
    self.SelectAndOpen('SELECT * FROM ' + cDisplayComponentsTable + ' WHERE ' + cDisplayComponentsFieldIdent +
        ' = ''' + aIdent + ''' ORDER BY ' + aOrderBy, aReadOnly);
end;

class procedure TDisplayComponentsDataAdaptor.ReadRecFromDataset(aDataset: TDataProvider;
    var vRec: TDisplayComponentsRec);
begin
    vRec.ComponentName := aDataset.FieldbyName(cDisplayComponentsFieldName).AsString;
    vRec.Ident := aDataset.FieldbyName(cDisplayComponentsFieldIdent).AsString;
    vRec.Value := aDataset.FieldbyName(cDisplayComponentsFieldValue).AsString;
end;

class function TDisplayComponentsDataAdaptor.ReadRecsFromDataset(aDataset: TDataProvider)
    : TArray<TDisplayComponentsRec>;
var
    x: integer;
begin
    x := 0;
    SetLength(result, aDataset.RecordCount);
    while not aDataset.Eof do
    begin
        ReadRecFromDataset(aDataset, result[x]);
        aDataset.Next;
        Inc(x);
    end;
end;

class procedure TDisplayComponentsDataAdaptor.WriteRecToDataset(aDataset: TDataProvider;
    const aRec: TDisplayComponentsRec; aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;
    aDataset.FieldbyName(cDisplayComponentsFieldName).AsString := aRec.ComponentName;
    aDataset.FieldbyName(cDisplayComponentsFieldIdent).AsString := aRec.Ident;
    aDataset.FieldbyName(cDisplayComponentsFieldValue).AsString := aRec.Value;
    aDataset.Post;
end;

class procedure TDisplayComponentsDataAdaptor.WriteRecsToDataset(aDataset: TDataProvider;
    const aRecs: TArray<TDisplayComponentsRec>; aAppend: boolean);
var
    x: integer;
begin
    for x := 0 to high(aRecs) do
    begin
        WriteRecToDataset(aDataset, aRecs[x], true);
    end;
end;

function TDisplayComponentsDataAdaptor.ReadRecsByComponentName(const aComponentName: string)
    : TArray<TDisplayComponentRec>;
var
    xRecs: TArray<TDisplayComponentsRec>;
begin
    self.SelectAndOpenComponentName(aComponentName, true);
    try
        xRecs := ReadRecsFromDataset(self.DataProvider);
        EXIT(ConvertComponentsToComponent(xRecs));
    finally
        self.Close();
    end;
end;

procedure TDisplayComponentsDataAdaptor.DeleteByComponentName(const aComponentName: string);
begin
    self.ExecSQLFmt('DELETE FROM ' + cDisplayComponentsTable + ' WHERE ' + cDisplayComponentsFieldName +
        ' = ''%s''', [aComponentName]);
end;

function TDisplayComponentsDataAdaptor.GetNameField: string;
begin
    EXIT(cDisplayComponentsFieldName);
end;

procedure TDisplayComponentsDataAdaptor.WriteRecsByComponentName(const aComponentName: string;
    const aRecs: TArray<TDisplayComponentRec>);
begin
    DeleteByComponentName(aComponentName);
    self.SelectAndOpenComponentName(aComponentName, false);
    try
        WriteRecsToDataset(self.DataProvider, ConvertComponentToComponents(aComponentName, aRecs), true);
    finally
        self.Close();
    end;
end;

function TDisplayComponentsDataAdaptor.ReadRecs(): TArray<TDisplayComponentsRec>;
begin
    self.SelectAndOpen('SELECT * FROM ' + cDisplayComponentsTable, true);
    try
        EXIT(ReadRecsFromDataset(self.DataProvider));
    finally
        self.Close();
    end;
end;

class function TDisplayComponentsDataAdaptor.ReadNamesForTypes(const aTypes: TArray<string>): TArray<string>;
var
    xList: TList<string>;
    xDA: TDisplayComponentsDataAdaptor;
    xComponentType: string;
    x: integer;
begin
    xList := TList<string>.Create;
    try
        xDA := TDisplayComponentsDataAdaptor.Create;
        try
            // alle Einträge mit dem Identifier "Type" suchen
            xDA.SelectAndOpenIdents(cDisplayComponentSettingType, cDisplayComponentsFieldName, true);
            try
                while not xDA.DataProvider.Eof do
                begin
                    xComponentType := xDA.DataProvider.FieldbyName(cDisplayComponentsFieldValue).AsString;
                    for x := 0 to high(aTypes) do
                        if (xComponentType = aTypes[x]) then
                            xList.Add(xDA.DataProvider.FieldbyName(cDisplayComponentsFieldName).AsString);

                    xDA.DataProvider.Next;
                end;
            finally
                xDA.Close;
            end;

            EXIT(xList.ToArray);
        finally
            FreeAndNil(xDA);
        end;
    finally
        FreeAndNil(xList);
    end;
end;

class function TDisplayComponentsDataAdaptor.ReadDisplayComponentType(const aDisplayComponentName
    : string): string;
var
    xDA: TDisplayComponentsDataAdaptor;
begin
    xDA := TDisplayComponentsDataAdaptor.Create;
    try
        xDA.SelectAndOpenNameAndIdent(aDisplayComponentName, cDisplayComponentSettingType, true);
        try
            EXIT(xDA.DataProvider.FieldbyName(cDisplayComponentsFieldValue).AsString);
        finally
            xDA.Close;
        end;
    finally
        FreeAndNil(xDA);
    end;
end;

class function TDisplayComponentsDataAdaptor.ReadDisplayIDs: TArray<string>;
var
    xList: TList<string>;
    xDA: TDisplayComponentsDataAdaptor;
begin
    xList := TList<string>.Create;
    try
        xDA := TDisplayComponentsDataAdaptor.Create;
        try
            // alle Einträge mit dem Identifier "DisplayID" suchen
            xDA.SelectAndOpenIdents(cDisplayComponentSettingDisplayID, cDisplayComponentsFieldValue, true);
            try
                while not xDA.DataProvider.Eof do
                begin
                    xList.Add(xDA.DataProvider.FieldbyName(cDisplayComponentsFieldValue).AsString);

                    xDA.DataProvider.Next;
                end;
            finally
                xDA.Close;
            end;

            EXIT(xList.ToArray);
        finally
            FreeAndNil(xDA);
        end;
    finally
        FreeAndNil(xList);
    end;
end;


end.
