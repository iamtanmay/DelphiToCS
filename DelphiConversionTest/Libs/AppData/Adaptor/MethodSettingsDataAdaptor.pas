{ ------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                        track-no improvement/change
  -------- --  ---------------------------   -------- --------------------------------------------------------
  16.06.09 wl                                TN4605   von MethodSettings.pas getrennt
  16.06.09 wl  DeleteUnusedSettings          TN4605   Settings löschen, zu denen keine Methode existiert
  17.06.10 pk                                TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  28.03.11 ts  ReadStartables                TN5504   Methods are ordered by methodname
  14.03.12 wl  FindMethodsByLayout           TN5831   neu: Findet alle Methoden, die ein bestimmtes Layout verwenden
  14.03.12 wl  TStartableMethodRec           TN5831   ersetzt TStringPairRec
  05.12.12 wl  ReadBuildingBlocks            TN6045   neu für BuildingBlock-Editor
  30.08.13 wl                                TN6236   komplett neue Struktur für MethodSettings
  28.11.13 ts  S..Startables/BuildingBlocks  TN6318   sortiert nach methodname
  ------------------------------------------------------------------------------------------------------------ }

unit MethodSettingsDataAdaptor;


interface


uses
    DataProvider,
    QueryDataAdaptor;

type
    TMethodSettingsRec = record
        MethodName: string;
        LayoutName: string;
        Attributes: integer;
        Comment: string;
        IsBuildingBlock: boolean;
        Startable: boolean;
        InitAtStart: boolean;
        DeleteRunDataAtStart: boolean;
        DisplayComponentName: string;
        ImageFileName: string;
        RestartEventName: string;
        EditInRunner: boolean;
    end;

    TMethodSettingsDataAdaptor = class(TQueryDataAdaptor)
    private const
        cTableName = 'METHODSETTINGS';

        cFieldNameMethodName = 'MethodName';
        cFieldNameLayoutName = 'LayoutName';
        cFieldNameAttributes = 'Attributes';
        cFieldNameComment = 'Comment';
        cFieldNameIsBuildingBlock = 'IsBuildingBlock';
        cFieldNameStartable = 'Startable';
        cFieldNameInitAtStart = 'InitAtStart';
        cFieldNameDeleteRunDataAtStart = 'DeleteRunDataAtStart';
        cFieldNameDisplayComponentName = 'DisplayComponentName';
        cFieldNameImageFileName = 'ImageFileName';
        cFieldNameRestartEventName = 'RestartEventName';
        cFieldNameEditInRunner = 'EditInRunner';
    private
        procedure SelectAndOpenBuildingBlocks(aReadOnly: boolean);
        procedure SelectAndOpenStartables(aReadOnly: boolean);
        procedure SelectAndOpenMethod(aName: string; aCaseSensitive: boolean; aReadOnly: boolean);
    protected
        function GetNameField(): string; override;
    public const
        cChooseLayoutAtRuntime = '*';
        cAttributeDefault = 0;
        cAttributeReadOnly = 1;
        cAttributeHidden = 2;
    public
        constructor Create();

        class function GetEmptyRec(): TMethodSettingsRec;
        class function GetStartableText(const aRec: TMethodSettingsRec): string;

        class function ReadRecFromDataset(aDataset: TDataProvider; out oRec: TMethodSettingsRec): boolean;
        class function ReadRecsFromDataset(aDataset: TDataProvider): TArray<TMethodSettingsRec>;

        class procedure WriteRecToDataset(aDataset: TDataProvider; const aRec: TMethodSettingsRec;
            aAppend: boolean);

        function ReadRec(const aMethodName: string): TMethodSettingsRec;
        procedure WriteRec(const aRec: TMethodSettingsRec);

        function ReadAllRecs(): TArray<TMethodSettingsRec>;
        function ReadStartables(): TArray<TMethodSettingsRec>;
        function ReadStartableMethodNames(): TArray<string>;
        function ReadBuildingBlocks(): TArray<TMethodSettingsRec>;
        function MethodExistsIntern(var vName: string; aCaseSensitive: boolean): boolean;
    end;


implementation


uses
    Generics.Collections,
    SysUtils;

{ TMethodSettingsDataAdaptor }

constructor TMethodSettingsDataAdaptor.Create();
begin
    inherited Create(cTableName);
end;

class function TMethodSettingsDataAdaptor.GetEmptyRec: TMethodSettingsRec;
begin
    result.MethodName := '';
    result.LayoutName := '';
    result.Comment := '';
    result.Attributes := cAttributeDefault;
    result.IsBuildingBlock := false;
    result.Startable := false;
    result.InitAtStart := true;
    result.DeleteRunDataAtStart := true;
    result.DisplayComponentName := '';
    result.ImageFileName := '';
    result.RestartEventName := '';
    result.EditInRunner := false;
end;

function TMethodSettingsDataAdaptor.GetNameField: string;
begin
    EXIT(cFieldNameMethodName);
end;

class function TMethodSettingsDataAdaptor.GetStartableText(const aRec: TMethodSettingsRec): string;
begin
    // Startable & layout
    if (aRec.Startable) then
    begin
        result := 'Startable Method';
        if (aRec.LayoutName <> '') then
            result := result + ', Layout: ' + aRec.LayoutName;
        if (aRec.InitAtStart) then
            result := result + ', Use standard init';
    end
    else
    begin
        result := 'Non-Startable Method';
    end;
end;

function TMethodSettingsDataAdaptor.ReadAllRecs: TArray<TMethodSettingsRec>;
begin
    self.SelectAndOpenAllSorted(true);
    try
        EXIT(ReadRecsFromDataset(self.DataProvider));
    finally
        self.Close();
    end;
end;

procedure TMethodSettingsDataAdaptor.SelectAndOpenBuildingBlocks(aReadOnly: boolean);
begin
    SelectAndOpen('SELECT * FROM "' + self.TableName + '" WHERE ' + cFieldNameIsBuildingBlock +
        '=TRUE order by ' + cFieldNameMethodName, aReadOnly);
end;

procedure TMethodSettingsDataAdaptor.SelectAndOpenStartables(aReadOnly: boolean);
begin
    SelectAndOpen('SELECT * FROM "' + self.TableName + '" WHERE ' + cFieldNameStartable + '=TRUE order by ' +
        cFieldNameMethodName, aReadOnly);
end;

function TMethodSettingsDataAdaptor.ReadBuildingBlocks: TArray<TMethodSettingsRec>;
begin
    self.SelectAndOpenBuildingBlocks(true);
    try
        EXIT(ReadRecsFromDataset(self.DataProvider));
    finally
        self.Close();
    end;
end;

function TMethodSettingsDataAdaptor.ReadRec(const aMethodName: string): TMethodSettingsRec;
begin
    self.SelectAndOpenByName(aMethodName, true);
    try
        ReadRecFromDataset(self.DataProvider, result);
    finally
        self.Close();
    end;
end;

class function TMethodSettingsDataAdaptor.ReadRecFromDataset(aDataset: TDataProvider;
    out oRec: TMethodSettingsRec): boolean;
begin
    if aDataset.Eof then
    begin
        oRec := GetEmptyRec();
        EXIT(true);
    end;

    oRec.MethodName := aDataset.FieldbyName(cFieldNameMethodName).AsString;
    oRec.LayoutName := aDataset.FieldbyName(cFieldNameLayoutName).AsString;
    oRec.Attributes := aDataset.FieldbyName(cFieldNameAttributes).AsInteger;
    oRec.Comment := aDataset.FieldbyName(cFieldNameComment).AsString;
    oRec.IsBuildingBlock := aDataset.FieldbyName(cFieldNameIsBuildingBlock).AsBoolean;
    oRec.Startable := aDataset.FieldbyName(cFieldNameStartable).AsBoolean;
    oRec.InitAtStart := aDataset.FieldbyName(cFieldNameInitAtStart).AsBoolean;
    oRec.DeleteRunDataAtStart := aDataset.FieldbyName(cFieldNameDeleteRunDataAtStart).AsBoolean;
    oRec.DisplayComponentName := aDataset.FieldbyName(cFieldNameDisplayComponentName).AsString;
    oRec.ImageFileName := aDataset.FieldbyName(cFieldNameImageFileName).AsString;
    oRec.RestartEventName := aDataset.FieldbyName(cFieldNameRestartEventName).AsString;
    oRec.EditInRunner := aDataset.FieldbyName(cFieldNameEditInRunner).AsBoolean;

    EXIT(false);
end;

class function TMethodSettingsDataAdaptor.ReadRecsFromDataset(aDataset: TDataProvider)
    : TArray<TMethodSettingsRec>;
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

function TMethodSettingsDataAdaptor.ReadStartableMethodNames: TArray<string>;
var
    x: integer;
begin
    self.SelectAndOpenStartables(true);
    try
        x := 0;
        SetLength(result, self.DataProvider.RecordCount);
        while not self.DataProvider.Eof do
        begin
            result[x] := self.DataProvider.FieldbyName(cFieldNameMethodName).AsString;
            self.DataProvider.Next;
            Inc(x);
        end;
    finally
        self.Close();
    end;
end;

function TMethodSettingsDataAdaptor.ReadStartables: TArray<TMethodSettingsRec>;
begin
    self.SelectAndOpenStartables(true);
    try
        EXIT(ReadRecsFromDataset(self.DataProvider));
    finally
        self.Close();
    end;
end;

procedure TMethodSettingsDataAdaptor.WriteRec(const aRec: TMethodSettingsRec);
var
    xAppend: boolean;
begin
    self.SelectAndOpenMethod(aRec.MethodName, false, false);
    try
        xAppend := self.DataProvider.IsEmpty;
        WriteRecToDataset(self.DataProvider, aRec, xAppend);
    finally
        self.Close();
    end;
end;

class procedure TMethodSettingsDataAdaptor.WriteRecToDataset(aDataset: TDataProvider;
    const aRec: TMethodSettingsRec; aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    aDataset.FieldbyName(cFieldNameMethodName).AsString := aRec.MethodName;
    aDataset.FieldbyName(cFieldNameLayoutName).AsString := aRec.LayoutName;
    aDataset.FieldbyName(cFieldNameAttributes).AsInteger := aRec.Attributes;
    aDataset.FieldbyName(cFieldNameComment).AsString := aRec.Comment;
    aDataset.FieldbyName(cFieldNameIsBuildingBlock).AsBoolean := aRec.IsBuildingBlock;
    aDataset.FieldbyName(cFieldNameStartable).AsBoolean := aRec.Startable;
    aDataset.FieldbyName(cFieldNameInitAtStart).AsBoolean := aRec.InitAtStart;
    aDataset.FieldbyName(cFieldNameDeleteRunDataAtStart).AsBoolean := aRec.DeleteRunDataAtStart;
    aDataset.FieldbyName(cFieldNameDisplayComponentName).AsString := aRec.DisplayComponentName;
    aDataset.FieldbyName(cFieldNameImageFileName).AsString := aRec.ImageFileName;
    aDataset.FieldbyName(cFieldNameRestartEventName).AsString := aRec.RestartEventName;
    aDataset.FieldbyName(cFieldNameEditInRunner).AsBoolean := aRec.EditInRunner;

    aDataset.Post;
end;

procedure TMethodSettingsDataAdaptor.SelectAndOpenMethod(aName: string; aCaseSensitive: boolean;
    aReadOnly: boolean);
var
    xSelect: string;
begin
    xSelect := 'SELECT * FROM ' + cTableName;

    if (aName = '') then
    begin
        self.SelectAndOpen(xSelect + ' ORDER BY ' + cFieldNameMethodName, aReadOnly);
        EXIT;
    end;

    if (aCaseSensitive) then
        xSelect := xSelect + ' WHERE ' + cFieldNameMethodName + ' = ''' + aName + ''''
    else
        xSelect := xSelect + ' WHERE UPPER(' + cFieldNameMethodName + ') = ''' + UpperCase(aName) + '''';

    SelectAndOpen(xSelect, aReadOnly);
end;

function TMethodSettingsDataAdaptor.MethodExistsIntern(var vName: string; aCaseSensitive: boolean): boolean;
begin
    self.SelectAndOpenMethod(vName, aCaseSensitive, true);
    try
        if (self.DataProvider.IsEmpty) then
        begin
            result := false;
        end
        else
        begin
            result := true;

            // Groß-/Kleinschreibung des Names lesen
            if (not aCaseSensitive) then
                vName := self.DataProvider.FieldByName(cFieldNameMethodName).AsString;
        end;
    finally
        Close();
    end;
end;


end.
