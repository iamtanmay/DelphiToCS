{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  08.03.13 wl                                      TN6095   Initial Revision
  14.05.13 wl                                      TN6095   überarbeitet
  ----------------------------------------------------------------------------------------------------------- }

unit MethodVarPagesDataAdaptor;


interface


uses
    DataProvider,
    MethodVariableTypes,
    QueryDataAdaptor;

type
    TMethodVarPagesDataAdaptor = class(TQueryDataAdaptor)
    private const
        cMethodVarPagesTable = 'METHODVARPAGES';
        cFieldNameMethodName = 'MethodName';
        cFieldNamePage = 'Page';
        cFieldNameFirstOrderIndex = 'FirstOrderIndex';
        cFieldNameLastOrderIndex = 'LastOrderIndex';
        cFieldNameCaption = 'Caption';
    protected
        function GetNameField(): string; override;
    public
        constructor Create();

        class function ReadRecFromDataset(aDataset: TDataProvider; var vRec: TMethodVarPageRec): boolean;
        class function ReadRecsFromDataset(aDataset: TDataProvider): TArray<TMethodVarPageRec>;

        class procedure WriteRecToDataset(aDataset: TDataProvider; const aRec: TMethodVarPageRec;
            aAppend: boolean);
        class procedure WriteRecsToDataset(aDataset: TDataProvider; const aRecs: TArray<TMethodVarPageRec>;
            aAppend: boolean);

        function ReadAllRecs(): TArray<TMethodVarPageRec>;
        function ReadRecs(const aMethodName: string): TArray<TMethodVarPageRec>;
        procedure WriteRecs(const aMethodName: string; aRecs: TArray<TMethodVarPageRec>);
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    Generics.Collections;

{ TMethodVarPagesDataAdaptor }

constructor TMethodVarPagesDataAdaptor.Create;
begin
    inherited Create(cMethodVarPagesTable);
end;

class function TMethodVarPagesDataAdaptor.ReadRecFromDataset(aDataset: TDataProvider;
    var vRec: TMethodVarPageRec): boolean;
begin
    if aDataset.Eof then
        EXIT(true);

    vRec.MethodName := aDataset.FieldbyName(cFieldNameMethodName).AsString;
    vRec.Page := aDataset.FieldbyName(cFieldNamePage).AsInteger;
    vRec.FirstOrderIndex := aDataset.FieldbyName(cFieldNameFirstOrderIndex).AsInteger;
    vRec.LastOrderIndex := aDataset.FieldbyName(cFieldNameLastOrderIndex).AsInteger;
    vRec.Caption := aDataset.FieldbyName(cFieldNameCaption).AsString;

    EXIT(false);
end;

class function TMethodVarPagesDataAdaptor.ReadRecsFromDataset(aDataset: TDataProvider)
    : TArray<TMethodVarPageRec>;
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

class procedure TMethodVarPagesDataAdaptor.WriteRecToDataset(aDataset: TDataProvider;
    const aRec: TMethodVarPageRec; aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;
    aDataset.FieldbyName(cFieldNameMethodName).AsString := aRec.MethodName;
    aDataset.FieldbyName(cFieldNamePage).AsInteger := aRec.Page;
    aDataset.FieldbyName(cFieldNameFirstOrderIndex).AsInteger := aRec.FirstOrderIndex;
    aDataset.FieldbyName(cFieldNameLastOrderIndex).AsInteger := aRec.LastOrderIndex;
    aDataset.FieldbyName(cFieldNameCaption).AsString := aRec.Caption;

    aDataset.Post;
end;

procedure TMethodVarPagesDataAdaptor.WriteRecs(const aMethodName: string; aRecs: TArray<TMethodVarPageRec>);
begin
    // dann schreiben
    self.SelectAndOpenAll(false);
    try
        WriteRecsToDataset(self.DataProvider, aRecs, true);
    finally
        self.Close();
    end;
end;

class procedure TMethodVarPagesDataAdaptor.WriteRecsToDataset(aDataset: TDataProvider;
    const aRecs: TArray<TMethodVarPageRec>; aAppend: boolean);
var
    x: integer;
begin
    for x := 0 to high(aRecs) do
    begin
        WriteRecToDataset(aDataset, aRecs[x], true);
    end;
end;

function TMethodVarPagesDataAdaptor.GetNameField: string;
begin
    EXIT(cFieldNameMethodName);
end;

function TMethodVarPagesDataAdaptor.ReadAllRecs(): TArray<TMethodVarPageRec>;
begin
    self.SelectAndOpenAll(true);
    try
        EXIT(ReadRecsFromDataset(self.DataProvider));
    finally
        self.Close();
    end;
end;

function TMethodVarPagesDataAdaptor.ReadRecs(const aMethodName: string): TArray<TMethodVarPageRec>;
begin
    self.SelectAndOpenByName(aMethodName, true);
    try
        EXIT(ReadRecsFromDataset(self.DataProvider));
    finally
        self.Close();
    end;
end;


end.
