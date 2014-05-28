{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  31.10.11 wl                                      TN5725   Initial Revision
  21.11.11 wl                                      TN5730   SYSTEMASPSPEED entfernt
  27.11.11 wl                                      TN5730   ISMIX wieder entfernt
  03.02.12 wl  WriteRec                            TN5792   neu für Editor
  ----------------------------------------------------------------------------------------------------------- }

unit SubstanceDataDataAdaptor;


interface


uses
    GeneralTypes,
    DataProvider,
    QueryDataAdaptor;

type
    TSubstanceDataRec = record
        Valid: boolean;
        SubstID: string;
        SubstColor: integer;
        LiqParam: string;
        FullName: string;
        Description: string;
    end;

    TSubstanceDataDataAdaptor = class(TQueryDataAdaptor)
    private const
        STR_TBL = 'SubstanceData';
        STR_FLD_SUBSTID = 'SUBSTID';
        STR_FLD_SUBSTCOLOR = 'SUBSTCOLOR';
        STR_FLD_LIQPARAM = 'LIQPARAM';
        STR_FLD_FULLNAME = 'FULLNAME';
        STR_FLD_DESCRIPTION = 'DESCRIPTION';
    private
        class procedure WriteRecAtCursor(aDataset: TDataProvider; const aRec: TSubstanceDataRec;
            aAppend: boolean);
    protected
        function GetNameField: string; override;
    public
        constructor Create();

        class function GetEmptyRec(): TSubstanceDataRec;
        class function ReadRecAtCursor(aDataset: TDataProvider): TSubstanceDataRec;
        class function ReadRecsAtCursor(aDataset: TDataProvider): TArray<TSubstanceDataRec>;
        function ReadSubstanceData(const aSubstanceData: string): TArray<TSubstanceDataRec>;
        procedure WriteRec(const aRec: TSubstanceDataRec);
        class function InstReadAllNames(): TStringArray;
        function ReadRec(const aName: string; out oRec: TSubstanceDataRec): boolean;
    end;


implementation


uses
    SysUtils;

{ TSubstanceDataDataAdaptor }

constructor TSubstanceDataDataAdaptor.Create;
begin
    inherited Create(STR_TBL);
end;

function TSubstanceDataDataAdaptor.GetNameField(): string;
begin
    result := STR_FLD_SUBSTID;
end;

class procedure TSubstanceDataDataAdaptor.WriteRecAtCursor(aDataset: TDataProvider;
    const aRec: TSubstanceDataRec; aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');

    if not aRec.Valid then
        EXIT;

    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    aDataset.FieldByName(STR_FLD_SUBSTID).AsString := aRec.SubstID;
    aDataset.FieldByName(STR_FLD_SUBSTCOLOR).AsInteger := aRec.SubstColor;
    aDataset.FieldByName(STR_FLD_LIQPARAM).AsString := aRec.LiqParam;
    aDataset.FieldByName(STR_FLD_FULLNAME).AsString := aRec.FullName;
    aDataset.FieldByName(STR_FLD_DESCRIPTION).AsString := aRec.Description;

    aDataset.Post;
end;

procedure TSubstanceDataDataAdaptor.WriteRec(const aRec: TSubstanceDataRec);
var
    xAppend: boolean;
begin
    if not aRec.Valid then
        EXIT;

    self.SelectAndOpenByName(aRec.SubstID, false);
    try
        xAppend := self.DataProvider.Eof;
        WriteRecAtCursor(self.DataProvider, aRec, xAppend);
    finally
        Close();
    end;
end;

function TSubstanceDataDataAdaptor.ReadSubstanceData(const aSubstanceData: string): TArray<TSubstanceDataRec>;
begin
    SelectAndOpenByName(aSubstanceData, true);
    try
        result := ReadRecsAtCursor(self.DataProvider);
    finally
        Close();
    end;
end;

class function TSubstanceDataDataAdaptor.ReadRecAtCursor(aDataset: TDataProvider): TSubstanceDataRec;
begin
    ASSERT(aDataSet.Active, 'Dataset not active');

    result.Valid := true;
    result.SubstID := aDataset.FieldByName(STR_FLD_SUBSTID).AsString;
    result.SubstColor := aDataset.FieldByName(STR_FLD_SUBSTCOLOR).AsInteger;
    result.LiqParam := aDataset.FieldByName(STR_FLD_LIQPARAM).AsString;
    result.FullName := aDataset.FieldByName(STR_FLD_FULLNAME).AsString;
    result.Description := aDataset.FieldByName(STR_FLD_DESCRIPTION).AsString;
end;

class function TSubstanceDataDataAdaptor.ReadRecsAtCursor(aDataset: TDataProvider): TArray<TSubstanceDataRec>;
var
    i: integer;
begin
    i := 0;
    SetLength(result, aDataset.RecordCount);
    while not aDataset.Eof do
    begin
        result[i] := ReadRecAtCursor(aDataset);
        aDataset.Next;
        Inc(i);
    end;
end;

class function TSubstanceDataDataAdaptor.InstReadAllNames(): TStringArray;
var
    xDA: TSubstanceDataDataAdaptor;
begin
    xDA := TSubstanceDataDataAdaptor.Create;
    try
        result := xDA.ReadAllNames();
    finally
        FreeAndNil(xDA);
    end;
end;

function TSubstanceDataDataAdaptor.ReadRec(const aName: string; out oRec: TSubstanceDataRec): boolean;
begin
    self.SelectAndOpenByName(aName, true);
    try
        if not self.DataProvider.Eof then
            oRec := self.ReadRecAtCursor(self.DataProvider)
        else
            oRec := GetEmptyRec();
        EXIT(oRec.Valid);
    finally
        Close();
    end;
end;

class function TSubstanceDataDataAdaptor.GetEmptyRec(): TSubstanceDataRec;
begin
    result.Valid := false;
    result.SubstID := '';
    result.SubstColor := 0;
    result.LiqParam := '';
    result.FullName := '';
    result.Description := '';
end;


end.
