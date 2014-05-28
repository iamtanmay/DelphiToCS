{ --------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Philipp Peter (pp)
  Description  : Data Adaptor for the DISCRETECARRIER.DB
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  16.04.13 pp                               TN6131   Initial revision
  -------------------------------------------------------------------------------------------------- }
unit DiscreteCarrierDataAdaptor;

interface


uses
    Classes,
    GeneralTypes,
    DataProvider,
    QueryDataAdaptor;

type
    TDiscretePos = record
        Position: integer;
        X_mm, Y_mm, Z_mm: double;
    end;

    TDiscretePosRecArray = array of TDiscretePos;

    TDiscreteCarrierDataAdaptor = class(TQueryDataAdaptor)
    private const
        STR_DISCRETECARRIERS_TBL = 'DISCRETECARRIERS';
        STR_DISCRETECARRIERS_FLD_NAME = 'NAME';
        STR_DISCRETECARRIERS_FLD_POSITION = 'Position';
        STR_DISCRETECARRIERS_FLD_X_MM = 'Pos_X_mm';
        STR_DISCRETECARRIERS_FLD_Y_MM = 'Pos_Y_mm';
        STR_DISCRETECARRIERS_FLD_Z_MM = 'Pos_Z_mm';

        INT_DISCRETECARRIERS_FLDLEN_NAME = 20;
        INT_DISCRETECARRIERS_FLDLEN_CAPTYPE = 10;

        STR_DISCRETECARRIERS_INDEX_FIELDS_1 = STR_DISCRETECARRIERS_FLD_NAME;
        STR_DISCRETECARRIERS_INDEX_FIELDS_2 = STR_DISCRETECARRIERS_FLD_POSITION;

        INT_MOST_RECENT_VERSION = 1;

        STR_SQL_WHERE_CARRIER = ' WHERE ' + STR_DISCRETECARRIERS_FLD_NAME + ' =''%s''';
        STR_SQL_FROM = ' FROM ' + STR_DISCRETECARRIERS_TBL;
        STR_SQL_DELETE_FMT = 'DELETE ' + STR_SQL_FROM + STR_SQL_WHERE_CARRIER;
        STR_SQL_DELETE_UNUSEDPOSITIONS_FMT = STR_SQL_DELETE_FMT + 'AND Position > %d';
        STR_SQL_SELECT_DISCRETECARRIERS_FMT = 'SELECT * ' + STR_SQL_FROM + STR_SQL_WHERE_CARRIER;
        STR_SQL_SELECT_DISCRETECARRIERSPOSITION_FMT = STR_SQL_SELECT_DISCRETECARRIERS_FMT + 'AND Position = ''%s''';
        STR_SQL_SELECT_ALL = 'SELECT * ' + STR_SQL_FROM;
    protected
        function GetNameField(): string; override;
    public
        constructor Create();

        procedure SelectAndOpenCarrier(const aCarrier: string; aReadOnly: boolean);
        procedure SelectAndOpenCarrierPosition(const aCarrierName: string; const aCarrierPosition: integer;
            aReadOnly: boolean);
        procedure DeleteCarrier(const aCarrier: string);
        procedure WritePositions(const aCarrierName: string; const aRecArray: TDiscretePosRecArray);
        function ReadCarrier(const aCarrier: string; var vRec: TDiscretePos): boolean;
        procedure ReadRecs(const aCarrier: string; var vRecs: TDiscretePosRecArray);
        class procedure ReadRecFromDataset(aCarrierName: string; aDataset: TDataProvider;
            var vRec: TDiscretePos);
        class procedure ReadRecsFromDataset(aCarrierName: string; aDataset: TDataProvider;
            var vRecs: TDiscretePosRecArray);
        class procedure WriteRecsToDataset(aCarrierName: string; aDataset: TDataProvider;
            const aRecs: TDiscretePosRecArray);
        class procedure WriteRecToDataset(aCarrierName: string; aDataset: TDataProvider;
            const aRec: TDiscretePos; aAppend: boolean);
        class function InstNameExists(const aName: string): boolean;
        class function InstReadAllNames(): TStringArray;

        class function NameFieldLength: integer;
        class function MakeDefaultCarrierRec(aPosition: integer): TDiscretePos;
    end;


implementation


uses
    Graphics,
    SysUtils;

{ TCarrierDataAdaptor }

constructor TDiscreteCarrierDataAdaptor.Create();
begin
    inherited Create(STR_DISCRETECARRIERS_TBL);
end;

function TDiscreteCarrierDataAdaptor.GetNameField(): string;
begin
    result := STR_DISCRETECARRIERS_FLD_NAME;
end;

class function TDiscreteCarrierDataAdaptor.InstNameExists(const aName: string): boolean;
var
    xDA: TDiscreteCarrierDataAdaptor;
begin
    xDA := TDiscreteCarrierDataAdaptor.Create;
    try
        result := xDA.NameExists(aName);
    finally
        FreeAndNil(xDA);
    end;
end;

class function TDiscreteCarrierDataAdaptor.InstReadAllNames: TStringArray;
var
    xDA: TDiscreteCarrierDataAdaptor;
begin
    xDA := TDiscreteCarrierDataAdaptor.Create;
    try
        result := xDA.ReadAllNames();
    finally
        FreeAndNil(xDA);
    end;
end;

procedure TDiscreteCarrierDataAdaptor.DeleteCarrier(const aCarrier: string);
begin
    self.ExecSQL(Format(STR_SQL_DELETE_FMT, [aCarrier]));
end;

procedure TDiscreteCarrierDataAdaptor.SelectAndOpenCarrier(const aCarrier: string; aReadOnly: boolean);
begin
    ASSERT(aCarrier <> '', 'Carrier name is empty');
    SelectAndOpen(Format(STR_SQL_SELECT_DISCRETECARRIERS_FMT, [aCarrier]), aReadOnly);
end;

procedure TDiscreteCarrierDataAdaptor.SelectAndOpenCarrierPosition(const aCarrierName: string;
    const aCarrierPosition: integer; aReadOnly: boolean);
begin
    ASSERT(aCarrierName <> '', 'Carrier name is empty');
    SelectAndOpen(Format(STR_SQL_SELECT_DISCRETECARRIERSPOSITION_FMT, [aCarrierName, IntToStr(aCarrierPosition)]),
        aReadOnly);
end;

procedure TDiscreteCarrierDataAdaptor.WritePositions(const aCarrierName: string;
    const aRecArray: TDiscretePosRecArray);
var
    xAppend: boolean;
    x: integer;
    xCount: integer;
begin
    xCount := high(aRecArray);
    if xCount < 0 then
        EXIT;

    self.SelectAndOpenCarrier(aCarrierName, false);
    try
        if self.DataProvider.RecordCount > xCount + 1 then
        begin
            self.ExecSQL(Format(STR_SQL_DELETE_UNUSEDPOSITIONS_FMT, [aCarrierName, xCount]));
        end;
    finally
        self.Close();
    end;

    for x := 0 to xCount do
    begin
        self.SelectAndOpenCarrierPosition(aCarrierName, aRecArray[x].Position, false);
        try

            xAppend := self.DataProvider.IsEmpty;
            self.WriteRecToDataset(aCarrierName, self.DataProvider, aRecArray[x], xAppend);
        finally
            self.Close();
        end;
    end;
end;

function TDiscreteCarrierDataAdaptor.ReadCarrier(const aCarrier: string; var vRec: TDiscretePos): boolean;
begin
    SelectAndOpenCarrier(aCarrier, true);
    try
        result := not self.DataProvider.IsEmpty();
        if not result then
            EXIT;
        ReadRecFromDataset(aCarrier, self.DataProvider, vRec);
    finally
        Close();
    end;
end;

class procedure TDiscreteCarrierDataAdaptor.ReadRecFromDataset(aCarrierName: string; aDataset: TDataProvider;
    var vRec: TDiscretePos);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    with vRec do
    begin
        Position := aDataset.FieldByName(STR_DISCRETECARRIERS_FLD_POSITION).AsInteger;
        X_mm := aDataset.FieldByName(STR_DISCRETECARRIERS_FLD_X_MM).AsFloat;
        Y_mm := aDataset.FieldByName(STR_DISCRETECARRIERS_FLD_Y_MM).AsFloat;
        Z_mm := aDataset.FieldByName(STR_DISCRETECARRIERS_FLD_Z_MM).AsFloat;
    end;
end;

class procedure TDiscreteCarrierDataAdaptor.ReadRecsFromDataset(aCarrierName: string; aDataset: TDataProvider;
    var vRecs: TDiscretePosRecArray);
var
    i: integer;
begin
    i := 0;
    SetLength(vRecs, aDataset.RecordCount);
    while not aDataset.Eof do
    begin
        ReadRecFromDataset(aCarrierName, aDataset, vRecs[i]);
        aDataset.Next;
        Inc(i);
    end;
end;

class procedure TDiscreteCarrierDataAdaptor.WriteRecToDataset(aCarrierName: string; aDataset: TDataProvider;
    const aRec: TDiscretePos; aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    with aRec do
    begin
        aDataset.FieldByName(STR_DISCRETECARRIERS_FLD_NAME).AsString := aCarrierName;
        aDataset.FieldByName(STR_DISCRETECARRIERS_FLD_POSITION).AsInteger := Position;
        aDataset.FieldByName(STR_DISCRETECARRIERS_FLD_X_MM).AsFloat := X_mm;
        aDataset.FieldByName(STR_DISCRETECARRIERS_FLD_Y_MM).AsFloat := Y_mm;
        aDataset.FieldByName(STR_DISCRETECARRIERS_FLD_Z_MM).AsFloat := Z_mm;
    end;
    aDataset.Post;
end;

class procedure TDiscreteCarrierDataAdaptor.WriteRecsToDataset(aCarrierName: string; aDataset: TDataProvider;
    const aRecs: TDiscretePosRecArray);
var
    i: integer;
begin
    for i := 0 to high(aRecs) do
    begin
        WriteRecToDataset(aCarrierName, aDataset, aRecs[i], true);
    end;
end;

procedure TDiscreteCarrierDataAdaptor.ReadRecs(const aCarrier: string; var vRecs: TDiscretePosRecArray);
begin
    self.SelectAndOpenCarrier(aCarrier, true);
    try
        ReadRecsFromDataset(aCarrier, self.DataProvider, vRecs);
    finally
        self.Close();
    end;
end;

class function TDiscreteCarrierDataAdaptor.NameFieldLength: integer;
begin
    result := INT_DISCRETECARRIERS_FLDLEN_NAME;
end;

class function TDiscreteCarrierDataAdaptor.MakeDefaultCarrierRec(aPosition: integer): TDiscretePos;
begin
    result.Position := aPosition;
    result.X_mm := 0;
    result.Y_mm := 0;
    result.Z_mm := 0;
end;


end.
