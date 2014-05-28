{ --------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Thomas Schubert (ts)
  Description  : Data Adaptor for the DISCRETERACK.DB
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  10.11.11 ts                               TN5702   Initial revision
  12.07.12 ts  WritePositions               TN5937   Positions were not saved correctly
  13.11.12 wl  GetKeyFields                 TN6015   kann entfernt werden, wenn Key = NameField
  -------------------------------------------------------------------------------------------------- }

unit DiscreteRackDataAdaptor;


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

    TDiscreteRackDataAdaptor = class(TQueryDataAdaptor)
    private const
        STR_DISCRETERACKS_TBL = 'DISCRETERACKS';
        STR_DISCRETERACKS_FLD_NAME = 'NAME';
        STR_DISCRETERACKS_FLD_POSITION = 'Position';
        STR_DISCRETERACKS_FLD_X_MM = 'Pos_X_mm';
        STR_DISCRETERACKS_FLD_Y_MM = 'Pos_Y_mm';
        STR_DISCRETERACKS_FLD_Z_MM = 'Pos_Z_mm';

        INT_DISCRETERACKS_FLDLEN_NAME = 20;
        INT_DISCRETERACKS_FLDLEN_CAPTYPE = 10;

        STR_DISCRETERACKS_INDEX_FIELDS_1 = STR_DISCRETERACKS_FLD_NAME;
        STR_DISCRETERACKS_INDEX_FIELDS_2 = STR_DISCRETERACKS_FLD_POSITION;

        INT_MOST_RECENT_VERSION = 1;

        STR_SQL_WHERE_RACK = ' WHERE ' + STR_DISCRETERACKS_FLD_NAME + ' =''%s''';
        STR_SQL_FROM = ' FROM ' + STR_DISCRETERACKS_TBL;
        STR_SQL_DELETE_FMT = 'DELETE ' + STR_SQL_FROM + STR_SQL_WHERE_RACK;
        STR_SQL_DELETE_UNUSEDPOSITIONS_FMT = STR_SQL_DELETE_FMT + 'AND Position > %d';
        STR_SQL_SELECT_DISCRETERACKS_FMT = 'SELECT * ' + STR_SQL_FROM + STR_SQL_WHERE_RACK;
        STR_SQL_SELECT_DISCRETERACKSPOSITION_FMT = STR_SQL_SELECT_DISCRETERACKS_FMT + 'AND Position = ''%s''';
        STR_SQL_SELECT_ALL = 'SELECT * ' + STR_SQL_FROM;
    protected
        function GetNameField(): string; override;
    public
        constructor Create();

        procedure SelectAndOpenRack(const aRack: string; aReadOnly: boolean);
        procedure SelectAndOpenRackPosition(const aRackName: string; const aRackPosition: integer;
            aReadOnly: boolean);
        procedure DeleteRack(const aRack: string);
        procedure WritePositions(const aRackName: string; const aRecArray: TDiscretePosRecArray);
        function ReadRack(const aRack: string; var vRec: TDiscretePos): boolean;
        procedure ReadRecs(const aRack: string; var vRecs: TDiscretePosRecArray);
        class procedure ReadRecFromDataset(aRackName: string; aDataset: TDataProvider;
            var vRec: TDiscretePos);
        class procedure ReadRecsFromDataset(aRackName: string; aDataset: TDataProvider;
            var vRecs: TDiscretePosRecArray);
        class procedure WriteRecsToDataset(aRackName: string; aDataset: TDataProvider;
            const aRecs: TDiscretePosRecArray);
        class procedure WriteRecToDataset(aRackName: string; aDataset: TDataProvider;
            const aRec: TDiscretePos; aAppend: boolean);
        class function InstNameExists(const aName: string): boolean;
        class function InstReadAllNames(): TStringArray;

        class function NameFieldLength: integer;
        class function MakeDefaultRackRec(aPosition: integer): TDiscretePos;
    end;


implementation


uses
    Graphics,
    SysUtils;

{ TRackDataAdaptor }

constructor TDiscreteRackDataAdaptor.Create();
begin
    inherited Create(STR_DISCRETERACKS_TBL);
end;

function TDiscreteRackDataAdaptor.GetNameField(): string;
begin
    result := STR_DISCRETERACKS_FLD_NAME;
end;

class function TDiscreteRackDataAdaptor.InstNameExists(const aName: string): boolean;
var
    xDA: TDiscreteRackDataAdaptor;
begin
    xDA := TDiscreteRackDataAdaptor.Create;
    try
        result := xDA.NameExists(aName);
    finally
        FreeAndNil(xDA);
    end;
end;

class function TDiscreteRackDataAdaptor.InstReadAllNames: TStringArray;
var
    xDA: TDiscreteRackDataAdaptor;
begin
    xDA := TDiscreteRackDataAdaptor.Create;
    try
        result := xDA.ReadAllNames();
    finally
        FreeAndNil(xDA);
    end;
end;

procedure TDiscreteRackDataAdaptor.DeleteRack(const aRack: string);
begin
    self.ExecSQL(Format(STR_SQL_DELETE_FMT, [aRack]));
end;

procedure TDiscreteRackDataAdaptor.SelectAndOpenRack(const aRack: string; aReadOnly: boolean);
begin
    ASSERT(aRack <> '', 'Rack name is empty');
    SelectAndOpen(Format(STR_SQL_SELECT_DISCRETERACKS_FMT, [aRack]), aReadOnly);
end;

procedure TDiscreteRackDataAdaptor.SelectAndOpenRackPosition(const aRackName: string;
    const aRackPosition: integer; aReadOnly: boolean);
begin
    ASSERT(aRackName <> '', 'Rack name is empty');
    SelectAndOpen(Format(STR_SQL_SELECT_DISCRETERACKSPOSITION_FMT, [aRackName, IntToStr(aRackPosition)]),
        aReadOnly);
end;

procedure TDiscreteRackDataAdaptor.WritePositions(const aRackName: string;
    const aRecArray: TDiscretePosRecArray);
var
    xAppend: boolean;
    x: integer;
    xCount: integer;
begin
    xCount := high(aRecArray);
    if xCount < 0 then
        EXIT;

    self.SelectAndOpenRack(aRackName, false);
    try
        if self.DataProvider.RecordCount > xCount + 1 then
        begin
            self.ExecSQL(Format(STR_SQL_DELETE_UNUSEDPOSITIONS_FMT, [aRackName, xCount]));
        end;
    finally
        self.Close();
    end;

    for x := 0 to xCount do
    begin
        self.SelectAndOpenRackPosition(aRackName, aRecArray[x].Position, false);
        try

            xAppend := self.DataProvider.IsEmpty;
            self.WriteRecToDataset(aRackName, self.DataProvider, aRecArray[x], xAppend);
        finally
            self.Close();
        end;
    end;
end;

function TDiscreteRackDataAdaptor.ReadRack(const aRack: string; var vRec: TDiscretePos): boolean;
begin
    SelectAndOpenRack(aRack, true);
    try
        result := not self.DataProvider.IsEmpty();
        if not result then
            EXIT;
        ReadRecFromDataset(aRack, self.DataProvider, vRec);
    finally
        Close();
    end;
end;

class procedure TDiscreteRackDataAdaptor.ReadRecFromDataset(aRackName: string; aDataset: TDataProvider;
    var vRec: TDiscretePos);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    with vRec do
    begin
        Position := aDataset.FieldByName(STR_DISCRETERACKS_FLD_POSITION).AsInteger;
        X_mm := aDataset.FieldByName(STR_DISCRETERACKS_FLD_X_MM).AsFloat;
        Y_mm := aDataset.FieldByName(STR_DISCRETERACKS_FLD_Y_MM).AsFloat;
        Z_mm := aDataset.FieldByName(STR_DISCRETERACKS_FLD_Z_MM).AsFloat;
    end;
end;

class procedure TDiscreteRackDataAdaptor.ReadRecsFromDataset(aRackName: string; aDataset: TDataProvider;
    var vRecs: TDiscretePosRecArray);
var
    i: integer;
begin
    i := 0;
    SetLength(vRecs, aDataset.RecordCount);
    while not aDataset.Eof do
    begin
        ReadRecFromDataset(aRackName, aDataset, vRecs[i]);
        aDataset.Next;
        Inc(i);
    end;
end;

class procedure TDiscreteRackDataAdaptor.WriteRecToDataset(aRackName: string; aDataset: TDataProvider;
    const aRec: TDiscretePos; aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    with aRec do
    begin
        aDataset.FieldByName(STR_DISCRETERACKS_FLD_NAME).AsString := aRackName;
        aDataset.FieldByName(STR_DISCRETERACKS_FLD_POSITION).AsInteger := Position;
        aDataset.FieldByName(STR_DISCRETERACKS_FLD_X_MM).AsFloat := X_mm;
        aDataset.FieldByName(STR_DISCRETERACKS_FLD_Y_MM).AsFloat := Y_mm;
        aDataset.FieldByName(STR_DISCRETERACKS_FLD_Z_MM).AsFloat := Z_mm;
    end;
    aDataset.Post;
end;

class procedure TDiscreteRackDataAdaptor.WriteRecsToDataset(aRackName: string; aDataset: TDataProvider;
    const aRecs: TDiscretePosRecArray);
var
    i: integer;
begin
    for i := 0 to high(aRecs) do
    begin
        WriteRecToDataset(aRackName, aDataset, aRecs[i], true);
    end;
end;

procedure TDiscreteRackDataAdaptor.ReadRecs(const aRack: string; var vRecs: TDiscretePosRecArray);
begin
    self.SelectAndOpenRack(aRack, true);
    try
        ReadRecsFromDataset(aRack, self.DataProvider, vRecs);
    finally
        self.Close();
    end;
end;

class function TDiscreteRackDataAdaptor.NameFieldLength: integer;
begin
    result := INT_DISCRETERACKS_FLDLEN_NAME;
end;

class function TDiscreteRackDataAdaptor.MakeDefaultRackRec(aPosition: integer): TDiscretePos;
begin
    result.Position := aPosition;
    result.X_mm := 0;
    result.Y_mm := 0;
    result.Z_mm := 0;
end;


end.
