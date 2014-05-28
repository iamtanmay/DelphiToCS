{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  28.10.11 wl                                      TN5728   Ableger von POSINFO, nur für die Verwaltung von Disposable Tips
  ----------------------------------------------------------------------------------------------------------- }

unit DisposableTipDataAdaptor;


interface


uses
    QueryDataAdaptor,
    DataProvider;

type
    TDisposableTipRec = record
        Valid: boolean;
        RackID: string;
        Pos: integer;
    end;

    TDisposableTipRecs = array of TDisposableTipRec;

    TDisposableTipDataAdaptor = class(TQueryDataAdaptor)
    private const
        STR_SQL_TBL = 'DisposableTip';
        STR_FLD_RACKID = 'RACKID'; // ALPHA(20)
        STR_FLD_POS = 'POS'; // SHORT
    private
        class procedure WriteDisposableTipDataToDataset(aDataset: TDataProvider; aAppend: boolean;
            aWriteAmount: boolean; const aRackID: string; aPos: integer);
        class function ReadRackIDFromDataset(aDataset: TDataProvider): string;
        class function ReadPosFromDataset(aDataset: TDataProvider): integer;

        class procedure WriteRackIDToDataset(aDataset: TDataProvider; const aRackID: string);
        class procedure WritePosToDataset(aDataset: TDataProvider; aPos: integer);
    public
        constructor Create();

        // TQuery-Based
        procedure SelectAndOpenAll(aReadOnly: boolean);
        procedure SelectAndOpenByRackID(const aRackID: string; aReadOnly: boolean = true);
        procedure SelectAndOpenByRackIDAndPos(const aRackID: string; aPos: integer; aReadOnly: boolean);

        function ReadRackID: string;
        function ReadPos(): integer;
        procedure WriteRackID(const aRackID: string);
        procedure WritePos(aPos: integer);

        procedure InternInsertIfNotFound(const aRackID: string; aPos: integer);
        function EditOrAppendRec(const aKeyRackID: string; aKeyPos, aKeyStep: integer; const aSubstID: string;
            const aOrigin: string; aAmount: double; aUnit: integer; aColor: integer): boolean;
        procedure AppendRec(aRackID: string; aPos, aStep: integer; aSubstID, aOrigin: string; aAmount: double;
            aUnitID: integer; aColor: integer);

        class function MakeDisposableTipRec(aValid: boolean; aRackID: string; aPos: integer)
            : TDisposableTipRec;
        class procedure WriteDisposableTipRecToDataset(aDataset: TDataProvider;
            aDisposableTipRec: TDisposableTipRec; aAppend: boolean);
        class function ReadDisposableTipRecFromDataset(aDataset: TDataProvider): TDisposableTipRec;
        class function ReadDisposableTipRecsFromDataset(aDataset: TDataProvider): TDisposableTipRecs;

        function GetPosFromRackID(const aRackID: string; out oPos: integer): boolean;
        procedure DeletePositions(const aRackID: string; aLastPos: integer);
    end;


implementation


uses
    SysUtils;

{ TDisposableTipDataAdaptor }

constructor TDisposableTipDataAdaptor.Create();
begin
    inherited Create(STR_SQL_TBL);
end;

procedure TDisposableTipDataAdaptor.SelectAndOpenAll(aReadOnly: boolean);
begin
    SelectAndOpen('SELECT * FROM ' + STR_SQL_TBL, aReadOnly);
end;

procedure TDisposableTipDataAdaptor.SelectAndOpenByRackID(const aRackID: string; aReadOnly: boolean = true);
begin
    SelectAndOpen('SELECT * FROM ' + STR_SQL_TBL + ' WHERE ' + STR_FLD_RACKID + ' = '''+aRackID+'''', aReadOnly);
end;

procedure TDisposableTipDataAdaptor.SelectAndOpenByRackIDAndPos(const aRackID: string; aPos: integer;
    aReadOnly: boolean);
begin
    SelectAndOpen('SELECT * FROM ' + STR_SQL_TBL + ' WHERE ' + STR_FLD_RACKID + ' = '''+aRackID+''' AND ' +
        STR_SQL_TBL + '.' + '"' + STR_FLD_POS + '"' + ' = ' + IntToStr( aPos), aReadOnly);
end;

function TDisposableTipDataAdaptor.ReadRackID(): string;
begin
    result := ReadRackIDFromDataset(self.DataProvider);
end;

function TDisposableTipDataAdaptor.ReadPos(): integer;
begin
    result := ReadPosFromDataset(self.DataProvider);
end;

procedure TDisposableTipDataAdaptor.WriteRackID(const aRackID: string);
begin
    WriteRackIDToDataset(self.DataProvider, aRackID);
end;

procedure TDisposableTipDataAdaptor.WritePos(aPos: integer);
begin
    WritePosToDataset(self.DataProvider, aPos);
end;

procedure TDisposableTipDataAdaptor.InternInsertIfNotFound(const aRackID: string; aPos: integer);
begin
    self.SelectAndOpenByRackIDAndPos(aRackID, aPos, false);
    try
        if not self.DataProvider.IsEmpty then
            EXIT;
        self.DataProvider.Append;
        WriteRackID(aRackID);
        WritePos(aPos);
        self.DataProvider.Post;
    finally
        self.Close();
    end;
end;

function TDisposableTipDataAdaptor.EditOrAppendRec(const aKeyRackID: string; aKeyPos, aKeyStep: integer;
    const aSubstID: string; const aOrigin: string; aAmount: double; aUnit: integer; aColor: integer): boolean;
// if entry for RackId, Pos and Step already exists then edit it
// otherwise append a new entry
var
    xAppend: boolean;
begin
    result := false;
    if trim(aKeyRackID) = '' then
        EXIT;

    SelectAndOpenByRackIDAndPos(aKeyRackID, aKeyPos, false);
    try
        xAppend := self.DataProvider.IsEmpty;

        WriteDisposableTipDataToDataset(self.DataProvider, xAppend, true, aKeyRackID, aKeyPos);

        result := true;
    finally
        Close();
    end;

end;

procedure TDisposableTipDataAdaptor.AppendRec(aRackID: string; aPos, aStep: integer;
    aSubstID, aOrigin: string; aAmount: double; aUnitID: integer; aColor: integer);
begin
    SelectAndOpenAll(false);
    try
        WriteDisposableTipDataToDataset(self.DataProvider, true, true, aRackID, aPos);
    finally
        Close();
    end;
end;

procedure TDisposableTipDataAdaptor.DeletePositions(const aRackID: string; aLastPos: integer);
begin
    self.ExecSQLFmt('delete from DisposableTip where ' + STR_FLD_RACKID + ' = ''%s'' and ' + STR_SQL_TBL + '.'
        + '"' + STR_FLD_POS + '"' + ' <= %d', [aRackID, aLastPos]);
end;

function TDisposableTipDataAdaptor.GetPosFromRackID(const aRackID: string; out oPos: integer): boolean;
begin
    oPos := -1;
    SelectAndOpenByRackID(aRackID, true);
    try
        result := not self.DataProvider.IsEmpty;
        if not result then
            EXIT;
        oPos := ReadPos();
    finally
        Close();
    end;
end;

class procedure TDisposableTipDataAdaptor.WriteDisposableTipDataToDataset(aDataset: TDataProvider;
    aAppend: boolean; aWriteAmount: boolean; const aRackID: string; aPos: integer);
begin
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    WriteRackIDToDataset(aDataset, aRackID);
    WritePosToDataset(aDataset, aPos);

    aDataset.Post;
end;

class procedure TDisposableTipDataAdaptor.WriteDisposableTipRecToDataset(aDataset: TDataProvider;
    aDisposableTipRec: TDisposableTipRec; aAppend: boolean);
begin
    WriteDisposableTipDataToDataset(aDataset, aAppend, true, aDisposableTipRec.RackID, aDisposableTipRec.Pos)
end;

class function TDisposableTipDataAdaptor.ReadDisposableTipRecFromDataset(aDataset: TDataProvider)
    : TDisposableTipRec;
begin
    result.Valid := false;
    if aDataset.Eof then
        Exit;

    result := MakeDisposableTipRec(true, ReadRackIDFromDataset(aDataset), ReadPosFromDataset(aDataset));
end;

class function TDisposableTipDataAdaptor.ReadDisposableTipRecsFromDataset(aDataset: TDataProvider)
    : TDisposableTipRecs;
var
    i: integer;
begin
    if aDataset.IsEmpty then
        EXIT;
    SetLength(result, aDataset.RecordCount);
    i := 0;
    while not aDataset.Eof do
    begin
        result[i] := ReadDisposableTipRecFromDataset(aDataset);
        Inc(i);
        aDataset.Next;
    end;
end;

class function TDisposableTipDataAdaptor.MakeDisposableTipRec(aValid: boolean; aRackID: string; aPos: integer)
    : TDisposableTipRec;
begin
    result.Valid := aValid;
    result.RackID := aRackID;
    result.Pos := aPos;
end;

class function TDisposableTipDataAdaptor.ReadRackIDFromDataset(aDataset: TDataProvider): string;
begin
    result := aDataset.FieldByName(STR_FLD_RACKID).AsString;
end;

class function TDisposableTipDataAdaptor.ReadPosFromDataset(aDataset: TDataProvider): integer;
begin
    result := aDataset.FieldByName(STR_FLD_POS).AsInteger;
end;

class procedure TDisposableTipDataAdaptor.WriteRackIDToDataset(aDataset: TDataProvider;
    const aRackID: string);
begin
    aDataset.FieldByName(STR_FLD_RACKID).AsString := aRackID;
end;

class procedure TDisposableTipDataAdaptor.WritePosToDataset(aDataset: TDataProvider; aPos: integer);
begin
    aDataset.FieldByName(STR_FLD_POS).AsInteger := aPos;
end;


end.
