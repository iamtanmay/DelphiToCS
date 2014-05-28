{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  28.10.11 wl                                      TN5729   Ableger von POSINFO, nur für die Verwaltung von Substanz-Racks
  31.10.11 wl                                      TN5725   SUBSTCOLOR -> SubstanceData
  17.11.11 wl                                      TN5725   UNIT und DESCRIPTION weg
  26.11.11 wl                                      TN5725   MinVolume,MaxVolume: neu, werden aber noch nicht verwendet
  27.11.11 wl                                      TN5730   neu: MinVolume wird mit geladen
  09.12.11 wl  EditOrAppendRec                     TN5761   mit MinVol und MaxVol
  09.12.11 wl  DeletePos                           TN5761   neu für SubstanceSave
  10.09.12 wl                                      TN5979   neu: MinVolume2
  28.03.13 wl                                      TN6120   Setting 'AutomaticLoadSet' ersetzt festen Namen 'AUTOLOAD'
  ----------------------------------------------------------------------------------------------------------- }

unit SubstanceSetDataAdaptor;


interface


uses
    DataProvider,
    QueryDataAdaptor;

type
    TSubstanceSetRec = record
        Valid: boolean;
        SetName: string;
        RackID: string;
        Pos: integer;
        SubstID: string;
        Amount: double;
        MinVolume1: double;
        MinVolume2: double;
        MaxVolume: double;
    end;

    TSubstanceSetDataAdaptor = class(TQueryDataAdaptor)
    private const
        // SubstanceSet.DB:
        cTableName = 'SubstanceSet';
        cFieldNameSETNAME = 'SETNAME'; // ALPHA(20)
        cFieldNameRACKID = 'RACKID'; // ALPHA(20)
        cFieldNamePOS = 'POS'; // SHORT
        cFieldNameID = 'SUBSTID'; // ALPHA(30 oder mehr)
        cFieldNameLASTCHANGE = 'LASTCHANGE'; // TIMESTAMP
        cFieldNameAMOUNT = 'Amount'; // NUMBER
        cFieldNameLASTAMOUNT = 'LASTAMOUNT'; // NUMBER
        cFieldNameMinVolume1 = 'MinVolume';
        cFieldNameMinVolume2 = 'MinVolume2';
        cFieldNameMaxVolume = 'MaxVolume';

        STR_SQL_SELECT = 'SELECT * FROM ' + cTableName;
        STR_SQL_DELETE = 'DELETE FROM ' + cTableName;
        STR_SQL_WHERE_POS = cTableName + '."' + cFieldNamePOS + '"';
    private
        class procedure WriteSubstanceSetDataToDataset(aDataset: TDataProvider; aAppend: boolean;
            aWriteAmount: boolean; const aSetName, aRackID: string; aPos: integer; const aSubstID: string;
            aAmount, aMinVol1, aMinVol2, aMaxVol: double);
        class function ReadSetNameFromDataset(aDataset: TDataProvider): string;
        class function ReadRackIDFromDataset(aDataset: TDataProvider): string;
        class function ReadPosFromDataset(aDataset: TDataProvider): integer;
        class function ReadAmountFromDataset(aDataset: TDataProvider): double;
        class function ReadMinVolume1FromDataset(aDataset: TDataProvider): double;
        class function ReadMinVolume2FromDataset(aDataset: TDataProvider): double;
        class function ReadMaxVolumeFromDataset(aDataset: TDataProvider): double;
        class function ReadSubstIDFromDataset(aDataset: TDataProvider): string;
    protected
        function GetNameField(): string; override;
    public
        constructor Create();
        // public methods

        // TQuery-Based
        procedure SelectAndOpenAll(aReadOnly: boolean);
        procedure SelectAndOpenBySetName(const aSetName: string; aReadOnly: boolean = true);
        procedure SelectAndOpenBySetNameAndRackID(const aSetName, aRackID: string; aReadOnly: boolean = true);
        procedure SelectBySetNameAndRackIDAndSubstID(const aSetName, aRackID, aSubstID: string;
            aReadOnly: boolean = true);
        procedure SelectAndOpenBySetNameAndRackIDAndPos(const aSetName, aRackID: string; aPos: integer;
            aReadOnly: boolean);
        procedure SelectAndOpenBySetNameAndRackIDAndPosAndSubstID(const aSetName, aRackID: string;
            aPos: integer; const aSubstID: string; aReadOnly: boolean);

        function ReadSetName: string;
        function ReadRackID: string;
        function ReadPos(): integer;
        function ReadSubstID(): string;
        function ReadAmount(): double;

        procedure EditOrAppendRec(aRec: TSubstanceSetRec);
        procedure DeletePos(const aSetName, aRackID: string; aPos: integer);
        procedure AppendRec(const aSetName, aRackID: string; aPos: integer; aSubstID: string;
            aAmount, aMinVol1, aMinVol2, aMaxVol: double);
        function GetPosFromRackIDAndSubstID(const aSetName, aRackID, aSubstID: string;
            out oPos: integer): boolean;
        function ReadSubstanceSetRecsBySetName(const aSetName: string): TArray<TSubstanceSetRec>;

        class function MakeSubstanceSetRec(aValid: boolean; const aSetName, aRackID: string; aPos: integer;
            aSubstID: string; aAmount, aMinVolume1, aMinVolume2, aMaxVolume: double): TSubstanceSetRec;
        class procedure WriteSubstanceSetRecToDataset(aDataset: TDataProvider; aRec: TSubstanceSetRec;
            aAppend: boolean);
        class function ReadSubstanceSetRecFromDataset(aDataset: TDataProvider): TSubstanceSetRec;
        class function ReadSubstanceSetRecsFromDataset(aDataset: TDataProvider): TArray<TSubstanceSetRec>;
    end;


implementation


uses
    SysUtils;

{ TSubstanceSetDataAdaptor }

constructor TSubstanceSetDataAdaptor.Create();
begin
    inherited Create(cTableName);
end;

procedure TSubstanceSetDataAdaptor.DeletePos(const aSetName, aRackID: string; aPos: integer);
begin
    self.DataProvider.ExecSQL(STR_SQL_DELETE + ' WHERE SETNAME = ''' + aSetName + ''' AND ' + cFieldNameRACKID
        + ' = ''' + aRackID + ''' AND ' + STR_SQL_WHERE_POS + ' = ' + IntToStr(aPos));
end;

procedure TSubstanceSetDataAdaptor.SelectAndOpenAll(aReadOnly: boolean);
begin
    SelectAndOpen(STR_SQL_SELECT, aReadOnly);
end;

procedure TSubstanceSetDataAdaptor.SelectAndOpenBySetName(const aSetName: string; aReadOnly: boolean = true);
begin
    SelectAndOpen(STR_SQL_SELECT + ' WHERE SETNAME = ''' + aSetName + '''', aReadOnly);
end;

procedure TSubstanceSetDataAdaptor.SelectAndOpenBySetNameAndRackID(const aSetName, aRackID: string;
    aReadOnly: boolean = true);
begin
    SelectAndOpen(Format(STR_SQL_SELECT + ' WHERE SETNAME = ''' + aSetName + ''' AND ' + cFieldNameRACKID +
        ' = ''%s''', [aRackID]), aReadOnly);
end;

procedure TSubstanceSetDataAdaptor.SelectBySetNameAndRackIDAndSubstID(const aSetName, aRackID,
    aSubstID: string; aReadOnly: boolean = true);
begin
    SelectAndOpen(Format(STR_SQL_SELECT + ' WHERE SETNAME = ''' + aSetName + ''' AND ' + cFieldNameRACKID +
        ' = ''%s''' + ' AND ' + cFieldNameID + ' = ''%s''', [aRackID, aSubstID]), aReadOnly);

end;

procedure TSubstanceSetDataAdaptor.SelectAndOpenBySetNameAndRackIDAndPosAndSubstID(const aSetName,
    aRackID: string; aPos: integer; const aSubstID: string; aReadOnly: boolean);
begin
    SelectAndOpen(Format(STR_SQL_SELECT + ' WHERE SETNAME = ''' + aSetName + ''' AND ' + cFieldNameRACKID +
        ' = ''%s''' + ' AND ' + STR_SQL_WHERE_POS + ' = %d' + ' AND ' + cFieldNameID + ' = ''%s''',
        [aRackID, aPos, aSubstID]), aReadOnly);
end;

procedure TSubstanceSetDataAdaptor.SelectAndOpenBySetNameAndRackIDAndPos(const aSetName, aRackID: string;
    aPos: integer; aReadOnly: boolean);
begin
    SelectAndOpen(Format(STR_SQL_SELECT + ' WHERE SETNAME = ''' + aSetName + ''' AND ' + cFieldNameRACKID +
        ' = ''%s''' + ' AND ' + STR_SQL_WHERE_POS + ' = %d', [aRackID, aPos]), aReadOnly);
end;

function TSubstanceSetDataAdaptor.ReadSetName(): string;
begin
    result := ReadSetNameFromDataset(self.DataProvider);
end;

function TSubstanceSetDataAdaptor.ReadRackID(): string;
begin
    result := ReadRackIDFromDataset(self.DataProvider);
end;

function TSubstanceSetDataAdaptor.ReadPos(): integer;
begin
    result := ReadPosFromDataset(self.DataProvider);
end;

function TSubstanceSetDataAdaptor.ReadSubstID(): string;
begin
    result := ReadSubstIDFromDataset(self.DataProvider);
end;

function TSubstanceSetDataAdaptor.ReadAmount(): double;
begin
    result := ReadAmountFromDataset(self.DataProvider);
end;

function TSubstanceSetDataAdaptor.ReadSubstanceSetRecsBySetName(const aSetName: string)
    : TArray<TSubstanceSetRec>;
begin
    self.SelectAndOpenBySetName(aSetName);
    try
        result := ReadSubstanceSetRecsFromDataset(self.DataProvider);
    finally
        Close();
    end;
end;

procedure TSubstanceSetDataAdaptor.EditOrAppendRec(aRec: TSubstanceSetRec);
// if entry for RackId, Pos already exists then edit it
// otherwise append a new entry
var
    xAppend: boolean;
begin
    SelectAndOpenBySetNameAndRackIDAndPos(aRec.SetName, aRec.RackID, aRec.Pos, false);
    try
        xAppend := self.DataProvider.IsEmpty;

        self.WriteSubstanceSetRecToDataset(self.DataProvider, aRec, xAppend);
    finally
        Close();
    end;
end;

procedure TSubstanceSetDataAdaptor.AppendRec(const aSetName, aRackID: string; aPos: integer; aSubstID: string;
    aAmount, aMinVol1, aMinVol2, aMaxVol: double);
begin
    SelectAndOpenAll(false);
    try
        WriteSubstanceSetDataToDataset(self.DataProvider, true, true, aSetName, aRackID, aPos, aSubstID,
            aAmount, aMinVol1, aMinVol2, aMaxVol);
    finally
        Close();
    end;
end;

function TSubstanceSetDataAdaptor.GetNameField: string;
begin
    EXIT(cFieldNameSETNAME);
end;

function TSubstanceSetDataAdaptor.GetPosFromRackIDAndSubstID(const aSetName, aRackID, aSubstID: string;
    out oPos: integer): boolean;
begin
    oPos := -1;
    SelectBySetNameAndRackIDAndSubstID(aSetName, aRackID, aSubstID, true);
    try
        result := not self.DataProvider.IsEmpty;
        if not result then
            EXIT;
        oPos := ReadPos();
    finally
        Close();
    end;
end;

class procedure TSubstanceSetDataAdaptor.WriteSubstanceSetDataToDataset(aDataset: TDataProvider;
    aAppend: boolean; aWriteAmount: boolean; const aSetName, aRackID: string; aPos: integer;
    const aSubstID: string; aAmount, aMinVol1, aMinVol2, aMaxVol: double);
var
    xLastAmount: double;
begin
    xLastAmount := aAmount;
    if aAppend then
    begin
        aDataset.Append;
    end
    else
    begin
        aDataset.Edit;
        xLastAmount := aDataset.FieldByName(cFieldNameAMOUNT).AsFloat;
    end;

    aDataset.FieldByName(cFieldNameSETNAME).AsString := aSetName;
    aDataset.FieldByName(cFieldNameRACKID).AsString := aRackID;
    aDataset.FieldByName(cFieldNamePOS).AsInteger := aPos;
    aDataset.FieldByName(cFieldNameID).AsString := aSubstID;
    aDataset.FieldByName(cFieldNameMaxVolume).AsFloat := aMaxVol;
    aDataset.FieldByName(cFieldNameMinVolume1).AsFloat := aMinVol1;
    aDataset.FieldByName(cFieldNameMinVolume2).AsFloat := aMinVol2;

    aDataset.FieldByName(cFieldNameAMOUNT).AsFloat := aAmount;
    if (xLastAmount <> aAmount) then
    begin
        aDataset.FieldByName(cFieldNameLASTAMOUNT).AsFloat := xLastAmount;
        aDataset.FieldByName(cFieldNameLASTCHANGE).AsDateTime := Now;
    end;

    aDataset.Post;
end;

class procedure TSubstanceSetDataAdaptor.WriteSubstanceSetRecToDataset(aDataset: TDataProvider;
    aRec: TSubstanceSetRec; aAppend: boolean);
begin
    WriteSubstanceSetDataToDataset(aDataset, aAppend, true, aRec.SetName, aRec.RackID, aRec.Pos, aRec.SubstID,
        aRec.Amount, aRec.MinVolume1, aRec.MinVolume2, aRec.MaxVolume)
end;

class function TSubstanceSetDataAdaptor.ReadSubstanceSetRecFromDataset(aDataset: TDataProvider)
    : TSubstanceSetRec;
begin
    result.Valid := false;
    if aDataset.Eof then
        Exit;

    result := MakeSubstanceSetRec(true, ReadSetNameFromDataset(aDataset), ReadRackIDFromDataset(aDataset),
        ReadPosFromDataset(aDataset), ReadSubstIDFromDataset(aDataset), ReadAmountFromDataset(aDataset),
        ReadMinVolume1FromDataset(aDataset), ReadMinVolume2FromDataset(aDataset),
        ReadMaxVolumeFromDataset(aDataset));
end;

class function TSubstanceSetDataAdaptor.ReadSubstanceSetRecsFromDataset(aDataset: TDataProvider)
    : TArray<TSubstanceSetRec>;
var
    i: integer;
begin
    if aDataset.IsEmpty then
        EXIT;
    SetLength(result, aDataset.RecordCount);
    i := 0;
    while not aDataset.Eof do
    begin
        result[i] := ReadSubstanceSetRecFromDataset(aDataset);
        Inc(i);
        aDataset.Next;
    end;
end;

class function TSubstanceSetDataAdaptor.MakeSubstanceSetRec(aValid: boolean; const aSetName, aRackID: string;
    aPos: integer; aSubstID: string; aAmount, aMinVolume1, aMinVolume2, aMaxVolume: double): TSubstanceSetRec;
begin
    result.Valid := aValid;
    result.SetName := aSetName;
    result.RackID := aRackID;
    result.Pos := aPos;
    result.SubstID := aSubstID;
    result.Amount := aAmount;
    result.MinVolume1 := aMinVolume1;
    result.MinVolume2 := aMinVolume2;
    result.MaxVolume := aMaxVolume;
end;

class function TSubstanceSetDataAdaptor.ReadSetNameFromDataset(aDataset: TDataProvider): string;
begin
    result := aDataset.FieldByName(cFieldNameSETNAME).AsString;
end;

class function TSubstanceSetDataAdaptor.ReadRackIDFromDataset(aDataset: TDataProvider): string;
begin
    result := aDataset.FieldByName(cFieldNameRACKID).AsString;
end;

class function TSubstanceSetDataAdaptor.ReadAmountFromDataset(aDataset: TDataProvider): double;
begin
    result := aDataset.FieldByName(cFieldNameAMOUNT).AsFloat;
end;

class function TSubstanceSetDataAdaptor.ReadMaxVolumeFromDataset(aDataset: TDataProvider): double;
begin
    result := aDataset.FieldByName(cFieldNameMaxVolume).AsFloat;
end;

class function TSubstanceSetDataAdaptor.ReadMinVolume1FromDataset(aDataset: TDataProvider): double;
begin
    result := aDataset.FieldByName(cFieldNameMinVolume1).AsFloat;
end;

class function TSubstanceSetDataAdaptor.ReadMinVolume2FromDataset(aDataset: TDataProvider): double;
begin
    result := aDataset.FieldByName(cFieldNameMinVolume2).AsFloat;
end;

class function TSubstanceSetDataAdaptor.ReadPosFromDataset(aDataset: TDataProvider): integer;
begin
    result := aDataset.FieldByName(cFieldNamePOS).AsInteger;
end;

class function TSubstanceSetDataAdaptor.ReadSubstIDFromDataset(aDataset: TDataProvider): string;
begin
    result := aDataset.FieldByName(cFieldNameID).AsString;
end;


end.
