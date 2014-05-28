{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Data adaptor for Sequence.db
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  18.01.07 pk                               TN3482   New
  05.02.07 pk                               TN3544   Changes for updatemanager
  07.08.07 wl  ReadAllItemNames             TN3811.3 neu
  02.10.07 wl                               TN3811.5 benutzt self.Query statt fQuery
  09.11.07 pk                               TN3921   Changes for updatemanager
  09.11.07 pk                               TN3922   Dataset changed to DataProvider
  23.09.08 wl                               TN4236   Vereinigung mit ..Fieldnames
  16.01.09 wl                               TN4362   an Änderungen in TQueryDataAdaptor angepasst
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  17.06.10 pk                               TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  20.06.11 wl                               TN5603   Neue experimentelle Version
  -------------------------------------------------------------------------------------------------- }

unit SequenceDataAdaptor;


interface


uses
    GeneralTypes,
    DataProvider,
    QueryDataAdaptor;

const
    INT_SEQUENCE_LEVEL_INVALID = 0;

type
    TSequenceRec = record
        Valid: boolean;
        SeqName: string;
        Level: integer;
        Position: integer;
        SeqTag: string;
        SubstID: string;
        Amount: double;
        AmountUnit: string;
        ValBool1: boolean;
        ValInt1: integer;
        ValFloat1: double;
        ValFloat2: double;
        ValFloat3: double;
        ValString1: string;
    end;

    TSequenceDataAdaptor = class(TQueryDataAdaptor)
    private
        class procedure WriteRecAtCursor(aDataset: TDataProvider; const aRec: TSequenceRec; aAppend: boolean);
        class procedure WriteRecsAtCursor(aDataset: TDataProvider; const aRecs: TArray<TSequenceRec>);
    protected
        function GetNameField: string; override;
    public
        constructor Create();

        class function ReadRecAtCursor(aDataset: TDataProvider): TSequenceRec;
        class function ReadRecsAtCursor(aDataset: TDataProvider): TArray<TSequenceRec>;
        function ReadSequence(const aSequence: string): TArray<TSequenceRec>;
        function ReadSequenceAtLevel(const aSequence: string; aLevel: integer): TArray<TSequenceRec>;
        function ReadSequenceAtLevelAndPosition(const aSequence: string; aLevel: integer; aPosition: integer)
            : TSequenceRec;
        procedure WriteRecs(const aRecs: TArray<TSequenceRec>);
        class function InstReadAllNames(): TStringArray;
    end;


implementation


uses
    SysUtils;

const
    STR_SEQUENCE_TBL = 'SEQUENCE';

    STR_SEQUENCE_FLD_SEQNAME = 'SEQNAME';
    STR_SEQUENCE_FLD_LEVEL = 'LEVEL';
    STR_SEQUENCE_FLD_RACKPOS = 'RACKPOS';
    STR_SEQUENCE_FLD_SEQTAG = 'SEQTAG';
    STR_SEQUENCE_FLD_SUBSTID = 'SUBSTID';
    STR_SEQUENCE_FLD_AMOUNT = 'AMOUNT';
    STR_SEQUENCE_FLD_AMOUNTUNIT = 'AMOUNTUNIT';
    STR_SEQUENCE_FLD_VAL_INT1 = 'VAL_INT1';
    STR_SEQUENCE_FLD_VAL_BOOL1 = 'VAL_BOOL1';
    STR_SEQUENCE_FLD_VAL_FLOAT1 = 'VAL_FLOAT1';
    STR_SEQUENCE_FLD_VAL_FLOAT2 = 'VAL_FLOAT2';
    STR_SEQUENCE_FLD_VAL_FLOAT3 = 'VAL_FLOAT3';
    STR_SEQUENCE_FLD_VAL_STRING1 = 'VAL_STRING1';

    INT_SEQUENCE_FLDLEN_SEQNAME = 20;
    INT_SEQUENCE_FLDLEN_SUBSTID = 20;

    STR_SQL_FLD_LEVEL = 's.''' + STR_SEQUENCE_FLD_LEVEL + '''';
    STR_SQL_WHERE_SEQUENCE = ' WHERE ' + STR_SEQUENCE_FLD_SEQNAME + ' =''%s''';
    STR_SQL_FROM = ' FROM ' + STR_SEQUENCE_TBL + ' s';

    { TSequenceDataAdaptor }

constructor TSequenceDataAdaptor.Create;
begin
    inherited Create(STR_SEQUENCE_TBL);
end;

function TSequenceDataAdaptor.GetNameField(): string;
begin
    result := STR_SEQUENCE_FLD_SEQNAME;
end;

class procedure TSequenceDataAdaptor.WriteRecAtCursor(aDataset: TDataProvider; const aRec: TSequenceRec;
    aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');

    if not aRec.Valid then
        EXIT;

    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    aDataset.FieldByName(STR_SEQUENCE_FLD_SEQNAME).AsString := aRec.SeqName;
    aDataset.FieldByName(STR_SEQUENCE_FLD_LEVEL).AsInteger := aRec.Level;
    aDataset.FieldByName(STR_SEQUENCE_FLD_RACKPOS).AsInteger := aRec.Position;
    aDataset.FieldByName(STR_SEQUENCE_FLD_SEQTAG).AsString := aRec.SeqTag;
    aDataset.FieldByName(STR_SEQUENCE_FLD_SUBSTID).AsString := aRec.SubstID;
    aDataset.FieldByName(STR_SEQUENCE_FLD_AMOUNT).AsFloat := aRec.Amount;
    aDataset.FieldByName(STR_SEQUENCE_FLD_AMOUNTUNIT).AsString := aRec.AmountUnit;
    aDataset.FieldByName(STR_SEQUENCE_FLD_VAL_INT1).AsInteger := aRec.ValInt1;
    aDataset.FieldByName(STR_SEQUENCE_FLD_VAL_BOOL1).AsBoolean := aRec.ValBool1;
    aDataset.FieldByName(STR_SEQUENCE_FLD_VAL_STRING1).AsString := aRec.ValString1;
    aDataset.FieldByName(STR_SEQUENCE_FLD_VAL_FLOAT1).AsFloat := aRec.ValFloat1;
    aDataset.FieldByName(STR_SEQUENCE_FLD_VAL_FLOAT2).AsFloat := aRec.ValFloat2;
    aDataset.FieldByName(STR_SEQUENCE_FLD_VAL_FLOAT3).AsFloat := aRec.ValFloat3;

    aDataset.Post;
end;

class procedure TSequenceDataAdaptor.WriteRecsAtCursor(aDataset: TDataProvider;
    const aRecs: TArray<TSequenceRec>);
var
    i: integer;
begin
    for i := 0 to high(aRecs) do
    begin
        WriteRecAtCursor(aDataset, aRecs[i], true);
    end;
end;

procedure TSequenceDataAdaptor.WriteRecs(const aRecs: TArray<TSequenceRec>);
begin
    SelectAndOpenAll(false);
    try
        WriteRecsAtCursor(self.DataProvider, aRecs);
    finally
        Close();
    end;
end;

function TSequenceDataAdaptor.ReadSequence(const aSequence: string): TArray<TSequenceRec>;
begin
    SelectAndOpenByName(aSequence, true);
    try
        result := ReadRecsAtCursor(self.DataProvider);
    finally
        Close();
    end;
end;

function TSequenceDataAdaptor.ReadSequenceAtLevel(const aSequence: string; aLevel: integer)
    : TArray<TSequenceRec>;
begin
    ASSERT(aSequence <> '', 'Sequence name is empty');
    SelectAndOpen(Format('SELECT * ' + STR_SQL_FROM + STR_SQL_WHERE_SEQUENCE + ' AND ' + STR_SQL_FLD_LEVEL +
        ' = %d', [aSequence, aLevel]), true);
    try
        result := ReadRecsAtCursor(self.DataProvider);
    finally
        Close();
    end;
end;

function TSequenceDataAdaptor.ReadSequenceAtLevelAndPosition(const aSequence: string;
    aLevel, aPosition: integer): TSequenceRec;
begin
    ASSERT(aSequence <> '', 'Sequence name is empty');
    SelectAndOpen(Format('SELECT * ' + STR_SQL_FROM + STR_SQL_WHERE_SEQUENCE + ' AND ' + STR_SQL_FLD_LEVEL +
        ' = %d' + ' AND ' + STR_SEQUENCE_FLD_RACKPOS + ' = %d', [aSequence, aLevel, aPosition]), true);
    try
        result := ReadRecAtCursor(self.DataProvider);
    finally
        Close();
    end;
end;

class function TSequenceDataAdaptor.ReadRecAtCursor(aDataset: TDataProvider): TSequenceRec;
begin
    ASSERT(aDataSet.Active, 'Dataset not active');

    result.Valid := true;
    result.SeqName := aDataset.FieldByName(STR_SEQUENCE_FLD_SEQNAME).AsString;
    result.Level := aDataset.FieldByName(STR_SEQUENCE_FLD_LEVEL).AsInteger;
    result.Position := aDataset.FieldByName(STR_SEQUENCE_FLD_RACKPOS).AsInteger;
    result.SeqTag := aDataset.FieldByName(STR_SEQUENCE_FLD_SEQTAG).AsString;
    result.SubstID := aDataset.FieldByName(STR_SEQUENCE_FLD_SUBSTID).AsString;
    result.Amount := aDataset.FieldByName(STR_SEQUENCE_FLD_AMOUNT).AsFloat;
    result.AmountUnit := aDataset.FieldByName(STR_SEQUENCE_FLD_AMOUNTUNIT).AsString;
    result.ValInt1 := aDataset.FieldByName(STR_SEQUENCE_FLD_VAL_INT1).AsInteger;
    result.ValBool1 := aDataset.FieldByName(STR_SEQUENCE_FLD_VAL_BOOL1).AsBoolean;
    result.ValString1 := aDataset.FieldByName(STR_SEQUENCE_FLD_VAL_STRING1).AsString;
    result.ValFloat1 := aDataset.FieldByName(STR_SEQUENCE_FLD_VAL_FLOAT1).AsFloat;
    result.ValFloat2 := aDataset.FieldByName(STR_SEQUENCE_FLD_VAL_FLOAT2).AsFloat;
    result.ValFloat3 := aDataset.FieldByName(STR_SEQUENCE_FLD_VAL_FLOAT3).AsFloat;
end;

class function TSequenceDataAdaptor.ReadRecsAtCursor(aDataset: TDataProvider): TArray<TSequenceRec>;
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

class function TSequenceDataAdaptor.InstReadAllNames(): TStringArray;
var
    xDA: TSequenceDataAdaptor;
begin
    xDA := TSequenceDataAdaptor.Create;
    try
        result := xDA.ReadAllNames();
    finally
        FreeAndNil(xDA);
    end;
end;


end.
