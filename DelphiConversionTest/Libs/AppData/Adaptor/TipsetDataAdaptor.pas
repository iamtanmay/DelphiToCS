{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Data adaptor for Tipset.db
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  30.08.07 pk                               TN3840.1 New
  02.10.07 wl                               TN3811.5 benutzt self.Query statt fQuery
  09.11.07 pk                               TN3921   Changes for updatemanager
  09.11.07 pk                               TN3922   Dataset changed to DataProvider
  03.07.08 wl                               TN4157
  17.09.08 wl                               TN4224   mit TipsetFieldNames vereinigt
  17.09.08 wl  ReadTipsetTip                TN4224   liest einen bestimmten Datensatz
  16.01.09 wl                               TN4362   an Änderungen in TQueryDataAdaptor angepasst
  04.15.10 pk                               TN5050   New: SelectAndOpenTipsetByDeviceName
  07.06.10 pk  ReadRecs                     TN5077   New
  17.06.10 pk                               TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  08.02.11 wl  Instance                     TN5475   entfernt
  13.11.12 wl  GetKeyFields                 TN6015   kann entfernt werden, wenn Key = NameField
  -------------------------------------------------------------------------------------------------- }

unit TipsetDataAdaptor;


interface


uses
    Classes,
    DataProvider,
    GeneralTypes,
    QueryDataAdaptor,
    CommonTypes;

type
    TTipsetRec = record
        Tipset: string;
        PipDeviceName: string;
        TipIndex: integer;
        TipType: string;
    end;

    TTipsetRecArray = array of TTipsetRec;

    TTipsetDataAdaptor = class(TQueryDataAdaptor)
    private const
        STR_TIPSET_TBL = 'TIPSET';
        STR_TIPSET_FLD_LAYOUT = 'LAYOUT';
        STR_TIPSET_FLD_PIPDEVNAME = 'PIPDEVICENAME';
        STR_TIPSET_FLD_TIP = 'TIP';
        STR_TIPSET_FLD_TIPTYPE = 'TIPTYPE';

        INT_TIPSET_FLDLEN_LAYOUT = 20;
        INT_TIPSET_FLDLEN_TIPTYPE = 20;

        STR_TIPSET_INDEX_FIELDS = STR_TIPSET_FLD_LAYOUT + ';' + STR_TIPSET_FLD_TIP;
        STR_SQL_WHERE_TIPSET = ' WHERE ' + STR_TIPSET_FLD_LAYOUT + ' =''%s''';
        STR_SQL_FROM = ' FROM ' + STR_TIPSET_TBL;
        STR_SQL_DELETE_FMT = 'DELETE ' + STR_SQL_FROM + STR_SQL_WHERE_TIPSET;
        STR_SQL_SELECT_TIPSET_FMT = 'SELECT * ' + STR_SQL_FROM + STR_SQL_WHERE_TIPSET;
        STR_SQL_SELECT_ALL = 'SELECT * ' + STR_SQL_FROM;

    protected
        function GetNameField(): string; override;
    public
        constructor Create();

        procedure SelectAndOpenAll(aReadOnly: boolean);
        procedure SelectAndOpenTipset(const aTipset: string; aReadOnly: boolean);
        procedure SelectAndOpenTipsetTip(const aTipset, aPipDeviceName: string; aTip: integer;
            aReadOnly: boolean);
        procedure SelectAndOpenTipsetByDeviceName(const aTipset, aPipDeviceName: string; aReadOnly: boolean);
        procedure DeleteTipset(const aTipset: string);
        procedure WriteRecs(const aRecs: TTipsetRecArray);
        procedure ReadTipsetTip(const aTipset, aPipDeviceName: string; aTip: integer; out oRec: TTipsetRec);
        procedure ReadRecs(out oRecs: TTipsetRecArray);
        procedure AddTipset(const aTipset, aPipDeviceName: string; aTipIndex: integer;
            const aTipType: string);
        class procedure ReadRecAtCursor(aDataset: TDataProvider; var aRec: TTipsetRec);
        class procedure ReadRecsAtCursor(aDataset: TDataProvider; var aRecs: TTipsetRecArray);
        class procedure WriteRecsAtCursor(aDataset: TDataProvider; const aRecs: TTipsetRecArray);
        class procedure WriteRecAtCursor(aDataset: TDataProvider; const aRec: TTipsetRec; aAppend: boolean);
    end;


implementation


uses
    Sysutils;

{ TTipsetDataAdaptor }

constructor TTipsetDataAdaptor.Create;
begin
    inherited Create(STR_TIPSET_TBL);
end;

function TTipsetDataAdaptor.GetNameField(): string;
begin
    result := STR_TIPSET_FLD_LAYOUT;
end;

procedure TTipsetDataAdaptor.DeleteTipset(const aTipset: string);
begin
    self.ExecSQL(Format(STR_SQL_DELETE_FMT, [aTipset]));
end;

procedure TTipsetDataAdaptor.SelectAndOpenAll(aReadOnly: boolean);
begin
    SelectAndOpen(STR_SQL_SELECT_ALL, aReadOnly)
end;

procedure TTipsetDataAdaptor.SelectAndOpenTipsetTip(const aTipset, aPipDeviceName: string; aTip: integer;
    aReadOnly: boolean);
begin
    ASSERT(aTipset <> '', 'Tipset name is empty');
    SelectAndOpenFmt('SELECT * FROM %s WHERE %s = ''%s'' AND %s = ''%s'' AND %s = %d',
        [STR_TIPSET_TBL, STR_TIPSET_FLD_LAYOUT, aTipset, STR_TIPSET_FLD_PIPDEVNAME, aPipDeviceName,
        STR_TIPSET_FLD_TIP, aTip], aReadOnly);
end;

procedure TTipsetDataAdaptor.SelectAndOpenTipsetByDeviceName(const aTipset, aPipDeviceName: string;
    aReadOnly: boolean);
begin
    ASSERT(aTipset <> '', 'Tipset name is empty');
    SelectAndOpenFmt('SELECT * FROM %s WHERE %s = ''%s'' AND %s = ''%s''',
        [STR_TIPSET_TBL, STR_TIPSET_FLD_LAYOUT, aTipset, STR_TIPSET_FLD_PIPDEVNAME, aPipDeviceName],
        aReadOnly);
end;

procedure TTipsetDataAdaptor.SelectAndOpenTipset(const aTipset: string; aReadOnly: boolean);
begin
    ASSERT(aTipset <> '', 'Tipset name is empty');
    SelectAndOpen(Format(STR_SQL_SELECT_TIPSET_FMT, [aTipset]), aReadOnly);
end;

procedure TTipsetDataAdaptor.WriteRecs(const aRecs: TTipsetRecArray);
begin
    SelectAndOpenAll(false);
    try
        WriteRecsAtCursor(self.DataProvider, aRecs);
    finally
        Close();
    end;
end;

procedure TTipsetDataAdaptor.ReadTipsetTip(const aTipset, aPipDeviceName: string; aTip: integer;
    out oRec: TTipsetRec);
begin
    SelectAndOpenTipsetTip(aTipset, aPipDeviceName, aTip, true);
    try
        ReadRecAtCursor(self.DataProvider, oRec);
    finally
        Close();
    end;
end;

procedure TTipsetDataAdaptor.ReadRecs(out oRecs: TTipsetRecArray);
begin
    self.SelectAndOpenAll(true);
    try
        ReadRecsAtCursor(self.DataProvider, oRecs);
    finally
        Close();
    end;
end;

procedure TTipsetDataAdaptor.AddTipset(const aTipset, aPipDeviceName: string; aTipIndex: integer;
    const aTipType: string);
var
    xTipsetRec: TTipsetRec;
begin
    SelectAndOpenAll(false);
    try
        xTipsetRec.Tipset := aTipset;
        xTipsetRec.PipDeviceName := aPipDeviceName;
        xTipsetRec.TipIndex := aTipIndex;
        xTipsetRec.TipType := aTipType;
        WriteRecAtCursor(self.DataProvider, xTipsetRec, true);
    finally
        Close();
    end;
end;

class procedure TTipsetDataAdaptor.ReadRecAtCursor(aDataset: TDataProvider; var aRec: TTipsetRec);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');

    aRec.TipSet := aDataset.FieldByName(STR_TIPSET_FLD_LAYOUT).AsString;
    aRec.PipDeviceName := aDataset.FieldByName(STR_TIPSET_FLD_PIPDEVNAME).AsString;
    aRec.TipIndex := aDataset.FieldByName(STR_TIPSET_FLD_TIP).AsInteger;
    aRec.TipType := aDataset.FieldByName(STR_TIPSET_FLD_TIPTYPE).AsString;
end;

class procedure TTipsetDataAdaptor.ReadRecsAtCursor(aDataset: TDataProvider; var aRecs: TTipsetRecArray);
var
    i: integer;
begin
    i := 0;
    SetLength(aRecs, aDataset.RecordCount);
    while not aDataset.Eof do
    begin
        ReadRecAtCursor(aDataset, aRecs[i]);
        aDataset.Next;
        Inc(i);
    end;

end;

class procedure TTipsetDataAdaptor.WriteRecAtCursor(aDataset: TDataProvider; const aRec: TTipsetRec;
    aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    aDataset.FieldByName(STR_TIPSET_FLD_LAYOUT).AsString := aRec.Tipset;
    aDataset.FieldByName(STR_TIPSET_FLD_PIPDEVNAME).AsString := aRec.PipDeviceName;
    aDataset.FieldByName(STR_TIPSET_FLD_TIP).AsInteger := aRec.TipIndex;
    aDataset.FieldByName(STR_TIPSET_FLD_TIPTYPE).AsString := aRec.TipType;

    aDataset.Post;
end;

class procedure TTipsetDataAdaptor.WriteRecsAtCursor(aDataset: TDataProvider; const aRecs: TTipsetRecArray);
var
    i: integer;
begin
    for i := 0 to high(aRecs) do
    begin
        WriteRecAtCursor(aDataset, aRecs[i], true);
    end;
end;


end.
