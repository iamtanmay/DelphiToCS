unit SeqenceDataAdaptor;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.05.04 wl                               TN1945   initial version
  08.11.05 wl                               TN2745   uses DM_Sampl statt DBScrip
  27.11.06 wl  StoreGridData                TN3411   benutzt eigene Tabelle statt DM_Sampl
  09.11.07 pk  GetNames                     TN3922   function commented out because QryTools2 removed
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  18.06.10 pk                               TN5152.1 New SelectAndOpenBySeqName
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Classes,
    Grids,
    GeneralTypes,
    DataProvider;

type
    TSequenceRec = record
        SeqName: string;
        ProductNo: integer;
        Level: integer;
        SubstID: string;
        Volume: extended;

    end;

    TSeqenceDataAdaptor = class
    private
        class procedure WriteRecToDataset(aDataset: TDataProvider; const aRec: TSequenceRec;
            aAppend: boolean);
    public
        class procedure Delete(aDataset: TDataProvider; const aSeqName: string; const aProductNo: integer;
            const aLevel: integer);
        class procedure AppendOrEdit(aDataset: TDataProvider; const aRec: TSequenceRec);
        class function NameExists(aName: string): boolean;
        class function ReadAllNames(): TStringArray;
        class procedure StoreGridData(aSeqName: string; aGrid: TStringGrid; aVolume: double;
            aRows, aCols: integer);
        class procedure SelectAndOpenBySeqName(const aDataset: TDataProvider; const aSeqName: string;
            const aReadOnly: boolean);
        class procedure SelectAndOpenBySeqNameProdLevel(const aDataset: TDataProvider; const aSeqName: string;
            const aProductNo: integer; const aLevel: integer; const aReadOnly: boolean);
    end;


    // ##################################################################################################


implementation


uses
    Forms,
    Controls,
    SysUtils,
    AppSettings,
    DataProviderFactory;

const
    TABLE_SEQUENCE = 'SEQUENCE';
    SEQUENCE_FLD_SEQNAME = 'SEQNAME';
    SEQUENCE_FLD_PRODUCTNO = 'PRODUCTNO';
    SEQUENCE_FLD_LEVEL = 'LEVEL'; // level is an SQL keyword so we need: Sequence."Level"
    SEQUENCE_FLD_SUBSTID = 'SUBSTID';
    SEQUENCE_FLD_VOLUME = 'VOLUME';

    cSQLFieldLevel = TABLE_SEQUENCE + '.' + '"LEVEL"'; // level is an SQL keyword so we need: Sequence."Level"

    cSQLSelect = 'SELECT * FROM ' + TABLE_SEQUENCE;
    cSQLWhereSeqFmt = SEQUENCE_FLD_SEQNAME + ' = ''%s''';
    cSQLWhereProdFmt = SEQUENCE_FLD_PRODUCTNO + ' = %d';
    cSQLWhereLevelFmt = cSQLFieldLevel + ' = %d';
    cSQLDelete = 'DELETE FROM ' + TABLE_SEQUENCE;

    cSQLWhereSeqProdLevel = cSQLWhereSeqFmt + ' AND ' + cSQLWhereProdFmt + ' AND ' + cSQLWhereLevelFmt;

class procedure TSeqenceDataAdaptor.Delete(aDataset: TDataProvider; const aSeqName: string;
    const aProductNo, aLevel: integer);
begin
    aDataset.ExecSQL(Format(cSQLDelete + ' WHERE ' + cSQLWhereSeqProdLevel, [aSeqName, aProductNo, aLevel]));
end;

class function TSeqenceDataAdaptor.NameExists(aName: string): boolean;
var
    xNames: TStringArray;
    x: integer;
begin
    result := false;
    xNames := ReadAllNames();

    for x := 0 to Length(xNames) - 1 do
    begin
        if (xNames[x] = aName) then
        begin
            result := true;
            exit;
        end;
    end;
end;

class function TSeqenceDataAdaptor.ReadAllNames(): TStringArray;
var
    xLastCursor: TCursor;
begin
    xLastCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
    { TODO -oPK : GetField not accessible }
    SetLength(result, 0);
    // GetField(aResultList, TAppSettings.Alias, TABLE_SEQUENCE, SEQUENCE_FLD_SEQNAME, true);
    Screen.Cursor := xLastCursor;
end;

class procedure TSeqenceDataAdaptor.WriteRecToDataset(aDataset: TDataProvider; const aRec: TSequenceRec;
    aAppend: boolean);
begin
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    aDataset.FieldByName(SEQUENCE_FLD_SEQNAME).AsString := aRec.SeqName;
    aDataset.FieldByName(SEQUENCE_FLD_PRODUCTNO).AsInteger := aRec.ProductNo;
    aDataset.FieldByName(SEQUENCE_FLD_LEVEL).AsInteger := aRec.Level;
    aDataset.FieldByName(SEQUENCE_FLD_SUBSTID).AsString := aRec.SubstID;
    aDataset.FieldByName(SEQUENCE_FLD_VOLUME).AsFloat := aRec.Volume;

    aDataset.Post;
end;

class procedure TSeqenceDataAdaptor.SelectAndOpenBySeqName(const aDataset: TDataProvider;
    const aSeqName: string; const aReadOnly: boolean);
begin
    aDataset.SelectAndOpen(Format(cSQLSelect + ' WHERE ' + cSQLWhereSeqFmt, [aSeqName]), aReadOnly);
end;

class procedure TSeqenceDataAdaptor.SelectAndOpenBySeqNameProdLevel(const aDataset: TDataProvider;
    const aSeqName: string; const aProductNo, aLevel: integer; const aReadOnly: boolean);
begin
    aDataset.SelectAndOpen(Format(cSQLSelect + ' WHERE ' + cSQLWhereSeqProdLevel,
        [aSeqName, aProductNo, aLevel]), aReadOnly);
end;

class procedure TSeqenceDataAdaptor.AppendOrEdit(aDataset: TDataProvider; const aRec: TSequenceRec);
var
    xAppend: boolean;
begin
    SelectAndOpenBySeqNameProdLevel(aDataset, aRec.SeqName, aRec.ProductNo, aRec.Level, false);
    try
        xAppend := aDataset.IsEmpty;
        WriteRecToDataset(aDataset, aRec, xAppend);
    finally
        aDataset.Close;
    end;
end;

class procedure TSeqenceDataAdaptor.StoreGridData(aSeqName: string; aGrid: TStringGrid; aVolume: double;
    aRows, aCols: integer);
var
    iLevel, iPos: integer;
    xSeqTable: TDataProvider;
    xRec: TSequenceRec;
begin
    // diese Funktion ist noch h‰ﬂlich und muﬂ mit TRBSeqEd.UpdateValues vereinigt werden

    xSeqTable := TDataProviderFactory.Instance.CreateDataProvider();
    try
        Screen.Cursor := crHourglass;
        for iLevel := 1 to aGrid.ColCount - 1 do
        begin
            for iPos := 1 to aGrid.RowCount - 1 do
            begin
                // ------------------------------------------ Tabellenfeld mit Subst-ID und Volumen beschreiben
                xRec.SeqName := aSeqName;
                xRec.ProductNo := iPos;
                xRec.Level := iLevel;
                xRec.SubstID := aGrid.Cells[iLevel, iPos];
                xRec.Volume := aVolume;

                if (xRec.SubstID <> '') then
                begin
                    AppendOrEdit(xSeqTable, xRec);
                end
                else
                begin
                    Delete(xSeqTable, xRec.SeqName, xRec.ProductNo, xRec.Level);
                end;
            end;
        end;

        xRec.SeqName := aSeqName;
        xRec.ProductNo := 0;
        xRec.Level := 0;
        xRec.SubstID := Format('Format: %dx%d', [aCols, aRows]);
        xRec.Volume := 0;

        // --------------------------------------------------------------------- Format des Reaktionsblocks
        AppendOrEdit(xSeqTable, xRec);

        Screen.Cursor := crDefault;

    finally
        FreeAndNil(xSeqTable);
    end;
end;


end.
