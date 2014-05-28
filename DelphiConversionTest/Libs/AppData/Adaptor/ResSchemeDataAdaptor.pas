{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Data Adaptor for the Resource Scheme Table (ResScheme)
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  02.02.05 pk                                TN2302.1 New
  01.07.05 pk  InstallTable                  TN2487   Do not call CreateTable
  11.01.06 pk  InstallTable                  TN2871.1 override
  05.02.07 pk                                TN3544   Changes for updatemanager
  07.08.07 wl  Instance                      TN3811.3 entfernt
  02.10.07 wl                                TN3811.5 benutzt self.Query statt fQuery
  09.11.07 pk                                TN3921   Changes for updatemanager
  09.11.07 pk                                TN3922   Dataset changed to DataProvider
  23.09.08 wl                                TN4236   Vereinigung mit ..Fieldnames
  17.11.08 pk                                TN4280   New ReadSchemes
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  10.03.11 wl  ReadSchemeIDsWithResourceNames TN5499  neu: für Darstellung in Action Parametern
  27.03.13 wl                                TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit ResSchemeDataAdaptor;


interface


uses
    DataProvider,
    QueryDataAdaptor;

type
    TResSchemeRec = record
        SchemeID: integer;
        ResID: string;
        UseLevel: integer;
        ReleaseLevel: integer;
    end;

    TResSchemeDataAdaptor = class(TQueryDataAdaptor)
    private
        procedure SelectAndOpenByID(aSchemeID: integer; aReadOnly: boolean);
    protected
        function GetNameField(): string; override;
        function GetKeyFields(): TArray<string>; override;
    public
        constructor Create();

        function ReadSchemeByID(aID: integer): TArray<TResSchemeRec>;
        function ReadSchemes(): TArray<TResSchemeRec>;
        function ReadSchemeIDsWithResourceNames(const aDelimiter1, aDelimiter2: string): TArray<string>;
        function GetResourceNames(const aSchemeIDAsText, aDelimiter: string): string;

        class function ReadRecsAtCursor(aDataset: TDataProvider): TArray<TResSchemeRec>;
        class function ReadRecAtCursor(aDataset: TDataProvider): TResSchemeRec;
    end;


implementation


uses
    SysUtils;

const
    STR_RESSCHEME_TBL = 'RESSCHEME';

    STR_RESSCHEME_FLD_ID = 'SCHEMEID';
    STR_RESSCHEME_FLD_RESID = 'RESID';
    STR_RESSCHEME_FLD_USELEVEL = 'USELEVEL';
    STR_RESSCHEME_FLD_RELEASELEVEL = 'RELLEVLEL';

    { INT_RESSCHEME_FLDLEN_RESID          = 20;

      STR_RESSCHEME_INDEX_FIELDS          =  STR_RESSCHEME_FLD_ID + ';'
      + STR_RESSCHEME_FLD_RESID + ';'
      + STR_RESSCHEME_FLD_USELEVEL + ';'
      + STR_RESSCHEME_FLD_RELEASELEVEL;
    }

    { TResSchemeDataAdaptor }

function TResSchemeDataAdaptor.GetResourceNames(const aSchemeIDAsText, aDelimiter: string): string;
var
    xScheme: TArray<TResSchemeRec>;
    xText: string;
    x: integer;
    xSchemeID: integer;
begin
    // Ressource-Namen als Kommentare hinzufügen
    xSchemeID := StrToIntDef(aSchemeIDAsText, -1);
    if (xSchemeID < 0) then
        EXIT('');

    xScheme := ReadSchemeByID(xSchemeID);
    xText := '';
    for x := 0 to high(xScheme) do
    begin
        xText := xText + xScheme[x].ResID;
        if (x < high(xScheme)) then
            xText := xText + aDelimiter; // Delimiter sollte Komma sein (für Aufzählung der Namen)
    end;
    result := xText;
end;

constructor TResSchemeDataAdaptor.Create;
begin
    inherited Create(STR_RESSCHEME_TBL);
end;

procedure TResSchemeDataAdaptor.SelectAndOpenByID(aSchemeID: integer; aReadOnly: boolean);
var
    xSQL: string;
begin
    xSQL := 'SELECT * FROM ' + STR_RESSCHEME_TBL + ' WHERE ' + STR_RESSCHEME_FLD_ID + ' = ' +
        IntToStr(aSchemeID);
    SelectAndOpen(xSQL, aReadOnly);
end;

function TResSchemeDataAdaptor.ReadSchemeByID(aID: integer): TArray<TResSchemeRec>;
var
    i: integer;
begin
    self.SelectAndOpenByID(aID, true);
    SetLength(result, self.DataProvider.RecordCount);
    i := 0;
    while not self.DataProvider.Eof do
    begin
        result[i] := ReadRecAtCursor(self.DataProvider);
        self.DataProvider.Next;
        Inc(i);
    end;
end;

function TResSchemeDataAdaptor.ReadSchemeIDsWithResourceNames(const aDelimiter1, aDelimiter2: string)
    : TArray<string>;
var
    xResNames: string;
    x: integer;
begin
    // Alle Namen lesen
    result := self.ReadAllNames;

    // Ressource-Namen als Kommentare hinzufügen
    for x := 0 to high(result) do
    begin
        xResNames := GetResourceNames(result[x], aDelimiter2);

        if (xResNames <> '') then
            result[x] := result[x] + aDelimiter1 + xResNames; // Delimiter1 kann z.B. Doppelpunkt sein
    end;
end;

function TResSchemeDataAdaptor.GetKeyFields: TArray<string>;
begin
    result := self.FieldKeyArrayOf([STR_RESSCHEME_FLD_ID]);
end;

function TResSchemeDataAdaptor.GetNameField: string;
begin
    result := STR_RESSCHEME_FLD_ID;
end;

class function TResSchemeDataAdaptor.ReadRecAtCursor(aDataset: TDataProvider): TResSchemeRec;
begin
    with result do
    begin
        SchemeID := aDataset.FieldByName(STR_RESSCHEME_FLD_ID).AsInteger;
        ResID := aDataset.FieldByName(STR_RESSCHEME_FLD_RESID).AsString;
        UseLevel := aDataset.FieldByName(STR_RESSCHEME_FLD_USELEVEL).AsInteger;
        ReleaseLevel := aDataset.FieldByName(STR_RESSCHEME_FLD_RELEASELEVEL).AsInteger;
    end;
end;

class function TResSchemeDataAdaptor.ReadRecsAtCursor(aDataset: TDataProvider): TArray<TResSchemeRec>;
var
    x: integer;
begin
    x := 0;
    SetLength(result, aDataset.RecordCount);

    while not aDataset.Eof do
    begin
        result[x] := ReadRecAtCursor(aDataset);
        Inc(x);
        aDataset.Next;
    end;
end;

function TResSchemeDataAdaptor.ReadSchemes(): TArray<TResSchemeRec>;
begin
    self.SelectAndOpenAll(true);
    try
        result := ReadRecsAtCursor(self.DataProvider);
    finally
        self.Close;
    end;
end;


end.
