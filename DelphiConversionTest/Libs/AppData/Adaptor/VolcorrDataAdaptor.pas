{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Tobias Hattemer (tbh)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op   method                       track-no improvement/change
  -------- ---  ---------------------------  -------- ----------------------------------------------
  07.06.05 tbh                               TN2385   New
  23.06.05 pk                                TN2471   various changes needed for Calib.dll
  23.06.05 pk                                TN2471   uses variants
  01.07.05 pk   ReadInterpolatedFactorForVol TN2486   raise exception if factor <= 0
  01.07.05 pk   InstallTable                 TN2487   Do not call CreateTable
  11.01.06 pk   InstallTable                 TN2871.1 override
  19.04.06 wl                                TN3051   alle Volumen-Werte sind extended
  05.02.07 pk                                TN3544   Changes for updatemanager
  07.08.07 wl                                TN3811.2 uses geändert
  09.11.07 pk                                TN3921   Changes for updatemanager
  09.11.07 pk                                TN3922   Query, Dataset changed to DataProvider
  03.07.08 wl                                TN4157
  23.09.08 wl                                TN4236   Vereinigung mit ..Fieldnames
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  13.07.09 pk                                TN4585.4 DataProvider.Locate removed, functionality replaced by SelectAndOpen
  05.10.09 pk  STR_SQL_SELECTBY_TIPNAMEVOL   TN4764   Volume correction with german decimal (comma) now works
  12.10.09 pk  SelectAndOpenByTipAndNameAndVol   TN4764   log sql string
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  17.06.10 pk                                TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  27.04.11 wl  SelectAndOpenByTipAndNameAndVol   TN5564   funktioniert jetzt auch mit TurboDB und deutschen Einstellungen
  31.10.11 wl  GetFactorForVol                   TN5731   Rückgebewert nicht mehr boolean
  01.11.11 wl  GetNameField                      TN5731   neu
  04.03.14 ts                                    TN6369   neu: STR_ORDER_BY
  -------------------------------------------------------------------------------------------------- }

unit VolcorrDataAdaptor;


interface


uses
    Classes,
    GeneralTypes,
    DataProvider,
    QueryDataAdaptor;

const
    STR_VOLCORRECT_CURVENAME_DEFAULT = 'DEFAULT';

    DBL_VOLCORR_DEFAULT_FACTOR = 1.0;

type
    TVolcorrDataAdaptor = class(TQueryDataAdaptor)
    private const
        STR_VOLCORRECT_TBL = 'VOLCORR';

        STR_VOLCORRECT_FLD_CURVENAME = 'NAME';
        STR_VOLCORRECT_FLD_PIPDEVNAME = 'PIPDEVICENAME';
        STR_VOLCORRECT_FLD_TIP = 'TIP';
        STR_VOLCORRECT_FLD_VOLUME = 'VOLUME';
        STR_VOLCORRECT_FLD_FACTOR = 'FACTOR';

        STR_VOLCORRECT_INDEX_FIELDS = STR_VOLCORRECT_FLD_CURVENAME + ';' + STR_VOLCORRECT_FLD_PIPDEVNAME + ';'
            + STR_VOLCORRECT_FLD_TIP + ';' + STR_VOLCORRECT_FLD_VOLUME;

        INT_VOLCORRECT_FLDLEN_NAME = 20;

        STR_SQL_SELECT_ALL = 'SELECT * FROM ' + STR_VOLCORRECT_TBL;

        STR_SQL_OPENCURVE = STR_SQL_SELECT_ALL + ' WHERE ' + STR_VOLCORRECT_FLD_CURVENAME + '=''%s'' AND ' +
            STR_VOLCORRECT_FLD_PIPDEVNAME + '=''%s'' AND ' + STR_VOLCORRECT_FLD_TIP + '=%d';

        STR_SQL_GETCURVELIST = 'SELECT DISTINCT ' + STR_VOLCORRECT_FLD_CURVENAME + ' FROM ' +
            STR_VOLCORRECT_TBL;
        STR_SQL_SETDEFAULT = 'UPDATE ' + STR_VOLCORRECT_TBL + ' SET ' + STR_VOLCORRECT_FLD_CURVENAME + '=''' +
            STR_VOLCORRECT_CURVENAME_DEFAULT + ''' WHERE ' + STR_VOLCORRECT_FLD_CURVENAME + '='''' OR ' +
            STR_VOLCORRECT_FLD_CURVENAME + ' is null';
        STR_ORDER_BY = ' ORDER BY ' + STR_VOLCORRECT_FLD_CURVENAME + ',' + STR_VOLCORRECT_FLD_PIPDEVNAME + ','
            + STR_VOLCORRECT_FLD_TIP + ',' + STR_VOLCORRECT_FLD_VOLUME;
    private
        class var uVolcorrDataAdaptor: TVolcorrDataAdaptor;
        procedure WriteRecord(const aCurveName, aPipDeviceName: string; aTip: integer; aVol: extended;
            aFactor: extended);
        procedure EditFactor(aFactor: extended);
        function ReadInterpolatedFactorForVol(aCurveName: string; aVol: extended;
            var aFactor: extended): boolean;
    protected
        function GetNameField(): string; override;
    public
        constructor Create();
        class function Instance(): TVolcorrDataAdaptor;

        procedure SelectCurveAndOpen(const aName, aPipDeviceName: string; aTip: integer; aReadOnly: boolean);
        procedure SelectAndOpenByTipAndNameAndVol(const aName, aPipDeviceName: string; aTip: integer;
            aVol: extended; aReadOnly: boolean);
        procedure SelectAllAndOpen(aReadOnly: boolean = true);
        procedure SetDefaultName;
        function GetFactorForVol(const aCurveName, aPipDeviceName: string; aTip: integer; aVol: extended)
            : extended;
        function GetExactFactorForVol(const aCurveName, aPipDeviceName: string; aTip: integer; aVol: extended;
            var aFactor: extended): boolean;
        procedure SetFactorForVol(const aCurveName, aPipDeviceName: string; aTip: integer; aVol: extended;
            aFactor: extended);
        function AddDefaultFactorForVol(const aCurveName, aPipDeviceName: string; aTip: integer;
            aVol: extended): extended;
        function ReadCurveNames(): TStringArray;
    end;


implementation


uses
    Variants,
    SysUtils,
    LogManager;

{ TVolcorrDataAdaptor }

class function TVolcorrDataAdaptor.Instance(): TVolcorrDataAdaptor;
begin
    if not Assigned(uVolcorrDataAdaptor) then
    begin
        uVolcorrDataAdaptor := TVolcorrDataAdaptor.Create();
    end;

    result := uVolcorrDataAdaptor;
end;

constructor TVolcorrDataAdaptor.Create;
begin
    inherited Create(STR_VOLCORRECT_TBL);
end;

procedure TVolcorrDataAdaptor.SelectAllAndOpen(aReadOnly: boolean = true);
begin
    self.SelectAndOpen(STR_SQL_SELECT_ALL + STR_ORDER_BY, aReadOnly);
end;

procedure TVolcorrDataAdaptor.SelectCurveAndOpen(const aName, aPipDeviceName: string; aTip: integer;
    aReadOnly: boolean);
begin
    self.SelectAndOpen(format(STR_SQL_OPENCURVE + STR_ORDER_BY, [aName, aPipDeviceName, aTip]), aReadOnly);
end;

procedure TVolcorrDataAdaptor.SelectAndOpenByTipAndNameAndVol(const aName, aPipDeviceName: string;
    aTip: integer; aVol: extended; aReadOnly: boolean);
var
    xSQLTerm: string;
begin
    xSQLTerm := STR_SQL_SELECT_ALL + ' WHERE ' + STR_VOLCORRECT_FLD_CURVENAME + '=''' + aName + ''' AND ' +
        STR_VOLCORRECT_FLD_PIPDEVNAME + '=''' + aPipDeviceName + ''' AND ' + STR_VOLCORRECT_FLD_TIP + '=' +
        IntToStr(aTip) + ' AND ' + STR_VOLCORRECT_FLD_VOLUME + '=' +
        FloatToStr(aVol, TFormatUtils.GetSettingsEnglishUS) + STR_ORDER_BY;
    gLogManager.log(xSQLTerm, false);
    self.SelectAndOpen(xSQLTerm, aReadOnly);
end;

function TVolcorrDataAdaptor.GetExactFactorForVol(const aCurveName, aPipDeviceName: string; aTip: integer;
    aVol: extended; var aFactor: extended): boolean;
// Record aus der Volcorr-Datenbank lesen
begin
    SelectAndOpenByTipAndNameAndVol(aCurveName, aPipDeviceName, aTip, aVol, true);
    try
        result := not self.DataProvider.IsEmpty;
        if not result then
            EXIT;
        aFactor := self.DataProvider.FieldByName(STR_VOLCORRECT_FLD_FACTOR).AsFloat;
    finally
        Close();
    end;
end;

function TVolcorrDataAdaptor.GetFactorForVol(const aCurveName, aPipDeviceName: string; aTip: integer;
    aVol: extended): extended;
// Record aus der Volcorr-Datenbank lesen
begin
    SelectCurveAndOpen(aCurveName, aPipDeviceName, aTip, true);
    try
        ReadInterpolatedFactorForVol(aCurveName, aVol, result);
    finally
        Close();
    end;
end;

function TVolcorrDataAdaptor.GetNameField: string;
begin
    EXIT(STR_VOLCORRECT_FLD_CURVENAME);
end;

function TVolcorrDataAdaptor.ReadInterpolatedFactorForVol(aCurveName: string; aVol: extended;
    var aFactor: extended): boolean;
var
    xVol: array [0 .. 1] of extended;
    xValue: array [0 .. 1] of extended;
begin
    aFactor := 1;
    result := not self.DataProvider.Eof;
    if not result then
        EXIT;

    self.DataProvider.First;
    while (not self.DataProvider.Eof) do
    begin
        if (self.DataProvider.FieldByName(STR_VOLCORRECT_FLD_VOLUME).AsFloat >= aVol) then
            BREAK;
        self.DataProvider.Next;
    end;

    xVol[1] := self.DataProvider.FieldByName(STR_VOLCORRECT_FLD_VOLUME).AsFloat;
    xValue[1] := self.DataProvider.FieldByName(STR_VOLCORRECT_FLD_FACTOR).AsFloat;
    // -------------------------- wenn aVol = Volume in Tabelle dann Wert zuweisen
    if (xVol[1] = aVol) then
    begin
        aFactor := xValue[1];
        EXIT;
    end;

    try
        // ------------------------- prüfen ob auch ein kleinerer Volume Wert existiert ---------
        self.DataProvider.Prior;
        xVol[0] := self.DataProvider.FieldByName(STR_VOLCORRECT_FLD_VOLUME).AsFloat;
        xValue[0] := self.DataProvider.FieldByName(STR_VOLCORRECT_FLD_FACTOR).AsFloat;
        if (xValue[0] <> xValue[1]) then
        begin
            // ------------------- wenn ja - dann Faktor aus beiden Volumen ermitteln ----------
            aFactor := xValue[0] + (aVol - xVol[0]) / (xVol[1] - xVol[0]) * (xValue[1] - xValue[0]);
            if aFactor <= 0 then
                raise Exception.CreateFmt
                    ('An interpolated factor which is <= 0 was calculated for Volume [%f]', [aVol]);
        end
        else
            aFactor := xValue[0];

    except
        result := false;
    end;

end;

procedure TVolcorrDataAdaptor.WriteRecord(const aCurveName, aPipDeviceName: string; aTip: integer;
    aVol: extended; aFactor: extended);
begin
    self.DataProvider.Append;

    self.DataProvider.FieldByName(STR_VOLCORRECT_FLD_CURVENAME).AsString := aCurveName;
    self.DataProvider.FieldByName(STR_VOLCORRECT_FLD_PIPDEVNAME).AsString := aPipDeviceName;
    self.DataProvider.FieldByName(STR_VOLCORRECT_FLD_TIP).AsInteger := aTip;
    self.DataProvider.FieldByName(STR_VOLCORRECT_FLD_VOLUME).AsFloat := aVol;
    self.DataProvider.FieldByName(STR_VOLCORRECT_FLD_FACTOR).AsFloat := aFactor;

    self.DataProvider.Post;
end;

procedure TVolcorrDataAdaptor.EditFactor(aFactor: extended);
begin
    self.DataProvider.Edit;
    self.DataProvider.FieldByName(STR_VOLCORRECT_FLD_FACTOR).AsFloat := aFactor;
    self.DataProvider.Post;
end;

function TVolcorrDataAdaptor.AddDefaultFactorForVol(const aCurveName, aPipDeviceName: string; aTip: integer;
    aVol: extended): extended;
begin
    SelectAllAndOpen(false);
    try
        result := DBL_VOLCORR_DEFAULT_FACTOR;
        WriteRecord(aCurveName, aPipDeviceName, aTip, aVol, result);
    finally
        Close();
    end;
end;

procedure TVolcorrDataAdaptor.SetFactorForVol(const aCurveName, aPipDeviceName: string; aTip: integer;
    aVol: extended; aFactor: extended);
begin
    self.SelectAndOpenByTipAndNameAndVol(aCurveName, aPipDeviceName, aTip, aVol, false);
    try
        if self.DataProvider.IsEmpty then
            raise Exception.CreateFmt('Record CurveName = %s, PipDevice = %s, Tip = %d, Vol = %f not found',
                [aCurveName, aPipDeviceName, aTip, aVol, aFactor]);

        EditFactor(aFactor);

    finally
        Close();
    end;
end;

function TVolcorrDataAdaptor.ReadCurveNames(): TStringArray;
var
    x: integer;
begin
    try
        self.SelectAndOpen(STR_SQL_GETCURVELIST, true);
        SetLength(result, self.DataProvider.RecordCount);
        x := 0;
        while not self.DataProvider.EOF do
        begin
            result[x] := self.DataProvider.Fields[0].AsString;
            Inc(x);
            self.DataProvider.Next;
        end;
    except
        SetLength(result, 0);
    end;
end;

procedure TVolcorrDataAdaptor.SetDefaultName;
begin
    self.ExecSQL(STR_SQL_SETDEFAULT);
end;


end.
