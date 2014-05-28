{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : data adaptor for METHODIMPORTDEFS.DB
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------- ----------------------------------------------
  31.08.05 wl                               TN2541.0  initial version
  05.01.06 wl  ReadMethImportDefsByKey      TN2541.0  wenn Keyname = '' -> EXIT
  11.01.06 pk  InstallTable                  TN2871.1 override
  04.05.06 wl                                TN2541.0  neu: SchedMin, SchedMax, ResID
  18.08.06 wl                                TN3258    SCHEDMIN,SCHEDMAX: 80 statt 10 Zeichen
  05.02.07 pk                                TN3544   Changes for updatemanager
  07.08.07 wl                                TN3811.2 uses geändert
  02.10.07 wl                                TN3811.5 benutzt self.Query statt fQuery
  09.11.07 pk                                TN3922    Dataset changed to DataProvider
  09.11.07 pk                                TN3921    Changes for Updatemanager
  23.09.08 wl                                TN4236   Vereinigung mit ..Fieldnames
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  17.06.10 pk                                TN5152.1  Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  -------------------------------------------------------------------------------------------------- }

unit MethImportDefsDataAdaptor;


interface


uses
    DataProvider,
    QueryDataAdaptor;

type
    TMethImportDefsRec = record
        KeyName: string;
        Action: string;
        Options: string;
        Comment: string;
        SchedMin: string;
        SchedMax: string;
        ResID: string;
    end;

    TMethImportDefsDataAdaptor = class(TQueryDataAdaptor)
    private
        procedure SelectAndOpenByKeyName(aKeyName: string; aReadOnly: boolean);
    public
        constructor Create();

        class function ReadRecAtCursor(aDataset: TDataProvider): TMethImportDefsRec;
        class function GetEmptyRec(): TMethImportDefsRec;
        function ReadMethImportDefsByKey(aKeyName: string): TMethImportDefsRec;
    end;

    // ##################################################################################################


implementation


uses
    SysUtils;

const
    STR_METHIMPORTDEFS_TBL = 'METHIMPORTDEFS';

    STR_METHIMPORTDEFS_FLD_KEYNAME = 'KEYNAME';
    STR_METHIMPORTDEFS_FLD_ACTION = 'ACTION';
    STR_METHIMPORTDEFS_FLD_OPTIONS = 'OPTIONS';
    STR_METHIMPORTDEFS_FLD_COMMENT = 'COMMENT';
    STR_METHIMPORTDEFS_FLD_SCHEDMIN = 'SCHEDMIN';
    STR_METHIMPORTDEFS_FLD_SCHEDMAX = 'SCHEDMAX';
    STR_METHIMPORTDEFS_FLD_RESID = 'RESID';

    INT_METHIMPORTDEFS_FLDLEN_KEYNAME = 40;
    INT_METHIMPORTDEFS_FLDLEN_ACTION = 20;
    INT_METHIMPORTDEFS_FLDLEN_SCHEDMIN = 80;
    INT_METHIMPORTDEFS_FLDLEN_SCHEDMAX = 80;
    INT_METHIMPORTDEFS_FLDLEN_RESID = 10;

    STR_METHIMPORTDEFS_INDEX_FIELDS = STR_METHIMPORTDEFS_FLD_KEYNAME;

    STR_SELECT_BY_KEYNAME_FMT = 'SELECT * FROM ' + STR_METHIMPORTDEFS_TBL + ' WHERE ' +
        STR_METHIMPORTDEFS_FLD_KEYNAME + ' = ''%s''';

    { TMethImportDefsDataAdaptor }

constructor TMethImportDefsDataAdaptor.Create;
begin
    inherited Create(STR_METHIMPORTDEFS_TBL);
end;

procedure TMethImportDefsDataAdaptor.SelectAndOpenByKeyName(aKeyName: string; aReadOnly: boolean);
var
    xSQL: string;
begin
    xSQL := Format(STR_SELECT_BY_KEYNAME_FMT, [aKeyName]);
    SelectAndOpen(xSQL, aReadOnly);
end;

class function TMethImportDefsDataAdaptor.ReadRecAtCursor(aDataset: TDataProvider): TMethImportDefsRec;
begin
    result.KeyName := aDataset.FieldByName(STR_METHIMPORTDEFS_FLD_KEYNAME).AsString;
    result.Action := aDataset.FieldByName(STR_METHIMPORTDEFS_FLD_ACTION).AsString;
    result.Options := aDataset.FieldByName(STR_METHIMPORTDEFS_FLD_OPTIONS).AsString;
    result.Comment := aDataset.FieldByName(STR_METHIMPORTDEFS_FLD_COMMENT).AsString;
    result.SchedMin := aDataset.FieldByName(STR_METHIMPORTDEFS_FLD_SCHEDMIN).AsString;
    result.SchedMax := aDataset.FieldByName(STR_METHIMPORTDEFS_FLD_SCHEDMAX).AsString;
    result.ResID := aDataset.FieldByName(STR_METHIMPORTDEFS_FLD_RESID).AsString;
end;

function TMethImportDefsDataAdaptor.ReadMethImportDefsByKey(aKeyName: string): TMethImportDefsRec;
begin
    result := self.GetEmptyRec();
    if (aKeyName = '') then
        EXIT;

    self.SelectAndOpenByKeyName(aKeyName, true);
    if not self.DataProvider.Eof then
    begin
        result := ReadRecAtCursor(self.DataProvider);
    end;
end;

class function TMethImportDefsDataAdaptor.GetEmptyRec(): TMethImportDefsRec;
begin
    result.KeyName := '';
    result.Action := '';
    result.Options := '';
    result.Comment := '';
    result.SchedMin := '';
    result.SchedMax := '';
    result.ResID := '';
end;


end.
