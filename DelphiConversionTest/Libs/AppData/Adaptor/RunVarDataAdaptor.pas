{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Data Adaptor for runtime variables which are stored in RunVar.db
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  25.08.05 pk                                TN2547  Initial Revision
  25.08.05 pk                                TN2547  uses variants
  11.01.06 pk  InstallTable                  TN2871.1 override
  05.05.06 pk                                TN3084  New Key Field : Priority
  16.05.06 wl                                TN3101  New Field : ChangeTime
  16.05.06 wl  ReadValueAndTime              TN3101   neu, liest Timestamp mit aus
  16.05.06 wl  WriteValue                    TN3102   schreibt Timestamp: jedes Mal wenn Value verändert wird
  03.08.06 pk  SelectAndOpenIdent            TN3245  No longer case-sensitive select for ident
  05.02.07 pk                                TN3544   Changes for updatemanager
  08.03.07 wl  DeleteValue                   TN3625   führt jetzt nicht mehr zu Fehler
  08.03.07 wl  WriteValue,DeleteValue        TN3475   jeder neue Wert wird in Log geschrieben
  07.08.07 wl  Create                        TN3811.2 hat Zusatz reintroduce
  09.11.07 pk                                TN3921   Changes for updatemanager
  09.11.07 pk                                TN3922   Dataset changed to DataProvider
  23.09.08 wl                                TN4236   Vereinigung mit RunVarFieldnames
  29.09.08 wl                                TN4242   'RUN' hat jetzt 50 chars
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  24.08.09 pk  BeginEdit                     TN4737   Write Priority field with AsString
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  17.06.10 pk                                TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  14.12.12 wl                                TN6054   'VAL' ist jetzt Memo, Priority ist integer, nutzlose Felder sind raus
  -------------------------------------------------------------------------------------------------- }

unit RunVarDataAdaptor;


interface


uses
    DataProvider,
    QueryDataAdaptor;

type
    TRunVarDataAdaptor = class(TQueryDataAdaptor)
    private const
        STR_RUNVAR_TBL = 'RUNVAR';
        STR_RUNVAR_FLD_RUN = 'RUN';
        STR_RUNVAR_FLD_PRIORITY = 'PRIORITY';
        STR_RUNVAR_FLD_IDENT = 'IDENT';
        STR_RUNVAR_FLD_VAL = 'VAL';
        STR_RUNVAR_FLD_VALCHANGETIME = 'VALCHANGETIME';
    private
        fRunName: string;
        fPriority: integer;
        procedure SelectAndOpenIdent(const aIdent: string; aReadOnly: boolean);
        procedure SelectAndOpenRun(aReadOnly: boolean);
        procedure BeginEdit(const aIdent: string);
        class function ReadIdentFromDataset(aDataset: TDataProvider): string;
        class function ReadValueFromDataset(aDataset: TDataProvider): string;
        class function ReadChangeTimeFromDataset(aDataset: TDataProvider): TDateTime;
        class procedure WriteValueToDataset(aDataset: TDataProvider; const aValue: string);
        class function ReadAllIdentsFromDataset(aDataset: TDataProvider): TArray<string>;
    public
        constructor Create(const aRunName: string; aPriority: integer); reintroduce;

        procedure DeleteIdentifier(const aIdent: string);
        function ReadValueAndTime(const aIdent: string; out oValue: string;
            out oChangeTime: TDateTime): boolean;
        procedure WriteValue(const aIdent, aValue: string);
        function ReadAllIdents(): TArray<string>;
    end;


implementation


uses
    SysUtils;
{ TRunVarDataAdaptor }

constructor TRunVarDataAdaptor.Create(const aRunName: string; aPriority: integer);
begin
    inherited Create(STR_RUNVAR_TBL);
    fRunName := aRunName;
    fPriority := aPriority;
end;

procedure TRunVarDataAdaptor.SelectAndOpenRun(aReadOnly: boolean);
begin
    SelectAndOpen('SELECT * FROM ' + STR_RUNVAR_TBL + ' WHERE ' + STR_RUNVAR_FLD_RUN + '=''' + fRunName + ''''
        + ' AND ' + STR_RUNVAR_FLD_PRIORITY + '=' + IntToStr(fPriority), aReadOnly);
end;

procedure TRunVarDataAdaptor.SelectAndOpenIdent(const aIdent: string; aReadOnly: boolean);
begin
    SelectAndOpen('SELECT * FROM ' + STR_RUNVAR_TBL + ' WHERE ' + STR_RUNVAR_FLD_RUN + '=''' + fRunName + ''''
        + ' AND ' + STR_RUNVAR_FLD_PRIORITY + '=' + IntToStr(fPriority) + ' AND UPPER(' + STR_RUNVAR_FLD_IDENT
        + ')' + '=''' + UpperCase(aIdent) + '''', aReadOnly);
end;

function TRunVarDataAdaptor.ReadValueAndTime(const aIdent: string; out oValue: string;
    out oChangeTime: TDateTime): boolean;
begin
    SelectAndOpenIdent(aIdent, true);
    try
        if self.DataProvider.Eof then
            EXIT(true);

        oValue := ReadValueFromDataset(self.DataProvider);
        oChangeTime := ReadChangeTimeFromDataset(self.DataProvider);
        EXIT(false);
    finally
        Close();
    end;
end;

function TRunVarDataAdaptor.ReadAllIdents(): TArray<string>;
begin
    SelectAndOpenRun(true);
    try
        result := ReadAllIdentsFromDataset(self.DataProvider);
    finally
        Close();
    end;
end;

procedure TRunVarDataAdaptor.DeleteIdentifier(const aIdent: string);
begin
    ExecSQL('DELETE FROM ' + STR_RUNVAR_TBL + ' WHERE ' + STR_RUNVAR_FLD_RUN + '=''' + fRunName + '''' +
        ' AND ' + STR_RUNVAR_FLD_PRIORITY + '=' + IntToStr(fPriority) + ' AND ' + 'UPPER(' +
        STR_RUNVAR_FLD_IDENT + ')' + '=''' + UpperCase(aIdent) + '''');
end;

procedure TRunVarDataAdaptor.BeginEdit(const aIdent: string);
begin
    if self.DataProvider.IsEmpty then
    begin
        self.DataProvider.Append;
        self.DataProvider.FieldByName(STR_RUNVAR_FLD_RUN).AsString := fRunName;
        self.DataProvider.FieldByName(STR_RUNVAR_FLD_PRIORITY).AsInteger := fPriority;
        self.DataProvider.FieldByName(STR_RUNVAR_FLD_IDENT).AsString := aIdent;
    end
    else
        self.DataProvider.Edit;
end;

procedure TRunVarDataAdaptor.WriteValue(const aIdent, aValue: string);
begin
    SelectAndOpenIdent(aIdent, false);
    try
        BeginEdit(aIdent);
        WriteValueToDataset(self.DataProvider, aValue);
        self.DataProvider.Post;
    finally
        Close();
    end;
end;

class function TRunVarDataAdaptor.ReadValueFromDataset(aDataset: TDataProvider): string;
begin
    result := aDataset.FieldByName(STR_RUNVAR_FLD_VAL).AsString;
end;

class function TRunVarDataAdaptor.ReadChangeTimeFromDataset(aDataset: TDataProvider): TDateTime;
begin
    result := aDataset.FieldByName(STR_RUNVAR_FLD_VALCHANGETIME).AsDateTime;
end;

class procedure TRunVarDataAdaptor.WriteValueToDataset(aDataset: TDataProvider; const aValue: string);
begin
    aDataset.FieldByName(STR_RUNVAR_FLD_VAL).AsString := aValue;
    aDataset.FieldByName(STR_RUNVAR_FLD_VALCHANGETIME).AsDateTime := Now;
end;

class function TRunVarDataAdaptor.ReadIdentFromDataset(aDataset: TDataProvider): string;
begin
    result := aDataset.FieldByName(STR_RUNVAR_FLD_IDENT).AsString;
end;

class function TRunVarDataAdaptor.ReadAllIdentsFromDataset(aDataset: TDataProvider): TArray<string>;
var
    x: integer;
begin
    SetLength(result, aDataset.RecordCount);
    x := 0;
    while not aDataset.EOf do
    begin
        result[x] := ReadIdentFromDataset(aDataset);
        Inc(x);
        aDataset.Next;
    end;
end;


end.
