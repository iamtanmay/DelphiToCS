{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Author       : Thomas Schubert (ts)
  Description  : Data Adaptor for the DATAHISTORY.DB
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  16.04.09 ts                               TN4477   initial revision
  17.06.10 pk                               TN5152.1  Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  -------------------------------------------------------------------------------------------------- }

unit DataHistoryDataAdaptor;


interface


uses
    Classes,
    DataProvider,
    QueryDataAdaptor,
    GeneralTypes;

const
    STR_DEF_HISTNAME_METHOD = 'METHOD';
    INT_NUMBER_OF_HISTORY_METHODS = 10;

type
    TDataHistoryArray = TStringArray;

    TDataHistoryDataAdaptor = class(TQueryDataAdaptor)
    public
        constructor Create();

        procedure SelectAndOpenHistory(aDataHistoryName: string; aReadOnly: boolean);
        procedure UpdateDataHistory(aDatahistoryName: string; aDataHistoryArray: TDataHistoryArray);
        function ReadHistoryDataset(aDataHistoryName: string): TDataHistoryArray;
        procedure DeleteHistoryByNameAndValue(aDataHistoryName, aDataHistoryValue: string);
    end;

    // ##################################################################################################


implementation


uses
    SysUtils,
    Variants;

const
    STR_DATAHISTORY_TBL = 'DATAHISTORY';
    STR_DATAHISTORY_FLD_HISTNAME = 'DATAHISTORYNAME';
    STR_DATAHISTORY_FLD_HISTVAL = 'DATAHISTORYVALUE';
    STR_DATAHISTORY_FLD_ID = 'ID';

    STR_SQL_DATAHISTORY_SELECT = 'SELECT * FROM ' + STR_DATAHISTORY_TBL;
    STR_SQL_DATAHISTORY_DELETE = 'DELETE FROM ' + STR_DATAHISTORY_TBL;

    INT_DATAHISTORY_MOST_RECENT_VERSION = 1;

    { TDataHistoryDataAdaptor }

constructor TDataHistoryDataAdaptor.Create();
begin
    inherited Create(STR_DATAHISTORY_TBL);
end;

procedure TDataHistoryDataAdaptor.SelectAndOpenHistory(aDataHistoryName: string; aReadOnly: boolean);
var
    xSelect: string;
begin
    xSelect := STR_SQL_DATAHISTORY_SELECT;
    if aDataHistoryName <> '' then
        xSelect := format('%s WHERE %s = ''%s''', [xSelect, STR_DATAHISTORY_FLD_HISTNAME, aDataHistoryName]);
    self.SelectAndOpen(xSelect, aReadOnly);
end;

procedure TDataHistoryDataAdaptor.DeleteHistoryByNameAndValue(aDataHistoryName, aDataHistoryValue: string);
var
    xDelete: string;
begin
    xDelete := STR_SQL_DATAHISTORY_DELETE + ' WHERE ' + STR_DATAHISTORY_FLD_HISTNAME + ' = ''' +
        aDataHistoryName + '''';
    if aDataHistoryValue <> '' then
        xDelete := xDelete + ' AND ' + STR_DATAHISTORY_FLD_HISTVAL + ' = ''' + aDataHistoryValue + '''';
    self.ExecSQL(xDelete);
end;

procedure TDataHistoryDataAdaptor.UpdateDataHistory(aDatahistoryName: string;
    aDataHistoryArray: TDataHistoryArray);
var
    i: integer;
begin
    self.DeleteHistoryByNameAndValue(aDatahistoryName, ''); // alle Einträge löschen
    self.SelectAndOpenHistory(aDataHistoryName, false);
    try
        for i := 0 to high(aDataHistoryArray) do
        begin // alle Menüeinträge in DATAHISTORY-Tabelle schreiben
            self.DataProvider.Append;
            self.DataProvider.FieldByName(STR_DATAHISTORY_FLD_HISTNAME).AsString := aDataHistoryName;
            self.DataProvider.FieldByName(STR_DATAHISTORY_FLD_HISTVAL).AsString := aDataHistoryArray[i];
            self.DataProvider.FieldByName(STR_DATAHISTORY_FLD_ID).AsInteger := i + 1;
            self.DataProvider.Post;
        end;
    finally
        self.Close;
    end;
end;

function TDataHistoryDataAdaptor.ReadHistoryDataset(aDataHistoryName: string): TDataHistoryArray;
var
    i: integer;
begin
    self.SelectAndOpenHistory(aDataHistoryName, false);
    try
        i := 0;
        SetLength(result, self.DataProvider.RecordCount);
        self.DataProvider.First;
        while not self.DataProvider.Eof do
        begin
            result[i] := self.DataProvider.FieldByName(STR_DATAHISTORY_FLD_HISTVAL).AsString;
            Inc(i);
            self.DataProvider.Next;
        end;
    finally
        self.Close;
    end;
end;


end.
