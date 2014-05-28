{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                        track-no improvement/change
  -------- --  ---------------------------   -------- -------------------------------------------------------------------
  11.07.08 wl                                TN4164   initial revision
  16.07.08 wl                                TN4164   some changes
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  ---------------------------------------------------------------------------------------------------------------------- }

unit WorkspaceDevicesDataAdaptor;


interface


uses
    DataProvider,
    QueryDataAdaptor;

type
    TWorkspaceDevicesRec = record
        WorkspaceID: integer;
        DeviceName: string;
    end;

    TWorkspaceDevicesRecArray = array of TWorkspaceDevicesRec;

    TWorkspaceDevicesDataAdaptor = class(TQueryDataAdaptor)
    private
        procedure SelectAndOpenByID(aWorkspaceID: integer; aReadOnly: boolean);
        class procedure ReadRecAtCursor(aDataset: TDataProvider; out oRec: TWorkspaceDevicesRec);
        class procedure WriteRecAtCursor(aDataset: TDataProvider; const aRec: TWorkspaceDevicesRec;
            aAppend: boolean);
    public
        constructor Create();

        procedure DeleteByWorkspaceID(aWorkspaceID: integer);
        procedure ReadByWorkspaceID(aWorkspaceID: integer; out oRec: TWorkspaceDevicesRecArray);
        procedure WriteByWorkspaceID(aWorkspaceID: integer; const aRec: TWorkspaceDevicesRecArray);
    end;


implementation


uses
    SysUtils;

const
    cWorkspaceDevicesTable = 'WORKSPACEDEVICES';

    cWorkspaceDevicesFieldWorkspaceID = 'WORKSPACEID';
    cWorkspaceDevicesFieldDeviceName = 'DEVICENAME';

    cSQLWhereID = ' WHERE ' + cWorkspaceDevicesFieldWorkspaceID + ' = %d';
    cSQLSelectByWorkspaceID = 'SELECT * FROM ' + cWorkspaceDevicesTable + cSQLWhereID;
    cSQLDeleteByWorkspaceID = 'DELETE FROM ' + cWorkspaceDevicesTable + cSQLWhereID;

    { TWorkspaceDevicesDataAdaptor }

constructor TWorkspaceDevicesDataAdaptor.Create();
begin
    inherited Create(cWorkspaceDevicesTable);
end;

procedure TWorkspaceDevicesDataAdaptor.SelectAndOpenByID(aWorkspaceID: integer; aReadOnly: boolean);
var
    xSQL: string;
begin
    xSQL := Format(cSQLSelectByWorkspaceID, [aWorkspaceID]);
    SelectAndOpen(xSQL, aReadOnly);
end;

procedure TWorkspaceDevicesDataAdaptor.ReadByWorkspaceID(aWorkspaceID: integer;
    out oRec: TWorkspaceDevicesRecArray);
var
    i: integer;
begin
    self.SelectAndOpenByID(aWorkspaceID, true);
    SetLength(oRec, self.DataProvider.RecordCount);
    i := 0;
    while not self.DataProvider.Eof do
    begin
        ReadRecAtCursor(self.DataProvider, oRec[i]);
        self.DataProvider.Next;
        Inc(i);
    end;
end;

procedure TWorkspaceDevicesDataAdaptor.WriteByWorkspaceID(aWorkspaceID: integer;
    const aRec: TWorkspaceDevicesRecArray);
var
    x: integer;
begin
    // delete all device name
    self.DeleteByWorkspaceID(aWorkspaceID);

    // write new device names
    self.SelectAndOpenAll(false);
    try
        for x := 0 to high(aRec) do
        begin
            aRec[x].WorkspaceID := aWorkspaceID;
            self.WriteRecAtCursor(self.DataProvider, aRec[x], true);
        end;
    finally
        self.Close();
    end;
end;

class procedure TWorkspaceDevicesDataAdaptor.ReadRecAtCursor(aDataset: TDataProvider;
    out oRec: TWorkspaceDevicesRec);
begin
    oRec.WorkspaceID := aDataset.FieldByName(cWorkspaceDevicesFieldWorkspaceID).AsInteger;
    oRec.DeviceName := aDataset.FieldByName(cWorkspaceDevicesFieldDeviceName).AsString;
end;

class procedure TWorkspaceDevicesDataAdaptor.WriteRecAtCursor(aDataset: TDataProvider;
    const aRec: TWorkspaceDevicesRec; aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    aDataset.FieldByName(cWorkspaceDevicesFieldWorkspaceID).AsInteger := aRec.WorkspaceID;
    aDataset.FieldByName(cWorkspaceDevicesFieldDeviceName).AsString := aRec.DeviceName;

    aDataset.Post;
end;

procedure TWorkspaceDevicesDataAdaptor.DeleteByWorkspaceID(aWorkspaceID: integer);
begin
    self.ExecSQL(Format(cSQLDeleteByWorkspaceID, [aWorkspaceID]));
end;


end.
