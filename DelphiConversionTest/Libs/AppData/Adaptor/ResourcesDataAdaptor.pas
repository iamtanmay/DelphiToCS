{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Data Adaptor for the Resources Table
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  02.02.05 pk                                TN2302.1 New
  08.11.05 wl  Create/VerifyResourcesTable   TN2745   von DBScrip hierher verschoben
  05.02.07 pk                                TN3544   Changes for updatemanager
  07.08.07 wl  Instance                      TN3811.3 entfernt
  20.08.07 pk  WriteDefaultResources         TN3811.3 Bug fix: To avoid key violation exception Write only if table is empty
  09.11.07 pk                                TN3921   Changes for updatemanager
  09.11.07 pk                                TN3922   Dataset changed to DataProvider
  23.09.08 wl                                TN4236   Vereinigung mit ..Fieldnames
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  -------------------------------------------------------------------------------------------------- }

unit ResourcesDataAdaptor;


interface


uses
    DataProvider,
    QueryDataAdaptor;

const

    STR_ALLDATASOURCE_NAME = 'ALL';
    STR_RESNAME_ROBOT = 'Robot';
    INT_SHARELEVEL_ROBOT = 1;
    STR_RESNAME_DELAY = 'Delay';
    INT_SHARELEVEL_DELAY = 10000;

type
    TResourceRec = record
        Valid: boolean;
        name: string;
        ResName: string;
        ShareLevel: integer;
    end;

    TResourceRecArray = array of TResourceRec;

    TResourcesDataAdaptor = class(TQueryDataAdaptor)
    private
        function IsTableEmpty(): boolean;
        class procedure ReadRecFromDataset(aDataset: TDataProvider; var aRec: TResourceRec);
        class procedure WriteRecToDataset(aDataset: TDataProvider; const aRec: TResourceRec;
            aAppend: boolean);
    public
        constructor Create();

        function ReadAll(): TResourceRecArray;
        procedure WriteRecs(aRecs: TResourceRecArray);
        class procedure WriteDefaultResources();

        class procedure SetResourceRec(var aRec: TResourceRec; aName, aResName: string; aShareLevel: integer);
    end;


implementation


uses
    SysUtils;

const
    STR_RESOURCES_TABLE = 'RESOURCES';
    STR_RESOURCES_FLD_NAME = 'NAME';
    STR_RESOURCES_FLD_RESNAME = 'RESNAME';
    STR_RESOURCES_FLD_SHARELEVEL = 'SHARELEVEL';

    STR_SQL_SELECT_ALL = 'SELECT * FROM ' + STR_RESOURCES_TABLE;

constructor TResourcesDataAdaptor.Create;
begin
    inherited Create(STR_RESOURCES_TABLE);
end;

function TResourcesDataAdaptor.ReadAll(): TResourceRecArray;
var
    i: integer;
begin
    self.SelectAndOpen(STR_SQL_SELECT_ALL, true);
    try
        SetLength(result, self.DataProvider.RecordCount);
        i := 0;
        while not self.DataProvider.Eof do
        begin
            ReadRecFromDataset(self.DataProvider, result[i]);
            self.DataProvider.Next;
            Inc(i);
        end;
    finally
        self.Close();
    end;
end;

procedure TResourcesDataAdaptor.WriteRecs(aRecs: TResourceRecArray);
var
    i: integer;
begin
    self.SelectAndOpen(STR_SQL_SELECT_ALL, false);
    try
        for i := 0 to high(aRecs) do
        begin
            WriteRecToDataset(self.DataProvider, aRecs[i], true);
        end;
    finally
        self.Close();
    end;
end;

class procedure TResourcesDataAdaptor.ReadRecFromDataset(aDataset: TDataProvider; var aRec: TResourceRec);
begin
    with aDataset, aRec do
    begin
        Valid := true;
        SetResourceRec(aRec, FieldByName(STR_RESOURCES_FLD_NAME).AsString,
            FieldByName(STR_RESOURCES_FLD_RESNAME).AsString, FieldByName(STR_RESOURCES_FLD_SHARELEVEL)
            .AsInteger);
    end;
end;

class procedure TResourcesDataAdaptor.WriteRecToDataset(aDataset: TDataProvider; const aRec: TResourceRec;
    aAppend: boolean);
begin
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    with aDataset, aRec do
    begin
        FieldByName(STR_RESOURCES_FLD_NAME).AsString := name;
        FieldByName(STR_RESOURCES_FLD_RESNAME).AsString := ResName;
        FieldByName(STR_RESOURCES_FLD_SHARELEVEL).AsInteger := ShareLevel;
    end;
    aDataset.Post;
end;

class procedure TResourcesDataAdaptor.SetResourceRec(var aRec: TResourceRec; aName, aResName: string;
    aShareLevel: integer);
begin
    with aRec do
    begin
        Valid := true;
        name := aName;
        ResName := aResName;
        ShareLevel := aShareLevel;
    end;
end;

function TResourcesDataAdaptor.IsTableEmpty(): boolean;
begin
    self.SelectAndOpen(STR_SQL_SELECT_ALL, true);
    try
        result := self.DataProvider.IsEmpty();
    finally
        self.Close();
    end;
end;

// ------------------------------------------------------------------------------------------------------------
class procedure TResourcesDataAdaptor.WriteDefaultResources();
// ------------------------------------------------------------------------------------------------------------
const
    INT_NUM_ENTRIES = 2;
var
    xRecs: TResourceRecArray;
    xDA: TResourcesDataAdaptor;
begin

    xDA := TResourcesDataAdaptor.Create();
    try
        // only write default resources into an empty table
        if not xDA.IsTableEmpty() then
            EXIT;

        SetLength(xRecs, INT_NUM_ENTRIES);
        SetResourceRec(xRecs[0], STR_ALLDATASOURCE_NAME, STR_RESNAME_ROBOT, INT_SHARELEVEL_ROBOT);
        SetResourceRec(xRecs[1], STR_ALLDATASOURCE_NAME, STR_RESNAME_DELAY, INT_SHARELEVEL_DELAY);

        xDA.WriteRecs(xRecs);
    finally
        xDA.Free;
    end;
end;


end.
