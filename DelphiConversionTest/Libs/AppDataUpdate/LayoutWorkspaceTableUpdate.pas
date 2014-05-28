{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Updater for Layout Workspace Table
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no improvement/change
  -------- --  -----------------------------------  -------- ------------------------------------------------------------
  24.06.08 pk                                        TN4139  Initial Revision
  15.02.11 pk                               	    TN4780   changes needed to make UpdateManager compatible with TurboDB
  11.04.11 wl                                      TN5549   uses Fileutilities
  ----------------------------------------------------------------------------------------------------------------------- }

unit LayoutWorkspaceTableUpdate;


interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TLayoutWorkspaceTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TLayoutWorkspaceTableStructDefV1 = class(TLayoutWorkspaceTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TLayoutWorkspaceTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    // Data updates
    TLayoutWorkspaceTableUpdateV1_1 = class(TTableUpdate)
    private
        procedure CustomFunc(aSender: TObject);
        procedure ReadLayoutNames(var vLayoutNames: TArray<string>);
        procedure WriteLayoutWorkspaces(const aLayoutNames: TArray<string>);
    protected
        function GetUpdateDescription: string; override;
        procedure DoVersionCheck; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils,
    DataProvider;

const
    cRevision1 = 1;
    cLayoutWorkspaceMajorVersion1 = 1;
    cLayoutWorkspaceMinorVersion1 = 1;

    { TLayoutWorkspaceTableStructDefV0 }

procedure TLayoutWorkspaceTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'LAYOUTWORKSPACE';
end;

{ TLayoutWorkspaceTableStructDefV1 }

procedure TLayoutWorkspaceTableStructDefV1.DoDefineStruct();
begin
    inherited;
    self.AddField('ID', tftInteger, 0);
    self.AddField('LayoutID', tftString, 20);
    self.AddField('Name', tftString, 50);
    self.AddField('WorkspaceTypeID', tftInteger, 0);
    self.AddField('ViewRelation', tftString, 50);
    self.AddIndex('ID');
end;

constructor TLayoutWorkspaceTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLayoutWorkspaceTableStructDefV0, cRevision1);
    AlterStructure(TLayoutWorkspaceTableStructDefV1);
    CopyMatchingFields([]);
end;

{ TLayoutWorkspaceDataUpdateV1_1 }

constructor TLayoutWorkspaceTableUpdateV1_1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLayoutWorkspaceTableStructDefV1, cLayoutWorkspaceMajorVersion1,
        cLayoutWorkspaceMinorVersion1);
    self.AlterStructure(TLayoutWorkspaceTableStructDefV1);
    CopyMatchingFields([]);
    self.CustomDataFunc(CustomFunc);
end;

procedure TLayoutWorkspaceTableUpdateV1_1.DoVersionCheck();
var
    xSettingDP: TDataProvider;
begin
    fVersionCheckResult := vcrOK;
    fUpdateReason := 'No LayoutWorkspaces were found';

    // if table does not exist, we will do the update
    if not fTableChangeAdaptor.DBPathTableExists('LayoutWorkspace') then
    begin
        fVersionCheckResult := vcrUpdate;
        EXIT;
    end;

    // if settings table exists but is empty we will do the update
    xSettingDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        xSettingDP.SelectAndOpen('SELECT * FROM LayoutWorkspace', true);
        try
            if not xSettingDP.IsEmpty then
                EXIT;
            fVersionCheckResult := vcrUpdate;
        finally
            xSettingDP.Close();
        end;
    finally
        xSettingDP.Free;
    end;
end;

function TLayoutWorkspaceTableUpdateV1_1.GetUpdateDescription: string;
begin
    result := 'Add a LayoutWorkspace for each existing layout';
end;

procedure TLayoutWorkspaceTableUpdateV1_1.ReadLayoutNames(var vLayoutNames: TArray<string>);
var
    xLayoutDP: TDataProvider;
    x: integer;
begin
    xLayoutDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try

        xLayoutDP.SelectAndOpen('SELECT DISTINCT Name FROM LAYOUT WHERE Run IS NULL', true);
        try
            x := 0;
            SetLength(vLayoutNames, xLayoutDP.RecordCount);
            while not xLayoutDP.Eof do
            begin
                vLayoutNames[x] := xLayoutDP.FieldByName('Name').AsString;
                xLayoutDP.Next;
                Inc(x);
            end;
        finally
            xLayoutDP.Close();
        end;
    finally
        xLayoutDP.Free;
    end;

end;

procedure TLayoutWorkspaceTableUpdateV1_1.WriteLayoutWorkspaces(const aLayoutNames: TArray<string>);
const
    cWorkspaceTypeIDForDefault = 1;
var
    xDP: TDataProvider;
    x: integer;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM LayoutWorkspace', false);
        try
            for x := 0 to high(aLayoutNames) do
            begin
                xDP.Append;
                xDP.FieldByName('ID').AsInteger := x + 1;
                xDP.FieldByName('LayoutID').AsString := aLayoutNames[x];
                xDP.FieldByName('Name').AsString := 'DEFAULT_1';
                xDP.FieldByName('WorkspaceTypeID').AsInteger := cWorkspaceTypeIDForDefault;
                xDP.Post;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

procedure TLayoutWorkspaceTableUpdateV1_1.CustomFunc(aSender: TObject);
var
    xRecArray: TArray<string>;
begin
    ReadLayoutNames(xRecArray);

    WriteLayoutWorkspaces(xRecArray);
end;


end.
