unit WorkspaceTableUpdate;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Updater for Workspace Table
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  24.06.08 pk                                TN4148  FieldNames Unit removed
  24.06.08 pk  WriteDefault                  TN4139  Append instead of Edit
  24.06.08 pk  TWorkspaceTableUpdateV1_1     TN4139  New X, Y, Z values also set for default
  16.07.08 wl  TWorkspaceTableUpdateV2       TN4164   Daten aus UsedArms in Tabelle Workspacedevices übertragen
  16.07.08 wl  TWorkspaceTableUpdateV2_1     TN4164   "UsedArms" löschen
  10.09.08 wl  WorkspaceUpdateV2CustomFunc   TN4220   Fehler behoben, der zu Absturz führte
  15.02.11 pk                               	TN4780   changes needed to make UpdateManager compatible with TurboDB
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Classes,
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TWorkspaceTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TWorkspaceTableStructDefV1 = class(TWorkspaceTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TWorkspaceTableStructDefV2_0 = class(TWorkspaceTableStructDefV1)
    protected
        procedure DoDefineStruct(); override;
    end;

    TWorkspaceTableStructDefV2_1 = class(TWorkspaceTableStructDefV2_0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TWorkspaceTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    // Data updates
    TWorkspaceTableUpdateV1_1 = class(TTableUpdate)
    private
        procedure WriteDefault(aSender: TObject);
    protected
        function GetUpdateDescription: string; override;
        procedure DoVersionCheck; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TStringArray = array of string;

    // table updates
    TWorkspaceTableUpdateV2 = class(TTableUpdate)
    private
        function Extract(var S: string; index, Count, Skip: Integer): string;
        function StringToList(aLstResult: TStrings; aStrText, aStrDelimiter: string): boolean;
        procedure InsertIntoWorkspaceDevices(aWorkspaceID: integer; const aArmNames: TStrings);
        procedure WorkspaceUpdateV2CustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TWorkspaceTableUpdateV2_1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils,
    DataProvider;

const
    cRevision1 = 1;
    cWorkspaceMajorVersion1 = 1;
    cWorkspaceMinorVersion1 = 1;
    cWorkspaceMajorVersion2 = 2;

    { TWorkspaceTableStructDefV0 }

procedure TWorkspaceTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'WORKSPACE';
end;

{ TWorkspaceTableStructDefV1 }

procedure TWorkspaceTableStructDefV1.DoDefineStruct();
begin
    inherited;
    self.AddField('ID', tftInteger, 0);
    self.AddField('Name', tftString, 50);
    self.AddField('Arms', tftString, 100);
    self.AddField('UseAllArms', tftBoolean, 0);
    self.AddField('WorldRelation', tftString, 50);
    self.AddField('ViewRelation', tftString, 50);
    self.AddField('X_mm', tftFloat, 0);
    self.AddField('Y_mm', tftFloat, 0);
    self.AddField('Z_mm', tftFloat, 0);
    self.AddField('Color', tftInteger, 0);
    self.AddIndex('ID');
end;

{ TWorkspaceTableStructDefV2_0 }

procedure TWorkspaceTableStructDefV2_0.DoDefineStruct();
begin
    inherited;
    self.AddField('UseAllDevices', tftBoolean, 0);
end;

{ TWorkspaceTableStructDefV2_1 }

procedure TWorkspaceTableStructDefV2_1.DoDefineStruct();
begin
    inherited;
    self.DelField('Arms');
    self.DelField('UseAllArms');
end;

{ TWorkspaceTableUpdateV1 }

constructor TWorkspaceTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TWorkspaceTableStructDefV0, cRevision1);
    AlterStructure(TWorkspaceTableStructDefV1);
    CopyMatchingFields([]);
end;

{ TWorkspaceDataUpdateV1_1 }

constructor TWorkspaceTableUpdateV1_1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TWorkspaceTableStructDefV1, cWorkspaceMajorVersion1,
        cWorkspaceMinorVersion1);
    self.AlterStructure(TWorkspaceTableStructDefV1);
    CopyMatchingFields([]);
    self.CustomDataFunc(WriteDefault);
end;

procedure TWorkspaceTableUpdateV1_1.DoVersionCheck();
var
    xSettingDP: TDataProvider;
begin
    fVersionCheckResult := vcrOK;
    fUpdateReason := 'The workspace "DEFAULT" is missing';

    // if table does not exist, we will do the update
    if not fTableChangeAdaptor.DBPathTableExists('WORKSPACE') then
    begin
        fVersionCheckResult := vcrUpdate;
        EXIT;
    end;

    // if settings table exists but is empty we will do the update
    xSettingDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        xSettingDP.SelectAndOpen('SELECT * FROM WORKSPACE', true);
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

function TWorkspaceTableUpdateV1_1.GetUpdateDescription: string;
begin
    result := 'Add "DEFAULT" workspace';
end;

procedure TWorkspaceTableUpdateV1_1.WriteDefault(aSender: TObject);
var
    xSettingDP: TDataProvider;
begin
    xSettingDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xSettingDP.SelectAndOpen('SELECT * FROM WORKSPACE', false);
        try

            xSettingDP.Append;
            xSettingDP.FieldByName('ID').AsInteger := 1;
            xSettingDP.FieldByName('Name').AsString := 'DEFAULT';
            xSettingDP.FieldByName('UseAllArms').AsBoolean := true;
            xSettingDP.FieldByName('X_mm').AsFloat := 2000;
            xSettingDP.FieldByName('Y_mm').AsFloat := 500;
            xSettingDP.FieldByName('Z_mm').AsFloat := 10;
            xSettingDP.FieldByName('Color').AsInteger := $00808000; // Teal;
            xSettingDP.Post;
        finally
            xSettingDP.Close();
        end;
    finally
        xSettingDP.Free;
    end;
end;

{ TWorkspaceTableUpdateV2 }

constructor TWorkspaceTableUpdateV2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TWorkspaceTableStructDefV1, cWorkspaceMajorVersion2);

    AlterStructure(TWorkspaceTableStructDefV2_0);
    CopyMatchingFields([]);
    self.CopyField('UseAllArms', 'UseAllDevices');

    self.CustomDataFunc(WorkspaceUpdateV2CustomFunc);
end;

function TWorkspaceTableUpdateV2.Extract(var S: string; index, Count, Skip: Integer): string;
// result is index upto and including Count
// S is right side from index + count + skip until end of string
begin
    result := Copy(S, index, Count);
    S := Copy(S, index + Count + Skip, Length(S));
end;

function TWorkspaceTableUpdateV2.StringToList(aLstResult: TStrings; aStrText, aStrDelimiter: string): boolean;
var
    intPos: integer;
begin
    result := true;
    if Length(aStrText) = 0 then
        Exit;
    while true do
    begin
        intPos := Pos(aStrDelimiter, aStrText);
        if intPos = 0 then
        begin
            aLstResult.Add(aStrText);
            Break;
        end;
        aLstResult.Add(Extract(aStrText, 1, intPos - 1, Length(aStrDelimiter)));
    end;
end;

procedure TWorkspaceTableUpdateV2.InsertIntoWorkspaceDevices(aWorkspaceID: integer;
    const aArmNames: TStrings);
var
    xDP: TDataProvider;
    x: integer;
begin
    xDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM WORKSPACEDEVICES', false);
        try
            for x := 0 to aArmNames.Count - 1 do
            begin
                xDP.Insert;
                xDP.FieldByName('WORKSPACEID').AsInteger := aWorkspaceID;
                xDP.FieldByName('DEVICENAME').AsString := aArmNames[x];
                xDP.Post;
            end;
        finally
            xDP.Close();
        end;

    finally
        xDP.Free;
    end;
end;

procedure TWorkspaceTableUpdateV2.WorkspaceUpdateV2CustomFunc(aSender: TObject);
var
    xDP: TDataProvider;
    xArms: string;
    xArmNames: TStringList;
begin
    xDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM WORKSPACE', true);
        try
            while not xDP.Eof do
            begin

                xArms := xDP.FieldByName('Arms').AsString;
                if (xArms <> '') then
                begin

                    xArmNames := TStringList.Create;
                    try
                        self.StringToList(xArmNames, xArms, ',');
                        self.InsertIntoWorkspaceDevices(xDP.FieldByName('ID').AsInteger, xArmNames);
                    finally
                        xArmNames.Free;
                    end;
                end;

                xDP.Next;
            end;
        finally
            xDP.Close();
        end;

    finally
        xDP.Free;
    end;
end;

{ TWorkspaceTableUpdateV2_1 }

constructor TWorkspaceTableUpdateV2_1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TWorkspaceTableStructDefV2_0, cWorkspaceMajorVersion2,
        cWorkspaceMinorVersion1);

    AlterStructure(TWorkspaceTableStructDefV2_1);
    CopyMatchingFields([]);
end;


end.
