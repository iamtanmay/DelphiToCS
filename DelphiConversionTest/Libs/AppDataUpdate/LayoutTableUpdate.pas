unit LayoutTableUpdate;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.06.08 pk                               TN4148   FieldNames unit removed
  24.06.08 pk  TLayoutTableUpdateV3         TN4139   New
  02.07.08 pk  TLayoutTableStructDefV3      TN4139   LinkedLayout is also part of index
  29.09.08 wl  TLayoutTableStructDefV4      TN4242   'RUN' (Method name) = 50 chars
  15.02.11 pk                               TN4780   changes needed to make UpdateManager compatible with TurboDB
  -------------------------------------------------------------------------------------------------- }


interface


uses
    TableStructDef,
    TableUpdate;

const
    INT_LAYOUT_MAJORREVISION_1 = 1;
    INT_LAYOUT_MAJORREVISION_2 = 2;
    INT_LAYOUT_MAJORREVISION_3 = 3;
    INT_LAYOUT_MAJORREVISION_4 = 4;
    INT_LAYOUT_MINORREVISION_1 = 1;
    INT_LAYOUT_MINORREVISION_2 = 2;

type
    // table structure definitions
    TLayoutTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TLayoutTableStructDefV1 = class(TLayoutTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TLayoutTableStructDefV2 = class(TLayoutTableStructDefV1)
    protected
        procedure DoDefineStruct(); override;
    end;

    TLayoutTableStructDefV3 = class(TLayoutTableStructDefV2)
    protected
        procedure DoDefineStruct(); override;
    end;

    TLayoutTableStructDefV4 = class(TLayoutTableStructDefV3)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TLayoutTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateID: integer);
    end;

    TLayoutTableUpdateV3 = class(TTableUpdate)
    public
        constructor Create(aUpdateID: integer);
    end;

    TUpdateV3_1Rec = record
        LayoutID: string;
        LayoutWorkspaceID: integer;
    end;

    TUpdateV3_1RecArray = array of TUpdateV3_1Rec;

    TLayoutTableUpdateV3_1 = class(TTableUpdate)
    private
        procedure CustomFunc(aSender: TObject);
        procedure ReadLayoutWorkspaces(var vRecArray: TUpdateV3_1RecArray);
        procedure UpdateWorkspaceIDForCarriers(const aRecArray: TUpdateV3_1RecArray);
    protected
        function GetUpdateDescription: string; override;
        procedure DoVersionCheck; override;
    public
        constructor Create(aUpdateID: integer);
    end;

    TLayoutTableUpdateV3_2 = class(TTableUpdate)
    private
        procedure CustomFunc(aSender: TObject);
    protected
        function GetUpdateDescription: string; override;
        procedure DoVersionCheck; override;
    public
        constructor Create(aUpdateID: integer);
    end;

    TLayoutTableUpdateV4 = class(TTableUpdate)
    public
        constructor Create(aUpdateID: integer);
    end;


implementation


uses
    SysUtils,
    Dialogs,
    DataProvider,
    TableChangeAdaptor;

const
    cLayoutTableNameV0 = 'LAYOUT';

    { TLayoutTableStructDefV0 }

procedure TLayoutTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := cLayoutTableNameV0;
end;

{ TLayoutTableStructDefV1 }

procedure TLayoutTableStructDefV1.DoDefineStruct();
begin
    inherited;
    AddField('RUN', tftString, 25);
    AddField('NAME', tftString, 20);
    AddField('RACKNAME', tftString, 30);
    AddField('CARRIERNAME', tftString, 20);
    AddField('RACKID', tftString, 20);
    AddField('SLOT', tftSmallint, 0);
    AddField('RACKTYP', tftString, 20);
    AddField('CARR_X_STEPS', tftInteger, 0);
    AddField('CARR_Y_STEPS', tftInteger, 0);
    AddField('CARR_Z_STEPS', tftInteger, 0);
    AddField('CARRIER', tftString, 20);
    AddIndex('RUN;NAME;RACKNAME;CARRIERNAME;RACKID');

end;

{ TLayoutTableStructDefV2 }

procedure TLayoutTableStructDefV2.DoDefineStruct();
begin
    inherited;
    DelField('CARR_X_STEPS');
    DelField('CARR_Y_STEPS');
    DelField('CARR_Z_STEPS');
    AddFieldAt('CARR_X', tftFloat, 0, 7);
    AddFieldAt('CARR_Y', tftFloat, 0, 8);
    AddFieldAt('CARR_Z', tftFloat, 0, 9);

end;

procedure TLayoutTableStructDefV3.DoDefineStruct();
begin
    inherited;
    AddFieldAt('WORKSPACEID', tftInteger, 0, 2);
    AddFieldAt('LINKEDLAYOUT', tftString, 20, 6);
    AddIndex('RUN;NAME;WORKSPACEID;RACKNAME;CARRIERNAME;RACKID;LINKEDLAYOUT');
end;

{ TLayoutTableStructDefV4 }

procedure TLayoutTableStructDefV4.DoDefineStruct;
begin
    inherited;
    self.ResizeField('RUN', 50);
end;

{ TLayoutTableUpdateV1 }

constructor TLayoutTableUpdateV1.Create(aUpdateID: integer);
begin
    inherited Create(aUpdateID, TLayoutTableStructDefV0, INT_LAYOUT_MAJORREVISION_1);
    AlterStructure(TLayoutTableStructDefV1);
    CopyMatchingFields([]);
end;

{ TLayoutTableUpdateV3 }

constructor TLayoutTableUpdateV3.Create(aUpdateID: integer);
begin
    inherited Create(aUpdateID, TLayoutTableStructDefV2, INT_LAYOUT_MAJORREVISION_3);
    AlterStructure(TLayoutTableStructDefV3);
    CopyMatchingFields([]);
end;

{ TLayoutTableUpdateV3_1 }

constructor TLayoutTableUpdateV3_1.Create(aUpdateID: integer);
begin
    inherited Create(aUpdateID, TLayoutTableStructDefV3, INT_LAYOUT_MAJORREVISION_3,
        INT_LAYOUT_MINORREVISION_1);
    AlterStructure(TLayoutTableStructDefV3);
    CopyMatchingFields([]);
    self.CustomDataFunc(CustomFunc);
end;

procedure TLayoutTableUpdateV3_1.ReadLayoutWorkspaces(var vRecArray: TUpdateV3_1RecArray);
var
    xDP: TDataProvider;
    x: integer;
begin
    xDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try

        xDP.SelectAndOpen('SELECT * FROM LayoutWorkspace', true);
        try
            x := 0;
            SetLength(vRecArray, xDP.RecordCount);
            while not xDP.Eof do
            begin
                vRecArray[x].LayoutWorkspaceID := xDP.FieldByName('ID').AsInteger;
                vRecArray[x].LayoutID := xDP.FieldByName('LayoutID').AsString;
                xDP.Next;
                Inc(x);
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;

end;

procedure TLayoutTableUpdateV3_1.UpdateWorkspaceIDForCarriers(const aRecArray: TUpdateV3_1RecArray);
var
    xLayoutDP: TDataProvider;
    x: integer;
begin
    xLayoutDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        for x := 0 to high(aRecArray) do
        begin
            xLayoutDP.ExecSQL('UPDATE ' + cLayoutTableNameV0 + ' SET ' + TSQL.FieldEq(cLayoutTableNameV0,
                'WorkspaceID', aRecArray[x].LayoutWorkspaceID) + ' WHERE ' + TSQL.FieldEq(cLayoutTableNameV0,
                'Name', aRecArray[x].LayoutID) + ' AND ' + TSQL.FieldNotNull(cLayoutTableNameV0, 'Carrier') +
                ' AND ' + TSQL.FieldIsNull(cLayoutTableNameV0, 'WorkspaceID'));
        end;
    finally
        xLayoutDP.Free;
    end;

end;

procedure TLayoutTableUpdateV3_1.CustomFunc(aSender: TObject);
var
    xRecArray: TUpdateV3_1RecArray;
begin
    ReadLayoutWorkspaces(xRecArray);

    UpdateWorkspaceIDForCarriers(xRecArray);
end;

procedure TLayoutTableUpdateV3_1.DoVersionCheck();
begin
    inherited;
    fUpdateReason := 'No WorkspaceIDs were found';
end;

function TLayoutTableUpdateV3_1.GetUpdateDescription: string;
begin
    result := 'Set WorkspaceID for each carrier';
end;

{ TLayoutTableUpdateV3_2 }

constructor TLayoutTableUpdateV3_2.Create(aUpdateID: integer);
begin
    inherited Create(aUpdateID, TLayoutTableStructDefV3, INT_LAYOUT_MAJORREVISION_3,
        INT_LAYOUT_MINORREVISION_2);
    AlterStructure(TLayoutTableStructDefV3);
    CopyMatchingFields([]);
    self.CustomDataFunc(CustomFunc);
end;

procedure TLayoutTableUpdateV3_2.CustomFunc(aSender: TObject);
begin
    // InputQuery( 'Workbench Y Offset Value'
end;

procedure TLayoutTableUpdateV3_2.DoVersionCheck;
begin
    inherited;

end;

function TLayoutTableUpdateV3_2.GetUpdateDescription: string;
begin
    result := 'Change the carrier Y values';
end;

{ TLayoutTableUpdateV4 }

constructor TLayoutTableUpdateV4.Create(aUpdateID: integer);
begin
    inherited Create(aUpdateID, TLayoutTableStructDefV3, INT_LAYOUT_MAJORREVISION_4);
    AlterStructure(TLayoutTableStructDefV4);
    CopyMatchingFields([]);
end;


end.
