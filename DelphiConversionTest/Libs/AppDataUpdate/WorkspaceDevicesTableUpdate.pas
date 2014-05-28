unit WorkspaceDevicesTableUpdate;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  class.method/member                    track-no improvement/change
  -------- --  -------------------------------------  -------- -------------------------------------
  10.07.08 wl                                         TN4164   intial revision
  16.07.08 wl                                         TN4164   removed: CopyMatchingFields
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TWorkspaceDevicesTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TWorkspaceDevicesTableStructDefV1 = class(TWorkspaceDevicesTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TWorkspaceDevicesTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils;

const
    INT_REVISION_1 = 1;

    { TWorkspaceDevicesTableStructDefV0 }

procedure TWorkspaceDevicesTableStructDefV0.DoDefineStruct;
begin
    inherited;

    fName := 'WORKSPACEDEVICES';
end;

{ TWorkspaceDevicesTableStructDefV1 }

procedure TWorkspaceDevicesTableStructDefV1.DoDefineStruct();
begin
    inherited;

    self.AddField('WORKSPACEID', tftInteger, 0);
    self.AddField('DEVICENAME', tftString, 40);

    // Index
    self.AddIndex('WORKSPACEID;DEVICENAME');
end;

constructor TWorkspaceDevicesTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TWorkspaceDevicesTableStructDefV0, INT_REVISION_1);

    AlterStructure(TWorkspaceDevicesTableStructDefV1);
end;


end.
