unit UpdateStatusTableUpdate;


{ --------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  03.02.11 pk                                        Initial Revision
  -------------------------------------------------------------------------------------------------- }
interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TUpdateStatusTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TUpdateStatusTableStructDefV1 = class(TUpdateStatusTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TUpdateStatusTableUpdateV1 = class(TTableUpdate)
        // protected
        // procedure DoVersionCheck(); override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils;

const
    INT_REVISION_1 = 1;

    { TUpdateStatusTableStructDefV0 }

procedure TUpdateStatusTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'UPDATESTATUS';
end;

{ TUpdateStatusTableStructDefV1 }

procedure TUpdateStatusTableStructDefV1.DoDefineStruct();
begin
    inherited;
    AddField('ID', tftInteger, 0);
    AddField('STATUS', tftSmallInt, 0);
    AddField('DESCRIPTION', tftString, 255);
    AddIndex('ID');
end;

constructor TUpdateStatusTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TUpdateStatusTableStructDefV0, INT_REVISION_1);
    AlterStructure(TUpdateStatusTableStructDefV1);
    CopyMatchingFields([]);
end;

// procedure TUpdateStatusTableUpdateV1.DoVersionCheck();
// begin
//
//
// inherited DoVersionCheck;
// end;


end.
