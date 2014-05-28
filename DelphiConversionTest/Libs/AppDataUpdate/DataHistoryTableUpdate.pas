unit DataHistoryTableUpdate;


{ --------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Thomas Schubert (ts)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  16.04.09 ts                               TN4477   initial revision
  -------------------------------------------------------------------------------------------------- }
interface


uses
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TDataHistoryTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TDataHistoryTableStructDefV1 = class(TDataHistoryTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TDataHistoryTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: integer);
    end;


implementation


uses
    SysUtils;

const
    INT_REVISION_1 = 1;

    STR_DATAHISTORY_TBL = 'DATAHISTORY';
    STR_DATAHISTORY_FLD_ID = 'ID';
    STR_DATAHISTORY_FLD_NAME = 'DATAHISTORYNAME';
    STR_DATAHISTORY_FLD_VALUE = 'DATAHISTORYVALUE';

    { TDataHistoryTableStructDefV0 }

procedure TDataHistoryTableStructDefV0.DoDefineStruct();
begin
    inherited;
    fName := STR_DATAHISTORY_TBL;
end;

{ TDataHistoryTableUpdater }

procedure TDataHistoryTableStructDefV1.DoDefineStruct();
begin
    inherited;
    AddField(STR_DATAHISTORY_FLD_ID, tftInteger, 0);
    AddField(STR_DATAHISTORY_FLD_NAME, tftString, 50);
    AddField(STR_DATAHISTORY_FLD_VALUE, tftString, 50);
    AddIndex(STR_DATAHISTORY_FLD_ID);
end;

constructor TDataHistoryTableUpdateV1.Create(aUpdateNumber: integer);
begin
    inherited Create(aUpdateNumber, TDataHistoryTableStructDefV0, INT_REVISION_1);
    AlterStructure(TDataHistoryTableStructDefV1);
    CopyMatchingFields([]);
end;


end.
