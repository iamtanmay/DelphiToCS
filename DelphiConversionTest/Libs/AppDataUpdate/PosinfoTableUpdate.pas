{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  20.10.11 wl                                      TN5723   Initial Revision
  28.10.11 wl  TPosinfoTableUpdateV1_1             TN5728   Disposables-Einträge (Step is null) werden gelöscht
  28.10.11 wl  TPosinfoTableUpdateV1_1             TN5729   Substances-Einträge (Step = 0) werden gelöscht
  31.10.11 wl  TPosinfoTableStructDefV1            TN5725   SUBSTCOLOR -> SubstanceData
  03.05.12 wl  TPosinfoTableStructDefV1            TN5886   Reihenfolge der Felder wieder wie früher
  ----------------------------------------------------------------------------------------------------------- }

unit PosinfoTableUpdate;


interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TPosinfoTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TPosinfoTableStructDefV1 = class(TPosinfoTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TPosinfoTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    // table updates
    TPosinfoTableUpdateV1_1 = class(TTableUpdate)
    strict private
        procedure DeleteDisposables(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils,
    DataProvider;

const
    INT_REVISION_1 = 1;
    INT_MINORREVISION_1 = 1;

    { TPosinfoTableStructDefV0 }

procedure TPosinfoTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'POSINFO';
end;

{ TPosinfoTableStructDefV1 }

procedure TPosinfoTableStructDefV1.DoDefineStruct();
begin
    inherited;
    AddField('RACKID', tftString, 20);
    AddField('POS', tftInteger, 0);
    AddField('STEP', tftInteger, 0);
    AddField('SUBSTID', tftString, 40);
    AddField('DATETIME', tftDateTime, 0);
    AddField('ORIGIN', tftString, 80);
    AddField('AMOUNT', tftFloat, 0);
    AddField('UNIT', tftInteger, 0);
    AddIndex('RACKID;POS;STEP;SUBSTID');
end;

{ TPosinfoTableUpdateV1 }

constructor TPosinfoTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TPosinfoTableStructDefV0, INT_REVISION_1);
    AlterStructure(TPosinfoTableStructDefV1);
    CopyMatchingFields([]);
end;

{ TPosinfoTableUpdateV1_1 }

constructor TPosinfoTableUpdateV1_1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TPosinfoTableStructDefV0, INT_REVISION_1, INT_MINORREVISION_1);
    AlterStructure(TPosinfoTableStructDefV1);
    CopyMatchingFields([]);

    self.CustomDataFunc(DeleteDisposables);
end;

procedure TPosinfoTableUpdateV1_1.DeleteDisposables(aSender: TObject);
var
    xPosinfoDP: TDataProvider;
begin
    xPosinfoDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xPosinfoDP.ExecSQL('delete from POSINFO where posinfo.step is null');
        xPosinfoDP.ExecSQL('delete from POSINFO where posinfo.step = 0');
    finally
        xPosinfoDP.Free;
    end;
end;


end.
