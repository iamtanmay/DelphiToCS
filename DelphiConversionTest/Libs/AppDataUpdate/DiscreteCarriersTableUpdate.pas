unit DiscreteCarriersTableUpdate;
{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Philipp Peter (pp)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  16.04.13 pp                                      TN6131   Initial Revision
  ----------------------------------------------------------------------------------------------------------- }


interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TDiscreteCarriersTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TDiscreteCarriersTableStructDefV1 = class(TDiscreteCarriersTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TDiscreteCarriersTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils,
    DataProvider;

const
    INT_REVISION_1 = 1;

    { TDiscreteRacksTableStructDefV0 }

procedure TDiscreteCarriersTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'DISCRETECARRIERS';
end;

{ TDiscreteRacksTableStructDefV1 }

procedure TDiscreteCarriersTableStructDefV1.DoDefineStruct();
begin
    inherited;
    self.AddField('Name', tftString, 20);
    self.AddField('Position', tftInteger, 0);
    self.AddField('Pos_X_mm', tftFloat, 0);
    self.AddField('Pos_Y_mm', tftFloat, 0);
    self.AddField('Pos_Z_mm', tftFloat, 0);
    AddIndex('Name;Position');
end;

{ TDiscreteRacksTableUpdateV1 }

constructor TDiscreteCarriersTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TDiscreteCarriersTableStructDefV0, INT_REVISION_1);
    AlterStructure(TDiscreteCarriersTableStructDefV1);
    CopyMatchingFields([]);
end;


end.
