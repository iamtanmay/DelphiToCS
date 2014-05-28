unit DiscreteRacksTableUpdate;

{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Thomas Schubert (ts)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  17.11.11 ts                                      TN5702   Initial Revision
  ----------------------------------------------------------------------------------------------------------- }


interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TDiscreteRacksTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TDiscreteRacksTableStructDefV1 = class(TDiscreteRacksTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TDiscreteRacksTableUpdateV1 = class(TTableUpdate)
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

procedure TDiscreteRacksTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'DISCRETERACKS';
end;

{ TDiscreteRacksTableStructDefV1 }

procedure TDiscreteRacksTableStructDefV1.DoDefineStruct();
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

constructor TDiscreteRacksTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TDiscreteRacksTableStructDefV0, INT_REVISION_1);
    AlterStructure(TDiscreteRacksTableStructDefV1);
    CopyMatchingFields([]);
end;


end.
