unit CarrierTableUpdate;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.06.08 pk                               TN4148   FieldNames unit removed
  09.06.09 pk  TCarrierTableStructDefV1     TN4587   YSTARTBEFOREX corrected to HTUBE_YSTARTBEFOREX
  16.04.13 pp  TCarrierTableStuctDefV3      TN6131   DiscreteCarrier hinzufügen
  -------------------------------------------------------------------------------------------------- }


interface


uses
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TCarrierTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TCarrierTableStructDefV1 = class(TCarrierTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TCarrierTableStructDefV2 = class(TCarrierTableStructDefV1)
    protected
        procedure DoDefineStruct(); override;
    end;

    TCarrierTableStructDefV3 = class(TCarrierTableStructDefV2)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TCarrierTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: integer);
    end;

    // table updates
    TCarrierTableUpdateV2 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: integer);
    end;

    // table updates
    TCarrierTableUpdateV3 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: integer);
    end;


implementation


const
    INT_REVISION_1 = 1;
    INT_REVISION_2 = 2;
    INT_REVISION_3 = 3;

    { TCarrierTableStructDefV0 }

procedure TCarrierTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'CARRIER';
end;

{ TCarrierTableStructV1 }

procedure TCarrierTableStructDefV1.DoDefineStruct();
begin
    inherited;
    AddField('NAME', tftString, 20);
    AddField('TYP', tftInteger, 0);
    AddField('X_MM', tftFloat, 0);
    AddField('Y_MM', tftFloat, 0);
    AddField('Z_MM', tftFloat, 0);
    AddField('ROWS', tftInteger, 0);
    AddField('COLS', tftInteger, 0);
    AddField('SLOTS', tftInteger, 0);
    AddField('SLOTX_MM', tftFloat, 0);
    AddField('SLOTY_MM', tftFloat, 0);

    AddField('SLOTX_FIRST_MM', tftFloat, 0);
    AddField('SLOTY_FIRST_MM', tftFloat, 0);
    AddField('SLOTZ_FIRST_MM', tftFloat, 0);
    AddField('SLOTX_LAST_MM', tftFloat, 0);
    AddField('SLOTY_LAST_MM', tftFloat, 0);
    AddField('SLOTZ_LAST_MM', tftFloat, 0);
    AddField('SLOTZ_CALCULATE', tftBoolean, 0);
    AddField('SLOTROTATION_DEGREE', tftFloat, 0);
    AddField('STACKERBUTTONS', tftInteger, 0);

    AddField('H_XSTART_MM', tftFloat, 0);
    AddField('H_YSTART_MM', tftFloat, 0);
    AddField('H_ZSTART_MM', tftFloat, 0);
    AddField('H_RSTART_DEGREE', tftFloat, 0);
    AddField('H_XRETAKE_MM', tftFloat, 0);
    AddField('H_YRETAKE_MM', tftFloat, 0);
    AddField('H_ZRETAKE_MM', tftFloat, 0);
    AddField('H_RRETAKE_DEGREE', tftFloat, 0);
    AddField('H_ZPUT_MM', tftFloat, 0);
    AddField('H_XPRESTART_MM', tftFloat, 0);
    AddField('H_DIRECTIONTURNTYPE', tftInteger, 0);

    AddField('HTUBE_USEVALUES', tftBoolean, 0);
    AddField('HTUBE_XSTART_MM', tftFloat, 0);
    AddField('HTUBE_YSTART_MM', tftFloat, 0);
    AddField('HTUBE_ZSTART_MM', tftFloat, 0);
    AddField('HTUBE_RSTART_DEGREE', tftFloat, 0);
    AddField('HTUBE_YSTARTBEFOREX', tftBoolean, 0);

    AddIndex('NAME');
end;

{ TCarrierTableStructV2 }

procedure TCarrierTableStructDefV2.DoDefineStruct();
begin
    inherited;
    AddField('HTUBE_KEEPROTATION', tftBoolean, 0);
end;

{ TCarrierTableStructDefV3 }

procedure TCarrierTableStructDefV3.DoDefineStruct;
begin
    inherited;
    AddField('DISCRETEPOSITIONS', tftBoolean, 0);
end;

{ TCarrierTableUpdateV1 }

constructor TCarrierTableUpdateV1.Create(aUpdateNumber: integer);
begin
    inherited Create(aUpdateNumber, TCarrierTableStructDefV0, INT_REVISION_1);
    AlterStructure(TCarrierTableStructDefV1);
    CopyMatchingFields([]);
end;

{ TCarrierTableUpdateV2 }

constructor TCarrierTableUpdateV2.Create(aUpdateNumber: integer);
begin
    inherited Create(aUpdateNumber, TCarrierTableStructDefV1, INT_REVISION_2);
    AlterStructure(TCarrierTableStructDefV2);
    CopyMatchingFields([]);
end;


{ TCarrierTableUpdateV3 }

constructor TCarrierTableUpdateV3.Create(aUpdateNumber: integer);
begin
    inherited Create(aUpdateNumber, TCarrierTableStructDefV2, INT_REVISION_3);
    AlterStructure(TCarrierTableStructDefV3);
    CopyMatchingFields([]);
end;

end.
