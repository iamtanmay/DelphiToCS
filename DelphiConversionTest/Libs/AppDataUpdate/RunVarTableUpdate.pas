{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.06.08 pk                               TN4148   FieldNames unit removed
  29.09.08 wl  TRunVarTableStructDefV2      TN4242   'RUN' (Method name) = 50 chars
  14.12.12 wl  TRunVarTableUpdateV3         TN6054   'VAL' ist jetzt Memo, Priority ist integer, nutzlose Felder sind raus
  -------------------------------------------------------------------------------------------------- }

unit RunVarTableUpdate;


interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TRunVarTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TRunVarTableStructDefV1 = class(TRunVarTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TRunVarTableStructDefV2 = class(TRunVarTableStructDefV1)
    protected
        procedure DoDefineStruct(); override;
    end;

    TRunVarTableStructDefV3 = class(TRunVarTableStructDefV2)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TRunVarTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TRunVarTableUpdateV2 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TRunVarTableUpdateV3 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils;

const
    INT_REVISION_1 = 1;
    INT_REVISION_2 = 2;
    INT_REVISION_3 = 3;

    { TRunVarTableStructDefV0 }

procedure TRunVarTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'RUNVAR';
end;

{ TRunVarTableStructDefV1 }

procedure TRunVarTableStructDefV1.DoDefineStruct();
begin
    inherited;
    self.AddField('RUN', tftString, 25);
    self.AddField('PRIORITY', tftString, 8);
    self.AddField('IDENT', tftString, 60);
    self.AddField('VAL', tftString, 100);
    self.AddField('DEFVAL', tftString, 100);
    self.AddField('MAXVAL', tftString, 100);
    self.AddField('MINVAL', tftString, 100);
    self.AddField('VALFORMAT', tftString, 100);
    self.AddField('PICKLIST', tftString, 100);
    self.AddField('DESCRIPTION', tftString, 200);
    self.AddField('VALCHANGETIME', tftDateTime, 0);
    self.AddIndex('RUN;PRIORITY;IDENT');
end;

{ TRunVarTableStructDefV2 }

procedure TRunVarTableStructDefV2.DoDefineStruct;
begin
    inherited;
    self.ResizeField('RUN', 50);
end;

{ TRunVarTableStructDefV3 }

procedure TRunVarTableStructDefV3.DoDefineStruct;
begin
    inherited;
    self.DelField('PRIORITY');
    self.DelField('VAL');
    self.DelField('DEFVAL');
    self.DelField('MAXVAL');
    self.DelField('MINVAL');
    self.DelField('VALFORMAT');
    self.DelField('PICKLIST');
    self.DelField('DESCRIPTION');
    self.AddFieldAt('PRIORITY', tftInteger, 0, 1);
    self.AddFieldAt('VAL', tftMemo, 0, 3);
end;

{ TRunVarTableUpdateV1 }

constructor TRunVarTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TRunVarTableStructDefV0, INT_REVISION_1);
    AlterStructure(TRunVarTableStructDefV1);
    CopyMatchingFields([]);
end;

{ TRunVarTableUpdateV2 }

constructor TRunVarTableUpdateV2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TRunVarTableStructDefV1, INT_REVISION_2);
    AlterStructure(TRunVarTableStructDefV2);
    CopyMatchingFields([]);
end;

{ TRunVarTableUpdateV3 }

constructor TRunVarTableUpdateV3.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TRunVarTableStructDefV2, INT_REVISION_3);
    AlterStructure(TRunVarTableStructDefV3);
    CopyMatchingFields([]);
end;


end.
