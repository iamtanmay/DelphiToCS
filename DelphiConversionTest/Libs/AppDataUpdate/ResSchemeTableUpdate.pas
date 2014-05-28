unit ResSchemeTableUpdate;


{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.06.08 pk                               TN4148   FieldNames unit removed
  -------------------------------------------------------------------------------------------------- }
interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TResSchemeTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TResSchemeTableStructDefV1 = class(TResSchemeTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TResSchemeTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils;

const
    INT_REVISION_1 = 1;

    { TResSchemeTableStructDefV0 }

procedure TResSchemeTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'RESSCHEME';
end;

{ TResSchemeTableStructDefV1 }

procedure TResSchemeTableStructDefV1.DoDefineStruct();
begin
    inherited;
    self.AddField('SCHEMEID', tftInteger, 0);
    self.AddField('RESID', tftString, 20);
    self.AddField('USELEVEL', tftInteger, 0);
    self.AddField('RELLEVLEL', tftInteger, 0);

    self.AddIndex('SCHEMEID;RESID;USELEVEL;RELLEVLEL');
end;

constructor TResSchemeTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TResSchemeTableStructDefV0, INT_REVISION_1);
    AlterStructure(TResSchemeTableStructDefV1);
    CopyMatchingFields([]);
end;


end.
