unit ImportDefTableUpdate;


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
    TImportDefTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TImportDefTableStructDefV1 = class(TImportDefTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TImportDefTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils;

const
    INT_REVISION_1 = 1;

    { TImportDefTableStructDefV0 }

procedure TImportDefTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'IMPDEF';
end;

{ TImportDefTableStructDefV1 }

procedure TImportDefTableStructDefV1.DoDefineStruct();
begin
    inherited;
    AddField('NAME', tftString, 50);
    AddField('MODE', tftInteger, 0);
    AddField('TARGETPATHNAME', tftString, 255);
    AddField('SOURCENAME', tftString, 50);
    AddIndex('NAME');
end;

constructor TImportDefTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TImportDefTableStructDefV0, INT_REVISION_1);
    AlterStructure(TImportDefTableStructDefV1);
    CopyMatchingFields([]);
end;


end.
