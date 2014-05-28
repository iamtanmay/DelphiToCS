{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  08.03.13 wl                                      TN6095   Initial Revision
  14.05.13 wl                                      TN6095   überarbeitet
  ----------------------------------------------------------------------------------------------------------- }

unit MethodVarPagesTableUpdate;


interface


uses
    Generics.Collections,
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TMethodVarPagesTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TMethodVarPagesTableStructDefV1 = class(TMethodVarPagesTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TMethodVarPagesTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils;

const
    INT_REVISION_1 = 1;
    cMethodVarPagesTable = 'METHODVARPAGES';
    cFieldNameMethodName = 'MethodName';
    cFieldNamePage = 'Page';
    cFieldNameFirstOrderIndex = 'FirstOrderIndex';
    cFieldNameLastOrderIndex = 'LastOrderIndex';
    cFieldNameCaption = 'Caption';

    { TMethodVarPagesTableStructDefV0 }

procedure TMethodVarPagesTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := cMethodVarPagesTable;
end;

{ TMethodVarPagesTableStructDefV1 }

procedure TMethodVarPagesTableStructDefV1.DoDefineStruct();
const
    cFieldLengthMethodName = 50;
    cFieldLengthCaption = 100;
begin
    inherited;
    AddField(cFieldNameMethodName, tftString, cFieldLengthMethodName);
    AddField(cFieldNamePage, tftInteger, 0);
    AddField(cFieldNameFirstOrderIndex, tftInteger, 0);
    AddField(cFieldNameLastOrderIndex, tftInteger, 0);
    AddField(cFieldNameCaption, tftString, cFieldLengthCaption);

    AddIndex(cFieldNameMethodName + ';' + cFieldNamePage);
end;

{ TMethodVarPagesTableUpdateV1 }

constructor TMethodVarPagesTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodVarPagesTableStructDefV0, INT_REVISION_1);
    AlterStructure(TMethodVarPagesTableStructDefV1);
    CopyMatchingFields([]);
end;


end.
