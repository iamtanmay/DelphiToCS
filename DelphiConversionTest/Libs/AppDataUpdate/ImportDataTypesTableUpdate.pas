unit ImportDataTypesTableUpdate;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Thomas Schubert (ts)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  19.02.09 ts                               TN4346   initial revision
  27.05.09 wl  TImportDataTypesTableStructDefV2  TN4476   Feld IMPORTFILENAME entfernt
  -------------------------------------------------------------------------------------------------- }


interface


uses
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TImportDataTypesTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TImportDataTypesTableStructDefV1 = class(TImportDataTypesTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TImportDataTypesTableStructDefV2 = class(TImportDataTypesTableStructDefV1)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TImportDataTypesTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: integer);
    end;

    TImportDataTypesTableUpdateV2 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: integer);
    end;


implementation


uses
    SysUtils;

const
    INT_REVISION_1 = 1;
    INT_REVISION_2 = 2;

    STR_IMPDATATYPES_TBL = 'IMPDATATYPES';
    STR_IMPDATATYPES_FLD_INDEX = 'INDEX';
    STR_IMPDATATYPES_FLD_FILENAME = 'IMPORTFILENAME';
    STR_IMPDATATYPES_FLD_DEFNAME = 'IMPORTDEFNAME';
    STR_IMPDATATYPES_FLD_COLUMN = 'IMPORTCOLUMN';
    STR_IMPDATATYPES_FLD_TYPE = 'IMPORTTYPE';

    { TImportDataTypesTableStructDefV0 }

procedure TImportDataTypesTableStructDefV0.DoDefineStruct();
begin
    inherited;
    fName := STR_IMPDATATYPES_TBL;
end;

{ TImportDataTypesTableStructDefV1 }

procedure TImportDataTypesTableStructDefV1.DoDefineStruct();
begin
    inherited;
    AddField(STR_IMPDATATYPES_FLD_INDEX, tftAutoInc, 0);
    AddField(STR_IMPDATATYPES_FLD_FILENAME, tftString, 50);
    AddField(STR_IMPDATATYPES_FLD_DEFNAME, tftString, 50);
    AddField(STR_IMPDATATYPES_FLD_COLUMN, tftString, 50);
    AddField(STR_IMPDATATYPES_FLD_TYPE, tftString, 50);
    AddIndex(STR_IMPDATATYPES_FLD_INDEX);
end;

{ TImportDataTypesTableStructDefV2 }

procedure TImportDataTypesTableStructDefV2.DoDefineStruct();
begin
    inherited;
    DelField(STR_IMPDATATYPES_FLD_FILENAME);
end;

{ TImportDataTypesTableUpdateV1 }

constructor TImportDataTypesTableUpdateV1.Create(aUpdateNumber: integer);
begin
    inherited Create(aUpdateNumber, TImportDataTypesTableStructDefV0, INT_REVISION_1);
    AlterStructure(TImportDataTypesTableStructDefV1);
    CopyMatchingFields([]);
end;

{ TImportDataTypesTableUpdateV2 }

constructor TImportDataTypesTableUpdateV2.Create(aUpdateNumber: integer);
begin
    inherited Create(aUpdateNumber, TImportDataTypesTableStructDefV1, INT_REVISION_2);
    AlterStructure(TImportDataTypesTableStructDefV2);
    CopyMatchingFields([]);
end;


end.
