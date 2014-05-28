{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.06.08 pk                               TN4148   FieldNames unit removed
  12.04.11 ts  TableStructDefV2             TN5548   new field: SKIPLINES
  22.07.11 wl  TImportFileDefTableUpdateV2_1  TN5619   wenn dbType = Standard, muss es ein SQL sein
  -------------------------------------------------------------------------------------------------- }

unit ImportFileDefTableUpdate;


interface


uses
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TImportFileDefTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TImportFileDefTableStructDefV1 = class(TImportFileDefTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TImportFileDefTableStructDefV2 = class(TImportFileDefTableStructDefV1)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TImportFileDefTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: integer);
    end;

    TImportFileDefTableUpdateV2 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: integer);
    end;

    TImportFileDefTableUpdateV2_1 = class(TTableUpdate)
    strict private
        procedure UpdateCustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: integer);
    end;


implementation


uses
    SysUtils,
    DataProvider;

const
    INT_REVISION_1 = 1;
    INT_REVISION_2 = 2;
    INT_MINORREVISION_1 = 1;

    { TImportFileDefTableStructDefV0 }

procedure TImportFileDefTableStructDefV0.DoDefineStruct();
begin
    inherited;
    fName := 'IMPFILE';
end;

{ TImportFileDefTableUpdater }

procedure TImportFileDefTableStructDefV1.DoDefineStruct();
begin
    inherited;
    AddField('NAME', tftString, 50);
    AddField('DATABASETYPE', tftInteger, 0);
    AddField('PATHNAME', tftString, 255);
    AddField('TABLENAME', tftString, 100);
    AddField('DELIMITER', tftString, 5);
    AddField('FILTER', tftString, 255);
    AddField('ORDERBY', tftString, 255);
    AddField('HASHEADER', tftBoolean, 0);
    AddField('ROWOFFSET', tftInteger, 0);
    AddField('USERNAME', tftString, 50);
    AddField('PASSWORD', tftString, 50);
    AddField('SQL', tftMemo, 230);
    AddIndex('NAME');
end;

procedure TImportFileDefTableStructDefV2.DoDefineStruct();
begin
    inherited;
    AddField('SKIPLINES', tftInteger, 0);
end;

constructor TImportFileDefTableUpdateV1.Create(aUpdateNumber: integer);
begin
    inherited Create(aUpdateNumber, TImportFileDefTableStructDefV0, INT_REVISION_1);
    AlterStructure(TImportFileDefTableStructDefV1);
    CopyMatchingFields([]);
end;

constructor TImportFileDefTableUpdateV2.Create(aUpdateNumber: integer);
begin
    inherited Create(aUpdateNumber, TImportFileDefTableStructDefV1, INT_REVISION_2);
    AlterStructure(TImportFileDefTableStructDefV2);
    CopyMatchingFields([]);
end;

constructor TImportFileDefTableUpdateV2_1.Create(aUpdateNumber: integer);
begin
    inherited Create(aUpdateNumber, TImportFileDefTableStructDefV2, INT_REVISION_2, INT_MINORREVISION_1);
    AlterStructure(TImportFileDefTableStructDefV2);
    CopyMatchingFields([]);
    self.CustomDataFunc(UpdateCustomFunc);
end;

procedure TImportFileDefTableUpdateV2_1.UpdateCustomFunc(aSender: TObject);
var
    xDP: TDataProvider;
    xPathName, xSQL: string;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('select * from IMPFILE', false);
        try
            while not xDP.Eof do
            begin
                try
                    if (xDP.FieldByName('DATABASETYPE').AsInteger <> 2) then
                        CONTINUE;

                    xSQL := xDP.FieldByName('SQL').AsString;
                    if (xSQL <> '') then
                        CONTINUE;

                    xPathName := xDP.FieldByName('PATHNAME').AsString;
                    if (xPathName = '') then
                        CONTINUE;

                    xDP.Edit;
                    xDP.FieldByName('SQL').AsString := 'select * from "' + xPathName + '"';
                    xDP.FieldByName('PATHNAME').AsString := '';
                    xDP.Post;
                finally
                    xDP.Next;
                end;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;


end.
