{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Thomas Schubert (ts)
  Description  : Change datatypes of *.txt and *.csv import files
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  09.02.09 ts                               TN4346   initial revision
  27.05.09 wl  TImportDataTypesDataAdaptor  TN4476   Feld IMPORTFILENAME entfernt
  27.05.09 wl  TImportDataTypesDataAdaptor  TN4476   Funktionen überarbeitet
  17.06.10 pk                               TN5152.1  Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  -------------------------------------------------------------------------------------------------- }

unit ImportDataTypesDataAdaptor;


interface


uses
    DataProvider,
    QueryDataAdaptor,
    ImportFileDefDataAdaptor,
    ImportColDefDataAdaptor;

type
    TDataTypesRec = record
        Column, DataType: string;
    end;

    TDataTypesRecArray = array of TDataTypesRec;

    TImportDataTypesDataAdaptor = class(TQueryDataAdaptor)
    public
        class function ImportDefExists(aImportDefName: string): boolean;
        class procedure DatasetToTable(const aImportDefName: string;
            const aDataTypesRecArray: TDataTypesRecArray);
        class function ImpTableToDataTypeRec(aImportDefName: string): TDataTypesRecArray;
        class procedure DeleteImportDef(aImportDefName: string);
        class procedure CopyImportDef(aImportDefToCopy, aNewImportDef: string);
    end;


implementation


uses
    SysUtils,
    Variants;

const
    STR_IMPDATATYPES_TBL = 'IMPDATATYPES';
    STR_IMPDATATYPES_FLD_INDEX = 'INDEX';
    STR_IMPDATATYPES_FLD_DEFNAME = 'IMPORTDEFNAME';
    STR_IMPDATATYPES_FLD_COLUMN = 'IMPORTCOLUMN';
    STR_IMPDATATYPES_FLD_TYPE = 'IMPORTTYPE';

    { TImportDataTypesDataAdaptor }

class procedure TImportDataTypesDataAdaptor.DatasetToTable(const aImportDefName: string;
    const aDataTypesRecArray: TDataTypesRecArray);
var
    i: integer;
    xDataAdaptor: TImportDataTypesDataAdaptor;
begin
    if Length(aDataTypesRecArray) < 1 then
        EXIT;

    xDataAdaptor := TImportDataTypesDataAdaptor.Create(STR_IMPDATATYPES_TBL);
    try
        xDataAdaptor.ExecSQL('Delete from ' + STR_IMPDATATYPES_TBL + ' where ' + STR_IMPDATATYPES_FLD_DEFNAME
            + ' = ''' + aImportDefName + '''');
        xDataAdaptor.SelectAndOpen('Select * from ' + STR_IMPDATATYPES_TBL + ' where ' +
            STR_IMPDATATYPES_FLD_DEFNAME + ' = ''' + aImportDefName + '''', false);

        for i := 0 to length(aDataTypesRecArray) - 1 do
        begin
            xDataAdaptor.DataProvider.Append;
            xDataAdaptor.DataProvider.FieldByName(STR_IMPDATATYPES_FLD_DEFNAME).AsString := aImportDefName;
            xDataAdaptor.DataProvider.FieldByName(STR_IMPDATATYPES_FLD_COLUMN).AsString :=
                aDataTypesRecArray[i].Column;
            xDataAdaptor.DataProvider.FieldByName(STR_IMPDATATYPES_FLD_TYPE).AsString :=
                aDataTypesRecArray[i].DataType;
            xDataAdaptor.DataProvider.Post;
            xDataAdaptor.DataProvider.Next;
        end;
        xDataAdaptor.Close;
    finally
        xDataAdaptor.Free;
    end;
end;

class function TImportDataTypesDataAdaptor.ImpTableToDataTypeRec(aImportDefName: string): TDataTypesRecArray;
var
    xDataAdaptor: TImportDataTypesDataAdaptor;
    i: integer;
begin
    xDataAdaptor := TImportDataTypesDataAdaptor.Create(STR_IMPDATATYPES_TBL);
    try
        xDataAdaptor.SelectAndOpen('Select * from ' + STR_IMPDATATYPES_TBL + ' where ' +
            STR_IMPDATATYPES_FLD_DEFNAME + ' = ''' + aImportDefName + '''', true);
        try
            setlength(result, xDataAdaptor.DataProvider.RecordCount);
            for i := 0 to xDataAdaptor.DataProvider.RecordCount - 1 do
            begin
                result[i].Column := xDataAdaptor.DataProvider.FieldByName
                    (STR_IMPDATATYPES_FLD_COLUMN).AsString;
                result[i].DataType := xDataAdaptor.DataProvider.FieldByName
                    (STR_IMPDATATYPES_FLD_TYPE).AsString;
                xDataAdaptor.DataProvider.Next;
            end;
        finally
            xDataAdaptor.Close;
        end;
    finally
        xDataAdaptor.Free;
    end;
end;

class function TImportDataTypesDataAdaptor.ImportDefExists(aImportDefName: string): boolean;
var
    xDataAdaptor: TImportColDefsDataAdaptor;
begin
    result := false;
    xDataAdaptor := TImportColDefsDataAdaptor.Create;
    try
        xDataAdaptor.SelectAndOpen('Select * from ' + STR_IMPDATATYPES_TBL + ' where ' +
            STR_IMPDATATYPES_FLD_DEFNAME + ' = ''' + aImportDefName + '''', false);
        if xDataAdaptor.DataProvider.RecordCount > 0 then
            result := true;
        xDataAdaptor.Close;
    finally
        xDataAdaptor.Free;
    end;
end;

class procedure TImportDataTypesDataAdaptor.DeleteImportDef(aImportDefName: string);
var
    xDataAdaptor: TImportColDefsDataAdaptor;
begin
    xDataAdaptor := TImportColDefsDataAdaptor.Create;
    try
        xDataAdaptor.ExecSQL('Delete from ' + STR_IMPDATATYPES_TBL + ' where ' + STR_IMPDATATYPES_FLD_DEFNAME
            + ' = ''' + aImportDefName + '''');
        xDataAdaptor.Close;
    finally
        xDataAdaptor.Free;
    end;
end;

class procedure TImportDataTypesDataAdaptor.CopyImportDef(aImportDefToCopy, aNewImportDef: string);
var
    xRecArray: TDataTypesRecArray;
begin
    xRecArray := TImportDataTypesDataAdaptor.ImpTableToDataTypeRec(aImportDefToCopy);
    TImportDataTypesDataAdaptor.DatasetToTable(aNewImportDef, xRecArray);
end;


end.
