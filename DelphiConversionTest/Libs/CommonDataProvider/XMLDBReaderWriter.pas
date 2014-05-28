{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.08.10 pk                                        TN5218     Initial revision
  13.09.10 pk                                        TN5218   Changes for Plugin database packages
  ----------------------------------------------------------------------------------------------------------------------- }

unit XMLDBReaderWriter;


interface


uses
    Streamable,
    StreamableDatasetClasses,
    XMLReaderWriter,
    DataProvider;

type
    TXMLDatabaseCatalogueTableItem = class(TStreamable)
    private
        fTableName: string;
    public
        constructor Create(const aTableName: string); reintroduce;
    published
        property TableName: string read fTableName write fTableName;
    end;

    TXMLDatabaseCatalogueTableList = class(TStreamableObjectList)
    private
        function GetTableAt(aIndex: Integer): TXMLDatabaseCatalogueTableItem;
    public
        function FindByTableName(const aTableName: string): TXMLDatabaseCatalogueTableItem;
        property this[aIndex: integer]: TXMLDatabaseCatalogueTableItem read GetTableAt; default;
    end;

    TXMLDatabaseCatalogue = class(TStreamable)
    private
        fCatalogueTableList: TXMLDatabaseCatalogueTableList;
    public
        constructor Create(); reintroduce;
        destructor Destroy(); override;
    published
        property CatalogueTableList: TXMLDatabaseCatalogueTableList read fCatalogueTableList
            write fCatalogueTableList;

    end;

    TXMLDBReaderWriter = class
    private const
        cCatalogueFileName = 'DBCatalogue';
    strict private
        class function CreateReaderWriter(const aPath: string; const aFileName: string): TXMLReaderWriter;
        class function CreateCatalogueReaderWriter(const aPath: string): TXMLReaderWriter;
        class function CreateTableDefReaderWriter(const aPath: string; const aTableName: string)
            : TXMLReaderWriter;
        class function CreateTableDataReaderWriter(const aPath: string; const aTableName: string)
            : TXMLReaderWriter;
    public
        class function ReadTableDef(const aPath: string; const aTableName: string): TStreamableTableDef;
        class procedure WriteTableDef(const aPath: string; const aTableName: string;
            const aTableDef: TStreamableTableDef);

        class function ReadTableData(const aPath: string; const aTableName: string): TStreamableRecordList;
        class procedure WriteTableData(const aPath: string; const aTableName: string;
            const aRecords: TStreamableRecordList);

        class function ReadCatalogue(const aPath: string): TXMLDatabaseCatalogue;
        class procedure WriteCatalogue(const aPath: string; const aCatalogue: TXMLDatabaseCatalogue);
    end;

    TStreamableTableDataTransferer = class(TXMLDBReaderWriter)
    public
        class procedure WriteRecordsToDataProvider(const aTableName: string;
            const aRecords: TStreamableRecordList; const aDataProvider: TDataProvider);
        class procedure ReadRecordsFromDataProvider(const aTableName: string;
            const aDataProvider: TDataProvider; const aRecords: TStreamableRecordList);
    end;


implementation


uses
    SysUtils,
    FileUtilities;

{ TXMLTableReaderWriter }

class function TXMLDBReaderWriter.CreateReaderWriter(const aPath: string; const aFileName: string)
    : TXMLReaderWriter;
var
    xFilePathName: string;
begin
    xFilePathName := TFileUtilities.IncludeTrailingPathDelimiter(aPath) + aFileName;
    result := TXMLReaderWriter.Create(xFilePathName, 0);
end;

class function TXMLDBReaderWriter.CreateTableDefReaderWriter(const aPath: string; const aTableName: string)
    : TXMLReaderWriter;
begin
    result := CreateReaderWriter(aPath, aTableName + '.HdrX');
end;

class function TXMLDBReaderWriter.CreateTableDataReaderWriter(const aPath: string; const aTableName: string)
    : TXMLReaderWriter;
begin
    result := CreateReaderWriter(aPath, aTableName + '.DatX');
end;

class function TXMLDBReaderWriter.CreateCatalogueReaderWriter(const aPath: string): TXMLReaderWriter;
begin
    result := CreateReaderWriter(aPath, cCatalogueFileName);
end;

class function TXMLDBReaderWriter.ReadTableDef(const aPath: string; const aTableName: string)
    : TStreamableTableDef;
var
    xXMLReaderWriter: TXMLReaderWriter;
begin
    try
        xXMLReaderWriter := CreateTableDefReaderWriter(aPath, aTableName);
        try
            xXMLReaderWriter.ReadFromFile();
            result := xXMLReaderWriter.CreateObjectFromRootNode<TStreamableTableDef>();
        finally
            FreeAndNil(xXMLReaderWriter);
        end;
    except
        on e: exception do
        begin
            raise Exception.CreateFmt('Error ReadTable Definition %s - %s', [aTableName, e.Message]);
        end;
    end;
end;

class procedure TXMLDBReaderWriter.WriteTableDef(const aPath: string; const aTableName: string;
    const aTableDef: TStreamableTableDef);
var
    xXMLReaderWriter: TXMLReaderWriter;
begin
    xXMLReaderWriter := CreateTableDefReaderWriter(aPath, aTableName);
    try
        xXMLReaderWriter.DataChanged();
        xXMLReaderWriter.Activate();
        xXMLReaderWriter.AddObjectToRootNode(aTableDef);
        xXMLReaderWriter.WriteToFile();
    finally
        FreeAndNil(xXMLReaderWriter);
    end;
end;

class function TXMLDBReaderWriter.ReadTableData(const aPath: string; const aTableName: string)
    : TStreamableRecordList;
var
    xXMLReaderWriter: TXMLReaderWriter;
begin
    try
        xXMLReaderWriter := CreateTableDataReaderWriter(aPath, aTableName);
        try
            xXMLReaderWriter.ReadFromFile();
            result := xXMLReaderWriter.CreateObjectFromRootNode<TStreamableRecordList>();
            if not Assigned(result) then
                result := TStreamableRecordList.Create();
        finally
            FreeAndNil(xXMLReaderWriter);
        end;
    except
        on e: exception do
        begin
            raise Exception.CreateFmt('Error ReadTable Data %s - %s', [aTableName, e.Message]);
        end;
    end;
end;

class procedure TXMLDBReaderWriter.WriteTableData(const aPath: string; const aTableName: string;
    const aRecords: TStreamableRecordList);
var
    xXMLReaderWriter: TXMLReaderWriter;
begin
    xXMLReaderWriter := CreateTableDataReaderWriter(aPath, aTableName);
    try
        xXMLReaderWriter.DataChanged();
        xXMLReaderWriter.Activate();
        xXMLReaderWriter.AddObjectToRootNode(aRecords);
        xXMLReaderWriter.WriteToFile();
    finally
        FreeAndNil(xXMLReaderWriter);
    end;
end;

class function TXMLDBReaderWriter.ReadCatalogue(const aPath: string): TXMLDatabaseCatalogue;
var
    xXMLReaderWriter: TXMLReaderWriter;
begin
    xXMLReaderWriter := CreateCatalogueReaderWriter(aPath);
    try
        xXMLReaderWriter.ReadFromFile();
        result := xXMLReaderWriter.CreateObjectFromRootNode<TXMLDatabaseCatalogue>();
        if not Assigned(result) then
        begin
            result := TXMLDatabaseCatalogue.Create();
        end;

    finally
        FreeAndNil(xXMLReaderWriter);
    end;
end;

class procedure TXMLDBReaderWriter.WriteCatalogue(const aPath: string;
    const aCatalogue: TXMLDatabaseCatalogue);
var
    xXMLReaderWriter: TXMLReaderWriter;
begin
    xXMLReaderWriter := CreateCatalogueReaderWriter(aPath);
    try
        xXMLReaderWriter.DataChanged();
        xXMLReaderWriter.Activate();
        xXMLReaderWriter.AddObjectToRootNode(aCatalogue);
        xXMLReaderWriter.WriteToFile();
    finally
        FreeAndNil(xXMLReaderWriter);
    end;
end;

{ TXMLDatabaseCatalogueTableList }

function TXMLDatabaseCatalogueTableList.FindByTableName(const aTableName: string)
    : TXMLDatabaseCatalogueTableItem;
var
    x: integer;
begin
    result := nil;
    for x := 0 to self.Count - 1 do
    begin
        if SameText(aTableName, self[x].TableName) then
        begin
            result := self[x];
            EXIT;
        end;
    end;

end;

function TXMLDatabaseCatalogueTableList.GetTableAt(aIndex: Integer): TXMLDatabaseCatalogueTableItem;
begin
    result := ( inherited Items[aIndex] as TXMLDatabaseCatalogueTableItem);
end;

{ TXMLDatabaseCatalogue }

constructor TXMLDatabaseCatalogue.Create;
begin
    inherited Create();
    fCatalogueTableList := TXMLDatabaseCatalogueTableList.Create();
end;

destructor TXMLDatabaseCatalogue.Destroy;
begin
    FreeAndNil(fCatalogueTableList);
    inherited;
end;

class procedure TStreamableTableDataTransferer.WriteRecordsToDataProvider(const aTableName: string;
    const aRecords: TStreamableRecordList; const aDataProvider: TDataProvider);
var
    xRecord: TStreamableRecord;
    i, x: integer;
    xFieldData: TStreamableFieldData;
begin
    aDataProvider.SelectAndOpenAll(aTableName, false);
    try

        for i := 0 to aRecords.Count - 1 do
        begin
            xRecord := aRecords[i];
            aDataProvider.Append;

            for x := 0 to xRecord.Count - 1 do
            begin
                xFieldData := xRecord[x];
                try
                    aDataProvider.Fields[x].AssignData(xFieldData);
                except
                    on e: exception do
                    begin
                        raise Exception.CreateFmt('Error assigning value for Table %s, Field %s - %s',
                            [aTableName, aDataProvider.Fields[x].FieldName, e.Message]);
                    end;

                end;
            end;
            aDataProvider.Post;
        end;

    finally
        aDataProvider.Close();
    end;
end;

class procedure TStreamableTableDataTransferer.ReadRecordsFromDataProvider(const aTableName: string;
    const aDataProvider: TDataProvider; const aRecords: TStreamableRecordList);
var
    xRecord: TStreamableRecord;
    x: integer;
    xFieldData: TStreamableFieldData;
begin
    aRecords.Clear;
    aDataProvider.SelectAndOpenAll(aTableName, false);
    try

        while not aDataProvider.Eof do
        begin
            xRecord := TStreamableRecord.Create();
            aRecords.Add(xRecord);

            for x := 0 to aDataProvider.Fields.Count - 1 do
            begin
                xFieldData := aDataProvider.Fields[x].CopyData();
                xRecord.Add(xFieldData);
            end;

            aDataProvider.Next;
        end;
    finally
        aDataProvider.Close();
    end;
end;

{ TXMLDatabaseCatalogueTableItem }

constructor TXMLDatabaseCatalogueTableItem.Create(const aTableName: string);
begin
    inherited Create();
    fTableName := aTableName;
end;


end.
