{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  17.04.13 wl                                      TN6106   Initial Revision
  ----------------------------------------------------------------------------------------------------------- }

unit FileExport;


interface


uses
    Generics.Collections,
    Streamable,
    DataProvider,
    QueryDataAdaptor,
    StreamableDatasetClasses,
    XMLReaderWriter;

type
    TTableInfoContainer = class(TStreamable)
    strict private
        fMajorVersion: integer;
        fMinorVersion: integer;
        fTableName: string;
        // fFieldDefs: TStreamableFieldDefList;
        fRecords: TStreamableRecordList;
    public
        constructor Create(); reintroduce;
        destructor Destroy(); override;
    published
        property MajorVersion: integer read fMajorVersion write fMajorVersion;
        property MinorVersion: integer read fMinorVersion write fMinorVersion;
        property TableName: string read fTableName write fTableName;
        // property FieldDefs: TStreamableFieldDefList read fFieldDefs write fFieldDefs;
        property Records: TStreamableRecordList read fRecords write fRecords;
    end;

    TTableInfoContainerList = class(TStreamableObjectList)
    private
        function GetDataAt(aIndex: integer): TTableInfoContainer;
    public
        property this[aIndex: integer]: TTableInfoContainer read GetDataAt; default;
    end;

    TStreamableTableDataTransferer = class
    public
        class procedure WriteRecordsToDataProvider(const aDataName: string;
            const aTableData: TTableInfoContainer; const aDataAdaptor: TQueryDataAdaptor);
        class function CreateRecordsFromDataProvider(const aDataName: string;
            const aDataAdaptor: TQueryDataAdaptor): TTableInfoContainer;
    end;

    TXMLDBReaderWriter = class
    public
        class function CreateTableData(const aFileName: string): TTableInfoContainerList;
        class procedure WriteTableData(const aPath, aExt: string; const aDataName: string;
            const aTables: TTableInfoContainerList);
    end;


implementation


uses
    SysUtils,
    TypeMapTranslator,
    FileUtilities;

{ TTableInfoContainer }

constructor TTableInfoContainer.Create;
begin
    inherited Create();
    fTableName := 'Unknown';
    // fFieldDefs := TStreamableFieldDefList.Create();
    fRecords := TStreamableRecordList.Create();

end;

destructor TTableInfoContainer.Destroy;
begin
    FreeAndNil(fRecords);
    // FreeAndNil(fFieldDefs);
    inherited;
end;

{ TTableInfoContainerList }

function TTableInfoContainerList.GetDataAt(aIndex: integer): TTableInfoContainer;
begin
    result := ( inherited Items[aIndex]) as TTableInfoContainer;
end;

{ TStreamableTableDataTransferer }

class procedure TStreamableTableDataTransferer.WriteRecordsToDataProvider(const aDataName: string;
    const aTableData: TTableInfoContainer; const aDataAdaptor: TQueryDataAdaptor);
var
    xRecord: TStreamableRecord;
    xRec, x: integer;
begin
    aDataAdaptor.SelectAndOpenAll(false);
    try
        // TabellenName
        if (aTableData.TableName <> aDataAdaptor.TableName) then
            raise Exception.Create('Wrong table name');

        { TODO : Version vergleichen }

        { TODO : Struktur vergleichen }

        // Daten in die Tabelle eintragen
        for xRec := 0 to aTableData.Records.Count - 1 do
        begin
            xRecord := aTableData.Records[xRec];
            aDataAdaptor.DataProvider.Append;

            // Field 0: Item Name
            aDataAdaptor.DataProvider.Fields[0].AssignData(TStrStreamableItem.Create(aDataName));

            for x := 0 to xRecord.Count - 1 do
            begin
                try
                    aDataAdaptor.DataProvider.Fields[x + 1].AssignData(xRecord[x]);
                except
                    on e: exception do
                    begin
                        raise Exception.CreateFmt('Error assigning value for Table %s, Field %s - %s',
                            [aDataAdaptor.TableName, aDataAdaptor.DataProvider.Fields[x].FieldName,
                            e.Message]);
                    end;

                end;
            end;
            aDataAdaptor.DataProvider.Post;
        end;

    finally
        aDataAdaptor.Close();
    end;
end;

class function TStreamableTableDataTransferer.CreateRecordsFromDataProvider(const aDataName: string;
    const aDataAdaptor: TQueryDataAdaptor): TTableInfoContainer;
var
    xRecord: TStreamableRecord;
    x: integer;
    xFieldData: TStreamableFieldData;
begin
    result := TTableInfoContainer.Create();

    // TabellenName
    result.TableName := aDataAdaptor.TableName;

    { TODO : Version lesen }

    // Daten lesen
    aDataAdaptor.SelectAndOpenByName(aDataName, true);
    try
        while not aDataAdaptor.DataProvider.Eof do
        begin
            xRecord := TStreamableRecord.Create();
            result.Records.Add(xRecord);

            // Field 0: Item Name wird nicht mitkopiert

            for x := 1 to aDataAdaptor.DataProvider.Fields.Count - 1 do
            begin
                xFieldData := aDataAdaptor.DataProvider.Fields[x].CopyData();
                xRecord.Add(xFieldData);
            end;

            aDataAdaptor.DataProvider.Next;
        end;
    finally
        aDataAdaptor.Close();
    end;
end;

{ TXMLDBReaderWriter }

class function TXMLDBReaderWriter.CreateTableData(const aFileName: string): TTableInfoContainerList;
var
    xXMLReaderWriter: TXMLReaderWriter;
begin
    try
        xXMLReaderWriter := TXMLReaderWriter.Create(aFileName, 0);
        try
            xXMLReaderWriter.ReadFromFile();
            result := xXMLReaderWriter.CreateObjectFromRootNode<TTableInfoContainerList>();
            if not Assigned(result) then
                result := TTableInfoContainerList.Create();
        finally
            FreeAndNil(xXMLReaderWriter);
        end;
    except
        on e: exception do
        begin
            raise Exception.CreateFmt('Error Read File %s - %s', [aFileName, e.Message]);
        end;
    end;
end;

class procedure TXMLDBReaderWriter.WriteTableData(const aPath, aExt: string; const aDataName: string;
    const aTables: TTableInfoContainerList);
var
    xXMLReaderWriter: TXMLReaderWriter;
    xFilePathName: string;
begin
    xFilePathName := TFileUtilities.IncludeTrailingPathDelimiter(aPath) + aDataName + '.' + aExt;
    xXMLReaderWriter := TXMLReaderWriter.Create(xFilePathName, 0);
    try
        xXMLReaderWriter.DataChanged();
        xXMLReaderWriter.Activate();
        xXMLReaderWriter.AddObjectToRootNode(aTables);
        xXMLReaderWriter.WriteToFile();
    finally
        FreeAndNil(xXMLReaderWriter);
    end;
end;


end.
