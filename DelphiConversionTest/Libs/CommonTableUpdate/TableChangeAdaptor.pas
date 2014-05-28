{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Classes which are specific to updating a special kind of table such a TTable
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  -----------------------------------------------
  06.02.07 pk                                TN3544   Initial Revision
  16.02.07 pk  UpdateData                    TN3581   Should also work if no source table exists
  01.08.07 wl                                TN3811.2 benutzt TDataAdaptor statt gCommonDll
  31.08.07 wl  TDataMapCopyMatchingOp.DoPerformDataMap TN3811.4  in AutoInc-Felder darf nichts eingetragen werden
  21.09.07 wl  TTableChangeAdaptor           TN3811.4 verwendet TTableUpdateStructDef.DbPath
  02.11.07 pk  TDataMapCopyFieldWithFuncOp            xDestvalue set to null before being passed to Function
  24.06.08 pk                                TN4148   uses UpdateManagerDataProvider instead of dataprovider
  10.01.11 wl                                TN5405   Änderungen fertig: Switch, Sensor, XWayValve
  11.01.11 wl                                TN5405   verbessertes Logging
  15.02.11 pk   	                    	     TN4780   changes needed to make UpdateManager compatible with TurboDB
  06.03.11 pk   	                    	     TN4780   comments changed
  26.03.12 wl                                    TN5844   Keine Compiler-Meldungen dank Generics
  17.04.13 wl                                    TN6106   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit TableChangeAdaptor;


interface


uses
    Generics.Collections,
    TableVersionInfo,
    TableUpdateDataMap,
    TableUpdateItem,
    TableStructDef,
    DataProvider,
    UpdaterTableProvider;

type
    TFieldPair = class
    private
        fSource: TDataField;
        fDest: TDataField;
    public
        constructor Create(aSource: TDataField; aDest: TDataField);
        procedure CopyValue();
        property Source: TDataField read fSource write fSource;
        property Dest: TDataField read fDest write fDest;
    end;

    TDataMapOp = class
    protected
        fPrepared: boolean;
        fSourceDataset, fDestDataset: TDataProvider;
        function GetField(aDataset: TDataProvider; aFieldName: string; var vField: TDataField): boolean;
        procedure DoPerformDataMap(); virtual;
        procedure DoPrepareDataMap(); virtual;

    public
        constructor Create(aSourceDataset, aDestDataset: TDataProvider);
        procedure PerformaDataMap();
        procedure PrepareDataMap();
    end;

    TDataMapCopyMatchingOp = class(TDataMapOp)
    protected
        fDataMap: TTableUpdateCopyMatchingDataMap;
        fFieldPairs: TObjectList<TFieldPair>;
        fStructDef: TTableStructDef;

        procedure FindMatchingFieldPairs();
        procedure DoPrepareDataMap(); override;
        procedure DoPerformDataMap(); override;
    public
        constructor Create(aStructDef: TTableStructDef; aSourceDataset, aDestDataset: TDataProvider;
            aDataMap: TTableUpdateCopyMatchingDataMap);
    end;

    TDataMapCopyFieldOp = class(TDataMapOp)
    protected
        fDataMap: TTableUpdateCopyFieldDataMap;
        fSourceField, fDestField: TDataField;
        procedure DoPrepareDataMap(); override;
        procedure DoPerformDataMap(); override;
    public
        constructor Create(aSourceDataset, aDestDataset: TDataProvider;
            aDataMap: TTableUpdateCopyFieldDataMap);
    end;

    TFieldArray = array of TDataField;

    TDataMapCopyFieldWithFuncOp = class(TDataMapOp)
    protected
        fSourceFields: TFieldArray;
        fDestField: TDataField;
        fDataMap: TTableUpdateCopyFieldWithFuncDataMap;
        fSourceValue: variant;
        procedure DoPrepareDataMap(); override;
        procedure DoPerformDataMap(); override;
    public
        constructor Create(aSourceDataset, aDestDataset: TDataProvider;
            aDataMap: TTableUpdateCopyFieldWithFuncDataMap);
    end;

    TDataMapConstFieldOp = class(TDataMapOp)
    protected
        fDestField: TDataField;
        fDataMap: TTableUpdateConstFieldDataMap;
        procedure DoPrepareDataMap(); override;
        procedure DoPerformDataMap(); override;
    public
        constructor Create(aDestDataset: TDataProvider; aDataMap: TTableUpdateConstFieldDataMap);
    end;

    TTableChangeAdaptor = class
    private
        fArchivePath: string;
        fStructDef: TTableStructDef;
        fVersionInfoAdaptor: TTableVersionInfoAdaptor;
        fTableProvider: TUpdaterTableProvider;
        fRevisionNumber: TTableRevisionNumber;
        function CreateDataMapOp(aSourceDataset, aDestDataset: TDataProvider; aDataMap: TTableUpdateDataMap)
            : TDataMapOp;

    public
        constructor Create(const aDBPath, aTempDBPath, aArchivePath: string; aStructDef: TTableStructDef;
            aRevisionNumber: TTableRevisionNumber);
        destructor Destroy(); override;
        procedure CommitChanges();
        procedure RollbackChanges();
        procedure UpdateStructure();
        procedure UpdateData(aDataDef: TTableUpdateDataDef);
        function ReadTableVersion(): TTableRevisionNumber;
        function CompareVersions(aFirstV, aSecondV: TTableRevisionNumber): TTableRevisionNumberCompareResult;
        function IsTableVersionMissing(aRevisonNumber: TTableRevisionNumber): boolean;
        function CreateDestDataProvider(): TDataProvider;
        function CreateSourceDataProvider(): TDataProvider;
        function DBPathTableExists(const aTableName: string): boolean;
        class function TableRevisionNumberFromInt(aMajorRevision, aMinorRevision: integer)
            : TTableRevisionNumber;
    end;

    TSQL = class
    public
        class function FieldIsNull(const aTableAlias: string; const aFieldName: string): string;
        class function FieldNotNull(const aTableAlias: string; const aFieldName: string): string;
        class function FieldEq(const aTableAlias: string; const aFieldName: string; const aValue: string)
            : string; overload;
        class function FieldEq(const aTableAlias: string; const aFieldName: string; const aValue: integer)
            : string; overload;
        class function FieldSameText(const aTableAlias: string; const aFieldName: string;
            const aValue: string): string;
    end;


implementation


uses
    Windows,
    SysUtils,
    Variants,
    UpdaterDataProviderFactory,
    StreamableDatasetClasses;

procedure TFieldPair.CopyValue;
begin
    fDest.Value := fSource.Value;
end;

constructor TFieldPair.Create(aSource: TDataField; aDest: TDataField);
begin
    inherited Create();
    fSource := aSource;
    fDest := aDest;
end;

{ TDataMapOp }

constructor TDataMapOp.Create(aSourceDataset, aDestDataset: TDataProvider);
begin
    inherited Create();
    fSourceDataset := aSourceDataset;
    fDestDataset := aDestDataset;
end;

procedure TDataMapOp.DoPerformDataMap;
begin

end;

procedure TDataMapOp.PerformaDataMap;
begin
    if fPrepared then
        DoPerformDataMap();
end;

procedure TDataMapOp.DoPrepareDataMap;
begin

end;

procedure TDataMapOp.PrepareDataMap;
begin
    fPrepared := false;
    DoPrepareDataMap();
end;

function TDataMapOp.GetField(aDataset: TDataProvider; aFieldName: string; var vField: TDataField): boolean;
begin
    result := aDataset.Fields.IndexOf(aFieldName) >= 0;
    if not result then
        EXIT;
    vField := aDataset.FieldByName(aFieldName);
end;

{ TDataMapCopyMatchingOp }
constructor TDataMapCopyMatchingOp.Create(aStructDef: TTableStructDef;
    aSourceDataset, aDestDataset: TDataProvider; aDataMap: TTableUpdateCopyMatchingDataMap);
begin
    inherited Create(aSourceDataset, aDestDataset);
    fDataMap := aDataMap;
    fStructDef := aStructDef;
    fFieldPairs := TObjectList<TFieldPair>.Create;
end;

procedure TDataMapCopyMatchingOp.FindMatchingFieldPairs();
var
    x: integer;
    xFieldName: string;
    xSourceField, xDestField: TDataField;
begin
    for x := 0 to fStructDef.FieldDefs.Count - 1 do
    begin
        xFieldName := fStructDef.FieldDefs[x].FieldName;
        if fDataMap.ExceptionFields.IndexOf(xFieldName) >= 0 then
            CONTINUE;
        if not GetField(fSourceDataset, xFieldName, xSourceField) then
            CONTINUE;
        if not GetField(fDestDataset, xFieldName, xDestField) then
            CONTINUE;
        fFieldPairs.Add(TFieldPair.Create(xSourceField, xDestField));
    end;
end;

procedure TDataMapCopyMatchingOp.DoPrepareDataMap();
begin
    FindMatchingFieldPairs();
    fPrepared := true;
end;

procedure TDataMapCopyMatchingOp.DoPerformDataMap();
var
    x: integer;
    xPair: TFieldPair;
begin
    for x := 0 to fFieldPairs.Count - 1 do
    begin
        xPair := fFieldPairs[x];
        if (xPair.Dest.DataType = dftAutoInc) then
            CONTINUE;
        xPair.Dest.Value := xPair.Source.Value;
    end;
end;

constructor TDataMapCopyFieldOp.Create(aSourceDataset, aDestDataset: TDataProvider;
    aDataMap: TTableUpdateCopyFieldDataMap);
begin
    inherited Create(aSourceDataset, aDestDataset);
    fDataMap := aDataMap;
end;

procedure TDataMapCopyFieldOp.DoPrepareDataMap();
begin
    if not GetField(fSourceDataset, fDataMap.SourceField, fSourceField) then
        EXIT;
    if not GetField(fDestDataset, fDataMap.DestField, fDestField) then
        EXIT;
    fPrepared := true;
end;

procedure TDataMapCopyFieldOp.DoPerformDataMap;
begin
    fDestField.Value := fSourceField.Value;
end;

{ TDataMapCopyFieldWithFuncOp }

constructor TDataMapCopyFieldWithFuncOp.Create(aSourceDataset, aDestDataset: TDataProvider;
    aDataMap: TTableUpdateCopyFieldWithFuncDataMap);
begin
    inherited Create(aSourceDataset, aDestDataset);
    fDataMap := aDataMap;
end;

procedure TDataMapCopyFieldWithFuncOp.DoPrepareDataMap();
var
    xField: TDataField;
    xSize: integer;
    x: integer;
begin
    SetLength(fSourceFields, Length(fDataMap.SourceFields));
    for x := 0 to high(fDataMap.SourceFields) do
    begin
        if not GetField(fSourceDataset, fDataMap.SourceFields[x], xField) then
            EXIT;
        fSourceFields[x] := xField;
    end;
    xSize := Length(fSourceFields);

    if not GetField(fDestDataset, fDataMap.DestField, fDestField) then
        EXIT;

    if xSize = 0 then
        EXIT;

    // prepare array
    if xSize > 1 then
        fSourceValue := VarArrayCreate([0, xSize - 1], varVariant);

    fPrepared := true;
end;

procedure TDataMapCopyFieldWithFuncOp.DoPerformDataMap;
var
    xDestValue: variant;
    xSize: integer;
    x: integer;
begin
    xSize := Length(fSourceFields);
    if xSize = 1 then
        fSourceValue := fSourceFields[0].Value
    else
    begin
        for x := 0 to xSize - 1 do
            fSourceValue[x] := fSourceFields[x].Value;
    end;
    xDestValue := null;
    fDataMap.DataMapFunc(fSourceValue, xDestValue);
    fDestField.Value := xDestValue;
end;

{ TDataMapConstFieldOp }

constructor TDataMapConstFieldOp.Create(aDestDataset: TDataProvider; aDataMap: TTableUpdateConstFieldDataMap);
begin
    inherited Create(nil, aDestDataset);
    fDataMap := aDataMap;
end;

procedure TDataMapConstFieldOp.DoPrepareDataMap;
begin
    if not GetField(fDestDataset, fDataMap.DestField, fDestField) then
        EXIT;
end;

procedure TDataMapConstFieldOp.DoPerformDataMap;
begin
    if (fDataMap.UpdateOnlyIfEmpty) and (not fDestField.IsNull) then
        EXIT;
    fDestField.Value := fDataMap.Value;
end;

{ TTableChangeAdaptor }

constructor TTableChangeAdaptor.Create(const aDBPath, aTempDBPath, aArchivePath: string;
    aStructDef: TTableStructDef; aRevisionNumber: TTableRevisionNumber);
begin
    inherited Create;
    fArchivePath := aArchivePath;
    fStructDef := aStructDef;
    fVersionInfoAdaptor := TTableVersionInfoAdaptor.Create(fStructDef.Name);
    fTableProvider := TUpdaterTableProvider.Create(aDBPath, aTempDBPath);
    fRevisionNumber := aRevisionNumber;
end;

destructor TTableChangeAdaptor.Destroy();
begin
    fVersionInfoAdaptor.Free;
    inherited Destroy();
end;

procedure TTableChangeAdaptor.CommitChanges();
begin
    fTableProvider.ArchiveTable(fStructDef.Name, fArchivePath);
    fTableProvider.CopyTableToDBPath(fStructDef.Name);
    fTableProvider.RemoveTempTable(fStructDef.Name);
end;

procedure TTableChangeAdaptor.RollbackChanges();
begin
    fTableProvider.RemoveTempTable(fStructDef.Name);
end;

procedure TTableChangeAdaptor.UpdateStructure();
begin
    fTableProvider.InstallNewTableAsTemp(fStructDef, fRevisionNumber);
end;

function TTableChangeAdaptor.CreateDataMapOp(aSourceDataset, aDestDataset: TDataProvider;
    aDataMap: TTableUpdateDataMap): TDataMapOp;
begin
    result := nil;
    if aDataMap is TTableUpdateCopyMatchingDataMap then
    begin
        if (not aSourceDataset.Active) or (aSourceDataset.RecordCount = 0) then
            EXIT;
        result := TDataMapCopyMatchingOp.Create(fStructDef, aSourceDataset, aDestDataset,
            aDataMap as TTableUpdateCopyMatchingDataMap);
    end
    else if aDataMap is TTableUpdateCopyFieldWithFuncDataMap then
    begin
        if (not aSourceDataset.Active) or (aSourceDataset.RecordCount = 0) then
            EXIT;
        result := TDataMapCopyFieldWithFuncOp.Create(aSourceDataset, aDestDataset,
            aDataMap as TTableUpdateCopyFieldWithFuncDataMap);
    end
    else if aDataMap is TTableUpdateCopyFieldDataMap then
    begin
        if (not aSourceDataset.Active) or (aSourceDataset.RecordCount = 0) then
            EXIT;
        result := TDataMapCopyFieldOp.Create(aSourceDataset, aDestDataset,
            aDataMap as TTableUpdateCopyFieldDataMap);
    end
    else if aDataMap is TTableUpdateConstFieldDataMap then
    begin
        result := TDataMapConstFieldOp.Create(aDestDataset, aDataMap as TTableUpdateConstFieldDataMap);
    end;
end;

function TTableChangeAdaptor.CreateDestDataProvider(): TDataProvider;
begin
    result := fTableProvider.CreateDataProviderForTempTable();
end;

function TTableChangeAdaptor.CreateSourceDataProvider(): TDataProvider;
begin
    result := fTableProvider.CreateDataProviderForDBPathTable();
end;

function TTableChangeAdaptor.DBPathTableExists(const aTableName: string): boolean;
begin
    result := fTableProvider.DBPathTableExists(aTableName)
end;

procedure TTableChangeAdaptor.UpdateData(aDataDef: TTableUpdateDataDef);
var
    x: integer;
    xSourceTable, xDestTable: TDataProvider;
    xDataMapOp: TDataMapOp;
    xDataMapOps: TObjectList<TDataMapOp>;
begin
    xSourceTable := CreateSourceDataProvider();
    xDestTable := CreateDestDataProvider();
    try
        if DBPathTableExists(fStructDef.Name) then
            xSourceTable.SelectAndOpen(Format('SELECT * FROM "%s"', [fStructDef.Name]), true);

        xDestTable.SelectAndOpen(Format('SELECT * FROM "%s"', [fStructDef.Name]), false);

        xDataMapOps := TObjectList<TDataMapOp>.Create(true);
        try
            for x := 0 to aDataDef.DataMaps.Count - 1 do
            begin
                xDataMapOp := CreateDataMapOp(xSourceTable, xDestTable, aDataDef.DataMaps[x]);
                if xDataMapOp = nil then
                    CONTINUE;
                xDataMapOp.PrepareDataMap();
                xDataMapOps.Add(xDataMapOp);
            end;

            if xDataMapOps.Count > 0 then
            begin
                while not xSourceTable.Eof do
                begin
                    xDestTable.Append;
                    for x := 0 to xDataMapOps.Count - 1 do
                    begin
                        xDataMapOps[x].PerformaDataMap;
                    end;
                    xDestTable.Post;
                    if xSourceTable.Active then
                        xSourceTable.Next;
                end;
            end;

        finally
            xDataMapOps.Free;
        end;

    finally
        xDestTable.Free;
        xSourceTable.Free;
    end;

    // execute a special function
    if Assigned(aDataDef.DataFunc) then
        aDataDef.DataFunc(self);
end;

function TTableChangeAdaptor.ReadTableVersion(): TTableRevisionNumber;
begin
    result := fVersionInfoAdaptor.ReadVersion(fTableProvider.GetDBTablePath(fStructDef.Name));
end;

function TTableChangeAdaptor.CompareVersions(aFirstV, aSecondV: TTableRevisionNumber)
    : TTableRevisionNumberCompareResult;
begin
    result := fVersionInfoAdaptor.CompareVersions(aFirstV, aSecondV);
end;

function TTableChangeAdaptor.IsTableVersionMissing(aRevisonNumber: TTableRevisionNumber): boolean;
begin
    result := fVersionInfoAdaptor.IsVersionMissing(aRevisonNumber);
end;

class function TTableChangeAdaptor.TableRevisionNumberFromInt(aMajorRevision, aMinorRevision: integer)
    : TTableRevisionNumber;
begin
    result := TTableVersionInfoAdaptor.TableRevisionNumberFromInt(aMajorRevision, aMinorRevision);
end;

class function TSQL.FieldEq(const aTableAlias: string; const aFieldName: string;
    const aValue: integer): string;
// example: a."Value" = SomeIntegerValue
begin
    result := Format(' %s."%s" = %d ', [aTableAlias, aFieldName, aValue]);
end;

class function TSQL.FieldEq(const aTableAlias: string; const aFieldName: string;
    const aValue: string): string;
// example: a."Value" = ''SomeTextValue''
begin
    result := Format(' %s."%s" = ''%s'' ', [aTableAlias, aFieldName, aValue]);
end;

class function TSQL.FieldNotNull(const aTableAlias: string; const aFieldName: string): string;
// example: a."Value" IS NOT NULL
begin
    result := Format(' %s."%s" IS NOT NULL ', [aTableAlias, aFieldName]);
end;

class function TSQL.FieldIsNull(const aTableAlias: string; const aFieldName: string): string;
// example: a."Value" = IS NULL
begin
    result := Format(' %s."%s" IS NULL ', [aTableAlias, aFieldName]);
end;

class function TSQL.FieldSameText(const aTableAlias: string; const aFieldName: string;
    const aValue: string): string;
// example: Upper( a."Value" ) = Upper(''SomeTextValue'')
begin
    result := Format(' Upper( %s."%s" ) = Upper( ''%s'' ) ', [aTableAlias, aFieldName, aValue]);
end;


end.
