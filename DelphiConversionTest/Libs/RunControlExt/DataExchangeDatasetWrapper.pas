{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk                                        TN4232      Initial Revision
  13.05.09 wl  TDataExchangeDatasetWrapper.Create    TN4564    Neuer Parameter SourceFileName überschreibt definierten Dateinamen
  20.05.09 wl  TDataExchangeDatasetWrapper.Read      TN4567    an Änderungen in DetermineSourceFieldValue angepasst
  20.03.09 wl  TDataExchangeDatasetWrapper.CreateDataset  TN4476   liest ImportDataTypes aus Tabelle (wenn vorhanden)
  25.06.10 wl  TDataExchangeDatasetWrapper.CreateDataset  TN5161   Parameter OrderBy kann Sortierung erweitern
  12.04.11 ts                                             TN5548   new: SkipLines setting to skip the first few lines of import file (if Excel-File)
  31.05.11 wl  TDataExchangeDatasetWrapper                TN5594   neu: DefRec-Property
  31.05.11 wl  CreateIdentValueFromVariant                TN5594   bei FloatToStr werden jetzt immer englische Einstellungen verwendet
  27.06.11 wl                                             TN5609   neu: TAttrValueUtils
  01.03.12 wl                                             TN5820   idtFloat statt idtNumber
  02.03.12 wl                                             TN5822   TArg statt TAttrValue
  05.07.12 wl  CreateIdentValueFromVariant                TN5917   --> TArgUtils
  24.04.13 wl                                             TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit DataExchangeDatasetWrapper;


interface


uses
    Generics.Defaults,
    Generics.Collections,
    ImportClasses,
    ImportDataAdaptor,
    ImportDataProvider,
    ParserIdentDataType;

type
    TDataExchangeIdentItem = class
    protected
        fIdentName: string;
        fIdentValue: TArg;
    public
        constructor Create(const aIdentName: string; const aIdentValue: TArg);
        property IdentName: string read fIdentName;
        property IdentValue: TArg read fIdentValue write fIdentValue;
    end;

    TDataExchangeIdentItemComparer = class(TComparer<TDataExchangeIdentItem>)
    public
        function Compare(const Left, Right: TDataExchangeIdentItem): Integer; override;
    end;

    TDataExchangeDatasetWrapper = class
    private
        fDataset: TImportQuery;
        fDefName: string;
        fFilter: string;
        fOrderBy: string;
        fDefRec: TImportDefRec;
        fCurrentRecordNo: integer;
        fIsOpen: boolean;
        fSourceFileName: string;
        procedure CreateDataset();
        function GetIsOpen: boolean;
        function GetRecordCount: integer;
    public
        constructor Create(const aDefName, aFilter, aOrderBy, aSourceFileName: string);
        destructor Destroy(); override;
        procedure Open();
        procedure CursorMove(aMoveRelative: boolean; aOffset: integer);
        function EOF: boolean;
        procedure read(const aResultList: TList<TDataExchangeIdentItem>);
        property DefName: string read fDefName;
        property Filter: string read fFilter;
        property OrderBy: string read fOrderBy;
        property SourceFileName: string read fSourceFileName;
        property CurrentRecordNo: integer read fCurrentRecordNo;
        property IsOpen: boolean read GetIsOpen;
        property RecordCount: integer read GetRecordCount;

        property DefRec: TImportDefRec read fDefRec;
    end;


implementation


uses
    SysUtils,
    SQLParser,
    ImportColDefDataAdaptor,
    ImportDataTypesDataAdaptor,
    Variants,
    GeneralTypes,
    ParserEvalNode;

{ TDataExchangeIdentItem }

constructor TDataExchangeIdentItem.Create(const aIdentName: string; const aIdentValue: TArg);
begin
    inherited Create();
    fIdentName := aIdentName;
    fIdentValue := aIdentValue;
end;

{ TDataExchangeIdentItemComparer }

function TDataExchangeIdentItemComparer.Compare(const Left, Right: TDataExchangeIdentItem): Integer;
begin
    Result := CompareStr(Left.IdentName, Right.IdentName)
end;

{ TDataExchangeDatasetWrapper }

constructor TDataExchangeDatasetWrapper.Create(const aDefName, aFilter, aOrderBy, aSourceFileName: string);
begin
    inherited Create();
    fDefName := aDefName;
    fFilter := aFilter;
    fOrderBy := aOrderBy;
    fSourceFileName := aSourceFileName;
    fDataset := nil;
    fIsOpen := false;
end;

destructor TDataExchangeDatasetWrapper.Destroy;
begin
    fDataset.Free;
    inherited;
end;

procedure TDataExchangeDatasetWrapper.CreateDataset();
var
    xFilter, xOrderBy: string;
    xDataTypesRecArray: TDataTypesRecArray;
begin
    // Source file name can be overwritten
    if (fSourceFileName <> '') then
    begin
        fDefRec.SourceFileDef.PathName := fSourceFileName;
    end;

    xFilter := TSQLParser.ConcatFilters(fFilter, fDefRec.SourceFileDef.Filter);
    xOrderBy := TSQLParser.ConcatOrders(fDefRec.SourceFileDef.OrderBy, fOrderBy);
    xDataTypesRecArray := TImportDataTypesDataAdaptor.ImpTableToDataTypeRec(fDefRec.SourceFileDef.Name);

    fDataset := TImports.CreateAnyDataSet(TEnDBType(fDefRec.SourceFileDef.DBType), '',
        fDefRec.SourceFileDef.PathName, fDefRec.SourceFileDef.TableName, fDefRec.SourceFileDef.Delimiter,
        fDefRec.SourceFileDef.HasHeader, fDefRec.SourceFileDef.RowOffset, xFilter, xOrderBy,
        fDefRec.SourceFileDef.UserName, fDefRec.SourceFileDef.Password, fDefRec.SourceFileDef.SQL, true,
        false, xDataTypesRecArray, fDefRec.SourceFileDef.SkipLines);
end;

procedure TDataExchangeDatasetWrapper.Open();
begin
    fDefRec := TImportDefDataAdaptor.QuickReadDef(fDefName, true, true);
    if fDefRec.Name = '' then
        raise Exception.CreateFmt('Definition "%s" does not exist', [fDefName]);
    CreateDataset();
    fCurrentRecordNo := 1;
    fIsOpen := true;
end;

procedure TDataExchangeDatasetWrapper.CursorMove(aMoveRelative: boolean; aOffset: integer);
begin
    if aMoveRelative then
    begin
        fDataset.MoveBy(aOffset);
        fCurrentRecordNo := fCurrentRecordNo + aOffset;
    end
    else
    begin
        ASSERT(aOffset > 0, 'Cursor move absolute offset must be >= 1');
        fDataset.First();
        fDataset.MoveBy(aOffset - 1);
        fCurrentRecordNo := aOffset;
    end;
end;

function TDataExchangeDatasetWrapper.EOF(): boolean;
begin
    result := fDataset.EOF;
end;

procedure TDataExchangeDatasetWrapper.Read(const aResultList: TList<TDataExchangeIdentItem>);
var
    x: integer;
    xColDef: TImportColDefRec;
    xValue: variant;
    xIdent: TDataExchangeIdentItem;
    xIdentValue: TArg;
begin

    if fCurrentRecordNo > fDataset.RecordCount then
        raise Exception.Create('Dataset cursor is beyond the end of the dataset');

    if fCurrentRecordNo < 1 then
        raise Exception.Create('Dataset cursor is beyond the beginning of the dataset');

    for x := 0 to high(fDefRec.ImportColDefs) do
    begin
        xColDef := fDefRec.ImportColDefs[x];
        xValue := TImports.DetermineSourceFieldValue(fDataset, xColDef.SourceCol,
            not xColDef.SourceNotRequired, xColDef.SourceDefault, fCurrentRecordNo);
        if VarIsNull(xValue) then
            raise Exception.CreateFmt('Value for column %s could not be read', [xColDef.SourceCol]);
        xIdentValue := TArgUtils.CreateArgByVariantValue(xValue, false);
        xIdent := TDataExchangeIdentItem.Create(xColDef.TargetCol, xIdentValue);
        aResultList.Add(xIdent);
    end;

    aResultList.Sort(TDataExchangeIdentItemComparer.Create);
end;

function TDataExchangeDatasetWrapper.GetIsOpen: boolean;
begin
    result := fIsOpen;
end;

function TDataExchangeDatasetWrapper.GetRecordCount: integer;
begin
    result := fDataset.RecordCount;
end;


end.
