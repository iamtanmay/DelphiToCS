{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Thomas Schubert (ts)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  25.05.11 ts                                        TN5590    initial revision
  01.08.11 wl  DoLoadSettings                        TN5642   fSQLStatement ist nur der Name des SQL-Statements, das SQL wird aus SQLTERMS.DB geladen
  22.08.11 wl  TSQLTableDisplayComponent.DoRefresh   TN5666   FreeAndNil eingebaut
  29.08.11 wl  TSQLTableDisplayComponent.DoRefresh   TN5675   try..except-Blöcke sollen "List index out of bounds"-Fehler verhindern
  22.09.11 wl  TSQLTableDisplayComponent.DoRefresh   TN5675.1 noch ein try..except-Block um die ganze Funktion
  17.11.11 wl  TSQLTableDisplayComponent.DoRefresh   TN5737   DefaultRowHeight := 18
  23.11.12 wl  TSQLTableDisplayComponent.DoLoadSettings  TN5918  Neu: Alias-Namen werden durch Pfade ersetzt
  29.11.12 wl                                        TN6015.2 DesignDisplayComponents abgeschafft (Es gibt nur noch RunDisplayComponents)
  29.11.12 wl  DoRefresh                             TN6015.2 in der Design-Ansicht werden Exceptions nicht unterdrückt
  01.03.13 ts  DoRefreshTable                        TN6102   Inhalt der Tabelle wird gelöscht, wenn Query kein Ergebnis hat
  ----------------------------------------------------------------------------------------------------------------------- }

unit SQLTableDisplayComponent;


interface


uses
    Grids,
    DisplayComponentIntf,
    DisplayComponentSettings,
    DisplayComponentTypeInfo,
    BasicDisplayComponent;

type
    TSQLTableDisplayComponentSettingList = class(TBasicDisplayComponentSettingList)
    private
        fSQLStatement: string;
    protected
        procedure AddCustomSettings(); override;
        procedure LoadSettings(); override;
        function GetShowDisplayIDSetting(): boolean; override;
    public
        property SQLStatement: string read fSQLStatement;
    end;

    TSQLTableDisplayComponent = class(TBasicDisplayComponent, ISQLTableDisplayComponent)
    private
        function GetSettings(): TSQLTableDisplayComponentSettingList;
        procedure DoRefreshTable();
    protected
        fCaption: string;
        fSQLStatement: string;
        function DoCreateDisplayControl: TDisplayControl; override;
        procedure DoInitControlProperties(); override;
        function GetControl: TStringGrid;
        property Control: TStringGrid read GetControl;
        procedure DoLoadSettings(); override;
        procedure DoRefresh();
    public
        constructor Create(const aName: string); override;
        destructor Destroy(); override;
        property Settings: TSQLTableDisplayComponentSettingList read GetSettings;
    end;

    TSQLTableDisplayComponentTypeInfoConst = record
    public const
        cTypeVersionSQLTable = '1.0.0';
        cTypeNameSQLTable = 'SQLTable';
    end;

    TRunSQLTableDisplayComponentTypeInfo = class(TRunDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TSQLTableDisplayComponentSettingsTypeInfo = class(TDisplayComponentSettingsTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    SysUtils,
    LogManager,
    GeneralTypes,
    DataProvider,
    DataProviderFactory,
    ControlUtils,
    SQLParser,
    SQLTermsDataAdaptor;

{ TSQLTableDisplayComponent }

constructor TSQLTableDisplayComponent.Create(const aName: string);
begin
    inherited;
    fCanHaveChildren := false;
end;

destructor TSQLTableDisplayComponent.Destroy;
begin
    inherited;
end;

function TSQLTableDisplayComponent.GetSettings: TSQLTableDisplayComponentSettingList;
begin
    result := fSettings as TSQLTableDisplayComponentSettingList;
end;

procedure TSQLTableDisplayComponent.DoLoadSettings;
var
    xRec: TSQLTermRec;
    xDA: TSQLTermsDataAdaptor;
begin
    inherited;

    xDA := TSQLTermsDataAdaptor.Create;
    try
        if (xDA.ReadSQLTermData(self.Settings.SQLStatement, xRec)) then
            fSQLStatement := TSQLParser.ResolveAliasesInSQL(xRec.Term);
    finally
        FreeAndNil(xDA);
    end;
end;

procedure TSQLTableDisplayComponent.DoRefreshTable();
var
    xQuery: TDataProvider;
    xCol, xRow: integer;
begin
    xQuery := TDataProviderFactory.Instance.CreateDataProvider();
    try
        xQuery.SelectAndOpen(fSQLStatement, true);

        try
            if xQuery.RecordCount = 0 then
            begin
                if self.GetControl.RowCount > 0 then
                begin
                    for xRow := 0 to self.GetControl.RowCount - 1 do
                        self.Control.Rows[xRow].Clear();
                end;
                EXIT;
            end;

            self.GetControl.RowCount := 1 + xQuery.RecordCount;
            self.Control.ColCount := xQuery.FieldCount;
            self.Control.FixedRows := 1;
            self.Control.FixedCols := 0;
            self.Control.DefaultRowHeight := 18;
            self.Control.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing];

            for xCol := 0 to xQuery.FieldCount - 1 do
            begin
                try
                    self.Control.Cells[xCol, 0] := xQuery.Fields[xCol].FieldName;
                except
                    TLogManager.Instance.Log('RESFRESHTABLE: Error at col ' + IntToStr(xCol) +
                        ', row 0', true);
                    // so was mache ich sonst nie, aber diese Funktion steigt manchmal mit "List Index out of bounds" aus
                end;
            end;

            xRow := 1;
            while not xQuery.Eof do
            begin
                for xCol := 0 to xQuery.FieldCount - 1 do
                begin
                    try
                        self.Control.Cells[xCol, xRow] := xQuery.Fields[xCol].AsString;
                    except
                        TLogManager.Instance.Log('RESFRESHTABLE: Error at col ' + IntToStr(xCol) + ', row ' +
                            IntToStr(xRow), true);
                        // so was mache ich sonst nie, aber diese Funktion steigt manchmal mit "List Index out of bounds" aus
                    end;
                end;
                xQuery.Next;
                Inc(xRow);
            end;
        finally
            xQuery.Close;
        end;
    finally
        FreeAndNil(xQuery);
    end;
end;

procedure TSQLTableDisplayComponent.DoRefresh;
begin
    if self.IsDesignMode then
    begin
        // Exceptions dürfen angezeigt werden!
        DoRefreshTable();
    end
    else
    begin
        try
            DoRefreshTable();
        except
            // ein Fehler hier darf nicht zum Absturz führen
            TLogManager.Instance.Log('Exception fetched, TSQLTableDisplayComponent.DoRefresh', true);
        end;
    end;

    TControlUtils.OptimizeGridColumnSizes(self.Control);
end;

function TSQLTableDisplayComponent.DoCreateDisplayControl: TDisplayControl;
begin
    result := TStringGrid.Create(nil);
end;

procedure TSQLTableDisplayComponent.DoInitControlProperties;
begin
    inherited;

    self.DoRefresh;
end;

function TSQLTableDisplayComponent.GetControl: TStringGrid;
begin
    result := fControl as TStringGrid;
end;

{ TSQLTableDisplayComponentSettingList }

procedure TSQLTableDisplayComponentSettingList.AddCustomSettings;
var
    xDA: TSQLTermsDataAdaptor;
    xNames: TArray<string>;
begin
    inherited;

    xDA := TSQLTermsDataAdaptor.Create;
    try
        xNames := xDA.ReadAllNames;
    finally
        FreeAndNil(xDA);
    end;

    self.AddStr('SQLStatement', 'Name of the SQL statement', '', xNames);
end;

function TSQLTableDisplayComponentSettingList.GetShowDisplayIDSetting: boolean;
begin
    result := true;
end;

procedure TSQLTableDisplayComponentSettingList.LoadSettings;
begin
    inherited;
    fSQLStatement := self.Find('SQLStatement').AsStr;
end;

{ TSQLTableDisplayComponentSettingsTypeInfo }

constructor TSQLTableDisplayComponentSettingsTypeInfo.Create(const aLibName, aLibVersion: string);
begin
    inherited Create(TSQLTableDisplayComponentTypeInfoConst.cTypeNameSQLTable,
        TSQLTableDisplayComponentTypeInfoConst.cTypeVersionSQLTable, aLibName, aLibVersion,
        TSQLTableDisplayComponent, TSQLTableDisplayComponentSettingList);
end;

{ TRunSQLTableDisplayComponentTypeInfo }

constructor TRunSQLTableDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
begin
    inherited Create(TSQLTableDisplayComponentTypeInfoConst.cTypeNameSQLTable,
        TSQLTableDisplayComponentTypeInfoConst.cTypeVersionSQLTable, aLibName, aLibVersion,
        TSQLTableDisplayComponent);
end;


end.
