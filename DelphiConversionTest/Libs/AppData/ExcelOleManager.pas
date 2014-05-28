{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Excel-OLE wrapper class
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  06.02.04 wl                               TN1414   initial version
  26.03.04 pk   Disconnect                  TN1836   New.  Called by destructor
  24.06.04 wl                               TN2007   uses Variants (nur Delphi 6 und 7)
  03.11.05 wl   IsExcelWorkbookOpen         TN2725   von ImportClasses hierher verschoben
  28.06.06 wl  TExcelOleManagerMethodSteps               TN3172    Neu: Objekt für den Excel-Export/Import von Methoden
  28.06.06 wl  TExcelOleManagerMethodSteps               TN3172    benutzt keine Tabelle, sondern das Method-Grid
  28.06.06 wl                               TN3172    Methoden-spezifisches --> TExcelOleManagerMethodSteps
  05.02.07 wl                               TN3543   uses Excel2000
  13.11.09 pk  TExcelOleManager             TN4867   only call excel.quit when excel variable is not equal to null
  25.11.09 pk  fExcel                       TN4867   changed to OLEVariant.  set to unassigned instead of null and use VarIsEmpty to check
  13.04.10 wl                               TN5044   uses FileUtilities
  18.06.10 pk  CreateXLSFileFromCSV         TN5152.1 from QryTools
  17.12.10 ts  PrintXLSFile                 TN5415   Neu (ohne Optionen für Schriftgröße etc.)
  12.04.11 ts  DeleteLines                  TN5548   new: Deletes a number of lines in an excel sheet
  11.07.12 wl                               TN5934   uses ExcelXP
  -------------------------------------------------------------------------------------------------- }

unit ExcelOleManager;


interface


uses
    db;

type
    TExcelOleManager = class
    private
        FExcel: OLEVariant;
    protected
        procedure CreateNewWorksheet(const aName: string);
        procedure SetWorksheetVisible();
        procedure SetFieldValue(const aRow, aColumn: integer; const aValue: string);
        function GetFieldValue(const aRow, aColumn: integer): string;
        function WorksheetHasName(const aName: string): boolean;
    public
        // constructor/destructor
        constructor Create;
        destructor Destroy; override;
        // public methods
        function Connect: boolean;
        procedure Disconnect();
        procedure Quit;
        function ConnectionExists: boolean;
        function ExportDataSet(aName: string; aDataSet: TDataSet; aIgnoreCol: integer): boolean;
        function ImportDataSet(aName: string; aDataSet: TDataSet; aIgnoreCol: integer): boolean;
        function WorksheetIsName(aName: string): boolean;
        function CreateXLSFromCSV(var aFileName: string): boolean;
        function CreateCSVFromXLS(var aFileName: string): boolean;
        procedure PrintXLSFile(aFileName: string);
        procedure DeleteLines(aFileName, aTableName: string; aCount: integer);

        // aus ImportClasses:
        class function IsExcelWorkbookOpen(aWorkbookName: string): boolean;
        class function CreateXLSFileFromCSV(aFileName: string; aDeleteCSV: boolean): boolean;
        class function CreateCSVFileFromXLS(var vFileName: string): boolean;
    end;


implementation


uses
    Variants,
    SysUtils,
    ComObj,
    ActiveX,
    ExcelXP,
    LogManager,
    FileUtilities;

{ TExcelOleManager }

constructor TExcelOleManager.Create;
begin
    inherited;
    FExcel := Unassigned;
end;

destructor TExcelOleManager.Destroy;
begin
    Disconnect();
    inherited;
end;

procedure TExcelOleManager.Disconnect();
begin
    try
        self.Quit;
    except
    end; // Very import to call Quit before setting reference to Null
    FExcel := Unassigned;
end;

function TExcelOleManager.Connect: boolean;
begin
    result := false;
    try
        Coinitialize(nil); // kann nicht schaden
        FExcel := CreateOleObject('Excel.Application'); // öffnet Excel als COM Server
        gLogManager.Log('Opening OLE Connection - Microsoft Excel Version ' +
            FExcel.Application.Version, false);
        result := true;
    except
        on E: Exception do
            gLogManager.Log(E.Message, false);
    end;
end;

function TExcelOleManager.ConnectionExists: boolean;
begin
    result := false;
    try
        gLogManager.Log('Microsoft Excel Version ' + FExcel.Application.Version, false);
        result := true;
    except
        on E: Exception do
            gLogManager.Log(E.Message, false);
    end;
end;

function TExcelOleManager.CreateXLSFromCSV(var aFileName: string): boolean;
begin
    // Erzeugt ein Excel File aus einem CSV File
    result := false;
    try
        gLogManager.Log('Microsoft Excel Version ' + FExcel.Application.Version + ' create XLS file from ' +
            aFilename, false);
        FExcel.Workbooks.Open(aFileName);
        // ,0,false,6,'','',true,xlWindows,gImport.Delimiter,false,false,0,false,0);
        aFileName := ChangeFileExt(aFileName, '.XLS');
        TFileUtilities.DeleteFile(aFileName);
        FExcel.ActiveWorkbook.SaveAs(aFileName, xlNormal);
        // ,'','',false,false,xlNoChange,xlLocalSessionChanges,false,null,null,0);
        FExcel.ActiveWorkbook.Close(false); // ,'',false,0);
        result := true;
    except
        on E: Exception do
            gLogManager.Log(E.Message, false);
    end;
end;

function TExcelOleManager.CreateCSVFromXLS(var aFileName: string): boolean;
begin
    // Erzeugt ein CSV File aus einem XLS File
    result := false;
    try
        gLogManager.Log('Microsoft Excel Version ' + FExcel.Application.Version + ' create CSV file from ' +
            aFilename, false);
        FExcel.Workbooks.Open(aFileName);
        // ,0,false,6,'','',true,xlWindows,gImport.Delimiter,false,false,0,false,0);
        aFileName := ChangeFileExt(aFileName, '.CSV');
        TFileUtilities.DeleteFile(aFileName);
        FExcel.ActiveWorkbook.SaveAs(aFileName, xlCSV);
        // ,'','',false,false,xlNoChange,xlLocalSessionChanges,false,null,null,0);
        FExcel.ActiveWorkbook.Close(false); // ,'',false,0);
        result := true;
    except
        on E: Exception do
            gLogManager.Log(E.Message, false);
    end;
end;

procedure TExcelOleManager.Quit;
begin
    if VarIsEmpty(fExcel) then
        EXIT;

    try
        FExcel.Quit;
    except
        on E: Exception do
            gLogManager.Log(E.Message, false);
    end;
end;

function TExcelOleManager.ExportDataSet(aName: string; aDataSet: TDataSet; aIgnoreCol: integer): boolean;
var
    xRow, xColumn: integer;
begin
    result := false;
    try
        self.CreateNewWorksheet(aName);
        xRow := 1;
        for xColumn := 0 to (aDataSet.FieldCount - 1 - aIgnoreCol) do
        begin
            self.SetFieldValue(xRow, xColumn + 1, aDataSet.Fields[xColumn].Fieldname);
        end;
        aDataSet.First;
        while not aDataSet.Eof do
        begin
            inc(xRow);
            for xColumn := 0 to (aDataSet.FieldCount - 1 - aIgnoreCol) do
            begin
                self.SetFieldValue(xRow, xColumn + 1, aDataSet.Fields[xColumn].AsString);
            end;
            aDataSet.next;
        end;
        self.SetWorksheetVisible();
        result := true;
    except
        on E: Exception do
            gLogManager.Log(E.Message, false);
    end;
end;

procedure TExcelOleManager.SetFieldValue(const aRow, aColumn: integer; const aValue: string);
begin
    FExcel.Worksheets.Item[1].Cells[aRow, aColumn].Value := aValue;
end;

function TExcelOleManager.GetFieldValue(const aRow, aColumn: integer): string;
begin
    result := FExcel.Worksheets.Item[1].Cells[aRow, aColumn].Value;
end;

procedure TExcelOleManager.CreateNewWorksheet(const aName: string);
begin
    FExcel.Workbooks.Add;
    FExcel.Worksheets.Item[1].Name := aName;
end;

procedure TExcelOleManager.SetWorksheetVisible();
begin
    FExcel.Visible := true;
end;

function TExcelOleManager.WorksheetHasName(const aName: string): boolean;
begin
    result := (FExcel.Worksheets.Item[1].Name = aName);
end;

function TExcelOleManager.WorksheetIsName(aName: string): boolean;
begin
    result := false;
    try
        if self.WorksheetHasName(aName) and (FExcel.Worksheets.Item[1].Cells[2, 1].value = aName) then
            result := true;
    except
        on E: Exception do
            gLogManager.Log(E.Message, false);
    end;
end;

function TExcelOleManager.ImportDataSet(aName: string; aDataSet: TDataSet; aIgnoreCol: integer): boolean;
var
    xRow, xColumn: integer;
    xVariant: Variant;
begin
    result := false;

    // delete data of DataSet
    aDataSet.First;
    while not aDataSet.eof do
        aDataSet.Delete; // !!!!
    aDataSet.Refresh;
    xRow := 2;

    // import data
    try
        while (FExcel.Worksheets.Item[1].Cells[xRow, 1].value = aName) do
        begin
            aDataSet.Insert; // .Append;
            for xColumn := 0 to (aDataSet.FieldCount - 1 - aIgnoreCol) do
            begin
                xVariant := FExcel.Worksheets.Item[1].Cells[xRow, xColumn + 1].value;

                if (aDataSet.Fields[xColumn].DataType = ftBoolean) then
                begin
                    if (xVariant = true) or (UpperCase(xVariant) = 'WAHR') or
                        (UpperCase(xVariant) = 'TRUE') then
                        aDataSet.Fields[xColumn].AsBoolean := true
                    else
                        aDataSet.Fields[xColumn].AsBoolean := false;
                end
                else
                    aDataSet.Fields[xColumn].value := xVariant;
            end;
            aDataSet.Post;
            inc(xRow);
        end;
        result := true;
    except
        on E: Exception do
            gLogManager.Log(E.Message, false);
    end;
end;

class function TExcelOleManager.IsExcelWorkbookOpen(aWorkbookName: string): boolean;
var
    xUnknown: IUnknown;
    xExcel: ExcelApplication;
    i: integer;
begin
    result := false;
    try
        try
            // Check if there's an instance already running
            OleCheck(GetActiveObject(Class_ExcelApplication, nil, xUnknown));
            // Make shure, this is indeed excel and get it's native interface
            OleCheck(xUnknown.QueryInterface(ExcelApplication, xExcel));
        except
            Exit;
        end;

        for i := 1 to xExcel.Workbooks.Count do
        begin
            if xExcel.Workbooks.Item[i].Name = aWorkbookName then
            begin
                result := true;
                Exit;
            end;
        end;
    finally
        xExcel := nil;
    end;
end;

class function TExcelOleManager.CreateXLSFileFromCSV(aFileName: string; aDeleteCSV: boolean): boolean;
var
    xExcel: TExcelOleManager;
    xCSVFile: string;
begin
    xCSVFile := aFileName;
    xExcel := TExcelOleManager.Create;
    result := false;
    if (xExcel.Connect) then
    begin

        // create Excel File from CSV File
        result := xExcel.CreateXLSFromCSV(aFileName);
        xExcel.Quit;

        // delete csv file
        if (aDeleteCSV) then
            try
                DeleteFile(xCSVFile);
            except
                on E: Exception do
                    gLogManager.Log(E.Message, false);
            end;
    end;
    xExcel.Free;
end;

class function TExcelOleManager.CreateCSVFileFromXLS(var vFileName: string): boolean;
var
    xExcel: TExcelOleManager;
begin
    xExcel := TExcelOleManager.Create;
    result := false;
    if (xExcel.Connect) then
    begin

        // create CSV File from Excel File
        result := xExcel.CreateCSVFromXLS(vFileName);
        xExcel.Quit;
    end;
    xExcel.Free;
end;

procedure TExcelOleManager.PrintXLSFile(aFileName: string);
var
    xColumnRange: OleVariant;
    x, y: integer;
begin
    FExcel.Workbooks.Open(aFileName);
    // Man kann auch noch einige Einstellungen von "Seite Einrichten" anpassen
    FExcel.ActiveSheet.PageSetup.Orientation := xlLandscape;
    FExcel.ActiveWindow.DisplayGridlines := true;

    FExcel.ActiveSheet.Cells.SpecialCells(xlCellTypeLastCell, EmptyParam).Activate; // PERSONAL.XLSB!Makro1
    // Get the value of the last row
    x := FExcel.ActiveSheet.UsedRange.Rows.Count;
    // Get the value of the last column
    y := FExcel.ActiveSheet.UsedRange.Columns.Count;

    FExcel.ActiveSheet.Range['A1:' + Chr(y + 64) + IntToStr(x)].Borders.Weight := xlThin;
    FExcel.ActiveSheet.Range['A1:' + Chr(y + 64) + IntToStr(x)].Font.Size := 8;
    xColumnRange := FExcel.ActiveSheet.Columns;
    xColumnRange.Columns.Autofit;
    FExcel.ActiveSheet.PageSetup.CenterFooter := '&S/&A';

    FExcel.ActiveWorkbook.Save;
    // Ausdrucken
    FExcel.Worksheets.PrintOut;
    FExcel.ActiveWorkbook.Close(false);
end;

procedure TExcelOleManager.DeleteLines(aFileName, aTableName: string; aCount: integer);
var
    x: integer;
begin
    FExcel.Workbooks.Open(aFileName);

    FExcel.Worksheets.Item[aTableName].Activate;

    for x := acount downto 1 do
    begin
        FExcel.ActiveSheet.Rows[x].Delete;
    end;
    x := FExcel.ActiveSheet.UsedRange.Rows.Count;
    if x > 1 then

        FExcel.ActiveWorkbook.Save;
    FExcel.ActiveWorkbook.Close(false);
end;


end.
