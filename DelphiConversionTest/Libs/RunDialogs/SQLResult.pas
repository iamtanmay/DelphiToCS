unit SQLResult;
{ --------------------------------------------------------------------------------------------------
  Anzeige der Action WrSQL
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure       Änderung / Neuerung
  -------- --  ---------------------    ------------------------------------------------------------
  14.11.00 mo                           neu erstellt
  27.12.02 wl  gmParseSQLStr            TN1293.5 aus qryTools hierher verschoben
  27.12.02 wl  gmReadSQLFile            TN1293.5 aus qryTools hierher verschoben
  27.12.02 wl  gmReadExcelRackInfo      TN1293.5 aus qryTools hierher verschoben
  27.12.02 wl  gmWriteResultfile        TN1293.5 aus qryTools hierher verschoben
  11.02.03 wl                           TN1345 XAP-Compiler-Direktiven entfernt
  04.03.03 wl                           TN1345 global_Alias entsorgt
  12.03.03 wl                           TN1293.5 uses posTools
  04.02.04 pk                           TN1719  gmSQLUpdate, gmSQLWriteFile from BasicLThr
  04.02.04 pk                           TN1719  New functions for Thread-Synchronized access
  14.06.04 pk  gmParseSQLStr            TN1980.1 METHODNAME uses CurrentRunName instead of PMethodName, RUNNAME uses LayoutRunName instead of PMethodName/ScriptName
  15.06.04 pk  gmParseSQLStr            TN1980.0 can now replace parameters in the SQLStr with the args given
  18.06.04 pk  gmSQLWriteFile           TN1996   New subpath parameter
  24.06.04 wl                           TN2007   uses Variants (nur Delphi 6 und 7)
  03.08.04 pk  gmParseSQLStr            TN2072 RunLayoutName instead of LayoutRunName
  19.01.05 pk                           TN2281.0  TStringArray, TIntArray from AppTypes to GeneralTypes
  17.03.05 pk  gmParseSQLStr            TN2352.1  Get CurrentRunName via ThrMan
  24.11.05 pk                           TN2805    ObjSampl replaced by ZARunnerObjects
  02.01.06 wl  gmSQLWriteFile           TN2876    Bestimmung des Dateinamens --> TSQLWriteFileAction
  02.01.06 wl  gmSQLWriteFile           TN2876    Erzeugen (und Löschen) einer temporären CSV-Datei: von TSQLWriteFileAction hierher
  18.01.06 wl  gmSQLWriteValues         TN2885    der Inhalt des ersten Records einer SELECT-Abfrage wird in TStrings geschrieben
  17.02.06 wl  gmWriteResultfile        TN2935    mit TEXT:... kann in eine Result-Datei freier Text eingefügt werden
  17.02.06 wl  gmWriteResultfile        TN2935    NOHEADER vor dem SQL statement bewirkt, dass Feldnamen nicht gezeigt werden!
  17.02.06 wl  alle Funktionen          TN2939    massive Änderungen, im Fehlerfall wird immer ErrBox mit Abort,Retry,Ignore gezeigt
  05.05.06 pk  gmParseSQLStr            TN3085    new : parse #PRIORITY#
  08.05.06 pk  gmParseSQLStr            TN3085    GetCurrentPriority moved to higher functions
  03.12.06 wl                           TN3243   alle ErrBox-Aufrufe durch ErrBoxSimple oder ErrBoxSerialIntf ersetzt
  16.12.06 pk  gmDisplaySQLError        TN3476   do not set global error, because this function is called in context of main thread
  16.12.06 pk  gmDisplaySQLError        TN3476   use eiabortretry instead of just abort
  13.03.07 wl  gmSQLWriteFile           TN3627    wenn aSeparator = TAB --> aSeparator := #9
  16.04.07 wl  gmSQLWriteFile           TN3547    Umstellung auf DataAdaptor statt SQL-File
  16.04.07 wl  gmParseSQLStr            TN3547    --> SQLTermParserInterface
  09.11.07 pk                           TN3923    class references to TErrorMessageFactory changed to gErrorMessageFactory
  08.09.08 pk                           TN4215    no uses Threadregistry for finding RunName, MethodName instead of "global" object
  06.11.08 pk                           TN4280    ThreadAPI instead of ThreadManager
  17.11.08 pk                           TN4280    ThreadAPI instead of ThreadManagerRun
  16.03.09 pk                           TN4470    Call EventManager.GetCurrentSubMethodName instead of ThreadAPI.GetExecDataName
  06.07.09 pk                           TN4585.4  code moved to SQLRunActionUtility
  08.09.09 pk                           TN4753    uses changed
  20.05.10 wl                           TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  21.06.10 wl                           TN5160   Position = poScreenCenter
  11.11.10 wl  Display                  TN5335   Autosize soll sich am Header-Text orientieren
  11.11.10 pk                           TN5152.1  uses DataProviderFactory
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Classes,
    Controls,
    ComCtrls,
    Forms,
    GeneralTypes;

type
    TSQLResultForm = class(TForm)
        ListView1: TListView;
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormCreate(Sender: TObject);
    private
        procedure Display(const aSQLStr: string; const aFileName: string);
    public
        class procedure InstanceDisplay(const aSQLStr: string; const aFileName: string);
    end;


implementation


{$R *.DFM}

uses
    DataProvider,
    ControlUtils,
    DataProviderFactory;

{ TSQLResultForm }

procedure TSQLResultForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caFree;
end;

procedure TSQLResultForm.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
end;

procedure TSQLResultForm.Display(const aSQLStr: string; const aFileName: string);
var
    xQuery: TDataProvider;
    x: integer;
    xItem: TListItem;
begin
    if aSQLStr = '' then
        EXIT;

    xQuery := TDataProviderFactory.Instance.CreateDataProvider();
    try
        self.Caption := 'SQL Result <' + aFileName + '>';

        // SELECT
        xQuery.SelectAndOpen(aSQLStr, true);

        for x := 0 to xQuery.FieldCount - 1 do
        begin
            self.ListView1.Columns.Add;
            self.ListView1.Columns[x].Caption := xQuery.Fields[x].Fieldname;
            self.ListView1.Columns[x].AutoSize := true;
            self.ListView1.Columns[x].Width := ColumnHeaderWidth;

        end;

        // Inhalt
        while not xQuery.EOF do
        begin
            xItem := self.ListView1.Items.Add;
            xItem.Caption := xQuery.Fields[0].AsString;
            for x := 1 to xQuery.FieldCount - 1 do
                xItem.Subitems.Add(xQuery.Fields[x].AsString);

            xQuery.Next;
        end;

    finally
        xQuery.Close;
    end;

    self.Show();
end;

class procedure TSQLResultForm.InstanceDisplay(const aSQLStr: string; const aFileName: string);
var
    xForm: TSQLResultForm;
begin
    xForm := TSQLResultForm.Create(Application);
    xForm.Display(aSQLStr, aFileName);

end;


end.
