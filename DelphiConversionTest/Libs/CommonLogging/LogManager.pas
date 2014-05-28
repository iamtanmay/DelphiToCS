{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : class that includes all logging, the basic logging (Error.dat) & process log
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  17.03.03 wl                               TN1332.2 initial version
  18.03.03 wl  GetLogFilePath               TN1332.2 new property function
  19.03.03 wl  CreateProcessLog             TN1332.4 benutzt gmGetAliasPath
  20.03.03 wl  CreateProcessLog             TN1332.4 bekommt Dateinamen mit Pfad übergeben
  24.03.03 wl  WrCommonLog                  TN1453   Fileobjekt wird auf jeden Fall wieder geschlossen
  25.03.03 wl  WriteLog                     TN1453   bei CFR21 Mode ccmPrepared wird in jede Zeile "not complient" geschrieben
  27.03.03 wl  Create                       TN1439.4 CFR21Mode kann sich ändern - darf nicht redundant hier gespeichert werden
  27.03.03 wl  WriteIntoLog                 TN1439.4 WrCommonLog als allgemeine class methode
  10.04.03 wl  FreeProcessLog               TN1332.4 Wenn kein ProcessLog vorhanden, passiert nichts
  16.04.03 wl  WriteLog                     TN1439.4  - CFR21 Part 11 Prepare Mode! (kligt besser)
  16.04.03 wl  StopStartupLogging           TN1332.4 beendet das Aufzeichnen der ersten Log-Zeilen
  30.05.03 wl  AddMemoLine                  TN1485.1 public class methods
  30.10.03 tbh WriteIntoLog                 TN1645   keine Anzeige von Exceptions aus Log-Versuch
  24.06.04 wl                               TN2007   FileCtrl-Aufrufe durch Utility-Aufrufe ersetzt
  11.06.07 wl  WriteLog                     TN3722.1 "CFR21 Part 11 Prepare Mode" wird nur noch hinter das Datum geschrieben
  11.06.07 wl  WriteLog                     TN3722.2 Verkürzung auf 254 Zeichen entfernt
  11.06.07 wl  MoveErrorFile                TN3722.3 Statt CopyFile und DeleteFile wird MoveFile verwendet - das spart enorm Rechenzeit
  --------------------------------------------------------------------------------------------------
  LogManager.pas:
  01.08.07 wl                               TN3811.2 unit umbenannt und erweitert
  01.08.07 wl                               TN3811.2 neu: Instanz gLogManager
  01.08.07 wl  Log, LogF                    TN3811.2 ersetzt alle bisherigen Logfunktionen in Communikation,PosTools,..
  03.08.07 wl                               TN3811.2 benutzt gmMoveFile statt MoveFile
  31.08.07 wl  Initialize                   TN3811.4 wird später aufgerufen als Create
  14.04.08 wl  LOG_...                      TN4060   von AppTypes hierher
  14.04.08 wl  Initialize                   TN4060   parameter StartNewLog instead of AppPurpose
  21.05.08 wl  LogFT                        TN4119   new uses TTypeSafeFormat
  09.07.08 pk  fStartupLog                  TN3944   removed. causes problems in multi threading
  10.11.08 pk  fOnDetermineContextDescription TN4280 New: for logging thread information for each log line
  12.01.09 wl  WrLog                        TN4383   fCriticalSection verhindert, dass beim Schreiben aus mehreren Threads heraus Zeilen verloren gehen
  09.04.09 pk  SplitLogFileIfNeeded         TN4520   New: archives old file and creates new file if fLineCount >= fLogMaxLinesPerFile
  06.08.09 wl  TSettingsMainStringLoader    TN4702   Strings werden direkt geladen
  24.08.09 pk  Log                          TN4735   OnDisplayLogText is no longer done in main thread
  13.04.10 wl                               TN5044   uses FileUtilities
  19.04.10 wl  CreateInstance,DestroyInstance  TN5044   neu zur Vermeidung von Zugriffen auf gLogManager
  19.04.10 wl                               TN5044   const-Werte zur Klasse hinzugefügt
  24.06.10 wl  Create                       TN5123   Name der Logdatei --> AppInstanceLogging
  24.06.10 wl  LOG_INFO,...                 TN5123   Const-Werte entfernt
  14.12.10 wl  ArchiveFiles,MoveErrorFile   TN5411   Funktionen modernisiert, DateDelimiter nicht mahr nötig
  31.12.10 wl  ArchiveFiles                 TN5419   das Verzeichnis Logfiles wird erzeugt, wenn es nicht da ist
  23.03.11 wl  DoWriteLog                   TN5514   Zeit jetzt mit Millisekunden
  29.05.12 wl  TDisplayLogInfoType          TN5904   neu
  29.05.12 wl  Log                          TN5904   mit ThreadID als Parameter
  27.01.13 wl  LogFileName                  TN6069   new property
  -------------------------------------------------------------------------------------------------- }

unit LogManager;


interface


uses
    StdCtrls,
    Classes,
    SyncObjs;

type
    TDisplayLogInfoType = (Never, DebugOnly, Always);
    TDisplayLogText = procedure(const aLogText: string; aThreadID: integer; aDisplayType: TDisplayLogInfoType)
        of object;
    TDetermineContextDescription = procedure(var vContextText: string) of object;

    TLogManager = class
    private const
        cLogInfo = $0100;
    private
        FCurrentHour: word;
        fLineCount: integer;
        fLogPageCount: integer;
        // FStartupLog: TStringList;
        FStartupLogging: boolean;
        // Common log file
        FLogFilePath: string;
        FLogFileName: string;
        FLogFileArchives: string;
        FLogStoreDays: integer;
        fLogMaxLinesPerFile: integer;

        // Process log file
        FProcessLogFileName: string;
        FProcessLogFile: Textfile;
        // Log Display (aus Communication)
        fOnDisplayLogText: TDisplayLogText;
        fCriticalSection: TCriticalSection;

        class var uInstance: TLogManager;
        //
        procedure MoveErrorFile;
        procedure ArchiveFiles;
        procedure WrProcessLog(const aLogText: string);
        procedure DoWrLog(const aLogText: string);
        procedure WrLog(const aLogText: string; aOnlyProcessLog: boolean);
        function GetProcessLogFileName: string;
        function GetLogFilePath: string;
        procedure StartNewLog;
        procedure SplitLogFileIfNeeded();
        procedure DoWriteLog(const aLogText: string; aLogType: Word; aOnlyProcessLog: boolean;
            aThreadID: integer);
        // public class methods
        class procedure WriteIntoLog(aLogText, aLogFileName: string);
        class function GetInstance: TLogManager; static;
        class function GetDisplayTypeFromBoolean(aValue: boolean): TDisplayLogInfoType;
        constructor Create(const aLogFileName: string);
    public
        // constructor
        destructor Destroy; override;
        class procedure CreateInstance(const aLogFileName: string);
        class procedure DestroyInstance();
        class property Instance: TLogManager read GetInstance;

        // public methods
        procedure Initialize(const aLogFilePath, aLogFileArchives: string;
            const aLogStoreDays, aLogMaxLinesPerFile: integer; aStartNewLog: boolean);
        procedure Log(const aLogText: string; aDisplay: boolean); overload;
        procedure Log(const aLogText: string; aDisplay: boolean; aLogType: integer); overload;
        procedure Log(const aLogText: string; aDisplayType: TDisplayLogInfoType); overload;
        procedure Log(const aLogText: string; aDisplayType: TDisplayLogInfoType; aLogType: integer); overload;
        procedure LogFT(const aLogText: string; const aArgs: array of const; aDisplay: boolean); overload;
        procedure LogFT(const aLogText: string; const aArgs: array of const; aDisplay: boolean;
            aLogType: integer); overload;

        // in C# nicht verfügbar! Stattdessen LogFT() verwenden!
        procedure LogF(const aLogText: string; const aArgs: array of const; aDisplay: boolean); overload;
        procedure LogF(const aLogText: string; const aArgs: array of const; aDisplay: boolean;
            aLogType: integer); overload;

        function CreateProcessLog(aFileName: string): boolean;
        procedure FreeProcessLog;
        procedure StopStartupLogging;
        // properties
        property ProcessLogFileName: string read GetProcessLogFileName;
        property LogFilePath: string read GetLogFilePath;
        property LogFileName: string read fLogFileName;
        property OnDisplayLogText: TDisplayLogText read fOnDisplayLogText write fOnDisplayLogText;
    end;

    // WL: Hack für die 1000 Methoden, die gLogManager benutzen:
function gLogManager(): TLogManager;


implementation


uses
    Windows,
    Forms,
    SysUtils,
    Dialogs,
    IOUtils,
    Types,
    ThreadUtils,
    FileUtilities,
    GeneralTypes,
    UtilLib;

function gLogManager(): TLogManager;
begin
    result := TLogManager.Instance;
end;

{ TLogManager }

constructor TLogManager.Create(const aLogFileName: string);
begin
    inherited Create;

    fCriticalSection := TCriticalSection.Create();

    FStartupLogging := false;
    FProcessLogFileName := '';
    FLogFileName := aLogFileName;

    FCurrentHour := 25;
    fLogPageCount := 1;

end;

class procedure TLogManager.CreateInstance(const aLogFileName: string);
begin
    uInstance := TLogManager.Create(aLogFileName);
end;

class procedure TLogManager.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;

procedure TLogManager.Initialize(const aLogFilePath, aLogFileArchives: string;
    const aLogStoreDays, aLogMaxLinesPerFile: integer; aStartNewLog: boolean);
begin

    FLogFilePath := aLogFilePath;
    FLogStoreDays := aLogStoreDays;
    FLogFileArchives := aLogFileArchives;
    fLogMaxLinesPerFile := aLogMaxLinesPerFile;
    // if application = Sampler or Layouter, start a new log (Error.dat)
    if (aStartNewLog) then
        StartNewLog();
end;

destructor TLogManager.Destroy;
begin
    // FStartupLog.Free;

    fCriticalSection.Free;

    inherited;
end;

procedure TLogManager.StopStartupLogging;
begin
    FStartupLogging := false;
end;

procedure TLogManager.StartNewLog;
begin
    ArchiveFiles; // Anlegen des Archivs und Umkopieren der Dateien
    MoveErrorFile; // Umkopieren der aktuellen Error.dat
    // FStartupLog := TStringList.Create;
    FStartupLogging := false;
    fLineCount := 0;
end;

procedure TLogManager.DoWriteLog(const aLogText: string; aLogType: Word; aOnlyProcessLog: boolean;
    aThreadID: integer);
var
    xPresent: TDateTime;
    xHour, xMin, xSec, xMSec: word;
begin
    // ------------------------------------------------------ Überprüfen, ob ein neuer Tag begonnen hat
    xPresent := Now;
    DecodeTime(xPresent, xHour, xMin, xSec, xMSec);
    // ------------------------------------------- Hinzufügen einer Extra-Zeile für einen neuen Tag
    if (FCurrentHour > xHour) then
    begin
        WrLog('Date: ' + DateToStr(xPresent), aOnlyProcessLog);
        FCurrentHour := xHour;
    end;

    // Hinzufügen von Zeit, ThreadID und Text
    WrLog(Format('%.2d:%.2d:%.2d:%.3d %4s: %4d: %s', [xHour, xMin, xSec, xMSec, IntToStr(aLogType), aThreadID,
        aLogText]), aOnlyProcessLog);
end;

procedure TLogManager.DoWrLog(const aLogText: string);
begin
    WriteIntoLog(aLogText, FLogFileName);
end;

procedure TLogManager.SplitLogFileIfNeeded();
const
    cTextSplit =
        '------------------------------------------------- Log file split, Page(%d) - %s ---------------------------------------------';
begin
    if fLogMaxLinesPerFile <= 0 then
        EXIT;

    if fLineCount < fLogMaxLinesPerFile then
        EXIT;

    DoWrLog(Format(cTextSplit, [fLogPageCount, 'END']));
    self.StartNewLog();
    Inc(fLogPageCount);
    DoWrLog(Format(cTextSplit, [fLogPageCount, 'BEGIN']));
end;

procedure TLogManager.WrLog(const aLogText: string; aOnlyProcessLog: boolean);
begin
    // write into startup log
    // if (FStartupLogging) then
    // FStartupLog.Add(aLogText);

    // write into common log file
    if (not aOnlyProcessLog) then
    begin

        fCriticalSection.Acquire;
        try
            SplitLogFileIfNeeded();

            DoWrLog(aLogText); // Common Log
            Inc(fLineCount);
        finally
            fCriticalSection.Release;
        end;
    end;

    // write into process log file
    WrProcessLog(aLogText);
end;

procedure TLogManager.WrProcessLog(const aLogText: string);
begin
    if (FProcessLogFileName <> '') then
        try
            Writeln(FProcessLogFile, aLogText);
            Flush(FProcessLogFile); // ensure that text has been written to the external file
        except
            // raise Exception.Create(' Error on writing Error.Dat :'+LogStr);
        end;
end;

class procedure TLogManager.WriteIntoLog(aLogText, aLogFileName: string);
var
    xLogFile: Textfile;
begin
    // ------------------------------------------------ LogText wird in die Error.dat-Datei geschrieben
    AssignFile(xLogFile, aLogFileName);
    if not FileExists(aLogFileName) then
    begin
{$I-}
        Rewrite(xLogFile);
        if IOResult = 0 then;
{$I+}
    end;
    try
        Append(xLogfile);
        if IOResult = 0 then
        begin
            Writeln(xLogfile, aLogText);
            CloseFile(xLogFile);
        end;
    except
        try
            CloseFile(xLogFile);
        except
        end;
    end;
end;

function TLogManager.CreateProcessLog(aFileName: string): boolean;
// var x: integer;
begin
    result := false;
    if TFileUtilities.ForceDirectories(ExtractFilePath(aFileName)) then
    begin
        try
            // initialize the file variable
            AssignFile(FProcessLogFile, aFileName);

            // these 2 methods can raise exceptions!
            Rewrite(FProcessLogFile);
            Append(FProcessLogFile);

            FProcessLogFileName := aFileName;
            FStartupLogging := false;
            result := true;
        except
            on E: Exception do
                ShowMessage(E.Message);
        end;
    end;

    // write startup log into process log
    if (result) then
    begin
        // for x := 0 to FStartupLog.Count-1 do
        // WrProcessLog(FStartupLog[x]);

        WrProcessLog('...');
        WrProcessLog('...');
        WrProcessLog('...');
    end;
end;

procedure TLogManager.FreeProcessLog;
begin
    if (FProcessLogFileName <> '') then
    begin
{$I-}CloseFile(FProcessLogFile); {$I+}
        FProcessLogFileName := '';
    end;
end;

// Error.dat umbenennen und in Archivverzeichnis verschieben
procedure TLogManager.MoveErrorFile;
var
    xDate: TDateTime;
    xDestPath, xArchivDir: string;
    x: integer;
begin
    if not TFile.Exists(fLogFileName) then
        EXIT;

    xDate := TFile.GetLastWriteTime(fLogFileName);

    xArchivDir := IncludeTrailingPathDelimiter(FLogFilePath) + FormatDateTime('yyyymmdd', xDate);

    // Suchen und Erstellen des Logfile-Verzechnisses
    if not TDirectory.Exists(xArchivDir) then
        TDirectory.CreateDirectory(xArchivDir);

    // Festlegen des Archivfile-Namens
    x := 0;
    repeat
        inc(x);
        xDestPath := IncludeTrailingPathDelimiter(xArchivDir) + Format('Error%.3d.dat', [x]);
    until not TFile.Exists(xDestPath);

    // Verschieben von Error.dat
    if not TFileUtilities.MoveFile(FLogFileName, xDestPath) then
    begin
        Application.Terminate;
        raise Exception.Create(TLanguageString.
            Read('Unable to copy file Error.dat in logfile directory: {0}!',
            'Die Datei Error.dat kann nicht in das Logfile-Verzeichnis {0} kopiert werden!', [xArchivDir]));
    end;
end;

// Alte Logfile-Archivverzeichnisse werden gelöscht oder umkopiert
procedure TLogManager.ArchiveFiles;
var
    xDirectories: TStringDynArray;
    xDirName: string;
    x: integer;
    xDirDate: TDateTime;
begin
    // Erstellen des Logfile-Verzechnisses
    if not TDirectory.Exists(FLogFilePath) then
        TDirectory.CreateDirectory(FLogFilePath);

    // suchen nach alten Logverzeichnissen
    xDirectories := TDirectory.GetDirectories(FLogFilePath);

    for x := 0 to Length(xDirectories) - 1 do
    begin

        xDirName := ExtractFilename(xDirectories[x]);
        try
            xDirDate := EncodeDate(StrToInt(Copy(xDirName, 1, 4)), StrToInt(Copy(xDirName, 5, 2)),
                StrToInt(Copy(xDirName, 7, 2)));

            // herausfinden, ob das Verzeichnis zu alt ist
            if (Date - xDirDate > fLogStoreDays) then
            begin

                if (FLogFileArchives = '') then
                begin

                    // Die Dateien werden gelöscht
                    TDirectory.Delete(xDirectories[x], true);
                end
                else
                begin
                    // Ein Archiv wird angelegt und die Dateien verschoben
                    TDirectory.Move(xDirectories[x], IncludeTrailingPathDelimiter(FLogFileArchives) +
                        xDirName);
                end;
            end;
        except
            // kein datiertes Logfile-Verzeichnis: ignorieren
            CONTINUE;
        end;
    end;
end;

class function TLogManager.GetDisplayTypeFromBoolean(aValue: boolean): TDisplayLogInfoType;
begin
    if (aValue) then
        EXIT(TDisplayLogInfoType.Always)
    else
        EXIT(TDisplayLogInfoType.Never)
end;

class function TLogManager.GetInstance: TLogManager;
begin
    result := uInstance;
end;

function TLogManager.GetLogFilePath: string;
begin
    result := FLogFilePath;
end;

function TLogManager.GetProcessLogFileName: string;
begin
    result := FProcessLogFileName;
end;

procedure TLogManager.Log(const aLogText: string; aDisplay: boolean; aLogType: integer);
begin
    self.Log(aLogText, GetDisplayTypeFromBoolean(aDisplay), aLogType);
end;

procedure TLogManager.Log(const aLogText: string; aDisplay: boolean);
begin
    self.Log(aLogText, GetDisplayTypeFromBoolean(aDisplay), cLogInfo);
end;

procedure TLogManager.Log(const aLogText: string; aDisplayType: TDisplayLogInfoType);
begin
    self.Log(aLogText, aDisplayType, cLogInfo);
end;

procedure TLogManager.Log(const aLogText: string; aDisplayType: TDisplayLogInfoType; aLogType: integer);
var
    xThreadID: integer;
begin
    xThreadID := TPlatformSpecificOS.GetCurrentThreadID;
    // Text anzeigen (in VCL-Komponenten)
    if (aDisplayType <> TDisplayLogInfoType.Never) and Assigned(fOnDisplayLogText) then
        self.OnDisplayLogText(aLogText, xThreadID, aDisplayType);

    // In Log-Datei schreiben
    self.DoWriteLog(aLogText, aLogType, false, xThreadID);
end;

procedure TLogManager.LogF(const aLogText: string; const aArgs: array of const; aDisplay: boolean;
    aLogType: integer);
begin
    self.Log(SysUtils.Format(aLogText, aArgs), aDisplay, aLogType)
end;

procedure TLogManager.LogF(const aLogText: string; const aArgs: array of const; aDisplay: boolean);
begin
    self.LogF(aLogText, aArgs, aDisplay, cLogInfo);
end;

procedure TLogManager.LogFT(const aLogText: string; const aArgs: array of const; aDisplay: boolean);
begin
    self.LogFT(aLogText, aArgs, aDisplay, cLogInfo);
end;

procedure TLogManager.LogFT(const aLogText: string; const aArgs: array of const; aDisplay: boolean;
    aLogType: integer);
begin
    self.Log(TTypeSafeFormat.Format(aLogText, aArgs), aDisplay, aLogType)
end;


end.
