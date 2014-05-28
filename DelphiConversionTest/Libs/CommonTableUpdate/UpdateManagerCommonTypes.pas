unit UpdateManagerCommonTypes;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Should be the only object that uses dbTables
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.06.08 pk                               TN4148    Code copied from Winlissy
  18.05.10 wl  TFileUtilities.GetApplicationDataPath  TN5111   kopiert von FileUtilities.pas
  11.04.11 wl                               TN5549    alles bis auf TLogManager entfernt
  -------------------------------------------------------------------------------------------------- }


interface


type
    TLogManager = class
    private const
        cLogFileName = 'UpdaterLog.dat';
        class var uLogManager: TLogManager;
        class function GetInstance: TLogManager; static;
    protected
        fLogFileName: string;
        fCurrentHour: word;
        class procedure WriteIntoLog(const aLogText: string; const aLogFileName: string);
    public
        constructor Create();
        procedure Log(const aLogText: string; aLogType: Word);
        class procedure CreateInstance;
        class procedure DestroyInstance;
        class property Instance: TLogManager read GetInstance;
    end;


implementation


uses
    Windows,
    SysUtils,
    Forms,
    ShlObj,
    UtilLib,
    FileUtilities;

{ TLogManager }

class procedure TLogManager.CreateInstance();
begin
    if Assigned(uLogManager) then
        EXIT;
    uLogManager := TLogManager.Create();
end;

class procedure TLogManager.DestroyInstance();
begin
    uLogManager.Free;
end;

class function TLogManager.GetInstance(): TLogManager;
begin
    result := uLogManager;
end;

constructor TLogManager.Create();
begin
    inherited Create();
    fLogFileName := TFileUtilities.GetApplicationDataPath() + cLogFileName;
    fCurrentHour := 25;
end;

class procedure TLogManager.WriteIntoLog(const aLogText: string; const aLogFileName: string);
var
    xLogFile: Textfile;
begin
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

procedure TLogManager.Log(const aLogText: string; aLogType: Word);
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
        WriteIntoLog('Date: ' + DateToStr(Date), fLogFileName);
        FCurrentHour := xHour;
    end;
    // -------------------------------------------------------------- Hinzufügen von Datum und Text
    WriteIntoLog(TimeToStr(Time) + Format('%4s: ', [IntToStr(aLogType)]) + aLogText, fLogFileName);
end;


end.
