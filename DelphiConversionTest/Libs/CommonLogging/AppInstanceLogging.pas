{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Inherits instances for Logging library
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  31.08.07 wl                               TN3811.4  initial version
  03.09.07 wl  Purpose                      TN3811.4  neue Member-Variable
  14.04.08 wl  Purpose                      TN4060    --> AppInstanceDataAdaptorCommon
  09.04.09 pk  InitializeLogManager         TN4520   New param aLogMaxLinesPerFile
  21.09.09 pk  uRefCount                    TN4753   New
  13.04.10 wl                               TN5044   FreeAndNil statt Free
  19.04.10 wl  CreateInstance               TN5044   uRefCount wird bei nicht vorhandener Instanz auf 0 gesetzt
  17.05.10 wl  Create                       TN5111   Logfile wird in C:\ProgramData geschrieben
  -------------------------------------------------------------------------------------------------- }

unit AppInstanceLogging;


interface


uses
    LogManager;

type
    TAppInstanceLogging = class sealed
    private
        fStartNewLog: boolean;
        class var uInstance: TAppInstanceLogging;
        class var uRefCount: integer;
        constructor Create(aStartNewLog: boolean);
    public
        destructor Destroy(); override;
        class procedure CreateInstance(aStartNewLog: boolean);
        class function Instance(): TAppInstanceLogging;
        class procedure DestroyInstance;

        procedure InitializeLogManager(const aLogFilePath, aLogFileArchives: string;
            const aLogStoreDays, aLogMaxLinesPerFile: integer);
    end;


implementation


uses
    SysUtils,
    FileUtilities;

{ TAppInstanceLogging }

class procedure TAppInstanceLogging.CreateInstance(aStartNewLog: boolean);
begin
    // create instance if instance does not exist
    if not Assigned(uInstance) then
    begin
        uRefCount := 0;
        uInstance := TAppInstanceLogging.Create(aStartNewLog);
    end;

    Inc(uRefCount);
end;

class function TAppInstanceLogging.Instance(): TAppInstanceLogging;
begin
    result := uInstance;
end;

class procedure TAppInstanceLogging.DestroyInstance;
begin
    if uRefCount = 1 then
        FreeAndNil(uInstance);

    Dec(uRefCount);
end;

constructor TAppInstanceLogging.Create(aStartNewLog: boolean);
const
    STR_LOGFILE_NAME = 'Error.dat';
var
    xLogPath: string;
begin
    inherited Create;

    fStartNewLog := aStartNewLog;
    xLogPath := TFileUtilities.GetApplicationDataPath();
    TFileUtilities.ForceDirectories(xLogPath);
    TLogManager.CreateInstance(xLogPath + STR_LOGFILE_NAME);
end;

destructor TAppInstanceLogging.Destroy();
begin
    TLogManager.DestroyInstance;

    inherited;
end;

procedure TAppInstanceLogging.InitializeLogManager(const aLogFilePath, aLogFileArchives: string;
    const aLogStoreDays, aLogMaxLinesPerFile: integer);
begin
    TLogManager.Instance.Initialize(aLogFilePath, aLogFileArchives, aLogStoreDays, aLogMaxLinesPerFile,
        fStartNewLog);
end;


end.
