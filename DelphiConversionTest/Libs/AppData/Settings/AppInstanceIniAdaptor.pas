{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Inherits instances for IniAdaptor library
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  31.08.07 wl                               TN3811.4  initial version
  03.09.07 wl  Create                       TN3811.4  neuer Parameter Purpose
  06.08.08 pk  Create                       TN4165.1  read app and robot settings
  06.08.08 pk  fMethodSettings              TN4165.1  removed
  13.08.08 pk  Create                       TN4165.1  read all settings
  16.02.09 wl                               TN4370    geändert: SetLanguage
  09.04.09 pk  InitLogging                  TN4520   New LogMaxLinesPerFile
  08.09.09 pk  Create                       TN4753    read only app settings
  26.10.09 wl  GetLocalIniFileName          TN4831    ILocalIniFile wird nicht mehr benutzt
  13.04.10 wl                               TN5044   uses FileUtilities
  17.05.10 wl                               TN5111   LocalIniPath,LogfilePath: Bei Win7 ProgramData statt Program Files
  20.05.10 wl                               TN5116   überarbeitet
  27.01.13 wl  Create                       TN6069   verwendet TAppInstanceAppData.GetDataPath
  -------------------------------------------------------------------------------------------------- }

unit AppInstanceIniAdaptor;


interface


uses
    CommonTypes,
    IniSettings,
    IniAccess;

type
    TAppInstanceIniAdaptor = class
    private const
        cLocalIniFileName = 'APPDATA.TMP';
    private
        // benutzt TAppInstanceDataAdaptorCommon
        fDataPath: string;
        fSettings: TIniSettings;

        class var uInstIniAdaptor: TAppInstanceIniAdaptor; // Single Instance

        function GetLocalIniFileName: string;
        procedure ReadAndSetLanguage();
        procedure InitLogging();
        constructor Create();
    public
        destructor Destroy(); override;
        class function CreateInstance(): TAppInstanceIniAdaptor;
        class procedure DestroyInstance();
        //
        function CreateIndividualArea(aAreaName: string; aAccessType: TIniAccessType): TIniAccess;
        function CreateAnyArea(aAreaName: string): TIniAccess;
        //
        property DataPath: string read fDataPath;
        property Settings: TIniSettings read fSettings;
        property LocalIniFileName: string read GetLocalIniFileName;
        class property Instance: TAppInstanceIniAdaptor read uInstIniAdaptor;
    end;


implementation


uses
    Forms,
    SysUtils,
    DataAdaptor,
    LogManager,
    AppInstanceAppData,
    AppInstanceLogging,
    GeneralTypes,
    FileUtilities;

{ TAppInstanceIniAdaptor }

class function TAppInstanceIniAdaptor.CreateInstance(): TAppInstanceIniAdaptor;
begin
    // create instance if instance does not exist
    if not Assigned(uInstIniAdaptor) then
        uInstIniAdaptor := TAppInstanceIniAdaptor.Create();

    // return instance
    result := uInstIniAdaptor;
end;

constructor TAppInstanceIniAdaptor.Create();
begin
    inherited Create;

    fDataPath := TAppInstanceAppData.GetDataPath; // subdirectory: Data;

    // Create Data Path (for Appdata.tmp)
    if not DirectoryExists(fDataPath) then
        CreateDir(fDataPath);

    // Check and create Run Alias (use
    fSettings := TIniSettings.Create(fDataPath);

    // read app settings
    fSettings.DataCache.ReadAreaIntoCache(STR_SETTINGS_AREA_APP);

    // Set Language for Ressource Loader
    ReadAndSetLanguage();

    // Set up Logging (Error.dat)
    InitLogging();

    // Log Prepare Mode
    if (TDataAdaptor.GetCFR21Mode = ccmPrepared) then
        gLogManager.Log('CFR21 Part 11 Prepare Mode!', false);
end;

destructor TAppInstanceIniAdaptor.Destroy;
begin
    FreeAndNil(fSettings);

    inherited;
end;

class procedure TAppInstanceIniAdaptor.DestroyInstance;
begin
    FreeAndNil(uInstIniAdaptor);
end;

function TAppInstanceIniAdaptor.GetLocalIniFileName: string;
begin
    result := TFileUtilities.IncludeTrailingPathDelimiter(self.DataPath) + cLocalIniFileName;
end;

procedure TAppInstanceIniAdaptor.ReadAndSetLanguage;
var
    xLanguage: integer;
    xIniAccess: IWinLissyIniAccess;
begin
    xIniAccess := fSettings.CreateAppIni();
    xLanguage := xIniAccess.ReadInteger('Display', 'Language');
    TLanguageString.SetLanguage(xLanguage);
end;

procedure TAppInstanceIniAdaptor.InitLogging();
var
    xIniAccess: IWinlissyIniAccess;
    xLogFilePath, xLogFileArchives: string;
    xLogStoreDays: integer;
    xLogMaxLinesPerFile: integer;
begin
    // read variables for logging
    xIniAccess := fSettings.CreateAppIni;
    xLogFilePath := xIniAccess.ReadString('Logfiles', 'Path');
    if (xLogFilePath = '') then
        xLogFilePath := TFileUtilities.GetApplicationDataPath() + 'Logfiles';
    xLogStoreDays := xIniAccess.ReadInteger('Logfiles', 'StoreDays');
    xLogFileArchives := xIniAccess.ReadString('Logfiles', 'Archives');
    xLogMaxLinesPerFile := xIniAccess.ReadInteger('Logfiles', 'MaxLinesPerFile');

    TAppInstanceLogging.Instance().InitializeLogManager(xLogFilePath, xLogFileArchives, xLogStoreDays,
        xLogMaxLinesPerFile);
end;

function TAppInstanceIniAdaptor.CreateAnyArea(aAreaName: string): TIniAccess;
begin
    result := fSettings.CreateIndividualArea(aAreaName, fSettings.AccessType);
end;

function TAppInstanceIniAdaptor.CreateIndividualArea(aAreaName: string; aAccessType: TIniAccessType)
    : TIniAccess;
begin
    result := fSettings.CreateIndividualArea(aAreaName, aAccessType);
end;


end.
