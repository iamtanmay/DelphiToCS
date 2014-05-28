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
  17.04.09 pk  Create                       TN4532   now with aAppHInstance parameter
  17.04.09 pk  Create                       TN4532   Loads some standard cursors
  07.05.10 wl  DetermineSchedulerSettings   TN5052   entfernt
  17.05.10 wl                               TN5111   LocalIniPath,LogfilePath: Bei Win7 ProgramData statt Program Files
  20.05.10 wl                               TN5116   AppInstanceAppCommon umbenannt
  21.07.10 pk                               TN5203   AppInstanceAppDataAdaptor renamed to AppInstanceAppData
  19.10.10 pk                               TN5305   changes needed for CoreClient/Server
  08.03.11 wl  ReadStartParameters          TN5495   NoStart bei anderem Alias entfernt (kommt noch aus DDE-Zeiten)
  26.07.11 wl  ReadStartParameters          TN5644   Designer startet jetzt simuliert, außer man startet mit "ACTIVE"
  11.09.11 wl  TAppDataInstancesStarter     TN5672   Instance entfernt
  -------------------------------------------------------------------------------------------------- }

unit AppDataInstancesStarter;


interface


uses
    CommonTypes;

type

    TAppDataInstancesStarterHelper = class
    public
        procedure DataProviderFactoryCreateInstance(); virtual;
        procedure DataProviderFactoryDestroyInstance(); virtual;
    end;

    TAppDataInstancesStarter = class
    private
        fHelper: TAppDataInstancesStarterHelper;
        procedure LoadStandardScreenCursors(const aAppHInstance: LongWord);
        function ReadStartParameters(aPurpose: TAppPurpose): TStartParameters;
    public
        constructor Create(aPurpose: TAppPurpose; aConnectionType: TMachineConnectionType;
            const aAppHInstance: LongWord; const aHelper: TAppDataInstancesStarterHelper);
        destructor Destroy(); override;
    end;


implementation


uses
    Windows,
    Forms,
    SysUtils,
    AppTypes,
    AppSettings,
    AppInstanceUserCommon,
    DialogUtils,
    AppInstanceIniAdaptor,
    FileUtilities,
    AppInstanceAppData,
    PluginLoader,
    DataAdaptor,
    AppInstanceStartupLib,
    DataProviderFactory,
    AppInstanceLogging;

{ TAppDataInstancesStarter }

constructor TAppDataInstancesStarter.Create(aPurpose: TAppPurpose; aConnectionType: TMachineConnectionType;
    const aAppHInstance: LongWord; const aHelper: TAppDataInstancesStarterHelper);
var
    xStartParameters: TStartParameters;
begin
    inherited Create();
    fHelper := aHelper;

    fHelper.DataProviderFactoryCreateInstance();

    // Start-Parameter lesen
    xStartParameters := ReadStartParameters(aPurpose);

    TPluginLoader.CreateInstance();
    TPluginLoader.Instance.LoadRegularPlugins;

    TAppInstanceAppData.CreateInstance(aPurpose, aConnectionType);

    TDataProviderFactory.Instance.Initialize(xStartParameters.SpecialAlias);

    // Init TAppInstanceIniAdaptor
    TAppInstanceIniAdaptor.CreateInstance();

    // Standardverzeichnis: C:\Programme\Zinsser\Sampler (WinXP) oder C:\ProgrmData\Zinsser\Sampler (Win 7)
    ChDir(TFileUtilities.GetApplicationDataPath());

    // Init TAppInstanceUserCommon
    TAppInstanceUserCommon.CreateInstance(xStartParameters);

    // define application name
    Application.Title := TAppInstanceUserCommon.Instance.GetAppName(aPurpose);

    // SystemDataPassword zurücksetzen
    TDataProviderFactory.Instance.ResetPassword;

    // Init TDialogUtils
    if aAppHInstance > 0 then
        LoadStandardScreenCursors(aAppHInstance);
end;

destructor TAppDataInstancesStarter.Destroy();
begin
    TAppInstanceUserCommon.DestroyInstance;
    TAppInstanceIniAdaptor.DestroyInstance;
    TAppInstanceAppData.DestroyInstance;
    TPluginLoader.DestroyInstance();
    fHelper.DataProviderFactoryDestroyInstance();
    FreeAndNil(fHelper);
    inherited;
end;

procedure TAppDataInstancesStarter.LoadStandardScreenCursors(const aAppHInstance: LongWord);
begin
    TDialogUtils.SetAppHInstance(aAppHInstance);
    TDialogUtils.LoadScreenCursor(crCustInfo, 'INFO');
    TDialogUtils.LoadScreenCursor(crCustZoom, 'ZOOM');
    TDialogUtils.LoadScreenCursor(crCustOpenHand, 'OPENHAND');
    TDialogUtils.LoadScreenCursor(crCustCloseHand, 'CLOSEHAND');
end;

function TAppDataInstancesStarter.ReadStartParameters(aPurpose: TAppPurpose): TStartParameters;
const
    STR_COMMAND_LINE_PARAM_ALIAS = 'A:';
    STR_COMMAND_LINE_PARAM_SIMULATION = 'SIMULATION';
    STR_COMMAND_LINE_PARAM_DEBUG = 'DEBUG';
    STR_COMMAND_LINE_PARAM_NOSTART = 'NOSTART';
    STR_COMMAND_LINE_PARAM_ACTIVE = 'ACTIVE';
var
    i: integer;
    xCommandLineParam: string;
    xIsSimulation: boolean;
begin
    // check start parameters for simulation, debug mode
    if (aPurpose = TAppPurpose.appEditAndStart) then
        xIsSimulation := true // Normalfall: Designer im Simulationsmodus
    else
        xIsSimulation := false;

    result.IsDebug := false;
    result.IsNoStart := false;
    result.SpecialAlias := '';

    for i := 1 to System.ParamCount do
    begin
        xCommandLineParam := UpperCase(System.ParamStr(i));
        if (xCommandLineParam = STR_COMMAND_LINE_PARAM_SIMULATION) then
        begin
            xIsSimulation := true
        end
        else if (xCommandLineParam = STR_COMMAND_LINE_PARAM_ACTIVE) then
        begin
            xIsSimulation := false // ACTIVE: Kein Simulationsmodus für Designer
        end
        else if (xCommandLineParam = STR_COMMAND_LINE_PARAM_DEBUG) then
        begin
            result.IsDebug := true;
        end
        else if (xCommandLineParam = STR_COMMAND_LINE_PARAM_NOSTART) then
        begin
            result.IsNoStart := true;
        end
        else if (Pos(STR_COMMAND_LINE_PARAM_ALIAS, xCommandLineParam) > 0) then
        begin
            result.SpecialAlias := Copy(xCommandLineParam, 3, Length(xCommandLineParam));
        end;
    end;

    // bei geänderten Alias: nur NoStart-Modus
    // if ( result.SpecialAlias <> '' ) then begin
    // result.IsNoStart := true;
    // end;

    result.AppMode := appModeSim;
    if (not xIsSimulation) and (aPurpose <> appSimple) then
    begin
        result.AppMode := appModeReal;
    end;

end;

{ TAppDataInstancesStarterHelper }

procedure TAppDataInstancesStarterHelper.DataProviderFactoryCreateInstance;
begin

end;

procedure TAppDataInstancesStarterHelper.DataProviderFactoryDestroyInstance;
begin

end;


end.
