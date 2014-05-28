{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.06.09 pk                                        TN4585.2    Initial Revision
  06.07.09 pk                                        TN4585.4    Various changes
  13.07.09 pk                                        TN4585.4    Uses Changed
  30.07.09 pk                                        TN4585.5    Various changes
  27.08.09 pk  TAppInstanceCoreServerLib.Create      TN4753      Does not create gSysLiqManager
  31.08.09 pk                                        TN4753      create instance of AppInstancePeripheryManagementExt
  01.10.09 pk  TAppInstanceCoreServerLib.Create      TN4753      ThreadManager.Create parameters changed
  12.10.09 pk  TAppInstanceCoreServerLib.Create      TN4753      calls TAppInstanceRunExecuting.CreateInstance
  26.10.09 wl  ReadServerSettings                    TN4831      IConfigurationSet replaces TIniFile
  09.03.10 pk                                        TN5015      ThreadManager changed to ThreadManagerSetup
  17.06.10 pk                                        TN5152.1    BDEDataProviderFactory replaced by LocalDataProviderFactory
  19.10.10 pk                                        TN5305      changes needed for CoreClient/Server
  11.09.11 wl  fAppInstanceRunExecuting              TN5672   Instanz wird hier verwaltet
  14.12.11 wl                                        TN5765   uses geändert
  15.12.11 wl                                        TN5767   uses geändert
  25.06.12 wl                                        TN5899   uses geändert
  08.08.12 wl                                        TN5946   uses geändert
  30.07.13 wl                                        TN6160   an TSystemEvents angepasst
  21.08.13 wl                                        TN6231   uses geändert
  26.08.13 wl                                        TN6236   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceCoreServerLib;


interface


uses
    RunAppInterface,
    CommonTypes,
    AppInstanceRunExecuting,
    ThreadClasses,
    GUIManager,
    ThrdMan;

type
    TCoreServerSettingsRec = record
        ServerPort: integer;
    end;

    TCoreServerAppInstanceRunExecutingHelper = class(TAppInstanceRunExecutingHelper)
    protected
        function CreateThreadManager: TThreadManagerSetup; override;
        function CreateGUIManager: TGUIManager; override;
    end;

    TAppInstanceCoreServerLib = class
    private
        fSettings: TCoreServerSettingsRec;
        fAppInstanceRunExecuting: TAppInstanceRunExecuting;
        constructor Create(aPurpose: TAppPurpose; const aAppHInstance: LongWord);
        class function ReadServerSettings: TCoreServerSettingsRec; static;
    public
        destructor Destroy(); override;
        class function CreateInstance(aPurpose: TAppPurpose; const aAppHInstance: LongWord)
            : TAppInstanceCoreServerLib;
        class procedure DestroyInstance();
        class function Instance(): TAppInstanceCoreServerLib;
    end;


implementation


uses
    SysUtils,
    ServerCoreServerCalls,
    CoreServerTCPIP,
    CoreClientInfo,
    CoreClientInfoManager,
    CoreServerEventManager,
    LogManager,
    ErrorManager,
    ErrorManagerRun,
    ConfigurationFile,
    ThreadManagerCoreServer,
    ErrorMessageFactory,
    ErrorMessageFactoryExt,
    SamStart,
    GUIManagerCoreServer,
    ResourceManager,
    ResourceManagerRun,
    ObjModul,
    ObjModulA,
    LiquidManager,
    Liquids,
    LayoutElementGraphicsDriverTypeManager,
    AppInstanceMethodBuilding,
    AppInstanceRunActionLib,
    CoreServerSystemEvents,
    RunLayoutManager,
    EventManager,
    RunEventManager,
    TraceManager,
    RunTraceManager,
    ReferenceIdentValueMemory,
    AppInstanceDisplayComponent,
    DisplayComponentEventManager,
    LayoutManager,
    RunGlobals,
    DataProvider,
    AppInstanceRunAppDataLib,
    AppInstancePeripheryManagement,
    AppInstancePeripheryManagementExt;

var
    uInstCoreServerLib: TAppInstanceCoreServerLib;

const
    cCoreServerSettingsSectionsApp = 'App';
    cCoreServerSettingsIdentServerAddress = 'ServerAddress';
    cCoreServerSettingsIdentServerPort = 'ServerPort';

    { TCoreServerAppInstanceRunExecutingHelper }
function TCoreServerAppInstanceRunExecutingHelper.CreateThreadManager: TThreadManagerSetup;
begin
    result := TThreadManagerCoreServer.Create();
end;

function TCoreServerAppInstanceRunExecutingHelper.CreateGUIManager: TGUIManager;
begin
    result := TGUIManagerCoreServer.Create();
end;

class function TAppInstanceCoreServerLib.ReadServerSettings(): TCoreServerSettingsRec;
var
    xIniFile: IConfigurationSet;
begin
    xIniFile := TConfigurationFile.Create('.\Startup.ini');
    xIniFile.Open(true);
    try
        result.ServerPort := xIniFile.ReadInteger(cCoreServerSettingsSectionsApp,
            cCoreServerSettingsIdentServerPort, 0);
    finally
        xIniFile.Close;
    end;
end;

{ TAppInstanceCoreServerLib }

constructor TAppInstanceCoreServerLib.Create(aPurpose: TAppPurpose; const aAppHInstance: LongWord);
begin
    inherited Create();
    fSettings := ReadServerSettings();
    TAppInstanceRunAppDataLib.CreateInstance(aPurpose, aAppHInstance);

    TCoreClientInfoManager.CreateInstance();
    TCoreServerEventManager.CreateInstance();
    TTCPIPCoreServer.CreateInstance(fSettings.ServerPort);
    TServerCoreServerCalls.CreateInstance();

    fAppInstanceRunExecuting := TAppInstanceRunExecuting.Create
        (TCoreServerAppInstanceRunExecutingHelper.Create());
    // ( gRunMain as TZARunnerRunMain ).InitSimModeGUI( gRunFlow.SimulationMode );
    TDisplayComponentEventManager.Instance.OnSimpleEvent :=
        TEventManager.Instance.DisplayComponentSimpleEvent;

end;

destructor TAppInstanceCoreServerLib.Destroy;
begin
    FreeAndNil(fAppInstanceRunExecuting);
    TServerCoreServerCalls.DestroyInstace();
    TTCPIPCoreServer.DestroyInstance();
    TCoreServerEventManager.DestroyInstace();
    TCoreClientInfoManager.DestroyInstance();

    TAppInstanceRunAppDataLib.DestroyInstance();
    inherited;
end;

class function TAppInstanceCoreServerLib.CreateInstance(aPurpose: TAppPurpose; const aAppHInstance: LongWord)
    : TAppInstanceCoreServerLib;
begin
    // create instance if instance does not exist
    if not Assigned(uInstCoreServerLib) then
        uInstCoreServerLib := TAppInstanceCoreServerLib.Create(aPurpose, aAppHInstance);

    // return instance
    result := uInstCoreServerLib;
end;

class function TAppInstanceCoreServerLib.Instance(): TAppInstanceCoreServerLib;
begin
    result := uInstCoreServerLib;
end;

class procedure TAppInstanceCoreServerLib.DestroyInstance;
begin
    uInstCoreServerLib.Free;
end;


end.
