{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------- --------  ----------------------------------------------------------
  27.05.10 wl                                        TN5116    soll das exakte Gegenstück zu AppInstanceRunnerLib werden
  28.05.10 wl                                        TN5116     überarbeitet
  04.06.10 wl                                        TN5116    uses geändert
  19.10.10 pk                                        TN5305    changes needed for CoreClient/Server
  11.09.11 wl  fAppInstanceDisplayComponent          TN5672   Instanz wird hier verwaltet
  20.04.12 wl  OnStopButtonClick                     TN5946   entfernt
  18.06.12 wl                                        TN5899   TLiquids-Instanz wird erzeugt und zerstört
  05.11.12 wl  Destroy                               TN6006   WriteAllVolumes mit Parameter SimulationMode
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceRunClientLib;


interface


uses
    ThreadClasses,
    RemoteMessageConstants,
    CoreEventMessageInfo,
    GUIManager,
    ThreadUtils,
    LayoutManager,
    AppInstanceDisplayComponent;

type
    TAppInstanceRunClientLib = class(TObject)
    private
        class var uInstance: TAppInstanceRunClientLib;
        class var uRefCount: integer;
    private
        fAppInstanceDisplayComponent: TAppInstanceDisplayComponent;
        constructor Create();
        function UseLiquids(): boolean;
        class property RefCount: integer read uRefCount write uRefCount;
        procedure SetControlEventMask();
        procedure StartEventHost();
        procedure DoOnEventHostEventPending(const aMessageInfo: TCoreEventMessageInfo);
        function VCL(const aArgs: TMessageArg): TMessageResult;
    private const
        cControlEvents: array [0 .. 4] of integer = (cControlEventIDProcessStarted,
            cControlEventIDProcessFinished, cControlEventIDErrorBox, cControlEventIDMessageBox,
            cControlEventIDAskRunStart); // , cControlEventIDProcessPaused);
    public
        destructor Destroy(); override;
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class function Instance(): TAppInstanceRunClientLib;
        procedure RegisterEventHost;
    end;

    TRunClientLayoutManager = class(TLayoutManager)
    public
        class procedure CreateInstance();
    end;


implementation


uses
    Windows,
    Forms,
    Buttons,
    SysUtils,
    Controls,
    LogManager,
    AppSettings,
    RunDialogsManager,
    AppInstanceRunDialogs,
    RunMainControls,
    DisplayComponentEventManager,
    EdExtern,
    EdExternClient,
    PluginLoader,
    CommonTypes,
    SamGlobe,
    Liquids,
    GeneralTypes,
    TCPIPCoreEventHost,
    CoreControllerClientCalls,
    CoreEventHandler,
    ZARunCoreEventHandler,
    AppInstanceCoreClientLib;

{ TAppInstanceRunClientLib }

class procedure TAppInstanceRunClientLib.CreateInstance();
begin
    if not Assigned(uInstance) then
        uInstance := TAppInstanceRunClientLib.Create();

    Inc(uRefCount);
end;

class procedure TAppInstanceRunClientLib.DestroyInstance;
begin
    if uRefCount = 1 then
        FreeAndNil(uInstance);

    Dec(uRefCount);
end;

class function TAppInstanceRunClientLib.Instance(): TAppInstanceRunClientLib;
begin
    result := uInstance;
end;

constructor TAppInstanceRunClientLib.Create();

begin
    inherited Create();

    gLogManager.Log('Startup - 1', false);
    TPluginLoader.CreateInstance();
    fAppInstanceDisplayComponent := TAppInstanceDisplayComponent.Create();

    gLogManager.Log(Application.Title + ': ' + TAppSettings.Version + ', ' + TAppSettings.Build, true);

    TEdExtern.Instance := TEdExternClient.Create;

    gGUIManager := TGUIManager.Create();

    TRunMainControls.CreateInstance();
    TAppInstanceRunDialogs.CreateInstance();

    gLogManager.Log('Startup - 2', false);
    // TRunMainControls.Instance.OnStopButtonClick := DoOnStopButtonClick;

    gLogManager.Log('Startup - 3', false);
    TCoreEventHandler.SetInstance(TZARunCoreEventHandler.Create());
    SetControlEventMask();

    gLogManager.Log('Startup - 4', false);
    TEdExtern.Instance.RegisterControllerForAllRunIDs();

    gLogManager.Log('Startup - 5', false);
    StartEventHost();

    gLogManager.Log('Startup - 6', false);

    // if TAppInstanceCoreClientLib.Instance.CoreClientSettingsRec.IsController then
    // RegisterController();
    if not TAppInstanceCoreClientLib.Instance.CoreClientSettingsRec.IsNoEventHost then
        RegisterEventHost();

    gLogManager.Log('Startup - 7', false);

    // aus AppInstanceRunnerLib übernommen:
    // ------------------------------------
    TRunClientLayoutManager.CreateInstance();
    TLayoutManager.Instance.CreateDefaultSceneGraphics(TRunMainControls.Instance.LayoutPanel);
    TRunMainControls.Instance.SetLayoutSceneGraphics();
    TLiquids.CreateInstance;
    if self.UseLiquids and (not(gHideVolumesWindow)) then
    begin
        // TLiquids.Instance.SetEvents( TGUIManagerRun.Instance.SystemVolume_Changed, TGUIManagerRun.Instance.WasteVolume_Changed );
        TRunMainControls.Instance.InitVolumesInfo();
    end;
end;

destructor TAppInstanceRunClientLib.Destroy;
begin
    // etwas spät, das sollte beim Entladen der Devices erfolgen
    TLiquids.Instance.WriteAllVolumes(false);

    TLiquids.DestroyInstance;
    TCoreEventHandler.DestroyInstace();
    TRunDialogsManager.Instance.UnloadAnyLoadedDisplayComponents();
    FreeAndNil(fAppInstanceDisplayComponent);

    TEdExtern.InstanceDestroy();
    TAppInstanceRunDialogs.DestroyInstance();
    TRunMainControls.DestroyInstance();
    TPluginLoader.DestroyInstance();

    inherited;
end;

procedure TAppInstanceRunClientLib.SetControlEventMask();
var
    xEventMessageIDs: TIntArray;
    x: integer;
begin
    SetLength(xEventMessageIDs, Length(cControlEvents));
    for x := 0 to high(cControlEvents) do
    begin
        xEventMessageIDs[x] := cControlEvents[x];
    end;
    TCoreControllerClientCalls.Instance.SetControlEventMask(xEventMessageIDs);
end;

function TAppInstanceRunClientLib.VCL(const aArgs: TMessageArg): TMessageResult;
var
    xObject: TObject;
    xMessageInfo: TCoreEventMessageInfo;

begin

    xObject := TObject(LongInt(aArgs));
    ASSERT(xObject is TCoreEventMessageInfo);
    xMessageInfo := xObject as TCoreEventMessageInfo;
    TCoreEventHandler.Instance.HandleMessage(xMessageInfo);
end;

procedure TAppInstanceRunClientLib.DoOnEventHostEventPending(const aMessageInfo: TCoreEventMessageInfo);
begin
    gmMessageAndGo(VCL, LongInt(aMessageInfo));
end;

procedure TAppInstanceRunClientLib.StartEventHost();
begin
    TTCPIPCoreEventHost.CreateInstance(TAppInstanceCoreClientLib.Instance.CoreClientSettingsRec.
        EventHostPort);
    TTCPIPCoreEventHost.Instance.OnEventHostEventPending := DoOnEventHostEventPending;
end;

procedure TAppInstanceRunClientLib.RegisterEventHost();
const
    cMaxWaitTimes = 5;
    cSleepTime = 2000;
var
    xSuccess: boolean;
    x: integer;
begin

    gLogManager.Log('Registering EventHost...', false);
    try
        TCoreClient.Instance.RegisterEventHost
            (TAppInstanceCoreClientLib.Instance.CoreClientSettingsRec.EventHostPort);
        for x := 1 to cMaxWaitTimes do
        begin
            xSuccess := TCoreClient.Instance.IsEventHostRegistered();
            if xSuccess then
                BREAK;
            Sleep(cSleepTime);
        end;
    except
        on E: exception do
        begin
            gLogManager.Log('EventHost could not be registered - ' + E.Message, false);
            xSuccess := false;
        end;
    end;

    if xSuccess then
    begin
        gLogManager.Log('EventHost registered', false);
    end
    else
    begin
        gLogManager.Log('EventHost could not be registered', false);
        raise Exception.Create('Event Host could not be registered');
    end;
end;

function TAppInstanceRunClientLib.UseLiquids(): boolean;
var
    xIniAccess: IWinlissyIniAccess;
begin
    xIniAccess := TAppSettings.CreateAppIni;
    result := xIniAccess.ReadBool('Display', 'UseLiquids');
end;

{ TRunClientLayoutManager }

class procedure TRunClientLayoutManager.CreateInstance();
begin
    TLayoutManager.SetInstance(TRunClientLayoutManager.Create(nil));
end;


initialization


TAppInstanceRunClientLib.RefCount := 0;


finalization


end.
