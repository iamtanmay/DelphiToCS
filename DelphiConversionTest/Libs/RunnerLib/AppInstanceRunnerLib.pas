{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------- --------  ----------------------------------------------------------
  11.12.09 pk  DoOnStopButtonClick                   TN4923     generate an Escape-key event so that the escape thread handles it instead of the main thread
  17.12.09 pk                                        TN4920     use RunFlow.AppSimulationMode instead of RunFlow.SimulationMode
  04.09.10 pk                                        TN5042     most functions that were in RunMain now accessible only via GUIManager
  28.05.10 wl                                        TN5116     überarbeitet
  21.07.10 pk                                        TN5203     call TEdExtern.InstanceDestroy later
  11.09.11 wl  fAppInstanceRunExecuting              TN5672   Instanz wird hier verwaltet
  11.09.11 wl                                        TN5672   Instance entfernt
  14.12.11 wl                                        TN5765   ohne TRunMain
  15.12.11 wl                                        TN5767   uses geändert
  20.04.12 wl  OnStopButtonClick                     TN5858   entfernt
  18.03.13 ts  TAppInstanceRunner.Create()           TN6109   UseTCPIP erzeugt TCPIPRunnerServer-Instanz zur Fernsteuerung
  23.04.13 wl  TAppInstanceRunner.Create()           TN6135   UseTCPIP --> AppInstanceRunnerRemote
  24.04.13 wl                                        TN6137   TLiquids.Instance statt gLiquids
  30.07.13 wl                                        TN6160   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceRunnerLib;


interface


uses
    ThreadClasses,
    AppInstanceRunExecuting,
    GUIManager,
    GUIManagerRunner;

type
    TAppInstanceRunnerRunExecutingHelper = class(TAppInstanceRunExecutingHelper)
    protected
        function CreateGUIManager: TGUIManager; override;
    end;

    TAppInstanceRunner = class(TObject)
    private
        fAppInstanceRunExecuting: TAppInstanceRunExecuting;
        function UseLiquids(): boolean;
    public
        constructor Create();
        destructor Destroy(); override;
    end;


implementation


uses
    Windows,
    SysUtils,
    Forms,
    Buttons,
    EdExtern,
    EdExternDirect,
    LogManager,
    AppSettings,
    RunDialogsManager,
    AppInstanceRunDialogs,
    RunMainControls,
    GUIManagerRun,
    RunFlow,
    LayoutManager,
    ThrdMan,
    EventManager,
    DisplayComponentEventManager,
    CommonTypes,
    SamGlobe,
    Liquids;

{ TAppInstanceRunnerRunExecutingHelper }

function TAppInstanceRunnerRunExecutingHelper.CreateGUIManager: TGUIManager;
begin
    result := TGUIManagerRunner.Create();
end;

{ TAppInstanceRunnerLib }

constructor TAppInstanceRunner.Create();

begin
    inherited Create();

    gLogManager.Log(Application.Title + ': ' + TAppSettings.Version + ', ' + TAppSettings.Build, true);
    TEdExtern.Instance := TEdExternDirect.Create;

    fAppInstanceRunExecuting := TAppInstanceRunExecuting.Create
        (TAppInstanceRunnerRunExecutingHelper.Create());
    TRunMainControls.CreateInstance();
    TAppInstanceRunDialogs.CreateInstance();

    // set events
    gLogManager.OnDisplayLogText := TGUIManagerRun.Instance.LogText_Display;
    TRunMainControls.Instance.InitSimModeGUI(gRunFlow.AppSimulationMode);
    TLayoutManager.Instance.CreateDefaultSceneGraphics(TRunMainControls.Instance.LayoutPanel);
    TRunMainControls.Instance.SetLayoutSceneGraphics();
    TDisplayComponentEventManager.Instance.OnSimpleEvent :=
        TEventManager.Instance.DisplayComponentSimpleEvent;

    if self.UseLiquids and (not(gHideVolumesWindow)) then
    begin
        TLiquids.Instance.SetEvents(TGUIManagerRun.Instance.SystemVolume_Changed,
            TGUIManagerRun.Instance.WasteVolume_Changed);
        TRunMainControls.Instance.InitVolumesInfo();
    end;
end;

destructor TAppInstanceRunner.Destroy;
begin
    TAppInstanceRunDialogs.DestroyInstance();

    TRunMainControls.DestroyInstance();
    FreeAndNil(fAppInstanceRunExecuting);
    TEdExtern.InstanceDestroy();

    inherited;
end;

function TAppInstanceRunner.UseLiquids(): boolean;
var
    xIniAccess: IWinlissyIniAccess;
begin
    xIniAccess := TAppSettings.CreateAppIni;
    result := xIniAccess.ReadBool('Display', 'UseLiquids');
end;


end.
