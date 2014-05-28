{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  28.09.09 pk                                        TN4753    InitialRevision
  12.11.09 pk  TAppInstanceRunExecutingHelper.Start  TN4843    do not create resourcemanager already done in ThreadManExt
  09.03.10 pk                                        TN5015    New TDisplayComponentEventHandlerManager
  27.07.10 ts  TAppInstanceRunExecutingHelper.Start  TN5022    TDllLoading.Instance( TRunAppInterface.Create()) -- RUN
  27.10.10 wl  TAppInstanceRunExecutingHelper.Start  TN5300    TErrorMessageFactoryExt.Create( true ); - mit MailHost
  15.11.10 pk                                        TN5340    Changes to prevent memory leak
  11.09.11 wl                                        TN5672   Instance entfernt
  14.09.11 wl                                        TN5672   uses ThreadAPICallbackImplementor
  20.10.11 wl  TAppInstanceRunExecutingHelper.Start  TN5723   statt InitProject wird InitExternalDLLs; gmConnectExtDatabases; aufgerufen
  14.12.11 wl                                        TN5765   ohne TRunMain
  15.12.11 wl  TAppInstanceRunExecutingHelper.Start  TN5767   erzeugt Instanz für DllLoadingV8 und DllLoadingV6
  27.12.11 wl                                        TN5768   an geändertes TRunGlobals angepasst
  25.06.12 wl                                        TN5899   uses geändert
  08.08.12 wl                                        TN5946   uses geändert
  17.01.13 wl                                        TN6068   uses geändert
  30.07.13 wl                                        TN6160   an TSystemEvents angepasst
  15.08.13 wl                                        TN6223   uses geändert
  21.08.13 wl                                        TN6231   uses geändert
  30.08.13 wl                                        TN6236   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceRunExecuting;


interface


uses
    DLLCall,
    GUIManager,
    AppInstanceDisplayComponent,
    ThrdMan;

type
    TAppInstanceRunExecutingHelper = class
    private
        fAppInterface: TAppInterface;
        fAppInstanceDisplayComponent: TAppInstanceDisplayComponent;
    protected
        function CreateThreadManager: TThreadManagerSetup; virtual;
        function CreateGUIManager: TGUIManager; virtual; abstract;
    public
        procedure Start();
        procedure Finish();
    end;

    TAppInstanceRunExecuting = class(TObject)
    private
        fHelper: TAppInstanceRunExecutingHelper;
    public
        constructor Create(const aHelper: TAppInstanceRunExecutingHelper);
        destructor Destroy(); override;
    end;


implementation


uses
    Windows,
    SysUtils,
    LogManager,
    ErrorManager,
    ErrorManagerRun,
    ErrorMessageFactory,
    ErrorMessageFactoryExt,
    SamStart,
    DevicesConflictManager,
    ResourceManager,
    DeviceInitHandling,
    ResourceManagerRun,
    ObjModul,
    ObjModulA,
    LiquidManager,
    Liquids,
    LayoutElementGraphicsDriverTypeManager,
    AppInstanceMethodBuilding,
    AppInstanceRunActionLib,
    RunLayoutManager,
    EventManager,
    RunEventManager,
    ThreadAPICallbackImplementor,
    DllLoadingV8,
    DllLoadingV6,
    TraceManager,
    RunTraceManager,
    ReferenceIdentValueMemory,
    DisplayComponentEventManager,
    LayoutManager,
    RunGlobals,
    DataProvider,
    AppInstanceRunCommon,
    AppInstancePeripheryManagement,
    AppInstancePeripheryManagementExt,
    ThrdManExt,
    RunAppInterface,
    ExternDllFunc,
    ThreadAPI,
    SystemEvents,
    SystemEventsRunner,
    DisplayComponentEventHandler;

{ TAppInstanceRunExecutingHelper }

function TAppInstanceRunExecutingHelper.CreateThreadManager: TThreadManagerSetup;
begin
    result := TThreadManagerRun.Create()
end;

procedure TAppInstanceRunExecutingHelper.Start();
var
    xAppInstance: TAppInterface;
begin
    TAppInstancePeripheryManagement.CreateInstance();
    TAppInstancePeripheryManagementExt.CreateInstance();

    gErrorManager := TErrorManagerRun.Create();
    TThreadAPI.Initialize(TThreadAPICallbackImplementor.ShowThreadException,
        TThreadAPICallbackImplementor.Log);
    TThreadManagerSetup.SetInstance(CreateThreadManager());
    TThreadManagerSetup.Instance.LaunchServiceProcess();

    gErrorMessageFactory := TErrorMessageFactoryExt.Create(true);
    fAppInterface := TRunAppInterface.Create();

    InitSamGlobals2;
    xAppInstance := TRunAppInterface.Create();
    TDllLoadingV8.Instance(xAppInstance);
    TDllLoadingV6.Instance(xAppInstance);

    gGUIManager := CreateGUIManager();
    fAppInstanceDisplayComponent := TAppInstanceDisplayComponent.Create();
    TDisplayComponentEventHandlerManager.CreateInstance();

    gModules := TModuleFinder.Create();
    TDeviceInitHandling.ResetInitID();
    TDevicesConflictManager.CreateInstance;
    TActionModules.CreateEvents();

    InitExternalDLLs;
    gmConnectExtDatabases;

    TSystemEvents.CreateInstance(TSystemEventsRunner.Create());

    TLayoutElementGraphicsDriverTypeManager.CreateInstance;

    TLayoutManager.SetInstance(TRunLayoutManager.Create(nil));

    TAppInstanceMethodBuilding.CreateInstance();
    TAppInstanceRunActionLib.CreateInstance();
    TEventManager.SetInstance(TRunEventManager.Create());

    TReferenceIdentValueMemoryManager.CreateInstance();

    TRunTraceManager.CreateInstance();

    TRunGlobals.CreateInstance;
end;

procedure TAppInstanceRunExecutingHelper.Finish();
begin
    TRunGlobals.DestroyInstance;

    TRunTraceManager.DestroyInstance();
    TReferenceIdentValueMemoryManager.DestroyInstance();
    TEventManager.DestroyInstance();

    TAppInstanceRunActionLib.DestroyInstance();
    TAppInstanceMethodBuilding.DestroyInstance();
    TLayoutManager.DestroyInstance();

    TSystemEvents.DestroyInstance();

    TActionModules.DisconnectModules();

    TDevicesConflictManager.DestroyInstance;

    gModules.Free;

    TDisplayComponentEventHandlerManager.DestroyInstance();
    FreeAndNil(fAppInstanceDisplayComponent);

    gGUIManager.Free;

    SaveSamGlobals();
    fAppInterface.Free;
    TThreadManagerSetup.DestroyInstance;
    TThreadAPI.Finalize;
    gErrorMessageFactory.Free;
    gErrorManager.Free;

    TAppInstancePeripheryManagementExt.DestroyInstance();
    TAppInstancePeripheryManagement.DestroyInstance();
end;

{ TAppInstanceRunExecuting }

constructor TAppInstanceRunExecuting.Create(const aHelper: TAppInstanceRunExecutingHelper);
begin
    inherited Create();
    fHelper := aHelper;
    fHelper.Start();
end;

destructor TAppInstanceRunExecuting.Destroy;
begin
    fHelper.Finish();
    fHelper.Free;
    inherited;
end;


end.
