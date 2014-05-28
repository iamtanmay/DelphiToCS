unit ActionHandlerLow;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : The Most Basic Action Hanlders required by all projects.
  For now: If the Layouter doesn't need a certain function don't put it in this unit,
  instead put it in the ActionHandler unit.
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  17.03.05 pk                               TN2353   New
  31.03.05 pk  TExecHanlder                 TN2362   BeforeInitialization, AfterFinalization
  19.04.05 pk  TBasicExecHandler            TN2392   name changed from TBasicActionHandler
  19.04.05 pk  TBasicExecHandler            TN2392   InterruptStart, InterruptFinish, ErrorSet
  29.04.05 pk  TBasicExecHandler.Finalize   TN2405   do not call Init when globalerr is set
  19.08.05 pk  TActionHandler.Execute       TN2568   set fCompleted to true if no exceptions
  22.12.05 pk  TBasicExecHandler.Initialize TN2875   Do not call init function if CancelInit is true
  10.04.06 pk  TBasicExecHandler.DoInitAction TN3031 New Param : FullInit
  01.12.06 pk  InterruptStart/Finish        TN3441 Thread suspend removed. instead of suspending let thread get stopped in lock
  07.12.06 pk                                   TN3455 uses threadclasses instead of objstructures
  07.12.06 pk  gmCreateBasicActionHandlerThread TN3455 CreateThread Freeonterminate=true
  14.04.08 wl                                     TN4060   uses DialogUtils
  03.07.08 wl                                    TN4157
  11.07.08 wl                                    TN4164   TActionModules wird ohne "gModules as" aufgerufen
  02.09.08 pk                                    TN4215   various changes
  06.11.08 pk  TBasicExecHandler                 TN4280   now inherits from TProcessHandler
  17.11.08 wl  FMainStopBtn                      TN4312   TBitBtn statt TButton
  24.11.08 pk  gmCreateBasicActionHandlerThread  TN4280   return SysHandleID
  27.11.08 pk  gmCreateBasicActionHandlerThread  TN4280   call Unpause
  02.12.08 pk  gmCreateBasicActionHandlerThread  TN4280   Create Process instead of just a thread
  17.12.08 pk  gmCreateBasicActionHandlerThread  TN4372   CreateProcess with new IsSimulated parmater
  25.01.10 pk  ResetError                        TN4963   New: This is needed before BeforeInitialize is called
  20.05.10 wl                                    TN5117   uses ControlUtils
  07.06.10 pk  DoPrepare                         TN5077   New
  11.06.10 pk  gmCreateBasicActionHandlerThread  TN5138   New IsSystemProcess Param
  16.11.10 wl  TBasicExecHandler.DoInitAction    TN5351   geänderter Aufruf von InitBasic
  19.11.10 wl  fCancelInit                       TN5358   entfernt
  15.08.13 wl                                    TN6223   uses geändert
  21.08.13 wl                                    TN6231   uses geändert
  22.08.13 wl  DoInitAction                      TN6233   statt InitBasic wird ConnectAndInit aufgerufen
  ---------------------------------------------------------------------------------------------------------------------- }


interface


uses
    Buttons,
    AppTypes,
    Action,
    Executable,
    ThreadClasses,
    ThreadingProcessHandler;

type
    // Takes care of Init Action, StopButton, Putting Back the Tool, Resetting Hardware
    TBasicExecHandler = class(TProcessHandler)
    protected
        fInitAtBegin, fInitAtEnd: boolean;
        fBtnInterrupt: TBitBtn;
        fPrepareSuccess: boolean;
        procedure EnableControls(aEnable: boolean); virtual;
        procedure BeforeInitialize(); virtual;
        procedure AfterInitialize(); virtual;
        procedure BeforeFinalize(); virtual;
        procedure AfterFinalize(); virtual;
        procedure DoInitAction(aFullInit: boolean); virtual;
        procedure ShowStopButton(aShow: boolean); virtual;
        function DoPrepare(): boolean; virtual;
        procedure DoUnPrepare(); virtual;
        procedure Initialize(); override;
        procedure Finalize(); override;
    public
        constructor Create(aInitAtBegin, aInitAtEnd: boolean; aBtnInterrupt: TBitBtn = nil);
    end;

    TBasicSingleActionExecHandler = class(TBasicExecHandler)
    protected
        fIntfExecutable: IExecutable;
        procedure DoPerform(); override;

    public
        constructor Create(aIntfExecutable: IExecutable; aInitAtBegin, aInitAtEnd: boolean;
            aBtnInterrupt: TBitBtn = nil);
    end;

function gmCreateBasicActionHandlerThread(aAction: TAction; aRunModes: TSamplerThreadModes): TSysHandleID;


implementation


uses
    ControlUtils,
    ErrorManager,
    ThrdMan,
    DeviceInitHandling,
    GUIManager,
    ToolHandling,
    ThreadAPI;

function gmCreateBasicActionHandlerThread(aAction: TAction; aRunModes: TSamplerThreadModes): TSysHandleID;
var
    xActionHandler: TBasicExecHandler;
begin
    xActionHandler := TBasicSingleActionExecHandler.Create(aAction, false, false, nil);

    result := TThreadAPI.CreateProcess('Single Action: - ' + aAction.ClassName, xActionHandler, nil, '',
        false, false);
end;

{ TBasicExecHandler }

constructor TBasicExecHandler.Create(aInitAtBegin, aInitAtEnd: boolean; aBtnInterrupt: TBitBtn = nil);
begin
    inherited Create();
    fInitAtBegin := aInitAtBegin;
    fInitAtEnd := aInitAtEnd;
    fBtnInterrupt := aBtnInterrupt;
end;

procedure TBasicExecHandler.BeforeInitialize();
begin
end;

procedure TBasicExecHandler.AfterInitialize();
begin
end;

procedure TBasicExecHandler.BeforeFinalize();
begin
end;

procedure TBasicExecHandler.AfterFinalize();
begin
end;

procedure TBasicExecHandler.EnableControls(aEnable: boolean);
begin
    ShowStopButton(not aEnable);
end;

function TBasicExecHandler.DoPrepare(): boolean;
begin
    result := true;
end;

procedure TBasicExecHandler.DoUnPrepare();
begin
end;

procedure TBasicExecHandler.Initialize();
begin
    fPrepareSuccess := DoPrepare();
    if not fPrepareSuccess then
        EXIT;

    EnableControls(false);
    gErrorManager.ResetGlobalErr();
    BeforeInitialize();

    DoInitAction(fInitAtBegin);

    inherited;

    AfterInitialize();

    RegisterPerform;
end;

procedure TBasicExecHandler.Finalize();
begin
    if fPrepareSuccess then
    begin
        BeforeFinalize();

        inherited;

        TDeviceInitHandling.HarwareReset();

        if fInitAtEnd and (not gErrorManager.IsGlobalErr) then
        begin
            DoInitAction(true);
        end;

        AfterFinalize();

        EnableControls(true);
    end;

    DoUnPrepare();
end;

procedure TBasicExecHandler.DoInitAction(aFullInit: boolean);
begin
    TDeviceInitHandling.ConnectAndInit(aFullInit);
end;

procedure TBasicExecHandler.ShowStopButton(aShow: boolean);
var
    xBtnInterrupt: TBitBtn;
begin
    xBtnInterrupt := fBtnInterrupt;
    if fBtnInterrupt = nil then
        xBtnInterrupt := TThreadManagerSetup.Instance.MainStopBtn;

    if xBtnInterrupt = nil then
        EXIT;
    gGUIManager.SetControlProp(xBtnInterrupt, cpVisible, Variant(aShow));
end;

{ TBasicSingleActionExecHandler }

constructor TBasicSingleActionExecHandler.Create(aIntfExecutable: IExecutable;
    aInitAtBegin, aInitAtEnd: boolean; aBtnInterrupt: TBitBtn = nil);
begin
    inherited Create(aInitAtBegin, aInitAtEnd, aBtnInterrupt);
    fIntfExecutable := aIntfExecutable;
end;

procedure TBasicSingleActionExecHandler.DoPerform();
begin
    fIntfExecutable.Execute();
    TThreadAPI.ExitProcess();
end;


end.
