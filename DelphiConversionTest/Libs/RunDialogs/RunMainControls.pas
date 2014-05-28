{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  07.04.09 pk  CreateStandardControls        TN4503  Set align for forms to client
  19.06.09 pk  fOnStopButtonClick            TN4620  New
  12.08.09 pk  SetLayoutSceneGraphics        TN4716  New
  12.08.09 pk  CreateStandardControls        TN4716  now creates an instance of TfrmViewLayout
  28.09.09 pk  OnChangeSysVol                TN4753  removed
  04.09.10 pk  TMainFormControls             TN5042  New: Code from TRunMain
  07.05.10 wl                                TN5052   UseScheduler boolean statt byte
  04.06.10 wl  TRunnerMainStringLoader       TN5116  --> ZARunnerMain
  18.06.10 wl                                TN5116  verwaltet TAction statt Buttons
  21.07.10 wl                                TN5116  Variable xEnabled entfernt
  15.11.10 pk                                TN5340  Changes to prevent memory leak-
  02.12.10 pk  AddDitiMenu                   TN5379  AutoHotKey property set to Manual. In mnuFillDTRClick use StringReplace
  21.03.11 wl                                TN5508  SimulationInfo --> Elemente nach AskRunStart verlegt
  03.08.11 wl                                TN5645   aufgeräumt, damit Enable/Disable der Start-Buttons endlich funktioniert
  17.11.11 wl                                TN5725   PositionInfo-Fenster entfernt
  13.01.12 ts  InitControls                  TN5779   fMnuToolsFillDTR invisible, wenn kein Layout geladen ist
  20.04.12 wl                                TN5858   an stark geändertes RunMain-Fenster angepasst
  09.05.12 wl  StateSetPause,-Abort,-Running TN5858   neu: können jetzt von außen aufgerufen werden
  29.05.12 wl  StateSetPause                 TN5894   neuer Zustand RequestState unterscheidet sich von Pause
  29.05.12 wl  RequestStepByStepMode         TN5894   neu: Für schrittweise Abarbeitung
  02.10.12 wl  ResetFontForWinXP             TN5960   verwendet TCustomForm
  05.11.12 wl  SetSimModeGUI                 TN6006   VolumesInfo wird jetzt auch aktualisiert
  04.12.12 wl  MainMethodSelection               TN5960   Methodenauswahl als neues Hauptfenster-Element
  24.01.14 tp  SetMainFormControls/SetPanelControls  TN6341   Calibrate added
  ----------------------------------------------------------------------------------------------------------------------- }

unit RunMainControls;


interface


uses
    Controls,
    Forms,
    StdCtrls,
    ActnList,
    ExtCtrls,
    Menus,
    Classes,
    MainRun,
    Generics.Collections,
    AppTypes;

type
    TRunMainControls = class
    private
        fIsVolumesInfoNeeded: boolean;
        fAlwaysSimulate: boolean;

        fEnabledByRun: boolean;
        fEnabledByOpenMethod: boolean;
        fEnabledStartAgain: boolean;
        fEnabledBySystemUser: boolean;

        // Hier erzeugte Elemente
        fDelayInfoControl: TControl;
        fRunInfoControl: TControl;
        fLogInfoControl: TControl;
        fLayoutInfoControl: TControl;
        fVolumesInfoControl: TControl;

        // Elemente des Main-Fensters
        fMainRun: TfrmMainRun;
        fLayoutPanel: TWinControl;
        fMainLogDisplay: TMemo;
        fMnuToolsFillDTR: TMenuItem;
        FMenuItemsDestroyList: TObjectList<TMenuItem>;
        fActStartWithSelect: TAction;
        fActStartLastStarted: TAction;
        fActStartOpenMethod: TAction;
        fActFlush: TAction;
        fActInit: TAction;
        fActCalibrate: TAction;
        fMnuFile: TMenuItem;
        fMnuTools: TMenuItem;
        fMnuUser: TMenuItem;
        fMnuMethod: TMenuItem;

        procedure SetActionsEnabled;
        procedure SetEnabledByUser;
        function GetOnStopButtonClick: TNotifyEvent;
        class var uInstance: TRunMainControls;
        class function GetInstance: TRunMainControls; static;
        function GetMainPanel: TWinControl;
        procedure SetForm(aForm: TCustomForm);
        procedure InitOEMLabel();
        procedure mnuFillDTRClick(Sender: TObject);
        procedure SetRunMainState(const Value: TRunMainState);
        function GetRunMainState: TRunMainState;
        function GetUserLogRun: boolean;
        procedure SetUserLogRun(const Value: boolean);
        procedure DoOnStopButtonClick(aSender: TObject);
    public
        constructor Create();
        destructor Destroy(); override;

        class procedure CreateInstance(); static;
        class procedure DestroyInstance(); static;

        procedure InitVolumesInfo();
        procedure SetLayoutSceneGraphics();
        procedure SetMainFormControls(aForm: TCustomForm; const aDefaultMethod: string; aUseLiquids: boolean;
            aActStartWithSelect, aActStartLastStarted, aActStartOpenMethod, aActFlush, aActInit,
            mnuToolsCalibrate: TAction; aMainRun: TfrmMainRun; aMnuFile, aMnuTools, aMnuUser, aMnuMethod,
            aMnuToolsFillDTR: TMenuItem; aMainMethodSelection: TPanel);
        procedure ChangeJobLabel(aLabelText: string);
        procedure AddDiTiMenu();
        procedure ClearDiTiMenu();

        procedure ChangeSimulationCaption(aIsSimMode: boolean);
        procedure InitSimModeGUI(aIsSimMode: boolean);
        procedure SetSimModeGUI(aIsSimMode: boolean);
        procedure ChangeMethodLabel(const aMethodName: string);
        procedure SetLayoutName(const Value: string);
        procedure EnableControlsDuringRun(aEnable: boolean);
        procedure EnableControlsByOpenMethod(aEnable: boolean);
        procedure SetPanelControls(aActStartWithSelect, aActStartLastStarted, aActStartOpenMethod, aActFlush,
            aActInit, aActCalibrate: TAction; aMainMethodSelection: TPanel);
        procedure SetMenus(aMnuFile, aMnuTools, aMnuUser, aMnuMethod, aMnuToolsFillDTR: TMenuItem);
        procedure StateSetAbort;
        procedure StateSetPause(aShowRequest: boolean);
        procedure StateSetRunning;
        function RequestStepByStepMode: boolean;

        property MainPanel: TWinControl read GetMainPanel;
        property DelayInfoControl: TControl read fDelayInfoControl;
        property RunInfoControl: TControl read fRunInfoControl;
        property LogInfoControl: TControl read fLogInfoControl;
        property LayoutInfoControl: TControl read fLayoutInfoControl;
        property VolumesInfoControl: TControl read fVolumesInfoControl;
        property LayoutPanel: TWinControl read fLayoutPanel write fLayoutPanel;
        property MainLogDisplay: TMemo read fMainLogDisplay write fMainLogDisplay;
        property OnStopButtonClick: TNotifyEvent read GetOnStopButtonClick;
        property IsVolumesInfoNeeded: boolean read fIsVolumesInfoNeeded;
        property RunMainState: TRunMainState read GetRunMainState write SetRunMainState;
        property UserLogRun: boolean read GetUserLogRun write SetUserLogRun;

        class property Instance: TRunMainControls read GetInstance;
    end;


implementation


uses
    SysUtils,
    Buttons,
    RunInformation,
    DelayInfo,
    EdExtern,
    AppSettings,
    LogInfo,
    VolumesInfo,
    ViewLayout,
    LayoutManager,
    CommonTypes;

{ TRunMainControls }

constructor TRunMainControls.Create;
begin
    inherited;

    fEnabledByRun := true;
    fEnabledByOpenMethod := false;
    fEnabledStartAgain := false;

    fAlwaysSimulate := false;

    fRunInfoControl := TFrmRunInformation.Create(nil);
    (fRunInfoControl as TForm).BorderStyle := bsNone;
    TFrmRunInformation.SetInstance(fRunInfoControl as TFrmRunInformation);

    fDelayInfoControl := TFraDelayInfo.Create(nil);
    (fDelayInfoControl as TForm).BorderStyle := bsNone;
    TFraDelayInfo.SetInstance(fDelayInfoControl as TFraDelayInfo);

    fLayoutInfoControl := TFrmViewLayout.Create(nil);
    (fLayoutInfoControl as TForm).BorderStyle := bsNone;
    fLayoutInfoControl.Align := alClient;
    fLayoutPanel := (fLayoutInfoControl as TFrmViewLayout).pnlDraw;

    fLogInfoControl := TFrmLogInfo.Create(nil);
    fLogInfoControl.Align := alClient;
    (fLogInfoControl as TForm).BorderStyle := bsNone;
    self.fMainLogDisplay := (fLogInfoControl as TFrmLogInfo).MainLogDisplay;

    fIsVolumesInfoNeeded := false;
    fVolumesInfoControl := TFrmVolumesInfo.Create(nil);
    fVolumesInfoControl.Align := alClient;
    (fVolumesInfoControl as TForm).BorderStyle := bsNone;

    FMenuItemsDestroyList := TObjectList<TMenuItem>.Create(true);
end;

destructor TRunMainControls.Destroy;
begin
    FreeAndNil(FMenuItemsDestroyList);

    FreeAndNil(fVolumesInfoControl);
    FreeAndNil(fLogInfoControl);
    FreeAndNil(fLayoutInfoControl);
    FreeAndNil(fDelayInfoControl);
    FreeAndNil(fRunInfoControl);

    inherited;
end;

class procedure TRunMainControls.CreateInstance;
begin
    uInstance := TRunMainControls.Create();
end;

class procedure TRunMainControls.DestroyInstance;
begin
    uInstance.Free;
end;

procedure TRunMainControls.EnableControlsDuringRun(aEnable: boolean);
begin
    SetEnabledByUser;
    fEnabledByRun := aEnable;
    if aEnable then
        fEnabledStartAgain := true;

    if Assigned(fMnuFile) then
        fMnuFile.Enabled := aEnable;
    if Assigned(fMnuTools) then
        fMnuTools.Enabled := aEnable;
    if Assigned(fMnuUser) then
        fMnuUser.Enabled := aEnable;
    if Assigned(fMnuMethod) and fMnuMethod.Visible then
        fMnuMethod.Enabled := aEnable;

    SetActionsEnabled;

    if (aEnable) then
        SetRunMainState(rmsReady)
    else
        SetRunMainState(rmsRunning);
end;

procedure TRunMainControls.EnableControlsByOpenMethod(aEnable: boolean);
begin
    fEnabledByOpenMethod := aEnable;
    SetActionsEnabled;
end;

procedure TRunMainControls.SetActionsEnabled;
begin
    if Assigned(fActStartOpenMethod) then
        fActStartOpenMethod.Enabled := fEnabledByRun and fEnabledByOpenMethod and fEnabledBySystemUser;
    if Assigned(fActStartLastStarted) then
        fActStartLastStarted.Enabled := fEnabledByRun and fEnabledStartAgain and fEnabledBySystemUser;
    if Assigned(fActStartWithSelect) then
        fActStartWithSelect.Enabled := fEnabledByRun and fEnabledBySystemUser;
    if Assigned(fActFlush) then
        fActFlush.Enabled := fEnabledByRun and fEnabledBySystemUser;
    if Assigned(fActInit) then
        fActInit.Enabled := fEnabledByRun and fEnabledBySystemUser;
end;

procedure TRunMainControls.SetEnabledByUser;
begin
    fEnabledBySystemUser := TAppSettings.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystem);
end;

procedure TRunMainControls.SetMenus(aMnuFile, aMnuTools, aMnuUser, aMnuMethod, aMnuToolsFillDTR: TMenuItem);
begin
    fMnuFile := aMnuFile;
    fMnuTools := aMnuTools;
    fMnuUser := aMnuUser;
    fMnuMethod := aMnuMethod;
end;

procedure TRunMainControls.SetPanelControls(aActStartWithSelect, aActStartLastStarted, aActStartOpenMethod,
    aActFlush, aActInit, aActCalibrate: TAction; aMainMethodSelection: TPanel);
begin
    fActStartWithSelect := aActStartWithSelect;
    fActStartLastStarted := aActStartLastStarted;
    fActStartOpenMethod := aActStartOpenMethod;
    fActFlush := aActFlush;
    fActInit := aActInit;
    fActCalibrate := aActCalibrate;

    fMainRun.MainMethodSelection := aMainMethodSelection;

    SetEnabledByUser;
    SetActionsEnabled;
end;

procedure TRunMainControls.SetRunMainState(const Value: TRunMainState);
begin
    fMainRun.RunMainState := Value;
end;

procedure TRunMainControls.InitVolumesInfo();
begin
    fIsVolumesInfoNeeded := true;
    (fVolumesInfoControl as TFrmVolumesInfo).InitVolumesInfo();
end;

class function TRunMainControls.GetInstance: TRunMainControls;
begin
    EXIT(uInstance);
end;

function TRunMainControls.GetMainPanel: TWinControl;
begin
    if Assigned(fMainRun) then
        EXIT(fMainRun.pnlMain)
    else
        EXIT(nil);
end;

function TRunMainControls.GetOnStopButtonClick: TNotifyEvent;
begin
    result := DoOnStopButtonClick;
end;

procedure TRunMainControls.DoOnStopButtonClick(aSender: TObject);
begin
    TEdExtern.Instance.RequestAndStateSetAbort;
end;

function TRunMainControls.GetRunMainState: TRunMainState;
begin
    EXIT(fMainRun.RunMainState);
end;

function TRunMainControls.RequestStepByStepMode: boolean;
begin
    EXIT(fMainRun.RequestStepByStepMode);
end;

function TRunMainControls.GetUserLogRun: boolean;
begin
    EXIT(fMainRun.UserLogRun);
end;

procedure TRunMainControls.SetLayoutSceneGraphics;
begin
    (fLayoutInfoControl as TFrmViewLayout).SceneGraphics := TLayoutManager.Instance.DefaultSceneGraphics;
end;

procedure TRunMainControls.SetForm(aForm: TCustomForm);
begin
    // was soll denn das?
    aForm.Top := 0;
    aForm.Left := 0;
    aForm.Caption := Application.Title;
end;

procedure TRunMainControls.InitOEMLabel();
var
    xOEMTitle: string;
begin
    // Oem Label
    xOEMTitle := TAppSettings.OEM.JobTitle;
    if xOEMTitle <> '' then
    begin
        if xOEMTitle = 'null' then
            xOEMTitle := '';
        fMainRun.lblJob.Caption := xOEMTitle;
    end;
end;

procedure TRunMainControls.SetMainFormControls(aForm: TCustomForm; const aDefaultMethod: string;
    aUseLiquids: boolean; aActStartWithSelect, aActStartLastStarted, aActStartOpenMethod, aActFlush, aActInit,
    mnuToolsCalibrate: TAction; aMainRun: TfrmMainRun; aMnuFile, aMnuTools, aMnuUser, aMnuMethod,
    aMnuToolsFillDTR: TMenuItem; aMainMethodSelection: TPanel);
begin
    SetForm(aForm);

    fMainRun := aMainRun;

    SetPanelControls(aActStartWithSelect, aActStartLastStarted, aActStartOpenMethod, aActFlush, aActInit,
        mnuToolsCalibrate, aMainMethodSelection);
    SetMenus(aMnuFile, aMnuTools, aMnuUser, aMnuMethod, aMnuToolsFillDTR);

    fMnuToolsFillDTR := aMnuToolsFillDTR;

    InitOEMLabel();

    if Assigned(aMnuToolsFillDTR) then
        aMnuToolsFillDTR.Visible := false;

    if (aDefaultMethod <> '') then
        aActStartLastStarted.Visible := false;

    SetRunMainState(rmsReady);
end;

procedure TRunMainControls.ChangeJobLabel(aLabelText: string);
begin
    fMainRun.LblJob.Caption := aLabelText;
end;

procedure TRunMainControls.ChangeSimulationCaption(aIsSimMode: boolean);
begin
    if Assigned(fMainRun) then
        self.fMainRun.edSimulationCaption.Visible := aIsSimMode;
end;

procedure TRunMainControls.ChangeMethodLabel(const aMethodName: string);
begin
    if Assigned(fMainRun) then
        fMainRun.edMethod.Text := aMethodName;
end;

procedure TRunMainControls.SetLayoutName(const Value: string);
begin
    // fMainRun.edLayoutName.Text := Value;
end;

procedure TRunMainControls.SetSimModeGUI(aIsSimMode: boolean);
var
    xIsSimMode: boolean;
begin
    xIsSimMode := fAlwaysSimulate or aIsSimMode;
    ChangeSimulationCaption(xIsSimMode);
    (TRunMainControls.Instance.VolumesInfoControl as TFrmVolumesInfo).ChangeSimulationMode(xIsSimMode);
end;

procedure TRunMainControls.SetUserLogRun(const Value: boolean);
begin
    fMainRun.UserLogRun := Value;
end;

procedure TRunMainControls.InitSimModeGUI(aIsSimMode: boolean);
begin
    fAlwaysSimulate := aIsSimMode;
    SetSimModeGUI(aIsSimMode);
end;

procedure TRunMainControls.ClearDiTiMenu();
begin
    FMenuItemsDestroyList.Clear();
end;

procedure TRunMainControls.mnuFillDTRClick(Sender: TObject);
var
    xDitiType: string;
begin
    if (Sender is TMenuItem) then
    begin
        xDitiType := (Sender as TMenuItem).Caption;
        if (xDitiType = fMnuToolsFillDTR.Caption) then
            xDitiType := '';

        // remove HotKey character
        if Pos('&', xDitiType) > 0 then
            xDitiType := StringReplace(xDitiType, '&', '', [rfReplaceAll]);

        TLayoutManager.Instance.CurrentLayout.FillDitiRacks(xDitiType);
    end;
end;

procedure TRunMainControls.StateSetAbort;
begin
    fMainRun.StateSetAbort;
end;

procedure TRunMainControls.StateSetPause(aShowRequest: boolean);
begin
    fMainRun.StateSetPause(aShowRequest);
end;

procedure TRunMainControls.StateSetRunning;
begin
    fMainRun.StateSetRunning;
end;

procedure TRunMainControls.AddDiTiMenu();
var
    x: integer;
    xMenuItem: TMenuItem;
    xListCount: integer;
begin
    if not Assigned(fMnuToolsFillDTR) then
        EXIT;

    fMnuToolsFillDTR.Visible := false;
    // Turn Off AutoHotKeys, otherwise there could be a problem in mnuFillDTRClick when several DitiTypes start with same character!!!
    fMnuToolsFillDTR.AutoHotkeys := maManual;
    if not Assigned(TLayoutManager.Instance.CurrentLayout) then
        EXIT;
    xListCount := TLayoutManager.Instance.CurrentLayout.DitiObsvList.Count;

    if (xListCount > 0) then
        fMnuToolsFillDTR.Visible := true;

    // create menu item for refilling the diti fetch racks
    if (xListCount = 1) and (TLayoutManager.Instance.CurrentLayout.DitiObsvList[0].DitiType = '') then
        fMnuToolsFillDTR.OnClick := mnuFillDTRClick;

    // create additional menu items for refilling the diti fetch racks of each type
    if (xListCount > 1) or (xListCount = 1) and
        (TLayoutManager.Instance.CurrentLayout.DitiObsvList[0].DitiType > '') then
        for x := 0 to xListCount - 1 do
        begin
            xMenuItem := TMenuItem.Create(nil);
            xMenuItem.AutoHotkeys := maManual;
            xMenuItem.Caption := TLayoutManager.Instance.CurrentLayout.DitiObsvList[x].DitiType;
            xMenuItem.OnClick := mnuFillDTRClick;
            fMnuToolsFillDTR.Add(xMenuItem);
            FMenuItemsDestroyList.Add(xMenuItem);
        end;
end;


end.
