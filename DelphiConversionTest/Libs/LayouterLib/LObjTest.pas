{ --------------------------------------------------------------------------------------------------
  SETUP
  --------------------------------------------------------------------------------------------------
  Rack-, Carrier- und Workbench-Objekte zum Testen der Rackpositionen
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure                    Änderung / Neuerung
  -------- --  -----------------------------------   -----------------------------------------------
  05.10.99 wl                                        neue Unit
  15.02.00 wl  TTestWorkbench.CreateArm              wenn Tip 1 nicht vorhanden, wird der nächste genommen
  22.02.00 wl  CreateTip                             Aufruf geändert (wie in ObjArm)
  16.05.00 wl  TTestRack.MouseDown1                  nach CheckTubePos werden die Motorpositionen geändert
  26.05.00 wl  TTestRack.MouseDown1       ruft gmCheckTubePos auf
  07.06.00 wl  TTestArm.SetUseTips                   aus LObjEdit kopiert (Update von fraEvaluate)
  11.10.01 mo                                        TN1067 TWorkbench => TWorkbenchExt
  17.10.01 mo                                        TN1067 Änderungen für SIAS
  28.12.01 tbh diverse                               TN1051 Aufrufe und Deklarationen von SetUseTips angepasst
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  12.12.02 wl                               TN1345 TWorkbenchExt => TWorkbench
  27.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  12.03.03 wl                               TN1293.5 uses posTools
  30.05.03 wl  TTestArm.SetUseTips          TN1493.2 Motor-Beschriftung automatisch beim Setzen von TipUsed
  18.03.04 wl  TTestArm,TTestTip            TN1765.1 --> LObjAll
  18.03.04 wl  TTestRack.MouseDown1         TN1825   zu Beginn wird GlobalErr zurückgesetzt (gCommManager.LayouterResetGlobalErr)
  19.03.04 wl  TTestWorkbench.CreateArm     TN1788   Create statt CreateArm (Anpassung an TArm)
  20.04.04 wl  TTestWorkbench.CreateArm     TN1788   an Änderungen von TArmPanel und TTipPanel angepasst
  21.06.05 pk  TTestWorkbench.CreateArm     TN2464.3 Do not call Arm.SetUseTips
  07.12.06 wl                               TN3243   uses SamCmd entfernt
  20.06.08 pk                               TN4139    Layout Elements no longer based on TPanel
  02.07.08 pk  gmCheckTubePos               TN4139   use rack pos number instead of row, col number
  03.07.08 wl                                         TN4157
  16.07.08 wl                               TN4164   using TMain directly instead of TMain.FraEvaluate1
  31.07.08 pk                               TN4139   Changes for showing stacker overview
  17.09.08 wl  DoCreateTipsetDevice         TN4224   benutzt TTipsetDevice
  20.09.08 wl  GetCurrentArm                TN4224   result muss nicht assigned sein
  23.09.08 wl                               TN4237   nicht mehr von TTestAndSetupLayout abgeleitet
  23.09.08 wl  TTestRackWell.DoMouseDown    TN4237   Fehler in Methode behoben
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  14.01.11 wl  TTestRackWell.DoMouseDown    TN5426   startet TRackPositionMoveAction
  20.09.11 wl                               TN5723   an Änderungen angepasst
  03.11.11 wl  DoCreateRackWell             TN5725   jetzt mit WellNr
  28.06.12 wl  DoMouseDown                  TN5527   vor dem Anfahren der Positinen werden jetzt nicht mehr alle Motorpositionen abgefragt
  -------------------------------------------------------------------------------------------------- }

unit LObjTest;


interface


uses
    Windows,
    Dialogs,
    Classes,
    SysUtils,
    Forms,
    Controls,
    Menus,
    Math,
    Tipset,
    Rack,
    RackWell,
    Carrier,
    CarrierSlot,
    AppTypes,
    CommonTypes,
    IntfArmDevice,
    TipSystem,
    LayoutElementCallbackTypes,
    LayoutWithDevices;

type
    TTestRackWell = class(TRackWell)
    private
        procedure DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
        procedure CreatePopup;
        procedure ChooseStackerOverviewClick(aSender: TObject);
        function GetCurrentArm: IArmDevice;
    protected
        procedure DoInitGraphics; override;

    end;

    TTestRack = class(TRack)
    private
        procedure ChooseStackerOverviewClick(aSender: TObject);
        procedure CreatePopup;
    protected
        function DoCreateRackWell(aWellNr: integer): TRackWell; override;
        procedure DoInitGraphics; override;
    public
        constructor Create();
    end;

    TTestCarrierSlot = class(TCarrierSlot)
    private
        procedure ChooseStackerOverviewClick(aSender: TObject);
        procedure CreatePopup;
    protected
        procedure DoInitGraphics(); override;
    public
        constructor Create();
    end;

    TTestCarrier = class(TCarrier)
    private
        procedure ChooseStackerOverviewClick(aSender: TObject);
        procedure CreatePopup;
    protected
        procedure DoInitGraphics; override;
        function DoCreateCarrierSlot(): TCarrierSlot; override;
    end;

    TTestLayout = class(TLayoutWithDevices)
    protected
        function DoCreateRack: TRack; override;
        function DoCreateCarrier: TCarrier; override;
    end;

var
    gRackPopUp: TPopupMenu;


implementation


uses
    RackPositionMoveAction,
    ActionHandlerLow,
    Layout,
    LayMain,
    LayGlobe,
    ObjModul,
    ErrorManager,
    GeneralTypes,
    PopupMenuInfo,
    ThrdMan;

{ TTestRackWell }

procedure TTestRackWell.DoInitGraphics;
begin
    inherited;
    self.Graphics.Callbacks.MouseDownCallback := DoMouseDown;
    self.Graphics.Cursor := cGraphicsMouseCursorUpArrow;
    CreatePopup();
end;

procedure TTestRackWell.CreatePopup();
begin
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create(TLanguageString.
        Read('Choose Stacker Level', 'Stackerebene wählen'), False, True, ChooseStackerOverviewClick,
        'mnuShowStackerLevelDialog'));
end;

procedure TTestRackWell.ChooseStackerOverviewClick(aSender: TObject);
var
    xCarrier: TCarrier;
begin
    xCarrier := TLayout.GetCarrierOfRack(self.Rack as TRack);
    (xCarrier as TTestCarrier).ChooseStackerOverviewClick(xCarrier);
end;

function TTestRackWell.GetCurrentArm(): IArmDevice;
begin
    result := Main.CurrentArm;
end;

procedure TTestRackWell.DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
var
    xUsedArm: IArmDevice;
    xRack: TRack;
    xAction: TRackPositionMoveAction;
begin
    xUsedArm := GetCurrentArm();
    if not Assigned(xUsedArm) then
        EXIT;
    if not Assigned(xUsedArm.PipDevice) then
        EXIT;

    gErrorManager.LayouterResetGlobalErr;
    if (gErrorManager.IsGlobalErr) or (aButton <> gmbLeft) or (ThrMan.SamThreadRunning(true)) then
        Exit;
    xRack := self.Rack as TRack;

    xAction := TRackPositionMoveAction.Create(xUsedArm, xRack, self.WellNr, xUsedArm.PipDevice.UseTips);
    gmCreateBasicActionHandlerThread(xAction, []);
end;

{ TTestRack }

constructor TTestRack.Create();
begin
    inherited Create();
end;

function TTestRack.DoCreateRackWell(aWellNr: integer): TRackWell;
begin
    result := TTestRackWell.Create(aWellNr);
end;

procedure TTestRack.DoInitGraphics;
begin
    inherited;
    CreatePopup();
end;

procedure TTestRack.ChooseStackerOverviewClick(aSender: TObject);
var
    xCarrier: TCarrier;
begin
    xCarrier := TLayout.GetCarrierOfRack(self);
    (xCarrier as TTestCarrier).ChooseStackerOverviewClick(xCarrier);
end;

procedure TTestRack.CreatePopup();
begin
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create(TLanguageString.
        Read('Choose Stacker Level', 'Stackerebene wählen'), False, True, ChooseStackerOverviewClick,
        'mnuShowStackerLevelDialog'));
end;

{ TTestCarrierSlot }

constructor TTestCarrierSlot.Create();
begin
    inherited Create();
end;

procedure TTestCarrierSlot.DoInitGraphics;
begin
    inherited;
    CreatePopup();
end;

procedure TTestCarrierSlot.ChooseStackerOverviewClick(aSender: TObject);
var
    xCarrier: TCarrier;
begin
    xCarrier := self.Carrier as TCarrier;
    (xCarrier as TTestCarrier).ChooseStackerOverviewClick(xCarrier);
end;

procedure TTestCarrierSlot.CreatePopup();
begin
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create(TLanguageString.
        Read('Choose Stacker Level', 'Stackerebene wählen'), False { checked } , true,
        ChooseStackerOverviewClick, 'mnuShowStackerLevelDialog'));
end;

{ TTestCarrier }

procedure TTestCarrier.DoInitGraphics;
begin
    inherited;
    CreatePopup();
end;

procedure TTestCarrier.ChooseStackerOverviewClick(aSender: TObject);
begin
    self.ShowStackerOverviewDialog();
end;

procedure TTestCarrier.CreatePopup();
begin
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create(TLanguageString.
        Read('Choose Stacker Level', 'Stackerebene wählen'), False, True, ChooseStackerOverviewClick,
        'mnuShowStackerLevelDialog'));
end;

function TTestCarrier.DoCreateCarrierSlot: TCarrierSlot;
begin
    result := TTestCarrierSlot.Create();
end;

{ TTestLayout }

function TTestLayout.DoCreateCarrier: TCarrier;
begin
    result := TTestCarrier.Create();
end;

function TTestLayout.DoCreateRack: TRack;
begin
    result := TTestRack.Create();
end;


end.
