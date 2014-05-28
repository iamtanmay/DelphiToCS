{ --------------------------------------------------------------------------------------------------
  SETUP
  --------------------------------------------------------------------------------------------------
  Rack-, Carrier- und Workbench-Objekte für das Move-Rack-Fenster  & TSamplerUser-Objekt
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure                    Änderung / Neuerung
  -------- --  -----------------------------------   -----------------------------------------------
  18.11.98 wl     Unit beinhaltet nur noch Objekte für Plate-Move-Fenster
  alle anderen Methoden --> ObjSetup
  17.12.98 wl                                        Unit ist lauffähig
  15.01.99 wl  TMoveCarrier.sMouseUp                 SelectStacerSlot als Workbenchfunktion
  09.03.99 wl  TSamplerUser.SetAccess                Definitionen nur vom Administrator zugänglich
  29.04.99 wl  TSamplerUser.SetAccess                'Layout neu/öffnen' nur für Administrator
  19.08.99 wz                                        --> Strings in Ressourcen
  16.03.00 wl  TMoveCarrier.sMouseUp/MouseDown1      'Sampler in Progress' erscheint nicht mehr
  28.04.00 wl                                        geänderte Bezeichnungen (TLayouterThread)
  26.05.00 wl  TMoveTestThread                       neu: zum Greifen und abstellen von Racks
  10.07.00 wl  TMoveWorkbench.WBResize               in try..except-Block eingebettet
  11.10.01 mo                                        TN1067 TWorkbench => TWorkbenchExt
  17.10.01 mo                                        TN1067 Änderungen für SIAS
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  05.12.02 wl  TMoveWorkbench.DoGetPlate             TN1345 Rackdata-Membernamen geändert
  12.12.02 wl                               TN1345 TWorkbenchExt => TWorkbench
  12.12.02 wl                               TN1345 Robot. statt Sam.
  27.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  05.02.03 wl  TMoveWorkbench.WBResize      TN1295.5 Exception abgefangen wenn FArm = nil
  12.03.03 wl                               TN1293.5 uses posTools
  02.06.03 wl  TMoveWorkbench.DoGetPlate    TN1485.4 TAppSettings.IsSias ersetzt die ifdefs
  02.09.03 wl  TMoveRack.MouseDown1         TN1559   Char-Array durch string ersetzt
  12.09.03 wl  TMoveCarrier.SlotChosen      TN1581.4  Anpassung an Änderungen in TCarrier
  12.09.03 wl  alle Funktionen              TN1581.4  keine Sonderbehandlung mehr für Stacker (EditStck.pas entfernt)
  18.09.03 wl  CreateWB/Destroy             TN1581.11 "Check Racks" als CheckBox in LayMain
  17.10.03 wl                               TN1581.15 uses Stackfrm entfernt
  10.12.03 wl  CreateWB                     TN1672    CheckXConflict entfernt (ist in GetPlate/PutPlate enthalten)
  19.12.03 wl  TMoveTestThread.PutPlate     TN1672    korrigiert
  04.02.04 pk                               TN1719   uses MessageHandling
  05.03.04 pk                               TN1775   New: TMovedRackStatus, TMoveTestAction
  12.03.04 wl                               TN1812   GetPlate/PutPlate-Aufruf geändert: TRack wird übereben
  18.03.04 wl  TMoveWorkbench.DoMovement    TN1825   zu Beginn wird GlobalErr zurückgesetzt (gCommManager.LayouterResetGlobalErr)
  19.03.04 wl  TMoveWorkbench.Create/Destro TN1831   An- und Abschalten von mnuTestReadBC entfernt
  08.06.04 wl  TMoveTestAction.ReleasePlate TN1963   benutzt gGrpArm.Motors.
  15.06.04 wl  TMoveTestAction.ReleasePlate TN1963   MoveGripper durch OpenGripper ersetzt
  29.06.04 pk  TMoveWorkbench.DoMovement    TN2009.10 Create and starts ActionHandlerThread
  17.03.05 pk                               TN2352.1  uses ActionHandlerLow
  17.11.05 wl  TMoveWorkbench.DoMovement    TN2771    benutzt Auswahlbox für Gripperarm (LayMain)
  05.01.06 pk  TMoveWorkbench.DoMovement    TN2877    use FindGripArmByNameOrFirst
  31.03.06 pk                               TN3009   Gripping values changed from steps to mm
  12.03.07 pk                               TN3628   New RackMoveOptions for rack moves
  12.11.07 pk                               TN3924   Steps changed to mm
  09.01.08 wl                               TN3972   beim Erzeugen von Actions wird ActionType nicht mehr übergeben
  20.06.08 pk                               TN4139    Layout Elements no longer based on TPanel
  03.07.08 wl                               TN4157
  09.07.08 pk  TMoveCarrierSlot.DoMouseDown TN4139   calls scenechanged
  11.07.08 pk  TMoveCarrierSlot.DoMouseDown TN4139   scenechanged removed again
  16.07.08 wl                               TN4164   using TMain directly instead of TMain.FraEvaluate1
  31.07.08 pk                               TN4139   Changes for showing stacker overview
  17.09.08 wl                               TN4224   TMoveLayout = class( TLayoutWithDevices )
  20.09.08 wl  GetCurrentArm                TN4224   result muss nicht assigned sein
  23.09.08 wl                               TN4237   uses LObjAll entfernt
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  13.04.10 wl                               TN5044   uses geändert
  03.11.11 wl  DoCreateRackWell             TN5725   jetzt mit WellNr
  21.08.13 wl                               TN6231   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit LObjMove;


interface


uses
    Windows,
    Dialogs,
    Classes,
    SysUtils,
    Forms,
    Controls,
    Rack,
    RackWell,
    Carrier,
    CarrierSlot,
    ThrdMan,
    AppTypes,
    Action,
    IntfArmDevice,
    Tipset,
    Layout,
    LayoutElementCallbackTypes,
    LayoutWithDevices;

type

    TMovedRackStatus = class
    private
        fRack: TRack;
        fIsValid: boolean;
        function GetRack(): TRack;
    public
        constructor Create();
        property Rack: TRack read GetRack write fRack;
        property IsValid: boolean read fIsValid write fIsValid;
    end;

    TMoveCommandType = (mcRelease, mcReleaseAndGet, mcGet, mcPut);

    TMoveTestAction = class(TAction)
    private
        fGripperArm: IArmDevice;
        fCheckRacks: boolean;
        fCommand: TMoveCommandType;
        fRack: TRack;
        fMovedRackStatus: TMovedRackStatus;
        procedure GetPlate();
        procedure PutPlate();
        procedure ReleasePlate();
        procedure UpdateStatus(aCommand: TMoveCommandType);
    public
        constructor Create(aCommand: TMoveCommandType; aGripperArm: IArmDevice; aRack: TRack;
            aCheckRacks: boolean; aMovedRackStatus: TMovedRackStatus);
        procedure ExecFirst(); override;
        procedure ExecLast(); override;
        function IsVarispanOK(aRack: TRack): boolean;
    end;

    TMoveRackWell = class(TRackWell)
    private
        procedure DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
    protected
        procedure DoInitGraphics; override;

    end;

    TMoveRack = class(TRack)
    private
        procedure DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
        procedure CreatePopup;
        procedure ChooseStackerOverviewClick(aSender: TObject);
    protected
        procedure DoInitGraphics; override;
        function DoCreateRackWell(aWellNr: integer): TRackWell; override;
    public
        constructor Create();
        procedure TransportRack(aUsedArm: IArmDevice);
    end;

    TMoveCarrier = class(TCarrier)
    private
        procedure CreatePopup;
    protected
        function DoCreateCarrierSlot(): TCarrierSlot; override;
        procedure DoInitGraphics; override;
    public
        procedure ChooseStackerOverviewClick(aSender: TObject);
    end;

    TMoveCarrierSlot = class(TCarrierSlot)
    private
        procedure DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
        procedure ChooseStackerOverviewClick(aSender: TObject);
        procedure CreatePopup;
    protected
        procedure DoInitGraphics(); override;
    public
        constructor Create();
    end;

    TMoveLayout = class(TLayoutWithDevices)
    protected
        fMovedRackStatus: TMovedRackStatus;
        procedure SetSelectedRack(aRack: TRack);
        function DoCreateRack: TRack; override;
        function DoCreateCarrier: TCarrier; override;
    public
        constructor Create(const aLayoutName, aRunName: string);
        destructor Destroy(); override;
        function DoMovement(aUsedArm: IArmDevice; aRack: TRack; aCommand: TMoveCommandType): boolean;
        property MovedRackStatus: TMovedRackStatus read fMovedRackStatus;
    end;


implementation


uses
    Utility2,
    SamGlobe,
    ErrorManager,
    LayMain,
    GeneralTypes,
    Device,
    ActionHandlerLow,
    ObjModul,
    OperationFactory,
    OperationRack,
    GUIManager,
    LayoutManager,
    PopupMenuInfo;

constructor TMovedRackStatus.Create();
begin
    fIsValid := false;
end;

function TMovedRackStatus.GetRack(): TRack;
begin
    result := nil;
    if fIsValid then
        result := fRack;
end;

{ TMoveRackWell }
procedure TMoveRackWell.DoInitGraphics;
begin
    inherited;
    self.Graphics.Callbacks.MouseDownCallback := DoMouseDown;
end;

procedure TMoveRackWell.DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
var
    xRack: TMoveRack;
    xUsedArm: IArmDevice;
begin
    if (aButton <> gmbLeft) or (ThrMan.SamThreadRunning(true)) then
        Exit;

    xUsedArm := Main.CurrentArm;
    if not assigned(xUsedArm) then
        EXIT;

    xRack := self.Rack as TMoveRack;
    xRack.TransportRack(xUsedArm);
end;

constructor TMoveRack.Create();
begin
    inherited Create();
end;

procedure TMoveRack.DoInitGraphics;
begin
    inherited;
    self.Graphics.Callbacks.MouseDownCallback := DoMouseDown;
    CreatePopup();
end;

function TMoveRack.DoCreateRackWell(aWellNr: integer): TRackWell;
begin
    result := TMoveRackWell.Create(aWellNr);
end;

procedure TMoveRack.ChooseStackerOverviewClick(aSender: TObject);
var
    xCarrier: TCarrier;
begin
    xCarrier := TLayout.GetCarrierOfRack(self);
    (xCarrier as TMoveCarrier).ChooseStackerOverviewClick(xCarrier);
end;

procedure TMoveRack.CreatePopup();
begin
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create(TLanguageString.
        Read('Choose Stacker Level', 'Stackerebene wählen'), False, True, ChooseStackerOverviewClick,
        'mnuShowStackerLevelDialog'));
end;

procedure TMoveRack.TransportRack(aUsedArm: IArmDevice);
var
    xCommand: TMoveCommandType;
    xSelectedRack: TRack;
begin
    xSelectedRack := (TLayoutManager.Instance.CurrentLayout as TMoveLayout).MovedRackStatus.Rack;

    xCommand := mcGet;
    if Assigned(xSelectedRack) then
    begin
        if self = xSelectedRack then
            xCommand := mcRelease
        else
            xCommand := mcReleaseAndGet;
    end;

    (TLayoutManager.Instance.CurrentLayout as TMoveLayout).DoMovement(aUsedArm, self, xCommand);
end;

procedure TMoveRack.DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
    aX, aY: double);
var
    xUsedArm: IArmDevice;
begin
    if (aButton <> gmbLeft) or (ThrMan.SamThreadRunning(true)) then
        Exit;

    xUsedArm := Main.CurrentArm;
    if not assigned(xUsedArm) then
        EXIT;

    self.TransportRack(xUsedArm);
end;

{ TMoveCarrierSlot }
constructor TMoveCarrierSlot.Create();
begin
    inherited Create();
end;

procedure TMoveCarrierSlot.DoInitGraphics;
begin
    inherited;
    self.Graphics.Callbacks.MouseDownCallback := DoMouseDown;
    CreatePopup();
end;

procedure TMoveCarrierSlot.ChooseStackerOverviewClick(aSender: TObject);
var
    xCarrier: TCarrier;
begin
    xCarrier := self.Carrier as TCarrier;
    (xCarrier as TMoveCarrier).ChooseStackerOverviewClick(xCarrier);
end;

procedure TMoveCarrierSlot.CreatePopup();
begin
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create(TLanguageString.
        Read('Choose Stacker Level', 'Stackerebene wählen'), False { checked } , true,
        ChooseStackerOverviewClick, 'mnuShowStackerLevelDialog'));
end;

procedure TMoveCarrierSlot.DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
var
    xSelectedRack: TRack;
    xCarrier: TCarrier;
    xUsedArm: IArmDevice;
begin
    if (aButton <> gmbLeft) or (ThrMan.SamThreadRunning(true)) then
        Exit;
    if Assigned(self.Rack) then
        EXIT;

    xUsedArm := Main.CurrentArm;
    if not assigned(xUsedArm) then
        EXIT;

    xSelectedRack := (TLayoutManager.Instance.CurrentLayout as TMoveLayout).fMovedRackStatus.Rack;
    if not Assigned(xSelectedRack) then
        Exit;

    xCarrier := self.Carrier as TCarrier;

    TLayoutManager.Instance.CurrentLayout.MoveRack(xSelectedRack, xCarrier.Name, self.SlotNr, rotation_0);

    (TLayoutManager.Instance.CurrentLayout as TMoveLayout).DoMovement(xUsedArm, xSelectedRack, mcPut);
end;

{ TMoveCarrier }

function TMoveCarrier.DoCreateCarrierSlot: TCarrierSlot;
begin
    result := TMoveCarrierSlot.Create();
end;

procedure TMoveCarrier.DoInitGraphics;
begin
    inherited;
    CreatePopup();
end;

procedure TMoveCarrier.ChooseStackerOverviewClick(aSender: TObject);
begin
    self.ShowStackerOverviewDialog();
    // (TLayoutManager.Instance.CurrentLayout as TSetupLayout).RefreshScene();
end;

procedure TMoveCarrier.CreatePopup();
begin
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create(TLanguageString.
        Read('Choose Stacker Level', 'Stackerebene wählen'), False { checked } , true,
        ChooseStackerOverviewClick, 'mnuShowStackerLevelDialog'));
end;

constructor TMoveLayout.Create(const aLayoutName, aRunName: string);
begin
    inherited Create(aLayoutName, aRunName);
    fMovedRackStatus := TMovedRackStatus.Create();
    Main.pnMoveRacks.Left := 408;
    Main.pnMoveRacks.Visible := true;
end;

destructor TMoveLayout.Destroy();
begin
    Main.pnMoveRacks.Visible := false;
    fMovedRackStatus.Free;
    inherited;
end;

function TMoveLayout.DoCreateRack: TRack;
begin
    result := TMoveRack.Create();
end;

function TMoveLayout.DoCreateCarrier: TCarrier;
begin
    result := TMoveCarrier.Create();
end;

function TMoveLayout.DoMovement(aUsedArm: IArmDevice; aRack: TRack; aCommand: TMoveCommandType): boolean;
// Get-Plate-Funktion - liefert bei erfolgreichem GetPlate das Rack als TRack
// ------------------------------------------------------------------------------
var
    xAction: TMoveTestAction;
begin
    result := false;
    gErrorManager.LayouterResetGlobalErr;

    // Get Gripper Arm
    if not Assigned(aUsedArm.GripDevice) then
        EXIT;

    result := true;
    xAction := TMoveTestAction.Create(aCommand, aUsedArm, aRack, Main.cbCheckRacks.Checked, fMovedRackStatus);
    gmCreateBasicActionHandlerThread(xAction, []);
end;

// --------------------------------------------------------------------------------------------------
procedure TMoveLayout.SetSelectedRack(aRack: TRack);
// --------------------------------------------------------------------------------------------------
begin
    fMovedRackStatus.Rack := aRack;
end;

// --------------------------------------------------------------------------------------------------
constructor TMoveTestAction.Create(aCommand: TMoveCommandType; aGripperArm: IArmDevice; aRack: TRack;
    aCheckRacks: boolean; aMovedRackStatus: TMovedRackStatus);
// --------------------------------------------------------------------------------------------------
begin
    inherited Create();
    fGripperArm := aGripperArm;
    fCheckRacks := aCheckRacks;
    fCommand := aCommand;
    fRack := aRack;
    fMovedRackStatus := aMovedRackStatus;
end;

// --------------------------------------------------------------------------------------------------
procedure TMoveTestAction.GetPlate;
// --------------------------------------------------------------------------------------------------
var
    xRackOp: TRackMoveOperation;
begin

    xRackOp := TOperationFactory.CreateRackMoveOp(fGripperArm, fRack);
    if fCheckRacks then
    begin
        if xRackOp.GetPlate(true, nil, nil, TRackMoveOperation.GetDefaultRackMoveOptions()) then
            gGUIManager.MessageBox(TLanguageString.Read('Rack found!', 'Rack wurde gefunden!'),
                TLanguageString.Read('Get Plate', 'Rack greifen'), 0)
        else
            gGUIManager.MessageBox(TLanguageString.Read('Rack not found!', 'Rack wurde nicht gefunden!'),
                TLanguageString.Read('Get Plate', 'Rack greifen'), 0);
    end
    else
        xRackOp.GetPlate(false, nil, nil, TRackMoveOperation.GetDefaultRackMoveOptions());

    xRackOp.Free;

    UpdateStatus(mcGet)
end;

// --------------------------------------------------------------------------------------------------
procedure TMoveTestAction.PutPlate;
// --------------------------------------------------------------------------------------------------
var
    xRackOp: TRackMoveOperation;
begin
    xRackOp := TOperationFactory.CreateRackMoveOp(fGripperArm, fRack);
    if FCheckRacks then
    begin
        if xRackOp.PutPlate(true, nil, nil, TRackMoveOperation.GetDefaultRackMoveOptions()) then
            gGUIManager.MessageBox(TLanguageString.Read('Rack found!', 'Rack wurde gefunden!'),
                TLanguageString.Read('Put Plate', 'Rack abstellen'), 0)
        else
            gGUIManager.MessageBox(TLanguageString.Read('Rack not found!', 'Rack wurde nicht gefunden!'),
                TLanguageString.Read('Put Plate', 'Rack abstellen'), 0);
    end
    else
        xRackOp.PutPlate(false, nil, nil, TRackMoveOperation.GetDefaultRackMoveOptions());

    xRackOp.Free;

    UpdateStatus(mcPut)
end;

// --------------------------------------------------------------------------------------------------
procedure TMoveTestAction.ReleasePlate;
// --------------------------------------------------------------------------------------------------
begin
    fGripperArm.GripDevice.OpenGripper(FRack.Structure.H_VOpen_mm, true);
    UpdateStatus(mcRelease)
end;

// --------------------------------------------------------------------------------------------------
procedure TMoveTestAction.ExecFirst;
// --------------------------------------------------------------------------------------------------
begin
    gGUIManager.SetCursor(crHourGlass);

    if (fCommand in [mcRelease, mcReleaseAndGet, mcGet]) and (not self.IsVarispanOK(fRack)) then
        Exit;

    case fCommand of
        mcRelease:
            begin
                ReleasePlate;
            end;
        mcReleaseAndGet:
            begin
                ReleasePlate;
                GetPlate;
            end;
        mcGet:
            begin
                GetPlate;
            end;
        mcPut:
            begin
                PutPlate;
            end;
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TMoveTestAction.UpdateStatus(aCommand: TMoveCommandType);
// --------------------------------------------------------------------------------------------------
var
    xIsNotError: boolean;
begin
    xIsNotError := (not gErrorManager.IsGlobalErr);
    // try is VERY IMPORTANT.  In case fMovedRackStatus is not set, we dont want to cause an exception
    try
        if (aCommand in [mcGet]) then
        begin
            if xIsNotError then
                fMovedRackStatus.Rack := fRack;
            fMovedRackStatus.IsValid := xIsNotError;
        end
        else if aCommand in [mcRelease, mcPut] then
        begin
            fMovedRackStatus.IsValid := false;
        end;
    except
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TMoveTestAction.ExecLast();
// --------------------------------------------------------------------------------------------------
begin
    gGUIManager.SetCursor(crDefault);
end;

// --------------------------------------------------------------------------------------------------
function TMoveTestAction.IsVarispanOK(aRack: TRack): boolean;
// --------------------------------------------------------------------------------------------------
begin
    result := true;
    if (fGripperArm.GripDevice.IsPosWithinRange(aRack.Structure.H_VClose_mm)) and
        (fGripperArm.GripDevice.IsPosWithinRange(aRack.Structure.H_VOpen_mm)) then
        EXIT;

    gGUIManager.MessageBox(TLanguageString.Read('Gripper Rackdata is not correct!',
        'Greifer Rackdaten sind nicht korrekt!'), '', MB_OK);
    result := false;
end;


end.
