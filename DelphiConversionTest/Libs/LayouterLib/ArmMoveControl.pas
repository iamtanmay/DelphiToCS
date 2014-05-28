{ --------------------------------------------------------------------------------------------------
  Ebene 2 (Sam-Interface)
  --------------------------------------------------------------------------------------------------
  Frame zur direkten Steuerung von Motoren  (ähnlich MoveCode-Fenster)
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  24.02.00 wl                         neue Unit (zunächst nur für Layouter)
  25.05.00 wl  btnInitClick           TMoveThread mit InitFirst statt TLayouterThread
  26.05.00 wl                         uses geändert
  07.06.00 wl                         Edit-Felder sind beim Start leer
  30.10.00 tbh SetMotorPosition       SaveXYMovement geht auch bei Bewegung in negative Richtung
  10.10.02 wl                         TN1293.2 Ini Access - uses geändert
  01.11.02 mo  BtnMoveClick           TN1301.4 SIAS198 Gripper Home and End corrected
  25.11.02 wl  edXPos                 TN1357   MaxLength = 5
  25.11.02 wl  btnInitClick           TN1359   führt das gleiche aus wie Init im Tools-Menü
  12.12.02 wl                         TN1345 Robot. statt Sam. - bei _Move geänderter Funktionsaufruf
  27.12.02 wl                         TN1293.5 uses und WinlissyIniAccess geändert
  30.05.03 wl                         TN1493.2 komplett überarbeitet
  02.06.03 wl                         TN1485.4 gemeinsame unit für ZP01 und ZP02
  03.06.03 wl                         TN1485.4 nochmal überarbeitet
  05.06.03 wl  InitControls           TN1493.2 GetPositions wird gleich am Anfang ausgeführt
  05.06.03 wl                         TN1493.3 Y-Guard ist immer angaschaltet
  16.06.03 wl  SetMotorPosition       TN1493.2 Y-Guard verschiebt nur noch die Tips, die wirklich im Weg stehen
  18.09.03 wl  SetMotorValues         TN1576    neu: Reverse für SIAS-HX-Motor - damit die Tasten nicht verdreht sind
  18.09.03 wl                         TN1581.16 Default bei XY-Motor: 5 Steps
  18.09.03 wl  SetMotorPosition       TN1581.16 neu: Safety gilt erst ab 11 Steps!
  01.10.03 wl  KeyMoveMotor           TN1576    Reverse wird auch bei Tasten berücksichtigt
  08.10.03 wl                         TN1581.16 Default bei V-Motor: 5 Steps, kein "Safety" für V-Motor
  10.12.03 wl                         TN1672    Funktionsnamen geändert
  19.12.03 wl                         TN1672    _Move durch Robot.ExternMotorMove ersetzt
  18.03.04 wl                         TN1825    bei allen User-Aktionen wird vorher gCommManager.LayouterResetGlobalErr ausgeführt
  18.03.04 wl                         TN1765.1  Umstellung von Robot auf Arm-Devices
  18.03.04 wl  SetMotorPosition       TN1765.1  große Teile --> DevicesArms
  18.03.04 wl  TabControl1Change      TN1740    Beim Umschalten zwischen Pipettier- und Greifarm werden die Motor-Werte aktualisiert
  18.03.04 wl  btnRefreshClick        TN1830    Neuer Refresh-Button: alle Positionen werden gelesen
  05.04.04 wl  InitControls           TN1788    Robot.ArmCount und .ArmType ersetzt durch ArmDevice-Properties
  20.04.04 wl  spinTipUsedChange      TN1788    gPipArm.SetUseTips statt WB.Arm.SetUseTips
  18.05.04 wl  SetMotorPosition       TN1928    Exit bei nicht übereinstimmender Position war zu irritierend -> entfernt
  28.06.04 pk  btnInitClick           TN2009.6  New Threading style
  29.06.04 pk  btnInitClick           TN2009.8  Starts TBasicInitAction
  17.03.05 pk                         TN2352.1  uses ActionHandlerLow
  03.06.05 wl  versch.Änderungen      TN2436    Anzeige von 2 Pipettierarmen ermöglicht
  15.06.05 pk  ChangeArmByIndex       TN2464    Change the tab to the give arm
  15.06.05 pk  CurrentArm             TN2464    new property
  21.06.05 pk  Reverse                TN2464.3  Removed - the logic of a reversed motor is implicitly contained in TMotorDevice
  21.06.05 pk  TipUsedChanged         TN2464.3  New : called when used tip is changed
  07.09.05 wl  ChangeArm              TN2593    Zeile eingefügt, um sicherzugehen, dass Value nicht kleiner als MinValue ist
  15.11.05 pk  ChangeArm              TN2742    show panels depending on if  motor are assigned
  07.04.06 pk  TGripControl           TN2958    New: Can now control a pneumatic gripper
  10.04.06 pk  BtnInitClick           TN3031    InitAction has new parameter aFullInit
  18.07.06 pk                         TN3204    New: Init button for each motor
  03.08.06 pk                         TN3244    Units (steps, mm) are now displayed for each movecontrol
  03.08.06 pk  ChangeArm              TN3246    Call SelectBoundShape
  11.10.06 wl  InitControls           TN3356    Wenn für einen Arm kein PipDevice definiert ist, soll es keine Access Violation geben
  08.12.06 wl                         TN3457    Beschränkung auf best. Zeichenanzahlen bei edXPos und edYPos aufgehoben
  26.01.07 pk  InitControls           TN3503    test for LocationBasedMotion moved to SetMotorDisplay
  01.03.07 wl  ChangeArmByIndex       TN3611    Prüfung ob Arm wirklich existiert
  06.03.07 wl  btnInitClick           TN3620    Kein ResetModules vor Init
  12.11.07 pk                         TN3924    Steps changed to mm
  07.01.08 pk  TMoveCtrlInitAction    TN3864    New: Updates Motor Position text boxes after init has been done
  07.01.08 pk  TGripControl           TN3864    ClampToMinOrMax implemented
  07.01.08 pk  InitControls           TN3971    swap up and down buttons for Z
  03.07.08 wl                         TN4157
  09.07.08 pk                         TN4139    various changes
  16.07.08 wl                         TN4164    Arm-Movement-Panel jetzt nur noch für einen Arm
  13.03.09 pk  OnClose                TN4465    New: set CloseAction to caFree
  26.06.09 wl                         TN4631    Neue Zeichnungen für die Pfeile
  30.06.09 wl                         TN4631    Noch mal neue Zeichnungen für die Pfeile
  16.07.09 pk                         TN4661    New: DetermineDirectionButtons
  22.07.09 pk                         TN4631    Noch mal neue Zeichnungen für die Pfeile
  19.05.10 wl                         TN5115    Inhalt der Step-Felder wird im Motor gespeichert, deshalb bleiben sie immer erhalten
  20.05.10 wl                         TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  07.06.10 wl                         TN5116   geänderter Aufruf von FindWorkspaceForArm
  11.06.10 pk                         TN5138    All Motor/Machine commands are now done over a thread. otherwise deadlocks occur
  17.06.10 wl  FindWorkspaceForArm    TN5150   von LayoutWithDevices heirher
  16.11.10 wl                         TN5351   uses ActionLow entfernt
  18.01.11 wl                         TN5166    TSetPositionMoveCtrlAction Call Control.EnableButtons
  22.03.11 wl                         TN5401   neu: VMotor kann jetzt auch bedient werden
  22.03.11 wl                         TN5401   überarbeitet: Kein statisches Array mehr
  23.03.11 wl  DoOnSafetyMove         TN5515   CombinedArmsMoveToZTravel wird jetzt explizit aufgerufen
  24.03.11 wl                         TN5401   Bugfix
  01.04.11 wl  SetMotorDisplay        TN5401   Bugfix
  05.04.11 wl  SetMotorDisplay        TN5401   Nach V-Motor-Move wird auch Y-Anzeige upgadated
  06.04.11 wl                         TN5401   Zusätzliche Anzeige für Y und V zusammen
  07.04.11 wl                         TN5401   optische Änderungen
  28.06.12 wl  GetPositions           TN5527   jetzt public
  21.08.13 wl                         TN6231   uses geändert
  22.08.13 wl  TMoveCtrlInitAction    TN6233   statt InitBasic wird ConnectAndInit aufgerufen
  -------------------------------------------------------------------------------------------------- }

unit ArmMoveControl;


interface


uses
    Forms,
    ExtCtrls,
    Spin,
    StdCtrls,
    Controls,
    Buttons,
    Classes,
    MoveControl,
    IntfArmDevice,
    CommonTypes,
    Action,
    Layout,
    Workspace;

type
    TArmMotorType = (amtXMotor, amtYMotor, amtZMotor, amtGMotor, amtRMotor, amtVMotor);

    TMoveCtrlInitAction = class(TAction)
    strict private
        fOnEndAction: TNotifyEvent;
    strict protected
        function GetMustPutBackTool: boolean; override;
    public
        procedure ExecFirst(); override;
        property OnEndAction: TNotifyEvent read fOnEndAction write fOnEndAction;
    end;

    TMoveToZTravelAction = class(TAction)
    strict private
        fArm: IArmDevice;
        fOnEndAction: TNotifyEvent;
    public
        constructor Create(const aArm: IArmDevice);
        procedure ExecFirst(); override;
        property OnEndAction: TNotifyEvent read fOnEndAction write fOnEndAction;
    end;

    TMoveCtrlAction = class(TAction)
    strict protected
        fControl: TMoveControl;
    public
        constructor Create(const aControl: TMoveControl);
    end;

    TSetPositionMoveCtrlAction = class(TMoveCtrlAction)
    strict private
        fIsSafety: boolean;
        fDirection: integer;
    public
        constructor Create(const aControl: TMoveControl; const aIsSafety: boolean; const aDirection: integer);
        procedure ExecFirst(); override;
    end;

    TSingleInitMoveCtrlAction = class(TMoveCtrlAction)
    public
        procedure ExecFirst(); override;
    end;

    TReadPositionsAction = class(TAction)
    strict private
        fControls: TArray<TMoveControl>;
        fTipUsed: integer;
    public
        constructor Create(const aControls: TArray<TMoveControl>; const aTipUsed: integer);
        procedure ExecFirst(); override;
    end;

    TfrmArmMoveControl = class(TForm)
        Panel1: TPanel;
        cbSafety: TCheckBox;
        cbKeys: TCheckBox;
        btnRefresh: TButton;
        btnSingleZTravel: TButton;
        grpXY: TGroupBox;
        YUp: TSpeedButton;
        XLeft: TSpeedButton;
        Label8: TLabel;
        XRight: TSpeedButton;
        YDown: TSpeedButton;
        X_Label: TLabel;
        Y_Label: TLabel;
        lblXUnits: TLabel;
        lblYUnits: TLabel;
        edXPos: TEdit;
        edYPos: TEdit;
        YStep: TEdit;
        XStep: TEdit;
        btnInitX: TButton;
        btnInitY: TButton;
        grpZ: TGroupBox;
        ZStepUp: TSpeedButton;
        Label4: TLabel;
        Label5: TLabel;
        ZStepDown: TSpeedButton;
        lblZUnits: TLabel;
        edZPos: TEdit;
        ZStep: TEdit;
        btnInitZ: TButton;
        grpRotation: TGroupBox;
        RStepLeft: TSpeedButton;
        Label15: TLabel;
        Label17: TLabel;
        RStepRight: TSpeedButton;
        lblRUnits: TLabel;
        edRPos: TEdit;
        RStep: TEdit;
        btnInitR: TButton;
        grpGrip: TGroupBox;
        GStepOpen: TSpeedButton;
        Label19: TLabel;
        Label20: TLabel;
        GStepClose: TSpeedButton;
        lblGUnits: TLabel;
        edGPos: TEdit;
        GStep: TEdit;
        btnInitG: TButton;
        grpCurrentTip: TGroupBox;
        spinTipUsed: TSpinEdit;
        rdoMoveMode: TRadioGroup;
        btnInitV: TButton;
        VStep: TEdit;
        edVPos: TEdit;
        lblVUnits: TLabel;
        VStepClose: TSpeedButton;
        VStepOpen: TSpeedButton;
        Label1: TLabel;
        edYVPos: TEdit;
        lblYVUnits: TLabel;
        Label2: TLabel;
        procedure btnInitClick(Sender: TObject);
        procedure BtnMoveClick(Sender: TObject);
        procedure edPosDblClick(Sender: TObject);
        procedure edPosKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure cbKeysClick(Sender: TObject);
        procedure btnRefreshClick(Sender: TObject);
        procedure spinTipUsedChange(Sender: TObject);
        procedure btnSingleZTravelClick(Sender: TObject);
        procedure btnInitSingleClick(Sender: TObject);
        procedure rdoMoveModeClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure edStepOnChange(Sender: TObject);
        procedure FormCreate(Sender: TObject);

    strict private
        fCurrentArm: IArmDevice;
        fMoveControls: TArray<TMoveControl>;
        function GetMoveControl(aMType: TArmMotorType): TMoveControl;
        function MotorExists(aMType: TArmMotorType): boolean;
        procedure EnableZControls();
        procedure EnableVControls();
        procedure EnableXYControls();
        procedure EnableTipControls();
        procedure EnableControls();
        procedure SetPosition(aControl: TMoveControl; aDirection: integer);
        procedure SetTipUsed(aValue: integer);
        function GetTipUsed: integer;
        procedure SetMotorDisplay(aIndex: integer; aMType: TArmMotorType;
            aPosDisplay, aSumPosDisplay, aStepDisplay: TEdit; aInitBtn: TButton; aUnitsLabel: TLabel);
        procedure KeyMoveMotor(var Key: Word; aMotor: TMoveControl; aDirection: integer);
        procedure SelectArmTip(aTipIndex: integer);
        procedure InitSingle(aControl: TMoveControl);
        procedure DoOnSafetyMove(aSender: TObject);

        procedure UpdateMoveMode;
        procedure OnEndAction(aSender: TObject);
        procedure DetermineDirectionButtons(const aMType: TArmMotorType;
            out oPlusBtn, oMinusBtn: TSpeedButton);
        class function FindWorkspaceForArm(aLayout: TLayout; const aArmDeviceName: string): TWorkspace;
    public
        // public methods
        procedure InitControls(aCurrentArm: IArmDevice);
        procedure ReadPositions;
        procedure GetPositions;
        procedure KeyHandling(var Key: Word; Shift: TShiftState);
        procedure TipUsedChanged();
        function GetCurrentX(): TPosMM;
        function GetCurrentY(): TPosMM;
        class procedure ReadPositionsForControls(const aControls: TArray<TMoveControl>;
            const aTipUsed: integer);
        class procedure GetPositionsForControls(const aControls: TArray<TMoveControl>;
            const aTipUsed: integer);
        // properties
        property TipUsed: integer read GetTipUsed write SetTipUsed;
        property CurrentArm: IArmDevice read fCurrentArm;
    end;


implementation


{$R *.DFM}

uses
    SysUtils,
    ControlUtils,
    ThrdMan,
    TipMapUtils,
    ActionHandlerLow,
    ErrorManager,
    SamHigh,
    IntfMotorBasedMotionDevice,
    AppSettings,
    LayoutManager,
    DeviceInitHandling,
    AppTypes;

{ TMoveCtrlInitAction }

procedure TMoveCtrlInitAction.ExecFirst;
begin
    TDeviceInitHandling.ConnectAndInit(true);

    if Assigned(fOnEndAction) then
        fOnEndAction(self);
end;

function TMoveCtrlInitAction.GetMustPutBackTool: boolean;
begin
    result := true;
end;

{ TMoveToZTravelAction }

constructor TMoveToZTravelAction.Create(const aArm: IArmDevice);
begin
    inherited Create();
    fArm := aArm;
end;

procedure TMoveToZTravelAction.ExecFirst;
begin
    gmMoveToZTravel(fArm);
    if Assigned(fOnEndAction) then
        fOnEndAction(self);
end;

{ TMoveCtrlAction }

constructor TMoveCtrlAction.Create(const aControl: TMoveControl);
begin
    inherited Create();
    fControl := aControl;
end;

{ TSetPositionMoveCtrlAction }

constructor TSetPositionMoveCtrlAction.Create(const aControl: TMoveControl; const aIsSafety: boolean;
    const aDirection: integer);
begin
    inherited Create(aControl);
    fIsSafety := aIsSafety;
    fDirection := aDirection;
end;

procedure TSetPositionMoveCtrlAction.ExecFirst;
begin
    try
        fControl.Move(fDirection, fIsSafety);
    finally
        fControl.EnableButtons(true);
    end;
end;

{ TSingleInitMoveCtrlAction }

procedure TSingleInitMoveCtrlAction.ExecFirst;
begin
    fControl.Init();
end;

{ TReadPositionsAction }

constructor TReadPositionsAction.Create(const aControls: TArray<TMoveControl>; const aTipUsed: integer);
begin
    inherited Create();
    fControls := aControls;
    fTipUsed := aTipUsed;
end;

procedure TReadPositionsAction.ExecFirst;
begin
    TfrmArmMoveControl.ReadPositionsForControls(fControls, fTipUsed);
end;

{ TfrmArmMoveControl }

procedure TfrmArmMoveControl.OnEndAction(aSender: TObject);
begin
    ReadPositions();
end;

procedure TfrmArmMoveControl.btnInitClick(Sender: TObject);
var
    xAction: TMoveCtrlInitAction;
begin
    if not ThrMan.SamThreadRunning(true) then
    begin
        xAction := TMoveCtrlInitAction.Create();
        xAction.OnEndAction := self.OnEndAction;
        gmCreateBasicActionHandlerThread(xAction, []);
    end;
end;

procedure TfrmArmMoveControl.btnRefreshClick(Sender: TObject);
var
    xAction: TReadPositionsAction;
begin
    if not ThrMan.SamThreadRunning(false) then
    begin
        gErrorManager.LayouterResetGlobalErr;
        xAction := TReadPositionsAction.Create(fMoveControls, self.GetTipUsed());
        gmCreateBasicActionHandlerThread(xAction, []);
    end;
end;

class procedure TfrmArmMoveControl.ReadPositionsForControls(const aControls: TArray<TMoveControl>;
    const aTipUsed: integer);
var
    x: integer;
    xMoveControl: TMoveControl;
begin
    for x := 0 to high(aControls) do
    begin
        xMoveControl := aControls[x];
        if not Assigned(xMoveControl) then
            CONTINUE;
        xMoveControl.SetTipIndex(aTipUsed - 1);
        xMoveControl.ReadPosition();
    end;
end;

class procedure TfrmArmMoveControl.GetPositionsForControls(const aControls: TArray<TMoveControl>;
    const aTipUsed: integer);
var
    x: integer;
    xMoveControl: TMoveControl;
begin
    for x := 0 to high(aControls) do
    begin
        xMoveControl := aControls[x];
        if not Assigned(xMoveControl) then
            CONTINUE;
        xMoveControl.SetTipIndex(aTipUsed - 1);
        xMoveControl.GetPosition();
    end;
end;

procedure TfrmArmMoveControl.ReadPositions;
begin
    ReadPositionsForControls(fMoveControls, GetTipUsed());
end;

procedure TfrmArmMoveControl.GetPositions();
begin
    GetPositionsForControls(fMoveControls, GetTipUsed());
end;

procedure TfrmArmMoveControl.UpdateMoveMode;
var
    x: integer;
    xMoveControl: TMoveControl;
begin
    for x := 0 to high(fMoveControls) do
    begin
        xMoveControl := fMoveControls[x];
        if not Assigned(xMoveControl) then
            CONTINUE;
        xMoveControl.MoveMode := self.rdoMoveMode.ItemIndex;
    end;
end;

procedure TfrmArmMoveControl.SetPosition(aControl: TMoveControl; aDirection: integer);
var
    xAction: TSetPositionMoveCtrlAction;
begin
    aControl.SetTipIndex(GetTipUsed() - 1);
    if not ThrMan.SamThreadRunning(true) then
    begin
        aControl.EnableButtons(false);
        xAction := TSetPositionMoveCtrlAction.Create(aControl, self.cbSafety.Checked, aDirection);
        gmCreateBasicActionHandlerThread(xAction, []);
    end;
end;

procedure TfrmArmMoveControl.DoOnSafetyMove(aSender: TObject);
begin
    // if all motors commented out
    if not Assigned(fCurrentArm) then
        EXIT;

    gmCombinedArmsMoveToZTravel(fCurrentArm);
    gmMoveToZTravel(fCurrentArm);
end;

procedure TfrmArmMoveControl.InitSingle(aControl: TMoveControl);
var
    xAction: TSingleInitMoveCtrlAction;
begin
    if not ThrMan.SamThreadRunning(true) then
    begin
        xAction := TSingleInitMoveCtrlAction.Create(aControl);
        gmCreateBasicActionHandlerThread(xAction, []);
    end;
    // aControl.Init();
end;

procedure TfrmArmMoveControl.BtnMoveClick(Sender: TObject);
var
    x: integer;
begin
    gErrorManager.LayouterResetGlobalErr;

    for x := 0 to high(fMoveControls) do
    begin
        if not Assigned(fMoveControls[x]) then
            CONTINUE;
        if (Sender = fMoveControls[x].PlusBtn) then
        begin
            SetPosition(fMoveControls[x], 1);
            EXIT;
        end;

        if (Sender = fMoveControls[x].MinusBtn) then
        begin
            SetPosition(fMoveControls[x], -1);
            EXIT;
        end;
    end;
end;

procedure TfrmArmMoveControl.KeyMoveMotor(var Key: Word; aMotor: TMoveControl; aDirection: integer);
begin
    gErrorManager.LayouterResetGlobalErr;
    SetPosition(aMotor, aDirection);
    Key := 0;
end;

procedure TfrmArmMoveControl.KeyHandling(var Key: Word; Shift: TShiftState);
begin
    // neues Schema
    if (Shift = [ssCtrl]) then
    begin
        if (Key = 100) then
            KeyMoveMotor(Key, fMoveControls[0], -1); // Ctrl 4
        if (Key = 102) then
            KeyMoveMotor(Key, fMoveControls[0], 1); // Ctrl 6
        if (Key = 104) then
            KeyMoveMotor(Key, fMoveControls[1], -1); // Ctrl 8
        if (Key = 98) then
            KeyMoveMotor(Key, fMoveControls[1], 1); // Ctrl 2
        if (Key = 105) then
            KeyMoveMotor(Key, fMoveControls[2], -1); // Ctrl 9
        if (Key = 99) then
            KeyMoveMotor(Key, fMoveControls[2], 1); // Ctrl 3
        if (Key = 111) then
            KeyMoveMotor(Key, fMoveControls[3], 1); // Ctrl /
        if (Key = 106) then
            KeyMoveMotor(Key, fMoveControls[3], -1); // Ctrl *
        if (Key = 109) then
            KeyMoveMotor(Key, fMoveControls[4], -1); // Ctrl -
        if (Key = 107) then
            KeyMoveMotor(Key, fMoveControls[4], 1); // Ctrl +
        if (Key = 103) then
            KeyMoveMotor(Key, fMoveControls[5], -1); // Ctrl 7
        if (Key = 97) then
            KeyMoveMotor(Key, fMoveControls[5], 1); // Ctrl 1
    end;

    // Zinsser-Schema
    if (cbKeys.Checked) then
    begin
        if (Key = 37) then
            KeyMoveMotor(Key, fMoveControls[0], -1); // Left
        if (Key = 39) then
            KeyMoveMotor(Key, fMoveControls[0], 1); // Right
        if (Key = 38) then
            KeyMoveMotor(Key, fMoveControls[1], -1); // Up
        if (Key = 40) then
            KeyMoveMotor(Key, fMoveControls[1], 1); // Down
        if (Key = 33) then
            KeyMoveMotor(Key, fMoveControls[2], -1); // PageUp
        if (Key = 34) then
            KeyMoveMotor(Key, fMoveControls[2], 1); // PageDown
        if (Key = 36) then
            KeyMoveMotor(Key, fMoveControls[3], 1); // Pos1
        if (Key = 35) then
            KeyMoveMotor(Key, fMoveControls[3], -1); // Ende
        if (Key = 45) then
            KeyMoveMotor(Key, fMoveControls[4], -1); // Einfügen
        if (Key = 46) then
            KeyMoveMotor(Key, fMoveControls[4], 1); // Entfernen
    end;
end;

procedure TfrmArmMoveControl.edPosDblClick(Sender: TObject);
var
    x: integer;
begin
    gErrorManager.LayouterResetGlobalErr;

    for x := 0 to high(fMoveControls) do
    begin
        if not Assigned(fMoveControls[x]) then
            CONTINUE;
        if (Sender = fMoveControls[x].PosDisplay) then
            SetPosition(fMoveControls[x], 0);
    end;
end;

procedure TfrmArmMoveControl.edPosKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = 13) then
        edPosDblClick(Sender);
end;

procedure TfrmArmMoveControl.edStepOnChange(Sender: TObject);
var
    x: integer;
begin
    for x := 0 to high(fMoveControls) do
    begin
        if not Assigned(fMoveControls[x]) then
            CONTINUE;
        if (Sender = fMoveControls[x].StepDisplay) then
            fMoveControls[x].ChangeDefinedIncrement();
    end;
end;

class function TfrmArmMoveControl.FindWorkspaceForArm(aLayout: TLayout; const aArmDeviceName: string)
    : TWorkspace;
var
    x: integer;
    xWorkspace: TWorkspace;
begin
    result := nil;
    for x := 0 to aLayout.Workspaces.Count - 1 do
    begin
        xWorkspace := aLayout.Workspaces[x];
        if xWorkspace.UseAllDevices or (xWorkspace.DeviceNames.IndexOf(aArmDeviceName) >= 0) then
        begin
            result := xWorkspace;
        end;
    end;
end;

procedure TfrmArmMoveControl.DetermineDirectionButtons(const aMType: TArmMotorType;
    out oPlusBtn, oMinusBtn: TSpeedButton);
var
    xWorkspace: TWorkspace;
begin
    xWorkspace := FindWorkspaceForArm(TLayoutManager.Instance.CurrentLayout, self.CurrentArm.Name);
    case aMType of
        amtXMotor:
            begin
                oPlusBtn := XRight;
                oMinusBtn := XLeft;
            end;
        amtYMotor:
            begin
                oPlusBtn := YUp;
                oMinusBtn := YDown;
                if xWorkspace.CurrentViewCoordSystem.ReflectY then
                begin
                    oPlusBtn := YDown;
                    oMinusBtn := YUp;
                end;
            end;
        amtZMotor:
            begin
                oPlusBtn := ZStepUp;
                oMinusBtn := ZStepDown;
            end;
        amtGMotor:
            begin
                oPlusBtn := GStepOpen;
                oMinusBtn := GStepClose;
            end;
        amtVMotor:
            begin
                oPlusBtn := VStepOpen;
                oMinusBtn := VStepClose;
            end;
        amtRMotor:
            begin
                oPlusBtn := RStepRight;
                oMinusBtn := RStepLeft;
            end;
    end;
end;

procedure TfrmArmMoveControl.SetMotorDisplay(aIndex: integer; aMType: TArmMotorType;
    aPosDisplay, aSumPosDisplay, aStepDisplay: TEdit; aInitBtn: TButton; aUnitsLabel: TLabel);
var
    xMoveControl: TMoveControl;
    xMotionDevice: IMotorBasedMotionDevice;
    xPlusBtn, xMinusBtn: TSpeedButton;

    function GetMotionDevice(out oDev: IMotorBasedMotionDevice): boolean;
    begin
        result := Supports(fCurrentArm.MotionDevice, IMotorBasedMotionDevice, oDev);
    end;

begin
    xMoveControl := nil;

    case aMType of
        amtXMotor:
            if GetMotionDevice(xMotionDevice) and Assigned(xMotionDevice.XMotor) then
            begin
                xMoveControl := TXMotorMoveControl.Create(xMotionDevice.XMotor);
                xMoveControl.OnSafetyMove := self.DoOnSafetyMove;
            end;
        amtYMotor:
            if GetMotionDevice(xMotionDevice) and Assigned(xMotionDevice.YMotors) then
            begin
                xMoveControl := TYMotorMoveControl.Create(xMotionDevice.YMotors);
                xMoveControl.OnSafetyMove := self.DoOnSafetyMove;
            end;
        amtZMotor:
            if GetMotionDevice(xMotionDevice) and Assigned(xMotionDevice.ZMotors) then
            begin
                xMoveControl := TZMotorMoveControl.Create(xMotionDevice.ZMotors);
            end;
        amtRMotor:
            if GetMotionDevice(xMotionDevice) and Assigned(xMotionDevice.RMotor) then
            begin
                xMoveControl := TRMotorMoveControl.Create(xMotionDevice.RMotor);
            end;
        amtGMotor:
            if Assigned(fCurrentArm.GripDevice) then
            begin
                xMoveControl := TGripMoveControl.Create(fCurrentArm.GripDevice);
            end;
        amtVMotor:
            if GetMotionDevice(xMotionDevice) and Assigned(xMotionDevice.YMotors) and
                Assigned(xMotionDevice.YMotors.VMotor) then
            begin
                xMoveControl := TVMotorMoveControl.Create(xMotionDevice.YMotors);
                xMoveControl.OnSafetyMove := self.DoOnSafetyMove;
            end;
    end;

    fMoveControls[aIndex] := xMoveControl;
    if not Assigned(xMoveControl) then
        EXIT;

    DetermineDirectionButtons(aMType, xPlusBtn, xMinusBtn);

    fMoveControls[aIndex].PosDisplay := aPosDisplay;
    fMoveControls[aIndex].SumPosDisplay := aSumPosDisplay;
    fMoveControls[aIndex].StepDisplay := aStepDisplay;
    fMoveControls[aIndex].PlusBtn := xPlusBtn;
    fMoveControls[aIndex].MinusBtn := xMinusBtn;
    fMoveControls[aIndex].InitBtn := aInitBtn;
    fMoveControls[aIndex].UnitsLabel := aUnitsLabel;
end;

function TfrmArmMoveControl.MotorExists(aMType: TArmMotorType): boolean;
var
    xDev: TMoveControl;
begin
    result := false;
    // if all motors commented out
    if not Assigned(fCurrentArm) then
        EXIT;
    xDev := GetMoveControl(aMType);
    result := Assigned(xDev);
end;

function TfrmArmMoveControl.GetMoveControl(aMType: TArmMotorType): TMoveControl;
var
    xIndex: integer;
begin
    xIndex := -1;
    case aMType of
        amtXMotor:
            xIndex := 0;
        amtYMotor:
            xIndex := 1;
        amtZMotor:
            xIndex := 2;
        amtRMotor:
            xIndex := 4;
        amtVMotor:
            xIndex := 5;
    end;
    ASSERT(xIndex >= 0);

    result := fMoveControls[xIndex];
end;

procedure TfrmArmMoveControl.cbKeysClick(Sender: TObject);
begin
    Label8.Visible := cbKeys.Checked;
    Label4.Visible := cbKeys.Checked;
    Label5.Visible := cbKeys.Checked;
    Label19.Visible := cbKeys.Checked;
    Label20.Visible := cbKeys.Checked;
    Label15.Visible := cbKeys.Checked;
    Label17.Visible := cbKeys.Checked;
end;

procedure TfrmArmMoveControl.SelectArmTip(aTipIndex: integer);
var
    xTips: TIPMAP;
begin
    xTips := 0;
    gmSelectTip(xTips, aTipIndex);
    fCurrentArm.PipDevice.SetUseTips('', xTips, false);
end;

procedure TfrmArmMoveControl.TipUsedChanged();
begin
    if not Assigned(fCurrentArm.PipDevice) then
        EXIT;
    if Assigned(TLayoutManager.Instance.CurrentLayout) then
    begin
        if spinTipUsed.Value < spinTipUsed.MinValue then
        begin
            spinTipUsed.Value := spinTipUsed.MinValue;
            EXIT;
        end;
        if spinTipUsed.Value > spinTipUsed.MaxValue then
        begin
            spinTipUsed.Value := spinTipUsed.MaxValue;
            EXIT;
        end;
        SelectArmTip(spinTipUsed.Value - 1);
    end;
end;

procedure TfrmArmMoveControl.spinTipUsedChange(Sender: TObject);
begin
    gErrorManager.LayouterResetGlobalErr;
    TipUsedChanged();
    GetPositions;
end;

procedure TfrmArmMoveControl.EnableZControls();
begin
    grpZ.Visible := MotorExists(amtZMotor);

    btnSingleZTravel.Visible := grpZ.Visible;
end;

procedure TfrmArmMoveControl.EnableXYControls();
var
    xXMotorExists, xYMotorExists: boolean;
begin
    xXMotorExists := MotorExists(amtXMotor);
    xYMotorExists := MotorExists(amtYMotor);

    grpXY.Visible := xXMotorExists or xYMotorExists;

    if not grpXY.Visible then
        EXIT;

    XRight.Visible := xXMotorExists;
    XLeft.Visible := xXMotorExists;

    YUp.Visible := xYMotorExists;
    YDown.Visible := xYMotorExists;
end;

procedure TfrmArmMoveControl.EnableTipControls();
begin
    grpCurrentTip.Visible := Assigned(fCurrentArm) and (Assigned(fCurrentArm.PipDevice)) and
        (MotorExists(amtYMotor) or MotorExists(amtZMotor));
    if not grpCurrentTip.Visible then
        EXIT;

    spinTipUsed.MinValue := 1;
    spinTipUsed.MaxValue := fCurrentArm.PipDevice.TipCount;
    spinTipUsed.Value := fCurrentArm.PipDevice.FirstUsedTipIndex + 1;
    if (spinTipUsed.Value < spinTipUsed.MinValue) then
        spinTipUsed.Value := spinTipUsed.MinValue;
    TipUsedChanged();
end;

procedure TfrmArmMoveControl.EnableVControls;
var
    xVMotorExists: boolean;
begin
    // Varispan-Controls
    xVMotorExists := MotorExists(amtVMotor);
    btnInitV.Visible := xVMotorExists;
    VStep.Visible := xVMotorExists;
    edVPos.Visible := xVMotorExists;
    lblVUnits.Visible := xVMotorExists;
    VStepClose.Visible := xVMotorExists;
    VStepOpen.Visible := xVMotorExists;
    Label1.Visible := xVMotorExists;
    edYVPos.Visible := xVMotorExists;
    lblYVUnits.Visible := xVMotorExists;

    if xVMotorExists then
    begin
        edYVPos.Left := 94;
        lblYVUnits.Left := 94;

        grpXY.Width := 298;

        edYPos.Left := 166;
        lblYUnits.Left := 166;
        YStep.Left := 166;
        btnInitY.Left := 166;
    end
    else
    begin
        edYVPos.Left := 166;
        lblYVUnits.Left := 166;
        grpXY.Width := 154;

        edYPos.Left := 94;
        lblYUnits.Left := 94;
        YStep.Left := 94;
        btnInitY.Left := 94;
    end;
end;

procedure TfrmArmMoveControl.EnableControls();
begin
    EnableTipControls();
    EnableXYControls();
    EnableZControls();
    grpGrip.Visible := Assigned(fCurrentArm) and Assigned(fCurrentArm.GripDevice);
    grpRotation.Visible := MotorExists(amtRMotor);
    EnableVControls;
end;

procedure TfrmArmMoveControl.SetTipUsed(aValue: integer);
begin
    gErrorManager.LayouterResetGlobalErr;

    spinTipUsed.Value := aValue;
    GetPositions;
end;

function TfrmArmMoveControl.GetTipUsed: integer;
begin
    // if all y and z motors are commented out
    if not grpCurrentTip.Visible then
        result := 1
    else
        result := spinTipUsed.Value;
end;

procedure TfrmArmMoveControl.InitControls(aCurrentArm: IArmDevice);
var
    xUseTips: TIPMAP;

begin
    fCurrentArm := aCurrentArm;
    self.Caption := fCurrentArm.Name;
    self.Panel1.Color := fCurrentArm.Color;

    SetLength(self.fMoveControls, 6); // für maximal 6 Motoren
    SetMotorDisplay(0, amtXMotor, edXPos, nil, XStep, btnInitX, lblXUnits);
    SetMotorDisplay(1, amtYMotor, edYPos, edYVPos, YStep, btnInitY, lblYUnits);
    SetMotorDisplay(2, amtZMotor, edZPos, nil, ZStep, btnInitZ, lblZUnits);
    SetMotorDisplay(3, amtGMotor, edGPos, nil, GStep, btnInitG, lblGUnits);
    SetMotorDisplay(4, amtRMotor, edRPos, nil, RStep, btnInitR, lblRUnits);
    SetMotorDisplay(5, amtVMotor, edVPos, edYVPos, VStep, btnInitV, lblVUnits);

    // Select first tip
    xUseTips := gmEmptyTipMap();
    gmSelectTip(xUseTips, 0);
    if Assigned(fCurrentArm.PipDevice) then
        fCurrentArm.PipDevice.SetUseTips('', xUseTips, false);

    EnableControls();
    TipUsedChanged();
    ReadPositions();

    cbKeysClick(nil);
end;

procedure TfrmArmMoveControl.btnSingleZTravelClick(Sender: TObject);
var
    xAction: TMoveToZTravelAction;
begin
    if ThrMan.SamThreadRunning(false) then
        EXIT;
    gErrorManager.LayouterResetGlobalErr;

    xAction := TMoveToZTravelAction.Create(fCurrentArm);
    xAction.OnEndAction := self.OnEndAction;
    gmCreateBasicActionHandlerThread(xAction, []);
end;

procedure TfrmArmMoveControl.btnInitSingleClick(Sender: TObject);
var
    x: integer;
begin
    gErrorManager.LayouterResetGlobalErr;

    for x := 0 to high(fMoveControls) do
    begin
        if not Assigned(fMoveControls[x]) then
            CONTINUE;
        if (Sender = fMoveControls[x].InitBtn) then
        begin
            InitSingle(fMoveControls[x]);
            EXIT;
        end;
    end;
end;

procedure TfrmArmMoveControl.rdoMoveModeClick(Sender: TObject);
begin
    UpdateMoveMode();
    self.GetPositions();
end;

function TfrmArmMoveControl.GetCurrentX: TPosMM;
var
    xMoveControl: TMoveControl;
begin
    xMoveControl := GetMoveControl(amtXMotor);
    ASSERT(Assigned(xMoveControl));
    result := xMoveControl.CurrentPos;
end;

function TfrmArmMoveControl.GetCurrentY: TPosMM;
var
    xMoveControl: TMoveControl;
begin
    xMoveControl := GetMoveControl(amtYMotor);
    ASSERT(Assigned(xMoveControl));
    result := xMoveControl.CurrentPos;
end;

procedure TfrmArmMoveControl.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caFree;
end;

procedure TfrmArmMoveControl.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
end;


end.
