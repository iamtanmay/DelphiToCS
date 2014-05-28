unit LayEvalu;
{ --------------------------------------------------------------------------------------------------
  LISSY LAYOUTER
  --------------------------------------------------------------------------------------------------
  Evaluieren einer Carrier-Position in X und Y
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  24.02.00 wl                         alle Movement-Buttons entfernt --> fraEvaluate
  Tube Position als Referenzpunkt möglich
  ACHTUNG: Varispan-Wert wird zwar richtig angezeigt, beim Update aber noch nicht berücksichtigt
  02.03.00 wl  SetTubeRef             springt mit SetFocus zurück ins Fenster
  22.03.00 mo  TfrmEvalCarr.FormShow + FormClose   Varispan buttons unsichtbar
  16.05.00 wl  X-,YUpdateBtnClick     Fehler beseitigt, der das Rack nach Update immer zu weit geschoben hat
  16.05.00 wl  neuer Aufruf für SetWB..-Befehle im Carrier-Objekt
  24.05.00 wl  X-,YUpdateBtnClick     da Carrier nicht mehr Parent der Racks ist, müssen alle Rackpositionen neu bestimmt werden
  26.05.00 wl                         uses geändert
  14.10.01 mo                         TN1067 Merge mit SIAS Änderungen
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  12.12.02 wl                         TN1345 geänderte Samintf-Aufrufe, geänderte CommonTypes, Robot. statt Sam.
  27.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  02.06.03 wl  FormShow, FormClose    TN1485.4 an geändertes MoveCtrl angepasst
  03.06.03 wl  FormKeyUp              TN1485.4 KeyPreview = true / bei Tastendruck wird Funktion aufgerufen
  03.07.03 wl                         TN1501   Compilerhinweise korrigiert
  12.09.03 wl  X-,YUpdateBtnClick     TN1581.4 DefineRack-Aufruf durch Änderungen an LObjEdit überflüssig
  10.12.03 wl  ReferenceClick         TN1672   CalculatePipSteps mit Y-Array statt mit Varispan
  16.12.03 wl  Button1Click           TN1672   MoveToXY mit Y-Array statt mit Varispan
  25.02.04 pk                         TN1763   btnOk sets modalresul=mrOk and btnCancel sets modalresult=mrCancel
  05.04.04 wl                         TN1788   Robot-Methoden durch gPipArm-Funktionen ersetzt
  19.04.04 wl  ReferenceClick         TN1788   TipIntegerArray statt TipWordArray
  09.07.04 wl  ReferenceClick         TN2010   für ZP02 und Tip > Tip1 werden jetzt sinnvolle Werte berechnet
  09.07.04 wl                         TN2010   Label für V entfernt, da V-Motor niemals benutzt wurde/wird
  10.07.04 wl  ReferenceClick         TN0972   Handler reference can be used for Rack positions
  10.07.04 wl  XUpdateBtnClick        TN0972   Handler reference works correctly with ZP02 (X-Motor reverse)
  10.07.04 wl  ReferenceClick         TN0972   The values 'H_XStart_mm' and 'H_YStart_mm' are not used for position calculation any more.
  10.07.04 wl  ReferenceClick         TN0972   if 'DistX_HandlerEnd_Tip1_mm' is not defined, 'DistX_H_Tip1' will be taken for calculation
  10.07.04 wl  FormShow               TN0972   Handler refence is always enabled (if a handler arm exists).
  10.07.04 wl                         TN1963   Anpassung an Änderungen von TRoboticInterface
  02.09.04 wl  btnMoveToRefPointClick TN2117   Bevor der Tip-Arm in XY bewgt wird, wird bei ZP01 erst der Greifer hochgefahren (Combined arm)
  02.09.04 wl  btnMoveToRefPointClick TN2117   Handler Reference: Bevor der Handler in XY bewgt wird, werden erst die Tips des Combined Arm hoch- und der Greifer zusammengefahren
  15.06.05 pk  GetChosenTipNo         TN2464   Use GetCurrentArm insted of gPipArm
  21.06.05 pk  MASSIVE CHANGES        TN2464   The RefHandler.Checked box was removed - Use the World Steps from the grip arm as a reference
  09.11.05 wl  btnMoveToRefPointClick TN2728   Neuer Parameter bei Motors.MoveHXY
  15.11.05 pk                         TN2742   React differently depending on if  motor are assigned
  14.02.07 wl  btnMoveToRefPointClick TN3147   geänderter Aufruf von gmCalculateXYStepBySinglePos, alles in try-finally-Blöcken
  12.11.07 pk                         TN3924   Steps changed to mm
  15.05.08 ts  X-,YUpdateBtnClick     TN4100   TipOffset bei Update mitberechnet
  15.05.08 wl  YUpdateBtnClick        TN4100   benutzt neue Funktion TMultiYStep.GetYPosWithoutVarispan
  20.06.08 pk                         TN4139    WB global object replaced by LayoutManager
  07.07.08 pk                         TN4139   various changes
  11.07.08 wl                         TN4164   Arm-Movement-Panel geändert (noch nicht fertig!!)
  20.09.08 wl                         TN4224   Funktioniert jetzt wieder
  10.08.09 wl                         TN4702   Strings werden jetzt direkt geladen
  12.08.09 pk  X-,YUpdateBtnClick     TN4714   Call SetWorldX, SetWorldY instead of Set_WB_X, Set_WB_Y
  21.08.09 wl  fStringLoader          TN4702   fStringLoader lädt Strings für Dialog-Elemente
  17.02.10 ts  EvaluateCarrier        TN4981   FOld_X/Y beinhalten Werte aus LAYOUT-Tabelle anstatt berechneter Werte
  13.04.10 wl                         TN5044   uses geändert
  23.04.10 wl  TCarrierEvaluationWithMachine  TN5070   neu: es könnte jetzt auch ein Evaluation-Fenster ohne Maschine geben
  30.04.10 wl                         TN5070   bei invisible Carrier wird der Rackname angezeigt
  20.05.10 wl                         TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  07.06.10 wl                         TN5116   Layout editieren vorbereiten
  11.06.10 pk  MoveToRefPointClick    TN5138   GetPositions changed to ReadPositions
  23.03.11 wl  MoveToRefPointClick    TN5515   CombinedArmsMoveToZTravel wird jetzt explizit aufgerufen
  21.02.12 wl                         TN5777   mit TMoveControl.RoundUnits wird auf 3 Nachkommastellen gerundet
  23.05.13 wl  MoveToRefPointClick    TN6153   verwendet neue Funktion MoveXYSimple (Ohne InternRackMovement)
  21.08.13 wl  IsPosReachableBySinglePos   TN6231   von SamHigh hierher
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Windows,
    Messages,
    SysUtils,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    Buttons,
    ExtCtrls,
    Math,
    IntfArmDevice,
    Rack,
    IntfMotorDevice,
    AppTypes,
    CommonTypes,
    Carrier,
    StringLoader,
    LayoutManager,
    EditingLayoutElements;

type
    TEvaluateCarrierStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmEvalCarr = class(TForm)
        Panel1: TPanel;
        XEdit: TEdit;
        yEdit: TEdit;
        yUpdateBtn: TButton;
        XUpdateBtn: TButton;
        RefPanel: TPanel;
        ref1: TRadioButton;
        ref2: TRadioButton;
        ref4: TRadioButton;
        ref3: TRadioButton;
        Label15: TLabel;
        mm: TLabel;
        refTube: TRadioButton;
        Bevel1: TBevel;
        Panel3: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        Label1: TLabel;
        Label2: TLabel;
        Edit1: TEdit;
        Label4: TLabel;
        Label5: TLabel;
        Edit2: TEdit;
        Label6: TLabel;
        btnMoveToRefPoint: TButton;
        Label7: TLabel;
        Label8: TLabel;
        Edit4: TEdit;
        Label9: TLabel;
        Edit5: TEdit;
        lblTube: TLabel;
        procedure FormCreate(Sender: TObject);
        procedure yUpdateBtnClick(Sender: TObject);
        procedure XUpdateBtnClick(Sender: TObject);
        procedure btnMoveToRefPointClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormShow(Sender: TObject);
        procedure btnCancelClick(Sender: TObject);
        procedure btnOKClick(Sender: TObject);
        procedure ReferenceClick(Sender: TObject);
        procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormDestroy(Sender: TObject);

    private
        FOld_X, FOld_Y, FNew_X, FNew_Y: TPosMM;
        FRef_X, FRef_Y: TPosMM;
        FRackHdl: TRack;
        FRackPos: integer;
        // FPanelVarVisible: boolean;
        fStringLoader: TEvaluateCarrierStringLoader;

        fCarrier: TCarrier;
        function IsMovementPossible: boolean;
        function GetArmInfo(out oArm: IArmDevice; out oTipIndex: integer): boolean;
        procedure ReferenceChanged();
        class function IsPosReachableBySinglePos(aUsedArm: IArmDevice; aTipIndex: integer;
            aX, aY: TPosMM): boolean;
    public
        procedure EvaluateCarrier(aCarrier: TCarrier; aRack: TRack);
        property New_X: TPosMM read FNew_X;
        property New_Y: TPosMM read FNew_Y;
        property Old_X: TPosMM read FOld_X;
        property Old_Y: TPosMM read FOld_Y;
        procedure UpdateRef;
        procedure SetTubeRef(RackHdl: TRack; P: integer);
    end;

    TCarrierEvaluationWithMachine = class(TCarrierEvaluation)
    private
        fEvaluateForm: TfrmEvalCarr;
    public
        procedure EvaluationStart(aCarrier: TCarrier; aRack: TRack); override;
        function EvaluationEndXYChanged(out oNewX, oNewY, oOldX, oOldY: TPosMM): boolean; override;
        procedure EvaluationSetTubeRef(aRack: TRack; aPosition: integer); override;
        procedure EvaluationReferenceChanged(); override;
    end;


implementation


{$R *.DFM}

uses
    UtilLib,
    GeneralTypes,
    MatrixMath,
    ControlUtils,
    ErrorManager,
    Utility2,
    LayMain,
    ArmMoveControl,
    GeometricClasses,
    OperationFactory,
    OperationAxisMove,
    MotorStepCalculator,
    SamHigh,
    GUIManager,
    CarrierDataAdaptor,
    MoveControl;

{ TEvaluateCarrierStringLoader }

procedure TEvaluateCarrierStringLoader.AddAllItems;
begin
    AddSingle(510, '&OK', '&OK');
    AddSingle(520, '&Cancel', '&Abbrechen');
    AddSingle(53040, 'Reference points out of limit', 'Referenzpunkt außerhalb des Pipettierbereichs');
    AddSingle(53060, 'Lissy Movement', 'Lissy Steuerung');
    AddSingle(53070, 'Saved Values:', 'Gespeicherte Werte:');
    AddSingle(53090, 'Update X', 'X aktualisieren');
    AddSingle(53100, 'Update Y', 'Y aktualisieren');
    AddSingle(53170, 'Reference Point:', 'Bezugspunkt:');
    AddSingle(53180, '&Move To Reference Point', '&Zum Bezugspunkt bewegen');
    AddSingle(53520, 'Click on the Rack position you want to use as reference point',
        'Klicken Sie auf die Rack-Position, die als Referenz dienen soll');
    AddSingle(53540, 'Current Values:', 'Aktuelle Werte:');
    AddSingle(53550, 'Left rear edge', 'Links hinten');
    AddSingle(53560, 'Right rear edge', 'Rechte hintere Ecke');
    AddSingle(53570, 'Left front edge', 'Links vorne');
    AddSingle(53580, 'Right front edge', 'Rechte vordere Ecke');
    AddSingle(53590, 'Rack position', 'Rackposition');
end;

{ TfrmEvalCarr }

procedure TfrmEvalCarr.EvaluateCarrier(aCarrier: TCarrier; aRack: TRack);
var
    xCarrierPos: TPoint4d;
begin
    fCarrier := aCarrier;
    if (fCarrier.TypeName = TCarrierDataAdaptor.InvisibleCarrierName) then
    begin
        self.Caption := TLanguageString.Read('Rack {0}: Evaluate Position', 'Rack {0}: Position überprüfen',
            [aRack.Name]);
    end
    else
    begin
        self.Caption := TLanguageString.Read('Carrier {0}: Evaluate Position',
            'Carrier {0}: Position überprüfen', [fCarrier.Name]);
    end;

    self.Visible := true;
    self.Panel1.Visible := true;

    xCarrierPos := fCarrier.CalcCarrierPosition(); // fCarrier.CreateCarrierPosition();

    FOld_X := TMoveControl.RoundUnits(fCarrier.PosX);
    // X-,Y-Position aus LAYOUT-Tabelle als alte Position speichern, da
    FOld_Y := TMoveControl.RoundUnits(fCarrier.PosY);
    // bei CalcCarrierPosition auch Offsets einbezogen werden und gleiche Carrier in unterschiedlichen Layouts beim Speichern nicht gefunden werden
    FNew_X := TMoveControl.RoundUnits(xCarrierPos.x);
    FNew_Y := TMoveControl.RoundUnits(xCarrierPos.y);
    xEdit.text := FloatToStr(FNew_X);
    yEdit.text := FloatToStr(FNew_Y);

    // We will get the arm info in ReferenceClick
    ReferenceChanged();

end;

procedure TfrmEvalCarr.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TEvaluateCarrierStringLoader.Create;
    fStringLoader.LoadLanguage(self);
end;

procedure TfrmEvalCarr.FormDestroy(Sender: TObject);
begin
    fStringLoader.Free;
end;

function TfrmEvalCarr.GetArmInfo(out oArm: IArmDevice; out oTipIndex: integer): boolean;
var
    xMoveControl: TfrmArmMoveControl;
begin
    result := false;
    oArm := nil;
    oTipIndex := -1;
    xMoveControl := Main.CurrentArmPanel;
    if not Assigned(xMoveControl) then
        EXIT;

    oArm := xMoveControl.CurrentArm;
    oTipIndex := xMoveControl.TipUsed - 1;
    result := Assigned(oArm) and (oTipIndex >= 0);
    self.btnMoveToRefPoint.Enabled := true; // Assigned( fArmMotorX ) or Assigned( fArmMotorY );
end;

procedure TfrmEvalCarr.FormShow(Sender: TObject);
begin
    // FPanelVarVisible := Main.CurrentArmPanel.grpGrip.Visible; // save value
    // Main.CurrentArmPanel.grpGrip.Visible := false;
    if gErrorManager.IsGlobalErr() then
    begin
        gGUIManager.MessageBox(TLanguageString.Read('Please init before!', 'Bitte vorher initialisieren!'),
            TLanguageString.Read('Start Evaluation', 'Evaluierung beginnen'), 0);
        exit;
    end;

end;

procedure TfrmEvalCarr.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    (fCarrier as TSetupCarrier).EvaluationEnd;
    // Main.CurrentArmPanel.grpGrip.Visible := FPanelVarVisible;
    Action := caFree;
end;

procedure TfrmEvalCarr.XUpdateBtnClick(Sender: TObject);
var
    NewX, XDif, xRef_X_withOffset: TPosMM;
    xXYStep: TXYStep;
    xArm: IArmDevice;
    xTipIndex: integer;
begin
    if not GetArmInfo(xArm, xTipIndex) then
    begin
        ShowMessage('Used Arm must be selected');
        EXIT;
    end;

    xXYStep := gmCalculateXYStepBySinglePos(xArm, xTipIndex, fRef_X, fRef_Y);
    xRef_X_withOffset := xXYStep.X;

    // ---------------------------------------------------------------------------- Differenz ermitteln
    // XDif := 0;
    NewX := Main.CurrentArmPanel.GetCurrentX();
    XDif := TMoveControl.RoundUnits(NewX - xRef_X_withOffset);

    // ------------------------------------------------------------------------------ Alle Werte ändern
    FRef_X := TMoveControl.RoundUnits(FRef_X + XDif);
    Edit1.Text := FloatToStr(FRef_X);

    XEdit.Text := FloatToStr(TValueConverter.StrToFloatTryBoth(XEdit.text) + XDif);

    // xLayoutPos := fCarrier.WorldPosToCarrierPos( MakePoint4d( TValueConverter.StrToFloatTryBoth( XEdit.text ), 0, 0 ) );

    (fCarrier as TSetupCarrier).SetWorldX(TValueConverter.StrToFloatTryBoth(XEdit.text));
    { TODO -opk -cWB : }
    // (fCarrier as TSetupCarrier).SetCarrPosition;
    if XDif <> 0 then
    begin
        (TLayoutManager.Instance.CurrentLayout as TSetupLayout).RefreshScene();
        (TLayoutManager.Instance.CurrentLayout as TSetupLayout).NotSaved := true;
    end;
end;

procedure TfrmEvalCarr.yUpdateBtnClick(Sender: TObject);
var
    NewY, YDif, xRef_Y_withOffset: TPosMM;
    xXYStep: TXYStep;
    xArm: IArmDevice;
    xTipIndex: integer;
begin
    if not GetArmInfo(xArm, xTipIndex) then
    begin
        ShowMessage('Used Arm must be selected');
        EXIT;
    end;

    xXYStep := gmCalculateXYStepBySinglePos(xArm, xTipIndex, fRef_X, fRef_Y);
    xRef_Y_withOffset := TMoveControl.RoundUnits(xXYStep.Y.GetYPosWithoutVarispan(xTipIndex));
    // ---------------------------------------------------------------------------- Differenz ermitteln
    // YDif := 0;
    // if TryStrToFloat( Main.fraEvaluate1.edYPos.text, NewY )
    NewY := Main.CurrentArmPanel.GetCurrentY();
    YDif := TMoveControl.RoundUnits(NewY - xRef_Y_withOffset);
    // ------------------------------------------------------------------------------ Alle Werte ändern
    FRef_Y := TMoveControl.RoundUnits(FRef_Y + YDif);
    Edit2.Text := FloatToStr(FRef_Y);
    YEdit.Text := FloatToStr(TValueConverter.StrToFloatTryBoth(YEdit.text) + YDif);
    (fCarrier as TSetupCarrier)
        .SetWorldY(TMoveControl.RoundUnits(TValueConverter.StrToFloatTryBoth(YEdit.text)));
    { TODO -opk -cWB : }
    // (fCarrier as TSetupCarrier).SetCarrPosition;
    if YDif <> 0 then
    begin
        (TLayoutManager.Instance.CurrentLayout as TSetupLayout).RefreshScene();
        (TLayoutManager.Instance.CurrentLayout as TSetupLayout).NotSaved := true;
    end;
end;

class function TfrmEvalCarr.IsPosReachableBySinglePos(aUsedArm: IArmDevice; aTipIndex: integer;
    aX, aY: TPosMM): boolean;
var
    xPipPosCheck: TPipPosCheck;
begin
    result := true;
    xPipPosCheck := TMotorStepCalculatorFactory.CreatePipPosCheck(aUsedArm);
    if not Assigned(xPipPosCheck) then
        EXIT;
    try
        xPipPosCheck.SetXYBySinglePos(aTipIndex, aX, aY);
        result := xPipPosCheck.IsPosReachable(aTipIndex);
    finally
        FreeAndNil(xPipPosCheck);
    end;
end;

function TfrmEvalCarr.IsMovementPossible: boolean;
var
    xArm: IArmDevice;
    xTipIndex: integer;
begin
    result := false;
    if not GetArmInfo(xArm, xTipIndex) then
    begin
        ShowMessage('Used Arm must be selected');
        EXIT;
    end;

    result := IsPosReachableBySinglePos(xArm, xTipIndex, fRef_X, fRef_Y);
end;

procedure TfrmEvalCarr.btnMoveToRefPointClick(Sender: TObject);
var
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xXYStep: TXYStep;
    xArm: IArmDevice;
    xTipIndex: integer;
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT;

    if not GetArmInfo(xArm, xTipIndex) then
    begin
        ShowMessage('Used Arm must be selected');
        EXIT;
    end;

    if (not IsMovementPossible) then
        EXIT;

    gmCombinedArmsMoveToZTravel(xArm);
    gmMoveToZTravel(xArm);

    if Assigned(xArm.GripDevice) then
    begin
        xArm.GripDevice.CloseFull();
    end;

    xXYStep := gmCalculateXYStepBySinglePos(xArm, xTipIndex, fRef_X, fRef_Y);
    try
        xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(xArm);
        try
            xXYOp.MoveXYSimple(xXYStep);
        finally
            xXYOp.Free;
        end;
    finally
        xXYStep.Free;
    end;

    Main.CurrentArmPanel.ReadPositions;

    // if ( fIsHandler ) then begin
    // fArm.MoveToZTravel;    // ZP01: Combined Arm wird auch hochgefahren
    // ( fArm as TGripperArmDevice ).Motors.OpenGripper(0, false);       // Greifer muß geschlossen sein - sonst ändert sich der y-wert
    // ( fArm as TGripperArmDevice ).Motors.MoveHXY( FRef_X, FRef_Y, nil, [] ) // Position mit Greifer anfahren
    // end
    // else begin
    // if (fTipIndex = -1) then begin
    // gmResSyncBox(53620{No tip selected!},0);
    // EXIT;
    // end;
    // fArm.MoveToZTravel;  // ZP01: Combined Arm wird auch hochgefahren
    // fArm.MoveToXY(FRef_X, gmGetYArrayFromYWithVarispan(FRef_Y, 0));
    // end;
    //

end;

procedure TfrmEvalCarr.btnCancelClick(Sender: TObject);
begin
    Close;
end;

procedure TfrmEvalCarr.btnOKClick(Sender: TObject);
begin
    FNew_X := TMoveControl.RoundUnits(TValueConverter.StrToFloatTryBoth(XEdit.text));
    FNew_Y := TMoveControl.RoundUnits(TValueConverter.StrToFloatTryBoth(YEdit.text));
    Close;
end;

procedure TfrmEvalCarr.UpdateRef;
begin
    Edit1.Text := FloatToStr(FRef_X);
    Edit2.Text := FloatToStr(FRef_Y);

    if (not IsMovementPossible) then
    begin
        Label6.Visible := true;
        btnMoveToRefPoint.Enabled := false;
    end
    else
    begin
        Label6.Visible := false;
        btnMoveToRefPoint.Enabled := true;
    end;
end;

procedure TfrmEvalCarr.ReferenceChanged();
var
    xPosition: TGeom3D_Position;
    xArm: IArmDevice;
    xTipIndex: integer;
begin
    if not GetArmInfo(xArm, xTipIndex) then
    begin
        ShowMessage('Used Arm must be selected');
        EXIT;
    end;

    FRef_X := -1;
    FRef_Y := -1;

    if { (not fIsHandler) and } (xTipIndex = -1) then
        EXIT; // Bei PipArm muß ein Tip gewählt sein
    if (refTube.Checked) and (not Assigned(FRackHdl)) then
        EXIT;

    if (refTube.Checked) then
    begin
        refTube.Visible := true;
        lblTube.Caption := TLanguageString.Read('Rack [{0}] Tube [{1}]', 'Rack [{0}] Position [{1}]',
            [FRackHdl.Name, FRackPos]);

        // Tube-Position
        xPosition := FRackHdl.CreateTubePos(FRackPos);
    end
    else
    begin
        refTube.Visible := false;
        lblTube.Caption := TLanguageString.
            Read('Click on the Rack position you want to use as reference point',
            'Klicken Sie auf die Rack-Position, die als Referenz dienen soll');
        FRackHdl := nil;
        FRackPos := 0;

        // Carrier-Corner-Position
        xPosition := (fCarrier as TSetupCarrier).CreateCornerPosition((ref2.Checked) or (ref4.Checked),
            // Right
            (ref3.Checked) or (ref4.Checked)); // Front
    end;

    FRef_X := TMoveControl.RoundUnits(xPosition.X);
    FRef_Y := TMoveControl.RoundUnits(xPosition.Y);
    xPosition.Free;

    // Y-Position des Tips (wenn nicht Tip 1)
    // if not fIsHandler then FRef_Y := FRef_Y - fTipIndex * Robot.YOffset;

    {
      // Bestimmung der Offsets (für Handler-Reference)
      if (refHandler.Checked) then begin
      if (DistX_HandlerEnd_Tip1_mm <> -1) then begin
      // klassisch: Berechnung über DistX_HandlerEnd_Tip1_mm
      xHandlerOfsX := Round(DistX_HandlerEnd_Tip1_mm * XSteps_per_mm);
      xHandlerOfsY := Round(DistY_HandlerEnd_Tip1_mm * YSteps_per_mm);
      end
      else begin
      // neu: wenn DistX_HandlerEnd_Tip1_mm nicht definiert, nimm DistX_H_Tip1
      //xHandlerOfsX := DistX_H_Tip1;
      //xHandlerOfsY := DistY_H_Tip1;
      end;

      FRef_X := FRef_X - xHandlerOfsX;
      if (Robot.IsZP02) then FRef_X := - FRef_X; // ZP02: Anschlag rechts
      FRef_Y := FRef_Y - xHandlerOfsY;
      end;
    }
    UpdateRef;
end;

procedure TfrmEvalCarr.SetTubeRef(RackHdl: TRack; p: integer);
begin
    FRackHdl := RackHdl;
    FRackPos := p;
    refTube.Checked := true;
    ReferenceChanged();
    refTube.SetFocus;
end;

procedure TfrmEvalCarr.ReferenceClick(Sender: TObject);
begin
    ReferenceChanged();
end;

procedure TfrmEvalCarr.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    Main.CurrentArmPanel.KeyHandling(Key, Shift);
end;

{ TCarrierEvaluationWithMachine }

function TCarrierEvaluationWithMachine.EvaluationEndXYChanged(out oNewX, oNewY, oOldX, oOldY: TPosMM)
    : boolean;
begin
    oNewX := fEvaluateForm.New_X;
    oNewY := fEvaluateForm.New_Y;
    oOldX := fEvaluateForm.Old_X;
    oOldY := fEvaluateForm.Old_Y;

    result := (fEvaluateForm.ModalResult <> mrCancel) and ((oNewX <> oOldX) or (oNewY <> oOldY));

    // Free macht das Fenster selbst
    fEvaluateForm := nil;
end;

procedure TCarrierEvaluationWithMachine.EvaluationReferenceChanged;
begin
    fEvaluateForm.ReferenceClick(nil);
end;

procedure TCarrierEvaluationWithMachine.EvaluationSetTubeRef(aRack: TRack; aPosition: integer);
begin
    fEvaluateForm.SetTubeRef(aRack, aPosition);
end;

procedure TCarrierEvaluationWithMachine.EvaluationStart(aCarrier: TCarrier; aRack: TRack);
begin
    gErrorManager.LayouterResetGlobalErr;

    fEvaluateForm := TfrmEvalCarr.Create(nil);
    fEvaluateForm.EvaluateCarrier(aCarrier, aRack);
end;


end.
