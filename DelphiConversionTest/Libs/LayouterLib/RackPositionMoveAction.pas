{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Anfahren der Positionen im Layouter
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  14.01.11 wl                               TN5426   initial version
  23.03.11 wl  ExecFirst                    TN5515   CombinedArmsMoveToZTravel wird jetzt explizit aufgerufen
  20.09.11 wl                               TN5723   an Änderungen angepasst
  28.10.11 wl                               TN5725   an TRack.PaintTubePositions angepasst
  02.02.11 wl  ExecFirst                    TN5791   verwendet TArray<TXRackPosition> statt TRack und Pos-Array
  28.06.12 wl  ExecFirst                    TN5527   Nach der Aktion werden die Positionen (aus dem Speicher) angezeigt
  23.05.13 wl  ExecFirst                    TN6153   verwendet neue Funktion MoveXYSimple (Ohne InternRackMovement)
  ---------------------------------------------------------------------------------------------------------------------- }

unit RackPositionMoveAction;


interface


uses
    IntfArmDevice,
    Rack,
    AppTypes,
    Action;

type
    TRackPositionMoveAction = class(TAction)
    private
        fArmDevice: IArmDevice;
        fTips: TIPMAP;
        fRack: TRack;
        fPosition: integer;
        procedure PositionMovement();
    public
        constructor Create(aUsedArm: IArmDevice; aHRack: TRack; aPos: integer; aUseTips: TIPMAP);
        procedure ExecFirst(); override;
    end;


implementation


uses
    Windows,
    SysUtils,
    CommonTypes,
    ArrayUtils,
    LiquidHandlingDiti,
    ErrorManager,
    TipMapUtils,
    OperationFactory,
    SamHigh,
    RackWell,
    RackTypes,
    GeneralTypes,
    OperationAxisMove,
    MotionSystemTravel,
    MotionSystemPipMove,
    GUIManager,
    IntfMotorDevice,
    LayMain;

{ TMoveToZTravelAction }

constructor TRackPositionMoveAction.Create(aUsedArm: IArmDevice; aHRack: TRack; aPos: integer;
    aUseTips: TIPMAP);
begin
    inherited Create();

    fArmDevice := aUsedArm;
    fRack := aHRack;
    fPosition := aPos;
    fTips := aUseTips;
end;

procedure TRackPositionMoveAction.ExecFirst;
begin
    if fPosition <= 0 then
        exit;

    fRack.PaintTubePos(fPosition, TRackWellDisplayType.Highlight);
    self.PositionMovement;
    fRack.PaintTubePos(fPosition, TRackWellDisplayType.default);

    Main.UpdateArmPositions;
end;

procedure TRackPositionMoveAction.PositionMovement;
var
    xRP: TArray<TXRackPosition>;
    i, iTip: integer;
    xZStep: TPipStepZPos;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xTipMap: TIPMAP;
    xXYStep: TXYStep;
    xCaption: string;
begin
    xCaption := TLanguageString.Read('Check Position', 'Überprüfung der Position');
    if (fTips = 0) then
    begin
        gGUIManager.MessageBox(TLanguageString.Read('Error: No tip selected!',
            'Fehler: Es ist keine Nadel ausgewählt!'), xCaption, 16);
        EXIT;
    end;

    if (fArmDevice.PipDevice.FirstUsedTipType = DispTip) and
        ((fTips and fArmDevice.PipDevice.DropTipMap) <> fTips) then
    begin
        if gGUIManager.MessageBox(TLanguageString.Read('Fetch disposable tip from DiTi rack?',
            'Soll eine Wegwerfspitze vom DiTi-Rack aufgenommen werden?'), xCaption, MB_YESNO) = IDYES then
            gmPickDispTips(fArmDevice, fTips);
    end;

    if gGUIManager.MessageBox(TLanguageString.Read('Move to position {0}?', 'Position {0} anfahren?',
        [fPosition]), xCaption, MB_ICONQUESTION + MB_YESNO) <> IDYES then
        EXIT;
    iTip := -1;
    for i := 0 to fArmDevice.PipDevice.TipCount - 1 do
        if (((fTips shr i) and 1) = 1) then
            iTip := i;
    if (iTip = -1) then
    begin
        gGUIManager.MessageBox(TLanguageString.Read('Error: No tip selected!',
            'Fehler: Es ist keine Nadel ausgewählt!'), xCaption, 16);
        EXIT;
    end;

    xRP := TXRackPositionUtils.GetEmptyXRackPositions(fArmDevice.PipDevice.TipCount);
    xRP[iTip].Rack := fRack;
    xRP[iTip].Pos := fPosition;

    xTipMap := gmEmptyTipMap();
    TTipMapUtils.SelectTip(xTipMap, iTip);

    xXYStep := gmCalculatePipStep(fArmDevice, xTipMap, xRP, xZStep);
    if not Assigned(xXYStep) then
        EXIT;
    try
        if (xZStep.Z.ZTravel[iTip] = 0) and ((xZStep.Z.ZScan[iTip] < 0) or (xZStep.Z.ZDisp[iTip] < 0) or
            (xZStep.Z.ZMax[iTip] < 0)) then
        begin
            gGUIManager.MessageBox(TLanguageString.Read('Error: Rack height is too big for pipetting!',
                'Fehler: Die Rack-Höhe ist zu groß zum Pipettieren!'), xCaption, 16);
            EXIT;
        end;
        if gErrorManager.IsGlobalErr() then
            EXIT;

        gmCombinedArmsMoveToZTravel(fArmDevice);

        // an die XY-Position fahren
        xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(fArmDevice);
        try
            xXYOp.MoveXYSimple(xXYStep);
            if gErrorManager.IsGlobalErr() then
                EXIT;
        finally
            FreeAndNil(xXYOp);
        end;

        // Z-Positionen anfahren
        xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(fArmDevice.MotionDevice);
        try
            // Z-Travel
            if gGUIManager.MessageBox(TLanguageString.Read('Goto Z-Travel?', 'Position Z-Travel anfahren?'),
                xCaption, MB_ICONQUESTION + MB_YESNO) <> IDYES then
                EXIT;
            xZMotion.MoveSingleZ(iTip, xZStep.Z.ZTravel[iTip], m_EXECUTE);
            if gErrorManager.IsGlobalErr() then
                EXIT;

            // Z-Scan
            if gGUIManager.MessageBox(TLanguageString.Read('Goto Z-Scan?', 'Position Z-Scan anfahren?'),
                xCaption, MB_ICONQUESTION + MB_YESNO) <> IDYES then
                EXIT;
            xZMotion.MoveSingleZ(iTip, xZStep.Z.ZScan[iTip], m_EXECUTE);
            if gErrorManager.IsGlobalErr() then
                EXIT;

            // Z-Dispense
            if gGUIManager.MessageBox(TLanguageString.Read('Goto Z-Dispense?',
                'Position Z-Dispense anfahren?'), xCaption, MB_ICONQUESTION + MB_YESNO) <> IDYES then
                EXIT;
            xZMotion.MoveSingleZ(iTip, xZStep.Z.ZDisp[iTip], m_EXECUTE);
            if gErrorManager.IsGlobalErr() then
                EXIT;

            // Z-Max
            if gGUIManager.MessageBox(TLanguageString.Read('Goto Z-Max?', 'Position Z-Max anfahren?'),
                xCaption, MB_ICONQUESTION + MB_YESNO) <> IDYES then
                EXIT;
            xZMotion.MoveSingleZ(iTip, xZStep.Z.ZMax[iTip], m_EXECUTE);
        finally
            FreeAndNil(xZMotion);
        end;
    finally
        FreeAndNil(xXYStep);
    end;
end;


end.
