{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Functions used by Operation units and by Actions to work with the Tool
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  ---------------------------       -------- -----------------------------------------------
  26.09.06 wl  Change-,PutRemovableTip           TN3326    neu: z.B. für PickFix-Tips, bisher nur in GrpArm, sollte aber auch in PipArm möglich sein
  26.09.06 wl  Get-,PutHandlerTool               TN3326    um RemovableTip-Funktionalität erweitert
  27.02.07 pk  TToolMoveOperation.GetHandlerTool TN3600    Dont do Opengripper function
  12.03.07 pk                                    TN3629    new RackMoveOptions
  20.06.08 pk                                    TN4139    WB global object replaced by LayoutManager
  25.09.08 wl                                    TN4242    TRunstCall ersetzt TDllCall
  17.11.08 pk  SetToolData                       TN4280    New ToolOwnerID param
  08.06.09 ts  Put/ChangeRemovableTip            TN4584    Funktionen werden verlassen, falls dem Arm kein PipDevice zugeordnet wurde
  23.03.11 wl  Get-,PutHandlerTool               TN5515    überarbeitet: Einige Aufrufe von MoveToZTravel entfernt
  15.08.11 ts  PutHandlerTool                    TN5658    der Greifer muss geschlossen werden, wenn der aktuelle Wert zu groß ist, sonst CRASH
  28.06.12 wl  PutHandlerTool                    TN5527   IGripDevice.read in GetPosition umbenannt
  -------------------------------------------------------------------------------------------------- }

unit OperationTool;


interface


uses
    CommonTypes,
    Rack,
    EventManager,
    IntfGripDevice,
    IntfMotionDevice,
    IntfArmDevice,
    IntfZTravelManager,
    OperationRack;

type

    TToolMoveOperation = class
    protected
        fUsedArm: IArmDevice; { TODO : this doesn't belong here. needed this for Rackmove }
        fMotionDevice: IMotionDevice;
        fGripper: IGripDevice;
        fZTravelManager: IGripZTravelManager;
        procedure SetToolData(aNewToolName: string; aToolIsRack: boolean; aToolData: TTubeToolData;
            aToolOwnerID: cardinal; aIsPipTool: boolean; aPipToolRelLength: TPosMM);
        procedure MoveToZTravel();
        procedure PutRemovableTip();
        procedure ChangeRemovableTips();
    public
        constructor Create(aUsedArm: IArmDevice; aMotionDevice: IMotionDevice; aGripper: IGripDevice;
            aZTravelManager: IGripZTravelManager);
        procedure PutHandlerTool(aEvBeforePut: TRunstCall = nil; aEvAfterPut: TRunstCall = nil);
        procedure GetHandlerTool(aNewToolName: string; aToolRack: TRack; aToolData: TTubeToolData;
            aToolOwnerID: cardinal; aIsPipTool: boolean; aPipToolRelLength: TPosMM;
            aEvBeforeGet, aEvAfterGet: TRunstCall);
        procedure ResetToolData();
    end;


implementation


uses
    AppTypes,
    ErrorManager,
    RunFlow,
    OperationAxisMove,
    OperationFactory,
    LayoutManager;

{ TToolMoveOperation }

constructor TToolMoveOperation.Create(aUsedArm: IArmDevice; aMotionDevice: IMotionDevice;
    aGripper: IGripDevice; aZTravelManager: IGripZTravelManager);
begin
    inherited Create();
    fUsedArm := aUsedArm;
    fMotionDevice := aMotionDevice;
    fGripper := aGripper;
    fZTravelManager := aZTravelManager;
end;

procedure TToolMoveOperation.ResetToolData();
begin
    fGripper.ResetToolData;
    fZTravelManager.Reset;
end;

procedure TToolMoveOperation.MoveToZTravel();
var
    xOperation: TZAxisTravelMoveOperation;
begin
    xOperation := TOperationFactory.CreateZAxisTravelMoveOp(fMotionDevice, fZTravelManager);
    if not Assigned(xOperation) then
        EXIT;
    try
        xOperation.MoveToZTravelAllTips();
    finally
        xOperation.Free;
    end;

end;

procedure TToolMoveOperation.SetToolData(aNewToolName: string; aToolIsRack: boolean; aToolData: TTubeToolData;
    aToolOwnerID: cardinal; aIsPipTool: boolean; aPipToolRelLength: TPosMM);
begin
    fGripper.SetToolData(aNewToolName, aToolIsRack, aToolData, aToolOwnerID);
    // Ist das Tool das erwartete Pipettiertool?
    if aIsPipTool then
    begin
        fZTravelManager.SetToPipTool(aPipToolRelLength);
    end
    else
    begin
        fZTravelManager.SetToTool;
    end;
end;

procedure TToolMoveOperation.PutRemovableTip();
begin
    if fUsedArm.PipDevice = nil then
        EXIT;
    // neu: Put tip funktion: bisher nur für Gripper Arm!
    if (fUsedArm.PipDevice.Tips[0].RemovableTipPutMethod <> '') then
    begin
        TLayoutManager.Instance.StartMethodRun(fUsedArm.PipDevice.Tips[0].RemovableTipPutMethod);
        fUsedArm.PipDevice.Tips[0].RemovableTipPutMethod := '';
    end;
end;

procedure TToolMoveOperation.ChangeRemovableTips();
var
    xMethodGetTip, xMethodPutTip: string;
begin
    if fUsedArm.PipDevice = nil then
        EXIT;
    // Diese Methodennamen könnten sich in der PutTip-Methode ändern, deswegen werden sie in variablen geschrieben
    xMethodGetTip := fUsedArm.PipDevice.Tips[0].MethodGetTip;
    xMethodPutTip := fUsedArm.PipDevice.Tips[0].MethodPutTip;

    // evtl. anderen Tip zurückbringen
    if (fUsedArm.PipDevice.Tips[0].RemovableTipPutMethod <> xMethodPutTip) then
        PutRemovableTip();

    // Das Tool ändert sich nicht!!!

    // Get tip funktion: bisher nur für Gripper Arm!
    if (xMethodGetTip <> '') and (fUsedArm.PipDevice.Tips[0].RemovableTipPutMethod = '') then
    begin
        TLayoutManager.Instance.StartMethodRun(xMethodGetTip);
        fUsedArm.PipDevice.Tips[0].RemovableTipPutMethod := xMethodPutTip;
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TToolMoveOperation.PutHandlerTool(aEvBeforePut, aEvAfterPut: TRunstCall);
// --------------------------------------------------------------------------------------------------
// -- Tool zurückbringen (es wird auf den gleichen Platz zurückgelegt, wo es herkam)
// --------------------------------------------------------------------------------------------------
var
    xRackOp: TRackMoveOperation;
    xRack: TRack;
begin
    if (not fGripper.HasTool) then
    begin

        // Es könnten ja Removable Tips gewechselt werden
        self.ChangeRemovableTips();
        EXIT;
    end;

    if (gErrorManager.IsGlobalErr) then
        EXIT;

    if (fGripper.ToolIsRack) then
    begin
        xRack := TLayoutManager.Instance.CurrentLayout.FindRackByName(fGripper.Tool.Name);
        self.PutRemovableTip();
        xRackOp := TOperationFactory.CreateRackMoveOp(fUsedArm, xRack);
        try
            if fGripper.GetPosition(true) > xRack.Structure.H_VClose_mm then
                fGripper.CloseGripper(xRack.Structure.H_VClose_mm, true);
            xRackOp.PutPlate(false, aEvBeforePut, aEvAfterPut,
                TRackMoveOperation.GetDefaultRackMoveOptions());
        finally
            xRackOp.Free;
        end;
        if (gErrorManager.IsGlobalErr) then
            EXIT;

        ResetToolData();

        // Nach dem Tool ablegen auf das richtige Z-Travel fahren (hier nötig??)
        MoveToZTravel();
    end
    else
    begin
        ResetToolData();
    end;
end;

procedure TToolMoveOperation.GetHandlerTool(aNewToolName: string; aToolRack: TRack; aToolData: TTubeToolData;
    aToolOwnerID: cardinal; aIsPipTool: boolean; aPipToolRelLength: TPosMM;
    aEvBeforeGet, aEvAfterGet: TRunstCall);
var
    xRackOp: TRackMoveOperation;
begin
    // evtl. anderes Tool zurückbringen
    if (fGripper.Tool.Name <> aNewToolName) then
        PutHandlerTool;

    if (fGripper.HasTool) then
        EXIT; // z.B. wenn FToolData.Name = aNewToolName
    if (gErrorManager.IsGlobalErr) then
        EXIT;

    // Tool aufnehmen
    if Assigned(aToolRack) then
    begin

        xRackOp := TOperationFactory.CreateRackMoveOp(fUsedArm, aToolRack);
        try
            xRackOp.GetPlate(false, aEvBeforeGet, aEvAfterGet,
                TRackMoveOperation.GetDefaultRackMoveOptions());

        finally
            xRackOp.Free;
        end;

        if (gErrorManager.IsGlobalErr) then
            EXIT;

        SetToolData(aNewToolName, true, aToolData, aToolOwnerID, aIsPipTool, aPipToolRelLength);

        // Nach dem Tool aufnehmen auf das richtige Tool-Z-Travel fahren (hier nötig??)
        MoveToZTravel();
    end
    else
    begin
        SetToolData(aNewToolName, false, aToolData, aToolOwnerID, aIsPipTool, aPipToolRelLength);
    end;
end;


end.
