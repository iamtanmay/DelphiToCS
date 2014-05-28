{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Functions used by Handling units and by Actions to work with the Tool
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  04.02.04 pk                                TN1719    New
  11.02.04 pk  gmResetError                  TN1744.1  No need to SetGlobalErr to None
  12.02.04 pk  gmResetError                  TN1744.1  -> ObjModulA.  gmBringBackTool was extracted from it
  12.03.04 wl  gmGet/PutHandlerTool          TN1812   GetPlate/PutPlate-Aufruf geändert: TRack wird übergeben
  05.04.04 wl  gmSetToolData,gmResetToolData,gmPutHandlerTool  TN1788   --> DevicesGrpArm
  05.04.04 wl  gmGetHandlerTool              TN1788   wichtige Teile --> DevicesGrpArm
  19.04.04 wl  gmGetRack                     TN1788   --> ObjWorkb
  20.04.04 wl  gmTakeOrPutTool               TN1788   aus DilutionManager.Dilution extrhiert
  20.04.04 wl  gmTakeOrPutTool               TN1788.5 Tool von ConcurrentArm wird nicht mehr weggebracht!!! (-> SIAS)
  22.04.04 wl  gmGetHandlerTool              TN1788   Funktion korrigiert
  10.05.04 pk  gmGetHandlerTool              TN1889.1 FindRackByName instead of GetRackFromName
  08.06.04 wl  gmGetHandlerTool              TN1963   benutzt aUsedArm (TGripperArmDevice)
  08.06.04 wl  gmBringBackTools              TN1963   ALLE(!) Gripper-Arme werden nach verbliebenen Tools abgefragt
  15.06.04 wl  gmArmBringBackTool            TN1963   MoveGripper durch OpenGripper ersetzt
  29.06.04 wl  gmGetHandlerTool              TN1963   Fehler korrigiert
  01.07.04 wl  gmResetAllToolData            TN1963   ALLE Gripper-Arme: Tooldaten zurücksetzen
  06.07.04 tbh gmArmBringBackTool            TN2020   Fehlerfenster nur im Fehlerfall sonst so abliefern
  29.07.04 pk  gmArmBringBackTool            TN2043   New Param: ShowDialog
  16.09.04 wl  gmGetHandlerTool              TN2138.2  wenn Toolname = '' muss Funktion auch durchlaufen werden!!
  11.03.05 pk  gmArmBringBackTool            TN2339.2  Call ToolErr via GUIManager
  20.10.05 wl  gmGetHandlerTool              TN2673    mit Events: aEvBeforeGet, aEvAfterGet
  24.04.06 pk  gmArmBringBackTool            TN2958   If no GripperDevice then exit
  07.09.06 wl  gmPutCombinedArmTool          TN3287    war 1. Teil von gmTakeOrPutTool
  07.09.06 wl  gmTakeAndPutTool              TN3287    war 2. Teil von gmTakeOrPutTool
  26.09.06 wl  gmTakeAndPutTool              TN3326    Options werden hier nicht mehr ausgewertet (unnötig)
  27.02.07 pk  gmGetHandlerTool              TN3600    dont call PutHandlerTool, it is called in GetHandlerTool
  20.06.08 pk                                TN4139    WB global object replaced by LayoutManager
  25.09.08 wl                                TN4242    TRunstCall ersetzt TDllCall
  17.11.08 pk  GetCurrentToolOwnerID         TN4280    New
  19.11.08 pk  gmBringBackToolsAtReset       TN4280    aShowDialog param
  09.06.09 pk                                TN4585.1  Uses ToolErr removed
  10.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  12.08.09 wl  ArmBringBackTool              TN4712   ToolErrDialog mit nur einem Rückgabewert
  02.11.10 wl  gmResetAllToolData            TN5323   GrippedContainer werden zurückgesetzt
  06.06.13 wl  ReadToolData                  TN6154   von Layout & SamStart hierher
  15.08.13 wl                                TN6223   uses geändert
  20.08.13 wl  RecoveryPutHandlerTool        TN6231   während der Recovery-Bewegungen wird ERecoveryMovementException abgefangen
  22.08.13 wl                                TN6231   globale Funktionen zu Methoden gemacht
  07.04.14 ts  ArmBringBackToolIntern        TN6370   new: SwitchDeviceForToolReturn
  -------------------------------------------------------------------------------------------------- }

unit ToolHandling;


interface


uses
    CommonTypes,
    EventManager,
    Rack,
    Layout,
    IntfArmDevice;

type
    TToolHandling = record
    private const
        cToolOwnerIDAll = 0;
    private
        class function GetCurrentToolOwnerID(): cardinal; static;
        class function ReadToolData(aLayout: TLayout; const aToolName: string; out oToolData: TTubeToolData)
            : boolean; static;
        class procedure BringBackToolsIntern(aShowDialog: boolean; aToolOwnerID: cardinal); static;
        class procedure ArmBringBackToolIntern(aUsedArm: IArmDevice; aShowDialog: boolean;
            aToolOwnerID: cardinal); static;
        class procedure GetHandlerToolIntern(aUsedArm: IArmDevice; aNewToolName: string;
            aToolData: TTubeToolData; aToolOwnerID: cardinal; aRack: TRack;
            aEvBeforeGet, aEvAfterGet: TRunstCall); static;
        class procedure ResetToolData(aUsedArm: IArmDevice); static;
        class procedure PutHandlerTool(aUsedArm: IArmDevice); static;
        class function RecoveryPutHandlerTool(aUsedArm: IArmDevice): boolean; static;
    public
        class procedure GetHandlerTool(aUsedArm: IArmDevice; aNewToolName: string;
            aEvBeforeGet: TRunstCall = nil; aEvAfterGet: TRunstCall = nil); static;
        class procedure BringBackTools(aShowDialog: boolean); static;
        class procedure BringBackToolsAtReset(aShowDialog: boolean); static;
        class procedure ResetAllToolData(); static;
        class procedure ArmBringBackTool(aUsedArm: IArmDevice; aShowDialog: boolean); static;
    end;


implementation


uses
    Windows,
    SysUtils,
    AppTypes,
    GeneralTypes,
    ErrorMessageFactory,
    ErrorInfo,
    ErrorManager,
    ObjModul,
    DeviceInitHandling,
    GUIManager,
    GUIManagerSetup,
    OperationTool,
    OperationFactory,
    ThreadAPI,
    AppSettings,
    RunFlow,
    LogManager,
    LayoutManager;

{ TToolHandling }

class function TToolHandling.GetCurrentToolOwnerID(): cardinal;
begin
    result := TThreadAPI.GetCurrentThreadID();
end;

class function TToolHandling.ReadToolData(aLayout: TLayout; const aToolName: string;
    out oToolData: TTubeToolData): boolean;
var
    xIniAccess: IWinlissyIniAccess;
    xTubeToolDataExists: boolean;
    xToolRack: TRack;
begin
    result := true;

    xIniAccess := gCommonDll.CreateRobotIni;
    oToolData.XOffset := xIniAccess.ReadFloat('TakeTubes', 'XOffset');
    oToolData.YOffset := xIniAccess.ReadFloat('TakeTubes', 'YOffset');
    oToolData.ZOffset := xIniAccess.ReadFloat('TakeTubes', 'ZOffset');
    oToolData.VOpen := xIniAccess.ReadFloat('TakeTubes', 'VOpen');
    oToolData.VDrop := xIniAccess.ReadFloat('TakeTubes', 'VDrop');
    oToolData.ZDrop := xIniAccess.ReadFloat('TakeTubes', 'ZDrop');
    oToolData.TubeDY := xIniAccess.ReadFloat('TakeTubes', 'TubeDY');
    oToolData.SafeMoveOffset := xIniAccess.ReadFloat('TakeTubes', 'SaveMoveOffset');
    oToolData.SafeMoveSpeed := xIniAccess.ReadInteger('TakeTubes', 'SaveMoveSpeed');
    oToolData.ShakeHeight := xIniAccess.ReadFloat('TakeTubes', 'ShakeHeight');
    oToolData.ShakeZOffset := xIniAccess.ReadFloat('TakeTubes', 'ShakeZOffset');
    oToolData.ShakeYOffset := xIniAccess.ReadFloat('TakeTubes', 'ShakeYOffset');
    oToolData.BringBackVOffset := xIniAccess.ReadFloat('TakeTubes', 'ToolBringBackVOffset');
    oToolData.GetTubeXYShiftStepCount := 0;
    oToolData.GetTubeXYShiftRadius1 := 0;
    oToolData.GetTubeXYShiftRadius2 := 0;

    if (aToolName = '') then
        EXIT;

    // Standardwerte lesen
    xTubeToolDataExists := xIniAccess.ValueExists(STR_ISEC_TUBETOOLDATA, aToolName);
    if xTubeToolDataExists then
        oToolData := xIniAccess.ReadTubeToolData(STR_ISEC_TUBETOOLDATA, aToolName);

    xToolRack := aLayout.FindRackByName(aToolName);

    // Tool ist KEIN Rack
    if not Assigned(xToolRack) then
    begin
        result := xTubeToolDataExists;
        EXIT;
    end;

    // Tool ist Rack
    oToolData.Name := aToolName;
    oToolData.TubeDY := xToolRack.Structure.TubeY_mm;
    oToolData.VOpen := xToolRack.Structure.H_VClose_mm;

    // X-Strecke zwischen der Referenzspitze Tip1 und dem Greifer des Tools (Position 1)
    oToolData.XOffset := (xToolRack.Structure.H_XTake_mm
        // Abstand Tool Rand <-> Greifposition am Tool x - Richtung in mm
        - xToolRack.Structure.PosX_First_mm); // Abstand Tool Rand <-> erste Position x - Richtung in mm

    // Y-Strecke zwischen der Referenzspitze Tip1 und dem Greifer des Tools (Position 1)
    oToolData.YOffset := (xToolRack.SizeY / 2 - xToolRack.Structure.PosY_First_mm);
    oToolData.ZOffset := (
        // xToolRack.Structure.ZDisp_mm - xToolRack.Structure.ZScan_mm // fragwürdig, deshalb ausgeklammert TBD_WL
        +xToolRack.Structure.H_ZTake_mm - xToolRack.Structure.TubeZ_mm);
end;

class procedure TToolHandling.PutHandlerTool(aUsedArm: IArmDevice);
var
    xToolOp: TToolMoveOperation;
begin
    xToolOp := TOperationFactory.CreateToolMoveOp(aUsedArm);
    try
        xToolOp.PutHandlerTool;
    finally
        FreeAndNil(xToolOp);
    end;
end;

class function TToolHandling.RecoveryPutHandlerTool(aUsedArm: IArmDevice): boolean;
begin
    result := true;
    gRunFlow.IsRecoveryMovement := true;
    try
        try
            PutHandlerTool(aUsedArm);
        except
            on E: ERecoveryMovementException do
            begin
                gErrorMessageFactory.ErrBoxSimple(E.Message, 'Bring back tool', eibAbort);
                result := false;
            end;
            else
            begin
                result := false;
                raise;
            end;
        end;
    finally
        gRunFlow.IsRecoveryMovement := false;
    end;
end;

class procedure TToolHandling.GetHandlerToolIntern(aUsedArm: IArmDevice; aNewToolName: string;
    aToolData: TTubeToolData; aToolOwnerID: cardinal; aRack: TRack; aEvBeforeGet, aEvAfterGet: TRunstCall);
var
    xToolOp: TToolMoveOperation;
    xIsPipTool: boolean;
    xPipToolLen: TPosMM;
begin
    xIsPipTool := false;
    xPipToolLen := 0;
    if (aNewToolName <> '') and Assigned(aUsedArm.PipDevice) and
        (aUsedArm.PipDevice.Tips[0].ToolName = aNewToolName) then
    begin
        xIsPipTool := true;
        xPipToolLen := aUsedArm.PipDevice.Tips[0].RelLength_mm;
    end;

    xToolOp := TOperationFactory.CreateToolMoveOp(aUsedArm);
    xToolOp.GetHandlerTool(aNewToolName, aRack, aToolData, aToolOwnerID, xIsPipTool, xPipToolLen,
        aEvBeforeGet, aEvAfterGet);
    xToolOp.Free;
end;

class procedure TToolHandling.ResetToolData(aUsedArm: IArmDevice);
var
    xToolOp: TToolMoveOperation;
begin
    xToolOp := TOperationFactory.CreateToolMoveOp(aUsedArm);
    try
        xToolOp.ResetToolData();
    finally
        FreeAndNil(xToolOp);
    end;
end;

class procedure TToolHandling.GetHandlerTool(aUsedArm: IArmDevice; aNewToolName: string;
    aEvBeforeGet, aEvAfterGet: TRunstCall);
var
    xRack: TRack;
    xToolData: TTubeToolData;
    xToolOwnerID: cardinal;
begin
    if not Assigned(aUsedArm) then
        exit;

    if (aNewToolName <> '') and (aUsedArm.GripDevice.Tool.Name = aNewToolName) then
        exit; // Tool ist bereits aufgenommen!

    // neues Tool aufnehmen
    if TToolHandling.ReadToolData(TLayoutManager.Instance.CurrentLayout, aNewToolName, xToolData) then
    begin
        xRack := nil;
        if (aNewToolName <> '') and not(pos('TakeTubes', aNewToolName) > 0) then
            xRack := TLayoutManager.Instance.CurrentLayout.FindRackByName(aNewToolName, true);

        xToolOwnerID := GetCurrentToolOwnerID();
        GetHandlerToolIntern(aUsedArm, aNewToolName, xToolData, xToolOwnerID, xRack, aEvBeforeGet,
            aEvAfterGet);
    end
    else
    begin // Bei der Angabe eines unbekannten Tools ist sofort Schluß
        gErrorManager.SetGlobalErr(ERR_MODULE, TLanguageString.Read('Tool or gripper setting {0} not found!',
            'Tool oder Greiferdefinition {0} konnte nicht gefunden werden!', [aNewToolName]));
    end;
end;

class procedure TToolHandling.ArmBringBackToolIntern(aUsedArm: IArmDevice; aShowDialog: boolean;
    aToolOwnerID: cardinal);
var
    xToolErrMessage: string;
    xErrorHandlingChoice: integer;
begin
    // Tool befindet sich im Handler! Formular mit Auswahlmöglichkeiten zur Fehlerbehandlung

    if (not Assigned(aUsedArm.GripDevice)) or (not aUsedArm.GripDevice.HasTool) or
        ((aToolOwnerID <> cToolOwnerIDAll) and (aUsedArm.GripDevice.ToolOwnerID <> aToolOwnerID)) then
        EXIT;

    xToolErrMessage := TLanguageString.Read('The tool {0} is still in the gripper!',
        'Das Tool {0} befindet sich noch im Greifer!', [aUsedArm.GripDevice.Tool.Name]);

    if not aShowDialog then
    begin
        TLogManager.Instance.Log(xToolErrMessage + ' - Automatic bring back tool for recovery', false);
        TDeviceInitHandling.SwitchDeviceForToolReturn(true);
        PutHandlerTool(aUsedArm);
        TDeviceInitHandling.SwitchDeviceForToolReturn(false);
        EXIT;
    end;

    xErrorHandlingChoice := (gGUIManager as TGUIManagerSetup).ToolErr_PromptInput(xToolErrMessage);

    case xErrorHandlingChoice of
        cToolErrResultOpenGripper:
            begin // Open Handler
                TDeviceInitHandling.SwitchDeviceForToolReturn(true);
                aUsedArm.GripDevice.OpenFull(false);
                ResetToolData(aUsedArm);
                TDeviceInitHandling.SwitchDeviceForToolReturn(false);
            end;

        cToolErrResultBringBack:
            begin
                TDeviceInitHandling.SwitchDeviceForToolReturn(true);
                if not RecoveryPutHandlerTool(aUsedArm) then
                begin
                    TDeviceInitHandling.SwitchDeviceForToolReturn(false);
                    EXIT;
                end;
                TDeviceInitHandling.SwitchDeviceForToolReturn(false);
            end;

        cToolErrResultCancel:
            begin
                EXIT;
            end;
    end;

    if (not gErrorManager.IsGlobalErr) and
        (gGUIManager.MessageBox(TLanguageString.
        Read('Is the tool out of the gripper? (On ""yes"" the machine will be initialized.)',
        'Ist das Tool außerhalb des Greifers? (Wenn ja, wird das Gerät initialisiert.)'),
        TLanguageString.Read('Init After Tool Error', 'Init nach einem Tool-Fehler'), mb_YESNO) = IDYES) then
        TDeviceInitHandling.AllArmsInitMotors;
end;

class procedure TToolHandling.ArmBringBackTool(aUsedArm: IArmDevice; aShowDialog: boolean);
var
    xToolOwnerID: cardinal;
begin
    xToolOwnerID := GetCurrentToolOwnerID();
    ArmBringBackToolIntern(aUsedArm, aShowDialog, xToolOwnerID);
end;

class procedure TToolHandling.BringBackToolsIntern(aShowDialog: boolean; aToolOwnerID: cardinal);
var
    x: integer;
begin
    for x := 0 to gArmManager.Count - 1 do
        if Assigned(gArmManager.Arms[x].GripDevice) then
            ArmBringBackToolIntern((gArmManager.Arms[x]), aShowDialog, aToolOwnerID);
end;

class procedure TToolHandling.BringBackTools(aShowDialog: boolean);
var
    x: integer;
    xToolOwnerID: cardinal;
begin
    xToolOwnerID := GetCurrentToolOwnerID();
    for x := 0 to gArmManager.Count - 1 do
        if Assigned(gArmManager.Arms[x].GripDevice) then
            ArmBringBackToolIntern((gArmManager.Arms[x]), aShowDialog, xToolOwnerID);
end;

class procedure TToolHandling.BringBackToolsAtReset(aShowDialog: boolean);
begin
    BringBackToolsIntern(aShowDialog, cToolOwnerIDAll);
end;

class procedure TToolHandling.ResetAllToolData();
var
    x: integer;
begin
    for x := 0 to gArmManager.Count - 1 do
    begin
        if Assigned(gArmManager.Arms[x].GripDevice) then
        begin
            // GrippedContainer zurücksetzen
            gArmManager.Arms[x].GripDevice.RemoveGrippedContainer();

            // Tool zurücksetzen
            ResetToolData(gArmManager.Arms[x]);
        end;
    end;
end;


end.
