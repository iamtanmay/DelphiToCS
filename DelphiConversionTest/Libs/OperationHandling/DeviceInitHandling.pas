{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  21.08.13 wl                                      TN6231   Initial Revision
  21.08.13 wl                                      TN6231   enthält Teile aus ObjModulA, SubstanceHandling und SamIntf
  22.08.13 wl  ConnectAndInit                      TN6233   Init enthält jetzt immer ConnectModules
  22.08.13 wl  ConnectAndInit                      TN6233   neu: ResetGlobalErr findet vor ConnectModules statt
  23.08.13 wl  DropAllDispTips                     TN6233   --> LiquidHandlingDiTi
  23.08.13 wl  ConnectAndInitRun                   TN6233.2 RefreshLayoutDeviceList,AllArmsStoreTips und InitTipStatus_AllArms werden auch ohne HardwareInit ausgeführt
  11.11.13 ts  AllArmsInitMotorsWithID             TN6300   InitOffset for ZMotors
  07.04.14 ts  SwitchDeviceForToolReturn           TN6370   new
  ----------------------------------------------------------------------------------------------------------- }

unit DeviceInitHandling;


interface


uses
    Driver,
    IntfArmDevice,
    AppTypes;

type
    TDeviceInitHandling = record
    private
        class var globalInitID: TDevInitID;
        class procedure InitArm(aUsedArm: IArmDevice; aInitID: TDevInitID); static;
        class procedure PrepareBalanceAndWait(aInitID: TDevInitID); static;
        class procedure InitializeSyringes(aInitID: TDateTime); static;
        class procedure InitTipStatus_AllArms(); static;
        class procedure InitConnections(); static;
        class procedure ResetConnections(); static;
        class procedure VortexerInit(); static;
        class procedure VortexerStop(); static;
        class procedure BCReaderInit(); static;
        class procedure XWayValveCheck(); static;
        class function GenerateResetID: TDevResetID; static;
        class procedure AllArmsInitMotorsWithID(aInitID: TDevInitID); static;
        class procedure ResetDecappers; static;
        class procedure RelayInit(); static;
        class procedure RelayReset; static;
        class procedure InitializeInitID(); static;
        class procedure ResetRediPump(); static;
        class procedure ResetError(); static;
        class procedure HardwareInitBasic; static;
        class procedure HardwareInitExtended; static;
        class procedure AllArmsStoreTips; static;
    public
        class procedure AllArmsInitMotors(); static;
        class procedure ResetInitID; static;
        class procedure ConnectAndInit(aHardwareInit: boolean); static;
        class procedure ConnectAndInitRun(aHardwareInit: boolean); static;
        class function GenerateInitID: TDevInitID; static;
        class procedure HarwareReset(); static;
        class function GetInitID: TDevInitID; static;
        class procedure SwitchDeviceForToolReturn(aInit: boolean); static;
    end;


implementation


uses
    Windows,
    SysUtils,
    SamHigh,
    CommonTypes,
    OperationFactory,
    MotionSystemTube,
    OperationAxisMove,
    PosinfoDataAdaptor,
    PipDeviceManager,
    TubeHandling,
    ErrorManager,
    SamGlobe,
    IntfSwitchDevice,
    IntfBalanceDevice,
    ObjModul,
    GUIManager,
    GeneralTypes,
    PeripheryManager,
    LayoutManager,
    LiquidHandlingLow,
    IntfThermostatDevice,
    IntfMixerDevice,
    IntfXWayValveDevice,
    IntfTriggerDevice,
    IntfBCReaderDevice,
    IntfPipDevice,
    ToolHandling,
    ThrdMan,
    GUIManagerSetup,
    XTrackConflictManager,
    RackTypes,
    Rack,
    ObjModulA,
    ConnectionManager,
    Connection,
    LogManager,
    ErrorMessageFactory,
    ErrorInfo,
    LiquidHandlingDiTi,
    DevicesConflictManager,
    IntfMotorBasedMotionDevice,
    IntfDecapperDevice,
    DeviceManager;

{ TDeviceInitHandling }

class procedure TDeviceInitHandling.ResetInitID;
begin
    globalInitID := INT_INITID_NONE;
end;

class procedure TDeviceInitHandling.InitArm(aUsedArm: IArmDevice; aInitID: TDevInitID);
var
    xInitOp: TInitOperation;
begin
    xInitOp := TOperationFactory.CreateInitOp(aUsedArm);
    try
        xInitOp.Init(aInitID);
    finally
        xInitOp.Free;
    end;
end;

class procedure TDeviceInitHandling.AllArmsInitMotorsWithID(aInitID: TDevInitID);
var
    x: integer;
    xMotorBasedMD: IMotorBasedMotionDevice;
    xArms: TArray<IArmDevice>;
begin
    gLogManager.Log('Init Motors (Home move)', false);

    xArms := gDeviceManager.FindModules<IArmDevice>();
    for x := 0 to high(xArms) do
    begin
        if not SysUtils.Supports(xArms[x].MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
            CONTINUE;
        xMotorBasedMD.ZMotors.Init(aInitID);
        xMotorBasedMD.ZMotors.MoveToInitOffset;
    end;

    for x := 0 to high(xArms) do
    begin
        InitArm((xArms[x]), aInitID);
    end;
end;

class procedure TDeviceInitHandling.AllArmsInitMotors();
begin
    AllArmsInitMotorsWithID(0)
end;

class procedure TDeviceInitHandling.HardwareInitBasic;
begin
    if not gErrorManager.IsGlobalErr() then
        InitConnections();

    if not gErrorManager.IsGlobalErr() then
        BCReaderInit();

    if not gErrorManager.IsGlobalErr() then
        RelayInit();

    if not gErrorManager.IsGlobalErr() then
        VortexerInit();

    if not gErrorManager.IsGlobalErr() then
        VortexerStop();

    if not gErrorManager.IsGlobalErr() then
        XWayValveCheck();

    if not gErrorManager.IsGlobalErr() then
        AllArmsInitMotorsWithID(globalInitID); // Home move

    if not gErrorManager.IsGlobalErr() then
        ResetDecappers;
end;

class procedure TDeviceInitHandling.ResetDecappers;
var
    xDataAdapter: TPosinfoDataAdaptor;
    xCapFoundInDatabase: boolean;
    xCapTubeID: string;
    xDecapper: IDecapperDevice;
    xCapPosition, xNirvana: TRackIDPosition;
    xSearchIndex: integer;
begin
    xSearchIndex := 0;
    while true do
    begin
        if not gDeviceManager.FindModuleExt(IDecapperDevice, xSearchIndex, xDecapper) then
            BREAK;

        // Find Cap in Database and clear entry
        xCapFoundInDatabase := false;
        if not TLayoutManager.Instance.IsCurrentLayoutEmpty then
        begin
            xDataAdapter := TPosinfoDataAdaptor.Create();
            try
                { TODO -owl : Cap Position }
                xCapPosition := TLayoutManager.Instance.CurrentLayout.GetRackIDPos
                    (gmMakeRackPos(xDecapper.CapPosRackName, xDecapper.CapPosition));
                xCapFoundInDatabase := xDataAdapter.CapExists(xCapPosition, xCapTubeID); // seek cap entry
                xNirvana.RackID := '';
                if (xCapFoundInDatabase) then
                    xDataAdapter.MoveCapEntry(xCapPosition, xNirvana); // delete cap entry
            finally
                xDataAdapter.Free;
            end;
        end;

        // set decapper to default state
        xDecapper.InitDecapper(xCapFoundInDatabase);
        // end;
    end;
end;

class procedure TDeviceInitHandling.BCReaderInit();
var
    xBCReaderDev: IBCReaderDevice;
    xSearchIndex: integer;
    xResetID: TDevResetID;
begin
    xSearchIndex := 0;
    xResetID := GenerateResetID;

    while true do
    begin
        if not gDeviceManager.FindModuleExt(IBCReaderDevice, xSearchIndex, xBCReaderDev) then
            BREAK;
        xBCReaderDev.Init(xResetID);
    end;
end;

class procedure TDeviceInitHandling.VortexerStop();
var
    xThermo: IThermostatDevice;
    xShaker: IMixerDevice;
    xSearchIndex: integer;
    xResetID: TDevResetID;
begin
    xSearchIndex := 0;
    xResetID := GenerateResetID;

    while true do
    begin
        if not gDeviceManager.FindModuleExt(IThermostatDevice, xSearchIndex, xThermo) then
            BREAK;
        xThermo.Reset(xResetID);
    end;

    xSearchIndex := 0;
    while true do
    begin
        if not gDeviceManager.FindModuleExt(IMixerDevice, xSearchIndex, xShaker) then
            BREAK;
        xShaker.Reset(xResetID);
    end;
end;

class procedure TDeviceInitHandling.VortexerInit();
var
    xThermo: IThermostatDevice;
    xShaker: IMixerDevice;
    xSearchIndex: integer;
begin
    xSearchIndex := 0;
    while true do
    begin
        if not gDeviceManager.FindModuleExt(IThermostatDevice, xSearchIndex, xThermo) then
            BREAK;
        xThermo.Init(globalInitID);
    end;

    xSearchIndex := 0;
    while true do
    begin
        if not gDeviceManager.FindModuleExt(IMixerDevice, xSearchIndex, xShaker) then
            BREAK;
        xShaker.Init(globalInitID);
    end;
end;

class procedure TDeviceInitHandling.XWayValveCheck();
var
    xWayValveDevice: IXWayValveDevice;
    xSearchIndex: integer;
begin
    xSearchIndex := 0;
    while true do
    begin
        if not gDeviceManager.FindModuleExt(IXWayValveDevice, xSearchIndex, xWayValveDevice) then
            BREAK;
        xWayValveDevice.CheckPositionReading();
    end;
end;

class procedure TDeviceInitHandling.ResetError();
var
    xErrReason: string;
begin
    try
        InitializeInitID();

        // get reason for the last error and display it!
        xErrReason := gErrorManager.GetAndDeleteErrorReason;
        if (xErrReason <> '') then
            gLogManager.Log('Reason: ' + xErrReason, true);

        gLogManager.Log('Reset to all Modules', false);

        gErrorManager.SetGlobalErrTemporary(true);
        // This is important! in ZP01 the _ResetModules class function resets the global
        // error pointer only half-way through instead of at the beggining
        try
            // Reset auf alle Module;
            ResetConnections();
            RelayReset();

            // wenn Modul Fehler hatte, GlobalErr zurücksetzen, damit Vort,.. schalten können
            if gErrorManager.IsGlobalErr then
                gErrorManager.SetGlobalErrTemporary(true);

            if not gErrorManager.IsGlobalErr() then
                VortexerStop();
            if not gErrorManager.IsGlobalErr() then
                XWayValveCheck();

            // wenn Modul Fehler hatte, GlobalErr zurücksetzen, damit ToolReturn funktioniert
            if gErrorManager.IsGlobalErr then
                gErrorManager.SetGlobalErrTemporary(true);

            if gErrorManager.IsGlobalErr then
                raise Exception.Create('Switch for Tool return could not be switched! Return tool manually!');
            TToolHandling.BringBackToolsAtReset(true);
        finally
            if not gErrorManager.IsGlobalErr then // nur setzen, wenn nicht schon gesetzt ist
                gErrorManager.SetGlobalErrTemporary(false);
        end;
    except
        on E: Exception do
        begin
            gErrorMessageFactory.ErrBoxSimple('Error occured during Run-Reset: ' + e.Message, 'Reset',
                eibAbort);
            gErrorManager.SetGlobalErr(ERR_APPLICATION, '');
        end;
    end;
end;

class procedure TDeviceInitHandling.PrepareBalanceAndWait(aInitID: TDevInitID);
var
    xGripperArm: IArmDevice;
    xDummy, xOccupiedPos, xOriginOccupiedPos: TXRackPosition;
    xBalanceArray: TBalanceArray;
    xBalanceDev: IBalanceDevice;
    i: integer;
    xDA: TPosinfoDataAdaptor;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;

    xDA := TPosinfoDataAdaptor.Create();
    try
        gModules.FindAllBalances(xBalanceArray);
        for i := low(xBalanceArray) to high(xBalanceArray) do
        begin
            xBalanceDev := xBalanceArray[i];
            if (xBalanceDev <> nil) then
            begin
                // Vorsicht!! Nur alte Version: Rackname=BalanceName - Tubes abräumen.
                if Assigned(TLayoutManager.Instance.CurrentLayout.FindRackByName(xBalanceDev.Name)) then
                begin
                    TLayoutManager.Instance.CurrentLayout.GetBalancePos(xDummy, xOccupiedPos,
                        xOriginOccupiedPos);
                    if (xOccupiedPos.Pos <> 0) then
                    begin
                        if (gMoveTubesAfterRestart = 1) then
                        begin
                            gGUIManager.MessageBox(TLanguageString.
                                Read('Robot removes tubes from the balance!',
                                'Tubes werden von der Waage genommen!'),
                                TLanguageString.Read('Tubes On Balance', 'Tubes auf der Waage'), 0);
                            while (xOccupiedPos.Pos <> 0) and (not gErrorManager.IsGlobalErr) do
                            begin
                                xGripperArm := gModules.FindFirstGripArm();
                                ASSERT(Assigned(xGripperArm), 'No grip arm');
                                TTubeHandling.GeneralBalanceTakeTube(xGripperArm, xOccupiedPos,
                                    xOriginOccupiedPos, [mtmNoStartPosBeforeGetTube], gMoveTubeFromBalance);
                            end;
                        end
                        else
                            gGUIManager.MessageBox(TLanguageString.Read('Please take tubes from the balance!',
                                'Bitte Tubes von der Waage nehmen'), TLanguageString.Read('Tubes On Balance',
                                'Tubes auf der Waage'), 0);
                    end;
                end;

                // Ende Tubes abräumen
                if not gErrorManager.IsGlobalErr() then
                    xBalanceDev.Init(aInitID);

                // Wait for thread (notwendig?)
                if not gErrorManager.IsGlobalErr() then
                    xBalanceDev.WaitForBalanceThread();
            end;
        end;
    finally
        xDA.Free;
    end;
end;

class procedure TDeviceInitHandling.ResetRediPump();
var
    xPumpDev: ISwitchDevice;
begin
    { TODO -oPK : FindRediPump }
    xPumpDev := nil; // self.FindRediPump();
    if not Assigned(xPumpDev) then
        Exit;
    xPumpDev.SwitchOff(false, 500);
end;

class procedure TDeviceInitHandling.SwitchDeviceForToolReturn(aInit: boolean);
var
    xSwitchDevice: ISwitchDevice;
    xSearchIndex: integer;
    xResetID: TDevResetID;
begin
    xResetID := GenerateResetID;
    xSearchIndex := 0;
    while true do
    begin
        if not gDeviceManager.FindModuleExt(ISwitchDevice, xSearchIndex, xSwitchDevice) then
            BREAK;
        if xSwitchDevice.SwitchForToolReturn then
            if aInit then
                xSwitchDevice.Init(globalInitID)
            else
                xSwitchDevice.Reset(xResetID);
    end;
end;

class procedure TDeviceInitHandling.RelayInit;
var
    xSwitchDevice: ISwitchDevice;
    xTriggerDevice: ITriggerDevice;
    xSearchIndex: integer;
begin
    xSearchIndex := 0;
    while true do
    begin
        if not gDeviceManager.FindModuleExt(ISwitchDevice, xSearchIndex, xSwitchDevice) then
            BREAK;
        xSwitchDevice.Init(globalInitID);
    end;

    xSearchIndex := 0;
    while true do
    begin
        if not gDeviceManager.FindModuleExt(ITriggerDevice, xSearchIndex, xTriggerDevice) then
            BREAK;
        xTriggerDevice.Init(globalInitID);
    end;
end;

class procedure TDeviceInitHandling.RelayReset;
var
    xSwitchDevice: ISwitchDevice;
    xTriggerDevice: ITriggerDevice;
    xSearchIndex: integer;
    xResetID: TDevResetID;
begin
    xResetID := GenerateResetID;

    xSearchIndex := 0;
    while true do
    begin
        if not gDeviceManager.FindModuleExt(ISwitchDevice, xSearchIndex, xSwitchDevice) then
            BREAK;
        xSwitchDevice.Reset(xResetID);
    end;

    xSearchIndex := 0;
    while true do
    begin
        if not gDeviceManager.FindModuleExt(ITriggerDevice, xSearchIndex, xTriggerDevice) then
            BREAK;
        xTriggerDevice.Reset(xResetID);
    end;
end;

class procedure TDeviceInitHandling.InitConnections();
var
    xConnection: IConnection;
    xSearchIndex: integer;
begin
    // this is a function that is executed once for each connection every time an init is done,
    // if you want a function that is only executed once for the lifetime of the application look at "Connect"
    xSearchIndex := 0;
    while true do
    begin
        if not gConnectionManager.FindModuleExt(IConnection, xSearchIndex, xConnection) then
            BREAK;
        xConnection.InitConnection();
    end;
end;

class procedure TDeviceInitHandling.ResetConnections();
var
    xConnection: IConnection;
    xSearchIndex: integer;
begin
    // this is a function that is executed once for each connection every time an Abort occurs.
    // if you want a function that is only executed once for the lifetime of the application look at "Disconnect"
    xSearchIndex := 0;
    while true do
    begin
        if not gConnectionManager.FindModuleExt(IConnection, xSearchIndex, xConnection) then
            BREAK;
        xConnection.ResetConnection();
    end;
end;

class procedure TDeviceInitHandling.InitializeInitID();
begin
    globalInitID := GenerateInitID();
end;

class function TDeviceInitHandling.GenerateInitID(): TDevInitID;
begin
    result := SysUtils.Now();
end;

class function TDeviceInitHandling.GenerateResetID: TDevResetID;
begin
    result := SysUtils.Now();
end;

class function TDeviceInitHandling.GetInitID: TDevInitID;
begin
    result := globalInitID;
end;

class procedure TDeviceInitHandling.HarwareReset();
begin
    ResetRediPump();

    // bei Fehler ResetToAllModules und Tool zurücksetzen
    if gErrorManager.IsGlobalErr then
    begin
        ResetError();
    end
end;

class procedure TDeviceInitHandling.InitTipStatus_AllArms();
var
    x, xTip: integer;
begin
    for x := 0 to gPipDeviceManager.Count - 1 do
    begin
        for xTip := 0 to gPipDeviceManager[x].TipCount - 1 do
        begin
            gPipDeviceManager[x].Tips[xTip].St_InitTipStatus();
            gPipDeviceManager[x].Tips[xTip].Wm_Clean;
        end;
    end;
end;

class procedure TDeviceInitHandling.AllArmsStoreTips;
var
    x: integer;
begin
    for x := 0 to gPipDeviceManager.Count - 1 do
    begin
        if Assigned(gPipDeviceManager[x]) then
            gPipDeviceManager[x].StoreTips();
    end;
end;

class procedure TDeviceInitHandling.HardwareInitExtended;
var
    xInitID: TDateTime;
begin
    xInitID := TDeviceInitHandling.GenerateInitID();

    TLiquidHandlingDiTi.DropAllDispTips;

    InitializeSyringes(xInitID);

    // Waage abräumen ind initialisieren
    PrepareBalanceAndWait(xInitID);
end;

class procedure TDeviceInitHandling.ConnectAndInit(aHardwareInit: boolean);
begin
    InitializeInitID();

    TDevicesConflictManager.Instance.SetXConflictResolveMode(xcMoveAway);

    TToolHandling.ResetAllToolData();

    gErrorManager.ResetGlobalErr();

    TActionModules.ConnectModules();

    if aHardwareInit then
        HardwareInitBasic;
end;

class procedure TDeviceInitHandling.ConnectAndInitRun(aHardwareInit: boolean);
var
    xStoredChangeTypes: Boolean;
begin
    ConnectAndInit(aHardwareInit);

    // Check change of tip types
    xStoredChangeTypes := gChangeTipTypes;
    gChangeTipTypes := false;
    try
        TPeripheryManager.Instance.RefreshLayoutDeviceList;
        AllArmsStoreTips;
    finally
        gChangeTipTypes := xStoredChangeTypes;
    end;

    if aHardwareInit then
        HardwareInitExtended;

    InitTipStatus_AllArms();

    TDevicesConflictManager.Instance.SetXConflictResolveMode(xcMoveAwayCheckID);
end;

class procedure TDeviceInitHandling.InitializeSyringes(aInitID: TDateTime);
var
    xDevPump: ISwitchDevice;
    i, xSearchIndex: integer;
    xTipUsageArray: TTipUsageRecArray;
    xArm: IArmDevice;
    xRack: TRack;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;

    if TLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT;

    xSearchIndex := 0;
    while TPeripheryManager.Instance.FindNextModule(IArmDevice, xSearchIndex, xArm) do
    begin
        if not Assigned(xArm.PipDevice) then
            CONTINUE;
        if Assigned(xArm.GripDevice) then
            CONTINUE; // Arms that have a GripDevice are not initialized

        xTipUsageArray := xArm.PipDevice.GetInitTipUsage([DefaultTip, DispTip]);

        if (Length(xTipUsageArray) = 0) then
            CONTINUE;

        gLogManager.LogF('%s - Init Syringes', [xArm.Name], true);

        xDevPump := TLiquidHandlingLow.FindDrySwitchDevice(dsdVacuumPump, xArm.PipDevice);
        if Assigned(xDevPump) then
            xDevPump.SwitchOn(false);

        for i := 0 to Length(xTipUsageArray) - 1 do
        begin
            if gErrorManager.IsGlobalErr() then
                EXIT;

            // Init Syringes (in Wastestation)
            xArm.PipDevice.SetUseTips(xTipUsageArray[i].TipTypeName, xTipUsageArray[i].TipMap, false);
            xRack := TLiquidHandlingLow.InitSyringes(xArm, xArm.PipDevice.UseTips, aInitID);
            TLiquidHandlingLow.FlushAfterInit(xArm, xRack, xArm.PipDevice.UseTips);

            // Sophas: Nadeln in Drystation
            TLiquidHandlingLow.Dry(xArm, (xArm.PipDevice.UseTips and xArm.PipDevice.DryAfterFlushTips));
        end;

        if Assigned(xDevPump) then
            xDevPump.SwitchOff(false);

        gmMoveToZTravel(xArm);
    end;
end;


end.
