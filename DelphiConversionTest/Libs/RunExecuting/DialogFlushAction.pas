unit DialogFlushAction;
{ ------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  27.05.10 wl                                    TN5116   von dlgFlush getrennt
  16.11.10 wl                                    TN5351   uses ActionLow entfernt
  ------------------------------------------------------------------------------------------------------------ }


interface


uses
    Graphics,
    StdCtrls,
    AppTypes,
    EdExtern,
    Action,
    IntfArmDevice;

type
    // ---------------------------------------------------------------------------------- TFlushThread
    TDialogFlushAction = class(TAction)
    private
        fPipDeviceName: string;
        fTipMap: TIPMAP;
        fAddDilutorMap: TIPMAP;
        FFlushChecks: TFlushThrChecks;
        FCheckBox: TCheckBox;
        fVolume: single;
        fCycles: integer;
        fUseCh1, fUseCh2: boolean;
        fUsePeriPump: boolean;
        fUseAddDilutors: boolean;
        procedure SyncSetColor(aColor: TColor);
        procedure FlushDiluent(aDilIndex: integer);
    public
        constructor Create(const aPipDeviceName: string; aTipMap: TIPMAP; aAddDilutorMap: TIPMAP;
            aFlushThrChecks: TFlushThrChecks; aVolume: double; aCycles: integer; aUseCh1, aUseCh2: boolean;
            aUsePeriPump, aUseAddDilutors: boolean);
        procedure ExecFirst(); override;
        procedure ExecLast(); override;
    end;


implementation


uses
    SysUtils,
    GUIManager,
    ObjModul,
    ThrdMan,
    ObjModulA,
    ErrorManager,
    SamIntf,
    ExecHandler,
    PipDeviceManager,
    ControlUtils,
    SubstanceHandling,
    IntfPipDevice,
    LogManager,
    GeneralTypes;

{ TDialogFlushAction }

constructor TDialogFlushAction.Create(const aPipDeviceName: string; aTipMap: TIPMAP; aAddDilutorMap: TIPMAP;
    aFlushThrChecks: TFlushThrChecks; aVolume: double; aCycles: integer; aUseCh1, aUseCh2: boolean;
    aUsePeriPump, aUseAddDilutors: boolean);
begin
    inherited Create();
    fPipDeviceName := aPipDeviceName;
    fTipMap := aTipMap;
    fVolume := aVolume;
    fCycles := aCycles;
    fUseCh1 := aUseCh1;
    fUseCh2 := aUseCh2;
    fUsePeriPump := aUsePeriPump;
    fFlushChecks := aFlushThrChecks;
    fUseAddDilutors := aUseAddDilutors;
    fAddDilutorMap := aAddDilutorMap;
end;

procedure TDialogFlushAction.SyncSetColor(aColor: TColor);
begin
    gGUIManager.SetControlProp(FCheckBox, cpColor, aColor);
end;

function GetTipMapFromNumber(aDilCount: integer): TIPMAP;
var
    x: integer;
begin
    result := 0;
    for x := 0 to aDilCount - 1 do
        result := result or (1 shl x);
end;

procedure TDialogFlushAction.FlushDiluent(aDilIndex: integer);
var
    xAddDilutorMap: integer;
    xPipDevice: IPipDevice;
    xUsedArm: IArmDevice;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;
    if (not FFlushChecks[aDilIndex].Enabled) or (not FFlushChecks[aDilIndex].Checked) then
        EXIT;

    FCheckBox := FFlushChecks[aDilIndex];
    SyncSetColor(clGreen);

    xAddDilutorMap := 0;
    // { TODO : add dilutors }if ( fUseAddDilutors ) then
    // xAddDilutorMap := GetTipMapFromNumber( fAddDilArm.GetNoOfAddDilutors() );

    xPipDevice := gPipDeviceManager.FindPipDevice_ByName(fPipDeviceName);
    if not(Assigned(xPipDevice)) then
        raise Exception.CreateFmt('Pipdevice [%s] not found', [fPipDeviceName]);
    xUsedArm := gModules.FindArmByPipDevice(xPipDevice);

    SubstanceHandling.gmExecuteFlush(xUsedArm, aDilIndex, fCycles, fVolume, fUsePeriPump, fUseCh1, fUseCh2,
        fTipMap, xAddDilutorMap);

    SyncSetColor(clBtnFace);
end;

// --------------------------------------------------------------------------------------------------
procedure TDialogFlushAction.ExecFirst();
// --------------------------------------------------------------------------------------------------
var
    x: integer;
begin
    TLogManager.Instance.Log('Dialog Flush Action : BEGIN', true);
    gGUIManager.SetStatusBar(TLanguageString.Read('Flush System', 'System spülen'));

    if (fUsePeripump) then
        FlushDiluent(1) // bei Peripump nicht jedes SystemLiquid - das geht nicht!
    else
        for x := 0 to high(fFlushChecks) do
            FlushDiluent(x);

    gGUIManager.SetStatusBar('');
end;

// ------------------------------------------------------------------------------
procedure TDialogFlushAction.ExecLast();
// ------------------------------------------------------------------------------
begin
    gLogManager.Log('Dialog Flush Action : END', true);
end;


end.
