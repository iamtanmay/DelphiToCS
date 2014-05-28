{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  21.08.13 wl                                      TN6231   Initial Revision
  21.08.13 wl                                      TN6231   enthält Teile aus ContainerHandling
  ----------------------------------------------------------------------------------------------------------- }

unit OperationBalance;


interface


uses
    IntfBalanceDevice,
    AppTypes,
    RackTypes;

type
    TBalanceOperation = class
    protected
        fBalanceDev: IBalanceDevice;
    public
        constructor Create(const aBalanceName: string);
        procedure WaitForBalance(const aWeightStoreKeyName: string; const aIsTimeoutStoreKeyName: string);
    end;

    TWeighPositionOperation = class(TBalanceOperation)
    private
        fRackIDPosition: TRackIDPosition;
        fNumValues: integer;
        fDeviation: double;
        fTarget: double;
        fSubstance: string;
        fStoreAsTare: boolean;
    public
        constructor Create(aRack: TXRackPosition; const aBalanceName: string; aNumValues: integer;
            aDeviation, aTarget: double; aStoreAsTare: boolean);
        procedure WeighPosition();
    end;

    TTareBalanceOperation = class(TBalanceOperation)
    private
        fNumValues: integer;
        fDeviation: double;
    public
        constructor Create(const aBalanceName: string; aNumValues: integer; aDeviation: double);
        procedure Tare();
    end;


implementation


uses
    SysUtils,
    ObjModul,
    ErrorManager,
    LogManager,
    EventManager,
    PosinfoDataAdaptor,
    CommonTypes,
    AppSettings;

{ TBalanceOperation }

constructor TBalanceOperation.Create(const aBalanceName: string);
begin
    inherited Create();
    if aBalanceName <> '' then
        fBalanceDev := gModules.FindBalanceByName(aBalanceName)
    else
        fBalanceDev := gModules.FindBalance;
end;

procedure TBalanceOperation.WaitForBalance(const aWeightStoreKeyName: string;
    const aIsTimeoutStoreKeyName: string);
var
    xWeight: double;
    xIsTimeout: boolean;
    xIsTimeoutAsStr: string;
    xStoreResults: boolean;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;
    if not Assigned(fBalanceDev) then
        EXIT;

    gLogManager.LogF('Wait for balance %s', [fBalanceDev.Name], true);
    xStoreResults := fBalanceDev.Wait(xWeight, xIsTimeout);

    if xStoreResults then
    begin
        if (aWeightStoreKeyName <> '') then
            TEventManager.Instance.StoreParserIdentRWValue(aWeightStoreKeyName, FloatToStr(xWeight));

        if (aIsTimeoutStoreKeyName <> '') then
        begin
            xIsTimeoutAsStr := '0';
            if xIsTimeout then
                xIsTimeoutAsStr := '1';
            TEventManager.Instance.StoreParserIdentRWValue(aIsTimeoutStoreKeyName, xIsTimeoutAsStr);
        end;
    end;

    fBalanceDev.OpenDoor;
end;

{ TWeighPositionOperation }

constructor TWeighPositionOperation.Create(aRack: TXRackPosition; const aBalanceName: string;
    aNumValues: integer; aDeviation, aTarget: double; aStoreAsTare: boolean);
const
    cDefNameDestination = 'Destination';
begin
    inherited Create(aBalanceName);

    fRackIDPosition := gmXRackPosToRackIDPos(aRack);
    fNumValues := aNumValues;
    fDeviation := aDeviation;
    fTarget := aTarget;
    fStoreAsTare := aStoreAsTare;
    fSubstance := cDefNameDestination;
end;

procedure TWeighPositionOperation.WeighPosition();
var
    xPosinfoDA: TPosinfoDataAdaptor;
    xIniAccess: IWinLissyIniAccess;
    xUseLastSubstID: boolean;
begin
    if not Assigned(fBalanceDev) then
        Exit;

    xIniAccess := gCommonDll.CreateRobotIni;
    xUseLastSubstID := xIniAccess.ReadBool('WGHP', 'UseLastSubstID');

    if (xUseLastSubstID) then
    begin
        xPosinfoDA := TPosInfoDataAdaptor.Create();
        try
            xPosinfoDA.GetLastSubstID(fSubstance, fRackIDPosition.RackID, fRackIDPosition.Pos);
        finally
            xPosinfoDA.Free;
        end;
    end;

    // fBalanceDev.OnAfterStoreWeight := self.CalcAndStoreNetWeight;
    fBalanceDev.StartWeight(fRackIDPosition.RackID, fRackIDPosition.Pos, fSubstance, fNumValues, fDeviation,
        fTarget, fStoreAsTare);

end;

constructor TTareBalanceOperation.Create(const aBalanceName: string; aNumValues: integer; aDeviation: double);
begin
    inherited Create(aBalanceName);

    fNumValues := aNumValues;
    fDeviation := aDeviation;
end;

procedure TTareBalanceOperation.Tare();
begin
    if not Assigned(fBalanceDev) then
        Exit;
    if (gErrorManager.IsGlobalErr) then
        Exit;

    fBalanceDev.StartTare(fNumValues, fDeviation);
end;


end.
