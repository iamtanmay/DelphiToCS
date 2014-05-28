{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : These are functions used by the action 'WGWIZ'
  and require Weighing and PowderHandling (no Tube- or Platehandling).
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  20.04.05 wl                               TN2377   initial revision
  28.04.05 wl                               TN2377   leicht geändert
  02.05.05 wl                               TN2377   grundsätzliches Waschen entfernt
  13.05.05 wl  gmWeighPosition              TN2377   es wird immer Tara gewogen (sonst wird das erste Gewicht falsch berechnet
  13.05.05 wl  gmWeighPosition              TN2377   VERBESSERUNGSWÜRDIG: es wird wird ein Gewicht von 0.01 eingetragen (nur um IBTools zu überlisten)
  13.05.05 wl  gmCalliPipetteAndWeighPowder TN2377.2 RestAtRWash: zum einwiegen wird die Nadel über die Waschstation gefahren
  22.05.05 wl                               TN2377   einige Calli-Funktionen --> PowderHandling
  21.06.05 pk                               TN2464.3  St_LhPtr : No longer a pointer - Too dangerous, caused access violations
  24.06.05 wl  gmDoCalli1                   TN2462    bei GetAndWriteWeighData wird SourceTubeID statt SourceRackID übergeben
  17.08.05 wl  gmDoCalli1                   TN2552    im Simulationsmodus wird die Schleife nicht durchlaufen
  17.11.05 wl                               TN2771    gmReadWriteRackID mit Parameter aUsedArm
  30.11.05 wl                               TN2818    statt global_CalliWPRTipName wird aUsedArm.FirstUsedTipName benutzt
  11.01.06 pk                               TN2871.0  St_ClearTipStatus : ResetAll param removed
  20.01.06 pk  gmDoCalli1                   TN2862    Now supports Aps and Disp events
  16.07.06 wl  gmDoCalli1                   TN3198    aCalli.CalcNextStep Statt RackID wird wieder TubeID übergeben
  16.07.06 wl  gmDoCalli1                   TN3199.1  das unsinnige Hinzufügen von 0.01 entfernt
  07.09.06 pk                               TN3292    use of PosinfodataAdaptor instance removed
  07.09.06 wl                               TN3287    MASSP action komplett überarbeitet --> WGWIZ
  07.09.06 wl  gmChangeMassPipetteTips      TN3287    IBTools bestimmt LiqH-Param, LiqHParam bestimmt das Tool
  13.09.06 wl                               TN3287    Calli-Funktionen aus PowderHandling hierher
  26.09.06 wl                               TN3326    Einige Bugs beseitigt
  27.09.06 wl  gmChangeWeighWizardTips      TN3326    SubstID kann wieder an LiqH-Namen angehängt werden
  03.10.06 wl                               TN3326    Diverse Änderungen für V 7.1.2
  18.10.06 wl  TWeighDbDllAdaptor           TN3362    jetzt als Basisklasse für TWeighDbDllAdaptor_Teaching & TWeighDbDllAdaptor_TargetWeight
  18.10.06 wl  TWeighDbDllAdaptor_Teaching  TN3362    neuer Modus: Volumen und Powder Parameter sind vorgegeben, GetTool wird nicht aufgerufen
  18.10.06 wl                               TN3362    viele Änderungen für neuen Teaching-Modus
  21.11.06 wl  gmTarePosition               TN3419    wenn UseLastWeighAsTare und Rackname wie vorher benutze letzte Einwaage als Tara
  21.11.06 wl  uLastWeight                  TN3419    Merker für die letzte getätigte Einwaage
  23.03.07 pk  gmTipsNeedingWash            TN3546    TipNeedsWash funciton parameters changed
  27.07.07 wl  InsertWeighDataWithTarget    TN3709.2  wenn IsStored = false, wird das Nettogewicht nicht geändert
  27.07.07 wl  gmDoWeighWizardWithWeighing  TN3709.2  Jetzt sind auch Retry und Ignore möglich
  30.08.07 pk                               TN3840.2  Uses LiqHTypes
  04.09.07 pk  gmTarePosition               TN3847    SetGlobalErr and ErrBox functions called here instead of in PosinfoDataAdaptor
  06.09.07 wl  gmDoWeighWizardWithWeighing  TN3828    neu: xTareStep ergibt sich beim Tarieren und wird an gmCalcNextWeight übergeben
  06.09.07 wl  gmTarePosition               TN3828   jetzt mit Rückgabewert: Step-Nummer
  06.09.07 wl  gmCalcNetWeight              TN3828    statt FirstWeighStep wird der Tara-Wert dieses WGWIZ-Schrittes gelesen
  09.11.07 pk                               TN3924    Steps changed to mm
  09.11.07 pk                               TN3923    class references to TErrorMessageFactory changed to gErrorMessageFactory
  27.11.07 wl  gmChangeWeighWizardTips      TN3897    geänderte Parameter bei ExecuteWash
  09.01.08 wl                               TN3972   interne Änderungen
  29.01.08 wl                               TN3980   uses geändert
  20.06.08 pk                               TN4139    WB global object replaced by LayoutManager
  03.07.08 wl                               TN4157
  09.07.08 pk                               TN4157    Call FindArmToUse with PipDevice Name
  08.09.08 pk                               TN4215    References to IBtools disabled for now
  25.09.08 wl                               TN4242    TRunstCall ersetzt TDllCall
  31.07.09 wl  gmDefineWashJob              TN4049    an änderungen in TWashRec angepasst
  08.09.09 pk                               TN4753    uses ErrorMessage replaced by ErrorInfo
  12.09.09 wl                               TN4740    TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  21.09.09 pk  gmDoWeighWizardPowder        TN4740    MaxTips replaced by TipCount
  28.05.10 wl                               TN5116   uses geändert
  15.12.11 wl                               TN5767   uses geändert
  02.02.11 wl  alle Funktionen              TN5791   verwendet TArray<TXRackPosition> statt TRack und Pos-Array
  25.04.13 wl  TWashJob                     TN6139.1 DestVol & DilRack entfernt
  15.08.13 wl  FindArmToUse                 TN6217   von SubstanceHandling hierher
  20.08.13 wl                               TN6231   an Änderungen in ToolHandling angepasst
  -------------------------------------------------------------------------------------------------- }

unit WeighWizardHandling;


interface


uses
    AppTypes,
    PowderHandling,
    IntfArmDevice,
    LiqHTypes,
    MethodTypes,
    WashHandling,
    MethodStepSettingRunStart,
    RackTypes;

type
    TWeighDbDllAdaptor = class
    private
        fSamDeviceName: PChar;
        fSubstanceID: string;
        fLastNetWeight: double;
        fProcessID: integer;
        fRunID: integer;
        fRunName: PChar;
        // result:
        fParamName: string;
        fDoWeigh: boolean;
        //
    protected
        function GetNextDispCount: integer; virtual; abstract;
        function GetNextVarixSteps(index: Integer): integer; virtual; abstract;
        function GetNextVolume(index: Integer): double; virtual; abstract;
        function InsertWeighDataWithTarget(aStep: integer; aTargetWeight, aCurrentNetWeight: double;
            var vIsStored: boolean): boolean;
    public
        constructor Create(const aSamDeviceName, aSubstanceID: string; const aRunName: string;
            aRunID: integer);
        //
        function GetTool(): boolean; virtual; abstract;
        function InsertWeighData(aStep: integer; aCurrentNetWeight: double): boolean; virtual; abstract;

        property SubstanceID: string read fSubstanceID write fSubstanceID;
        property ParamName: string read fParamName;
        property NextVolume[index: Integer]: double read GetNextVolume;
        property NextVarixSteps[index: Integer]: integer read GetNextVarixSteps;
        property NextDispCount: integer read GetNextDispCount;
        property DoWeigh: boolean read fDoWeigh;
    end;

    TWeighDbDllAdaptor_TargetWeight = class(TWeighDbDllAdaptor)
    private
        fTargetWeight: double;
        fToolResult: TCWGetToolResult;
    protected
        function GetNextDispCount: integer; override;
        function GetNextVarixSteps(index: Integer): integer; override;
        function GetNextVolume(index: Integer): double; override;
    public
        constructor Create(const aSamDeviceName, aSubstanceID: string; aTargetWeight: double;
            const aRunName: string; aRunID: integer);
        //
        function GetTool(): boolean; override;
        function InsertWeighData(aStep: integer; aCurrentNetWeight: double): boolean; override;
    end;

    TWeighDbDllAdaptor_Teaching = class(TWeighDbDllAdaptor)
    private
        fTeachVol: double;
        fTeachParam: string;
        fFirstDisp: boolean;
    protected
        function GetNextDispCount: integer; override;
        function GetNextVarixSteps(index: Integer): integer; override;
        function GetNextVolume(index: Integer): double; override;
    public
        constructor Create(const aSamDeviceName, aSubstanceID: string; const aRunName: string;
            aRunID: integer; aTeachVol: double; const aTeachParam: string);
        //
        function GetTool(): boolean; override;
        function InsertWeighData(aStep: integer; aCurrentNetWeight: double): boolean; override;
    end;

    TWeighWizardProcess = class
    private
        fDllAdaptor: TWeighDbDllAdaptor;
        fSource: TXRackPosition;
        fDest: TXRackPosition;
        fRestAtRWash: integer;
        function GetDoWeigh: boolean;
        function GetNextDispCount: integer;
        function GetNextVarixSteps(index: Integer): integer;
        function GetNextVolume(index: Integer): double;
        function GetParamName: string;
    public
        constructor Create(const aRunName: string; aSRack, aDRack: string; aSRackPos, aDRackPos: integer;
            aTargetWeight, aTeachVol: double; const aTeachParam: string);
        destructor Destroy(); override;
        //
        procedure GetPipTool();
        function GetAndWriteWeighData(aStep: integer; aNetWeight: double): boolean;
        property ParamName: string read GetParamName;
        property NextVolume[index: Integer]: double read GetNextVolume;
        property NextVarixSteps[index: Integer]: integer read GetNextVarixSteps;
        property NextDispCount: integer read GetNextDispCount;
        property DoWeigh: boolean read GetDoWeigh;
    end;

procedure gmWeighWizardResetLastWeight();

procedure gmDoWeighWizard(const aRunName: string; const aSRack, aDRack: string; aSRackPos, aDRackPos: integer;
    aDestMass, aTeachVol: double; aWashManager: TWashHandling; const aAspRunRec: TAspirateEvRunRec;
    const aDispRunRec: TDispenseEvRunRec; const aTeachParam: string; aUseLastWeighAsTare: boolean);


implementation


uses
    Windows,
    Math,
    SysUtils,
    LogManager,
    ErrorManager,
    TipMapUtils,
    PosInfoDataAdaptor,
    LiqHDataAdaptor,
    PlateHandling,
    AppSettings,
    IntfBalanceDevice,
    SamGlobe,
    ObjModul,
    ToolHandling,
    RunFlow,
    TipTypeDataAdaptor,
    LayoutManager,
    CommonTypes,
    IntfPipDevice,
    PipDeviceManager,
    Rack,
    MethodGUIParsing,
    PowderHandlingHigh,
    TipSystem,
    SamHigh,
    IntfMotorBasedMotionDevice,
    ErrorInfo,
    IntfMotorDriver,
    ErrorMessageFactory;

{ TWeighLogicDbDllAdaptor }

constructor TWeighDbDllAdaptor.Create(const aSamDeviceName, aSubstanceID: string; const aRunName: string;
    aRunID: integer);
begin
    inherited Create;

    fSamDeviceName := PChar(aSamDeviceName);
    fSubstanceID := aSubstanceID;
    fRunID := aRunID;
    fRunName := PChar(aRunName);

    fLastNetWeight := 0;
    fProcessID := 0;

end;

function TWeighDbDllAdaptor.InsertWeighDataWithTarget(aStep: integer;
    aTargetWeight, aCurrentNetWeight: double; var vIsStored: boolean): boolean;
begin
    ASSERT(false, 'Not implemented');
    result := false;
    { TODO -oPK -cAction package : }
    {
      var
      xStoredWeight: double;
      begin
      // nur das speichern, was beim letzten Dispense dazugekommen ist
      xStoredWeight := aCurrentNetWeight - fLastNetWeight;

      // scheinbar soll der gespeicherte Wert nicht mehr als 2 Nachkommastellen haben
      xStoredWeight := ROUND( xStoredWeight * 100 ) / 100;

      result := CWInsertWeighData( fSamDeviceName, PChar( fParamName ), PChar( fSubstanceID ), PChar( fParamName ),
      fRunName, fRunID, fProcessID, aStep, xStoredWeight, aTargetWeight, vIsStored );

      // Net weight speichern
      if ( vIsStored ) then
      fLastNetWeight := aCurrentNetWeight;
    }
end;

{ TWeighLogicDbDllAdaptor_TargetWeight }

constructor TWeighDbDllAdaptor_TargetWeight.Create(const aSamDeviceName, aSubstanceID: string;
    aTargetWeight: double; const aRunName: string; aRunID: integer);
begin
    inherited Create(aSamDeviceName, aSubstanceID, aRunName, aRunID);

    fTargetWeight := aTargetWeight;
end;

function TWeighDbDllAdaptor_TargetWeight.GetTool(): boolean;
begin
    ASSERT(false, 'Not implemented');
    result := false;
    { TODO -oPK -cAction package : }
    {
      result := CWGetTool( fSamDeviceName, PChar( fSubstanceID ), fProcessID, fTargetWeight, fToolResult );

      // Kunstgriff: Toolname in IBTools = Powder Handling Parameter Name in WinLissy

      fParamName := fToolResult.Tool; // alternativ: fToolResult.ParamSet.Name (bringt dasselbe)
      fDoWeigh := fToolResult.DoWeigh;
    }
end;

function TWeighDbDllAdaptor_TargetWeight.GetNextDispCount: integer;
begin
    if (fDoWeigh) then
    begin
        if (fToolResult.Step <> -1) then
            result := 1
        else
            result := 0;
    end
    else
    begin
        result := high(fToolResult.StepArray) + 1;
    end;
end;

function TWeighDbDllAdaptor_TargetWeight.GetNextVarixSteps(index: Integer): integer;
begin
    if (fDoWeigh) then
    begin
        result := fToolResult.Step;
    end
    else
    begin
        result := fToolResult.StepArray[index];
    end;
end;

function TWeighDbDllAdaptor_TargetWeight.GetNextVolume(index: Integer): double;
begin
    if (fDoWeigh) then
    begin
        result := fToolResult.CorrectVol;
    end
    else
    begin
        result := fToolResult.CorrectVolArray[index];
    end;
end;

function TWeighDbDllAdaptor_TargetWeight.InsertWeighData(aStep: integer; aCurrentNetWeight: double): boolean;
var
    xIsStored: boolean;
begin
    result := InsertWeighDataWithTarget(aStep, fTargetWeight, aCurrentNetWeight, xIsStored);
end;

{ TWeighLogicDbDllAdaptor_Teaching }

constructor TWeighDbDllAdaptor_Teaching.Create(const aSamDeviceName, aSubstanceID, aRunName: string;
    aRunID: integer; aTeachVol: double; const aTeachParam: string);
begin
    inherited Create(aSamDeviceName, aSubstanceID, aRunName, aRunID);
    fTeachVol := aTeachVol;
    fTeachParam := aTeachParam;
    fFirstDisp := true;
end;

function TWeighDbDllAdaptor_Teaching.GetNextDispCount: integer;
begin
    if (fFirstDisp) then
        result := 1
    else
        result := 0;
end;

function TWeighDbDllAdaptor_Teaching.GetNextVarixSteps(index: Integer): integer;
begin
    result := -1;
end;

function TWeighDbDllAdaptor_Teaching.GetNextVolume(index: Integer): double;
begin
    result := fTeachVol;
end;

function TWeighDbDllAdaptor_Teaching.GetTool(): boolean;
begin
    result := true;
    fParamName := fTeachParam;
    fDoWeigh := true;
end;

function TWeighDbDllAdaptor_Teaching.InsertWeighData(aStep: integer; aCurrentNetWeight: double): boolean;
var
    xIsStored: boolean;
begin
    result := InsertWeighDataWithTarget(aStep, aCurrentNetWeight, aCurrentNetWeight, xIsStored);
    if (xIsStored) then
        fFirstDisp := false;
end;

{ TWeighWizardProcess }

constructor TWeighWizardProcess.Create(const aRunName: string; aSRack, aDRack: string;
    aSRackPos, aDRackPos: integer; aTargetWeight, aTeachVol: double; const aTeachParam: string);
var
    xIniAccess: IWinlissyIniAccess;
    xSubstanceID: string;
begin
    inherited Create;

    FSource := TLayoutManager.Instance.CurrentLayout.FindXRackPos(aSRack, aSRackPos, true);
    FDest := TLayoutManager.Instance.CurrentLayout.FindXRackPos(aDRack, aDRackPos, true);

    // Check: Source und dest müssen Rack-ID haben
    if (fSource.Rack.RackID = '') then
        gErrorManager.SetGlobalErr(ERR_APPLICATION, Format('Rack-ID of source rack %s must be defined',
            [fSource.Rack.RackID]));
    if (fDest.Rack.RackID = '') then
        gErrorManager.SetGlobalErr(ERR_APPLICATION, Format('Rack-ID of destination rack %s must be defined',
            [fDest.Rack.RackID]));

    xSubstanceID := TPosinfoDataAdaptor.GetTubeID(gmXRackPosToRackIDPos(fSource));

    if (aTeachVol > 0) then
    begin
        fDllAdaptor := TWeighDbDllAdaptor_Teaching.Create(gSamDeviceName, xSubstanceID, aRunName, 0,
            aTeachVol, aTeachParam);
    end
    else
    begin
        fDllAdaptor := TWeighDbDllAdaptor_TargetWeight.Create(gSamDeviceName, xSubstanceID, aTargetWeight,
            aRunName, 0)
    end;

    xIniAccess := gCommonDll.CreateAppIni;
    fRestAtRWash := xIniAccess.ReadInteger('Calli', 'RestAtRWash');
end;

destructor TWeighWizardProcess.Destroy;
begin
    fDllAdaptor.Free;

    inherited;
end;

procedure TWeighWizardProcess.GetPipTool();
begin
    fDllAdaptor.GetTool();
end;

function TWeighWizardProcess.GetAndWriteWeighData(aStep: integer; aNetWeight: double): boolean;
begin
    result := fDllAdaptor.InsertWeighData(aStep, aNetweight);
end;

function TWeighWizardProcess.GetDoWeigh: boolean;
begin
    result := fDllAdaptor.DoWeigh;
end;

function TWeighWizardProcess.GetNextDispCount: integer;
begin
    result := fDllAdaptor.NextDispCount;
end;

function TWeighWizardProcess.GetNextVarixSteps(index: Integer): integer;
begin
    result := fDllAdaptor.NextVarixSteps[index];
end;

function TWeighWizardProcess.GetNextVolume(index: Integer): double;
begin
    result := fDllAdaptor.NextVolume[index];
end;

function TWeighWizardProcess.GetParamName: string;
begin
    result := fDllAdaptor.ParamName;
end;


// ==================================================================================================
// allgemeine Calli-Funktionen
// ==================================================================================================

var
    uLastWeight: double = 0;
    uLastRackName: string = '';

procedure gmStoreLastWeight(xLastWeight: double; xLastRackName: string);
begin
    uLastWeight := xLastWeight;
    uLastRackName := xLastRackName;
end;

procedure gmWeighWizardResetLastWeight();
begin
    uLastWeight := 0;
    uLastRackName := '';
end;

function gmCalcNetWeight(aDPos: TXRackPosition; out oWeighUnit: integer; aTareStep: integer): double;
var
    xLastWeight, xFirstWeight: double;
    xDataAdaptor: TPosinfoDataAdaptor;
begin
    xDataAdaptor := TPosinfoDataAdaptor.Create();
    try
        xDataAdaptor.GetLastTareWeightAndUnit(aDPos.Rack.RackID, aDPos.Pos, xLastWeight, oWeighUnit);
        xDataAdaptor.GetAnyWeightAndUnit(aDPos.Rack.RackID, aDPos.Pos, aTareStep, xFirstWeight, oWeighUnit);
    finally
        xDataAdaptor.Free;
    end;

    gmStoreLastWeight(xLastWeight, aDPos.Rack.Name);
    result := xLastWeight - xFirstWeight;
end;

procedure gmJustMoveXUpOffset(aUsedArm: IArmDevice; aBalanceRack: TRack);
var
    xOffsetPos: TPosMM;
    xMotorBasedMD: IMotorBasedMotionDevice;
begin
    if not Assigned(aBalanceRack) then
        EXIT;
    if gErrorManager.IsGlobalErr() then
        EXIT;

    xOffsetPos := aBalanceRack.Structure.MOffsetUpX_mm;

    gmMoveToZTravel(aUsedArm);

    if not Supports(aUsedArm.MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
        EXIT;
    xMotorBasedMD.XMotor.Move(xOffsetPos, m_EXECUTE, AT_MOVE_REL); // HACK
end;

procedure gmCalliTare(aUsedArm: IArmDevice; aBalanceDev: IBalanceDevice; aTips: TIPMAP;
    aRestAtRWash: integer);
var
    xSwitchTips: TIPMAP;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;
    if (aBalanceDev = nil) then
        EXIT;

    xSwitchTips := 0;
    case (aRestAtRWash) of
        2:
            xSwitchTips := TPowderHandling.RestAtRediWash(aUsedArm, aTips, true);
    end;

    aBalanceDev.StartTare(0, 0);
    aBalanceDev.OpenDoor;

    case (aRestAtRWash) of
        2:
            TPowderHandling.SwitchRediPump(aUsedArm.PipDevice, xSwitchTips, true); // Pumpe wieder anschalten
    end;
end;

procedure gmWeighDestAfterPipetting(aUsedArm: IArmDevice; aBalanceDev: IBalanceDevice; aTips: TIPMAP;
    aDestPos: TXRackPosition; aBalanceRack: TRack; aRestAtRWash: integer);
var
    xSwitchTips: TIPMAP;
    xRackIDPos: TRackIDPosition;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;
    if (aBalanceDev = nil) then
        EXIT;

    xSwitchTips := 0;
    case (aRestAtRWash) of
        1:
            TPowderHandling.RestAtRediWash(aUsedArm, aTips, false);
        2:
            xSwitchTips := TPowderHandling.RestAtRediWash(aUsedArm, aTips, true);
        9:
            gmJustMoveXUpOffset(aUsedArm, aBalanceRack);
    end;

    xRackIDPos := gmXRackPosToRackIDPos(aDestPos);
    aBalanceDev.StartWeight(xRackIDPos.RackID, xRackIDPos.Pos, 'Destination', 0, 0, 0);
    aBalanceDev.OpenDoor;

    case (aRestAtRWash) of
        2:
            TPowderHandling.SwitchRediPump(aUsedArm.PipDevice, xSwitchTips, true); // Pumpe wieder anschalten
    end;
end;

function gmTarePosition(aUsedArm: IArmDevice; aBalanceDev: IBalanceDevice; aTips: TIPMAP;
    aRestAtRWash: integer; aDest: TXRackPosition; aUseLastWeighAsTare: boolean): integer;
// Rückgabewert: Step
var
    xRackIDPosition: TRackIDPosition;
    xTareWeight: double;
    xUnit: Integer;
    xBalanceDev: IBalanceDevice;
    xPosinfoDA: TPosinfoDataAdaptor;
    xDoTare: boolean;
begin
    result := 0;
    xBalanceDev := gModules.FindBalance;
    if not Assigned(xBalanceDev) then
        Exit;

    xRackIDPosition := gmXRackPosToRackIDPos(aDest);

    xPosinfoDA := TPosinfoDataAdaptor.Create();
    try
        xDoTare := true;
        if (aUseLastWeighAsTare) and (uLastRackName = aDest.Rack.Name) then
        begin
            xTareWeight := uLastWeight;
            xDoTare := false;
        end;

        if (xDoTare) then
        begin
            gmCalliTare(aUsedArm, aBalanceDev, aTips, aRestAtRWash);
            xPosinfoDA.GetBalanceTareValByPos(xBalanceDev.Name, 0, xTareWeight, xUnit);
        end;

        if not xPosinfoDA.WriteSubstWeight(xBalanceDev.Name, xTareWeight, xRackIDPosition.RackID,
            xRackIDPosition.Pos, 'Destination', result) then
        begin
            // ganz schön hart: kein Tara - GlobalError wird gesetzt!
            gErrorManager.SetGlobalErr(ERR_APPLICATION, '');
            gErrorMessageFactory.ErrBoxSimple('Could not store weight',
                Format('No tare weight found for Rack [%s], Pos [%d]', [xRackIDPosition.RackID,
                xRackIDPosition.Pos]), eibAbort);
        end;

    finally
        xPosinfoDA.Free;
    end;
end;

function gmDefineWashJob(aSource: TXRackPosition; aVolume: double; aLiqH: TLiqHandlingRec): TWashJob;
begin
    // TWashJob definieren
    result.IsPipAction := true;
    result.SRack := aSource.Rack.Name;
    result.SPos := aSource.Pos;
    result.SourceVol := aVolume;
    result.DilRackVol := 0;
    result.DispMixVol := 0;
    result.SampleAspMixVol := 0;
    result.Wash := aLiqH.Wash;
    result.UsedTipType := aLiqH.UsedTipType;
end;

procedure gmDoWeighWizardPowder(aUsedArm: IArmDevice; aWashJob: TWashJob; aSource, aDest: TXRackPosition;
    aVolume: double; var vVarixMotorSteps: integer; aLiqH: TLiqHandlingRec;
    const aAspRunRec: TAspirateEvRunRec; const aDispRunRec: TDispenseEvRunRec);
var
    i: Integer;
    xArmTipIndex: integer;
    xPowderSource, xPowderDest: TArray<TXRackPosition>;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;

    xArmTipIndex := -1;
    for i := 0 to aUsedArm.PipDevice.TipCount - 1 do
        if (((aUsedArm.PipDevice.UseTips shr i) and 1) = 1) then
            xArmTipIndex := i;
    if (xArmTipIndex < 0) then
        EXIT;

    gLogManager.LogF('WGWIZ: Volume: %8g µL', [aVolume], true);

    xPowderSource := TXRackPositionUtils.GetEmptyXRackPositions(aUsedArm.PipDevice.TipCount);
    xPowderSource[xArmTipIndex].Rack := aSource.Rack;
    xPowderSource[xArmTipIndex].Pos := aSource.Pos;
    xPowderDest := TXRackPositionUtils.GetEmptyXRackPositions(aUsedArm.PipDevice.TipCount);
    xPowderDest[xArmTipIndex].Rack := aDest.Rack;
    xPowderDest[xArmTipIndex].Pos := aDest.Pos;

    // Aspirate und Dispense-volumen eintragen
    aUsedArm.PipDevice.St_ClearTipStatus();
    aUsedArm.PipDevice.Tips[xArmTipIndex].St_SVol := aVolume;
    aUsedArm.PipDevice.Tips[xArmTipIndex].St_DVol := aVolume;
    aUsedArm.PipDevice.Tips[xArmTipIndex].St_LhPtr := aLiqH;
    aUsedArm.PipDevice.Tips[xArmTipIndex].Wm_SaveJobToTip(aWashJob, wtPowder); // als TWashJob speichern

    // Verteilung ausführen
    gmAspiratePowder(aUsedArm, aUsedArm.PipDevice.UseTips, xPowderSource, vVarixMotorSteps, aAspRunRec);
    gmDispensePowder(aUsedArm, aUsedArm.PipDevice.UseTips, xPowderDest, aDispRunRec);
end;

function gmTipsNeedingWash(aUsedArm: IArmDevice; aArmTipIndex: integer; aWashJob: TWashJob): TIPMAP;
begin
    result := 0; // No tips will be washed

    // Which tips are needed in the next step but are dirty?
    if aUsedArm.PipDevice.Tips[aArmTipIndex].Wm_TipNeedsWash(aWashJob) then
        TTipMapUtils.SelectTip(result, aArmTipIndex);
end;

procedure gmFindPipDeviceToUse(out oPipDevice: IPipDevice; var vUsedTips: TipMap;
    const aPipDeviceName: string; const aUsedTipType: string);
begin
    oPipDevice := gPipDeviceManager.FindPipDevice_ByName(aPipDeviceName);
    oPipDevice.SetUseTips(aUsedTipType, vUsedTips, true);
    vUsedTips := oPipDevice.UseTips;

    gLogManager.LogF('Tipmap %d - Total tipmap %d - Tip Type: %s',
        [oPipDevice.UseTips, vUsedTips, aUsedTipType], false);
end;

procedure gmFindArmToUse(out oUsedArm: IArmDevice; var vUsedTips: TipMap; const aPipDeviceName: string;
    const aUsedTipType: string);
var
    xPipDevice: IPipDevice;
begin
    gmFindPipDeviceToUse(xPipDevice, vUsedTips, aPipDeviceName, aUsedTipType);
    oUsedArm := gModules.FindArmByPipDevice(xPipDevice);
end;

procedure gmPutCombinedArmTool(aUsedArm: IArmDevice);
begin
    { TODO : combined arm }
    {
      // Tool am CombinedArm immer ablegen! ( Tool am ConcurrentArm (--> SIAS) nicht ablegen! )
      if (aUsedArm.CombinedArm is TGripperArmDevice) then begin
      (aUsedArm.CombinedArm as TGripperArmDevice).PutHandlerTool;
      end;
    }
end;

procedure gmTakeAndPutTool(aUsedArm: IArmDevice);
// var xGrpArm: TGripperArmDevice;
// var
// xPipToolName : string;
begin
    if not Assigned(aUsedArm.GripDevice) then
        exit; // Tool-Handling nur für Gripper-Arm

    // Pipettier-Tool aufnehmen
    { TODO -oPK -cDEVICE : Removalblepipdevice }
    { if (aUsedArm.PipDevice is TRemovablePipDevice) then begin
      xPipToolName := ((aUsedArm.PipDevice as TRemovablePipDevice).PossiblePipToolName );
      gmGetHandlerTool(aUsedArm, xPipToolName);
      end
      else begin
      // andere Tools ablegen
      PutHandlerTool( aUsedArm );
      end;
    }
end;

procedure gmWashCombinedArm(aUsedArm: IArmDevice);
begin
    if (aUsedArm = nil) then
        EXIT;

    { TODO : combined arm }
    {
      // CombinedArm immer waschen! ( ConcurrentArm (--> SIAS) nicht waschen! )
      if (aUsedArm.CombinedArm <> nil) then
      ExecuteWash( aUsedArm.CombinedArm, false, aUsedArm.CombinedArm.DirtyTips );
    }
end;



procedure gmChangeWeighWizardTips(aLiqParName: string; aWashManager: TWashHandling; aSource: TXRackPosition;
    aVolume: double; out oUsedArm: IArmDevice; out oArmTipIndex: integer; out oLiqH: TLiqHandlingRec;
    out oWashJob: TWashJob; const aSubstanceID: string);
var
    xTypeData: TTipType;
    xTipsNeedingWash: TIPMAP;
    xDA: TLiqHDataAdaptor;
    xLQHName: string;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;

    // Kunstgriff: wenn Paramname_SubstID existiert, benutze dies! (das ist noch von Tobias)
    xDA := TLiqHDataAdaptor.Create();
    try
        xLQHName := aLiqParName + '_' + aSubstanceID;
        if (xDA.LiqParamNameExists(xLQHName, false)) then
            aLiqParName := xLQHName;

        // Liquid Handling Parameter bestimmen
        xDA.ReadRec(aLiqParName, oLiqH);
        if (aLiqParName <> oLiqH.PARAMNAME) then
            gErrorManager.SetGlobalErr(ERR_APPLICATION, Format('Powder handling parameter %s does not exist',
                [aLiqParName]));
    finally
        xDA.Free;
    end;

    if (not TTipTypeDataAdaptor.TipTypeExists(oLiqH.UsedTipType, xTypeData)) then
        gErrorManager.SetGlobalErr(ERR_APPLICATION, Format('Tip type %s does not exist',
            [oLiqH.UsedTipType]));

    // jetzt werden die neuen UseTips gesetzt!
    // -- 1 -- UseTips zum 1. Mal bestimmen!!!
    gmFindArmToUse(oUsedArm, oLiqH.UsedTips, oLiqH.UsedPipDevice, oLiqH.UsedTipType);

    if not Assigned(oUsedArm) then
        gErrorManager.SetGlobalErr(ERR_APPLICATION, Format('No Arm found for Tipmap %d, Tip type %s',
            [oLiqH.UsedTips, oLiqH.UsedTipType]));

    // Waschen von Combined Arm und Ablegen von Combined arm tool muss hier passieren
    gmWashCombinedArm(oUsedArm);
    gmPutCombinedArmTool(oUsedArm);

    // Waschen wenn notwendig
    oWashJob := gmDefineWashJob(aSource, aVolume, oLiqH);
    xTipsNeedingWash := gmTipsNeedingWash(oUsedArm, oUsedArm.PipDevice.FirstUsedTipIndex, oWashJob);

    if (xTipsNeedingWash <> 0) then
    begin
        // zum Waschen wird UseTips wieder zurückgesetzt
        oUsedArm.PipDevice.SetUseTips(oUsedArm.PipDevice.LastUseTipType,
            oUsedArm.PipDevice.LastUseTips, true);
        aWashManager.ExecuteWash(oUsedArm, xTipsNeedingWash);
    end;

    // -- 2 -- UseTips wieder auf neueren Stand setzen
    gmFindArmToUse(oUsedArm, oLiqH.UsedTips, oLiqH.UsedPipDevice, oLiqH.UsedTipType);

    // Tool aufnehmen
    gmTakeAndPutTool(oUsedArm);

    // -- 3 -- UseTips wieder auf neueren Stand setzen
    gmFindArmToUse(oUsedArm, oLiqH.UsedTips, oLiqH.UsedPipDevice, oLiqH.UsedTipType);
end;

procedure gmDoWeighWizardWithWeighing(aMassPA: TWeighWizardProcess; aWashManager: TWashHandling;
    const aAspRunRec: TAspirateEvRunRec; const aDispRunRec: TDispenseEvRunRec; aUseLastWeighAsTare: boolean);
var
    xWeighLoopCnt, xArmTipIndex: integer;
    xBalanceDev: IBalanceDevice;
    xUsedArm: IArmDevice;
    xLiqH: TLiqHandlingRec;
    xWashJob: TWashJob;
    xNetWeight, xSumVol: double;
    xWeighUnit, xTareStep: integer;
    xVarixMotorSteps: integer;
begin
    xUsedArm := nil;
    xWeighLoopCnt := 1;
    xBalanceDev := gModules.FindBalance;
    xSumVol := 0;
    xNetWeight := 0;
    xTareStep := INT_POSINFO_STEP_TARE_WEIGHT;

    // Einwaage - Schleife
    while (not gErrorManager.IsGlobalErr) and (aMassPA.NextDispCount > 0) do
    begin

        // UsedTips definieren, Waschen, Tool aufnehmen
        gmChangeWeighWizardTips(aMassPA.ParamName, aWashManager, aMassPA.fSource, aMassPA.NextVolume[0],
            xUsedArm, xArmTipIndex, xLiqH, xWashJob, aMassPA.fDllAdaptor.SubstanceID);

        // Tarieren (beim 1. Schritt)
        if (xWeighLoopCnt = 1) then
            xTareStep := gmTarePosition(xUsedArm, xBalanceDev, xUsedArm.PipDevice.UseTips,
                aMassPA.fRestAtRWash, aMassPA.fDest, aUseLastWeighAsTare);

        // Start mit der Pipettierung
        xVarixMotorSteps := aMassPA.NextVarixSteps[0];
        gmDoWeighWizardPowder(xUsedArm, xWashJob, aMassPA.fSource, aMassPA.fDest, aMassPA.NextVolume[0],
            xVarixMotorSteps, xLiqH, aAspRunRec, aDispRunRec);

        // und wiegen
        gmWeighDestAfterPipetting(xUsedArm, xBalanceDev, xUsedArm.PipDevice.UseTips, aMassPA.fDest,
            aMassPA.fDest.Rack, aMassPA.fRestAtRWash);

        // Gesamtvolumen berechnen
        xSumVol := xSumVol + aMassPA.NextVolume[0];

        // Nettogewicht berechnen
        xNetWeight := gmCalcNetWeight(aMassPA.fDest, xWeighUnit, xTareStep);

        if gErrorManager.IsGlobalErr() then
            EXIT;

        // im Simulationsmodus keine Werte in CalliW-DB eintragen (Debug-Modus: doch!)
        if (gRunFlow.SimulationMode) and (not TAppsettings.IsDebugMode) then
            BREAK;

        // Ergebnis in CalliW-DB eintragen
        if not aMassPA.GetAndWriteWeighData(xVarixMotorSteps, xNetWeight) then
        begin
            case gErrorMessageFactory.ErrBoxSimple('Not enough powder!', 'Weigh Wizard Action',
                eibAbortRetryIgnore) of
                IDIGNORE:
                    EXIT;
                IDABORT:
                    gErrorManager.SetGlobalErr(ERR_USER, '');
            end;
        end;
        if gErrorManager.IsGlobalErr() then
            EXIT;

        // Nächsten Schritt von CalliW-DB erfragen
        aMassPA.GetPipTool();

        inc(xWeighLoopCnt);
    end;

    // Wiege-Ergebnis schreiben
    TPosinfoDataAdaptor.WriteWeight(aMassPA.fDest.Rack.RackID, aMassPA.fSource.Rack.RackID,
        '[' + xBalanceDev.Name + ']', aMassPA.fDest.Pos, cPosinfoNetWeight, xWeighUnit, xNetWeight);

    gLogManager.LogF('Weighing Wizard finished: Total Steps: %d, Net Weight: %8g mg, Volume: %8g µL',
        [xWeighLoopCnt - 1, xNetWeight, xSumVol], true);
end;

procedure gmDoWeighWizardWithoutWeighing(aMassPA: TWeighWizardProcess; aWashManager: TWashHandling;
    const aAspRunRec: TAspirateEvRunRec; const aDispRunRec: TDispenseEvRunRec);
var
    x: integer;
    xArmTipIndex: integer;
    xUsedArm: IArmDevice;
    xLiqH: TLiqHandlingRec;
    xWashJob: TWashJob;
    xSumVol: double;
    xVarixMotorSteps: integer;
begin
    xSumVol := 0;

    // nur verteilen - nicht wiegen
    for x := 0 to aMassPA.NextDispCount - 1 do
    begin

        // UsedTips definieren, Waschen, Tool aufnehmen // Kunstgriff: Toolname muß einem Liquid Handling Parameter-Namen entsprechen
        gmChangeWeighWizardTips(aMassPA.ParamName, aWashManager, aMassPA.fSource, aMassPA.NextVolume[x],
            xUsedArm, xArmTipIndex, xLiqH, xWashJob, aMassPA.fDllAdaptor.SubstanceID);

        // ein Pulververteilschritt
        xVarixMotorSteps := aMassPA.NextVarixSteps[x];
        gmDoWeighWizardPowder(xUsedArm, xWashJob, aMassPA.fSource, aMassPA.fDest, aMassPA.NextVolume[x],
            xVarixMotorSteps, xLiqH, aAspRunRec, aDispRunRec);

        xSumVol := xSumVol + aMassPA.NextVolume[x];
    end;

    gLogManager.LogF('Weighing Wizard finished: Total Steps: %d, Volume: %8g µL',
        [aMassPA.NextDispCount, xSumVol], true);
end;

procedure gmDoWeighWizard(const aRunName: string; const aSRack, aDRack: string; aSRackPos, aDRackPos: integer;
    aDestMass, aTeachVol: double; aWashManager: TWashHandling; const aAspRunRec: TAspirateEvRunRec;
    const aDispRunRec: TDispenseEvRunRec; const aTeachParam: string; aUseLastWeighAsTare: boolean);
var
    xMassPA: TWeighWizardProcess;
begin
    // Anbindung zu Datenbank muss vorhanden sein
    if (gWeighLogicDbName = '') then
    begin
        gErrorManager.SetGlobalErr(ERR_USER, 'No WeighLogicDbName');
        EXIT;
    end;

    // MASSP Action starten
    xMassPA := TWeighWizardProcess.Create(aRunName, aSRack, aDRack, aSRackPos, aDRackPos, aDestMass,
        aTeachVol, aTeachParam);
    try
        // Tool-Result von IBTools ermitteln
        xMassPA.GetPipTool();

        // CalliW-Schritte ausführen
        if (xMassPA.DoWeigh) then
            gmDoWeighWizardWithWeighing(xMassPA, aWashManager, aAspRunRec, aDispRunRec, aUseLastWeighAsTare)
        else
            gmDoWeighWizardWithoutWeighing(xMassPA, aWashManager, aAspRunRec, aDispRunRec);

    finally
        xMassPA.Free;
    end;
end;


end.
