{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------- --------  ----------------------------------------------------------
  19.06.09 pk   fRackPositions                       TN4538     New
  28.07.09 pk   DoBatchEnded                         TN4683     fRackPositions is stepnumber-based not tipnumber
  28.08.09 pk                                        TN4753     uses Liquids
  08.09.09 pk                                        TN4753     uses ErrorMessage replaced by ErrorInfo
  12.09.09 wl                                        TN4740     TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  22.09.09 pk   IsStepCompatible                     TN4759     UsedTips can be different for each pipstep
  16.12.09 pk   DoBeforeAddStepToBatch               TN4950     Set UsedTips and UsedTipType of CombinedRunStep
  04.02.10 pk                                        TN4972     Changes for Restart
  08.04.10 pk   CheckSkipTips                        TN4996     now does check if tip < 0 instead of = 0
  29.06.10 pk                                        TN5143     changes for using different LiqParams together
  31.08.10 wl  IsLiqParamOK                          TN5251     keine Prüfung, wenn der aktuelle Schritt nichts transferiert
  31.08.10 wl  IsLiqParamOK                          TN5251     Vergleich erfolgt mit dem letzten Schritt, der ein Volumen hat
  19.10.10 wl  IsLiqParamOK                          TN5251.1   Compiler-Fehler beseitigt
  06.04.11 wl                                        TN5501    Liquid-Handling-Daten brauchen nicht mehr neu geladen werden
  19.07.11 wl                                        TN5630   DTransAirRetake boolean statt double
  14.12.11 wl                                        TN5765   uses geändert, bentzt nur noch TMethodGUIParser
  02.02.11 wl  IsStepCompatible                      TN5791   Prüfung auf gleichen Racknamen entfernt
  03.02.11 wl  ChangeWashOrWasteDestRackPos          TN5791   entfernt
  24.04.13 wl                                        TN6137   verwendet TLiquids.FindDiluentIndex
  06.08.13 wl  IsStepCompatible                      TN6210   Abbruchkriterium für Verdünnungsreihen von TBasicPipetteRunAction.Dilution hierher
  15.08.13 wl  IsStepCompatible                      TN6217   Inhalt von FindArmToUse von SubstanceHandling hierher
  15.08.13 wl  IsStepCompatible                      TN6217   Klare Fehlermeldung, wenn PipDevice nicht existiert
  21.08.13 wl  IsPosReachable                        TN6231   von SamHigh hierher
  27.11.13 wl  IsPosReachable                        TN6313   ChangePosForMultiTipRack wird jetzt vor der Positions-Prüfung ausgeführt
  27.11.13 wl  IsStepCompatible                      TN6313   verschiedene MultiTip Racks dürfen nicht kombiniert werden
  28.11.13 wl  IsStepCompatible                      TN6277   MultiTipRack-Prüfung nicht für Multi-Pipetting
  ----------------------------------------------------------------------------------------------------------------------- }

unit BasicPipetteRunStepBatcher;


interface


uses
    SysUtils,
    IntfArmDevice,
    RunStep,
    LiqHTypes,
    BasicPipetteRunStep,
    RunStepBatcher,
    Rack,
    BasicPipetteTypes;

type
    EGenPipSeqBreakLoop = class(Exception);

    TBasicPipetteRunStepBatcher = class(TRunStepBatcher)
    private
        fSkipTips: integer;
        fTipsArePredefined: boolean;
        fUsedArm: IArmDevice;
        fRackPositions: TPipetteRackPositions;
        procedure ErrBoxPip(const aMessage: string; const aCurrentStep: TBasicPipetteRunStep);
        function CheckDefinedTip(const aCurrentStep, aPrevStep: TBasicPipetteRunStep;
            aTipsArePredefined: boolean): Boolean;
        function MultiTipOK(aCurrentStep, aPrevStep: TBasicPipetteRunStep; aIndex: integer): Boolean;

        function CheckSkipTips(const aUsedArm: IArmDevice; const aIndex, aTipIndex: integer;
            var vSkipTips: integer): Boolean;
        procedure ObtainDilRackPositions(aIndex: integer; aDilRack: string; aDilPos, aDiluent: integer);
        function GetCurrentIndex: integer;
        function IsLiqParamOK(const aCurrentStep: TBasicPipetteRunStep): boolean;
        class function IsPosReachable(aUsedArm: IArmDevice; aTipIndex: integer; aRack: TRack;
            var vPos: integer): boolean;
    protected
        function GetCompositeRunStep: TCombinedBasicPipetteRunStep;
        procedure DoBeforeAddStepToBatch(const aStep: TRunStep); override;
        procedure IsStepCompatible(const aStep: TRunStep); override;
        procedure DoBatchEnded(const aCurrentBatchIndex: integer); override;
        procedure DoErrorOccured(const aMessage: string; const aStep: TRunStep); override;
    public
        constructor Create(const aCombinedRunStep: TCombinedBasicPipetteRunStep);
        property CompositeRunStep: TCombinedBasicPipetteRunStep read GetCompositeRunStep;
    end;


implementation


uses
    RackTypes,
    ErrorManager,
    AppTypes,
    LayoutManager,
    MethodGUIParsing,
    Liquids,
    TipMapUtils,
    MotorStepCalculator,
    PipDeviceManager,
    ObjModul,
    SamGlobe,
    LogManager,
    ErrorInfo,
    ErrorMessageFactory,
    IntfPipDevice;

{ TBasicPipetteRunStepBatcher }

constructor TBasicPipetteRunStepBatcher.Create(const aCombinedRunStep: TCombinedBasicPipetteRunStep);
begin
    inherited Create(aCombinedRunStep);
    fTipsArePredefined := false;
end;

const
    cFirstStepIndex = 0;
    // --------------------------------------------------------------------------------------------------
    // Pipettiersequenz erstellen
    // --------------------------------------------------------------------------------------------------

function TBasicPipetteRunStepBatcher.MultiTipOK(aCurrentStep, aPrevStep: TBasicPipetteRunStep;
    aIndex: integer): Boolean;
begin
    result := true;
    if (not aPrevStep.LiqHRec.SampleAspMultiPip) or (aCurrentStep.Tip < 0) then
        Exit; // Tip muî gesetzt sein
    if (aPrevStep.Tip >= aCurrentStep.Tip) then
        result := false;
end;

class function TBasicPipetteRunStepBatcher.IsPosReachable(aUsedArm: IArmDevice; aTipIndex: integer;
    aRack: TRack; var vPos: integer): boolean;
var
    xPipPosCheck: TPipPosCheck;
begin
    xPipPosCheck := TMotorStepCalculatorFactory.CreatePipPosCheck(aUsedArm);
    try
        // Spezialfall MultiTip-Rack: Change Position for Multi-Tip-Rack
        if (aRack.IsMultiTipRack()) then
        begin
            vPos := aTipIndex + 1;
        end;

        xPipPosCheck.SetXYByRackPos(aTipIndex, aRack, vPos);
        result := xPipPosCheck.IsPosReachable(aTipIndex);
    finally
        FreeAndNil(xPipPosCheck);
    end;
end;

// --------------------------------------------------------------------------
function TBasicPipetteRunStepBatcher.CheckSkipTips(const aUsedArm: IArmDevice;
    const aIndex, aTipIndex: integer; var vSkipTips: integer): Boolean;
// --------------------------------------------------------------------------
// Increments vSkipTips if needed and returns true
// If a proper tip cannot be found then return false
// --------------------------------------------------------------------------
var
    xIsSkipTip: boolean;
    xCurrentTipIndex: integer;
begin
    result := false;
    while true do
    begin
        xIsSkipTip := false;
        xCurrentTipIndex := aIndex + vSkipTips;
        if (xCurrentTipIndex >= aUsedArm.PipDevice.TipCount) then
            EXIT;

        if (not gmTipSelected(aUsedArm.PipDevice.UseTips, xCurrentTipIndex)) then
            xIsSkipTip := true;

        if (not xIsSkipTip) and (aTipIndex < 0) then
        begin // nicht prüfen bei Multipip -
            if (not IsPosReachable(aUsedArm, xCurrentTipIndex, fRackPositions.Source[aIndex].Rack,
                fRackPositions.Source[aIndex].Pos)) then
                xIsSkipTip := true;

            if (not xIsSkipTip) then
            begin
                if (not IsPosReachable(aUsedArm, xCurrentTipIndex, fRackPositions.Dest[aIndex].Rack,
                    fRackPositions.Dest[aIndex].Pos)) then
                    xIsSkipTip := true;
            end;

            if (not xIsSkipTip) and (fRackPositions.Dil.DilRackType = drNormal) then
            begin
                if (not IsPosReachable(aUsedArm, xCurrentTipIndex, fRackPositions.Dil.Positions[aIndex].Rack,
                    fRackPositions.Dil.Positions[aIndex].Pos)) then
                    xIsSkipTip := true;
            end;
        end;

        if not xIsSkipTip then
            BREAK;

        Inc(vSkipTips);
    end;

    result := true;
end;

// --------------------------------------------------------------------------
function TBasicPipetteRunStepBatcher.CheckDefinedTip(const aCurrentStep, aPrevStep: TBasicPipetteRunStep;
    aTipsArePredefined: boolean): Boolean;
// --------------------------------------------------------------------------
// überprüft ob der nächste tip für die Pipettiersequenz benutzt werden darf
// - nur notwendig wenn die tips zuvor definiert wurden
// ---------------------------------------------------------------------------
begin
    result := true;
    // ------------------------------------------- Tip wurde nicht vordefiniert -> exit -----
    if (aCurrentStep.Tip < 0) and (not aTipsArePredefined) then
        EXIT; // 18.08.05 pk TN2566.1 an alternative to the line below. But does not prevent source and destvol from being different
    // and (aPrevStep.SourceVol = aPrevStep.DestVol) then exit;  // 18.08.05 pk I could not understand what the point of this was. I replaced it with the above line

    // ---------------- Der vorhergehende Tip ist größer oder gleich dem aktuellen Tip ------
    if (aPrevStep.Tip >= aCurrentStep.Tip) then
        result := false;

    // ---------------------------------------------------------------------------------------
    if not result then
        gLogManager.Log('CheckDefinedTip =false', true)
    else
        gLogManager.Log('CheckDefinedTip =true', false);
end;

procedure TBasicPipetteRunStepBatcher.ObtainDilRackPositions(aIndex: integer; aDilRack: string;
    aDilPos: integer; aDiluent: integer);
var
    xDilRackType: TDilRackType;
    xDiluentNum: integer;
begin
    ASSERT(aIndex >= 0);
    xDilRackType := TMethodGUIParser.DetermineDilRackType(aDilRack);

    // check whether the given name in dilrack is actually the name of a sys liquid
    if xDilRackType = drNormal then
    begin
        xDiluentNum := TLiquids.Instance.FindDiluentIndex(aDilRack, false) + 1;
        // if it is the name of sys liq, then change dilrack to SYSTEM and set diluent
        if xDiluentNum >= 0 then
        begin
            aDiluent := xDiluentNum;
            aDilPos := 0;
            xDilRackType := drSystem;
        end;
    end;

    // if current dilracktype or diluent dont match previous then Exit
    // otherwise set the dilrack TYPE and the DILUENT
    if (aIndex = cFirstStepIndex) then
    begin
        fRackPositions.Dil.DilRackType := xDilRackType;
        fRackPositions.Dil.Diluent := aDiluent
    end
    else
    begin
        if (fRackPositions.Dil.DilRackType <> xDilRackType) then
            RaiseBreak('Current Diluent Rack Type not same as Last Diluent Rack Type');
        if (fRackPositions.Dil.Diluent <> aDiluent) then
            RaiseBreak('Current Diluent not same as Last Diluent');
    end;

    // check to see if the dilrack name given can be found on the layout
    if xDilRackType = drNormal then
    begin
        // set the dil POS
        fRackPositions.Dil.Positions[aIndex].Rack := TLayoutManager.Instance.CurrentLayout.FindRackByName
            (aDilRack, true);
        fRackPositions.Dil.Positions[aIndex].Pos := aDilPos;
    end;
end;

procedure TBasicPipetteRunStepBatcher.ErrBoxPip(const aMessage: string;
    const aCurrentStep: TBasicPipetteRunStep);
var
    xErrorInfo: TErrorInfo;
begin
    xErrorInfo := TErrorInfo.Create();
    try
        xErrorInfo.Init('An error occured during the preparation of a pipette step', 'Pipette Error',
            eibAbort);
        if Assigned(aCurrentStep) then
        begin
            xErrorInfo.AddText(Format('Action: [%s]', [aCurrentStep.StepName]));
            xErrorInfo.AddText(Format('Source rack: [%s]', [aCurrentStep.SRack]));
            xErrorInfo.AddText(Format('Source pos: [%d]', [aCurrentStep.SPos]));
            xErrorInfo.AddText(Format('Dest rack: [%s]', [aCurrentStep.DRack]));
            xErrorInfo.AddText(Format('Dest pos: [%d]', [aCurrentStep.DPos]));
            xErrorInfo.AddText(Format('Volume: [%f]', [aCurrentStep.DestVol]));
        end;
        xErrorInfo.AddText('');
        xErrorInfo.AddText('Error: ' + aMessage);
        gErrorMessageFactory.ShowAnyError(xErrorInfo);
        gErrorManager.SetGlobalErr(ERR_APPLICATION);
    finally
        FreeAndNil(xErrorInfo);
    end;
end;

function TBasicPipetteRunStepBatcher.IsLiqParamOK(const aCurrentStep: TBasicPipetteRunStep): boolean;
var
    xIncompatibleFieldName: string;
    xPrevStep: TBasicPipetteRunStep;
    xPrevStepContainsVolume: boolean;
    xIndex: integer;
begin
    if not aCurrentStep.StepContainsAnyVolume() then
    begin
        EXIT(true); // Schritt hat kein Volumen -> Vergleich unnötig
    end;

    xPrevStep := nil;
    xPrevStepContainsVolume := false;
    // suche den vorhergehenden Schritt, der ein Volumen hat
    for xIndex := (self.CompositeRunStep.Count - 1) downto 0 do
    begin
        xPrevStep := self.CompositeRunStep[xIndex];
        xPrevStepContainsVolume := xPrevStep.StepContainsAnyVolume();
        if xPrevStepContainsVolume then
            BREAK;
    end;

    if (xPrevStep = nil) or (not xPrevStepContainsVolume) then
        EXIT(true); // der vorhergehende Schritt hat kein Volumen -> Vergleich unnötig

    // Vergleich der Liquid Handling Parameter
    if not gUseDifferentLiqParamsTogether then
    begin
        result := SameText(aCurrentStep.LiqHRec.PARAMNAME, xPrevStep.LiqHRec.PARAMNAME);
    end
    else
    begin
        result := TLiqHandlingParamComparer.AreCompatible(aCurrentStep.LiqHRec, xPrevStep.LiqHRec,
            xIncompatibleFieldName);
        if not result then
            TLogManager.Instance.LogFT
                ('Liquid Handling Parameters {0} and {1} could not be combined - different values in field: {2}',
                [aCurrentStep.LiqHRec.PARAMNAME, xPrevStep.LiqHRec.PARAMNAME, xIncompatibleFieldName], false);
    end;
end;

procedure TBasicPipetteRunStepBatcher.DoErrorOccured(const aMessage: string; const aStep: TRunStep);
var
    xStep: TBasicPipetteRunStep;
begin
    ASSERT(aStep is TBasicPipetteRunStep);
    xStep := aStep as TBasicPipetteRunStep;

    ErrBoxPip(aMessage, xStep);
end;

function TBasicPipetteRunStepBatcher.GetCompositeRunStep: TCombinedBasicPipetteRunStep;
begin
    result := fCompositeRunStep as TCombinedBasicPipetteRunStep;
end;

function TBasicPipetteRunStepBatcher.GetCurrentIndex(): integer;
begin
    result := (self.CompositeRunStep.Count)
end;

procedure TBasicPipetteRunStepBatcher.DoBeforeAddStepToBatch(const aStep: TRunStep);
var
    xStep: TBasicPipetteRunStep;
    xTips: TIPMAP;
begin
    inherited;

    ASSERT(aStep is TBasicPipetteRunStep);
    xStep := aStep as TBasicPipetteRunStep;

    // Set xUsedArm using first record only (Arm must always be the same)
    if (self.CompositeRunStep.IsEmpty) then
    begin
        self.CompositeRunStep.UsedArm := fUsedArm;
        self.CompositeRunStep.UsedTipType := xStep.UsedTipType;
        // WL: wie funktioniert dann gUseDifferentTipTypesTogether?

        // ----- Events zuweisen -----------------------------------------------
        self.CompositeRunStep.AspS := xStep.AspS;
        self.CompositeRunStep.AspD := xStep.AspD;
        self.CompositeRunStep.Disp := xStep.Disp;

        // Wird Multipipetting verwendet ?
        self.CompositeRunStep.IsMultiPip := false;
        if (xStep.LiqHRec.SampleAspMultipip) and not(xStep.SourceVol = xStep.DestVol) then
            self.CompositeRunStep.IsMultiPip := true;
    end;

    // ist das richtig??
    // if self.CompositeRunStep.Disp.TransAir > 0 then
    // xStep.DTransAirRetake := true; // self.CompositeRunStep.Disp.TransAir;

    fTipsArePredefined := (xStep.Tip >= 0);
    if fTipsArePredefined then
    begin
        gLogManager.Log('UseTip ' + inttostr(xStep.Tip + 1), true);
    end
    else
        xStep.Tip := self.GetCurrentIndex() + fSkipTips; // oder nächsten Tip eintragen

    // set UsedTips
    xTips := self.CompositeRunStep.UsedTips;
    TTipMapUtils.SelectTip(xTips, xStep.Tip);
    self.CompositeRunStep.UsedTips := xTips;
end;

procedure TBasicPipetteRunStepBatcher.IsStepCompatible(const aStep: TRunStep);
var
    xStep, xPrevStep: TBasicPipetteRunStep;
    xStepIndex: integer;
    x: integer;
    xPipDevice: IPipDevice;
begin
    inherited;

    ASSERT(aStep is TBasicPipetteRunStep);
    xStep := aStep as TBasicPipetteRunStep;

    xStepIndex := GetCurrentIndex();

    if xStep.DRack = '' then
    begin
        xStep.DRack := xStep.SRack; // das ist doch Mist!
        xStep.DPos := xStep.SPos;
    end
    else if xStep.SRack = '' then
    begin
        xStep.SRack := xStep.DRack;
        xStep.SPos := xStep.DPos;
    end;

    if (self.CompositeRunStep.IsEmpty) then
    begin
        // Set fUsedArm using first record only
        xPipDevice := gPipDeviceManager.FindPipDevice_ByName(xStep.PipDeviceName);
        if not Assigned(xPipDevice) then // PipDevice not is defined
            raise Exception.Create('Pip-Device ' + xStep.PipDeviceName + ' could not be found');
        fUsedArm := gModules.FindArmByPipDevice(xPipDevice);
        if not Assigned(fUsedArm) then // Arm not is defined
            raise Exception.Create('Used arm for ' + xStep.PipDeviceName + ' could not be found');
    end
    else if not SameText(xStep.PipDeviceName, fUsedArm.PipDevice.Name) then
        RaiseBreak('Different Pip Device');

    if not(self.CompositeRunStep.IsEmpty) then
        if not SameText(xStep.UsedTipType, self.CompositeRunStep.UsedTipType) then
            RaiseBreak('Different Tip Type'); // WL: wie funktioniert dann gUseDifferentTipTypesTogether?

    fUsedArm.PipDevice.SetUseTips(xStep.UsedTipType, xStep.UsedTips, true);
    xStep.UsedTips := fUsedArm.PipDevice.UseTips;

    if (self.CompositeRunStep.IsEmpty) then
    begin
        fRackPositions.Source := TXRackPositionUtils.GetEmptyXRackPositions(fUsedArm.PipDevice.TipCount);
        fRackPositions.Dest := TXRackPositionUtils.GetEmptyXRackPositions(fUsedArm.PipDevice.TipCount);
        fRackPositions.Dil := TBasicPipetteRunStep.GetEmptyDilRackPositions(fUsedArm.PipDevice.TipCount);
    end;

    // check if current Step can be done
    if (xStepIndex >= fUsedArm.PipDevice.TipCount) then
        RaiseBreak('No of samples > TipCount'); // Ensure Sample number is smaller than tip count

    // SOURCERACK
    fRackPositions.Source[xStepIndex].Rack := TLayoutManager.Instance.CurrentLayout.FindRackByName
        (xStep.SRack, true);
    fRackPositions.Source[xStepIndex].Pos := xStep.SPos;

    // DESTRACK
    fRackPositions.Dest[xStepIndex].Rack := TLayoutManager.Instance.CurrentLayout.FindRackByName
        (xStep.DRack, true);
    fRackPositions.Dest[xStepIndex].Pos := xStep.DPos;

    // IMPORTANT!!! the DilutionRackPositions record will be changed HERE!
    ObtainDilRackPositions(xStepIndex, xStep.DilRack, xStep.DilPos, xStep.Diluent);

    // Spezialfall MultiTip-Rack: Verschiedene Multi-Tip-racks lassen sich nicht kombinieren
    if (xStepIndex > 0) and (xStep.Tip < 0) then // gilt nicht für Multi-Pipetting
    begin
        if (fRackPositions.Source[xStepIndex].Rack.IsMultiTipRack()) and
            (fRackPositions.Source[xStepIndex].Rack <> fRackPositions.Source[xStepIndex - 1].Rack) then
            RaiseBreak('Current MultiTip Source Rack not same as Last Source Rack');

        if (fRackPositions.Dest[xStepIndex].Rack.IsMultiTipRack()) and
            (fRackPositions.Dest[xStepIndex].Rack <> fRackPositions.Dest[xStepIndex - 1].Rack) then
            RaiseBreak('Current MultiTip Destination Rack not same as Last Destination Rack');

        if (fRackPositions.Dil.DilRackType = drNormal) and
            (fRackPositions.Dil.Positions[xStepIndex].Rack.IsMultiTipRack()) and
            (fRackPositions.Dil.Positions[xStepIndex].Rack <> fRackPositions.Dil.Positions[xStepIndex - 1]
            .Rack) then
            RaiseBreak('Current MultiTip Diluent Rack not same as Last Diluent Rack');
    end;

    // can the rack positions be reached
    if not CheckSkipTips(fUsedArm, xStepIndex, xStep.Tip, fSkipTips) then
        RaiseBreak('No valid tip found or rack position could not be reached with tips');

    // check if current Step can be combined with the previous Steps
    if not(self.CompositeRunStep.IsEmpty) then
    begin
        xPrevStep := self.CompositeRunStep[self.CompositeRunStep.Count - 1];
        if not self.IsLiqParamOK(xStep) then
            RaiseBreak('Different Liquid Handling Param');
        if not MultiTipOK(xStep, xPrevStep, xStepIndex) then
            RaiseBreak('MultiTip failed');

        if not CheckDefinedTip(xStep, xPrevStep, fTipsArePredefined) then
            RaiseBreak('Check Defined Tip failed');

        // Z.B. Verdünnungsreihe: Ein Verduennungsschritt muss vor der nächsten Entnahme
        // ausgeführt werden:
        if (xStep.SourceVol > 0) then
        begin
            for x := 0 to self.CompositeRunStep.Count - 1 do
            begin
                // Ein vorhergehender Schritt hat ein Dest-/DilVol in die aktuelle Source-Position
                if ((self.CompositeRunStep[x].DestVol > 0) or (self.CompositeRunStep[x].DilVol > 0)) //
                    and (fRackPositions.Source[xStepIndex].Pos = fRackPositions.Dest[x].Pos) and
                    (fRackPositions.Source[xStepIndex].Rack = fRackPositions.Dest[x].Rack) then
                    RaiseBreak('Position must be filled before it can be taken as source');
            end;
        end;
    end;
end;

procedure TBasicPipetteRunStepBatcher.DoBatchEnded;
begin
    inherited;
    if (fSkipTips > 0) then
        gLogManager.Log('SkipTips =' + IntToStr(fSkipTips), true);

    // add info from rackpositions to runstep.rackpositions only if tip is selected
    self.CompositeRunStep.SetRackPositions(fRackPositions, self.CompositeRunStep.Count);
end;


end.
