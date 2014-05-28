{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no  improvement/change
  -------- --  ----------------------------------  -------- ----------------------------------------------------------
  15.08.12 wl  TMethodStepSetting_MultiPipParam    TN5956   Default-Werte für Source und Dest Criteria wieder wie in 8.0.3
  06.03.13 wl  OnGetPickList                       TN6103   Parameter geändert
  21.10.13 wl  TMethodStepSetting_MultiPipParam    TN6276   Parameter überarbeitet, neue hinzugefügt
  27.11.13 wl                                      TN6277   neu: CheckSource & CheckDest
  11.12.13 wl                                      TN6277   Default=YES für CheckSource & CheckDest
  ----------------------------------------------------------------------------------------------------------------------- }

unit BasicPipetteMethodStepSettings;


interface


uses
    Classes,
    MethodStep,
    CustomSetting,
    RunStep,
    BasicPipetteRunStep,
    BasicPipetteTypes;

type
    TCustomSetting_MultiPipTipScattering = class(TCustomLeafSetting)
    private const
        cTipScatteringNone = 'None';
        cTipScatteringStandard = 'Standard';
    private
        function DoOnGetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
    public
        constructor Create(const aKey, aDescription: string; aOnAddEditFunctions: TNotifyEvent);
        class function FromStr(const aValue: string): TMultiPipTipScatteringType;
        class function ToStr(const aValue: TMultiPipTipScatteringType): string;
    end;

    TCustomSetting_MultiPipStepShift = class(TCustomLeafSetting)
    private const
        cStepShiftNoShift = 'NoShift';
        cStepShiftReduceSourceSteps = 'ReduceSourceSteps';
    private
        function DoOnGetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
    public
        constructor Create(const aKey, aDescription: string; aOnAddEditFunctions: TNotifyEvent);
        class function FromStr(const aValue: string): TMultiPipStepShiftType;
        class function ToStr(const aValue: TMultiPipStepShiftType): string;
    end;

    TMethodStepSetting_MultiPipParam = class(TMethodStepCompositeSetting)
    private const
        cMethOptionKeyMultiPipEnabled = 'Enabled';
        cMethOptionKeyMultiPipTipScattering = 'TipScattering';
        cMethOptionKeyMultiPipStepShift = 'StepShift';
        cMethOptionKeyMultiPipSourceCriteria = 'SourceCriteria';
        cMethOptionKeyMultiPipDestCriteria = 'DestCriteria';
        cMethOptionKeyMultiPipCheckSource = 'CheckSource';
        cMethOptionKeyMultiPipCheckDest = 'CheckDest';
    private
        function GetMultiPipEnabled: TCustomSetting;
        function GetMultiPipTipScattering: TCustomSetting;
        function GetMultiPipStepShift: TCustomSetting;
        function GetMultiPipSourceCriteria: TCustomSetting;
        function GetMultiPipDestCriteria: TCustomSetting;
        function GetMultiPipCheckSource: TCustomSetting;
        function GetMultiPipCheckDest: TCustomSetting;
        function GetMultiPipSourceCriteriaPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        function GetMultiPipDestCriteriaPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
    protected
        function GetOptionSummary: string; override;
    public
        constructor Create(const aKey, aDescription: string; aOnAddEditFunctions: TNotifyEvent;
            const aVisible: boolean);
        property MultiPipEnabled: TCustomSetting read GetMultiPipEnabled;
        property MultiPipTipScattering: TCustomSetting read GetMultiPipTipScattering;
        property MultiPipStepShift: TCustomSetting read GetMultiPipStepShift;
        property MultiPipSourceCriteria: TCustomSetting read GetMultiPipSourceCriteria;
        property MultiPipDestCriteria: TCustomSetting read GetMultiPipDestCriteria;
        property MultiPipCheckSource: TCustomSetting read GetMultiPipCheckSource;
        property MultiPipCheckDest: TCustomSetting read GetMultiPipCheckDest;
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    MethodGUIParsing,
    CustomLeafSettings;

{ TCustomSetting_MultiPipTipScattering }

constructor TCustomSetting_MultiPipTipScattering.Create(const aKey, aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true);
    self.OnGetPickList := DoOnGetPickList;
end;

function TCustomSetting_MultiPipTipScattering.DoOnGetPickList(aOnCreateParams
    : TCustomSettingCreateEditParamsEvent): TArray<string>;
begin
    SetLength(result, 2);
    result[0] := cTipScatteringNone;
    result[1] := cTipScatteringStandard;
end;

class function TCustomSetting_MultiPipTipScattering.FromStr(const aValue: string): TMultiPipTipScatteringType;
begin
    if SameText(aValue, cTipScatteringStandard) then
        EXIT(stoStandard);

    EXIT(stoNone);
end;

class function TCustomSetting_MultiPipTipScattering.ToStr(const aValue: TMultiPipTipScatteringType): string;
begin
    case aValue of
        stoStandard:
            EXIT(cTipScatteringStandard);
        else
            EXIT(cTipScatteringNone);
    end;
end;

{ TCustomSetting_MultiPipStepShift }

constructor TCustomSetting_MultiPipStepShift.Create(const aKey, aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true);
    self.OnGetPickList := DoOnGetPickList;
end;

function TCustomSetting_MultiPipStepShift.DoOnGetPickList(aOnCreateParams
    : TCustomSettingCreateEditParamsEvent): TArray<string>;
begin
    SetLength(result, 2);
    result[0] := cStepShiftNoShift;
    result[1] := cStepShiftReduceSourceSteps;
end;

class function TCustomSetting_MultiPipStepShift.FromStr(const aValue: string): TMultiPipStepShiftType;
begin
    if SameText(aValue, cStepShiftReduceSourceSteps) then
        EXIT(ssoReduceSourceSteps);

    EXIT(ssoNoShift);
end;

class function TCustomSetting_MultiPipStepShift.ToStr(const aValue: TMultiPipStepShiftType): string;
begin
    case aValue of
        ssoReduceSourceSteps:
            EXIT(cStepShiftReduceSourceSteps);
        else
            EXIT(cStepShiftNoShift);
    end;
end;

{ TMethodStepSetting_MultiPipParam }

constructor TMethodStepSetting_MultiPipParam.Create(const aKey, aDescription: string;
    aOnAddEditFunctions: TNotifyEvent; const aVisible: boolean);
var
    xSetting: TCustomSetting;
begin
    inherited Create(aKey, aDescription, aVisible);
    AddParam(TCustomSetting_Ternary.Create(cMethOptionKeyMultiPipEnabled,
        TLanguageString.Read('Use Multipipetting', 'Multipipetting verwenden'), aOnAddEditFunctions, true));
    AddParam(TCustomSetting_MultiPipTipScattering.Create(cMethOptionKeyMultiPipTipScattering,
        TLanguageString.Read('Few Sources: Scatter steps to several tips',
        'Wenige Quellen: Auf mehrere Nadeln verteilen'), aOnAddEditFunctions));
    AddParam(TCustomSetting_MultiPipStepShift.Create(cMethOptionKeyMultiPipStepShift,
        TLanguageString.Read('Shift steps to have less steps',
        'Schritte verschieben, um weninger Schritte zu haben'), aOnAddEditFunctions));
    xSetting := AddParam(TCustomLeafSetting.Create(cMethOptionKeyMultiPipSourceCriteria,
        TLanguageString.Read('Grouping Criteria (Default: {0})', 'Gruppierungskriterien (Default: {0])',
        [cSourceSortCriteriaDefault]), true));
    xSetting.OnGetPickList := self.GetMultiPipSourceCriteriaPickList;
    xSetting := AddParam(TCustomLeafSetting.Create(cMethOptionKeyMultiPipDestCriteria,
        TLanguageString.Read('Sorting Criteria (Default: {0})', 'Sortierungskriterien (Default: {0})',
        [cDestSortCriteriaDefault]), true));
    xSetting.OnGetPickList := self.GetMultiPipDestCriteriaPickList;
    AddParam(TCustomSetting_Ternary.Create(cMethOptionKeyMultiPipCheckSource,
        TLanguageString.Read('Check if source positions can be reached (Default=YES)',
        'Prüfen, ob Quellpositionen erreicht werden (Default=YES)'), aOnAddEditFunctions, true));
    AddParam(TCustomSetting_Ternary.Create(cMethOptionKeyMultiPipCheckDest,
        TLanguageString.Read('Check if destination positions can be reached (Default=YES)',
        'Prüfen, ob Zielpositionen erreicht werden (Default=YES)'), aOnAddEditFunctions, true));
end;

function TMethodStepSetting_MultiPipParam.GetOptionSummary: string;
begin
    result := '';

    if (self.MultiPipEnabled.Value = 'YES') then
    begin
        result := result + ', Tip Scattering: ' + self.MultiPipTipScattering.Value;
        result := result + ', Step Shift: ' + self.MultiPipStepShift.Value;

        if self.MultiPipSourceCriteria.Value <> '' then
            result := result + ', Group by ' + self.MultiPipSourceCriteria.Value;
        if self.MultiPipDestCriteria.Value <> '' then
            result := result + ', Sort by ' + self.MultiPipDestCriteria.Value;
    end;
end;

function TMethodStepSetting_MultiPipParam.GetMultiPipSourceCriteriaPickList
    (aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
begin
    SetLength(result, 3);
    result[0] := cSourceSortCriteriaDefault;
    result[1] := cSourceSortCriteriaDefault2;
    result[2] := cSourceSortCriteriaNone;
end;

function TMethodStepSetting_MultiPipParam.GetMultiPipDestCriteriaPickList
    (aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
begin
    SetLength(result, 3);
    result[0] := cDestSortCriteriaDefault;
    result[1] := cDestSortCriteriaDefault2;
    result[2] := cDestSortCriteriaNone;
end;

function TMethodStepSetting_MultiPipParam.GetMultiPipEnabled: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyMultiPipEnabled);
end;

function TMethodStepSetting_MultiPipParam.GetMultiPipTipScattering: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyMultiPipTipScattering);
end;

function TMethodStepSetting_MultiPipParam.GetMultiPipStepShift: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyMultiPipStepShift);
end;

function TMethodStepSetting_MultiPipParam.GetMultiPipSourceCriteria: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyMultiPipSourceCriteria);
end;

function TMethodStepSetting_MultiPipParam.GetMultiPipDestCriteria: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyMultiPipDestCriteria);
end;

function TMethodStepSetting_MultiPipParam.GetMultiPipCheckSource: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyMultiPipCheckSource);
end;

function TMethodStepSetting_MultiPipParam.GetMultiPipCheckDest: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyMultiPipCheckDest);
end;


end.
