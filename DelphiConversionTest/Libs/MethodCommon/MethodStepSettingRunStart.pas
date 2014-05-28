{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  25.09.08 wl                               TN4242   initial revision
  06.10.08 pk                               TN4258   Various changes
  15.10.08 pk  Params                       TN4258   type explicitly defined TCustomSetting_MethodParams
  15.10.08 pk  Create                       TN4258   MethodName setting passed as superiorsetting for MethodParams
  13.02.09 wl  TContainerActionRunStarts    TN4429   neu: InBetweeen
  20.02.09 wl  TMethodStepSetting_...       TN4438   aDescription direkt angegeben statt mit aResNo
  07.05.10 pk                               TN5092   TRunStartOptions is now a class (instead of record)
  12.05.10 wl                               TN5064   uses VariableCompositeSetting
  07.02.11 wl                               TN5460   uses geändert
  01.03.12 wl                               TN5822   uses geändert
  14.12.12 wl                               TN6054   neu: TCustomSetting_MethodName.GetMethodName
  ---------------------------------------------------------------------------------------------------------------------- }

unit MethodStepSettingRunStart;


interface


uses
    Classes,
    MethodStep,
    CustomSetting,
    MethodTypes,
    Streamable,
    VariableCompositeSetting;

type
    TContainerActionRunStarts = class(TStreamable)
    private
        fBeforeGet: TRunStartOptions;
        fAfterGet: TRunStartOptions;
        fInBetween: TRunStartOptions;
        fBeforePut: TRunStartOptions;
        fAfterPut: TRunStartOptions;
        fBCBeforeRead: TRunStartOptions;
        fBCAfterRead: TRunStartOptions;
    public
        constructor Create(); override;
        destructor Destroy(); override;
    published
        property BeforeGet: TRunStartOptions read fBeforeGet write fBeforeGet;
        property AfterGet: TRunStartOptions read fAfterGet write fAfterGet;
        property InBetween: TRunStartOptions read fInBetween write fInBetween;
        property BeforePut: TRunStartOptions read fBeforePut write fBeforePut;
        property AfterPut: TRunStartOptions read fAfterPut write fAfterPut;
        property BCBeforeRead: TRunStartOptions read fBCBeforeRead write fBCBeforeRead;
        property BCAfterRead: TRunStartOptions read fBCAfterRead write fBCAfterRead;
    end;

    // ------ Aspirate Sample Action
    TAspirateEvRunRec = class(TStreamable)
    private
        fBeforeAsp: TRunStartOptions;
        fBeforePickLq: TRunStartOptions;
        fAfterPickLq: TRunStartOptions;
        fAfterAsp: TRunStartOptions;
    public
        constructor Create(); override;
        destructor Destroy(); override;
    published
        property BeforeAsp: TRunStartOptions read fBeforeAsp write fBeforeAsp;
        property BeforePickLq: TRunStartOptions read fBeforePickLq write fBeforePickLq;
        property AfterPickLq: TRunStartOptions read fAfterPickLq write fAfterPickLq;
        property AfterAsp: TRunStartOptions read fAfterAsp write fAfterAsp;
    end;

    // ------ Dispense Action
    TDispenseEvRunRec = class(TStreamable)
    private
        fBeforeDispense: TRunStartOptions;
        fBeforeDispLq: TRunStartOptions;
        fAfterDispLq: TRunStartOptions;
        fAfterDispense: TRunStartOptions;
        fTransAir: integer;
    public
        constructor Create(); override;
        destructor Destroy(); override;
        property BeforeDispense: TRunStartOptions read fBeforeDispense write fBeforeDispense;
        property BeforeDispLq: TRunStartOptions read fBeforeDispLq write fBeforeDispLq;
        property AfterDispLq: TRunStartOptions read fAfterDispLq write fAfterDispLq;
        property AfterDispense: TRunStartOptions read fAfterDispense write fAfterDispense;
        property TransAir: integer read fTransAir write fTransAir;
        // es fehlen noch ca. 100 Paramter
    end;

    TMethodStepSetting_RunStartEvent = class(TMethodStepCompositeSetting)
    private
        function GetParams: TCustomSetting_MethodParams;
        function GetRunName: TCustomSetting;
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
        procedure EvaluateParseValue(const aResult: TRunStartOptions);
        property RunName: TCustomSetting read GetRunName;
        property Params: TCustomSetting_MethodParams read GetParams;
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    MethodGUIParsing,
    CustomLeafSettings,
    MethodStepSettings,
    CustomEditFunctionParams;

const
    cOptionKeyPrefix = 'RUNST';
    cOptionKeyName = cOptionKeyPrefix + 'NAME';
    cOptionKeyParams = cOptionKeyPrefix + 'PARAMS';

    { TMethodStepSetting_RunStartEvent }

constructor TMethodStepSetting_RunStartEvent.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
var
    xMethodNameParam: TCustomSetting_MethodName;
begin
    inherited Create(aKey, aDescription, true);
    xMethodNameParam:=TCustomSetting_MethodName.Create(cOptionKeyName, TLanguageString.Read('Method Name',
        'Methodenname'), aOnAddEditFunctions);
        AddParam(   xMethodNameParam);
    AddParam(TCustomSetting_MethodParams.Create(cOptionKeyParams, TLanguageString.Read('Method Parameters',
        'Methodenparameter'), aOnAddEditFunctions, DoOnGetEditFunctionParams,
        xMethodNameParam.GetMethodName));
    SetValue('');
end;

procedure TMethodStepSetting_RunStartEvent.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TMethodParamsEditFunctionParams.Create(self.RunName, self.Params);
end;

procedure TMethodStepSetting_RunStartEvent.EvaluateParseValue(const aResult: TRunStartOptions);
begin
    aResult.RunName := self.RunName.ParseValue;
    self.Params.EvaluateParseValue(aResult.Params);
end;

function TMethodStepSetting_RunStartEvent.GetParams: TCustomSetting_MethodParams;
begin
    result := self.Find(cOptionKeyParams) as TCustomSetting_MethodParams;
end;

function TMethodStepSetting_RunStartEvent.GetRunName: TCustomSetting;
begin
    result := self.Find(cOptionKeyName);
end;

{ TContainerActionRunStarts }

constructor TContainerActionRunStarts.Create;
begin
    inherited;
    fBeforeGet := TRunStartOptions.Create();
    fAfterGet := TRunStartOptions.Create();
    fInBetween := TRunStartOptions.Create();
    fBeforePut := TRunStartOptions.Create();
    fAfterPut := TRunStartOptions.Create();
    fBCBeforeRead := TRunStartOptions.Create();
    fBCAfterRead := TRunStartOptions.Create();
end;

destructor TContainerActionRunStarts.Destroy;
begin
    FreeAndNil(fBeforeGet);
    FreeAndNil(fAfterGet);
    FreeAndNil(fInBetween);
    FreeAndNil(fBeforePut);
    FreeAndNil(fAfterPut);
    FreeAndNil(fBCBeforeRead);
    FreeAndNil(fBCAfterRead);
    inherited;
end;

{ TAspirateEvRunRec }

constructor TAspirateEvRunRec.Create;
begin
    inherited;
    fBeforeAsp := TRunStartOptions.Create();
    fBeforePickLq := TRunStartOptions.Create();
    fAfterPickLq := TRunStartOptions.Create();
    fAfterAsp := TRunStartOptions.Create();
end;

destructor TAspirateEvRunRec.Destroy;
begin
    FreeAndNil(fBeforeAsp);
    FreeAndNil(fBeforePickLq);
    FreeAndNil(fAfterPickLq);
    FreeAndNil(fAfterAsp);
    inherited;
end;

{ TDispenseEvRunRec }

constructor TDispenseEvRunRec.Create;
begin
    inherited;
    fBeforeDispense := TRunStartOptions.Create();
    fBeforeDispLq := TRunStartOptions.Create();
    fAfterDispLq := TRunStartOptions.Create();
    fAfterDispense := TRunStartOptions.Create();
end;

destructor TDispenseEvRunRec.Destroy;
begin
    FreeAndNil(fBeforeDispense);
    FreeAndNil(fBeforeDispLq);
    FreeAndNil(fAfterDispLq);
    FreeAndNil(fAfterDispense);
    inherited;
end;


end.
