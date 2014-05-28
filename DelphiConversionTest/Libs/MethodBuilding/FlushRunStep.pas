{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Types and structures used by Run step FLUSH
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                    track-no  improvement/change
  -------- --  ---------------------------------------  --------  ----------------------------------------------
  07.01.08 wl                                            TN3972    initial version
  20.05.08 wl  TMethodStepSetting_..GetOptionSummary     TN4113    new
  03.07.08 wl                                            TN4157
  16.07.08 pk  GetValue, SetValue                        TN4157    PipDeviceName Property was missing
  08.09.08 pk                                            TN4215
  23.09.08 wl  DilIndex                                  TN4238    Diluent in DilIndex umbenannt, da Index immer 0-basiert
  06.10.08 pk                                            TN4258    Various changes
  06.10.08 pk                                            TN4258    LinkAndViewDataRemoved
  10.11.08 pk DoCreateRunSteps                           TN4279    new aRunSteps param
  20.02.09 wl  ..RunStepInfo.Create                TN4438   bestimmt fCaption und fDescription (GetResNo entfernt)
  20.02.09 wl  ..RunStep.DoReadData,DoWriteData    TN4438   entfernt
  20.02.09 wl  TMethodStepSetting_...              TN4438   aDescription direkt angegeben statt mit aResNo
  12.05.10 wl  GetBitOptionsKeyValueText           TN5064   neu: BitOptions werden hübsch angezeigt
  06.10.10 pk                                      TN5290   New DoOnGetEditFunctionParams event to get params for EditFunction
  08.10.10 pk                                      TN5295   New DoGetCategoryName(s)
  07.02.11 wl                                      TN5460   uses geändert
  30.06.11 wl                                      TN5620   jetzt mit Icon
  14.12.11 wl                                      TN5765   uses geändert
  08.08.12 wl  TRunStep.Diluent                    TN5946   DiluentName statt Index
  10.08.12 wl                                      TN5947   BitOptions durch 3 neue Parameter ersetzt
  13.08.12 wl  WriteFlushOptions                   TN5947   Schreibt Optionen für Standard-Flush
  03.09.13 wl  DryAfterWash                        TN6238   neuer Parameter
  18.09.13 wl  WriteFlushOptions                   TN6252.3 kann jetzt auch Kanal 1 und 2 gleichzeitig
  30.09.13 wl                                      TN6260   Beschreibung für Volume geändert
  -------------------------------------------------------------------------------------------------- }

unit FlushRunStep;


interface


uses
    Classes,
    RunStepInfo,
    RunStepInfoTypeInfo,
    MethodStep,
    RunStepBuilder,
    MethodTypes,
    MethodStepDataFields,
    CustomSetting,
    RunStepBuilderHelper,
    RunStep,
    RunStepTypeInfo;

const
    cActionNameFlush = 'FLUSH';

type
    TFlushRunStepInfo = class(TRunStepInfo)
    protected
        function GetIconIndex: integer; override;
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
    public
        constructor Create(); override;
    end;

    TFlushRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo(): TRunStepInfo; override;
    end;

    TFlushRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        procedure DoCreateRunStepInfoCreator(); override;
        function DoGetSingleCategoryName: string; override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TMethodStepSetting_Flush = class(TMethodStepCompositeSetting)
    private const
        cMethOptionKeyPrefix = 'FLUSH';
        cMethOptionKeyPipDevice = cMethOptionKeyPrefix + 'PIPDEV';
        cMethOptionKeyTipMap = cMethOptionKeyPrefix + 'TIPMAP';
        cMethOptionKeyDiluent = 'DILUENT';
        cMethOptionKeyCycles = cMethOptionKeyPrefix + 'CYCLES';
        cMethOptionKeyVolume = cMethOptionKeyPrefix + 'VOLUME';
        cMethOptionKeyPumpNumber = cMethOptionKeyPrefix + 'CHAN';
        cMethOptionKeyUseAllPumps = cMethOptionKeyPrefix + 'USEALL';
        cMethOptionKeyUsePeripump = cMethOptionKeyPrefix + 'PERI';
        cMethOptionKeyDryAfterWash = cMethOptionKeyPrefix + 'DRY';
    private
        function GetPipDeviceName: TCustomSetting;
        function GetTipMap: TCustomSetting;
        function GetDiluent: TCustomSetting;
        function GetCycles: TCustomSetting;
        function GetVolume: TCustomSetting;
        function GetPumpNumber: TCustomSetting;
        function GetUseAllPumps: TCustomSetting;
        function GetUsePeripump: TCustomSetting;
        function GetDryAfterWash: TCustomSetting;
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    protected
        function GetOptionSummary: string; override;
    public
        constructor Create(aOnAddEditFunctions: TNotifyEvent);

        class function WriteFlushOptions(const aPipDeviceName: string; aTipMap: integer;
            aAddDilutorMap: integer; const aDiluentName: string; aVolume, aCycles: integer;
            aUseCh1, aUseCh2: boolean; aUsePeriPump, aUseAddDilutors: boolean;
            aDryAfterWash: boolean): string;

        property PipDeviceName: TCustomSetting read GetPipDeviceName;
        property TipMap: TCustomSetting read GetTipMap;
        property Diluent: TCustomSetting read GetDiluent;
        property Cycles: TCustomSetting read GetCycles;
        property Volume: TCustomSetting read GetVolume;
        property PumpNumber: TCustomSetting read GetPumpNumber;
        property UseAllPumps: TCustomSetting read GetUseAllPumps;
        property UsePeripump: TCustomSetting read GetUsePeripump;
        property DryAfterWash: TCustomSetting read GetDryAfterWash;
    end;

    TFlushMethodStep = class(TMethodStep)
    private
        function GetMainSubOptionSetting: TMethodStepSetting_Flush;
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
    public
        property MainSubOptionSetting: TMethodStepSetting_Flush read GetMainSubOptionSetting;
    end;

    TFlushRunStepBuilder = class(TRunStepByMethodStepBuilder)
    private
        function GetMStep: TFlushMethodStep;
        function GetRunOptions(): TMethodStepSetting_Flush;
    protected
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
    public
        function GetMethodStepClass(): TMethodStepClass; override;
        property MStep: TFlushMethodStep read GetMStep;
        property RunOptions: TMethodStepSetting_Flush read GetRunOptions;
    end;

    TFlushRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TFlushRunStep = class(TRunStep)
    strict private
        fPipDeviceName: string;
        fTipMap: integer;
        fDiluent: string;
        fCycles: integer;
        fVolume: double;
        fPumpNumber: integer;
        fUseAllPumps: boolean;
        fUsePeriPump: boolean;
        fDryAfterWash: TTernary;
    protected
        function DoGetDescription(): string; override;
    published
        property PipDeviceName: string read fPipDeviceName write fPipDeviceName;
        property TipMap: integer read fTipMap write fTipMap;
        property Diluent: string read fDiluent write fDiluent;
        property Cycles: integer read fCycles write fCycles;
        property Volume: double read fVolume write fVolume;
        property PumpNumber: integer read fPumpNumber write fPumpNumber;
        property UseAllPumps: boolean read fUseAllPumps write fUseAllPumps;
        property UsePeriPump: boolean read fUsePeriPump write fUsePeriPump;
        property DryAfterWash: TTernary read fDryAfterWash write fDryAfterWash;
    end;

    TFlushRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TFlushRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    SysUtils,
    CustomLeafSettings,
    MethodGUIParsing,
    GeneralTypes,
    CustomEditFunctionParams,
    MethodStepSettings;

{ TFlushRunStepInfo }

constructor TFlushRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('Flush', 'Spülen');
    fDescription := TLanguageString.Read('This action moves the tips to the wash station and flushes.',
        'Diese Aktion bewegt die Nadeln zur Waschstation und spült.');
end;

function TFlushRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoSchedulingAndIterate;
end;

function TFlushRunStepInfo.GetDefaultName: string;
begin
    result := cActionNameFlush;
end;

function TFlushRunStepInfo.GetIconIndex: integer;
begin
    EXIT(cImageIndexActionFlush);
end;

{ TFlushRunStepInfoCreator }

function TFlushRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TFlushRunStepInfo.Create();
end;

{ TFlushRunStepInfoTypeInfo }

constructor TFlushRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionFlush = '1.0.0';
    cStepTypeNameFlush = cActionNameFlush;
begin
    inherited Create(cStepTypeNameFlush, cStepTypeVersionFlush, aLibName, aLibVersion);
end;

procedure TFlushRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TFlushRunStepInfoCreator.Create();
end;

function TFlushRunStepInfoTypeInfo.DoGetSingleCategoryName: string;
begin
    result := self.CategoryNamePipetting;
end;

{ TMethodStepSetting_Flush }

constructor TMethodStepSetting_Flush.Create(aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create('', '', false);

    AddParam(TCustomSetting_PipDeviceName.Create(cMethOptionKeyPipDevice,
        TLanguageString.Read('Pipetting device name', 'Pipettierdevice (Name)'), aOnAddEditFunctions));
    AddParam(TCustomSetting_TipMap.Create(cMethOptionKeyTipMap, TLanguageString.
        Read('Used Tips (Bitmap), empty = all tips will be flushed',
        'Benutzte Nadeln (Bitmap), leer = alle Nadeln werden gewaschen'), aOnAddEditFunctions,
        DoOnGetEditFunctionParams));
    AddParam(TCustomSetting_DiluentName.Create(cMethOptionKeyDiluent,
        TLanguageString.Read('System liquid name', 'Name der Systemflüssigkeit'), aOnAddEditFunctions));
    AddParam(TCustomLeafSetting.Create(cMethOptionKeyCycles, TLanguageString.Read('Number of flush cycles',
        'Anzahl Wash-Zyklen'), true));
    AddParam(TCustomLeafSetting.Create(cMethOptionKeyVolume, TLanguageString.
        Read('Volume [µL], empty = take syringe volume',
        'Volumen [µL], leer = nimm das Spritzenvolumen'), true));
    AddParam(cMethOptionKeyPumpNumber, TLanguageString.Read('Channel (Pump number)',
        'Kanal (Pumpennummer)'), true);
    AddParam(TCustomSetting_Ternary.Create(cMethOptionKeyUseAllPumps,
        TLanguageString.Read('Wash 2nd channel/pump, too (if possible)',
        '2. Kanal/Pumpe zusätzlich spülen (wenn möglich)'), aOnAddEditFunctions));
    AddParam(TCustomSetting_Ternary.Create(cMethOptionKeyUsePeriPump,
        TLanguageString.Read('Use Peripump (if possible)', 'Peripumpe verwenden (wenn möglich)'),
        aOnAddEditFunctions));
    AddParam(TCustomSetting_Ternary.Create(cMethOptionKeyDryAfterWash,
        TLanguageString.Read('Dry tips after wash (if possible)',
        'Spitzen trocknen nach dem Waschen (wenn möglich)'), aOnAddEditFunctions));

    SetValue('');
end;

procedure TMethodStepSetting_Flush.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TPipDeviceAndUsedTipsEditFunctionParams.Create(self.PipDeviceName, self.TipMap);
end;

function TMethodStepSetting_Flush.GetCycles: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyCycles);
end;

function TMethodStepSetting_Flush.GetDiluent: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyDiluent);
end;

function TMethodStepSetting_Flush.GetPipDeviceName: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyPipDevice);
end;

function TMethodStepSetting_Flush.GetPumpNumber: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyPumpNumber);
end;

function TMethodStepSetting_Flush.GetTipMap: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyTipMap);
end;

function TMethodStepSetting_Flush.GetUseAllPumps: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyUseAllPumps);
end;

function TMethodStepSetting_Flush.GetUsePeripump: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyUsePeripump);
end;

function TMethodStepSetting_Flush.GetDryAfterWash: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyDryAfterWash);
end;

function TMethodStepSetting_Flush.GetVolume: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyVolume);
end;

class function TMethodStepSetting_Flush.WriteFlushOptions(const aPipDeviceName: string;
    aTipMap, aAddDilutorMap: integer; const aDiluentName: string; aVolume, aCycles: integer;
    aUseCh1, aUseCh2, aUsePeriPump, aUseAddDilutors: boolean; aDryAfterWash: boolean): string;
var
    xPumpNumber: integer;
    xUse2Channels: boolean;
begin
    if aUseCh1 and aUseCh2 then
    begin
        xUse2Channels := true;
        xPumpNumber := 1;
    end
    else
    begin
        xUse2Channels := false;
        if aUseCh2 then
            xPumpNumber := 2
        else
            xPumpNumber := 1;
    end;

    EXIT(cMethOptionKeyVolume + '=' + IntToStr(aVolume) + ',' + cMethOptionKeyCycles + '=' + IntToStr(aCycles)
        + ',' + cMethOptionKeyUsePeripump + '=' + TMethodGUIParser.BoolToYesNoStr(aUsePeriPump) + ',' +
        cMethOptionKeyPumpNumber + '=' + IntToStr(xPumpNumber) + ',' + cMethOptionKeyUseAllPumps + '=' +
        TMethodGUIParser.BoolToYesNoStr(xUse2Channels) + ',' + cMethOptionKeyPipDevice + '=' + aPipDeviceName
        + ',' + cMethOptionKeyTipMap + '=' + IntToStr(aTipMap) + ',' + cMethOptionKeyDiluent + '=' +
        aDiluentName + ',' + cMethOptionKeyDryAfterWash + '=' + TMethodGUIParser.BoolToYesNoStr
        (aDryAfterWash));
end;

function TMethodStepSetting_Flush.GetOptionSummary: string;
begin
    if (StrToIntDef(self.Cycles.Value, 0) > 0) then
        result := TTypeSafeFormat.Format('Diluent: {0}, {1} flush cycles',
            [self.Diluent.Value, self.Cycles.Value])
    else
        result := TTypeSafeFormat.Format('Diluent: {0}, {1} uL', [self.Diluent.Value, self.Volume.Value]);
end;

{ TFlushMethodStep }

procedure TFlushMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    AddOptionViewSubParam(TMethodStepSetting_Flush.Create(aOnAddEditFunctions));
end;

function TFlushMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TFlushRunStepInfo.Create();
end;

function TFlushMethodStep.GetMainSubOptionSetting: TMethodStepSetting_Flush;
begin
    result := inherited MainSubOptionSetting as TMethodStepSetting_Flush;
end;

{ TFlushRunStepBuilder }

function TFlushRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TFlushMethodStep;
end;

function TFlushRunStepBuilder.GetMStep: TFlushMethodStep;
begin
    result := inherited MStep as TFlushMethodStep;
end;

function TFlushRunStepBuilder.GetRunOptions(): TMethodStepSetting_Flush;
begin
    result := self.MStep.MainSubOptionSetting;
end;

procedure TFlushRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper);
const
    INT_OPTION_KEY_FLUSH_TIPMAP_DEFAULT = 255;
var
    xGeneralRunStep: TRunStep;
    xRunStep: TFlushRunStep;
begin
    xGeneralRunStep := CreateRunStepByNameAndAdd(aRunSteps);
    xRunStep := xGeneralRunStep as TFlushRunStep;

    xRunStep.PipDeviceName := self.RunOptions.PipDeviceName.ParseValue;
    xRunStep.TipMap := StrToIntDef(self.RunOptions.TipMap.ParseValue, INT_OPTION_KEY_FLUSH_TIPMAP_DEFAULT);
    xRunStep.Diluent := self.RunOptions.Diluent.ParseValue;
    xRunStep.Cycles := StrToIntDef(self.RunOptions.Cycles.ParseValue, 0);
    xRunStep.Volume := TMethodGUIParser.ParseStrToFloatDef(self.RunOptions.Volume.ParseValue, 0);
    xRunStep.PumpNumber := TMethodGUIParser.StrToPumpNumber(self.RunOptions.PumpNumber.ParseValue);
    xRunStep.UseAllPumps := TMethodGUIParser.YesNoStrToBool(self.RunOptions.UseAllPumps.ParseValue, false);
    xRunStep.UsePeriPump := TMethodGUIParser.YesNoStrToBool(self.RunOptions.UsePeriPump.ParseValue, true);
    xRunStep.DryAfterWash := TMethodGUIParser.YesNoStrToTernary(self.RunOptions.DryAfterWash.ParseValue);
end;

{ TFlushRunStepBuilderTypeInfo }

constructor TFlushRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const // 1 - 5
    cStepTypeVersionFlush = '1.0.0';
    cStepTypeNameFlush = cActionNameFlush;
begin
    inherited Create(cStepTypeNameFlush, cStepTypeVersionFlush, aLibName, aLibVersion,
        TRunStepBuilderMetaClassCreator.Create(TFlushRunStepBuilder, TFlushMethodStep));
end;

{ TFlushRunStep }

function TFlushRunStep.DoGetDescription: string;
begin
    if fCycles > 0 then
        result := TTypeSafeFormat.Format('Diluent: {0}, {1} flush cycles', [fDiluent, IntToStr(fCycles)])
    else
        result := TTypeSafeFormat.Format('Diluent: {0}, {1} uL', [fDiluent, FloatToStr(fVolume)]);
end;

{ TFlushRunStepCreator }

function TFlushRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TFlushRunStep.Create();
end;

{ TFlushRunStepTypeInfo }

constructor TFlushRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionFlush = '1.0.0';
    cStepTypeNameFlush = cActionNameFlush;
begin
    inherited Create(cStepTypeNameFlush, cStepTypeVersionFlush, aLibName, aLibVersion);
end;

procedure TFlushRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TFlushRunStepCreator.Create();
end;


end.
