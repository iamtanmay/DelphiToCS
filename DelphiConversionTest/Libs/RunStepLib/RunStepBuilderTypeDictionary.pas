{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Project      : New Editor
  Author       : Wolfgang Lyncke (wl)
  Description  : Manager for method steps and run steps
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  17.08.04 wl                               TN2008.3  initial version
  24.09.04 wl                               TN2008.3  für MoveR, Delay und Msg alles eingerichtet
  22.11.04 wl                               TN2213    jetzt mit allen Actions
  21.12.04 wl                               TN2247.1  neu: mit WashTips
  28.04.05 wl  Create                       TN2377    neu: TCalliMethodStep,TSimpleCalliMethodStep
  04.05.05 wl  Create                       TN2410    neu: TManualFillMethodStep
  15.06.05 pk  GetRealActionName            TN2464    function name changed : call DetermineCalliType insted of isCalliAction
  24.06.05 wl  Create                       TN2459    MessageWithBeeper entfernt
  01.08.05 wl  GetRealActionName            TN2503    IsPipetteOrCalliOrManualFillAction ersetzt ungenaue Abfrage IsPipetteAction
  31.08.05 wl  GetComments                  TN2541.0  von MethodEditor hierher
  31.08.05 wl  RefreshComments              TN2541.0  neu
  14.09.05 wl  Create                       TN2598    neu: Asp und DSP
  16.09.05 wl  Create                       TN2603    neu: TBalanceWaitMethodStep
  05.10.05 wl  GetCommentsWithMethName      TN2575    bei ADDM,RUN,RUNST werden keine Standardtexte mehr als Kommentare eingetragen
  05.10.05 wl  GetMethodNameFromAction      TN2575    --> MethodGUIParsing
  11.10.05 wl  Create                       TN2658    neue Rack-Action TOOLG
  20.10.05 wl  Create                       TN2659    neue Rack-Action TIPSG
  20.10.05 wl  Create                       TN2660    neue Rack-Action TIPSR
  08.11.05 wl  Create                       TN2660    Action TIPSR und TIPSG zunächst noch inaktiv
  08.11.05 wl  Create                       TN2728    neue Action XYMovement und ZPosMovement
  20.11.05 wl  Create                       TN2784    ACTION CHKE und READE werden nicht mehr aufgeführt
  20.11.05 wl  Create                       TN2784    neue Action ChangeTips
  20.11.05 pk  CreateMethodStep             TN2891    requires DataLink.  If you dont have a link, use CreateMethodStepWithDefDataLink
  20.01.06 pk  fDefaultDataLink             TN2891    used in CreateMethodStepWithDefDataLink
  24.01.06 wl  Create                       TN2885    neu: TSQLSelectMethodStep
  25.03.06 pk  Create                       TN2997    New : TInitDeviceMethodStep
  11.04.06 wl  Create                       TN3014    REAG Action wieder eingeführt
  07.09.06 wl  Create                       TN3288    CALLI action entfernt
  07.09.06 wl  Create                       TN3287    Neu: WGWIZ statt MASSP !!!
  20.09.06 wl  Create                       TN3318    neu: TMotorMoveMethodStep
  06.11.06 wl  Create                       TN3394    neu: TSensorCheckMethodStep
  05.12.06 wl  Create                       TN3448    Actions WASH und WASH? entfernt
  05.12.06 wl  Create                       TN3448    Actions WFSLA, IMPRA, EXPRA, CANCM, PORT, QUADR, EDITL sind nicht mehr sichtbar
  18.01.07 pk  Create                       TN3482    TAddSequenceMethodStep
  19.02.07 wl  Create                       TN3585    neu: TVirtualRackMoveAction
  21.02.07 wl  Create                       TN3588    neu: TTipsGetMethodStep
  18.09.07 pk                               TN3864    uses RackTypes removed
  31.08.07 wl  Create                       TN3844   neu: TPowderDetectionMethodStep
  14.04.08 wl                               TN4060    does not use ModuleTypeInfo any more
  06.05.08 wl  ReadMethod                   TN4074    Method can be read with or without Inactive records
  21.05.08 wl  Create                       TN4119    DictionaryName new for Logging
  08.09.08 pk                               TN4215    TUknownMethodStep
  25.09.08 wl                               TN4242    uses MethodStepSettingRunStart
  06.10.08 pk  GetAddedMethodNameFromAction TN4258    no longer class function
  10.11.08 pk  GetAddedMethodNameFromAction TN4280    StartThread instead of RunStart
  19.11.08 pk  GetAddedMethodNameFromAction TN4280    StartThread removed
  20.02.09 wl                               TN4438    benutzt TLanguageString
  07.04.09 pk  SaveFavouriteActions         TN4497    Call WriteFromCache
  08.05.09 wl                               TN4561    Action-Description wird nicht mehr als Kommentar hinzugefügt
  04.11.09 pk                               TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  31.05.10 wl  GetRunStepBuilderCreator     TN5120   ComparableActionName entfernt
  21.09.10 pk                               TN5089    Various changes to MethodEditor for displaying indented actions
  23.09.10 pk  CreateMethodStep             TN5089    Now possible to create MethodStep and set all its fields (including comment)
  29.09.10 pk                               TN5283    Short and Long method step comments combined
  29.09.10 pk  IsActionExecutable           TN5284    New
  09.08.12 wl                               TN5946   Laden der statischen Member entfernt
  13.12.12 wl  SaveFavouriteActions         TN6054   entfernt
  27.02.13 wl                               TN6045   uses Generics.Collections
  30.08.13 wl  GetCommentWithMethName       TN6236   verwendet TMethodSettingsDataAdaptor
  -------------------------------------------------------------------------------------------------- }

unit RunStepBuilderTypeDictionary;


interface


uses
    Classes,
    TypeDictionary,
    TypeInfo,
    RunStepInfo,
    MethodStep,
    MethodTypes,
    MethodStepDataFields,
    RunStepBuilder,
    MethodDataAdaptor;

type
    TRunStepBuilderClass = class of TRunStepBuilder;
    TRunStepBuilderClassArray = array of TRunStepBuilderClass;

    TRunStepBuilderTypeDictionary = class(TTypeDictionary)
    private
        fDefaultDataLink: TMethodStepDataLink;
        class function GetCommentWithMethName(const aActionName, aCommentString, aMethodName: string): string;
        function CreateRunStepBuilderNoPrepare(aActionName: string): TRunStepByMethodStepBuilder;
        function GetRunStepBuilderCreator(const aActionName: string): TRunStepBuilderCreator;

        class var uInstance: TRunStepBuilderTypeDictionary;
        // private constructor (for singleton class)
        constructor Create();
    protected
        function IsValidType(aTypeInfo: TTypeInfo): boolean; override;
        procedure InitTypeInfoList(); override;
    public
        class procedure CreateInstance();
        class procedure DestroyInstance;
        class function Instance: TRunStepBuilderTypeDictionary;
        function CreateRunStepInfo(const aActionName: string): TRunStepInfo;
        function IsActionExecutable(const aActionName: string): boolean;

        function CreateMethodStep(const aActionName: string; const aMDataLink: TMethodStepDataLink;
            const aOnAddEditFunctions: TNotifyEvent): TMethodStep; overload;
        function CreateMethodStep(const aActionName: string; const aOnAddEditFunctions: TNotifyEvent)
            : TMethodStep; overload;
        function CreateMethodStep(const aActionName: string): TMethodStep; overload;
        function CreateMethodStep(const aActionName: string; const aOptions: string; const aComment: string;
            const aInactive: boolean; const aCreateIfInactive: boolean;
            const aOnAddEditFunctions: TNotifyEvent): TMethodStep; overload;
        function CreateMethodStepFromMethodRec(const aMethodRec: TMethodRec; const aCreateIfInactive: boolean;
            const aOnAddEditFunctions: TNotifyEvent): TMethodStep;
        function CreateRunStepBuilder(aActionName: string; aMDataLink: TMethodStepDataLink;
            aOnAddEditFunctions: TNotifyEvent): TRunStepByMethodStepBuilder;
        function CreateRunStepBuilderWithDefDataLink(const aActionName: string): TRunStepByMethodStepBuilder;
        procedure GetActionDescription(aActionName: string; out oCaption, oDescription: string);
        function GetComment(const aActionName, aOptions, aCommentString: string): string;
        procedure RefreshComments(aMethodStep: TMethodStep);
        function ActionChangesRackPositionInLayout(const aActionName: string): boolean;
        function ActionPaintsPositions(const aActionName: string): boolean;

        function GetAddedMethodNameFromAction(const aActionName, aOptions: string): string;

        property DefaultDataLink: TMethodStepDataLink read fDefaultDataLink;
    end;


implementation


uses
    SysUtils,
    UnknownMethodStep,
    MethodSettingsDataAdaptor,
    SubMethodRunStep,
    MethodStepSettingRunStart,
    RunStepInfoFactory,
    PluginLoader;

{ TRunStepBuilderTypeDictionary }

class function TRunStepBuilderTypeDictionary.Instance(): TRunStepBuilderTypeDictionary;
begin
    result := uInstance;
end;

class procedure TRunStepBuilderTypeDictionary.CreateInstance;
begin
    if Assigned(uInstance) then
        EXIT;
    uInstance := TRunStepBuilderTypeDictionary.Create();
end;

class procedure TRunStepBuilderTypeDictionary.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;

{ TRunStepBuilderTypeDictionary }

constructor TRunStepBuilderTypeDictionary.Create;
begin
    inherited Create('Run Step Type Dictionary');

    fDefaultDataLink := TMethodStepDataLink.Create();
    fDefaultDataLink.InitWithDefaults();
end;

procedure TRunStepBuilderTypeDictionary.InitTypeInfoList;
begin
    TPluginLoader.LoadAllTypes(self);
end;

function TRunStepBuilderTypeDictionary.IsValidType(aTypeInfo: TTypeInfo): boolean;
begin
    result := aTypeInfo is TRunStepBuilderTypeInfo;
end;

procedure TRunStepBuilderTypeDictionary.GetActionDescription(aActionName: string;
    out oCaption, oDescription: string);
var
    xRunStepInfo: TRunStepInfo;
begin
    oCaption := '';
    oDescription := '';
    xRunStepInfo := self.CreateRunStepInfo(aActionName);
    if not Assigned(xRunStepInfo) then
        EXIT;
    try
        oCaption := xRunStepInfo.Caption;
        oDescription := xRunStepInfo.Description;
    finally
        xRunStepInfo.Free;
    end;
end;

function TRunStepBuilderTypeDictionary.IsActionExecutable(const aActionName: string): boolean;
var
    xRunStepInfo: TRunStepInfo;
begin
    result := true;
    xRunStepInfo := self.CreateRunStepInfo(aActionName);
    try
        // it maybe wierd that when we dont find an action it is executable but this should be checked later
        if not Assigned(xRunStepInfo) then
            EXIT;

        result := xRunStepInfo.IsExecutable;
    finally
        xRunStepInfo.Free;
    end;

end;

function TRunStepBuilderTypeDictionary.GetRunStepBuilderCreator(const aActionName: string)
    : TRunStepBuilderCreator;
var
    xTypeInfo: TTypeInfo;
    xActionName: string;
begin
    xActionName := aActionName;

    xTypeInfo := self.GetTypeFromTypeName(xActionName);
    result := nil;
    if (xTypeInfo is TRunStepBuilderTypeInfo) then
        result := (xTypeInfo as TRunStepBuilderTypeInfo).RunStepBuilderCreator;
end;

function TRunStepBuilderTypeDictionary.CreateRunStepInfo(const aActionName: string): TRunStepInfo;
var
    xCreator: TRunStepBuilderCreator;
begin
    result := nil;

    xCreator := GetRunStepBuilderCreator(aActionName);
    if not Assigned(xCreator) then
        EXIT;

    result := TRunStepInfoFactory.CreateRunStepInfoByTypeName(aActionName);
end;

function TRunStepBuilderTypeDictionary.CreateMethodStep(const aActionName: string;
    const aMDataLink: TMethodStepDataLink; const aOnAddEditFunctions: TNotifyEvent): TMethodStep;
var
    xCreator: TRunStepBuilderCreator;
begin
    xCreator := self.GetRunStepBuilderCreator(aActionName);
    if (xCreator = nil) then
    begin
        result := TUnknownMethodStep.Create(aActionName, aMDataLink, aOnAddEditFunctions);
    end
    else
    begin
        result := xCreator.CreateMethodStep(aActionName, aMDataLink, aOnAddEditFunctions);
    end;
    result.MDataLink := aMDataLink;
    result.OnAddEditFunctions := aOnAddEditFunctions;
end;

function TRunStepBuilderTypeDictionary.CreateMethodStep(const aActionName: string): TMethodStep;
begin
    result := CreateMethodStep(aActionName, self.DefaultDataLink, TNotifyEvent(nil));
end;

function TRunStepBuilderTypeDictionary.CreateMethodStep(const aActionName: string;
    const aOnAddEditFunctions: TNotifyEvent): TMethodStep;
begin
    result := CreateMethodStep(aActionName, self.DefaultDataLink, aOnAddEditFunctions);
end;

function TRunStepBuilderTypeDictionary.CreateMethodStep(const aActionName: string; const aOptions: string;
    const aComment: string; const aInactive: boolean; const aCreateIfInactive: boolean;
    const aOnAddEditFunctions: TNotifyEvent): TMethodStep;

begin
    result := nil;
    if (aInactive) and (not aCreateIfInactive) then
        EXIT;

    result := CreateMethodStep(aActionName, aOnAddEditFunctions);
    if not Assigned(result) then
        EXIT;

    result.M_Options := aOptions;
    result.M_Comments := aComment;
    result.InactiveAsBool := aInactive;
end;

function TRunStepBuilderTypeDictionary.CreateMethodStepFromMethodRec(const aMethodRec: TMethodRec;
    const aCreateIfInactive: boolean; const aOnAddEditFunctions: TNotifyEvent): TMethodStep;
begin
    result := CreateMethodStep(aMethodRec.Action, aMethodRec.Options, aMethodRec.Comment, aMethodRec.Inactive,
        aCreateIfInactive, aOnAddEditFunctions);
end;

function TRunStepBuilderTypeDictionary.CreateRunStepBuilderNoPrepare(aActionName: string)
    : TRunStepByMethodStepBuilder;
var
    xCreator: TRunStepBuilderCreator;
    xBuilder: TRunStepBuilder;
begin
    result := nil;

    xCreator := GetRunStepBuilderCreator(aActionName);
    if not Assigned(xCreator) then
        EXIT;

    xBuilder := xCreator.CreateRunStepBuilder(aActionName);

    ASSERT(xBuilder is TRunStepByMethodStepBuilder, 'Run Step Builder can not be created');
    result := xBuilder as TRunStepByMethodStepBuilder;
end;

function TRunStepBuilderTypeDictionary.CreateRunStepBuilder(aActionName: string;
    aMDataLink: TMethodStepDataLink; aOnAddEditFunctions: TNotifyEvent): TRunStepByMethodStepBuilder;
begin
    result := CreateRunStepBuilderNoPrepare(aActionName);
    result.MDataLink := aMDataLink;
    result.OnAddEditFunctions := aOnAddEditFunctions;
    result.Prepare();
end;

function TRunStepBuilderTypeDictionary.CreateRunStepBuilderWithDefDataLink(const aActionName: string)
    : TRunStepByMethodStepBuilder;
begin
    result := CreateRunStepBuilderNoPrepare(aActionName);
    if not Assigned(result) then
        EXIT;
    result.MDataLink := self.DefaultDataLink;
    result.OnAddEditFunctions := nil;
    result.Prepare();
end;

class function TRunStepBuilderTypeDictionary.GetCommentWithMethName(const aActionName, aCommentString,
    aMethodName: string): string;
var
    xDA: TMethodSettingsDataAdaptor;
    xRec: TMethodSettingsRec;
begin
    if (aMethodName <> '') then
    begin
        // Kommentar aus Untermethode
        xDA := TMethodSettingsDataAdaptor.Create;
        try
            xRec := xDA.ReadRec(aMethodName);
            EXIT(xRec.Comment);
        finally
            FreeAndNil(xDA);
        end;
    end;

    EXIT(aCommentString);
end;

function TRunStepBuilderTypeDictionary.GetComment(const aActionName, aOptions,
    aCommentString: string): string;
var
    xMethodName: string;
begin
    xMethodName := GetAddedMethodNameFromAction(aActionName, aOptions);

    EXIT(GetCommentWithMethName(aActionName, aCommentString, xMethodName); end;

        procedure TRunStepBuilderTypeDictionary.RefreshComments(aMethodStep: TMethodStep);
    begin aMethodStep.M_Comments := GetComment(aMethodStep.ActionName, aMethodStep.M_Options,
        aMethodStep.M_Comments); end;

        function TRunStepBuilderTypeDictionary.GetAddedMethodNameFromAction(const aActionName,
        aOptions: string): string; var xMethodStep: TMethodStep; begin

        xMethodStep := self.CreateMethodStep(aActionName);
        try if Assigned(xMethodStep) then begin xMethodStep.M_Options := aOptions; end;

        // Ermitteln des Methoden/Run-Namens
        if xMethodStep is TSubMethodMethodStep then begin result := (xMethodStep as TSubMethodMethodStep)
        .MainSubOptionSetting.MethName.Value; end else result := ''; finally xMethodStep.Free; end; end;

        function TRunStepBuilderTypeDictionary.ActionChangesRackPositionInLayout(const aActionName: string)
        : boolean; var xRunStepInfo: TRunStepInfo; begin result := false;
        xRunStepInfo := self.CreateRunStepInfo(aActionName); if not Assigned(xRunStepInfo) then EXIT;
        try result := xRunStepInfo.ChangesRackPositionInLayout; finally xRunStepInfo.Free; end; end;

        function TRunStepBuilderTypeDictionary.ActionPaintsPositions(const aActionName: string): boolean;
        var xRunStepInfo: TRunStepInfo; begin result := false;

        xRunStepInfo := self.CreateRunStepInfo(aActionName); if not Assigned(xRunStepInfo) then EXIT;
        try result := xRunStepInfo.PaintPositions; finally xRunStepInfo.Free; end; end;

    end.
