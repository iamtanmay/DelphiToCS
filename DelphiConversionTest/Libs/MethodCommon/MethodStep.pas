{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Project      : New Editor
  Author       : Wolfgang Lyncke (wl)
  Description  : Manager for method steps
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  17.08.04 wl                               TN2008.3  initial version
  24.09.04 wl                               TN2008.3  für MoveR, Delay und Msg alles eingerichtet
  11.10.04 wl                               TN2008.3  improved
  21.12.04 wl                               TN2247.1  verschiedene Ergänzungen
  26.08.05 wl  TMethodStep.GetEditingProperties  TN2541.0  Bezug auf cxEdit entfernt
  01.11.05 wl  TMethodStep.AsMethodRec           TN2541.0  füllt TMethodRec mit Daten
  03.11.05 wl  TMethodStep.AddMethodImportOption TN2724    liest MethodImport-Parameter und fügt sie einem gegebenen String hinzu
  03.11.05 wl  TMethodStep.SetMethodImportOption TN2724    schreibt in die MethodImport-Parameter
  03.11.05 wl  TMethodStep.SetValue              TN2724    ruft SetMethodImportOption auf
  03.11.05 wl  TMethodStep.GetValue              TN2724    ruft AddMethodImportOption auf
  20.01.06 pk  TMethodStepDataLink               TN2891    New
  20.01.06 pk  TMethodStepDataField              TN2891    New
  20.01.06 pk  TMethodStep                       TN2891    inherits from TMethodStepCompositeParameter
  20.01.06 pk  TMethodStep                       TN2891    fDataFields : keeps a list of TMethodStepDataFields which can read and write data
  26.01.06 pk  TMethodStep.AddOptionViewParam    TN2901    call AddSchedulerViewParams
  06.04.06 pk                                    TN3024    New field SchedSharedID
  10.04.06 pk                                    TN3032    New field Iterate
  18.04.06 wl  TMethodStep.AsMethodRec           TN3025    Inactive komplett entfernt
  21.07.06 pk  cxGrid1TableView1CellDblClick    TN3213   Use TSubMethodMethodStep cast to get method name
  21.07.06 pk  TMethodStep.ReadData             TN3213   read all datafields
  02.10.06 wl  OnGetMaxLength                    TN3236    neue Property von TMethodStepParameter und TMethodStepDataLink
  02.10.06 wl  TMethodStep.LinkViewAndDataParam  TN3236    hier wird MaxLenght eines Parameters bestimmt
  07.12.06 wl                                    TN3456    aOwner aus TMethodStepParameter entfernt
  07.12.06 wl  HasGenericImportOptions           TN3456    ersetzt fHasGenericOptions
  26.07.07 pk  fNameParam                        TN3805    New
  03.08.07 wl                                    TN3811.2 uses IntMap statt ObjStructures
  08.01.08 wl  TMethodStepDataLink,TMethodStepDataFields  TN3972   --> MethodStepDataFields
  09.01.08 wl                                    TN3972    Massive Changes
  07.02.08 wl  CreateOptionsTree                 TN4009    Trennung: Scheduling Options und Iterate
  07.02.08 wl  TMethodStep.GetOptionSummary      TN4009    Summary = Value, wenn es nur einen Parameter gibt
  17.04.08 wl                                    TN4067    several changes to make the import work again
  06.05.08 wl  M_Inactive,InactiveAsBool         TN4074    New Parameter that represents the field Inactive
  04.06.08 wl  GetGenericImportOptions           TN4136    no result if HasGenericImportAction = false
  04.06.08 wl  RemoveGenericImportOptions        TN4136    EXIT if HasGenericImportAction = false
  19.09.08 pk  HasMultiOptions                   TN4215    New: Does the options field have more than one action parameter
  06.10.08 pk  fParseField, fKey, fConstant      TN4258    New
  06.10.08 pk  M_SRack, etc                      TN4258    removed
  07.10.08 pk  GetOptionSummary                  TN4258    return blank by default
  20.02.09 wl  TMethodStepSetting_SchedulingOptions  TN4438    aDescription direkt angegeben statt mit aResNo
  23.02.09 wl  TMethodStepSetting_Comments       TN4438    Fehler korrigiert
  19.08.09 wl                                    TN4702   Strings werden jetzt direkt geladen
  07.05.10 wl                                    TN5087    erheblich aufgeräumt
  12.05.10 wl  GetSummary                        TN5064    erzeugt eine Zeile für Action-Typ, eine für Kommentar und weitere für alle Parameter
  17.05.10 wl  GetSummary                        TN5064    Kommentarzeile ist optional
  21.09.10 pk  IsCodeStep                        TN5089    New
  29.09.10 pk                                    TN5283    Short and Long method step comments combined
  30.09.10 pk  DoFindText                        TN5287    New: needed for Find in Methods
  07.02.11 wl  SetFirstSubOptionValue            TN5461   wird für MethodenEditor benötigt
  05.03.11 wl  RelatedItemParam                  TN5472   ersetzt NameParam
  29.06.11 wl  EvaluateType                      TN5618   ersetzt IsConstant (Boolean)
  18.11.11 wl                                    TN5741   Iterate unsichtbar gemacht (braucht das irgendwer?)
  14.12.11 wl                                    TN5765   ohne Iterate und SchedulingOptions
  01.03.12 wl                                    TN5822   uses geändert
  21.03.13 wl                                    TN6045   an TCustomSetting-Änderungen angepasst
  07.11.13 ts  GetMainSubOptionSetting           TN6297   result ist nil, wenn Parameter nicht vorhanden ist
  -------------------------------------------------------------------------------------------------- }

unit MethodStep;


interface


uses
    Classes,
    MethodStepDataFields,
    CustomSetting,
    AppTypes,
    MethodDataAdaptor,
    RunStepInfo,
    MethodTypes,
    Streamable;

const
    cInactiveTrueAsString = 'I';
    cInactiveFalseAsString = '';

type
    TMethodStepCompositeSetting = class(TCustomCompositeSetting)
    protected
        function GetOptionSummary: string; virtual; // nur noch Dummy-Funktion
    public
        function AddParam(const aDescription: string; aVisible: boolean;
            aEditType: TCustomSettingEditType = cpeNone; aOnAddEditFunctions: TNotifyEvent = nil)
            : TCustomSetting; overload;
        property OptionSummary: string read GetOptionSummary;
    end;

    TMethodStepSetting_Comments = class(TCustomLeafSetting)
    public
        constructor Create();
    end;

    TMethodStep = class
    private
        // fActionName: string;
        fDataFields: TMethodStepDataFields;
        fOnParamChange: TNotifyEvent;
        fOnAddEditFunctions: TNotifyEvent;
        fUnparsedOptions: string;
        fSeq: integer;
        fStepParam: TMethodStepCompositeSetting;
        fCommentsParam: TMethodStepSetting_Comments;
        fOptionsParam: TMethodStepCompositeSetting;
        fInactiveParam: TCustomSetting;
        fRelatedItemParam: TCustomSetting;

        function GetM_Comments: string;
        procedure SetM_Comments(const aValue: string);

        procedure DataField_OnReadData(aSender: TObject; var vValue: string);
        procedure DataField_OnWriteData(aSender: TObject; const aValue: string);

        procedure CreateCommentsTree(aOnAddEditFunctions: TNotifyEvent);
        procedure CreateOptionsTree(aOnAddEditFunctions: TNotifyEvent);
        procedure Setting_OnChange(aSender: TObject);
        function GetActionName: string;
        function GetM_Inactive: string;
        procedure SetM_Inactive(const aValue: string);
        function GetInactiveAsBool: boolean;
        function GetMainSubOptionSetting: TCustomSetting;
        function GetOptionSummary: string;
    protected
        fMDataLink: TMethodStepDataLink;
        fStepInfo: TRunStepInfo;

        class procedure LinkViewAndDataParam(aParam: TCustomSetting; aDataField: TMethodStepDataField);
        function AddDataParam(aDataID: integer): TMethodStepDataField;
        function AddOptionViewSubParam(aParam: TCustomSetting): TCustomSetting;

        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); virtual; abstract;
        function CreateStepInfo(): TRunStepInfo; virtual; abstract;
        function GetSummaryFirstLine(): string; virtual;
        function GetSummaryParameters(): string; virtual;
        function GetIsCodeStep(): boolean;
        //
        function GetM_Options: string;
        procedure SetM_Options(const aValue: string);
    public
        // constructor/destructor
        constructor Create(const aActionName: string; aMDataLink: TMethodStepDataLink;
            aOnAddEditFunctions: TNotifyEvent); virtual;
        destructor Destroy; override;
        //
        function GetDefaultName: string;

        function GetActionCaption(): string;
        function GetActionCaptionText(): string; virtual;
        function GetActionDescription(): string;
        function AsMethodRec(const aMethodName: string; aSeq: integer): TMethodRec;
        procedure ReadData();
        procedure SetInactiveAsBool(aValue: boolean);
        function GetSummary(): string;

        function FindOptionSubSetting(const aKey: string): TCustomSetting;

        function FindText(const aSearchText: string; const aResults: TCustomSettingSearchResultList): boolean;

        procedure SetFirstSubOptionValue(const aValue: string); virtual;

        // Properties
        property ActionName: string read GetActionName;
        property M_Seq: integer read fSeq write fSeq;
        property M_Options: string read GetM_Options write SetM_Options;
        property M_UnparsedOptions: string read fUnparsedOptions write fUnparsedOptions;
        property M_Comments: string read GetM_Comments write SetM_Comments;
        property M_Inactive: string read GetM_Inactive write SetM_Inactive;
        property InactiveAsBool: boolean read GetInactiveAsBool write SetInactiveAsBool;
        property Settings: TMethodStepCompositeSetting read fStepParam;
        property RelatedItemParam: TCustomSetting read fRelatedItemParam write fRelatedItemParam;
        property OnAddEditFunctions: TNotifyEvent read fOnAddEditFunctions write fOnAddEditFunctions;

        property MDataLink: TMethodStepDataLink read fMDataLink write fMDataLink;
        property OnParamChange: TNotifyEvent read fOnParamChange write fOnParamChange;
        property DataFields: TMethodStepDataFields read fDataFields;
        property StepInfo: TRunStepInfo read fStepInfo;
        property OptionsParam: TMethodStepCompositeSetting read fOptionsParam write fOptionsParam;
        property MainSubOptionSetting: TCustomSetting read GetMainSubOptionSetting;
        property OptionSummary: string read GetOptionSummary;
        property SummaryFirstLine: string read GetSummaryFirstLine;
        property SummaryParamters: string read GetSummaryParameters;
        property IsCodeStep: boolean read GetIsCodeStep;
    end;

    TMethodStepClass = class of TMethodStep;

    TMethodStepCompositeConcatSetting = class(TMethodStepCompositeSetting)
    protected
        function GetValue: string; override;
        procedure SetValue(const aValue: string); override;
    public
        constructor Create(const aKey: string);
    end;


implementation


uses
    SysUtils,
    MethodGUIParsing,
    CustomLeafSettings,
    ImportDataAdaptor,
    AppSettings,
    GeneralTypes;

{ TMethodStep }

constructor TMethodStep.Create(const aActionName: string; aMDataLink: TMethodStepDataLink;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create;

    // fActionName := aActionName;
    fRelatedItemParam := nil;
    fStepInfo := CreateStepInfo();

    fMDataLink := aMDataLink;
    fDataFields := TMethodStepDataFields.Create();
    fUnparsedOptions := '';
    fSeq := 0;

    fStepParam := TMethodStepCompositeConcatSetting.Create('MainSetting');

    CreateOptionsTree(aOnAddEditFunctions);
    CreateCommentsTree(aOnAddEditFunctions);

    fInactiveParam := TCustomLeafSetting.Create('', '', false);
    fStepParam.AddParam(fInactiveParam);
    LinkViewAndDataParam(fInactiveParam, AddDataParam(fMDataLink.Inactive));
    fInactiveParam.SetOnChangeForAll(self.Setting_OnChange);
end;

procedure TMethodStep.CreateCommentsTree(aOnAddEditFunctions: TNotifyEvent);
begin
    fCommentsParam := TMethodStepSetting_Comments.Create();

    fStepParam.AddParam(fCommentsParam);
    LinkViewAndDataParam(fCommentsParam, AddDataParam(fMDataLink.Comment));
    fCommentsParam.SetOnChangeForAll(self.Setting_OnChange);
end;

procedure TMethodStep.CreateOptionsTree(aOnAddEditFunctions: TNotifyEvent);
var
    xOptionsDataField: TMethodStepDataField;
begin
    // für die Options wird alles festgelegt
    xOptionsDataField := AddDataParam(self.fMDataLink.Option);
    fOptionsParam := TMethodStepCompositeConcatSetting.Create('Options');
    fStepParam.AddParam(fOptionsParam);
    xOptionsDataField.Param := fOptionsParam;

    // hier kommen alle Setting, die in den Options codiert sind (spezifisch für die Action)
    CreateOptionsTreeSpecific(aOnAddEditFunctions);

    // setzen des Root-Data-Handle für Options und alle Sub-Settings
    fOptionsParam.RootDataHandle := xOptionsDataField;
    fOptionsParam.SetOnChangeForAll(self.Setting_OnChange);
end;

destructor TMethodStep.Destroy;
begin
    fDataFields.Free;
    fStepInfo.Free;

    inherited;
end;

function TMethodStep.GetActionCaption(): string;
begin
    result := fStepInfo.Caption;
end;

function TMethodStep.GetActionCaptionText: string;
begin
    EXIT(self.GetActionCaption + ': ');
end;

function TMethodStep.GetActionDescription(): string;
begin
    result := fStepInfo.Description;
end;

function TMethodStep.GetIsCodeStep(): boolean;
begin
    result := fStepInfo.IsCodeStep;
end;

procedure TMethodStep.Setting_OnChange(aSender: TObject);
var
    xParam: TCustomSetting;
    xDataField: TMethodStepDataField;
begin
    if not(aSender is TCustomSetting) then
        EXIT;
    xParam := (aSender as TCustomSetting);

    if (xParam.DataHandle is TMethodStepDataField) then
    begin
        xDataField := xParam.DataHandle as TMethodStepDataField;
        xDataField.WriteData();
    end;
    if (xParam.RootDataHandle is TMethodStepDataField) then
    begin
        xDataField := xParam.RootDataHandle as TMethodStepDataField;
        xDataField.WriteData();
    end;
end;

procedure TMethodStep.DataField_OnReadData(aSender: TObject; var vValue: string);
begin
    if Assigned(self.fMDataLink.OnReadData) then
        self.fMDataLink.OnReadData(aSender, (aSender as TMethodStepDataField).DataID, vValue);
end;

procedure TMethodStep.DataField_OnWriteData(aSender: TObject; const aValue: string);
begin
    if Assigned(self.fMDataLink.OnWriteData) then
        self.fMDataLink.OnWriteData(aSender, (aSender as TMethodStepDataField).DataID, aValue);
end;

class procedure TMethodStep.LinkViewAndDataParam(aParam: TCustomSetting; aDataField: TMethodStepDataField);
begin
    aParam.DataHandle := aDataField;
    aDataField.Param := aParam;
end;

function TMethodStep.AddDataParam(aDataID: integer): TMethodStepDataField;
begin
    result := TMethodStepDataField.Create(aDataID, nil);
    result.OnReadData := self.DataField_OnReadData;
    result.OnWriteData := self.DataField_OnWriteData;

    fDataFields.AddObject(aDataID, result);
end;

function TMethodStep.AddOptionViewSubParam(aParam: TCustomSetting): TCustomSetting;
begin
    result := fOptionsParam.AddParam(aParam);
end;

function TMethodStep.GetM_Options: string;
begin
    result := fDataFields.GetValue(fMDataLink.Option);
end;

procedure TMethodStep.SetM_Options(const aValue: string);
begin
    fDataFields.SetValue(fMDataLink.Option, aValue);
end;

function TMethodStep.GetM_Comments: string;
begin
    result := fCommentsParam.Value;
end;

procedure TMethodStep.SetFirstSubOptionValue(const aValue: string);
begin
    // nothing happens
end;

procedure TMethodStep.SetM_Comments(const aValue: string);
begin
    fCommentsParam.Value := aValue;
end;

function TMethodStep.AsMethodRec(const aMethodName: string; aSeq: integer): TMethodRec;
begin
    result := TMethodDataAdaptor.MakeMethodRec(true { Valid } , aMethodName, aSeq, GetDefaultName,
        self.M_Options, self.M_Comments, self.InactiveAsBool);
end;

procedure TMethodStep.ReadData();
var
    i: integer;
begin
    for i := 0 to fDataFields.Count - 1 do
    begin
        fDataFields[i].ReadData();
    end;
end;

function TMethodStep.GetActionName: string;
begin
    result := GetDefaultName; // fActionName;
end;

function TMethodStep.GetDefaultName: string;
begin
    result := fStepInfo.DefaultName;
end;

function TMethodStep.GetM_Inactive: string;
begin
    result := fDataFields.GetValue(fMDataLink.Inactive);
end;

procedure TMethodStep.SetM_Inactive(const aValue: string);
begin
    fDataFields.SetValue(fMDataLink.Inactive, aValue);
end;

procedure TMethodStep.SetInactiveAsBool(aValue: boolean);
begin
    if (aValue) then
        fDataFields.SetValue(fMDataLink.Inactive, cInactiveTrueAsString)
    else
        fDataFields.SetValue(fMDataLink.Inactive, cInactiveFalseAsString);
end;

function TMethodStep.GetInactiveAsBool: boolean;
begin
    result := fDataFields.GetValue(fMDataLink.Inactive) = cInactiveTrueAsString;
end;

function TMethodStep.FindOptionSubSetting(const aKey: string): TCustomSetting;
begin
    result := fOptionsParam.Find(aKey);
    ASSERT(Assigned(result), TTypeSafeFormat.Format('Options parameter [{0}] not found', [aKey]));
end;

function TMethodStep.GetMainSubOptionSetting: TCustomSetting;
begin
    result := nil;
    if fOptionsParam.Params.Count > 0 then
        result := fOptionsParam.Params[0];
end;

function TMethodStep.GetSummaryFirstLine: string;
begin
    // Zuerst der ActionName und einfache Überschrift
    result := self.GetActionCaption + ' (' + self.ActionName + ')';
end;

function TMethodStep.GetSummaryParameters(): string;
begin
    result := '';
    if Assigned(self.MainSubOptionSetting) then
        self.MainSubOptionSetting.GetParamText(result, 0);
end;

function TMethodStep.GetSummary(): string;
begin
    result := self.GetSummaryFirstLine;

    // Dann ausführlich: Alle Parameter geordnet anzeigen
    result := result + GetSummaryParameters();
end;

function TMethodStep.GetOptionSummary: string;
begin
    if (self.MainSubOptionSetting is TMethodStepCompositeSetting) then
        result := (self.MainSubOptionSetting as TMethodStepCompositeSetting).OptionSummary
    else if Assigned(self.MainSubOptionSetting) then
        result := self.MainSubOptionSetting.Value
    else
        result := '';
end;

function TMethodStep.FindText(const aSearchText: string;
    const aResults: TCustomSettingSearchResultList): boolean;
begin
    result := self.OptionsParam.FindText(aSearchText, aResults);
end;

{ TMethodStepCompositeConcatSetting }

constructor TMethodStepCompositeConcatSetting.Create(const aKey: string);
begin
    inherited Create(aKey, '', false);
end;

function TMethodStepCompositeConcatSetting.GetValue: string;
var
    xValue: string;
    x: integer;
begin
    result := '';
    for x := 0 to self.Params.Count - 1 do
    begin
        xValue := self.Params[x].Value;
        result := TMethodGUIParser.ConcatOptions(result, xValue);
    end;
end;

procedure TMethodStepCompositeConcatSetting.SetValue(const aValue: string);
var
    x: integer;
begin
    for x := 0 to self.Params.Count - 1 do
    begin
        self.Params[x].Value := aValue;
    end;
end;

{ TMethodStepCompositeSetting }

function TMethodStepCompositeSetting.AddParam(const aDescription: string; aVisible: boolean;
    aEditType: TCustomSettingEditType; aOnAddEditFunctions: TNotifyEvent): TCustomSetting;
begin
    result := inherited AddParam('', aDescription, aVisible, aEditType, aOnAddEditFunctions);
end;

function TMethodStepCompositeSetting.GetOptionSummary: string;
begin
    result := '';
end;

{ TMethodStepSetting_Comments }

constructor TMethodStepSetting_Comments.Create();
const
    cIsCommentPropVisibleInPropertiesEditor = false;
begin
    inherited Create('Comments', TLanguageString.Read('Comment', 'Kommentar'),
        cIsCommentPropVisibleInPropertiesEditor);
    self.EvaluateType := TCustomSettingEvaluateType.Constant;
    SetValue('');
end;


end.
