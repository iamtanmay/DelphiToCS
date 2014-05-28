{ --------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Handle calls to Delphi Graphics ( VCL ) functions
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                             track-no improvement/change
  -------- --  --------------------------------   -------- -----------------------------------------
  08.04.09 pk                                     TN4585.1 New
  23.06.09 pk  SetMainPanelParentVisible          TN4538   check if RunMainControls is assigned
  06.07.09 pk  SQLResult_Display                  TN4585.4 New
  24.07.09 pk  InternGroupDisplay_InsertInfo      TN4675   New
  31.07.09 ts  Intern-/ GroupDisplay_InsertInfo   TN4666   New InfoGroupBehaviour
  12.08.09 wl  ToolErr_PromptInput                TN4712    ein Parameter entfernt
  21.08.09 wl  TRunDialogsManager.VCL             TN4702   Form-Instanzen von Stop & RunAbortInfo gelöscht
  26.08.09 pk  fDisplayComponentManager           TN4753   New: instead of using TDisplayComponentManager.Instance
  08.09.09 pk                                     TN4753   call to ErrorMessageFactory.ErrorBox replaced by ErrorMessage.InstPromptModal
  24.09.09 pk  UnloadDisplayComponent             TN4753   call fDisplaycomponentManager.Clear
  28.09.09 pk  SystemVolume_Changed               TN4753   New
  04.11.09 pk                               	 TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  04.02.10 pk                               	 TN4972   Changes for Restart
  05.02.10 ts  InternGroupDisplay_InsertInfo      TN5008   if aDisplayID = '' (INFO-Action), insert info into default window
  01.04.09.ts  ChartDisplay_AddValue              TN4785   New
  01.04.10 pk                                     TN5003.2  New EnterKeyNavigationOnly parameter
  04.09.10 pk  DisplayComponents_RunEnable        TN5042   New: used for enabling/disabling stop button on displaycomponents
  04.09.10 pk                                     TN5042   New: SimulationMode_Enable, etc. functions from TRunMain
  23.04.10 pk  EditMethodParameters_PromptModal   TN5062   New Parameter aIsBuildMode
  27.04.10 pk  VCL:vclRunAbortPromptModal         TN5074   TStrings changed to TStringArray
  11.05.10 pk  InternLoadDisplayComponentToMain   TN5110   Clear old log lines so that reloading loginfo window does not take so much time
  19.05.10 pk  AskRunStart_PromptModal            TN5114   New
  07.06.10 pk  RestartPreparations_PromptModal    TN5077   New
  20.07.10 pk                                     TN5201   ChangeTime function now with Progress parameter
  26.10.10 pk  AskRestart_PromptModal             TN5297   New
  27.10.10 pk  InternDisplayComponentsRunEnable   TN5313   enable/disable all buttons, not just Stop button
  14.12.10 wl  TRunDialogsManager.VCL             TN5411   IntToObj ersetzt
  02.02.11 wl  SelectItemBox                      TN5466   neu
  21.03.11 wl                                     TN5508  SimulationInfo --> Elemente nach AskRunStart verlegt
  14.04.11 wl  vclAskRunStartPromptModal          TN5554   Bugfix
  25.05.11 ts  SQLTable_Refresh                   TN5590   new: Refresh for SQLTableDisplayComponent
  17.11.11 wl                                     TN5725   PositionInfo-Fenster entfernt
  14.12.11 wl                                     TN5765   MethDone-Dialog durch MessageBox ersetzt
  20.04.12 wl                                     TN5858   an stark geändertes RunMain-Fenster angepasst
  29.05.12 wl  LogTextDisplay                     TN5904   mit ThreadID und DisplayType als Parameter
  29.05.12 wl  fLogDebugInfo                      TN5958   neu: entscheidet, ob Debug-Logs angezeigt werden oder nicht
  29.05.12 wl  Create                             TN5958   fLogDebugInfo wird (noch recht primitiv) durch AppPurpose definiert
  23.11.12 wl  Instance                           TN6015.1 Es gibt keine Instanz mehr von TDisplayComponentSettingsManager
  14.12.12 wl  Parser_InputModal                  TN6054   neuer Parameter: aDataCache:TMethodVariablesDataCache
  22.02.13 wl                                     TN6055   MethodVariablesDataCache entfernt
  11.03.13 wl  TBasicRunEditFunctions             TN6095   neu: für Aufruf von Parameter-Fenster
  27.03.13 wl                                     TN6095   uses geändert
  14.05.13 wl                                     TN6095   mit MethodVarPages
  28.08.13 wl  ChartDisplay_AddValue              TN6236   entfernt
  18.09.13 wl                                     TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit RunDialogsManager;


interface


uses
    Generics.Collections,
    ThreadClasses,
    ThreadUtils,
    RunMainControls,
    DisplayComponentManager,
    DisplayComponentIntf,
    RunDefaultDisplayLayout,
    ErrorInfo,
    ParserStoredIdentifier,
    Streamable,
    LogManager,
    AppTypes,
    BasicEditFunctions,
    ActionDataCache,
    RestartPreparationStep;

type
    TBasicRunEditFunctions = class(TBasicEditFunctions)
    protected
        function IsCurrentLayoutEmpty(): boolean; override;
        function GetCurrentLayoutName(): string; override;
    end;

    TRunDialogsManager = class
    private
        fDisplayComponentManager: TDisplayComponentManager;
        fCurrentDisplayComponent: IDisplayComponent;
        fDefaultDisplayComponent: TFrmRunDefaultDisplayLayout;
        fLogDebugInfo: boolean;
        class var uInstance: TRunDialogsManager;
        procedure LoadDefaultDisplayComponentToMain();
        procedure UnloadDisplayComponent();
        procedure SetMainPanelParentVisible(const aVisible: boolean);
        function InteractiveMessageAndWait(aFunc: TFunc; const aArgs: TMessageArg): TMessageResult;
        function NonInteractiveMessageAndWait(aFunc: TFunc; const aArgs: TMessageArg): TMessageResult;
        procedure NonInteractiveMessageAndLeave(aFunc: TFunc; const aArgs: TMessageArg);
        procedure InternGroupDisplay_InsertInfo(const aDisplayID: string; const aGroupNames: TArray<string>;
            const aKey, aText: string; aInfoGroupBehaviour: TInfoGroupBehaviour);
        procedure InternSQLTable_Refresh(const aDisplayID: string);
        procedure InternLoadDisplayComponentToMain(const aDisplayComponentName: string;
            const aContextID: string);
        procedure InternDisplayComponentsRunEnable(const aIsRunning: boolean);

        function VCL(const aArgs: TMessageArg): TMessageResult;
    public
        constructor Create();
        destructor Destroy(); override;
        class property Instance: TRunDialogsManager read uInstance;
        class procedure CreateInstance;
        class procedure DestroyInstance;

        procedure LogText_Display(const aLogText: string; aThreadID: integer;
            aDisplayType: TDisplayLogInfoType);

        function Barcode_PromptInputGUI(var vText: string; aCaption: string; aLabelCaption: string;
            aNoRackBtnCaption: string): integer;

        function MessageBox_Show(const aText, aCaption: string;
            aButtons, aIcon, aDefaultButton: integer): integer;
        function SelectItemBox_Show(const aItems: TArray<string>;
            const aText, aCaption, aButtonText: string): string;

        function ToolErr_PromptInput(const aMessage: string): integer;

        procedure SetRunMainState(const aState: TRunMainState);
        function GetRunMainState: TRunMainState;

        function ErrBox_PromptModal(const aErrorInfo: TErrorInfo): integer;

        // RunAbortInfo
        function RunAbortInfo_PromptModal(aActionStatus: TArray<string>): integer;
        // MethDone
        function RackPlaceList_PromptModal(const aRunName, aLayoutName, aRackTypes, aCaptionStr: string;
            aAbortAllowed, aMarkRacks: boolean): integer;

        procedure Delay_ChangeTime(const aID: string; aNewTime: string; const aProgress: integer);
        procedure Delay_InsertInfo(const aID: string; aTime, aDescription: string);
        procedure Delay_DeleteInfo(const aID: string);
        function Delay_IsCancelled(const aID: string): boolean;

        procedure GroupDisplay_InsertInfo(const aDisplayID: string; const aGroupNames: TArray<string>;
            const aKey, aText: string; aInfoGroupBehaviour: TInfoGroupBehaviour);
        procedure GroupDisplay_StartNewRun(const aMethodName: string);

        procedure SQLTable_Refresh(const aID: string);

        function AskRunStart_PromptModal(const aMethodName: string; const aIsSimChangeAllowed: boolean;
            var vIsSim: boolean; var vSimulationAskWeight: boolean;
            var vSimulationSpeed_Percent: integer): integer;

        function Trace_PromptModal(const aSourceDataName: string;
            const aActionsIterator: TActionListDataCacheIterator; const aRestartOnlyAtMarks: boolean;
            out oContinueAtID: int64; out oContinueAtSegmentIndex: integer;
            out oContinueAtDescription: string): integer; virtual;

        function AskRestart_PromptModal(const aSourceDataName, aDateLastStoppedStr: string;
            const aActionListDataCache: TActionListDataCache; const aRestartOnlyAtMarks: boolean;
            out oContinueAtID: int64; out oContinueAtSegmentIndex: integer): integer;

        function EditMethodParameters_PromptModal(const aSubMethodName: string;
            const aParameters: TStreamableKeyValueList; aAllowGlobalIdents, aShowEditor: boolean;
            const aIsBuildMode: boolean): boolean; virtual;

        function Parser_InputModal(const aScopeName: string;
            aIdentsNeedingInput: TObjectList<TParserStoredIdent>; const aEnterKeyNavigationOnly: boolean)
            : integer; virtual;
        procedure SQLResult_Display(const aSQLStr: string; const aFileName: string); virtual;

        procedure SystemVolume_Changed(const aSender: TObject); virtual;
        procedure WasteVolume_Changed(const aSender: TObject); virtual;

        procedure LoadDisplayComponentToMain(const aDisplayComponentName, aContextID: string);
        procedure DisplayComponents_RunEnable(const aEnable: boolean);
        procedure UnloadAnyLoadedDisplayComponents();

        procedure SimulationMode_Enable(const aEnabled: boolean);

        function RestartPreparations_PromptModal(const aMethodName: string;
            const aRestartPreparationList: TRestartPreparationStepList): integer;

        procedure LayoutName_Change(const aLayoutName: string);
        procedure MethodName_Change(const aMethodName: string);
        procedure DiTiMenu_Clear();
        procedure DiTiMenu_Load();
    end;


implementation


uses
    Forms,
    Variants,
    SysUtils,
    Controls,

    AppSettings,
    GUIManager,
    BarcDlg,
    RunInformation,
    RunAbortInfo,
    RckPlace,
    DelayInfo,
    ToolErr,
    RunStandardDisplayComponents,
    DisplayComponentSettingsManager,
    DisplayComponentTypeDictionary,
    AskRunStart,
    TraceEditor,
    ParserValueRequest,
    CommonTypes,
    EditMethodParams,
    SQLResult,
    ErrorMessage,
    VolumesInfo,
    LogInfo,
    LayoutManager,
    AskRestart,
    RestartPreparations;

type
    TVCLGUIManagerReal = (vclLogTextDisplay, vclBarcodePromptInput, vclRunAbortPromptModal,
        vclShowRackListInstance, vclDelayChangeTime, vclDelayInsertInfo, vclDelayDeleteInfo,
        vclDelayIsCancelled, vclGroupDisplayInsertInfo, vclGroupDisplayStartNewRun, vclDisplayComponentLoad,
        vclGetRunMainState, vclSetRunMainState, vclErrBoxModal, vclToolErrPromptInput, vclTracePromptModal,
        vclEditMethodParametersPromptModal, vclAskRunStartPromptModal, vclAskRestartPromptModal,
        vclRestartPreparationsPromptModal, vclParserInputModal, vclSQLResultDisplay, vclSystemVolumeChanged,
        vclWasteVolumeChanged, vclSimulationModeEnable, vclDisplayComponentsRunEnable, vclLayoutNameChange,
        vclMethodLabelChange, vclDiTiMenuClear, vclDiTiMenuLoad, vclSQLTableRefresh);

    { TBasicRunEditFunctions }

function TBasicRunEditFunctions.GetCurrentLayoutName: string;
begin
    EXIT(TLayoutManager.Instance.CurrentLayout.name);
end;

function TBasicRunEditFunctions.IsCurrentLayoutEmpty: boolean;
begin
    EXIT(TLayoutManager.Instance.IsCurrentLayoutEmpty);
end;

{ TRunDialogsManager }

class procedure TRunDialogsManager.CreateInstance();
begin
    uInstance := TRunDialogsManager.Create();
end;

class procedure TRunDialogsManager.DestroyInstance();
begin
    uInstance.Free;
end;

constructor TRunDialogsManager.Create();
begin
    inherited;
    fDisplayComponentManager := TDisplayComponentManager.Create();
    fLogDebugInfo := (TAppSettings.AppPurpose = TAppPurpose.appEditAndStart);
end;

destructor TRunDialogsManager.Destroy();
begin
    UnloadDisplayComponent();
    fDisplayComponentManager.Free;
    inherited;
end;

procedure TRunDialogsManager.InternLoadDisplayComponentToMain(const aDisplayComponentName: string;
    const aContextID: string);
var
    xErrors: TList<string>;
begin
    // clear the log because it it gets too long it causes the whole app to crash
    if TRunMainControls.Instance.LogInfoControl is TfrmLogInfo then
        (TRunMainControls.Instance.LogInfoControl as TfrmLogInfo).MainLogDisplay.Clear;

    UnloadDisplayComponent();

    SetMainPanelParentVisible(true);

    if aDisplayComponentName = '' then
    begin
        LoadDefaultDisplayComponentToMain();
    end
    else
    begin
        xErrors := TList<string>.Create();
        try
            fCurrentDisplayComponent := fDisplayComponentManager.LoadComponentToWindow(aDisplayComponentName,
                aContextID, false, TRunMainControls.Instance.MainPanel, xErrors);
        finally
            FreeAndNil(xErrors);
        end;
    end;
end;

procedure TRunDialogsManager.InternSQLTable_Refresh(const aDisplayID: string);
var
    xSQLTableDisplayComponent: ISQLTableDisplayComponent;
    xSearchIndex: integer;

begin
    xSearchIndex := 0;
    while true do
    begin
        if not fDisplayComponentManager.FindDisplayComponentByDisplayIDExt(ISQLTableDisplayComponent,
            aDisplayID, xSearchIndex, xSQLTableDisplayComponent) then
            EXIT;
        xSQLTableDisplayComponent.DoRefresh;
    end;
end;

procedure TRunDialogsManager.InternDisplayComponentsRunEnable(const aIsRunning: boolean);
var
    xButtonDisplayComponent: IButtonDisplayComponent;
    xSearchIndex: integer;
begin
    TRunMainControls.Instance.EnableControlsDuringRun(not aIsRunning);

    xSearchIndex := 0;
    if Assigned(fCurrentDisplayComponent) then
    begin

        while true do
        begin
            if not fDisplayComponentManager.FindDisplayComponentExt(IButtonDisplayComponent, xSearchIndex,
                xButtonDisplayComponent) then
                BREAK;
            xButtonDisplayComponent.Enabled := aIsRunning;
        end;

    end;
end;

procedure TRunDialogsManager.InternGroupDisplay_InsertInfo(const aDisplayID: string;
    const aGroupNames: TArray<string>; const aKey, aText: string; aInfoGroupBehaviour: TInfoGroupBehaviour);
var
    xRunInfoDisplayComponent: IRunInfoPanelDisplayComponent;
    xSearchIndex: integer;

begin
    xSearchIndex := 0;
    while true do
    begin
        if aDisplayID = '' then
        begin
            TfrmRunInformation.Instance.InsertInfo(aGroupNames, aKey, aText, aInfoGroupBehaviour);
            EXIT;
        end
        else
        begin
            if not fDisplayComponentManager.FindDisplayComponentByDisplayIDExt(IRunInfoPanelDisplayComponent,
                aDisplayID, xSearchIndex, xRunInfoDisplayComponent) then
                EXIT;
            xRunInfoDisplayComponent.InsertInfo(aGroupNames, aKey, aText, aInfoGroupBehaviour);
        end;
    end;
end;

function TRunDialogsManager.VCL(const aArgs: TMessageArg): TMessageResult;
var
    xType: TVCLGUIManagerReal;
    xStr1: string;
    xInt1: integer;
    xInt641: int64;
    xInt2: integer;
    xBool1, xBool2: boolean;
    xEditFunctions: TBasicRunEditFunctions;
begin
    xType := aArgs[0];

    case xType of
        vclToolErrPromptInput:
            result := TToolErrForm.ShowDialog(string(aArgs[1]));
        vclGetRunMainState:
            result := TRunMainControls.Instance.RunMainState;
        vclSetRunMainState:
            TRunMainControls.Instance.RunMainState := aArgs[1];
        vclErrBoxModal:
            result := TfrmErrorMessage.InstPromptModal(TErrorInfo(LongInt(aArgs[1])));
        vclBarcodePromptInput:
            begin
                xInt1 := TBarcodeDLG.InstPromptInput(xStr1, string(aArgs[1]), string(aArgs[2]),
                    string(aArgs[3]));
                result := VarArrayOf([xInt1, xStr1]);
            end;
        vclRunAbortPromptModal:
            begin
                result := TfrmRunAbortInfo.PromptModal(aArgs[1]);
            end;
        vclShowRackListInstance:
            result := TRackPlaceList.InstPromptModal(aArgs[1], aArgs[2], aArgs[3], aArgs[4], aArgs[5],
                aArgs[6]);
        vclDelayInsertInfo:
            TFraDelayInfo.Instance.DelayInsertInfo(aArgs[1], aArgs[2], aArgs[3]);
        vclDelayDeleteInfo:
            TFraDelayInfo.Instance.DelayDeleteInfo(aArgs[1]);
        vclDelayChangeTime:
            TFraDelayInfo.Instance.DelayChangeTime(aArgs[1], aArgs[2], aArgs[3]);
        vclDelayIsCancelled:
            result := TFraDelayInfo.Instance.DelayIsCancelled(aArgs[1]);
        vclDisplayComponentLoad:
            InternLoadDisplayComponentToMain(aArgs[1], aArgs[2]);
        vclDisplayComponentsRunEnable:
            InternDisplayComponentsRunEnable(aArgs[1]);

        vclGroupDisplayInsertInfo:
            InternGroupDisplay_InsertInfo(aArgs[1], TArray<string>(LongInt(aArgs[2])), aArgs[3], aArgs[4],
                aArgs[5]);
        vclGroupDisplayStartNewRun:
            TfrmRunInformation.Instance.StartNewRun(aArgs[1]);

        vclTracePromptModal:
            begin
                xInt1 := TFrmTraceEditor.InstanceDisplayTrace(aArgs[1],
                    TActionListDataCacheIterator(LongInt(aArgs[2])), aArgs[3], xInt641, xInt2, xStr1);
                result := VarArrayOf([xInt1, xInt641, xInt2, xStr1]);
            end;
        vclAskRestartPromptModal:
            begin
                xInt1 := TFrmAskRestart.InstancePromptModal(aArgs[1], aArgs[2],
                    TActionListDataCache(LongInt(aArgs[3])), aArgs[4], xInt641, xInt2);
                result := VarArrayOf([xInt1, xInt641, xInt2]);
            end;
        vclAskRunStartPromptModal:
            begin
                xBool1 := aArgs[3];
                xBool2 := aArgs[4];
                xInt2 := aArgs[5];
                xInt1 := TFrmAskRunStart.InstanceShowModal(aArgs[1], aArgs[2], xBool1, xBool2, xInt2);
                result := VarArrayOf([xInt1, xBool1, xBool2, xInt2]);
            end;
        vclEditMethodParametersPromptModal:
            begin
                xEditFunctions := TBasicRunEditFunctions.Create;
                try
                    result := TEditMethodParams.EditMethodParameters(aArgs[1],
                        TStreamableKeyValueList(LongInt(aArgs[2])), aArgs[3], aArgs[4], aArgs[5],
                        xEditFunctions);
                finally
                    FreeAndNil(xEditFunctions);
                end;
            end;
        vclParserInputModal:
            begin
                xEditFunctions := TBasicRunEditFunctions.Create;
                try
                    result := TParserValueRequest.ShowModal(aArgs[1],
                        TObjectList<TParserStoredIdent>(LongInt(aArgs[2])), nil, true, false, aArgs[3],
                        xEditFunctions);
                finally
                    FreeAndNil(xEditFunctions);
                end;
            end;
        vclSQLResultDisplay:
            TSQLResultForm.InstanceDisplay(aArgs[1], aArgs[2]);
        vclLogTextDisplay:
            TRunMainControls.Instance.MainLogDisplay.Lines.Add(Copy(aArgs[1], 1, 250));
        vclSystemVolumeChanged:
            (TRunMainControls.Instance.VolumesInfoControl as TFrmVolumesInfo)
                .ChangeSysVol(TObject(LongInt(aArgs[1])));
        vclWasteVolumeChanged:
            (TRunMainControls.Instance.VolumesInfoControl as TFrmVolumesInfo)
                .ChangeWasteVol(TObject(LongInt(aArgs[1])));
        vclSimulationModeEnable:
            TRunMainControls.Instance.SetSimModeGUI(aArgs[1]);
        vclLayoutNameChange:
            TRunMainControls.Instance.SetLayoutName(aArgs[1]);
        vclMethodLabelChange:
            TRunMainControls.Instance.ChangeMethodLabel(aArgs[1]);
        vclDiTiMenuClear:
            TRunMainControls.Instance.ClearDiTiMenu();
        vclDiTiMenuLoad:
            TRunMainControls.Instance.AddDiTiMenu();
        vclRestartPreparationsPromptModal:
            result := TFrmRestartPreparations.InstanceShowModal(aArgs[1],
                TRestartPreparationStepList(LongInt(aArgs[2])));
        vclSQLTableRefresh:
            InternSQLTable_Refresh(aArgs[1]);
    end;
end;

function TRunDialogsManager.InteractiveMessageAndWait(aFunc: TFunc; const aArgs: TMessageArg): TMessageResult;
begin
    result := gGUIManager.InteractiveMessageAndWait(aFunc, aArgs);
end;

function TRunDialogsManager.NonInteractiveMessageAndWait(aFunc: TFunc; const aArgs: TMessageArg)
    : TMessageResult;
begin
    result := gGUIManager.NonInteractiveMessageAndWait(aFunc, aArgs);
end;

procedure TRunDialogsManager.NonInteractiveMessageAndLeave(aFunc: TFunc; const aArgs: TMessageArg);
begin
    gmMessageAndGo(aFunc, aArgs);
end;

function TRunDialogsManager.Trace_PromptModal(const aSourceDataName: string;
    const aActionsIterator: TActionListDataCacheIterator; const aRestartOnlyAtMarks: boolean;
    out oContinueAtID: int64; out oContinueAtSegmentIndex: integer;
    out oContinueAtDescription: string): integer;
var
    xResult: variant;
begin
    xResult := InteractiveMessageAndWait(self.VCL, VarArrayOf([vclTracePromptModal, aSourceDataName,
        LongInt(aActionsIterator), aRestartOnlyAtMarks]));
    result := xResult[0];
    oContinueAtID := xResult[1];
    oContinueAtSegmentIndex := xResult[2];
    oContinueAtDescription := xResult[3];
end;

function TRunDialogsManager.AskRestart_PromptModal(const aSourceDataName, aDateLastStoppedStr: string;
    const aActionListDataCache: TActionListDataCache; const aRestartOnlyAtMarks: boolean;
    out oContinueAtID: int64; out oContinueAtSegmentIndex: integer): integer;
var
    xResult: variant;
begin
    xResult := InteractiveMessageAndWait(self.VCL, VarArrayOf([vclAskRestartPromptModal, aSourceDataName,
        aDateLastStoppedStr, LongInt(aActionListDataCache), aRestartOnlyAtMarks]));
    result := xResult[0];
    oContinueAtID := xResult[1];
    oContinueAtSegmentIndex := xResult[2];
end;

function TRunDialogsManager.EditMethodParameters_PromptModal(const aSubMethodName: string;
    const aParameters: TStreamableKeyValueList; aAllowGlobalIdents, aShowEditor: boolean;
    const aIsBuildMode: boolean): boolean;
begin
    result := InteractiveMessageAndWait(self.VCL,
        VarArrayOf([vclEditMethodParametersPromptModal, aSubMethodName, LongInt(aParameters),
        aAllowGlobalIdents, aShowEditor, aIsBuildMode]));
end;

function TRunDialogsManager.Parser_InputModal(const aScopeName: string;
    aIdentsNeedingInput: TObjectList<TParserStoredIdent>; const aEnterKeyNavigationOnly: boolean): integer;
begin
    result := InteractiveMessageAndWait(self.VCL,
        VarArrayOf([vclParserInputModal, aScopeName, LongInt(aIdentsNeedingInput), aEnterKeyNavigationOnly]));
end;

procedure TRunDialogsManager.SQLResult_Display(const aSQLStr, aFileName: string);
begin
    self.NonInteractiveMessageAndLeave(self.VCL, VarArrayOf([vclSQLResultDisplay, aSQLStr, aFileName]));
end;

procedure TRunDialogsManager.SQLTable_Refresh(const aID: string);
begin
    NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclSQLTableRefresh, aID]));
end;

function TRunDialogsManager.ErrBox_PromptModal(const aErrorInfo: TErrorInfo): integer;
begin
    result := InteractiveMessageAndWait(self.VCL, VarArrayOf([vclErrBoxModal, LongInt(aErrorInfo)]));
end;

function TRunDialogsManager.ToolErr_PromptInput(const aMessage: string): integer;
begin
    result := InteractiveMessageAndWait(self.VCL, VarArrayOf([vclToolErrPromptInput, aMessage]));
end;

function TRunDialogsManager.Barcode_PromptInputGUI(var vText: string; aCaption: string; aLabelCaption: string;
    aNoRackBtnCaption: string): integer;
var
    xResult: variant;
begin
    xResult := InteractiveMessageAndWait(self.VCL, VarArrayOf([vclBarcodePromptInput, aCaption, aLabelCaption,
        aNoRackBtnCaption]));
    result := xResult[0];
    vText := xResult[1];
end;

function TRunDialogsManager.RunAbortInfo_PromptModal(aActionStatus: TArray<string>): integer;
begin
    result := InteractiveMessageAndWait(self.VCL, VarArrayOf([vclRunAbortPromptModal, aActionStatus]));
end;

procedure TRunDialogsManager.SetRunMainState(const aState: TRunMainState);
begin
    NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclSetRunMainState, aState]));
end;

function TRunDialogsManager.GetRunMainState: TRunMainState;
begin
    result := NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclGetRunMainState]));
end;

function TRunDialogsManager.RackPlaceList_PromptModal(const aRunName, aLayoutName, aRackTypes,
    aCaptionStr: string; aAbortAllowed, aMarkRacks: boolean): integer;
begin
    result := InteractiveMessageAndWait(self.VCL, VarArrayOf([vclShowRackListInstance, aRunName, aLayoutName,
        aRackTypes, aCaptionStr, aAbortAllowed, aMarkRacks]));
end;

procedure TRunDialogsManager.Delay_ChangeTime(const aID: string; aNewTime: string; const aProgress: integer);
begin
    NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclDelayChangeTime, aID, aNewTime, aProgress]));
end;

procedure TRunDialogsManager.Delay_DeleteInfo(const aID: string);
begin
    NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclDelayDeleteInfo, aID]));
end;

procedure TRunDialogsManager.Delay_InsertInfo(const aID: string; aTime, aDescription: string);
begin
    NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclDelayInsertInfo, aID, aTime, aDescription]));
end;

function TRunDialogsManager.Delay_IsCancelled(const aID: string): boolean;
begin
    result := NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclDelayIsCancelled, aID]));
end;

procedure TRunDialogsManager.GroupDisplay_InsertInfo(const aDisplayID: string;
    const aGroupNames: TArray<string>; const aKey, aText: string; aInfoGroupBehaviour: TInfoGroupBehaviour);
begin
    NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclGroupDisplayInsertInfo, aDisplayID,
        LongInt(aGroupNames), aKey, aText, aInfoGroupBehaviour]));
end;

procedure TRunDialogsManager.GroupDisplay_StartNewRun(const aMethodName: string);
begin
    NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclGroupDisplayStartNewRun, aMethodName]));
end;

function TRunDialogsManager.AskRunStart_PromptModal(const aMethodName: string;
    const aIsSimChangeAllowed: boolean; var vIsSim: boolean; var vSimulationAskWeight: boolean;
    var vSimulationSpeed_Percent: integer): integer;
var
    xResult: variant;
begin
    xResult := InteractiveMessageAndWait(self.VCL, VarArrayOf([vclAskRunStartPromptModal, aMethodName,
        aIsSimChangeAllowed, vIsSim, vSimulationAskWeight, vSimulationSpeed_Percent]));
    result := xResult[0];
    vIsSim := xResult[1];
    vSimulationAskWeight := xResult[2];
    vSimulationSpeed_Percent := xResult[3];
end;

procedure TRunDialogsManager.LogText_Display(const aLogText: string; aThreadID: integer;
    aDisplayType: TDisplayLogInfoType);
begin
    if (aDisplayType = TDisplayLogInfoType.DebugOnly) and not fLogDebugInfo then
        EXIT;

    NonInteractiveMessageAndWait(self.VCL,
        VarArrayOf([vclLogTextDisplay, IntToStr(aThreadID) + '|' + aLogText]));
end;

procedure TRunDialogsManager.SystemVolume_Changed(const aSender: TObject);
begin
    NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclSystemVolumeChanged, LongInt(aSender)]));
end;

procedure TRunDialogsManager.WasteVolume_Changed(const aSender: TObject);
begin
    NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclWasteVolumeChanged, LongInt(aSender)]));
end;

procedure TRunDialogsManager.SetMainPanelParentVisible(const aVisible: boolean);
begin
    if (TRunMainControls.Instance = nil) or (not Assigned(TRunMainControls.Instance.MainPanel)) then
        EXIT;
    if Assigned(TRunMainControls.Instance.MainPanel.Parent) then
    begin

        TRunMainControls.Instance.MainPanel.Parent.Visible := aVisible;
    end;
end;

procedure TRunDialogsManager.SimulationMode_Enable(const aEnabled: boolean);
begin
    NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclSimulationModeEnable, aEnabled]));
end;

function TRunDialogsManager.RestartPreparations_PromptModal(const aMethodName: string;
    const aRestartPreparationList: TRestartPreparationStepList): integer;
begin
    result := InteractiveMessageAndWait(self.VCL, VarArrayOf([vclRestartPreparationsPromptModal, aMethodName,
        LongInt(aRestartPreparationList)]));
end;

procedure TRunDialogsManager.UnloadDisplayComponent();
begin
    if Assigned(fDefaultDisplayComponent) then
    begin
        fDefaultDisplayComponent.Visible := false;
        fDefaultDisplayComponent.Unload();
        fDefaultDisplayComponent.Parent := nil;
        FreeAndNil(fDefaultDisplayComponent);
    end
    else if Assigned(fCurrentDisplayComponent) then
    begin
        fCurrentDisplayComponent.Unload();
        fCurrentDisplayComponent := nil;
        fDisplayComponentManager.ClearDisplayComponents();
        Application.ProcessMessages();
    end;
    SetMainPanelParentVisible(false);
end;

procedure TRunDialogsManager.LoadDefaultDisplayComponentToMain();
begin
    fDefaultDisplayComponent := TFrmRunDefaultDisplayLayout.Create(nil);
    fDefaultDisplayComponent.Parent := TRunMainControls.Instance.MainPanel;
    fDefaultDisplayComponent.Load(TRunMainControls.Instance.LayoutInfoControl,
        TRunMainControls.Instance.DelayInfoControl, TRunMainControls.Instance.RunInfoControl,
        TRunMainControls.Instance.LogInfoControl, TRunMainControls.Instance.VolumesInfoControl,
        TRunMainControls.Instance.IsVolumesInfoNeeded);

    fDefaultDisplayComponent.Visible := true;
end;

procedure TRunDialogsManager.LoadDisplayComponentToMain(const aDisplayComponentName: string;
    const aContextID: string);
begin
    NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclDisplayComponentLoad, aDisplayComponentName,
        aContextID]));
    // NonInteractiveMessageAndWait( self.VCL, VarArrayOf( [ vclShowRunInformation, true ] ) );
end;

procedure TRunDialogsManager.DisplayComponents_RunEnable(const aEnable: boolean);
begin
    NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclDisplayComponentsRunEnable, aEnable]));
end;

procedure TRunDialogsManager.LayoutName_Change(const aLayoutName: string);
begin
    NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclLayoutNameChange, aLayoutName]));
end;

procedure TRunDialogsManager.MethodName_Change(const aMethodName: string);
begin
    NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclMethodLabelChange, aMethodName]));
end;

procedure TRunDialogsManager.DiTiMenu_Clear;
begin
    NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclDiTiMenuClear]));
end;

procedure TRunDialogsManager.DiTiMenu_Load;
begin
    NonInteractiveMessageAndWait(self.VCL, VarArrayOf([vclDiTiMenuLoad]));
end;

procedure TRunDialogsManager.UnloadAnyLoadedDisplayComponents();
begin
    UnloadDisplayComponent();
end;

function TRunDialogsManager.MessageBox_Show(const aText, aCaption: string;
    aButtons, aIcon, aDefaultButton: integer): integer;
begin
    result := gGUIManager.MessageBox(aText, aCaption, aButtons, aIcon, aDefaultButton);
end;

function TRunDialogsManager.SelectItemBox_Show(const aItems: TArray<string>;
    const aText, aCaption, aButtonText: string): string;
begin
    if aButtonText = '' then
        result := gGUIManager.SelectItemBox(aItems, aText, aCaption)
    else
        result := gGUIManager.SelectItemBox(aItems, aText, aCaption, aButtonText);
end;


end.
