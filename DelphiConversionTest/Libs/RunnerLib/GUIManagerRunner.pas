{ --------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Handle calls to Delphi Graphics ( VCL ) functions
  Used by Runner
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  -------------------------------   -------- ------------------------------------------
  08.06.09 pk                                     TN4585.1  Initial Revision
  24.07.09 pk                                     TN4585.1  EditMethodParameters_PromptModal, Parser_InputModal, etc implemented
  31.07.09 ts  GroupDisplay_InsertInfo            TN4666    new: InfoGroupBehaviour instead of HideGroup
  04.08.09 ts  ToolErr_PromptInput                TN4698    new: override function for Tool-Error-Box
  12.08.09 wl  ToolErr_PromptInput                TN4712    ein Parameter entfernt
  27.08.09 pk                                     TN4753    uses changed
  08.09.09 pk                                     TN4753    uses ErrorMessage replaced by ErrorInfo
  28.09.09 pk  LogText_Display                    TN4753    new
  04.11.09 pk                               	 TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  04.02.10 pk                               	 TN4972    Changes for Restart
  01.04.10 ts  ChartDisplay_AddValue              TN4785    new
  01.04.10 pk  Parser_InputModal                  TN5003.2  New EnterKeyNavigationOnly parameter
  04.09.10 pk  DisplayComponents_RunEnable        TN5042   New: used for enabling/disabling stop button on displaycomponents
  04.09.10 pk                                     TN5042   New: SimulationMode_Enable, etc. functions from TRunMain
  23.04.10 pk  EditMethodParameters_PromptModal   TN5062   New Parameter aIsBuildMode
  07.05.10 pk  EditMethodParameters_PromptModal   TN5092   aParams is now TArgValueList instead of array
  19.05.10 pk  AskRunStart_PromptModal            TN5114   New
  07.06.10 pk  RestartPreparations_PromptModal    TN5077   New
  20.07.10 pk                                     TN5201   ChangeTime function now with Progress parameter
  26.10.10 pk  AskRestart_PromptModal             TN5297   Changes for ActionData segment concept
  21.03.11 wl  AskRunStart_PromptModal            TN5508   neue Parameter
  25.05.11 ts  SQLTable_Refresh                   TN5590   new: Refresh for SQLTableDisplayComponent
  17.11.11 wl                                     TN5725   PositionInfo-Fenster entfernt
  14.12.11 wl                                     TN5765   MethDone-Dialog durch MessageBox ersetzt
  02.03.12 wl                                     TN5822   uses geändert
  20.04.12 wl  Stop_PromptModal                   TN5858   entfernt
  20.04.12 wl  GetRunMainState,SetRunMainState    TN5858   neu für RunMain-Anzeige
  09.05.12 wl  RequestAndStateSetAbort            TN5858   von ThrManExt hierher
  09.05.12 wl  SetUserLogRun                      TN5858   neu
  29.05.12 wl  LogText_Display                    TN5904    mit ThreadID und DisplayType als Parameter
  29.05.12 wl  RequestAndStateSetAbort            TN5894    Zustand kann Running oder Pause sein!
  14.12.12 wl  Parser_InputModal                  TN6054   neuer Parameter: aDataCache:TMethodVariablesDataCache
  22.02.13 wl                                     TN6055   MethodVariablesDataCache entfernt
  28.08.13 wl  ChartDisplay_AddValue              TN6236   entfernt
  -------------------------------------------------------------------------------------------------- }

unit GUIManagerRunner;


interface


uses
    Generics.Collections,
    GeneralTypes,
    MethodTypes,
    GUIManagerRun,
    ThreadUtils,
    LogManager,
    ParserStoredIdentifier,
    AppTypes,
    ActionDataCache,
    RestartPreparationStep;

type
    TVCLGUIManagerRunner = (vclStopPromptModal);

    TVCLHandlingRunner = class
    private
        class function VCL(const aArgs: TMessageArg): TMessageResult;
    end;

    TGUIManagerRunner = class(TGUIManagerRun)
    protected
        function Barcode_PromptInputGUI(var vText: string; aCaption: string; aLabelCaption: string;
            aNoRackBtnCaption: string): integer; override;
    public
        class function Instance(): TGUIManagerRunner;

        procedure LogText_Display(const aLogText: string; aThreadID: integer;
            aDisplayType: TDisplayLogInfoType); override;

        function ErrBox_PromptModal(aPErrorInfo: integer): integer; override;

        // RunAbortInfo
        function RunAbortInfo_PromptModal(const aActionStatus: TStringArray): integer; override;
        // MethDone
        function RackPlaceList_PromptModal(const aRunName, aLayoutName, aRackTypes, aCaptionStr: string;
            aAbortAllowed, aMarkRacks: boolean): integer; override;

        procedure Delay_ChangeTime(const aID: string; aNewTime: string; const aProgress: integer); override;
        procedure Delay_InsertInfo(const aID: string; aTime, aDescription: string); override;
        procedure Delay_DeleteInfo(const aID: string); override;
        function Delay_IsCancelled(const aID: string): boolean; override;

        procedure GroupDisplay_InsertInfo(const aDisplayID: string; const aGroupNames: TStringArray;
            const aKey, aText: string; aInfoGroupBehaviour: TInfoGroupBehaviour); override;
        procedure GroupDisplay_StartNewRun(const aMethodName: string); override;

        procedure SQLTable_Refresh(const aDisplayID: string); override;

        function AskRunStart_PromptModal(const aMethodName: string; const aIsSimChangeAllowed: boolean;
            var vIsSim: boolean; var vSimulationAskWeight: boolean; var vSimulationSpeed_Percent: integer)
            : integer; override;
        procedure DisplayComponents_RunEnable(const aEnable: boolean); override;
        procedure LoadDisplayComponentToMain(const aDisplayComponentName, aContextID: string); override;

        function AskRestart_PromptModal(const aSourceDataName: string; const aDateLastStoppedStr: string;
            const aActionListDataCache: TActionListDataCache; const aRestartOnlyAtMarks: boolean;
            out oContinueAtID: int64; out oContinueAtSegmentIndex: integer): integer; override;

        function RestartPreparations_PromptModal(const aMethodName: string;
            const aRestartPreparationList: TRestartPreparationStepList): integer; override;

        function EditMethodParameters_PromptModal(const aSubMethodName: string;
            const aParameters: TKeyArgValueList; aAllowGlobalIdents, aShowEditor: boolean): boolean; override;

        function Parser_InputModal(const aScopeName: string;
            aIdentsNeedingInput: TObjectList<TParserStoredIdent>; const aEnterKeyNavigationOnly: boolean)
            : integer; override;
        procedure SQLResult_Display(const aSQLStr: string; const aFileName: string); override;

        procedure SystemVolume_Changed(aSender: TObject); override;
        procedure WasteVolume_Changed(aSender: TObject); override;

        procedure UnloadAnyLoadedDisplayComponents();

        function ToolErr_PromptInput(aMessage: string): integer; override;

        procedure SimulationMode_Enable(const aEnabled: boolean); override;
        procedure SetRunMainState(const aState: TRunMainState); override;
        function GetRunMainState: TRunMainState; override;
        procedure RequestAndStateSetAbort; override;
        procedure SetUserLogRun(aValue: boolean); override;
        function GetStepByStepMode: boolean; override;
        procedure StateSetPause; override;

        procedure LayoutName_Change(const aLayoutName: string); override;
        procedure SourceLabel_Change(const aSourceKind: string; const aSourceName: string); override;
        procedure DiTiMenu_Clear(); override;
        procedure DiTiMenu_Load(); override;
    end;


implementation


uses
    Controls,
    Variants,
    SysUtils,
    ErrorInfo,
    GUIManager,
    RunMainControls,
    RequestAbort,
    RunDialogsManager;

{ TVCLHandlingRunner }

class function TVCLHandlingRunner.VCL(const aArgs: TMessageArg): TMessageResult;
var
    xType: TVCLGUIManagerRunner;
begin
    xType := aArgs[0];
    case xType of
        vclStopPromptModal:
            result := TfrmRequestAbort.ShowStopDialog;
    end;
end;

{ TGUIManagerRunner }

function TGUIManagerRunner.ErrBox_PromptModal(aPErrorInfo: integer): integer;
begin
    result := TRunDialogsManager.Instance.ErrBox_PromptModal(TErrorInfo(aPErrorInfo));
end;

function TGUIManagerRunner.Barcode_PromptInputGUI(var vText: string; aCaption: string; aLabelCaption: string;
    aNoRackBtnCaption: string): integer;
begin
    result := TRunDialogsManager.Instance.Barcode_PromptInputGUI(vText, aCaption, aLabelCaption,
        aNoRackBtnCaption);
end;

function TGUIManagerRunner.RunAbortInfo_PromptModal(const aActionStatus: TStringArray): integer;
begin
    result := TRunDialogsManager.Instance.RunAbortInfo_PromptModal(aActionStatus);
end;

procedure TGUIManagerRunner.SetRunMainState(const aState: TRunMainState);
begin
    TRunDialogsManager.Instance.SetRunMainState(aState);
end;

procedure TGUIManagerRunner.SetUserLogRun(aValue: boolean);
begin
    TRunMainControls.Instance.UserLogRun := aValue;
end;

function TGUIManagerRunner.RackPlaceList_PromptModal(const aRunName, aLayoutName, aRackTypes,
    aCaptionStr: string; aAbortAllowed, aMarkRacks: boolean): integer;
begin
    result := TRunDialogsManager.Instance.RackPlaceList_PromptModal(aRunName, aLayoutName, aRackTypes,
        aCaptionStr, aAbortAllowed, aMarkRacks);
end;

function TGUIManagerRunner.RestartPreparations_PromptModal(const aMethodName: string;
    const aRestartPreparationList: TRestartPreparationStepList): integer;
begin
    result := TRunDialogsManager.Instance.RestartPreparations_PromptModal(aMethodName,
        aRestartPreparationList);
end;

procedure TGUIManagerRunner.Delay_ChangeTime(const aID: string; aNewTime: string; const aProgress: integer);
begin
    TRunDialogsManager.Instance.Delay_ChangeTime(aID, aNewTime, aProgress);
end;

procedure TGUIManagerRunner.Delay_DeleteInfo(const aID: string);
begin
    TRunDialogsManager.Instance.Delay_DeleteInfo(aID);
end;

procedure TGUIManagerRunner.Delay_InsertInfo(const aID: string; aTime, aDescription: string);
begin
    TRunDialogsManager.Instance.Delay_InsertInfo(aID, aTime, aDescription);
end;

function TGUIManagerRunner.Delay_IsCancelled(const aID: string): boolean;
begin
    result := TRunDialogsManager.Instance.Delay_IsCancelled(aID);
end;

procedure TGUIManagerRunner.StateSetPause;
begin
    TRunMainControls.Instance.StateSetPause(false);
end;

procedure TGUIManagerRunner.RequestAndStateSetAbort;
var
    xDialogResult: integer;
begin
    if (TGUIManagerRun.Instance.GetRunMainState = rmsReady) or
        (TGUIManagerRun.Instance.GetRunMainState = rmsRequestState) then
        EXIT;

    // auf jeden Fall Pause/RequestState-Zustand herstellen
    TRunMainControls.Instance.StateSetPause(true);

    // Fragen, welcher Zustand genau hergestellt werden soll
    xDialogResult := InteractiveMessageAndWait(TVCLHandlingRunner.VCL, VarArrayOf([vclStopPromptModal]));
    case (xDialogResult) of
        mrYes:
            TRunMainControls.Instance.StateSetAbort;
        mrNo:
            TRunMainControls.Instance.StateSetRunning;
        else
            TRunMainControls.Instance.StateSetPause(false);
    end;
end;

function TGUIManagerRunner.GetRunMainState: TRunMainState;
begin
    EXIT(TRunDialogsManager.Instance.GetRunMainState)
end;

function TGUIManagerRunner.GetStepByStepMode: boolean;
begin
    EXIT(TRunMainControls.Instance.RequestStepByStepMode)
end;

procedure TGUIManagerRunner.GroupDisplay_InsertInfo(const aDisplayID: string; const aGroupNames: TStringArray;
    const aKey, aText: string; aInfoGroupBehaviour: TInfoGroupBehaviour);
begin
    TRunDialogsManager.Instance.GroupDisplay_InsertInfo(aDisplayID, aGroupNames, aKey, aText,
        aInfoGroupBehaviour);
end;

procedure TGUIManagerRunner.GroupDisplay_StartNewRun(const aMethodName: string);
begin
    TRunDialogsManager.Instance.GroupDisplay_StartNewRun(aMethodName);
end;

function TGUIManagerRunner.AskRunStart_PromptModal(const aMethodName: string;
    const aIsSimChangeAllowed: boolean; var vIsSim: boolean; var vSimulationAskWeight: boolean;
    var vSimulationSpeed_Percent: integer): integer;
begin
    result := TRunDialogsManager.Instance.AskRunStart_PromptModal(aMethodName, aIsSimChangeAllowed, vIsSim,
        vSimulationAskWeight, vSimulationSpeed_Percent);
end;

procedure TGUIManagerRunner.DisplayComponents_RunEnable(const aEnable: boolean);
begin
    TRunDialogsManager.Instance.DisplayComponents_RunEnable(aEnable);
end;

procedure TGUIManagerRunner.LayoutName_Change(const aLayoutName: string);
begin
    TRunDialogsManager.Instance.LayoutName_Change(aLayoutName);
end;

procedure TGUIManagerRunner.LoadDisplayComponentToMain(const aDisplayComponentName: string;
    const aContextID: string);
begin
    TRunDialogsManager.Instance.LoadDisplayComponentToMain(aDisplayComponentName, aContextID);
end;

procedure TGUIManagerRunner.LogText_Display(const aLogText: string; aThreadID: integer;
    aDisplayType: TDisplayLogInfoType);
begin
    TRunDialogsManager.Instance.LogText_Display(aLogText, aThreadID, aDisplayType);
end;

procedure TGUIManagerRunner.SystemVolume_Changed(aSender: TObject);
begin
    TRunDialogsManager.Instance.SystemVolume_Changed(aSender);
end;

procedure TGUIManagerRunner.WasteVolume_Changed(aSender: TObject);
begin
    TRunDialogsManager.Instance.WasteVolume_Changed(aSender);
end;

procedure TGUIManagerRunner.UnloadAnyLoadedDisplayComponents();
begin
    TRunDialogsManager.Instance.UnloadAnyLoadedDisplayComponents();
end;

class function TGUIManagerRunner.Instance: TGUIManagerRunner;
begin
    result := gGUIManager as TGUIManagerRunner;
end;

function TGUIManagerRunner.AskRestart_PromptModal(const aSourceDataName, aDateLastStoppedStr: string;
    const aActionListDataCache: TActionListDataCache; const aRestartOnlyAtMarks: boolean;
    out oContinueAtID: int64; out oContinueAtSegmentIndex: integer): integer;
begin
    result := TRunDialogsManager.Instance.AskRestart_PromptModal(aSourceDataName, aDateLastStoppedStr,
        aActionListDataCache, aRestartOnlyAtMarks, oContinueAtID, oContinueAtSegmentIndex);
end;

procedure TGUIManagerRunner.DiTiMenu_Clear;
begin
    TRunDialogsManager.Instance.DiTiMenu_Clear();
end;

procedure TGUIManagerRunner.DiTiMenu_Load;
begin
    TRunDialogsManager.Instance.DiTiMenu_Load();
end;

function TGUIManagerRunner.EditMethodParameters_PromptModal(const aSubMethodName: string;
    const aParameters: TKeyArgValueList; aAllowGlobalIdents, aShowEditor: boolean): boolean;
begin
    result := TRunDialogsManager.Instance.EditMethodParameters_PromptModal(aSubMethodName, aParameters,
        aAllowGlobalIdents, aShowEditor, true);
end;

function TGUIManagerRunner.Parser_InputModal(const aScopeName: string;
    aIdentsNeedingInput: TObjectList<TParserStoredIdent>; const aEnterKeyNavigationOnly: boolean): integer;
begin
    result := TRunDialogsManager.Instance.Parser_InputModal(aScopeName, aIdentsNeedingInput,
        aEnterKeyNavigationOnly);
end;

procedure TGUIManagerRunner.SimulationMode_Enable(const aEnabled: boolean);
begin
    TRunDialogsManager.Instance.SimulationMode_Enable(aEnabled);
end;

procedure TGUIManagerRunner.SourceLabel_Change(const aSourceKind, aSourceName: string);
begin
    TRunDialogsManager.Instance.MethodName_Change(aSourceName);
end;

procedure TGUIManagerRunner.SQLResult_Display(const aSQLStr, aFileName: string);
begin
    TRunDialogsManager.Instance.SQLResult_Display(aSQLStr, aFileName);
end;

procedure TGUIManagerRunner.SQLTable_Refresh(const aDisplayID: string);
begin
    TRunDialogsManager.Instance.SQLTable_Refresh(aDisplayID);
end;

function TGUIManagerRunner.ToolErr_PromptInput(aMessage: string): integer;
begin
    result := TRunDialogsManager.Instance.ToolErr_PromptInput(aMessage);
end;


end.
