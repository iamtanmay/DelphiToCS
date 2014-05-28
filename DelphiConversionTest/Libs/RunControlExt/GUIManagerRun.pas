{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Handle calls to Delphi Graphics ( VCL ) functions
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  -------------------------------   -------- ------------------------------------------
  09.06.09 pk                                    TN4585.1 Real functions moved to GUIManagerRunner
  06.07.09 pk  SQLResult_Display                 TN4585.4 New
  27.07.09 pk  GroupDisplay_InsertInfo           TN4675   New DisplayID Parameter
  31.07.09 ts  GroupDisplay_InsertInfo           TN4666   New InfoGroupBehavior instead of aHideGroup
  08.09.09 pk                                    TN4753   uses ErrorMessage replaced by ErrorInfo
  28.09.09 pk  SystemVolume_Changed              TN4753   New reference counting
  04.11.09 pk                               	TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  04.02.10 pk                               	TN4972   Changes for Restart
  18.02.10 pk  LogText_Display                   TN4985.1 New LogText_Display
  01.04.10 ts  ChartDisplay_AddValue             TN4785   New
  01.04.10 pk  Parser_InputModal                 TN5003.2 New EnterKeyNavigationOnly parameter
  04.09.10 pk  DisplayComponents_RunEnable       TN5042   New: used for enabling/disabling stop button on displaycomponents
  04.09.10 pk                                    TN5042   New: SimulationMode_Enable, etc. functions from TRunMain
  07.05.10 pk  EditMethodParameters_PromptModal  TN5092   aParams is now TArgValueList instead of array
  19.05.10 pk  AskRunStart_PromptModal           TN5114   New
  07.06.10 pk  RestartPreparations_PromptModal   TN5077   New
  20.07.10 pk                                    TN5201   ChangeTime function now with Progress parameter
  26.10.10 pk  AskRestart_PromptModal            TN5297   New
  02.02.11 wl  ChooseLayout                      TN5466   von RunMain hierher
  21.03.11 wl  AskRunStart_PromptModal           TN5508   neue Parameter
  25.05.11 ts  SQLTable_Refresh                  TN5590   new: Refresh for SQLTableDisplayComponent
  17.11.11 wl                                    TN5725   PositionInfo-Fenster entfernt
  14.12.11 wl                                    TN5765   MethDone-Dialog durch MessageBox ersetzt
  01.03.12 wl                                    TN5822   uses geändert
  20.04.12 wl  GetRunMainState,SetRunMainState    TN5858   neu für RunMain-Anzeige
  09.05.12 wl  RequestAndStateSetAbort            TN5858   neu
  09.05.12 wl  SetUserLogRun                      TN5858   neu
  29.05.12 wl  StateSetPause                      TN5894   neu
  14.12.12 wl  Parser_InputModal                  TN6054   neuer Parameter: aDataCache:TMethodVariablesDataCache
  22.02.13 wl                                     TN6055   MethodVariablesDataCache entfernt
  30.07.13 wl  InterruptSignalStart,-End         TN6160   rufen neue Methoden TSystemEvents.ModalMessageStart,-Finish auf
  15.08.13 wl                                    TN6223   uses geändert
  28.08.13 wl  ChartDisplay_AddValue             TN6236   entfernt
  22.11.13 ts  InterruptSignalStart,-End         TN6311   if TSystemEvents.Instance = nil, nothing happens
  -------------------------------------------------------------------------------------------------- }

unit GUIManagerRun;


interface


uses
    Generics.Collections,
    GeneralTypes,
    GUIManagerSetup,
    TrackingSemaphore,
    AppTypes,
    ParserStoredIdentifier,
    MethodTypes,
    ActionDataCache,
    RestartPreparationStep;

type
    TGUIManagerRun = class(TGUIManagerSetup)
    protected
        fSerialMessageCriticalSection: TTrackingSemaphore;
        procedure InterruptSignalStart(); override;
        procedure InterruptSignalEnd(); override;
        procedure AcquireSerialAccess; override;
        procedure ReleaseSerialAccess; override;
    public
        constructor Create();
        destructor Destroy(); override;

        function RunAbortInfo_PromptModal(const aActionStatus: TStringArray): integer; virtual;

        function RackPlaceList_PromptModal(const aRunName, aLayoutName, aRackTypes, aCaptionStr: string;
            aAbortAllowed, aMarkRacks: boolean): integer; virtual;

        procedure Delay_ChangeTime(const aID: string; aNewTime: string; const aProgress: integer); virtual;
        procedure Delay_InsertInfo(const aID: string; aTime, aDescription: string); virtual;
        procedure Delay_DeleteInfo(const aID: string); virtual;
        function Delay_IsCancelled(const aID: string): boolean; virtual;

        procedure GroupDisplay_InsertInfo(const aDisplayID: string; const aGroupNames: TStringArray;
            const aKey, aText: string; aInfoGroupBehaviour: TInfoGroupBehaviour); virtual;
        procedure GroupDisplay_StartNewRun(const aMethodName: string); virtual;

        procedure SQLTable_Refresh(const aDisplayID: string); virtual;

        function AskRunStart_PromptModal(const aMethodName: string; const aIsSimChangeAllowed: boolean;
            var vIsSim: boolean; var vSimulationAskWeight: boolean; var vSimulationSpeed_Percent: integer)
            : integer; virtual; abstract;

        procedure LoadDisplayComponentToMain(const aDisplayComponentName: string;
            const aContextID: string); virtual;
        procedure DisplayComponents_RunEnable(const aEnable: boolean); virtual;

        function AskRestart_PromptModal(const aSourceDataName: string; const aDateLastStoppedStr: string;
            const aActionListDataCache: TActionListDataCache; const aRestartOnlyAtMarks: boolean;
            out oContinueAtID: int64; out oContinueAtSegmentIndex: integer): integer; virtual;

        function RestartPreparations_PromptModal(const aMethodName: string;
            const aRestartPreparationList: TRestartPreparationStepList): integer; virtual;

        function EditMethodParameters_PromptModal(const aSubMethodName: string;
            const aParameters: TKeyArgValueList; aAllowGlobalIdents, aShowEditor: boolean): boolean; virtual;

        function Parser_InputModal(const aScopeName: string;
            aIdentsNeedingInput: TObjectList<TParserStoredIdent>; const aEnterKeyNavigationOnly: boolean)
            : integer; virtual;
        procedure SQLResult_Display(const aSQLStr: string; const aFileName: string); virtual;

        procedure SystemVolume_Changed(aSender: TObject); virtual;
        procedure WasteVolume_Changed(aSender: TObject); virtual;

        procedure SimulationMode_Enable(const aEnabled: boolean); virtual;
        procedure LayoutName_Change(const aLayoutName: string); virtual;
        procedure SourceLabel_Change(const aSourceKind: string; const aSourceName: string); virtual;
        procedure DiTiMenu_Clear(); virtual;
        procedure DiTiMenu_Load(); virtual;

        procedure SetRunMainState(const aState: TRunMainState); virtual;
        function GetRunMainState: TRunMainState; virtual;
        procedure StateSetPause; virtual;
        procedure RequestAndStateSetAbort; virtual;
        procedure SetUserLogRun(aValue: boolean); virtual;
        function GetStepByStepMode: boolean; virtual;

        function ChooseLayout(const aCaption: string): string;

        class function Instance(): TGUIManagerRun;
    end;


implementation


uses
    Windows,
    GUIManager,
    SystemEvents,
    ThreadAPI,
    LayoutDataAdaptor;

{ TGUIManagerRun }

class function TGUIManagerRun.Instance: TGUIManagerRun;
begin
    result := gGUIManager as TGUIManagerRun;
end;

procedure TGUIManagerRun.InterruptSignalEnd;
begin
    inherited;
    if TSystemEvents.Instance <> nil then
        TSystemEvents.Instance.ModalMessageFinish;
end;

procedure TGUIManagerRun.InterruptSignalStart();
begin
    inherited;
    if TSystemEvents.Instance <> nil then
        TSystemEvents.Instance.ModalMessageStart;
end;

constructor TGUIManagerRun.Create;
begin
    inherited Create();
    fSerialMessageCriticalSection := TThreadAPI.CreateSemaphore(1);
end;

destructor TGUIManagerRun.Destroy;
begin
    fSerialMessageCriticalSection.Free;
    inherited;
end;

procedure TGUIManagerRun.SetRunMainState(const aState: TRunMainState);
begin

end;

procedure TGUIManagerRun.SetUserLogRun(aValue: boolean);
begin
    //
end;

procedure TGUIManagerRun.SimulationMode_Enable(const aEnabled: boolean);
begin

end;

procedure TGUIManagerRun.SourceLabel_Change(const aSourceKind, aSourceName: string);
begin

end;

procedure TGUIManagerRun.SQLResult_Display(const aSQLStr, aFileName: string);
begin

end;

procedure TGUIManagerRun.SQLTable_Refresh(const aDisplayID: string);
begin

end;

procedure TGUIManagerRun.SystemVolume_Changed(aSender: TObject);
begin

end;

procedure TGUIManagerRun.WasteVolume_Changed(aSender: TObject);
begin

end;

function TGUIManagerRun.AskRestart_PromptModal(const aSourceDataName: string;
    const aDateLastStoppedStr: string; const aActionListDataCache: TActionListDataCache;
    const aRestartOnlyAtMarks: boolean; out oContinueAtID: int64;
    out oContinueAtSegmentIndex: integer): integer;
begin
    result := 0;
end;

procedure TGUIManagerRun.DiTiMenu_Clear;
begin

end;

procedure TGUIManagerRun.DiTiMenu_Load;
begin

end;

function TGUIManagerRun.EditMethodParameters_PromptModal(const aSubMethodName: string;
    const aParameters: TKeyArgValueList; aAllowGlobalIdents, aShowEditor: boolean): boolean;
begin
    result := true;
end;

procedure TGUIManagerRun.DisplayComponents_RunEnable(const aEnable: boolean);
begin

end;

function TGUIManagerRun.RunAbortInfo_PromptModal(const aActionStatus: TStringArray): integer;
begin
    result := 0;
end;

function TGUIManagerRun.RackPlaceList_PromptModal(const aRunName, aLayoutName, aRackTypes,
    aCaptionStr: string; aAbortAllowed, aMarkRacks: boolean): integer;
begin
    result := 0;
end;

procedure TGUIManagerRun.Delay_ChangeTime(const aID: string; aNewTime: string; const aProgress: integer);
begin

end;

procedure TGUIManagerRun.Delay_DeleteInfo(const aID: string);
begin

end;

procedure TGUIManagerRun.Delay_InsertInfo(const aID: string; aTime, aDescription: string);
begin

end;

function TGUIManagerRun.Delay_IsCancelled(const aID: string): boolean;
begin
    result := false;
end;

function TGUIManagerRun.GetRunMainState: TRunMainState;
begin
    EXIT(rmsReady);
end;

function TGUIManagerRun.GetStepByStepMode: boolean;
begin
    result := false;
end;

procedure TGUIManagerRun.GroupDisplay_InsertInfo(const aDisplayID: string; const aGroupNames: TStringArray;
    const aKey, aText: string; aInfoGroupBehaviour: TInfoGroupBehaviour);
begin
    // Dummy
end;

procedure TGUIManagerRun.GroupDisplay_StartNewRun(const aMethodName: string);
begin

end;

function TGUIManagerRun.RestartPreparations_PromptModal(const aMethodName: string;
    const aRestartPreparationList: TRestartPreparationStepList): integer;
begin
    result := 0;
end;

procedure TGUIManagerRun.LayoutName_Change(const aLayoutName: string);
begin

end;

procedure TGUIManagerRun.LoadDisplayComponentToMain(const aDisplayComponentName: string;
    const aContextID: string);
begin
end;

function TGUIManagerRun.Parser_InputModal(const aScopeName: string;
    aIdentsNeedingInput: TObjectList<TParserStoredIdent>; const aEnterKeyNavigationOnly: boolean): integer;
begin
    result := 0;
end;

procedure TGUIManagerRun.AcquireSerialAccess();
begin
    fSerialMessageCriticalSection.Enter();
end;

procedure TGUIManagerRun.ReleaseSerialAccess();
begin
    fSerialMessageCriticalSection.Leave();
end;

procedure TGUIManagerRun.StateSetPause;
begin
    //
end;

procedure TGUIManagerRun.RequestAndStateSetAbort;
begin
    //
end;

function TGUIManagerRun.ChooseLayout(const aCaption: string): string;
var
    xDA: TLayoutDataAdaptor;
begin
    xDA := TLayoutDataAdaptor.Create;
    try
        result := TGUIManagerRun.Instance.SelectItemBox(xDA.ReadAllNames(),
            TLanguageString.Read('Please choose a layout:', 'Bitte ein Layout wählen:'), aCaption);
    finally
        xDA.Free;
    end;
end;


end.
