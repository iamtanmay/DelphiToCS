unit SchedMakerClasses;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  16.02.05 pk                                TN2315   New
  28.02.05 pk  AddNewAction                  TN2314.1 no need to initialize min and max time before calling DetermineMinMaxTime
  02.03.05 pk  TSchedMakerProcess            TN2328   New field : fPriority
  07.11.05 pk                                TN2737   Various changes for Dynamic Scheduling
  14.11.05 pk                                TN2758   Various changes for Dynamic Scheduling
  06.06.06 pk  TSchedMakerSession.Destroy    TN3134   call fSession.Free
  29.01.07 pk  AddNewProcess                 TN3527   ProcessID argument given to SchedSession.CreateProcess
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Contnrs,
    ObjScheduler,
    SchedMakerTypes;

type

    TSchedMakerStepData = class
    end;

    TSchedMakerAction = class
    private
        fSchedAction: TSchedAction;
        fExecAction: TSchedMakerExecAction;
        function GetIsPending(): boolean;
    public
        constructor Create(aSchedAction: TSchedAction; aExecAction: TSchedMakerExecAction);
        destructor Destroy(); override;
        procedure RefreshData(aRefreshOnlyPending: boolean);
        property SchedAction: TSchedAction read fSchedAction write fSchedAction;
        property ExecAction: TSchedMakerExecAction read fExecAction write fExecAction;
        property IsPending: boolean read GetIsPending;
    end;

    TSchedMakerStepList = class(TObjectList)
    public
        constructor Create();
        procedure AddStep(aStep: TSchedMakerAction);
    end;

    TSchedMakerProcess = class(TSchedMakerStepList)
    private
        fSchedProcess: TSchedProcess;
        fSourceName: string;
        fProcessID: integer;
        fExeName: string;
        fExeProcessID: integer;
        class function MakeFlags(aMode: TEnScheduleMode; aStateFlag: TExecActionState): TSchedActionFlags;
    public
        constructor Create(aSchedProcess: TSchedProcess; aProcessID: integer; const aSourceName: string;
            const aExeName: string; aExeProcessID: integer);
        destructor Destroy(); override;

        function AddNewAction(aExecAction: TSchedMakerExecAction; aMode: TEnScheduleMode;
            aElapsedTime: cardinal): TSchedMakerAction;
        function FindNextPendingAction(var vAction: TSchedMakerAction): boolean;
        property SchedProcess: TSchedProcess read fSchedProcess;
        property ExeName: string read fExeName write fExeName;
        property ExeProcessID: integer read fExeProcessID;
        property ProcessID: integer read fProcessID;
    end;

    TSchedMakerProcessList = class(TObjectList)
    private
        function GetProcessAt(aIndex: integer): TSchedMakerProcess;
    public
        constructor Create(aOwnsObjects: boolean);
        procedure AddProcess(aProcess: TSchedMakerProcess);
        property Processes[aIndex: integer]: TSchedMakerProcess read GetProcessAt; default;
    end;

    TSchedMakerSession = class(TSchedMakerProcessList)
    private
        fSchedSession: TSchedSession;
        fSessionName: string;
    public
        constructor Create(const aSessionName: string);
        destructor Destroy(); override;
        procedure ClearSession();
        function AddNewProcess(aProcessID: integer; const aSourceName: string; const aExeName: string;
            aExePriority: integer): TSchedMakerProcess;
        function FindProcessByProcessID(aProcessID: integer): TSchedMakerProcess;
        property SchedSession: TSchedSession read fSchedSession;
        property SessionName: string read fSessionName;
    end;


implementation


const
    INT_DEFAULT_PRIORITY = 1;

constructor TSchedMakerAction.Create(aSchedAction: TSchedAction; aExecAction: TSchedMakerExecAction);
begin
    inherited Create();
    fSchedAction := aSchedAction;
    fExecAction := aExecAction;
end;

destructor TSchedMakerAction.Destroy();
begin
    fExecAction.Free;
    inherited;
end;

procedure TSchedMakerAction.RefreshData(aRefreshOnlyPending: boolean);
begin
    with fSchedAction do
    begin
        // Do not update the already ended or started actions
        // This IF is needed to prevent updating the data of actions that have already been started in the ZARunner
        if aRefreshOnlyPending and (not GetIsPending()) then
            EXIT;
        fExecAction.MinTime := MinLength;
        fExecAction.MaxTime := MaxLength;
        fExecAction.SchedStart := StartTime;
        fExecAction.SchedEnd := EndTime;
    end;
end;

function TSchedMakerAction.GetIsPending(): boolean;
begin
    result := [afStarted, afNoSchedule] * fSchedAction.Flags = [];
end;

constructor TSchedMakerStepList.Create();
begin
    inherited;
end;

procedure TSchedMakerStepList.AddStep(aStep: TSchedMakerAction);
begin
    self.Add(aStep);
end;

constructor TSchedMakerProcess.Create(aSchedProcess: TSchedProcess; aProcessID: integer;
    const aSourceName: string; const aExeName: string; aExeProcessID: integer);
begin
    inherited Create();
    fSchedProcess := aSchedProcess;
    fProcessID := aProcessID;
    fSourceName := aSourceName;
    fExeName := aExeName;
    fExeProcessID := aExeProcessID;
end;

destructor TSchedMakerProcess.Destroy();
begin
    inherited;
end;

// ------------------------------------------------------------------------------------------------------------
class function TSchedMakerProcess.MakeFlags(aMode: TEnScheduleMode; aStateFlag: TExecActionState)
    : TSchedActionFlags;
// ------------------------------------------------------------------------------------------------------------
begin
    result := [];
    case aMode of
        smRunTime:
            begin
                // if aStateFlag >= 1 then
                // result := result + [afStarted];
                if aStateFlag in [easFinished, easFailed] then
                    result := result + [afNoSchedule];
            end;
        smRescheduleLive:
            begin
                if aStateFlag in [easStarted, easFinished, easFailed] then
                    result := result + [afStarted];
                if aStateFlag in [easFinished, easFailed] then
                    result := result + [afNoSchedule];
            end;
        smNormal:
            begin
                if aStateFlag in [easFinished, easFailed] then
                    result := result + [afStarted, afNoSchedule];
            end;
    end;
end;

// ------------------------------------------------------------------------------------------------------------
function TSchedMakerProcess.AddNewAction(aExecAction: TSchedMakerExecAction; aMode: TEnScheduleMode;
    aElapsedTime: cardinal): TSchedMakerAction;
// ------------------------------------------------------------------------------------------------------------
var
    xActionID: cardinal;
    xSchedAction: TSchedAction;
    xMinTime, xMaxTime: cardinal;
    xSchedStart, xSchedEnd: integer;
    xFlags: TSchedActionFlags;
    xState: TExecActionState;
begin
    aExecAction.DetermineTimes(aElapsedTime, (aMode = smRescheduleLive), xMinTime, xMaxTime, xSchedStart,
        xSchedEnd, xState);
    xFlags := MakeFlags(aMode, xState);
    // add new action to SchedProcess
    xActionID := fSchedProcess.AppendToActionList(aExecAction.ActionName, 0, aExecAction.SharedID,
        aExecAction.IsShareParent, xMinTime, xMaxTime, xFlags);

    xSchedAction := fSchedProcess.GetActionByID(xActionID);

    if (aMode = smRunTime) then
    begin
        // this is needed if the start and end schedule times are read straight from the database and when no scheduling will be done
        if xSchedStart > 0 then
            xSchedAction.StartTime := xSchedStart;
        if xSchedEnd > 0 then
            xSchedAction.EndTime := xSchedEnd;
    end;

    // create new SchedMakerAction
    result := TSchedMakerAction.Create(xSchedAction, aExecAction);
    // add Action to process list
    self.AddStep(result);

end;

function TSchedMakerProcess.FindNextPendingAction(var vAction: TSchedMakerAction): boolean;
var
    i: integer;
begin
    result := true;
    for i := 0 to self.Count - 1 do
    begin
        vAction := self[i] as TSchedMakerAction;
        if vAction.IsPending then
            EXIT;
    end;
    result := false;
end;

constructor TSchedMakerProcessList.Create(aOwnsObjects: boolean);
begin
    inherited Create(aOwnsObjects);
end;

procedure TSchedMakerProcessList.AddProcess(aProcess: TSchedMakerProcess);
begin
    self.Add(aProcess);
end;

constructor TSchedMakerSession.Create(const aSessionName: string);
begin
    inherited Create(true);
    fSchedSession := TSchedSession.Create(nil);
    fSessionName := aSessionName;
end;

destructor TSchedMakerSession.Destroy();
begin
    fSchedSession.Free;
    inherited;
end;

function TSchedMakerSession.AddNewProcess(aProcessID: integer; const aSourceName: string;
    const aExeName: string; aExePriority: integer): TSchedMakerProcess;
var
    xSchedProcess: TSchedProcess;
begin
    // add new process to SchedSession
    xSchedProcess := FSchedSession.CreateProcess(aProcessID, INT_DEFAULT_PRIORITY);

    // create / add new SchedMakerProcess
    result := TSchedMakerProcess.Create(xSchedProcess, aProcessID, aSourceName, aExeName, aExePriority);
    // add process to session list
    self.AddProcess(result);
end;

function TSchedMakerSession.FindProcessByProcessID(aProcessID: integer): TSchedMakerProcess;
var
    xProcess: TSchedMakerProcess;
    i: integer;
begin
    result := nil;
    for i := 0 to self.Count - 1 do
    begin
        xProcess := self[i];
        if xProcess.ProcessID <> aProcessID then
            CONTINUE;
        result := xProcess;
        EXIT;
    end;
end;

procedure TSchedMakerSession.ClearSession();
begin
    fSchedSession.Reset();
    self.Clear;
end;

function TSchedMakerProcessList.GetProcessAt(aIndex: integer): TSchedMakerProcess;
begin
    result := self.Items[aIndex] as TSchedMakerProcess;
end;


end.
