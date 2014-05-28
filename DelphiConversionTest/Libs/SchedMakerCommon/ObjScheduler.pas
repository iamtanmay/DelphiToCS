unit ObjScheduler;


{ --------------------------------------------------------------------------------------------------
  TScheduler, TSchedResUsage, TSchedAction , TSchedResource, TSchedProcess TSchedSession objects for Scheduler
  TScheduler object inherits TSiasScheduler object from .ocx library
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure          track-no  Änderung / Neuerung
  -------- --  -------------------------   --------  -------------------------------------------------
  14.06.02 pk                                        Created
  25.06.02 pk                                        Destructor override directive added to all objects
  14.07.02 pk                                        New ShareManager object
  14.07.02 pk                                        Write data to TScheduler only when Save functions are called
  14.07.02 pk                                        Do not save Actions unless the Enabled flag is true
  14.07.02 pk                                        SequenceID and SharedID from Cardinal => Integer
  02.09.02 pk                                        TSchedResource.Assign and TSchedResource overloaded constructor added
  02.09.02 pk                                        Properties added to all objects for more flexibility
  07.04.03 wl  TSchedAction                  TN1345  some properties --> TSchedActionMinimal (SchedActionThread.pas)
  06.08.03 pk                                        TSchedShareItem.Verify Corrected
  08.08.03 pk                                TN1529  Documenation
  08.12.03 pk  TScheduler.Create             TN1697  Better error handling if the .ocx is not registered
  21.07.04 pk  TSchedActionMinimal           TN2049  Removed. TSchedAction code moved back here from TSchedActionThread
  27.01.05 pk  TSchedAction.UpdateTimes      TN2281.3if afNoSchedule is in flags then set start/endtime to 0
  02.02.05 pk  TScheduler.SetDefaultValues   TN2303  Algorithm changed from Simple to Advanced
  07.11.05 pk                                TN2737  Shift function extended to make backward shifting possible
  14.11.05 pk                                TN2758  Shift function extended to shift started action of a certain process
  06.04.06 pk  TSchedSession.Save            TN3024  ShareManager.Verify removed for now
  16.05.06 pk  TSchedSession.fErrorList      TN3103  Save all errors in list
  23.05.06 pk  TSchedError.fActionID         TN3103  Changed to cardinal
  06.06.06 pk  TSchedProcess.Create          TN3132  Number of actions per process changed from 1000 to 10000
  13.12.06 pk  TSchedSession                 TN3469  New: Cancelschedule
  29.01.07 pk                                TN3527  user ZASchedulerLib instead of SiasSchedulerLib
  29.01.07 pk                                TN3527  integer times changed to cardinal
  29.01.07 pk  TSchedSession                 TN3527  AppendToProcessList changed to CreateProcess. Get ProcessID as argument, instead of generating it
  29.01.07 pk  Shift                         TN3527  shifting is now done using algorithm in ocx
  26.03.07 pk  TSchedAction.ProcessID        TN3645  New
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  ---------------------------------------------------------------------------------------------------
}
interface


uses
    classes,
    Sysutils,
    ListClasses,
    ZASCHEDULERLib_TLB;

type

    TSchedAction = class; // forward declaration

    TSchedOptComplexity = (ocTrivial, ocSimple, ocAdvanced, ocShift);
    TSchedOptSamePriority = (osSimple, osDispatch, osLonger, osShorter);
    TListSortCompare = function(Item1, Item2: Pointer): Integer;

    TEnVerifyOption = (vfDisableParentless, vfDisableChildless);
    TSetVerifyOptions = set of TEnVerifyOption;

    ENoSchedulerObject = class(Exception);
    // --------------------------------------------------------------------------------------------------
    TSchedShiftType = (sstShift, sstUnshift);
    TSchedSessionShiftEvent = procedure(aActionID: integer; aMinTime: cardinal; aShiftTime: cardinal;
        aShiftType: TSchedShiftType) of object;

    TScheduler = class(TZAScheduler)

    private
        FCount: Smallint;
        fMaxPriority: cardinal;
        fOnSessionShift: TSchedSessionShiftEvent;
        function ReadResourceCount: Cardinal;
        function ReadSessionDuration: Cardinal;
        function ReadProcessCount: Cardinal;
        function ReadActionCount: Cardinal;
        function ReadFlag1: TSchedOptComplexity;
        procedure SetFlag1(Flag: TSchedOptComplexity);
        function ReadFlag2: TSchedOptSamePriority;
        procedure SetFlag2(Flag: TSchedOptSamePriority);
        function GetInternalPriority(aPriority: cardinal): cardinal;
    public
        class function Create(AOwner: TComponent): TScheduler; reintroduce;
        // constructor Create(AOwner: TComponent; SessionName:string=''); reintroduce;
        destructor Destroy; override;
        procedure OpenSession(aSessionName: string; aMaxPriority: cardinal);
        procedure SetDefaultValues;
        function DoAdjustActionData(aActionID: Integer; aAdjustType: Integer; aValue: cardinal): boolean;
        procedure AddResource(const ID: cardinal; const name: string; const Sharelevel: SmallInt);
        function AddProcess(ProcessId: Integer; const name: WideString; Priority: Smallint;
            Color: Smallint): Integer;

        property ResourceCount: Cardinal read ReadResourceCount;
        property ProcessCount: Cardinal read ReadProcessCount;
        property ActionCount: Cardinal read ReadActionCount;
        property SessionDuration: Cardinal read ReadSessionDuration;
        property OptComplexity: TSchedOptComplexity read ReadFlag1 write SetFlag1;
        property OptSamePriority: TSchedOptSamePriority read ReadFlag2 write SetFlag2;
        property OnSessionShift: TSchedSessionShiftEvent read fOnSessionShift write fOnSessionShift;
    end;

    // --------------------------------------------------------------------------------------------------

    TSchedShareItem = class
    private
        FSharedID: integer;
        FParentAction: TSchedAction;
        FChildActions: TStringKeyObjectValueList;
        function GetCount: integer;
        function GetItemAt(aIntIndex: integer): TSchedAction;
    public
        constructor Create(SharedID: integer);
        destructor Destroy; override;
        procedure SetSharedID(SharedID: integer);
        function Add(Action: TSchedAction): integer;
        procedure Verify(aOptions: TSetVerifyOptions);
        property SharedID: integer read FSharedID write SetSharedID;
        property ParentAction: TSchedAction read FParentAction;
        property Count: integer read GetCount;
        property Items[aIntIndex: integer]: TSchedAction read GetItemAt;
    end;

    // --------------------------------------------------------------------------------------------------
    TSchedShareManager = class
    private
        FItems: TStringKeyObjectValueList;
        FVerifyOptions: TSetVerifyOptions;
        function GetCount: integer;
        function GetItemAt(aIntIndex: integer): TSchedShareItem;
    public
        constructor Create;
        destructor Destroy; override;
        function Add(Action: TSchedAction): integer;
        function ChangeID(aIntOldSharedID, aIntNewSharedID: integer): boolean;
        procedure Verify;
        property VerifyOptions: TSetVerifyOptions read FVerifyOptions write FVerifyOptions;
        property Count: integer read GetCount;
        property Items[aIntIndex: integer]: TSchedShareItem read GetItemAt; default;
    end;
    // --------------------------------------------------------------------------------------------------

    TSchedResUsage = class
    private
        FScheduler: TScheduler;
        FActionID: Cardinal;
        FResourceID: Cardinal;
        FUseLevel: SmallInt;
        FReleaseLevel: SmallInt;
        FAcquired: boolean;

    public
        constructor Create(Scheduler: TScheduler; ActionID, ResourceID: Cardinal;
            const UseLevel, Releaselevel: SmallInt);
        destructor Destroy; override;
        procedure Save(aNumTimesScheduled: integer);
        property ID: cardinal read FResourceID;
        property UseLevel: SmallInt read FUseLevel;
        property ReleaseLevel: SmallInt read FReleaseLevel;
        property Acquired: boolean read FAcquired write FAcquired;
    end;

    // --------------------------------------------------------------------------------------------------
    TSchedActionFlag = (afStarted, afNoSchedule, afNoWrite);
    TSchedActionFlags = set of TSchedActionFlag;
    TShiftType = (stForward, stBackward);

    TSchedAction = class
    private
        FScheduler: TScheduler;
        FID: Cardinal;
        FSequenceID: variant;
        FName: string;
        FSharedID: integer;
        FProcessPriority: integer;
        FStartTime: cardinal;
        FEndTime: cardinal;
        FIsShareParent: boolean;
        FFlags: TSchedActionFlags;
        FMinLength: Cardinal;
        FMaxLength: Cardinal;
        FProcessID: Cardinal;
        FResUsageList: TList;

        function GetRealStartTime: cardinal;
        function GetRealEndTime: cardinal;
        procedure SetRealStartTime(aStart: cardinal);
        procedure SetRealEndTime(aEnd: cardinal);
        procedure SetDuration(aDuration: cardinal);
        function GetResUsageByIndex(aCrdIndex: cardinal): TSchedResUsage;
        function GetStartTime(): cardinal;
        function GetEndTime(): cardinal;
    public
        constructor Create(Scheduler: TScheduler; SequenceID: variant; ActionID: Cardinal; name: string;
            ProcessID, ProcessPriority: Cardinal; SharedID: integer; IsShareParent: boolean;
            MinLength, MaxLength: Cardinal; aFlags: TSchedActionFlags);
        destructor Destroy; override;

        procedure Shift(aShiftBy: cardinal; aShiftType: TShiftType; aShiftStarted: boolean;
            aShiftOnlyStarted: boolean);
        procedure UpdateTimes;
        function AppendToResUsageList(ResourceID: integer; UseLevel, Releaselevel: SmallInt): cardinal;
        procedure Save(aNumTimesScheduled: integer);
        procedure SaveResources(aNumTimesScheduled: integer);
        function NextAvailResUsageID: cardinal;
        function GetResourceUsageCount: integer;

        property ID: cardinal read fID;
        property MinLength: cardinal read fMinLength write fMinLength;
        property MaxLength: cardinal read fMaxLength write fMaxLength;
        property name: string read fName;
        property SharedID: integer read fSharedID write fSharedID;
        property SequenceID: variant read fSequenceID write FSequenceID;
        property ProcessPriority: integer read fProcessPriority;
        property ProcessID: cardinal read fProcessID;
        property StartTime: cardinal read GetStartTime write fStartTime;
        property EndTime: cardinal read GetEndTime write fEndTime;
        property IsShareParent: boolean read fIsShareParent;
        property Flags: TSchedActionFlags read FFlags write FFlags;
        property RealStartTime: cardinal read GetRealStartTime write SetRealStartTime;
        property RealEndTime: cardinal read GetRealEndTime write SetRealEndTime;
        property Duration: cardinal write SetDuration;
        property ResUsages[aCrdIndex: cardinal]: TSchedResUsage read GetResUsageByIndex;
        property ResUsageCount: integer read GetResourceUsageCount;
    end;

    // --------------------------------------------------------------------------------------------------
    TSchedResource = class
    private
        FScheduler: TScheduler;
    protected
        FID: cardinal;
        FName: string;
        FShareLevel: SmallInt;
    public
        constructor Create(Scheduler: TScheduler; const ID: cardinal; const name: string;
            const Sharelevel: SmallInt); overload;
        constructor Create(aSchedResource: TSchedResource); overload;
        procedure Assign(aSchedResource: TSchedResource);
        property ID: cardinal read FID;
        property name: string read FName;
        property ShareLevel: SmallInt read FShareLevel;
    end;
    // --------------------------------------------------------------------------------------------------

    TSchedProcessDataHandle = record
        DatasourceName: string;
        Priority: smallint;
    end;

    TSchedProcess = class
    private
        FScheduler: TScheduler;
        FID: Cardinal;
        FName: string;
        FPriority: cardinal;
        FActionIDRoot: Cardinal;
        FActionList: TStringList;
        FShareManager: TSchedShareManager;
        FOldDataHandle: TSchedProcessDataHandle;
        property Priority: cardinal read fPriority;
    public
        constructor Create(scheduler: TScheduler; ShareManager: TSchedShareManager; ProcessID: cardinal;
            const name: string; const Priority: Integer);
        destructor Destroy; override;
        procedure UpdateTimes;
        function AppendToActionList(name: string; SequenceID: variant; SharedID: integer;
            IsShareParent: boolean; MinTime, MaxTime: cardinal; aFlags: TSchedActionFlags): cardinal;
        function GetActionByIndex(index: cardinal): TSchedAction;
        function GetActionById(ActionID: cardinal): TSchedAction;
        procedure Save(aNumTimesScheduled: integer);
        procedure SaveActions(aNumTimesScheduled: integer);
        { procedure   AddResourceUseToLastAction(ResourceID, UseLevel, ReleaseLevel:cardinal);
          procedure   AddResourceUseToAction(ActionID, ResourceID, UseLevel, ReleaseLevel:cardinal);
        }
        function GetActionCount: integer;
        function NextAvailActID: cardinal;
        function LastActID: cardinal;
        procedure Shift(aShiftBy: cardinal; aShiftType: TShiftType; aShiftStarted: boolean;
            aShiftOnlyStarted: boolean);

        procedure SetOldDataHandle(aDataHandle: TSchedProcessDataHandle);

        class function MakeSchedProcessDataHandle(aDatasourceName: string; aPriority: integer)
            : TSchedProcessDataHandle;

        property name: string read FName;

        property Actions[aIntIndex: cardinal]: TSchedAction read GetActionByIndex;
        property ActionCount: integer read GetActionCount;
        property OldDataHandle: TSchedProcessDataHandle read fOldDataHandle;
        property ProcessID: cardinal read FID;
    end;

    // --------------------------------------------------------------------------------------------------
    TSchedError = class
    private
        FScheduler: TScheduler;
        FIndex: Smallint;
        FErrorCode: Integer;
        FTime: Integer;
        FProcessID: Integer;
        FActionID: cardinal;
        FResourceID: Integer;
        FText: WideString;
        FDescription: WideString;

    public
        constructor Create(Scheduler: TScheduler);
        procedure AssignByErrorInd(index: smallint);
        function GetErrorCount: smallInt;
        function GetCurrentError(intErrorCode: integer): string;
        property Time: integer read fTime;
        property ProcessID: integer read fProcessID;
        property ActionID: cardinal read fActionID;
        property ResourceID: integer read fResourceID;
        property Description: widestring read fDescription;
    end;

    // --------------------------------------------------------------------------------------------------
    TSchedSession = class
    private
        FOwner: TComponent;
        FSessionName: string;
        fErrorList: TGeneralObjectList;
        FScheduler: TScheduler;
        FProcessIDRoot: cardinal;
        FProcessList: TStringList;
        FResourceList: TStringList;
        FShareManager: TSchedShareManager;
        FNumTimesScheduled: integer;

        fShiftActionID: integer;
        fShiftStartTime: cardinal;
        fShiftBy: cardinal;
        fShiftType: TSchedShiftType;

        function GetProcessCount: cardinal;
        function GetResourceCount: cardinal;
        function GetResourceByIndex(index: cardinal): TSchedResource;
        procedure DoOnSessionShift(aActionID: integer; aShiftStartTime: cardinal; aShiftBy: cardinal;
            aShiftType: TSchedShiftType);

    protected
        procedure SaveProcesses;
        procedure Save;
    public

        constructor Create(AOwner: TComponent);
        destructor Destroy; override;
        procedure Open(aSessionName: string; aMaxPriority: cardinal);
        procedure Reset();
        function ScheduleSession(aStartTime: integer = 0): integer;
        procedure CancelSchedule();
        // procedure   Shift( aShiftBy : cardinal; aShiftType : TShiftType; aShiftStartedActionForPriority : cardinal; aShiftOnlyStarted : boolean );

        function CreateProcess(aProcessID: integer; aPriority: integer): TSchedProcess;
        function AppendToResourceList(shareLevel: smallInt; resourceName: string): cardinal;
        function GetSessionDuration: integer;

        function GetErrorList(): TGeneralObjectList;
        function GetResIDByName(name: string): cardinal;

        function GetProcessByIndex(index: cardinal): TSchedProcess;
        function GetProcessByID(processID: cardinal): TSchedProcess;
        function GetActionCountByProcID(processID: integer): integer;
        function GetProcessByName(name: string): TSchedProcess;
        function GetProcessByOldDataHandle(aOldDataHandle: TSchedProcessDataHandle): TSchedProcess;
        function GetProcessByPriority(aPriority: cardinal): TSchedProcess;

        function NextAvailResID: cardinal;
        function NextAvailProcID: cardinal;
        procedure UpdateTimes;
        function ShiftIfNeeded(): boolean;
        function IsShiftNeeded(): boolean;
        procedure Shift(aShiftStartTime: cardinal; aShiftBy: cardinal; aShiftType: TSchedShiftType);

        property Processes[aIntIndex: cardinal]: TSchedProcess read GetProcessByIndex;
        property Resources[aIntIndex: cardinal]: TSchedResource read GetResourceByIndex;
        property ProcessCount: cardinal read GetProcessCount;
        property ResourceCount: cardinal read GetResourceCount;
        property ShareManager: TSchedShareManager read FShareManager;
    end;
    // --------------------------------------------------------------------------------------------------

function ActionComp(Item1, Item2: Pointer): Integer;


implementation


// ##################################################################################################

uses
    dialogs;

const
    INT_NUM_ACTIONS_PER_PROCESS = 10000;
    STR_SCHEDULER_OCX_NAME = 'ZAScheduler';

    // *********************************** global functions *********************************************
    // --------------------------------------------------------------------------------------------------
function ActionComp(Item1, Item2: Pointer): Integer;
// --------------------------------------------------------------------------------------------------
var
    action1, action2: TSchedAction;
begin
    result := 0;
    action1 := Item1;
    action2 := Item2;
    if (action1.StartTime > 0) or (action2.StartTime > 0) then
    begin
        result := action1.StartTime - action2.StartTime;
        // result := action1.SequenceID - action2.SequenceID;
    end;

    if result = 0 then
        result := action1.ProcessPriority - action2.ProcessPriority;

end;

// ***********************************TSchedShareManager*********************************************
// --------------------------------------------------------------------------------------------------
constructor TSchedShareManager.Create;
// --------------------------------------------------------------------------------------------------
begin
    inherited Create();
    FItems := TStringKeyObjectValueList.Create(true);
    FVerifyOptions := [vfDisableParentless, vfDisableChildless];
end;

// --------------------------------------------------------------------------------------------------
destructor TSchedShareManager.Destroy;
// --------------------------------------------------------------------------------------------------
var
    i: integer;
    ShareItem: TSchedShareItem;
begin
    for i := 0 to FItems.Count - 1 do
    begin
        ShareItem := Items[i];
        ShareItem.Free;
    end;
    FreeAndNil(fItems);

end;

// --------------------------------------------------------------------------------------------------
function TSchedShareManager.Add(Action: TSchedAction): integer;
// --------------------------------------------------------------------------------------------------
// If the Action is not enabled, or if it has an invalid SharedID then do nothing.
// Otherwise, if the share group with the SharedID of the Action does not yet exist in
// the ShareManager then create a new share group.
// Add the Action to the Share Group
// --------------------------------------------------------------------------------------------------
const
    STR_UNDEFINED_SHAREDID: string = '0';
var
    intIndex: integer;
    intSharedID: cardinal;
    strSharedID: string;
    shareItem: TSchedShareItem;
begin
    result := 0;
    intSharedID := Action.SharedID;
    strSharedID := IntToStr(intSharedID);
    if strSharedID = STR_UNDEFINED_SHAREDID then
        Exit;
    if (afNoSchedule in Action.Flags) then
        Exit;
    intIndex := FItems.IndexOf(strSharedID);
    if intIndex = -1 then
    begin
        shareItem := TSchedShareItem.Create(intSharedID);
        intIndex := FItems.AddObject(strSharedID, shareItem);
    end;
    shareItem := FItems.Objects[intIndex] as TSchedShareItem;
    shareItem.Add(Action);

end;

// --------------------------------------------------------------------------------------------------
function TSchedShareManager.GetCount: integer;
// --------------------------------------------------------------------------------------------------
// Get the number of Share groups that are currently stored in this ShareManager
// --------------------------------------------------------------------------------------------------
begin
    result := FItems.Count;
end;

// --------------------------------------------------------------------------------------------------
function TSchedShareManager.GetItemAt(aIntIndex: integer): TSchedShareItem;
// --------------------------------------------------------------------------------------------------
begin
    result := FItems.Objects[aIntIndex] as TSchedShareItem;
end;

// --------------------------------------------------------------------------------------------------
function TSchedShareManager.ChangeID(aIntOldSharedID, aIntNewSharedID: integer): boolean;
// --------------------------------------------------------------------------------------------------
// find the share group with the SharedID aIntOldSharedID and change its SharedID to aIntNewSharedID
// --------------------------------------------------------------------------------------------------
var
    intIndex: integer;
    shareItem: TSchedShareItem;
begin
    result := false;
    intIndex := FItems.IndexOf(IntToStr(aIntOldSharedID));
    if intIndex = -1 then
        Exit;
    shareItem := Items[intIndex];
    shareItem.SetSharedID(aIntNewSharedID);
    result := true;
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedShareManager.Verify;
// --------------------------------------------------------------------------------------------------
// For each share group verify that the group is valid.
// --------------------------------------------------------------------------------------------------
var
    i: integer;
    ShareItem: TSchedShareItem;
begin
    for i := 0 to FItems.Count - 1 do
    begin
        ShareItem := FItems.Objects[i] as TSchedShareItem;
        ShareItem.Verify(FVerifyOptions);
    end;
end;

// ***********************************TSchedShareItem************************************************
// --------------------------------------------------------------------------------------------------
constructor TSchedShareItem.Create(SharedID: integer);
// --------------------------------------------------------------------------------------------------
begin
    inherited Create();
    FParentAction := nil;
    FSharedID := SharedID;
    FChildActions := TStringKeyObjectValueList.Create();
end;

// --------------------------------------------------------------------------------------------------
destructor TSchedShareItem.Destroy;
// --------------------------------------------------------------------------------------------------
begin
    FChildActions.Free;
    inherited;
end;

// --------------------------------------------------------------------------------------------------
function TSchedShareItem.Add(Action: TSchedAction): integer;
// --------------------------------------------------------------------------------------------------
// add the action 'Action' to this Share Group.
// if the action's IsShareParent flag is true, then add it as the parent action
// otherwise add it to the list of child actions.
// --------------------------------------------------------------------------------------------------
begin
    if Action.IsShareParent then
    begin
        FParentAction := Action;
        result := -1;
    end
    else
    begin
        result := FChildActions.AddObject(IntToStr(Action.SequenceID), Action);
    end;
end;

// --------------------------------------------------------------------------------------------------
function TSchedShareItem.GetCount: integer;
// --------------------------------------------------------------------------------------------------
begin
    result := FChildActions.Count;
end;

// --------------------------------------------------------------------------------------------------
function TSchedShareItem.GetItemAt(aIntIndex: integer): TSchedAction;
// --------------------------------------------------------------------------------------------------
begin
    result := FChildActions.Objects[aIntIndex] as TSchedAction;
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedShareItem.SetSharedID(SharedID: integer);
// --------------------------------------------------------------------------------------------------
var
    i: integer;
    action: TSchedAction;
begin
    if FParentAction <> nil then
        FParentAction.SharedID := SharedID;
    FSharedID := SharedID;
    for i := 0 to FChildActions.Count - 1 do
    begin
        action := Items[i];
        action.SharedID := FSharedID;
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedShareItem.Verify(aOptions: TSetVerifyOptions);
// --------------------------------------------------------------------------------------------------
// If the vfDisableParentless option is in aOptions and no parent for this share group is assigned,
// set the enabled flag of all the children to false.
// If the vfDisableChildless option is in the aOptions and no children are in the child list of the
// group then set the enabled flag of the parent to false.
// --------------------------------------------------------------------------------------------------
var
    i: integer;
    action: TSchedAction;
begin
    if vfDisableParentless in aOptions then
    begin
        if not Assigned(FParentAction) then
        begin
            for i := 0 to FChildActions.Count - 1 do
            begin
                action := Items[i];
                action.Flags := action.Flags + [afNoSchedule];
                Exit;
            end;
        end;
    end;

    if vfDisableChildless in aOptions then
    begin
        if ((FChildActions.Count = 0) and Assigned(FParentAction)) then
        begin
            FParentAction.Flags := FParentAction.Flags + [afNoSchedule];
        end;
    end;
end;

// ******************************************TSchedError*********************************************
// --------------------------------------------------------------------------------------------------
constructor TSchedError.Create(Scheduler: TScheduler);
// --------------------------------------------------------------------------------------------------
begin
    inherited Create;
    FScheduler := Scheduler;
    FIndex := -1;
    FErrorCode := -1;
    FTime := -1;
    FResourceID := -1;
    FActionID := -0;
    FResourceID := -1;
    FText := '';
    FDescription := '';
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedError.AssignByErrorInd(index: smallint);
// --------------------------------------------------------------------------------------------------
var
    xActionID: integer;
begin
    FIndex := index;

    FScheduler.GetError(FIndex, FErrorCode, FTime, FProcessID, xActionID, FResourceID, FText, FDescription);
    if xActionID >= 0 then
        fActionID := cardinal(xActionID)
    else
        fActionID := 0;
end;

// --------------------------------------------------------------------------------------------------
function TSchedError.GetErrorCount: smallInt;
// --------------------------------------------------------------------------------------------------
// var smlResult:smallInt;
begin
    result := 0;
    // FScheduler.DisplayErrors(1020);
    FScheduler.GetErrorCount(result);
end;

// --------------------------------------------------------------------------------------------------
function TSchedError.GetCurrentError(intErrorCode: integer): string;
// --------------------------------------------------------------------------------------------------
var
    strResult: widestring;
begin
    FScheduler.GetErrorText(intErrorCode, self.FText);
    FScheduler.GetErrorDescription(self.FDescription);
    result := strResult;
end;

// *************************************TSchedResource***********************************************
// --------------------------------------------------------------------------------------------------
constructor TSchedResource.Create(Scheduler: TScheduler; const ID: cardinal; const name: string;
    const Sharelevel: SmallInt);
// --------------------------------------------------------------------------------------------------

begin
    inherited Create;
    FScheduler := Scheduler;
    FID := ID;
    FName := name;
    FShareLevel := Sharelevel;
    FScheduler.AddResource(FID, FName, FShareLevel);
end;

// --------------------------------------------------------------------------------------------------
constructor TSchedResource.Create(aSchedResource: TSchedResource);
// --------------------------------------------------------------------------------------------------
begin
    inherited Create;
    self.Assign(aSchedResource);
    FScheduler.AddResource(FID, FName, FShareLevel);
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedResource.Assign(aSchedResource: TSchedResource);
// --------------------------------------------------------------------------------------------------
begin
    FScheduler := aSchedResource.FScheduler;
    FID := aSchedResource.FID;
    FName := aSchedResource.FName;
    FShareLevel := aSchedResource.FShareLevel;
end;

// **************************************TSchedSession***********************************************
// --------------------------------------------------------------------------------------------------
constructor TSchedSession.Create(AOwner: TComponent);
// --------------------------------------------------------------------------------------------------
begin
    inherited Create;
    FOwner := AOwner;
    FSessionName := 'UNKNOWN'; // aSessionName;
    FScheduler := TScheduler.Create(FOwner);
    fScheduler.OnSessionShift := DoOnSessionShift;
    // FScheduler.OpenSession( aSessionName );
    fErrorList := TGeneralObjectList.Create;
    FProcessIDRoot := 1;
    FProcessList := TStringList.Create;
    FResourceList := TStringList.Create;
    FShareManager := TSchedShareManager.Create;
    FNumTimesScheduled := 0;
end;

// --------------------------------------------------------------------------------------------------
destructor TSchedSession.Destroy;
// --------------------------------------------------------------------------------------------------
var
    process: TSchedProcess;
    resource: TSchedResource;
    i: integer;
begin
    FOwner := nil;
    FSessionName := '';
    FErrorList.Free;
    if Assigned(FProcessList) then
    begin
        for i := 0 to FProcessList.Count - 1 do
        begin
            process := GetProcessByIndex(i);
            process.Free;
        end;
    end;
    if Assigned(FResourceList) then
    begin
        for i := 0 to FResourceList.Count - 1 do
        begin
            resource := GetResourceByIndex(i);
            resource.Free;
        end;
    end;
    FProcessList.Free;
    FResourceList.Free;
    FScheduler.Free;
    FShareManager.Free;
    inherited Destroy;
end;

procedure TSchedSession.Open(aSessionName: string; aMaxPriority: cardinal);
begin
    fScheduler.OpenSession(aSessionName, aMaxPriority);
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedSession.Reset();
// --------------------------------------------------------------------------------------------------
var
    process: TSchedProcess;
    resource: TSchedResource;
    i: integer;
begin
    // FOwner:=nil;
    // FError.
    if Assigned(FProcessList) then
    begin
        for i := 0 to FProcessList.Count - 1 do
        begin
            process := GetProcessByIndex(i);
            process.Free;
        end;
    end;
    if Assigned(FResourceList) then
    begin
        for i := 0 to FResourceList.Count - 1 do
        begin
            resource := GetResourceByIndex(i);
            resource.Free;
        end;
    end;
    FProcessList.Clear;
    FResourceList.Clear;

    fScheduler.CloseSession();
    // fScheduler.OpenSession( fSessionName, fScheduler. );

    FShareManager.Free;
    FShareManager := TSchedShareManager.Create;
    FNumTimesScheduled := 0;
end;

// --------------------------------------------------------------------------------------------------
function TSchedSession.ScheduleSession(aStartTime: integer = 0): integer;
// --------------------------------------------------------------------------------------------------
// Call Save to add all of the Scheduling info that is currently stored in the Session object
// to the Scheduler object.
// Attempt to Schedule, if there was an error, retrieve the error from the Scheduler and exit.
// If the scheduling was successful, update the times stored in the action objects with the times calculated
// by the Scheduler.
// Make a list of all of the actions in sorted order with the action that has the earliest start time
// being the first element of the list.
// --------------------------------------------------------------------------------------------------
begin

    Save();
    result := FScheduler.Schedule(aStartTime);
    // if result > 0 then EXIT;

    UpdateTimes();
    Inc(FNumTimesScheduled);

    fShiftBy := 0;
end;

procedure TSchedSession.CancelSchedule();
begin
    fScheduler.Stop();
end;
{
  //--------------------------------------------------------------------------------------------------
  procedure TSchedSession.Shift( aShiftBy : cardinal; aShiftType : TShiftType; aShiftStartedActionForPriority : cardinal; aShiftOnlyStarted : boolean );
  //--------------------------------------------------------------------------------------------------
  var
  xProcess:TSchedProcess;
  i:integer;
  xShiftStarted : boolean;
  begin
  for i:=0 to FProcessList.Count-1 do begin
  xProcess:= GetProcessByIndex(i);
  xShiftStarted := ( aShiftStartedActionForPriority = xProcess.Priority );
  if aShiftOnlyStarted and ( not xShiftStarted ) then CONTINUE;
  xProcess.Shift( aShiftBy, aShiftType, xShiftStarted, aShiftOnlyStarted );
  end;
  end;
}

procedure TSchedSession.DoOnSessionShift(aActionID: integer; aShiftStartTime: cardinal; aShiftBy: cardinal;
    aShiftType: TSchedShiftType);
begin
    fShiftActionID := aActionID;
    fShiftStartTime := aShiftStartTime;
    fShiftBy := aShiftBy;
    fShiftType := aShiftType;
end;

function TSchedSession.IsShiftNeeded(): boolean;
begin
    result := fShiftBy > 0;
end;

function TSchedSession.ShiftIfNeeded(): boolean;
begin
    result := true;
    self.Shift(fShiftStartTime, fShiftBy, fShiftType);

    // reset shifttime
    fShiftBy := 0;

end;

procedure TSchedSession.Shift(aShiftStartTime: cardinal; aShiftBy: cardinal; aShiftType: TSchedShiftType);
var
    xShiftTime: integer;
begin
    if aShiftType = sstShift then
        xShiftTime := integer(aShiftBy)
    else
        xShiftTime := -integer(aShiftBy);

    fScheduler.Shift(integer(aShiftStartTime), xShiftTime, fShiftActionID);
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedSession.UpdateTimes;
// --------------------------------------------------------------------------------------------------
// For each process in the session, update the times with the times calculated by the Scheduler
// --------------------------------------------------------------------------------------------------
var
    process: TSchedProcess;
    i: integer;
begin
    for i := 0 to FProcessList.Count - 1 do
    begin
        process := GetProcessByIndex(i);
        process.UpdateTimes();
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedSession.Save();
// --------------------------------------------------------------------------------------------------
// Call the verify function of the Share Manager to disable all of the invalid actions
// Call SaveProcesses to add all of the process objects to the Scheduler
// --------------------------------------------------------------------------------------------------
begin
    // FShareManager.Verify;
    SaveProcesses();
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedSession.SaveProcesses;
// --------------------------------------------------------------------------------------------------
// Add all of the processes in the processlist to the Scheduler
// --------------------------------------------------------------------------------------------------
var
    process: TSchedProcess;
    i: integer;
begin
    for i := 0 to FProcessList.Count - 1 do
    begin
        process := GetProcessByIndex(i);
        process.Save(FNumTimesScheduled);
    end;
end;

// --------------------------------------------------------------------------------------------------
function TSchedSession.CreateProcess(aProcessID: integer; aPriority: integer): TSchedProcess;
// --------------------------------------------------------------------------------------------------
// Create a new process with the processname 'processName'.  If processName is not provided then
// a unique process name will be assigned.
// Add the process to the processlist.
// Return the ID of the process.
// --------------------------------------------------------------------------------------------------
var
    xProcessName: string;
begin
    xProcessName := 'P' + IntToStr(aProcessID);
    result := TSchedProcess.Create(FScheduler, FShareManager, aProcessID, xProcessName, aPriority);
    FProcessList.AddObject(xProcessName, result);
end;

// --------------------------------------------------------------------------------------------------
function TSchedSession.AppendToResourceList(shareLevel: smallInt; resourceName: string): cardinal;
// --------------------------------------------------------------------------------------------------
// Create a new resource object and add it to the ResourceList
// --------------------------------------------------------------------------------------------------
var
    resource: TSchedResource;
    resourceID: cardinal;
begin
    resourceID := NextAvailResID();
    resource := TSchedResource.Create(FScheduler, resourceID, resourceName, shareLevel);
    FResourceList.AddObject(resourceName, resource);
    result := resourceID;
end;

function TSchedSession.GetErrorList(): TGeneralObjectList;
var
    xErrorCount: smallint;
    i: integer;
    xCurrErr: TSchedError;
begin
    fErrorList.Clear;
    FScheduler.GetErrorCount(xErrorCount);
    for i := 0 to xErrorCount - 1 do
    begin
        xCurrErr := TSchedError.Create(fScheduler);
        fErrorList.Add(xCurrErr);
        xCurrErr.AssignByErrorInd(i);
    end;
    result := fErrorList;
end;

// --------------------------------------------------------------------------------------------------
function TSchedSession.GetSessionDuration: integer;
// --------------------------------------------------------------------------------------------------
begin
    result := fScheduler.ReadSessionDuration();
end;

// --------------------------------------------------------------------------------------------------
function TSchedSession.GetResIDByName(name: string): cardinal;
// --------------------------------------------------------------------------------------------------
begin
    result := FResourceList.IndexOf(name);

end;

// --------------------------------------------------------------------------------------------------
function TSchedSession.GetResourceCount: cardinal;
// --------------------------------------------------------------------------------------------------
begin
    // result:=FScheduler.readResourceCount;
    result := FResourceList.Count;
end;

// --------------------------------------------------------------------------------------------------
function TSchedSession.GetProcessCount: cardinal;
// --------------------------------------------------------------------------------------------------
begin
    // result:=FScheduler.readProcessCount;
    result := FProcessList.Count;
end;

// --------------------------------------------------------------------------------------------------
function TSchedSession.NextAvailResID: cardinal;
// --------------------------------------------------------------------------------------------------
begin
    result := FResourceList.Count;
end;

// --------------------------------------------------------------------------------------------------
function TSchedSession.NextAvailProcID: cardinal;
// --------------------------------------------------------------------------------------------------
begin
    result := cardinal(FProcessList.Count) + FProcessIDRoot;
end;

// --------------------------------------------------------------------------------------------------
function TSchedSession.GetProcessByID(processID: cardinal): TSchedProcess;
// --------------------------------------------------------------------------------------------------
begin
    result := FProcessList.Objects[processID - FProcessIDRoot] as TSchedProcess;
end;

// --------------------------------------------------------------------------------------------------
function TSchedSession.GetResourceByIndex(index: cardinal): TSchedResource;
// --------------------------------------------------------------------------------------------------
begin
    result := FResourceList.Objects[index] as TSchedResource;
end;

// --------------------------------------------------------------------------------------------------
function TSchedSession.GetProcessByIndex(index: cardinal): TSchedProcess;
// --------------------------------------------------------------------------------------------------
begin
    result := FProcessList.Objects[index] as TSchedProcess;
end;

// --------------------------------------------------------------------------------------------------
function TSchedSession.GetProcessByName(name: string): TSchedProcess;
// --------------------------------------------------------------------------------------------------
var
    xIndex: integer;
    processID: integer;
begin
    result := nil;
    xIndex := FProcessList.IndexOf(name);
    if xIndex < 0 then
        Exit;

    processID := cardinal(xIndex) + FProcessIDRoot;
    result := GetProcessByID(processID);
end;

function TSchedSession.GetProcessByOldDataHandle(aOldDataHandle: TSchedProcessDataHandle): TSchedProcess;
var
    i: integer;
    xProcess: TSchedProcess;
begin
    result := nil;
    for i := 0 to fProcessList.Count - 1 do
    begin
        xProcess := fProcessList.Objects[i] as TSchedProcess;
        if (xProcess.OldDataHandle.DatasourceName = aOldDataHandle.DatasourceName) and
            (xProcess.OldDataHandle.Priority = aOldDataHandle.Priority) then
        begin
            result := xProcess;
            EXIT;
        end;
    end;

end;

function TSchedSession.GetProcessByPriority(aPriority: cardinal): TSchedProcess;
var
    i: integer;
begin
    for i := 0 to fProcessList.Count - 1 do
    begin
        result := fProcessList.Objects[i] as TSchedProcess;
        if (result.Priority = aPriority) then
            EXIT;
    end;
    result := nil;
end;

// --------------------------------------------------------------------------------------------------
function TSchedSession.GetActionCountByProcID(processID: integer): integer;
// --------------------------------------------------------------------------------------------------
var
    process: TSchedProcess;
begin
    process := GetProcessByID(processID);
    result := process.GetActionCount();
end;

// *****************************************TSchedProcess********************************************
// --------------------------------------------------------------------------------------------------
constructor TSchedProcess.Create(Scheduler: TScheduler; ShareManager: TSchedShareManager; ProcessID: cardinal;
    const name: string; const Priority: Integer);
// --------------------------------------------------------------------------------------------------
begin
    inherited Create;
    FScheduler := Scheduler;
    FID := ProcessID;
    FName := name;
    FPriority := Priority;
    FActionIDRoot := FID * INT_NUM_ACTIONS_PER_PROCESS;
    FActionList := TStringList.Create;
    FShareManager := ShareManager;
end;

// --------------------------------------------------------------------------------------------------
destructor TSchedProcess.Destroy;
// --------------------------------------------------------------------------------------------------
var
    xAction: TSchedAction;
    i: integer;
begin
    FScheduler := nil;
    for i := 0 to FActionList.Count - 1 do
    begin
        xAction := GetActionByIndex(i);
        FreeAndNil(xAction);
    end;
    FActionList.Free;
    inherited Destroy;
end;

procedure TSchedProcess.SetOldDataHandle(aDataHandle: TSchedProcessDataHandle);
begin
    fOldDataHandle.DatasourceName := aDataHandle.DatasourceName;
    fOldDataHandle.Priority := aDataHandle.Priority;
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedProcess.Shift(aShiftBy: cardinal; aShiftType: TShiftType; aShiftStarted: boolean;
    aShiftOnlyStarted: boolean);
// --------------------------------------------------------------------------------------------------
var
    xAction: TSchedAction;
    i: integer;
begin
    for i := 0 to FActionList.Count - 1 do
    begin
        xAction := GetActionByIndex(i);
        xAction.Shift(aShiftBy, aShiftType, aShiftStarted, aShiftOnlyStarted);
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedProcess.UpdateTimes;
// --------------------------------------------------------------------------------------------------
// For each action in the ActionList of this process, call UpdateTimes to update the action
// times in the Action objects with the times that are calculated by the Scheduler object after
// a new Scheduling has been requested.
// --------------------------------------------------------------------------------------------------
var
    action: TSchedAction;
    i: integer;
begin
    for i := 0 to FActionList.Count - 1 do
    begin
        action := GetActionByIndex(i);
        action.UpdateTimes();
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedProcess.Save(aNumTimesScheduled: integer);
// --------------------------------------------------------------------------------------------------
// Add this process to the Scheduler object and call SaveActions to add each action in this process
// to the Scheduler
// --------------------------------------------------------------------------------------------------
begin
    FScheduler.AddProcess(FID, FName, FPriority, 0);
    SaveActions(aNumTimesScheduled);
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedProcess.SaveActions(aNumTimesScheduled: integer);
// --------------------------------------------------------------------------------------------------
// For each action in this process, add the action to the Scheduler
// --------------------------------------------------------------------------------------------------
var
    action: TSchedAction;
    i: integer;
begin
    for i := 0 to FActionList.Count - 1 do
    begin
        action := GetActionByIndex(i);
        action.Save(aNumTimesScheduled);
    end;

end;

// --------------------------------------------------------------------------------------------------
function TSchedProcess.GetActionCount: integer;
// --------------------------------------------------------------------------------------------------
// Return the number of actions in this process.
// This number does NOT reflect the number of actions that are currently added to the Scheduler
// --------------------------------------------------------------------------------------------------
begin
    { FScheduler.GetProcessActionCount(FID,smlResult);
      result:=smlResult; }
    result := FActionList.Count;
end;

// --------------------------------------------------------------------------------------------------
function TSchedProcess.NextAvailActID: cardinal;
// --------------------------------------------------------------------------------------------------
// Return a unique ID that is NOT used for any other action in this process
// --------------------------------------------------------------------------------------------------
begin
    result := cardinal(FActionList.Count) + FActionIDRoot;
end;

// --------------------------------------------------------------------------------------------------
function TSchedProcess.LastActID: cardinal;
// --------------------------------------------------------------------------------------------------
// Return the ID of the last action that was added to the ActionList of this process
// --------------------------------------------------------------------------------------------------
begin
    ASSERT(FActionList.Count > 0, 'Action list does not contain any actions');
    result := cardinal(FActionList.Count - 1) + FActionIDRoot;
end;

// --------------------------------------------------------------------------------------------------
function TSchedProcess.AppendToActionList(name: string; SequenceID: variant; SharedID: integer;
    IsShareParent: boolean; MinTime, MaxTime: cardinal; aFlags: TSchedActionFlags): cardinal;
// --------------------------------------------------------------------------------------------------
// Create a SchedAction Object and add it to the ActionList of this process
// --------------------------------------------------------------------------------------------------
var
    action: TSchedAction;
    actionId: cardinal;
begin
    actionId := NextAvailActID();
    action := TSchedAction.Create(FScheduler, SequenceID, actionID, name, FID, FPriority, SharedID,
        IsShareParent, MinTime, MaxTime, aFlags);
    FActionList.AddObject(IntToStr(actionID), action);
    FShareManager.Add(action);
    result := actionID
end;

// --------------------------------------------------------------------------------------------------
function TSchedProcess.GetActionByIndex(index: cardinal): TSchedAction;
// --------------------------------------------------------------------------------------------------
begin
    result := FActionList.Objects[index] as TSchedAction;
end;

// --------------------------------------------------------------------------------------------------
function TSchedProcess.GetActionById(ActionID: cardinal): TSchedAction;
// --------------------------------------------------------------------------------------------------
var
    intIndex: integer;
begin
    intIndex := FActionList.IndexOf(IntToStr(ActionID));
    result := GetActionByIndex(intIndex);
end;

class function TSchedProcess.MakeSchedProcessDataHandle(aDatasourceName: string; aPriority: integer)
    : TSchedProcessDataHandle;
begin
    with result do
    begin
        DatasourceName := aDatasourceName;
        Priority := aPriority;
    end;
end;

{
  //--------------------------------------------------------------------------------------------------
  procedure TSchedProcess.AddResourceUseToLastAction(ResourceID,UseLevel, ReleaseLevel:cardinal);
  //--------------------------------------------------------------------------------------------------
  var
  actionID:integer;
  begin
  actionID:= LastActID();
  AddResourceUseToAction(actionID,ResourceID,UseLevel,ReleaseLevel);
  end;
  //--------------------------------------------------------------------------------------------------
  procedure TSchedProcess.AddResourceUseToAction(ActionID, ResourceID, UseLevel, ReleaseLevel:cardinal);
  //--------------------------------------------------------------------------------------------------
  var
  action:TSchedAction;
  actionIndex:cardinal;
  begin
  actionIndex:=ActionID-FActionIDRoot;
  action := FActionList.Objects[actionIndex] as TSchedAction;
  action.AppendToResUsageList(ResourceID, UseLevel, ReleaseLevel);
  end;

}
// ******************************************TSchedAction********************************************
// --------------------------------------------------------------------------------------------------
constructor TSchedAction.Create(Scheduler: TScheduler; SequenceID: variant; ActionID: Cardinal; name: string;
    ProcessID, ProcessPriority: Cardinal; SharedID: integer; IsShareParent: boolean;
    MinLength, MaxLength: Cardinal; aFlags: TSchedActionFlags);
// --------------------------------------------------------------------------------------------------
begin
    inherited Create;
    FScheduler := Scheduler;
    FID := ActionID;
    FSequenceID := SequenceID;
    FName := name;
    FMinLength := MinLength;
    FMaxLength := MaxLength;
    FSharedID := SharedID;
    FProcessID := ProcessID;
    FProcessPriority := ProcessPriority;
    fStartTime := 0;
    fEndTime := 0;
    FResUsageList := TList.Create;
    FFlags := aFlags;
    FIsShareParent := IsShareParent;
    // FScheduler.AddAction(FID,FName,FProcessID,FSharedID,FMinLength,FMaxLength,0,0);
end;

// --------------------------------------------------------------------------------------------------
destructor TSchedAction.Destroy;
// --------------------------------------------------------------------------------------------------
var
    i: integer;
    resUsage: TSchedResUsage;
begin
    FScheduler := nil;
    for i := 0 to FResusageList.Count - 1 do
    begin
        resUsage := FResusageList[i];
        resUsage.Free;
    end;
    FResusageList.Free;
    inherited Destroy;
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedAction.Shift(aShiftBy: cardinal; aShiftType: TShiftType; aShiftStarted: boolean;
    aShiftOnlyStarted: boolean);
// --------------------------------------------------------------------------------------------------
begin
    // if we want are not allowed to shift started and this action is started exit
    if (not aShiftStarted) and (afStarted in fFlags) then
        Exit;

    // if we are ONLY allowed to shift started and this action is NOT started then exit
    if (aShiftOnlyStarted) and (not(afStarted in fFlags)) then
        EXIT;
    if (fStartTime = 0) or (fEndTime = 0) then
        Exit;

    if aShiftType = stForward then
    begin
        // if started we should NOT shift the start time
        if not(afStarted in fFlags) then
            fStartTime := fStartTime + aShiftBy;

        fEndTime := fEndTime + aShiftBy;
    end
    else if aShiftType = stBackward then
    begin
        fStartTime := fStartTime - aShiftBy;
        fEndTime := fEndTime - aShiftBy;
    end
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedAction.UpdateTimes;
// --------------------------------------------------------------------------------------------------
// Update this Actions's Start and End Time fields with the start and end times that have
// been calculated by the Scheduler for this action
// --------------------------------------------------------------------------------------------------
var
    xStartTime, xDuration, xStatus: integer;
begin
    fStartTime := 0;
    fEndTime := 0;
    if (afNoSchedule in fFlags) then
        EXIT;

    FScheduler.GetActionScheduleTimes(fID, xStartTime, xDuration, xStatus);
    if (xStartTime <= 0) and (xDuration <= 0) then
        EXIT;
    fStartTime := xStartTime;
    fEndTime := fStartTime + cardinal(xDuration);
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedAction.Save(aNumTimesScheduled: integer);
// --------------------------------------------------------------------------------------------------
// Add this Action to the Scheduler object.
// Save all of the resources for this action to the Scheduler as well
// If the Enabled flag is false, then dont do any of the above
// --------------------------------------------------------------------------------------------------
begin
    if (afNoSchedule in FFlags) then
        Exit;
    if aNumTimesScheduled = 0 then
    begin
        FScheduler.AddAction(FID, FName, FProcessID, FSharedID, FMinLength, FMaxLength, 0, 0);
    end
    else
    begin
        // FScheduler.UpdateAction( FID, FMinLength, MaxLength );
    end;

    SaveResources(aNumTimesScheduled);
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedAction.SaveResources(aNumTimesScheduled: integer);
// --------------------------------------------------------------------------------------------------
// For each resource usage in the ResUsageList of this Action, add the resource usage to the Scheduler
// --------------------------------------------------------------------------------------------------
var
    i: integer;
    resUsage: TSchedResUsage;
begin
    for i := 0 to FResUsageList.Count - 1 do
    begin
        resUsage := FResUsageList[i];
        resUsage.Save(aNumTimesScheduled);
    end;
end;

// --------------------------------------------------------------------------------------------------
function TSchedAction.NextAvailResUsageID: cardinal;
// --------------------------------------------------------------------------------------------------
// Get a unique ID that is not used by any other Resource Usage in this Action
// --------------------------------------------------------------------------------------------------
begin
    result := FResUsageList.Count;
end;

// --------------------------------------------------------------------------------------------------
function TSchedAction.AppendToResUsageList(ResourceID: integer; UseLevel, Releaselevel: SmallInt): cardinal;
// --------------------------------------------------------------------------------------------------
// Create a new resource usage object.  Add it to the ResUsageList of this action
// --------------------------------------------------------------------------------------------------
var
    SchedResUsage: TSchedResUsage;
    resUsageID: cardinal;

begin
    resUsageID := NextAvailResUsageID;
    SchedResUsage := TSchedResUsage.Create(FScheduler, FID, ResourceID, UseLevel, ReleaseLevel);
    FResUsageList.Add(SchedResUsage);
    result := resUsageID;
end;

// --------------------------------------------------------------------------------------------------
function TSchedAction.GetResourceUsageCount: integer;
// --------------------------------------------------------------------------------------------------
var
    x: smallint;
begin
    FScheduler.GetActionResourceUseCount(FID, x);
    result := x;
end;

// --------------------------------------------------------------------------------------------------
function TSchedAction.GetRealStartTime(): cardinal;
// --------------------------------------------------------------------------------------------------
// Get the Start time of this action from the corresponding action in the Scheduler object.
// --------------------------------------------------------------------------------------------------
var
    xStartAsInt, xDurationAsInt: integer;
begin
    result := 0;
    FScheduler.GetActionRealTimes(FID, xStartAsInt, xDurationAsInt);
    if xStartAsInt > 0 then
        result := cardinal(xStartAsInt);
end;

// --------------------------------------------------------------------------------------------------
function TSchedAction.GetRealEndTime(): cardinal;
// --------------------------------------------------------------------------------------------------
// Get the End time of this action from the corresponding action in the Scheduler object.
// --------------------------------------------------------------------------------------------------
var
    xStartAsInt, xDurationAsInt: integer;
begin
    result := 0;
    FScheduler.GetActionRealTimes(FID, xStartAsInt, xDurationAsInt);
    if (xStartAsInt > 0) and (xDurationAsInt > 0) then
        result := cardinal(xStartAsInt) + cardinal(xDurationAsInt);
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedAction.SetRealStartTime(aStart: cardinal);
// --------------------------------------------------------------------------------------------------
// set the Start time of the action that corresponds to this action in the Scheduler object.
// --------------------------------------------------------------------------------------------------
begin
    FScheduler.DoAdjustActionData(FID, 0, integer(aStart));
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedAction.SetRealEndTime(aEnd: cardinal);
// --------------------------------------------------------------------------------------------------
// set the End time of the action that corresponds to this action in the Scheduler object.
// --------------------------------------------------------------------------------------------------
begin

    FScheduler.DoAdjustActionData(FID, 1, integer(aEnd - GetRealStartTime()));
end;

procedure TSchedAction.SetDuration(aDuration: cardinal);
begin
    FScheduler.DoAdjustActionData(FID, 2, integer(aDuration));
    UpdateTimes;
end;

// --------------------------------------------------------------------------------------------------
function TSchedAction.GetResUsageByIndex(aCrdIndex: cardinal): TSchedResUsage;
// --------------------------------------------------------------------------------------------------
begin
    result := nil;
    if aCrdIndex >= cardinal(FResUsageList.Count) then
        Exit;
    result := TSchedResUsage(FResUsageList[aCrdIndex]);
end;

function TSchedAction.GetStartTime(): cardinal;
var
    xRealStartTime: cardinal;
begin
    xRealStartTime := self.RealStartTime;
    if xRealStartTime > 0 then
        result := xRealStartTime
    else
        result := fStartTime;
end;

function TSchedAction.GetEndTime(): cardinal;
var
    xRealEndTime: cardinal;
begin
    xRealEndTime := self.RealEndTime;
    if xRealEndTime > 0 then
        result := xRealEndTime
    else
        result := fEndTime;
end;

// ***************************************TSchedResUsage*********************************************
// --------------------------------------------------------------------------------------------------
constructor TSchedResUsage.Create(Scheduler: TScheduler; ActionID, ResourceID: cardinal;
    const UseLevel, Releaselevel: SmallInt);
// --------------------------------------------------------------------------------------------------
begin
    inherited Create;
    FScheduler := Scheduler;
    FActionID := ActionID;
    FResourceID := ResourceID;
    FUseLevel := UseLevel;
    FReleaseLevel := ReleaseLevel;
    FAcquired := false;

end;

// --------------------------------------------------------------------------------------------------
destructor TSchedResUsage.Destroy;
// --------------------------------------------------------------------------------------------------
begin
    inherited Destroy;
end;

// --------------------------------------------------------------------------------------------------
procedure TSchedResUsage.Save(aNumTimesScheduled: integer);
// --------------------------------------------------------------------------------------------------
// Add this resource usage to the Scheduler object
// --------------------------------------------------------------------------------------------------
begin
    FScheduler.AddResourceUse(FActionID, FResourceID, FUseLevel, FReleaseLevel);
end;

// ****************************************TScheduler************************************************
// --------------------------------------------------------------------------------------------------
class function TScheduler.Create(AOwner: TComponent): TScheduler;
// --------------------------------------------------------------------------------------------------
begin
    try
        result := inherited Create(AOwner);
    except
        raise ENoSchedulerObject.CreateFmt('TScheduler.Create -> could not create %s object' +
            #13#10'Make sure the %s.ocx is registered', [STR_SCHEDULER_OCX_NAME, STR_SCHEDULER_OCX_NAME]);
    end;
end;

procedure TScheduler.OpenSession(aSessionName: string; aMaxPriority: cardinal);
var
    xSessionName: string;
begin
    SetDefaultValues;
    fMaxPriority := aMaxPriority;
    xSessionName := 'Session';
    if aSessionName <> '' then
        xSessionName := aSessionName;
    inherited OpenSession(xSessionName);
end;

// --------------------------------------------------------------------------------------------------
destructor TScheduler.Destroy;
// --------------------------------------------------------------------------------------------------
begin
    self.CloseSession;
    inherited Destroy;
end;

// --------------------------------------------------------------------------------------------------
procedure TScheduler.SetDefaultValues;
// --------------------------------------------------------------------------------------------------
begin
    TimeUnit := 1000;
    TextOutput := false;
    GraphicOutput := false;
    LogOutputFile := '.\SCHEDULE.TXT';
    CheckLevel := 0;
    OptComplexity := ocAdvanced;
    OptSamePriority := osSimple; // osDispatch;
    Flag7 := 0;
end;

function TScheduler.DoAdjustActionData(aActionID: Integer; aAdjustType: Integer; aValue: cardinal): boolean;
var
    xShiftTime, xMinTime: integer;
    xShiftTimeAsCard: cardinal;
    xShiftType: TSchedShiftType;
begin
    result := false;
    AdjustActionData(aActionID, aAdjustType, integer(aValue), xMinTime, xShiftTime);
    if xShiftTime = 0 then
        EXIT;

    result := true;
    xShiftTimeAsCard := cardinal(Abs(xShiftTime));
    if xShiftTime > 0 then
        xShiftType := sstShift
    else
        xShiftType := sstUnshift;

    self.fOnSessionShift(aActionID, xMinTime, xShiftTimeAsCard, xShiftType);

end;

// --------------------------------------------------------------------------------------------------
procedure TScheduler.AddResource(const ID: cardinal; const name: string; const Sharelevel: SmallInt);
// --------------------------------------------------------------------------------------------------
begin
    inherited AddResource(ID, name, Sharelevel);
end;

// --------------------------------------------------------------------------------------------------
function TScheduler.readResourceCount: Cardinal;
// --------------------------------------------------------------------------------------------------
begin
    FCount := 0;
    GetResourceCount(FCount);
    result := FCount;
end;

// --------------------------------------------------------------------------------------------------
function TScheduler.readProcessCount: Cardinal;
// --------------------------------------------------------------------------------------------------
begin
    FCount := 0;
    GetProcessCount(FCount);
    result := FCount;
end;

// --------------------------------------------------------------------------------------------------
function TScheduler.readActionCount: Cardinal;
// --------------------------------------------------------------------------------------------------
begin
    FCount := 0;
    GetActionCount(FCount);
    result := FCount;
end;

// --------------------------------------------------------------------------------------------------
function TScheduler.readSessionDuration: Cardinal;
// --------------------------------------------------------------------------------------------------
var
    Duration: Integer;
begin
    Duration := 0;
    GetSessionDuration(Duration);
    result := Duration;
end;

function TScheduler.GetInternalPriority(aPriority: cardinal): cardinal;
begin
    result := fMaxPriority - aPriority;
end;

function TScheduler.AddProcess(ProcessId: Integer; const name: WideString; Priority: Smallint;
    Color: Smallint): Integer;
begin
    result := inherited AddProcess(ProcessID, name, GetInternalPriority(Priority), Color);
end;

// --------------------------------------------------------------------------------------------------
function TScheduler.ReadFlag1: TSchedOptComplexity;
// --------------------------------------------------------------------------------------------------
begin
    result := TSchedOptComplexity(Flag1);
end;

// --------------------------------------------------------------------------------------------------
procedure TScheduler.SetFlag1(Flag: TSchedOptComplexity);
// --------------------------------------------------------------------------------------------------
begin
    Flag1 := integer(Flag);
end;

// --------------------------------------------------------------------------------------------------
function TScheduler.ReadFlag2: TSchedOptSamePriority;
// --------------------------------------------------------------------------------------------------
begin
    result := TSchedOptSamePriority(Flag2);
end;

// --------------------------------------------------------------------------------------------------
procedure TScheduler.SetFlag2(Flag: TSchedOptSamePriority);
// --------------------------------------------------------------------------------------------------
begin
    Flag2 := integer(Flag);
end;


end.
