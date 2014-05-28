{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------- --------  ----------------------------------------------------------
  06.11.08 pk                                        TN4280    Initial Revision
  10.11.08 pk  DoThreadStartStep                     TN4280    new
  10.11.08 pk  PrepareCallMethod                     TN4279    Now also passes non-string params
  10.11.08 pk                                        TN4279    More logging
  17.11.08 pk                                        TN4280    ResourceAcquire/Release moved to Actions
  19.11.08 pk DoThreadStartStep                      TN4280    moved to TThreadStartRunAction
  09.12.08 pk PrepareCallMethod                      TN4279    made class function
  09.12.08 pk ProcessNext                            TN4279    case for ADDM step moved to AddMethodRunAction
  10.12.08 pk MarkStopAddress                        TN4279    New
  17.02.09 pk                                        TN4232    Various changes for Run Trace
  24.02.09 pk PrepareCallMethod                      TN4232    Call TraceManager.AddCallStackFrame
  25.02.09 pk PrepareCallMethod                      TN4279    also handles case of setting global vars for main method
  04.03.09 pk                                        TN4232    uses GeneralTypes
  16.03.09 pk GetCurrentSubMethodName                TN4470    New
  04.02.10 pk                                        TN4972    Changes for Restart
  07.05.10 pk PrepareCallMethod                      TN5092    aParams is now TArgValueList instead of KeyValueArray
  19.05.10 pk PrepareCallMethod                      TN5092    call AddCallStackFrameForCurrentThread before adding params
  07.06.10 pk ClearPendingSteps                      TN5077    call tracemanager with PendingRunSteps as argument
  15.11.10 pk                                        TN5340    Changes to prevent memory leak
  01.03.12 wl                                        TN5822   uses geändert
  25.06.13 wl  FinalizeMethod                        TN6178   schreibt Rückgabewerte in die aufrufende Methode zurück
  25.06.13 wl  WriteReturnKeyValues                  TN6178   neu: wird von RETURN-Action verwendet
  25.06.13 wl  PrepareCallMethod                     TN6178   schreibt "Return variable names" von ADDM action
  14.08.13 wl                                        TN6218   verwendet TRunStepListIterator
  15.08.13 wl                                        TN6223   uses geändert
  01.10.13 wl                                        TN6251   Rückgabewerte für Threads vorbereitet
  ----------------------------------------------------------------------------------------------------------------------- }

unit RunStepBuilderProcessor;


interface


uses
    GeneralTypes,
    ThreadClasses,
    MemoryClasses,
    RunStepBuilder,
    RunStep,
    Streamable,
    MethodTypes,
    MethodCompiledFile,
    ParserEvalTable,
    ParserIdentDataType;

type
    TSetReturnKeyValuesEvent = procedure(aSender: TObject; aReturnKeyValues: TArray<TStreamableItem>)
        of object;

    TRunStepBuilderProcessor = class(TProcessor)
    private
        fCallStack: TCallStack;
        fProgCounter: TRelativeMemAddress;
        fProg: TMethodProgramCode;
        fStopAddress: TRelativeMemAddress;
        fCurrentStep: TRunStepByMethodStepBuilder;
        fPendingRunSteps: TRunStepList;
        fPendingRunStepsIterator: TRunStepListLinearIterator;
        fOnSetReturnKeyValues: TSetReturnKeyValuesEvent;

        function GetProgramCounter: TRelativeMemAddress;

        function GetProgram: TMethodProgramCode;

        function GetCallStack(): TCallStack;
        function FetchStep(): TRunStepByMethodStepBuilder;
        function IsEOF: boolean;
        function IsBOF: boolean;

        property CallStack: TCallStack read GetCallStack;
        property ProgCounter: TRelativeMemAddress read GetProgramCounter;
        property Prog: TMethodProgramCode read GetProgram;
        procedure FinalizeMethod;
        procedure InitializeMethod;
        function IsExternalError: boolean;
        function IsStopAddress(): boolean;
        procedure IncrementProgCounter();
        procedure Fetch();
        class procedure WriteProgramCounterToFile(const aProgCounter: TRelativeMemAddress);

        class function GetCurrentThreadProgramCounter: TRelativeMemAddress;
    public
        constructor Create(aOnSetReturnKeyValues: TSetReturnKeyValuesEvent);
        destructor Destroy(); override;

        function ProcessNext(): boolean; override;
        procedure MarkStopAddress();
        function IsPendingSteps: boolean;
        procedure AddStepsToPendingSteps(const aRunSteps: TRunStepList; const aTrace: boolean);
        procedure ClearPendingSteps();
        procedure PendingStepsIteratorPosChanged();
        procedure PendingStepsMoveCursor(const aCursorPos: integer);
        property CurrentStep: TRunStepByMethodStepBuilder read fCurrentStep;
        property PendingRunStepsIterator: TRunStepListLinearIterator read fPendingRunStepsIterator;

        class procedure PrepareCallMethod(const aMethodName: string; const aParams: TKeyArgValueList;
            const aIsEvent: boolean); overload;
        class procedure PrepareCallMethod(const aMethodName: string; const aParams: TKeyArgValueList;
            const aIsEvent: boolean; const aReturnKeyNames: TArray<string>); overload;
        class procedure JumpLocal(const aAddress: integer);
        class procedure Jump(const aLabelName: string; const aAddress: integer);
        class procedure GetCurrentProgramCounterInfo(out oLabelName: string; out oAddress: integer);
        class function GetCurrentSubMethodName(): string;
        class procedure AddStepsToPendingStepsForCurrentThread(const aRunSteps: TRunStepList;
            const aTrace: boolean);
        class procedure AssignProcessorForCurrentThread(const aProcessor: TProcessor);
        class function GetProcessorForCurrentThread(): TProcessor;
        class procedure PendingStepsMoveCursorForCurrentThread(const aCursorPos: integer);
        class procedure WriteReturnKeyValues(const aValues: TArray<TStreamableItem>);
    end;


implementation


uses
    SysUtils,
    LogManager,
    Identifier,
    TypeMapTranslator,
    MathUtils,
    ErrorManager,
    ParserIdentifier,
    ResourceManager,
    ProcessRegistry,
    ThreadAPI,
    RunTraceManager;

{ TRunStepBuilderProcessor }

constructor TRunStepBuilderProcessor.Create(aOnSetReturnKeyValues: TSetReturnKeyValuesEvent);
begin
    inherited Create();
    fCurrentStep := nil;
    fOnSetReturnKeyValues := aOnSetReturnKeyValues;

    fStopAddress := TRelativeMemAddress.Create();

    // becareful, will own and destroy any steps added to it
    fPendingRunSteps := TRunStepList.Create(true);
    fPendingRunStepsIterator := TRunStepListLinearIterator.Create(fPendingRunSteps);
end;

destructor TRunStepBuilderProcessor.Destroy();
begin
    fPendingRunStepsIterator.Free;
    fPendingRunSteps.Free;

    fStopAddress.Free;

    inherited;
end;

function TRunStepBuilderProcessor.GetCallStack(): TCallStack;
var
    xThreadImage: TThreadImage;
begin
    if not Assigned(fCallStack) then
    begin
        xThreadImage := TThreadAPI.GetCurrentThreadImage();
        fCallStack := xThreadImage.CallStack;
    end;
    result := fCallStack;
end;

procedure TRunStepBuilderProcessor.InitializeMethod();
begin

end;

procedure TRunStepBuilderProcessor.FinalizeMethod();
var
    xCurrentStackFrame: TCallStackFrame;
    xExitingMethName: string;
    x: integer;
    xReturnKeyNames: TArray<string>;
    xReturnKeyValues: TArray<TStreamableItem>;
begin
    xCurrentStackFrame := self.CallStack.CurrentFrame;
    ASSERT(Assigned(xCurrentStackFrame));
    xExitingMethName := self.ProgCounter.LabelName;

    self.ProgCounter.LabelName := xCurrentStackFrame.ReturnAddress.LabelName;
    self.ProgCounter.RelativeAddress := xCurrentStackFrame.ReturnAddress.RelativeAddress;
    xReturnKeyNames := xCurrentStackFrame.ReturnKeyNames.ToArray;
    xReturnKeyValues := xCurrentStackFrame.ReturnKeyValues.ToArray;
    xCurrentStackFrame.ReturnKeyNames.Clear;
    xCurrentStackFrame.ReturnKeyValues.Clear;

    TRunTraceManager.Instance.RemoveCurrentCallStackFrameForCurrentThread();

    self.CallStack.Pop();

    // Jetzt wieder in der alten Methode: Zurückschreiben der Rückgabewerte
    if not Assigned(fOnSetReturnKeyValues) or (Length(xReturnKeyNames) > 0) then
    begin
        for x := 0 to high(xReturnKeyValues) do
        begin
            if x > high(xReturnKeyNames) then
                xReturnKeyValues[x].Free // löschen wenn nicht gebraucht
            else
                TMethodEvalTable.SetIdentValRaw(xReturnKeyNames[x], xReturnKeyValues[x]);
        end;
    end
    else
    begin
        fOnSetReturnKeyValues(self, xReturnKeyValues);
    end;
end;

class procedure TRunStepBuilderProcessor.WriteReturnKeyValues(const aValues: TArray<TStreamableItem>);
var
    xCurrentStackFrame: TCallStackFrame;
    xThreadImage: TThreadImage;
    x: integer;
begin
    xThreadImage := TThreadAPI.GetCurrentThreadImage();
    xCurrentStackFrame := xThreadImage.CallStack.CurrentFrame;
    ASSERT(Assigned(xCurrentStackFrame));

    for x := 0 to high(aValues) do
    begin
        xCurrentStackFrame.ReturnKeyValues.Add(aValues[x]);
    end;
end;

function TRunStepBuilderProcessor.IsExternalError(): boolean;
begin
    // if is global error raise exception
    result := gErrorManager.IsGlobalErr;
end;

function TRunStepBuilderProcessor.IsPendingSteps: boolean;
begin
    result := not fPendingRunStepsIterator.IsEOF;
end;

procedure TRunStepBuilderProcessor.AddStepsToPendingSteps(const aRunSteps: TRunStepList;
    const aTrace: boolean);
begin
    fPendingRunSteps.Clear();
    fPendingRunStepsIterator.MoveFirst;

    fPendingRunSteps.AddSteps(aRunSteps);
    if aTrace then
        TRunTraceManager.Instance.AddPendingSteps(aRunSteps);
end;

procedure TRunStepBuilderProcessor.ClearPendingSteps();
begin
    TRunTraceManager.Instance.ClearPendingSteps(fPendingRunSteps);

    fPendingRunSteps.Clear();
    fPendingRunStepsIterator.MoveFirst;
end;

procedure TRunStepBuilderProcessor.PendingStepsIteratorPosChanged();
begin
    TRunTraceManager.Instance.PendingStepsIteratorPosChanged(fPendingRunStepsIterator);
end;

procedure TRunStepBuilderProcessor.PendingStepsMoveCursor(const aCursorPos: integer);
begin
    fPendingRunStepsIterator.DataCursor := aCursorPos;
end;

procedure TRunStepBuilderProcessor.Fetch();
begin
    fCurrentStep := FetchStep();
    IncrementProgCounter();
end;

function TRunStepBuilderProcessor.ProcessNext: boolean;
begin
    // result := true;

    // ClearPendingSteps();
    // fPendingRunStepsIterator.MoveFirst();

    fCurrentStep := nil;

    while true do
    begin

        if self.CallStack.IsEmpty or IsStopAddress() then
        begin
            result := false;
            EXIT;
        end;

        if self.IsBOF then
        begin
            InitializeMethod();
            IncrementProgCounter();
        end;

        if IsExternalError() then
        begin
            result := false;
            EXIT;
        end;

        if self.IsEOF then
        begin
            FinalizeMethod();
            CONTINUE;
        end;

        Fetch();
        if not Assigned(fCurrentStep) then
        begin
            CONTINUE;
        end;

        result := true;
        EXIT;

    end;
end;

function TRunStepBuilderProcessor.FetchStep: TRunStepByMethodStepBuilder;
begin
    result := self.Prog.CompiledCode.FindLineByAddress(self.ProgCounter);
end;

function TRunStepBuilderProcessor.IsBOF: boolean;
begin
    result := self.Prog.CompiledCode.IsBOFAddress(self.ProgCounter);
end;

function TRunStepBuilderProcessor.IsEOF: boolean;
begin
    result := self.Prog.CompiledCode.IsEOFAddress(self.ProgCounter);
end;

procedure TRunStepBuilderProcessor.IncrementProgCounter();
begin
    self.ProgCounter.Increment();
    WriteProgramCounterToFile(self.ProgCounter);
end;

function TRunStepBuilderProcessor.GetProgramCounter: TRelativeMemAddress;
begin
    if not Assigned(fProgCounter) then
    begin
        fProgCounter := GetCurrentThreadProgramCounter();
    end;

    result := fProgCounter;
end;

function TRunStepBuilderProcessor.GetProgram: TMethodProgramCode;
var
    xProcess: TProcess;
begin
    if not Assigned(fProg) then
    begin
        xProcess := TThreadAPI.GetCurrentProcess();
        fProg := xProcess.AddressSpace.ProgramCode as TMethodProgramCode;
    end;
    result := fProg;
end;

procedure TRunStepBuilderProcessor.MarkStopAddress;
begin
    fStopAddress.LabelName := self.ProgCounter.LabelName;
    fStopAddress.RelativeAddress := self.ProgCounter.RelativeAddress;
end;

function TRunStepBuilderProcessor.IsStopAddress(): boolean;
begin
    result := (fStopAddress.RelativeAddress <> fStopAddress.BOFAddress) and
        SameText(fStopAddress.LabelName, self.ProgCounter.LabelName) and
        (fStopAddress.RelativeAddress = self.ProgCounter.RelativeAddress);
end;

class procedure TRunStepBuilderProcessor.WriteProgramCounterToFile(const aProgCounter: TRelativeMemAddress);
begin
    TRunTraceManager.Instance.CurrentProgramCounterChanged(aProgCounter);
end;

class function TRunStepBuilderProcessor.GetCurrentThreadProgramCounter: TRelativeMemAddress;
var
    xThreadImage: TThreadImage;
begin
    xThreadImage := TThreadAPI.GetCurrentThreadImage();
    result := xThreadImage.TCB.ProgramCounter;
end;

class procedure TRunStepBuilderProcessor.GetCurrentProgramCounterInfo(out oLabelName: string;
    out oAddress: integer);
var
    xProgCounter: TRelativeMemAddress;
begin
    xProgCounter := GetCurrentThreadProgramCounter();
    oLabelName := xProgCounter.LabelName;
    oAddress := xProgCounter.RelativeAddress;
end;

class function TRunStepBuilderProcessor.GetCurrentSubMethodName: string;
var
    xDummy: integer;
begin
    GetCurrentProgramCounterInfo(result, xDummy);
end;

class procedure TRunStepBuilderProcessor.Jump(const aLabelName: string; const aAddress: integer);
var
    xProgCounter: TRelativeMemAddress;
begin
    xProgCounter := GetCurrentThreadProgramCounter();
    xProgCounter.LabelName := aLabelName;
    xProgCounter.RelativeAddress := aAddress;
    WriteProgramCounterToFile(xProgCounter);
end;

class procedure TRunStepBuilderProcessor.JumpLocal(const aAddress: integer);
var
    xProgCounter: TRelativeMemAddress;
begin
    xProgCounter := GetCurrentThreadProgramCounter();
    xProgCounter.RelativeAddress := aAddress;
    WriteProgramCounterToFile(xProgCounter);
end;

class procedure TRunStepBuilderProcessor.PrepareCallMethod(const aMethodName: string;
    const aParams: TKeyArgValueList; const aIsEvent: boolean);
begin
    PrepareCallMethod(aMethodName, aParams, aIsEvent, nil);
end;

class procedure TRunStepBuilderProcessor.PrepareCallMethod(const aMethodName: string;
    const aParams: TKeyArgValueList; const aIsEvent: boolean; const aReturnKeyNames: TArray<string>);
var
    x: integer;
    xCurrentStackFrame: TCallStackFrame;
    xIdent: TIdentifier;
    xThreadImage: TThreadImage;
    xCallStack: TCallStack;
    xProgCounter: TRelativeMemAddress;
    xIsLocalKey: boolean;
    xIsMainMethod: boolean;
begin
    xThreadImage := TThreadAPI.GetCurrentThreadImage();
    xCallStack := xThreadImage.CallStack;
    xProgCounter := xThreadImage.TCB.ProgramCounter;

    xIsMainMethod := (not aIsEvent) and xCallStack.IsEmpty;

    // add new stackframe
    xCallStack.Push();
    xCurrentStackFrame := xCallStack.CurrentFrame;
    ASSERT(Assigned(xCurrentStackFrame));

    // save return address
    xCurrentStackFrame.ReturnAddress.LabelName := xProgCounter.LabelName;
    xCurrentStackFrame.ReturnAddress.RelativeAddress := xProgCounter.RelativeAddress;
    xCurrentStackFrame.ReturnKeyNames.AddRange(aReturnKeyNames);

    TRunTraceManager.Instance.AddCallStackFrameForCurrentThread(xCurrentStackFrame);

    // add method params to stack frame
    if Assigned(aParams) then
    begin
        for x := 0 to aParams.Count - 1 do
        begin
            xIsLocalKey := (TParserIdentifier.GetIdentType(aParams[x].Key) = itLocal);

            if (not xIsMainMethod) and (not xIsLocalKey) then
                raise Exception.Create('Cannot pass global variable "' + aParams[x].Key + '" to a submethod');

            if xIsLocalKey then
                xIdent := TIdentifier.Create(aParams[x].Key, false)
            else
                xIdent := TMethodEvalTable.FindOrCreateIdent(aParams[x].Key);

            TMethodEvalTable.SetIdentValRaw(xIdent.Key, aParams[x].Value);

            // if the key is a global var don't store this in Callstack
            if xIsLocalKey then
                xCurrentStackFrame.CallArgs.Add(xIdent);
        end;
    end;
    // set jump address
    Jump(aMethodName, TRelativeMemAddress.BOFAddress);
end;

class procedure TRunStepBuilderProcessor.AddStepsToPendingStepsForCurrentThread(const aRunSteps: TRunStepList;
    const aTrace: boolean);
var
    xThreadImage: TThreadImage;
begin
    xThreadImage := TThreadAPI.GetCurrentThreadImage();
    (xThreadImage.TCB.Processor as TRunStepBuilderProcessor).AddStepsToPendingSteps(aRunSteps, aTrace);
end;

class function TRunStepBuilderProcessor.GetProcessorForCurrentThread(): TProcessor;
var
    xThreadImage: TThreadImage;
begin
    xThreadImage := TThreadAPI.GetCurrentThreadImage();
    result := xThreadImage.TCB.Processor;
end;

class procedure TRunStepBuilderProcessor.AssignProcessorForCurrentThread(const aProcessor: TProcessor);
var
    xThreadImage: TThreadImage;
begin
    xThreadImage := TThreadAPI.GetCurrentThreadImage();
    xThreadImage.TCB.Processor := aProcessor;
end;

class procedure TRunStepBuilderProcessor.PendingStepsMoveCursorForCurrentThread(const aCursorPos: integer);
var
    xThreadImage: TThreadImage;
begin
    xThreadImage := TThreadAPI.GetCurrentThreadImage();
    (xThreadImage.TCB.Processor as TRunStepBuilderProcessor).PendingStepsMoveCursor(aCursorPos);
end;


end.
