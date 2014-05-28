unit PendingStepsDataCache;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.02.10 pk                                        TN4972    Initial Revision
  07.06.10 pk                                        TN5077    inherits from TTraceDataCache
  15.11.10 pk                                        TN5340     Changes to prevent memory leak
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    XMLReaderWriter,
    Streamable,
    RunStep,
    RelativeMemAddressData,
    TraceDataCache;

const
    cPendingStepsTypeID = 'PendingSteps';

type
    TPendingStepsData = class(TStreamable)
    private
        fSteps: TRunStepList;
        fCursorPos: integer;
    public
        constructor Create(); override;
        destructor Destroy(); override;
        procedure ResetCursorPos();
    published
        property Steps: TRunStepList read fSteps write fSteps;
        property CursorPos: integer read fCursorPos write fCursorPos;
    end;

    TPendingStepsDataCache = class(TTraceDataCache)
    private
        fPendingStepsData: TPendingStepsData;
        fReaderWriter: TXMLReaderWriter;
        procedure DataChanged;
    protected
        function GetTypeID: string; override;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Init(); override;
        procedure Clear; override;
        function read(): boolean; override;
        procedure Flush(); override;
        procedure SetPathName(const aValue: string); override;
        procedure AddStep(const aRunStep: TRunStep);
        procedure AddSteps(const aRunSteps: TRunStepList);
        procedure ClearSteps();
        procedure SetCursorPos(const aCursorPos: integer);
        property PendingStepsData: TPendingStepsData read fPendingStepsData;
    end;


implementation


uses
    SysUtils,
    Classes;

const
    cAutoFlushBufferCycle = 0;

    { TPendingStepsDataCache }

constructor TPendingStepsDataCache.Create();
begin
    inherited Create();
    fPendingStepsData := nil;
    fReaderWriter := TXMLReaderWriter.Create('', cAutoFlushBufferCycle);
end;

destructor TPendingStepsDataCache.Destroy;
begin
    fReaderWriter.Free;
    inherited;
end;

procedure TPendingStepsDataCache.Init();
begin
    fPendingStepsData := TPendingStepsData.Create();
    fReaderWriter.Activate();
    fReaderWriter.AddObjectToRootNode(fPendingStepsData);
end;

procedure TPendingStepsDataCache.DataChanged();
begin
    fReaderWriter.DataChanged();
    // fReaderWriter.SetObjectToVirtualObject( fPendingStepsData, fPendingStepsData.Tag );
end;

procedure TPendingStepsDataCache.Flush;
begin
    fReaderWriter.WriteToFile();
end;

function TPendingStepsDataCache.GetTypeID: string;
begin
    result := cPendingStepsTypeID;
end;

procedure TPendingStepsDataCache.AddStep(const aRunStep: TRunStep);
begin
    fPendingStepsData.Steps.AddStep(aRunStep);
    self.DataChanged();
end;

procedure TPendingStepsDataCache.AddSteps(const aRunSteps: TRunStepList);
begin
    fPendingStepsData.Steps.AddSteps(aRunSteps);
    self.DataChanged();
end;

procedure TPendingStepsDataCache.ClearSteps();
begin
    fPendingStepsData.Steps.Clear();
    fPendingStepsData.ResetCursorPos;
    self.DataChanged();
end;

procedure TPendingStepsDataCache.Clear;
begin
    FreeAndNil(fPendingStepsData);
    fReaderWriter.DisActivate();
end;

function TPendingStepsDataCache.Read(): boolean;
begin
    result := false;
    if not fReaderWriter.ReadFromFile() then
        EXIT;
    fPendingStepsData := fReaderWriter.CreateObjectFromRootNode<TPendingStepsData>();
    if not Assigned(fPendingStepsData) then
        EXIT;
    result := true;
end;

procedure TPendingStepsDataCache.SetCursorPos(const aCursorPos: integer);
begin
    fPendingStepsData.CursorPos := aCursorPos;
    self.DataChanged();
end;

procedure TPendingStepsDataCache.SetPathName(const aValue: string);
begin
    fReaderWriter.SetPathName(aValue);
end;

{ TPendingStepsData }

constructor TPendingStepsData.Create();
begin
    inherited Create();
    fSteps := TRunStepList.Create(true);
    self.ResetCursorPos;
end;

destructor TPendingStepsData.Destroy();
begin
    FreeAndNil(fSteps);
    inherited;
end;

procedure TPendingStepsData.ResetCursorPos;
begin
    fCursorPos := 0;
end;

// initialization
// RegisterClasses( [TPendingStepsData ] );


end.
