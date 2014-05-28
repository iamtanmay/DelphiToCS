{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk                                        TN4232      Initial Revision
  24.02.09 pk  ExecFirst                             TN4232      check TraceManager.Enabled
  30.03.09 pk  OpenDataset                           TN4491      New
  13.05.09 wl  TDatasetOpenRunAction.ExecFirst       TN4564    Aufruf von TDataExchangeDatasetWrapper mit Parameter SourceFileName
  08.09.09 pk                                        TN4753      uses ErrorMessage replaced by ErrorInfo
  04.02.10 pk                                        TN4972      Changes for Restart
  07.05.10 pk  ExecFirst                             TN5092      changes needed due to changes to SetIdentifierValue
  07.06.10 pk                                        TN5077      uses changed
  25.06.10 wl  ExecFirst                             TN5161    OrderBy ermöglich zusätzliche Sortierung
  01.03.12 wl                                        TN5822   uses geändert
  24.04.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit DatasetRunAction;


interface


uses
    RunStep,
    RunAction,
    RunActionTypeInfo,
    DatasetRunStep,
    DataExchangeDatasetWrapper;

type
    // --------------------------------------------------------------------------------------------------------- Open
    TDatasetOpenRunAction = class(TRunAction)
    private
        function GetRunStep: TDatasetOpenRunStep;
        function OpenDataset(const aDataset: TDataExchangeDatasetWrapper): boolean;
    public
        procedure ExecFirst(); override;
        property RunStep: TDatasetOpenRunStep read GetRunStep;
    end;

    TDatasetOpenRunActionCreator = class(TRunActionCreator)
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; override;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; override;
    end;

    TDatasetOpenRunActionTypeInfo = class(TRunActionTypeInfo)
    protected
        procedure DoCreateRunActionCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // --------------------------------------------------------------------------------------------------------- Close
    TDatasetCloseRunAction = class(TRunAction)
    private
        function GetRunStep: TDatasetCloseRunStep;
    public
        procedure ExecFirst(); override;
        property RunStep: TDatasetCloseRunStep read GetRunStep;
    end;

    TDatasetCloseRunActionCreator = class(TRunActionCreator)
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; override;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; override;
    end;

    TDatasetCloseRunActionTypeInfo = class(TRunActionTypeInfo)
    protected
        procedure DoCreateRunActionCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // ------------------------------------------------------------------------------------------------------ CursorMove
    TDatasetCursorMoveRunAction = class(TRunAction)
    private
        function GetRunStep: TDatasetCursorMoveRunStep;
    public
        procedure ExecFirst(); override;
        property RunStep: TDatasetCursorMoveRunStep read GetRunStep;
    end;

    TDatasetCursorMoveRunActionCreator = class(TRunActionCreator)
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; override;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; override;
    end;

    TDatasetCursorMoveRunActionTypeInfo = class(TRunActionTypeInfo)
    protected
        procedure DoCreateRunActionCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // ------------------------------------------------------------------------------------------------------ Read
    TDatasetReadRunAction = class(TRunAction)
    private
        function GetRunStep: TDatasetReadRunStep;
    public
        procedure ExecFirst(); override;
        property RunStep: TDatasetReadRunStep read GetRunStep;
    end;

    TDatasetReadRunActionCreator = class(TRunActionCreator)
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; override;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; override;
    end;

    TDatasetReadRunActionTypeInfo = class(TRunActionTypeInfo)
    protected
        procedure DoCreateRunActionCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    Generics.Collections,
    SysUtils,
    Windows,
    DatasetRunStepInfo,
    MethodCompiledFile,
    RunTraceManager,
    StandardRunEffectData,
    AppTypes,
    ErrorInfo,
    ErrorManager,
    ErrorMessageFactory;

{ TDatasetOpenRunActionCreator }

function TDatasetOpenRunActionCreator.GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := aRunStep is TDatasetOpenRunStep;
end;

function TDatasetOpenRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := TDatasetOpenRunAction.Create(aRunStep);
end;

{ TDatasetOpenRunAction }

function TDatasetOpenRunAction.GetRunStep: TDatasetOpenRunStep;
begin
    result := fRunStep as TDatasetOpenRunStep;
end;

function TDatasetOpenRunAction.OpenDataset(const aDataset: TDataExchangeDatasetWrapper): boolean;
var
    xButton: integer;
begin
    result := true;
    while true do
    begin
        if gErrorManager.IsGlobalErr() then
            BREAK;

        try
            aDataset.Open();
            BREAK;
        except
            on E: Exception do
            begin
                xButton := gErrorMessageFactory.ErrBoxSimple(E.Message, 'Dataset Open Error', eibAbortRetry);
                if (xButton = idAbort) then
                begin
                    gErrorManager.SetGlobalErr(ERR_APPLICATION);
                    result := false;
                    BREAK;
                end;
            end;
        end;
    end;

end;

procedure TDatasetOpenRunAction.ExecFirst();
var
    xDataset: TDataExchangeDatasetWrapper;
    xID: integer;
begin
    xDataset := TDataExchangeDatasetWrapper.Create(self.RunStep.DefName, self.RunStep.Filter,
        self.RunStep.OrderBy, self.RunStep.SourceFileName);
    if not OpenDataset(xDataset) then
    begin
        xDataset.Free;
        EXIT;
    end;

    xID := TMethodEvalTable.AllocateReference(xDataset);
    TMethodEvalTable.SetIdentValAsRef(self.RunStep.DatasetObjectIdentName, xID);
    TMethodEvalTable.SetIdentValAsInt(self.RunStep.RecordCountIdentName, xDataset.RecordCount);
end;

{ TDatasetOpenRunActionTypeInfo }

constructor TDatasetOpenRunActionTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDatasetOpen = '1.0.0';
    cStepTypeNameDatasetOpen = cActionNameDatasetOpen;
begin
    inherited Create(cStepTypeNameDatasetOpen, cStepTypeVersionDatasetOpen, aLibName, aLibVersion);
end;

procedure TDatasetOpenRunActionTypeInfo.DoCreateRunActionCreator;
begin
    fRunActionCreator := TDatasetOpenRunActionCreator.Create();

end;

// ------------------------------------------------------------------------------------------------------------- Close
{ TDatasetCloseRunActionCreator }

function TDatasetCloseRunActionCreator.GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := aRunStep is TDatasetCloseRunStep;
end;

function TDatasetCloseRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := TDatasetCloseRunAction.Create(aRunStep);
end;

{ TDatasetCloseRunAction }

function TDatasetCloseRunAction.GetRunStep: TDatasetCloseRunStep;
begin
    result := fRunStep as TDatasetCloseRunStep;
end;

procedure TDatasetCloseRunAction.ExecFirst();
begin
    // we assume that this function will close the dataset and free the object.
    TMethodEvalTable.DeallocateReference(self.RunStep.DatasetObject);
end;

{ TDatasetCloseRunActionTypeInfo }

constructor TDatasetCloseRunActionTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDatasetClose = '1.0.0';
    cStepTypeNameDatasetClose = cActionNameDatasetClose;
begin
    inherited Create(cStepTypeNameDatasetClose, cStepTypeVersionDatasetClose, aLibName, aLibVersion);
end;

procedure TDatasetCloseRunActionTypeInfo.DoCreateRunActionCreator;
begin
    fRunActionCreator := TDatasetCloseRunActionCreator.Create();
end;

// ------------------------------------------------------------------------------------------------------------- CursorMove
{ TDatasetCursorMoveRunActionCreator }

function TDatasetCursorMoveRunActionCreator.GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := aRunStep is TDatasetCursorMoveRunStep;
end;

function TDatasetCursorMoveRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := TDatasetCursorMoveRunAction.Create(aRunStep);
end;

{ TDatasetCursorMoveRunAction }

function TDatasetCursorMoveRunAction.GetRunStep: TDatasetCursorMoveRunStep;
begin
    result := fRunStep as TDatasetCursorMoveRunStep;
end;

procedure TDatasetCursorMoveRunAction.ExecFirst();
var
    xDataset: TDataExchangeDatasetWrapper;
    xOldCursorPos, xNewCursorPos: integer;
begin
    xDataset := TMethodEvalTable.FindObjectByReference(self.RunStep.DatasetObject)
        as TDataExchangeDatasetWrapper;
    xOldCursorPos := xDataset.CurrentRecordNo;
    xDataset.CursorMove(self.RunStep.IsRelativeMove, self.RunStep.MoveOffset);
    xNewCursorPos := xDataset.CurrentRecordNo;
    TMethodEvalTable.ReferenceChanged(self.RunStep.DatasetObject);
    TMethodEvalTable.SetIdentValAsInt(self.RunStep.RecordNumIdentName, xDataset.CurrentRecordNo);
    if TRunTraceManager.Instance.IsEnabled then
        TRunTraceManager.Instance.AddRunEffectDataToCurrentAction
            (TDatasetCursorMoveRunEffectData.Create(self.RunStep.DatasetObject, xOldCursorPos,
            xNewCursorPos));
end;

{ TDatasetCursorMoveRunActionTypeInfo }

constructor TDatasetCursorMoveRunActionTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDatasetCursorMove = '1.0.0';
    cStepTypeNameDatasetCursorMove = cActionNameDatasetCursorMove;
begin
    inherited Create(cStepTypeNameDatasetCursorMove, cStepTypeVersionDatasetCursorMove, aLibName,
        aLibVersion);
end;

procedure TDatasetCursorMoveRunActionTypeInfo.DoCreateRunActionCreator;
begin
    fRunActionCreator := TDatasetCursorMoveRunActionCreator.Create();
end;

// ----------------------------------------------------------------------------------------------------------------- Read
{ TDatasetReadRunActionCreator }

function TDatasetReadRunActionCreator.GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := aRunStep is TDatasetReadRunStep;
end;

function TDatasetReadRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := TDatasetReadRunAction.Create(aRunStep);
end;

{ TDatasetReadRunAction }

function TDatasetReadRunAction.GetRunStep: TDatasetReadRunStep;
begin
    result := fRunStep as TDatasetReadRunStep;
end;

procedure TDatasetReadRunAction.ExecFirst();
var
    xDataset: TDataExchangeDatasetWrapper;
    xList: TObjectList<TDataExchangeIdentItem>;
    x: integer;
begin
    xDataset := TMethodEvalTable.FindObjectByReference(self.RunStep.DatasetObject)
        as TDataExchangeDatasetWrapper;
    xList := TObjectList<TDataExchangeIdentItem>.Create();
    try
        xDataset.Read(xList);
        for x := 0 to xList.Count - 1 do
        begin
            TMethodEvalTable.SetIdentValRaw(xList[x].IdentName, xList[x].IdentValue);
        end;
    finally
        xList.Free;
    end;

    TMethodEvalTable.ReferenceChanged(self.RunStep.DatasetObject);
end;

{ TDatasetReadRunActionTypeInfo }

constructor TDatasetReadRunActionTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDatasetRead = '1.0.0';
    cStepTypeNameDatasetRead = cActionNameDatasetRead;
begin
    inherited Create(cStepTypeNameDatasetRead, cStepTypeVersionDatasetRead, aLibName, aLibVersion);
end;

procedure TDatasetReadRunActionTypeInfo.DoCreateRunActionCreator;
begin
    fRunActionCreator := TDatasetReadRunActionCreator.Create();
end;


end.
