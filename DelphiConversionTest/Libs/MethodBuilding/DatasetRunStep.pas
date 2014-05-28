unit DatasetRunStep;
{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk                                        TN4232    Initial Revision
  20.02.09 wl  ..RunStep.DoReadData,DoWriteData      TN4438    entfernt
  13.05.09 wl  TDatasetOpenRunStep                   TN4564    neu property SourceFileName
  25.06.10 wl  TDatasetOpenRunStep                   TN5161    OrderBy ermöglich zusätzliche Sortierung
  ----------------------------------------------------------------------------------------------------------------------- }


interface


uses
    Classes,
    RunStepInfo,
    RunStepInfoTypeInfo,
    RunStep,
    RunStepTypeInfo;

type
    TDatasetOpenRunStep = class(TRunStep)
    private
        fDatasetObjectIdentName: string;
        fDefName: string;
        fFilter: string;
        fRecordCountIdentName: string;
        fSourceFileName: string;
        fOrderBy: string;
    protected
        function DoGetDescription(): string; override;
    published
        property DatasetObjectIdentName: string read fDatasetObjectIdentName write fDatasetObjectIdentName;
        property DefName: string read fDefName write fDefName;
        property Filter: string read fFilter write fFilter;
        property RecordCountIdentName: string read fRecordCountIdentName write fRecordCountIdentName;
        property SourceFileName: string read fSourceFileName write fSourceFileName;
        property OrderBy: string read fOrderBy write fOrderBy;
    end;

    TDatasetOpenRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TDatasetOpenRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TDatasetCloseRunStep = class(TRunStep)
    protected
        fDatasetObject: integer;
        function DoGetDescription(): string; override;
    published
        property DatasetObject: integer read fDatasetObject write fDatasetObject;
    end;

    TDatasetCloseRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TDatasetCloseRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TDatasetCursorMoveRunStep = class(TRunStep)
    protected
        fDatasetObject: integer;
        fIsRelativeMove: boolean;
        fMoveOffset: integer;
        fRecordNumIdentName: string;
        function DoGetDescription(): string; override;
    published
        property DatasetObject: integer read fDatasetObject write fDatasetObject;
        property IsRelativeMove: boolean read fIsRelativeMove write fIsRelativeMove;
        property MoveOffset: integer read fMoveOffset write fMoveOffset;
        property RecordNumIdentName: string read fRecordNumIdentName write fRecordNumIdentName;
    end;

    TDatasetCursorMoveRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TDatasetCursorMoveRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TDatasetReadRunStep = class(TRunStep)
    protected
        fDatasetObject: integer;
        function DoGetDescription(): string; override;
    published
        property DatasetObject: integer read fDatasetObject write fDatasetObject;
    end;

    TDatasetReadRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TDatasetReadRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    MethodGUIParsing,
    DatasetRunStepInfo;

{ TDatasetOpenRunStep }

function TDatasetOpenRunStep.DoGetDescription: string;
begin
    result := Format('Dataset Open, Definition Name: %s, Filter: %s, Sort: %s, Assign Object to: %s',
        [fDefName, fFilter, fOrderBy, fDatasetObjectIdentName]);
    if (fSourceFileName <> '') then
        result := result + ', Source file: ' + fSourceFileName;
end;

{ TDatasetOpenRunStepCreator }

function TDatasetOpenRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TDatasetOpenRunStep.Create();
end;

{ TDatasetOpenRunStepTypeInfo }

constructor TDatasetOpenRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDatasetOpen = '1.0.0';
    cStepTypeNameDatasetOpen = cActionNameDatasetOpen;
begin
    inherited Create(cStepTypeNameDatasetOpen, cStepTypeVersionDatasetOpen, aLibName, aLibVersion);
end;

procedure TDatasetOpenRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TDatasetOpenRunStepCreator.Create();
end;

{ TDatasetCloseRunStep }

function TDatasetCloseRunStep.DoGetDescription: string;
begin
    result := Format('Dataset Close, Object: %d', [fDatasetObject]);
end;

{ TDatasetCloseRunStepCreator }

function TDatasetCloseRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TDatasetCloseRunStep.Create();
end;

{ TDatasetCloseRunStepTypeInfo }

constructor TDatasetCloseRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDatasetClose = '1.0.0';
    cStepTypeNameDatasetClose = cActionNameDatasetClose;
begin
    inherited Create(cStepTypeNameDatasetClose, cStepTypeVersionDatasetClose, aLibName, aLibVersion);
end;

procedure TDatasetCloseRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TDatasetCloseRunStepCreator.Create();
end;

{ TDatasetCursorMoveRunStep }

function TDatasetCursorMoveRunStep.DoGetDescription: string;
var
    xCursorMoveTypeAsString: string;
begin
    if self.IsRelativeMove then
        xCursorMoveTypeAsString := 'Relative'
    else
        xCursorMoveTypeAsString := 'Absolute';

    result := Format('Dataset Move Cursor, Object: %d, Move type: %s, Offset: %d',
        [fDatasetObject, xCursorMoveTypeAsString, fMoveOffset]);
end;

{ TDatasetCursorMoveRunStepCreator }

function TDatasetCursorMoveRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TDatasetCursorMoveRunStep.Create();
end;

{ TDatasetCursorMoveRunStepTypeInfo }

constructor TDatasetCursorMoveRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDatasetCursorMove = '1.0.0';
    cStepTypeNameDatasetCursorMove = cActionNameDatasetCursorMove;
begin
    inherited Create(cStepTypeNameDatasetCursorMove, cStepTypeVersionDatasetCursorMove, aLibName,
        aLibVersion);
end;

procedure TDatasetCursorMoveRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TDatasetCursorMoveRunStepCreator.Create();
end;

{ TDatasetReadRunStep }

function TDatasetReadRunStep.DoGetDescription: string;
begin
    result := Format('Dataset Read, Object: %d', [fDatasetObject]);
end;

{ TDatasetReadRunStepCreator }

function TDatasetReadRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TDatasetReadRunStep.Create();
end;

{ TDatasetReadRunStepTypeInfo }

constructor TDatasetReadRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDatasetRead = '1.0.0';
    cStepTypeNameDatasetRead = cActionNameDatasetRead;
begin
    inherited Create(cStepTypeNameDatasetRead, cStepTypeVersionDatasetRead, aLibName, aLibVersion);
end;

procedure TDatasetReadRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TDatasetReadRunStepCreator.Create();
end;


end.
