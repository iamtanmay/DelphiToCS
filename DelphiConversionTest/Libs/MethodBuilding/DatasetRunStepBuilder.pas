unit DatasetRunStepBuilder;
{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk                                        TN4232    Initial Revision
  20.02.09 wl  TMethodStepSetting_...                TN4438    aDescription direkt angegeben statt mit aResNo
  08.05.09 wl  GetOptionSummary                      TN4555   Zuweisungs-Schreibweise für Rückgabeparameter: ... := ...
  13.05.09 wl  TMethodStepSetting_DatasetOpen        TN4564    neu property SourceFileName
  25.06.10 wl  TMethodStepSetting_DatasetOpen        TN5161    OrderBy ermöglich zusätzliche Sortierung
  11.03.11 wl                                        TN5500   Summary deutsch und englisch
  ----------------------------------------------------------------------------------------------------------------------- }


interface


uses
    Classes,
    RunStepInfo,
    RunStepInfoTypeInfo,
    MethodStep,
    RunStepBuilder,
    CustomSetting,
    MethodStepDataFields,
    RunStepBuilderHelper,
    CustomLeafSettings,
    RunStep,
    RunStepTypeInfo,
    AppTypes;

type
    TCustomSetting_DefName = class(TCustomLeafSetting)
    public
        constructor Create(const aKey, aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    // --------------------------------------------------------------------------------------------- Open
    TMethodStepSetting_DatasetOpen = class(TMethodStepCompositeSetting)
    private
        function GetDefName: TCustomSetting;
        function GetFilter: TCustomSetting;
        function GetDatasetObjectIdentName: TCustomSetting;
        function GetRecordCountIdentName: TCustomSetting;
        function GetSourceFileName: TCustomSetting;
        function GetOrderBy: TCustomSetting;
    protected
        function GetOptionSummary: string; override;
    public
        constructor Create(aOnAddEditFunctions: TNotifyEvent);
        property DefName: TCustomSetting read GetDefName;
        property Filter: TCustomSetting read GetFilter;
        property DatasetObjectIdentName: TCustomSetting read GetDatasetObjectIdentName;
        property RecordCountIdentName: TCustomSetting read GetRecordCountIdentName;
        property SourceFileName: TCustomSetting read GetSourceFileName;
        property OrderBy: TCustomSetting read GetOrderBy;
    end;

    TDatasetOpenMethodStep = class(TMethodStep)
    private
        function GetMainSubOptionSetting: TMethodStepSetting_DatasetOpen;
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
    public
        constructor Create(const aActionName: string; aMDataLink: TMethodStepDataLink;
            aOnAddEditFunctions: TNotifyEvent = nil); override;
        property MainSubOptionSetting: TMethodStepSetting_DatasetOpen read GetMainSubOptionSetting;
    end;

    TDatasetOpenRunStepBuilder = class(TRunStepByMethodStepBuilder)
    protected
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
        function GetMStep: TDatasetOpenMethodStep;
        function GetRunOptions(): TMethodStepSetting_DatasetOpen;
    public
        function GetMethodStepClass(): TMethodStepClass; override;
        property MStep: TDatasetOpenMethodStep read GetMStep;
        property RunOptions: TMethodStepSetting_DatasetOpen read GetRunOptions;
    end;

    TDatasetOpenRunStepBuilderCreator = class(TRunStepBuilderCreator)
    public
        function CreateMethodStep(aActionName: string; aMDataLink: TMethodStepDataLink;
            aOnAddEditFunctions: TNotifyEvent): TMethodStep; override;
        function CreateRunStepBuilder(const aActionName: string): TRunStepBuilder; override;
    end;

    TDatasetOpenRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // --------------------------------------------------------------------------------------------- Close
    TMethodStepSetting_DatasetClose = class(TMethodStepCompositeSetting)
    private
        function GetDatasetObject: TCustomSetting;
    protected
        function GetOptionSummary: string; override;
    public
        constructor Create(aOnAddEditFunctions: TNotifyEvent);
        property DatasetObject: TCustomSetting read GetDatasetObject;
    end;

    TDatasetCloseMethodStep = class(TMethodStep)
    private
        function GetMainSubOptionSetting: TMethodStepSetting_DatasetClose;
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
    public
        constructor Create(const aActionName: string; aMDataLink: TMethodStepDataLink;
            aOnAddEditFunctions: TNotifyEvent = nil); override;
        property MainSubOptionSetting: TMethodStepSetting_DatasetClose read GetMainSubOptionSetting;
    end;

    TDatasetCloseRunStepBuilder = class(TRunStepByMethodStepBuilder)
    protected
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
        function GetMStep: TDatasetCloseMethodStep;
        function GetRunOptions(): TMethodStepSetting_DatasetClose;
    public
        function GetMethodStepClass(): TMethodStepClass; override;
        property MStep: TDatasetCloseMethodStep read GetMStep;
        property RunOptions: TMethodStepSetting_DatasetClose read GetRunOptions;
    end;

    TDatasetCloseRunStepBuilderCreator = class(TRunStepBuilderCreator)
    public
        function CreateMethodStep(aActionName: string; aMDataLink: TMethodStepDataLink;
            aOnAddEditFunctions: TNotifyEvent): TMethodStep; override;
        function CreateRunStepBuilder(const aActionName: string): TRunStepBuilder; override;
    end;

    TDatasetCloseRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // --------------------------------------------------------------------------------------------- CursorMove
    TMethodStepSetting_DatasetCursorMove = class(TMethodStepCompositeSetting)
    private
        function GetDatasetObject: TCustomSetting;
        function GetIsRelativeMove: TCustomSetting;
        function GetMoveOffset: TCustomSetting;
        function GetRecordNumIdentName: TCustomSetting;
    protected
        function GetOptionSummary: string; override;
    public
        constructor Create(aOnAddEditFunctions: TNotifyEvent);
        property DatasetObject: TCustomSetting read GetDatasetObject;
        property IsRelativeMove: TCustomSetting read GetIsRelativeMove;
        property MoveOffset: TCustomSetting read GetMoveOffset;
        property RecordNumIdentName: TCustomSetting read GetRecordNumIdentName;
    end;

    TDatasetCursorMoveMethodStep = class(TMethodStep)
    private
        function GetMainSubOptionSetting: TMethodStepSetting_DatasetCursorMove;
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
    public
        constructor Create(const aActionName: string; aMDataLink: TMethodStepDataLink;
            aOnAddEditFunctions: TNotifyEvent = nil); override;
        property MainSubOptionSetting: TMethodStepSetting_DatasetCursorMove read GetMainSubOptionSetting;
    end;

    TDatasetCursorMoveRunStepBuilder = class(TRunStepByMethodStepBuilder)
    protected
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
        function GetMStep: TDatasetCursorMoveMethodStep;
        function GetRunOptions(): TMethodStepSetting_DatasetCursorMove;
    public
        function GetMethodStepClass(): TMethodStepClass; override;
        property MStep: TDatasetCursorMoveMethodStep read GetMStep;
        property RunOptions: TMethodStepSetting_DatasetCursorMove read GetRunOptions;
    end;

    TDatasetCursorMoveRunStepBuilderCreator = class(TRunStepBuilderCreator)
    public
        function CreateMethodStep(aActionName: string; aMDataLink: TMethodStepDataLink;
            aOnAddEditFunctions: TNotifyEvent): TMethodStep; override;
        function CreateRunStepBuilder(const aActionName: string): TRunStepBuilder; override;
    end;

    TDatasetCursorMoveRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // --------------------------------------------------------------------------------------------- Read
    TMethodStepSetting_DatasetRead = class(TMethodStepCompositeSetting)
    private
        function GetDatasetObject: TCustomSetting;
    protected
        function GetOptionSummary: string; override;
    public
        constructor Create(aOnAddEditFunctions: TNotifyEvent);
        property DatasetObject: TCustomSetting read GetDatasetObject;
    end;

    TDatasetReadMethodStep = class(TMethodStep)
    private
        function GetMainSubOptionSetting: TMethodStepSetting_DatasetRead;
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
    public
        constructor Create(const aActionName: string; aMDataLink: TMethodStepDataLink;
            aOnAddEditFunctions: TNotifyEvent = nil); override;
        property MainSubOptionSetting: TMethodStepSetting_DatasetRead read GetMainSubOptionSetting;
    end;

    TDatasetReadRunStepBuilder = class(TRunStepByMethodStepBuilder)
    protected
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
        function GetMStep: TDatasetReadMethodStep;
        function GetRunOptions(): TMethodStepSetting_DatasetRead;
    public
        function GetMethodStepClass(): TMethodStepClass; override;
        property MStep: TDatasetReadMethodStep read GetMStep;
        property RunOptions: TMethodStepSetting_DatasetRead read GetRunOptions;
    end;

    TDatasetReadRunStepBuilderCreator = class(TRunStepBuilderCreator)
    public
        function CreateMethodStep(aActionName: string; aMDataLink: TMethodStepDataLink;
            aOnAddEditFunctions: TNotifyEvent): TMethodStep; override;
        function CreateRunStepBuilder(const aActionName: string): TRunStepBuilder; override;
    end;

    TDatasetReadRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    SysUtils,
    MethodTypes,
    MethodGUIParsing,
    GeneralTypes,
    DatasetRunStepInfo,
    DatasetRunStep,
    ImportDataAdaptor;

const
    cMethOptionKeyPrefix = 'DS';

    cMethOptionKeyDefName = cMethOptionKeyPrefix + 'DEFNAME';
    cMethOptionKeyFilter = cMethOptionKeyPrefix + 'FILTER';
    cMethOptionKeyDatasetObjectIdentName = cMethOptionKeyPrefix + 'OBJIDENTNAME';
    cMethOptionKeyRecordCountIdentName = cMethOptionKeyPrefix + 'RECCOUNTIDENTNAME';
    cMethOptionKeySourceFileName = cMethOptionKeyPrefix + 'SOURCEFILE';
    cMethOptionKeyOrderBy = cMethOptionKeyPrefix + 'ORDERBY';

    cMethOptionKeyDatasetObject = cMethOptionKeyPrefix + 'OBJECT';

    cMethOptionKeyIsRelativeMove = cMethOptionKeyPrefix + 'MOVETYPE';
    cMethOptionKeyMoveOffset = cMethOptionKeyPrefix + 'MOVEOFFSET';
    cMethOptionKeyRecordNumIdentName = cMethOptionKeyPrefix + 'RECNUMIDENTNAME';

    { TCustomSetting_DefName }

constructor TCustomSetting_DefName.Create(const aKey, aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeNone, aOnAddEditFunctions);
end;

{ TMethodStepSetting_DatasetOpen }

constructor TMethodStepSetting_DatasetOpen.Create(aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create('', '', false);
    AddParam(TCustomSetting_RunVarName.Create(cMethOptionKeyDatasetObjectIdentName,
        TLanguageString.Read('Store current import as ...', 'Aktuellen Import speichern als ...'),
        aOnAddEditFunctions));
    AddParam(TCustomSetting_ImportDefName.Create(cMethOptionKeyDefName,
        TLanguageString.Read('Definition name (variable import)', 'Definitionsname (Variablenimport)'),
        aOnAddEditFunctions, INT_DEF_MODE_METHVARIMPORT));
    AddParam(cMethOptionKeyFilter, TLanguageString.Read('Filter', 'Filter'), true);
    AddParam(cMethOptionKeyOrderBy, TLanguageString.Read('Sorting order, separated with comma',
        'Sortierreihenfolge, getrennt mit Komma'), true);
    AddParam(TCustomSetting_FileName.Create(cMethOptionKeySourceFileName,
        TLanguageString.Read('Path and name of the import file', 'Pfad und Name der Import-Datei'),
        aOnAddEditFunctions));
    AddParam(TCustomSetting_RunVarName.Create(cMethOptionKeyRecordCountIdentName,
        TLanguageString.Read('Store the number of lines as ...', 'Die Zeilenanzahl speichern unter ...'),
        aOnAddEditFunctions));
    SetValue('');
end;

function TMethodStepSetting_DatasetOpen.GetDefName: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyDefName);
end;

function TMethodStepSetting_DatasetOpen.GetFilter: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyFilter);
end;

function TMethodStepSetting_DatasetOpen.GetOrderBy: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyOrderBy);
end;

function TMethodStepSetting_DatasetOpen.GetSourceFileName: TCustomSetting;
begin
    result := self.Find(cMethOptionKeySourceFileName);
end;

function TMethodStepSetting_DatasetOpen.GetDatasetObjectIdentName: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyDatasetObjectIdentName);
end;

function TMethodStepSetting_DatasetOpen.GetRecordCountIdentName: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyRecordCountIdentName);
end;

function TMethodStepSetting_DatasetOpen.GetOptionSummary: string;
begin
    result := '[' + self.DatasetObjectIdentName.Value + '], Definition: ' + self.DefName.Value;

    if (self.Filter.Value <> '') then
        result := result + ', Filter: ' + self.Filter.Value;
    if (self.OrderBy.Value <> '') then
        result := result + ', OrderBy: ' + self.OrderBy.Value;
    if (self.SourceFileName.Value <> '') then
        result := result + TLanguageString.Read(', File: ', ', Datei: ') + self.SourceFileName.Value;

    if (self.RecordCountIdentName.Value <> '') then
        result := result + '  (' + self.RecordCountIdentName.Value + TLanguageString.
            Read(' := Line Count)', ' := Anzahl Zeilen)');
end;

{ TDatasetOpenMethodStep }

constructor TDatasetOpenMethodStep.Create(const aActionName: string; aMDataLink: TMethodStepDataLink;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited;

end;

procedure TDatasetOpenMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    AddOptionViewSubParam(TMethodStepSetting_DatasetOpen.Create(aOnAddEditFunctions));
end;

function TDatasetOpenMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TDatasetOpenRunStepInfo.Create();
end;

function TDatasetOpenMethodStep.GetMainSubOptionSetting: TMethodStepSetting_DatasetOpen;
begin
    result := inherited MainSubOptionSetting as TMethodStepSetting_DatasetOpen;
end;

{ TDatasetOpenRunStepBuilder }

function TDatasetOpenRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TDatasetOpenMethodStep;
end;

function TDatasetOpenRunStepBuilder.GetMStep: TDatasetOpenMethodStep;
begin
    result := inherited MStep as TDatasetOpenMethodStep;
end;

function TDatasetOpenRunStepBuilder.GetRunOptions(): TMethodStepSetting_DatasetOpen;
begin
    result := self.MStep.MainSubOptionSetting;
end;

procedure TDatasetOpenRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep;
    aHelper: TRunStepBuilderHelper);
var
    xGeneralRunStep: TRunStep;
    xRunStep: TDatasetOpenRunStep;
begin
    xGeneralRunStep := CreateRunStepByNameAndAdd(aRunSteps);
    xRunStep := xGeneralRunStep as TDatasetOpenRunStep;

    xRunStep.DefName := self.RunOptions.DefName.ParseValue;
    xRunStep.Filter := self.RunOptions.Filter.ParseValue;
    xRunStep.DatasetObjectIdentName := self.RunOptions.DatasetObjectIdentName.ParseValue;
    xRunStep.RecordCountIdentName := self.RunOptions.RecordCountIdentName.ParseValue;
    xRunStep.SourceFileName := self.RunOptions.SourceFileName.ParseValue;
    xRunStep.OrderBy := self.RunOptions.OrderBy.ParseValue;
end;

{ TDatasetOpenRunStepBuilderCreator }

function TDatasetOpenRunStepBuilderCreator.CreateMethodStep(aActionName: string;
    aMDataLink: TMethodStepDataLink; aOnAddEditFunctions: TNotifyEvent): TMethodStep;
begin
    result := TDatasetOpenMethodStep.Create(aActionName, aMDataLink, aOnAddEditFunctions);
end;

function TDatasetOpenRunStepBuilderCreator.CreateRunStepBuilder(const aActionName: string): TRunStepBuilder;
begin
    result := TDatasetOpenRunStepBuilder.Create(aActionName);
end;

{ TDatasetOpenRunStepBuilderTypeInfo }

constructor TDatasetOpenRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDatasetOpen = '1.0.0';
    cStepTypeNameDatasetOpen = cActionNameDatasetOpen;
begin
    inherited Create(cStepTypeNameDatasetOpen, cStepTypeVersionDatasetOpen, aLibName, aLibVersion,
        TDatasetOpenRunStepBuilderCreator.Create());
end;

{ TMethodStepSetting_DatasetClose }

constructor TMethodStepSetting_DatasetClose.Create(aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create('', '', false);
    AddParam(cMethOptionKeyDatasetObject, TLanguageString.Read('Import variable', 'Import-Variable'), true);
    SetValue('');
end;

function TMethodStepSetting_DatasetClose.GetDatasetObject: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyDatasetObject);
end;

function TMethodStepSetting_DatasetClose.GetOptionSummary: string;
begin
    result := '[' + self.DatasetObject.Value + ']';
end;

{ TDatasetCloseMethodStep }

constructor TDatasetCloseMethodStep.Create(const aActionName: string; aMDataLink: TMethodStepDataLink;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited;

end;

procedure TDatasetCloseMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    AddOptionViewSubParam(TMethodStepSetting_DatasetClose.Create(aOnAddEditFunctions));
end;

function TDatasetCloseMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TDatasetCloseRunStepInfo.Create();
end;

function TDatasetCloseMethodStep.GetMainSubOptionSetting: TMethodStepSetting_DatasetClose;
begin
    result := inherited MainSubOptionSetting as TMethodStepSetting_DatasetClose;
end;

{ TDatasetCloseRunStepBuilder }

function TDatasetCloseRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TDatasetCloseMethodStep;
end;

function TDatasetCloseRunStepBuilder.GetMStep: TDatasetCloseMethodStep;
begin
    result := inherited MStep as TDatasetCloseMethodStep;
end;

function TDatasetCloseRunStepBuilder.GetRunOptions(): TMethodStepSetting_DatasetClose;
begin
    result := self.MStep.MainSubOptionSetting;
end;

procedure TDatasetCloseRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep;
    aHelper: TRunStepBuilderHelper);
var
    xGeneralRunStep: TRunStep;
    xRunStep: TDatasetCloseRunStep;
begin
    xGeneralRunStep := CreateRunStepByNameAndAdd(aRunSteps);
    xRunStep := xGeneralRunStep as TDatasetCloseRunStep;

    xRunStep.DatasetObject := StrToInt(self.RunOptions.DatasetObject.ParseValue);
end;

{ TDatasetCloseRunStepBuilderCreator }

function TDatasetCloseRunStepBuilderCreator.CreateMethodStep(aActionName: string;
    aMDataLink: TMethodStepDataLink; aOnAddEditFunctions: TNotifyEvent): TMethodStep;
begin
    result := TDatasetCloseMethodStep.Create(aActionName, aMDataLink, aOnAddEditFunctions);
end;

function TDatasetCloseRunStepBuilderCreator.CreateRunStepBuilder(const aActionName: string): TRunStepBuilder;
begin
    result := TDatasetCloseRunStepBuilder.Create(aActionName);
end;

{ TDatasetCloseRunStepBuilderTypeInfo }

constructor TDatasetCloseRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDatasetClose = '1.0.0';
    cStepTypeNameDatasetClose = cActionNameDatasetClose;
begin
    inherited Create(cStepTypeNameDatasetClose, cStepTypeVersionDatasetClose, aLibName, aLibVersion,
        TDatasetCloseRunStepBuilderCreator.Create());
end;

// ---------------------------------------------------------------------------------------------------------- CursorMove
{ TMethodStepSetting_DatasetCursorMove }

constructor TMethodStepSetting_DatasetCursorMove.Create(aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create('', '', false);
    AddParam(cMethOptionKeyDatasetObject, TLanguageString.Read('Import variable', 'Import-Variable'), true);
    AddParam(TCustomSetting_YesNo.Create(cMethOptionKeyIsRelativeMove, TLanguageString.Read('Move relative',
        'Relativen Versatz'), aOnAddEditFunctions));
    AddParam(cMethOptionKeyMoveOffset, TLanguageString.Read('Offset', 'Versatz'), true);
    AddParam(TCustomSetting_RunVarName.Create(cMethOptionKeyRecordNumIdentName,
        TLanguageString.Read('Store the new line number as ...', 'Die neue Zeilennummer speichern unter ...'),
        aOnAddEditFunctions));
    SetValue('');
end;

function TMethodStepSetting_DatasetCursorMove.GetDatasetObject: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyDatasetObject);
end;

function TMethodStepSetting_DatasetCursorMove.GetIsRelativeMove: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyIsRelativeMove);
end;

function TMethodStepSetting_DatasetCursorMove.GetMoveOffset: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyMoveOffset);
end;

function TMethodStepSetting_DatasetCursorMove.GetRecordNumIdentName: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyRecordNumIdentName);
end;

function TMethodStepSetting_DatasetCursorMove.GetOptionSummary: string;
begin
    result := '[' + self.DatasetObject.Value + '], ';

    if TMethodGUIParser.YesNoStrToBool(self.IsRelativeMove.Value) then
        result := result + TLanguageString.Read('{0} step relative', '{0} Schritt relativ',
            [self.MoveOffset.Value])
    else
        result := result + TLanguageString.Read(' go to line: {0}', ' gehe zu Zeile: {0}',
            [self.MoveOffset.Value]);

    if (self.RecordNumIdentName.Value <> '') then
        result := result + TLanguageString.Read(' ({0} := New line)', ' ({0} := Neue Zeile)',
            [self.RecordNumIdentName.Value]);
end;

{ TDatasetCursorMoveMethodStep }

constructor TDatasetCursorMoveMethodStep.Create(const aActionName: string; aMDataLink: TMethodStepDataLink;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited;

end;

procedure TDatasetCursorMoveMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    AddOptionViewSubParam(TMethodStepSetting_DatasetCursorMove.Create(aOnAddEditFunctions));
end;

function TDatasetCursorMoveMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TDatasetCursorMoveRunStepInfo.Create();
end;

function TDatasetCursorMoveMethodStep.GetMainSubOptionSetting: TMethodStepSetting_DatasetCursorMove;
begin
    result := inherited MainSubOptionSetting as TMethodStepSetting_DatasetCursorMove;
end;

{ TDatasetCursorMoveRunStepBuilder }

function TDatasetCursorMoveRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TDatasetCursorMoveMethodStep;
end;

function TDatasetCursorMoveRunStepBuilder.GetMStep: TDatasetCursorMoveMethodStep;
begin
    result := inherited MStep as TDatasetCursorMoveMethodStep;
end;

function TDatasetCursorMoveRunStepBuilder.GetRunOptions(): TMethodStepSetting_DatasetCursorMove;
begin
    result := self.MStep.MainSubOptionSetting;
end;

procedure TDatasetCursorMoveRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep;
    aHelper: TRunStepBuilderHelper);
var
    xGeneralRunStep: TRunStep;
    xRunStep: TDatasetCursorMoveRunStep;
begin
    xGeneralRunStep := CreateRunStepByNameAndAdd(aRunSteps);
    xRunStep := xGeneralRunStep as TDatasetCursorMoveRunStep;

    xRunStep.DatasetObject := StrToInt(self.RunOptions.DatasetObject.ParseValue);
    xRunStep.IsRelativeMove := TMethodGUIParser.YesNoStrToBool(self.RunOptions.IsRelativeMove.ParseValue);
    xRunStep.MoveOffset := StrToInt(self.RunOptions.MoveOffset.ParseValue);
    xRunStep.RecordNumIdentName := self.RunOptions.RecordNumIdentName.ParseValue;
end;

{ TDatasetCursorMoveRunStepBuilderCreator }

function TDatasetCursorMoveRunStepBuilderCreator.CreateMethodStep(aActionName: string;
    aMDataLink: TMethodStepDataLink; aOnAddEditFunctions: TNotifyEvent): TMethodStep;
begin
    result := TDatasetCursorMoveMethodStep.Create(aActionName, aMDataLink, aOnAddEditFunctions);
end;

function TDatasetCursorMoveRunStepBuilderCreator.CreateRunStepBuilder(const aActionName: string)
    : TRunStepBuilder;
begin
    result := TDatasetCursorMoveRunStepBuilder.Create(aActionName);
end;

{ TDatasetCursorMoveRunStepBuilderTypeInfo }

constructor TDatasetCursorMoveRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDatasetCursorMove = '1.0.0';
    cStepTypeNameDatasetCursorMove = cActionNameDatasetCursorMove;
begin
    inherited Create(cStepTypeNameDatasetCursorMove, cStepTypeVersionDatasetCursorMove, aLibName, aLibVersion,
        TDatasetCursorMoveRunStepBuilderCreator.Create());
end;

// ------------------------------------------------------------------------------------------------------------- Read
{ TMethodStepSetting_DatasetRead }

constructor TMethodStepSetting_DatasetRead.Create(aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create('', '', false);
    AddParam(cMethOptionKeyDatasetObject, TLanguageString.Read('Import variable', 'Import-Variable'), true);
    SetValue('');
end;

function TMethodStepSetting_DatasetRead.GetDatasetObject: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyDatasetObject);
end;

function TMethodStepSetting_DatasetRead.GetOptionSummary: string;
begin
    result := '[' + self.DatasetObject.Value + ']';
end;

{ TDatasetReadMethodStep }

constructor TDatasetReadMethodStep.Create(const aActionName: string; aMDataLink: TMethodStepDataLink;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited;

end;

procedure TDatasetReadMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    AddOptionViewSubParam(TMethodStepSetting_DatasetRead.Create(aOnAddEditFunctions));
end;

function TDatasetReadMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TDatasetReadRunStepInfo.Create();
end;

function TDatasetReadMethodStep.GetMainSubOptionSetting: TMethodStepSetting_DatasetRead;
begin
    result := inherited MainSubOptionSetting as TMethodStepSetting_DatasetRead;
end;

{ TDatasetReadRunStepBuilder }

function TDatasetReadRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TDatasetReadMethodStep;
end;

function TDatasetReadRunStepBuilder.GetMStep: TDatasetReadMethodStep;
begin
    result := inherited MStep as TDatasetReadMethodStep;
end;

function TDatasetReadRunStepBuilder.GetRunOptions(): TMethodStepSetting_DatasetRead;
begin
    result := self.MStep.MainSubOptionSetting;
end;

procedure TDatasetReadRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep;
    aHelper: TRunStepBuilderHelper);
var
    xGeneralRunStep: TRunStep;
    xRunStep: TDatasetReadRunStep;
begin
    xGeneralRunStep := CreateRunStepByNameAndAdd(aRunSteps);
    xRunStep := xGeneralRunStep as TDatasetReadRunStep;

    xRunStep.DatasetObject := StrToInt(self.RunOptions.DatasetObject.ParseValue);
end;

{ TDatasetReadRunStepBuilderCreator }

function TDatasetReadRunStepBuilderCreator.CreateMethodStep(aActionName: string;
    aMDataLink: TMethodStepDataLink; aOnAddEditFunctions: TNotifyEvent): TMethodStep;
begin
    result := TDatasetReadMethodStep.Create(aActionName, aMDataLink, aOnAddEditFunctions);
end;

function TDatasetReadRunStepBuilderCreator.CreateRunStepBuilder(const aActionName: string): TRunStepBuilder;
begin
    result := TDatasetReadRunStepBuilder.Create(aActionName);
end;

{ TDatasetReadRunStepBuilderTypeInfo }

constructor TDatasetReadRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDatasetRead = '1.0.0';
    cStepTypeNameDatasetRead = cActionNameDatasetRead;
begin
    inherited Create(cStepTypeNameDatasetRead, cStepTypeVersionDatasetRead, aLibName, aLibVersion,
        TDatasetReadRunStepBuilderCreator.Create());
end;


end.
