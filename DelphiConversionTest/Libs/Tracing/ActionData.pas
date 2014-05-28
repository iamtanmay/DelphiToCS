{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.02.10 pk                                        TN4972    Initial Revision
  07.06.10 pk                                        TN5077    TRunEffectListData Moved to RunEffectData
  09.06.10 pk  fStepName                             TN5077    New: this is important when runstep is not defined
  26.10.10 pk                                        TN5297    Changes for ActionData Segment concept
  29.10.10 pk  AddActionSegment                      TN5297    remove assertion and use FreeAndNil
  15.11.10 pk                                        TN5340    Changes to prevent memory leak
  18.09.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit ActionData;


interface


uses
    Streamable,
    ActionIDDataCache,
    RelativeMemAddressData,
    RunStep,
    RunEffectData,
    TypeInfo,
    TypeDictionary;

type
    TRunEffectSegment = class(TStreamable)
    private
        fRunEffects: TRunEffectListData;
    public
        constructor Create(); overload; override;
        destructor Destroy(); override;
        procedure AddRunEffect(const aRunEffectData: TRunEffectData);
    published
        property RunEffects: TRunEffectListData read fRunEffects write fRunEffects;
    end;

    TActionRunEffectSegment = class(TRunEffectSegment)
    private
        fRunStep: TRunStep;
        fStartTime: string;
        fFinishTime: string;
    public

        constructor Create(); overload; override;
        destructor Destroy(); override;
    published
        property StartTime: string read fStartTime write fStartTime;
        property FinishTime: string read fFinishTime write fFinishTime;
        property RunStep: TRunStep read fRunStep write fRunStep;
    end;

    TRunEffectSegmentList = class(TStreamableObjectList)
    private
        function GetSegmentAt(aIndex: integer): TRunEffectSegment;
    public
        property Segments[aIndex: integer]: TRunEffectSegment read GetSegmentAt; default;
    end;

    TActionData = class(TStreamable)
    private
        fActionID: TActionID;
        fStepName: string;
        fUndone: boolean;
        fAddress: TRelativeMemAddressData;
        fIsInEvent: boolean;
        fPrepareSegment: TRunEffectSegment;

        function GetCurrentSegment: TRunEffectSegment;
        function GetCurrentActionRunEffectSegment: TActionRunEffectSegment; virtual; abstract;
        function GetActionSegmentAt(aIndex: integer): TActionRunEffectSegment; virtual; abstract;
        function GetSegmentCount: integer; virtual; abstract;
    protected
        function CreateActionSegment(): TActionRunEffectSegment;
        function GetDescription: string; virtual;
        function GetIsUndoablePerSegment(): boolean; virtual;
    public
        constructor Create(); overload; override;
        constructor Create(const aActionID: TActionID; const aStepName: string; aIsInEvent: boolean);
            reintroduce; overload;
        destructor Destroy(); override;
        class function DateTimeValueToStr(const aValue: TDateTime): string;
        procedure AddActionSegment(); virtual; abstract;
        procedure RemoveActionSegment(const aActionSegment: TActionRunEffectSegment); virtual; abstract;
        procedure ClearPrepareSegment();
        procedure AddRunEffectToCurrentSegment(const aRunEffectData: TRunEffectData);
        property Description: string read GetDescription;
        property CurrentSegment: TRunEffectSegment read GetCurrentSegment;
        property CurrentActionSegment: TActionRunEffectSegment read GetCurrentActionRunEffectSegment;
        property ActionSegmentCount: integer read GetSegmentCount;
        property ActionSegments[aIndex: integer]: TActionRunEffectSegment read GetActionSegmentAt;
        property IsUndoablePerSegment: boolean read GetIsUndoablePerSegment;

    published
        property StepName: string read fStepName write fStepName;
        property ActionID: TActionID read fActionID write fActionID;
        // property Undone : boolean read fUndone write fUndone;
        property Address: TRelativeMemAddressData read fAddress write fAddress;
        property IsInEvent: boolean read fIsInEvent write fIsInEvent;
        property PrepareSegment: TRunEffectSegment read fPrepareSegment write fPrepareSegment;

    end;

    TSingleSegmentActionData = class(TActionData)
    private
        fActionSegment: TActionRunEffectSegment;
    protected
        function GetCurrentActionRunEffectSegment: TActionRunEffectSegment; override;
        function GetActionSegmentAt(aIndex: integer): TActionRunEffectSegment; override;
        function GetSegmentCount: integer; override;

    public
        procedure AddActionSegment(); override;
        procedure RemoveActionSegment(const aActionSegment: TActionRunEffectSegment); override;
    published
        property ActionSegment: TActionRunEffectSegment read fActionSegment write fActionSegment;
    end;

    TMultiSegmentActionData = class(TActionData)
    private
        fActionSegments: TRunEffectSegmentList;
    protected
        function GetCurrentActionRunEffectSegment: TActionRunEffectSegment; override;
        function GetActionSegmentAt(aIndex: integer): TActionRunEffectSegment; override;
        function GetSegmentCount: integer; override;
        function GetIsUndoablePerSegment: boolean; override;
    public
        constructor Create(); override;
        destructor Destroy(); override;
        procedure AddActionSegment(); override;
        procedure RemoveActionSegment(const aActionSegment: TActionRunEffectSegment); override;

    published
        property ActionSegments: TRunEffectSegmentList read fActionSegments write fActionSegments;
    end;

    TActionDataCreatorTypeInfo = class(TTypeInfo)
    protected
        function DoCreateActionData(): TActionData; virtual; abstract;
        function DoSupportsData(const aStepName: string): boolean; virtual; abstract;
    public
        constructor Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string);
        function CreateActionData(const aActionID: TActionID; const aStepName: string): TActionData;
        function SupportsData(const aStepName: string): boolean;
    end;

    TSingleSegmentActionDataCreatorTypeInfo = class(TActionDataCreatorTypeInfo)
    protected
        function DoCreateActionData(): TActionData; override;
    end;

    TDefaultSingleSegmentActionDataCreatorTypeInfo = class(TSingleSegmentActionDataCreatorTypeInfo)
    protected
        function DoSupportsData(const aStepName: string): boolean; override;
    public
        constructor Create();
    end;

    TMultiSegmentActionDataCreatorTypeInfo = class(TActionDataCreatorTypeInfo)
    protected
        function DoCreateActionData(): TActionData; override;

    end;

    TActionDataCreatorTypeDictionary = class(TTypeDictionary)
    private
        fDefaultTypeInfo: TActionDataCreatorTypeInfo;
    protected
        function IsValidType(aTypeInfo: TTypeInfo): boolean; override;
        procedure InitTypeInfoList; override;
    public
        constructor Create();
        destructor Destroy(); override;
        function FindBySupportsData(const aStepName: string): TActionDataCreatorTypeInfo;
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    PluginLoader;

procedure TRunEffectSegment.AddRunEffect(const aRunEffectData: TRunEffectData);
begin
    fRunEffects.List.Add(aRunEffectData);
end;

constructor TRunEffectSegment.Create();
begin
    inherited;
    fRunEffects := TRunEffectListData.Create();
end;

destructor TRunEffectSegment.Destroy();
begin
    FreeAndNil(fRunEffects);
    inherited;
end;

constructor TActionRunEffectSegment.Create();
begin
    inherited Create();

    fStartTime := '';
    fFinishTime := '';
    fRunStep := nil;
end;

destructor TActionRunEffectSegment.Destroy();
begin
    FreeAndNil(fRunStep);
    inherited;
end;

{ TActionData }
class function TActionData.DateTimeValueToStr(const aValue: TDateTime): string;
const
    cFormatTimeStamp = 'dd.mm.yyyy, hh:nn:ss';
begin
    result := FormatDateTime(cFormatTimeStamp, aValue);
end;

procedure TActionData.AddRunEffectToCurrentSegment(const aRunEffectData: TRunEffectData);
var
    xCurrentSegment: TRunEffectSegment;
begin
    xCurrentSegment := self.CurrentSegment;
    ASSERT(Assigned(xCurrentSegment));
    xCurrentSegment.AddRunEffect(aRunEffectData);

end;

procedure TActionData.ClearPrepareSegment;
begin
    fPrepareSegment.RunEffects.Clear();
end;

constructor TActionData.Create(const aActionID: TActionID; const aStepName: string; aIsInEvent: boolean);
begin
    inherited Create();
    fStepName := aStepName;
    fActionID := aActionID;
    fAddress := TRelativeMemAddressData.Create();
    fIsInEvent := aIsInEvent;
    fPrepareSegment := TRunEffectSegment.Create();
    fUndone := false;
end;

constructor TActionData.Create;
begin
    Create(0, '', false);
end;

destructor TActionData.Destroy();
begin
    FreeAndNil(fPrepareSegment);
    FreeAndNil(fAddress);
    inherited;
end;

function TActionData.GetCurrentSegment: TRunEffectSegment;
begin
    result := self.CurrentActionSegment;
    if result = nil then
        result := self.PrepareSegment;

end;

function TActionData.GetDescription: string;
var
    xCurrentSegment: TActionRunEffectSegment;
begin
    result := fStepName;

    xCurrentSegment := self.CurrentActionSegment;
    if (xCurrentSegment <> nil) and (xCurrentSegment.RunStep <> nil) then
        result := xCurrentSegment.RunStep.Description
end;

function TActionData.GetIsUndoablePerSegment: boolean;
begin
    result := false;
end;

function TActionData.CreateActionSegment(): TActionRunEffectSegment;
begin
    result := TActionRunEffectSegment.Create();
end;

{ TMultiSegmentActionData }

constructor TMultiSegmentActionData.Create();
begin
    inherited Create();
    fActionSegments := TRunEffectSegmentList.Create(true);
end;

destructor TMultiSegmentActionData.Destroy;
begin
    FreeAndNil(fActionSegments);
    inherited;
end;

procedure TMultiSegmentActionData.AddActionSegment();
var
    xActionRunEffectSegment: TActionRunEffectSegment;
begin
    xActionRunEffectSegment := CreateActionSegment();
    fActionSegments.Add(xActionRunEffectSegment);
end;

function TMultiSegmentActionData.GetActionSegmentAt(aIndex: integer): TActionRunEffectSegment;
begin
    result := fActionSegments[aIndex] as TActionRunEffectSegment;
end;

function TMultiSegmentActionData.GetSegmentCount: integer;
begin
    result := fActionSegments.Count;
end;

function TMultiSegmentActionData.GetIsUndoablePerSegment: boolean;
begin
    result := true;
end;

function TMultiSegmentActionData.GetCurrentActionRunEffectSegment: TActionRunEffectSegment;
begin
    result := nil;
    if self.ActionSegmentCount = 0 then
        EXIT;
    result := self.ActionSegments[self.ActionSegmentCount - 1] as TActionRunEffectSegment;
end;

procedure TMultiSegmentActionData.RemoveActionSegment(const aActionSegment: TActionRunEffectSegment);
begin
    fActionSegments.Remove(aActionSegment);
end;

{ TRunEffectSegmentList }

function TRunEffectSegmentList.GetSegmentAt(aIndex: integer): TRunEffectSegment;
begin
    result := inherited Items[aIndex] as TRunEffectSegment;
end;

{ TSingleSegmentActionData }

procedure TSingleSegmentActionData.AddActionSegment();
begin
    FreeAndNil(fActionSegment);
    fActionSegment := CreateActionSegment();
end;

function TSingleSegmentActionData.GetActionSegmentAt(aIndex: integer): TActionRunEffectSegment;
begin
    result := fActionSegment;
end;

function TSingleSegmentActionData.GetSegmentCount: integer;
begin
    result := 0;
    if (fActionSegment <> nil) then
    begin
        result := 1;
    end;

end;

procedure TSingleSegmentActionData.RemoveActionSegment(const aActionSegment: TActionRunEffectSegment);
begin
    if fActionSegment = nil then
        EXIT;
    FreeAndNil(fActionSegment);
end;

function TSingleSegmentActionData.GetCurrentActionRunEffectSegment: TActionRunEffectSegment;
begin
    result := fActionSegment;
end;

{ TActionDataCreatorTypeInfo }

constructor TActionDataCreatorTypeInfo.Create(const aTypeName, aTypeInfoVersion, aLibName,
    aLibVersion: string);
begin
    inherited Create(aTypeName, aTypeInfoVersion, aLibName, aLibVersion);
end;

function TActionDataCreatorTypeInfo.CreateActionData(const aActionID: TActionID; const aStepName: string)
    : TActionData;
begin
    result := DoCreateActionData();
    result.ActionID := aActionID;
    result.StepName := aStepName;
    result.IsInEvent := false;
end;

function TActionDataCreatorTypeInfo.SupportsData(const aStepName: string): boolean;
begin
    result := DoSupportsData(aStepName);
end;

{ TSingleSegmentActionDataCreatorTypeInfo }

function TSingleSegmentActionDataCreatorTypeInfo.DoCreateActionData(): TActionData;
begin
    result := TSingleSegmentActionData.Create();
end;

{ TDefaultSingleSegmentActionDataCreatorTypeInfo }

constructor TDefaultSingleSegmentActionDataCreatorTypeInfo.Create;
begin
    inherited Create(self.ClassName, '1.0.0', 'None', 'None');
end;

function TDefaultSingleSegmentActionDataCreatorTypeInfo.DoSupportsData(const aStepName: string): boolean;
begin
    result := false;
end;
{ TMultiSegmentActionDataCreatorTypeInfo }

function TMultiSegmentActionDataCreatorTypeInfo.DoCreateActionData: TActionData;
begin
    result := TMultiSegmentActionData.Create();
end;

{ TActionDataCreatorTypeInfoList }

constructor TActionDataCreatorTypeDictionary.Create;
begin
    inherited Create('TActionDataCreatorTypeDictionary');
    fDefaultTypeInfo := TDefaultSingleSegmentActionDataCreatorTypeInfo.Create();
end;

destructor TActionDataCreatorTypeDictionary.Destroy;
begin
    FreeAndNil(fDefaultTypeInfo);
    inherited;
end;

function TActionDataCreatorTypeDictionary.FindBySupportsData(const aStepName: string)
    : TActionDataCreatorTypeInfo;
var
    xTypeInfo: TTypeInfo;
    xActionDataCreatorTypeInfo: TActionDataCreatorTypeInfo;
begin
    result := fDefaultTypeInfo;
    for xTypeInfo in self.TypeInfos do
    begin
        xActionDataCreatorTypeInfo := xTypeInfo as TActionDataCreatorTypeInfo;
        if xActionDataCreatorTypeInfo.SupportsData(aStepName) then
        begin
            result := xActionDataCreatorTypeInfo;
            EXIT;
        end;
    end;

end;

function TActionDataCreatorTypeDictionary.IsValidType(aTypeInfo: TTypeInfo): boolean;
begin
    result := aTypeInfo is TActionDataCreatorTypeInfo;
end;

procedure TActionDataCreatorTypeDictionary.InitTypeInfoList;
begin
    TPluginLoader.LoadAllTypes(self);
end;


end.
