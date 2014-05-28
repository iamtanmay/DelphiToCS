{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  07.06.10 pk                                        TN5077     initial revision
  10.04.13 wl                                        TN6045   uses Generics.Collections
  ----------------------------------------------------------------------------------------------------------------------- }

unit RestartPreparationStep;


interface


uses
    Generics.Collections,
    ActionData,
    RunEffectData;

type
    TRestartPreparationsVisualManager = class
    public
        procedure AddText(const aText: string); virtual; abstract;
    end;

    TRestartPreparationStep = class
    protected
        fVisualManager: TRestartPreparationsVisualManager;
        fRunEffectData: TRunEffectData;
        procedure DoVisualize(); virtual; abstract;
    public
        constructor Create(const aRunEffectData: TRunEffectData);
        procedure SetVisualManager(const aVisualManager: TRestartPreparationsVisualManager);
        procedure Visualize();
    end;

    TRestartPreparationStepList = class(TObjectList<TRestartPreparationStep>);

    TRestartPreparationStepCreatorTypeInfo = class
    protected
        procedure DoCreateAndAddRestartPreparationSteps(const aRestartPreparations
            : TRestartPreparationStepList; const aRunEffectData: TRunEffectData); virtual;
        function DoCreateRestartPreparationStep(const aRunEffectData: TRunEffectData)
            : TRestartPreparationStep; virtual;
        function DoSupportsData(const aRunEffectData: TRunEffectData): boolean; virtual; abstract;
    public
        function SupportsData(const aRunEffectData: TRunEffectData): boolean;
        procedure CreateAndAddRestartPreparationSteps(const aRestartPreparations: TRestartPreparationStepList;
            const aRunEffectData: TRunEffectData);

    end;

    TRestartPreparationStepCreatorTypeInfoList = class(TObjectList<TRestartPreparationStepCreatorTypeInfo>)
    public
        function FindBySupportsData(const aRunEffectData: TRunEffectData)
            : TRestartPreparationStepCreatorTypeInfo;
    end;


implementation


{ TRunEffectCreatorTypeInfo }

procedure TRestartPreparationStepCreatorTypeInfo.CreateAndAddRestartPreparationSteps
    (const aRestartPreparations: TRestartPreparationStepList; const aRunEffectData: TRunEffectData);
begin
    DoCreateAndAddRestartPreparationSteps(aRestartPreparations, aRunEffectData);
end;

procedure TRestartPreparationStepCreatorTypeInfo.DoCreateAndAddRestartPreparationSteps
    (const aRestartPreparations: TRestartPreparationStepList; const aRunEffectData: TRunEffectData);
begin
    aRestartPreparations.Add(self.DoCreateRestartPreparationStep(aRunEffectData));
end;

function TRestartPreparationStepCreatorTypeInfo.DoCreateRestartPreparationStep(const aRunEffectData
    : TRunEffectData): TRestartPreparationStep;
begin
    result := nil;
    ASSERT('Not implemented');
end;

function TRestartPreparationStepCreatorTypeInfo.SupportsData(const aRunEffectData: TRunEffectData): boolean;
begin
    result := DoSupportsData(aRunEffectData);
end;
{ TRunEffectCreatorTypeInfoList }

function TRestartPreparationStepCreatorTypeInfoList.FindBySupportsData(const aRunEffectData: TRunEffectData)
    : TRestartPreparationStepCreatorTypeInfo;
var
    xTypeInfo: TRestartPreparationStepCreatorTypeInfo;
begin
    result := nil;
    for xTypeInfo in self do
    begin
        if not xTypeInfo.SupportsData(aRunEffectData) then
            CONTINUE;
        result := xTypeInfo;
        EXIT;
    end;

end;

{ TRestartPreparationStep }

constructor TRestartPreparationStep.Create(const aRunEffectData: TRunEffectData);
begin
    inherited Create();
    fRunEffectData := aRunEffectData;
end;

procedure TRestartPreparationStep.SetVisualManager(const aVisualManager: TRestartPreparationsVisualManager);
begin
    fVisualManager := aVisualManager;
end;

procedure TRestartPreparationStep.Visualize;
begin
    DoVisualize();
end;


end.
