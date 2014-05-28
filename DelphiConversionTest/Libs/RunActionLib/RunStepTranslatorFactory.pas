unit RunStepTranslatorFactory;
{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.02.10 pk                                        TN4972     Initial Revision
  ----------------------------------------------------------------------------------------------------------------------- }


interface


uses
    AppTypes,
    Action,
    RunStepTranslator,
    RunStep,
    RunStepFactory,
    RunStepTranslatorTypeInfo;

type
    TRunStepTranslatorFactory = class
    public
        constructor Create();
        class function CreateTranslatorByRunStep(const aRunStep: TRunStep): TRunStepTranslator;
        class function FindTypeInfoByRunStep(aRunStep: TRunStep): TRunStepTranslatorTypeInfo;
    end;


implementation


uses
    SysUtils,
    MethodGUIParsing,
    ErrorManager,
    TypeInfo,
    RunStepTranslatorTypeDictionary;

constructor TRunStepTranslatorFactory.Create();
begin
    inherited Create;
end;

class function TRunStepTranslatorFactory.FindTypeInfoByRunStep(aRunStep: TRunStep)
    : TRunStepTranslatorTypeInfo;
var
    xTypeInfo: TTypeInfo;
    xRunStepTranslatorTypeInfo: TRunStepTranslatorTypeInfo;
begin
    result := nil;
    for xTypeInfo in TRunStepTranslatorTypeDictionary.Instance.TypeInfos do
    begin
        xRunStepTranslatorTypeInfo := xTypeInfo as TRunStepTranslatorTypeInfo;
        if xRunStepTranslatorTypeInfo.RunStepTranslatorCreator.IsValideCreatorForRunStep(aRunStep) then
        begin
            result := xRunStepTranslatorTypeInfo;
            EXIT;
        end;
    end;
end;

class function TRunStepTranslatorFactory.CreateTranslatorByRunStep(const aRunStep: TRunStep)
    : TRunStepTranslator;
var
    xTypeInfo: TRunStepTranslatorTypeInfo;
begin
    result := nil;

    xTypeInfo := FindTypeInfoByRunStep(aRunStep);
    if not Assigned(xTypeInfo) then
        EXIT;

    result := xTypeInfo.RunStepTranslatorCreator.CreateRunStepTranslator();

end;


end.
