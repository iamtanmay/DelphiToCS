unit RunStepTranslatorTypeInfo;


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
    RunStepTranslator,
    TypeInfo,
    RunStep;

type

    TRunStepTranslatorCreator = class
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; virtual; abstract;
        function DoCreateRunStepTranslator(): TRunStepTranslator; virtual; abstract;
    public
        function CreateRunStepTranslator(): TRunStepTranslator;
        function IsValideCreatorForRunStep(const aRunStep: TRunStep): boolean;
    end;

    TRunStepTranslatorTypeInfo = class(TTypeInfo)
    protected
        fRunStepTranslatorCreator: TRunStepTranslatorCreator;
        function DoCreateRunStepTranslatorCreator(): TRunStepTranslatorCreator; virtual; abstract;

    public
        constructor Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string);
        property RunStepTranslatorCreator: TRunStepTranslatorCreator read fRunStepTranslatorCreator;

    end;


implementation


{ TRunStepTranslatorTypeInfo }

constructor TRunStepTranslatorTypeInfo.Create(const aTypeName, aTypeInfoVersion, aLibName,
    aLibVersion: string);
begin
    inherited Create(aTypeName, aTypeInfoVersion, aLibName, aLibVersion);
    fRunStepTranslatorCreator := DoCreateRunStepTranslatorCreator();
end;

function TRunStepTranslatorCreator.CreateRunStepTranslator(): TRunStepTranslator;
begin
    result := DoCreateRunStepTranslator();
end;

function TRunStepTranslatorCreator.IsValideCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := GetIsValidCreatorForRunStep(aRunStep);
end;


end.
