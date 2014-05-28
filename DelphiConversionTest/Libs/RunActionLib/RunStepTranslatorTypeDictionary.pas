unit RunStepTranslatorTypeDictionary;


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
    TypeDictionary,
    TypeInfo;

type
    TRunStepTranslatorTypeDictionary = class(TTypeDictionary)
    protected
        procedure InitTypeInfoList; override;
        function IsValidType(aTypeInfo: TTypeInfo): boolean; override;
    public
        class procedure CreateInstance();
        class function Instance(): TRunStepTranslatorTypeDictionary;
        class procedure DestroyInstance();
    end;


    // ##################################################################################################


implementation


uses
    RunStepTranslatorTypeInfo,
    PluginLoader;

var
    uRunStepTranslatorTypeDictionary: TRunStepTranslatorTypeDictionary = nil;

class procedure TRunStepTranslatorTypeDictionary.CreateInstance();
begin
    if Assigned(uRunStepTranslatorTypeDictionary) then
        EXIT;
    uRunStepTranslatorTypeDictionary := TRunStepTranslatorTypeDictionary.Create
        ('RunStepTranslator Type Dictionary');
end;

class procedure TRunStepTranslatorTypeDictionary.DestroyInstance();
begin
    uRunStepTranslatorTypeDictionary.Free;
end;

class function TRunStepTranslatorTypeDictionary.Instance(): TRunStepTranslatorTypeDictionary;
begin
    result := uRunStepTranslatorTypeDictionary;
end;

function TRunStepTranslatorTypeDictionary.IsValidType(aTypeInfo: TTypeInfo): boolean;
begin
    result := aTypeInfo is TRunStepTranslatorTypeInfo;
end;

procedure TRunStepTranslatorTypeDictionary.InitTypeInfoList;

begin
    TPluginLoader.LoadAllTypes(self);
end;


end.
