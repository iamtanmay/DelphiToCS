unit RunStepTypeDictionary;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk  RegisterRunStepClasses                TN4232    New
  04.11.09 pk                               	    TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    Classes,
    TypeDictionary,
    TypeInfo;

type
    TRunStepTypeDictionary = class(TTypeDictionary)
    protected
        procedure InitTypeInfoList; override;
        function IsValidType(aTypeInfo: TTypeInfo): boolean; override;
    public
        class procedure CreateInstance();
        class function Instance(): TRunStepTypeDictionary;
        class procedure DestroyInstance();
        procedure RegisterRunStepClasses();
    end;


    // ##################################################################################################


implementation


uses
    RunStepTypeInfo,
    PluginLoader;

var
    uRunStepTypeDictionary: TRunStepTypeDictionary = nil;

class procedure TRunStepTypeDictionary.CreateInstance();
begin
    if Assigned(uRunStepTypeDictionary) then
        EXIT;
    uRunStepTypeDictionary := TRunStepTypeDictionary.Create('RunStep Type Dictionary');
end;

class procedure TRunStepTypeDictionary.DestroyInstance();
begin
    uRunStepTypeDictionary.Free;
end;

class function TRunStepTypeDictionary.Instance(): TRunStepTypeDictionary;
begin
    result := uRunStepTypeDictionary;
end;

function TRunStepTypeDictionary.IsValidType(aTypeInfo: TTypeInfo): boolean;
begin
    result := aTypeInfo is TRunStepTypeInfo;
end;

procedure TRunStepTypeDictionary.InitTypeInfoList;
// var
// xTypeInfoList : TTypeInfoList;

begin

    // statisch geladenen Package
    // RunStepBuilderMain.GetModuleTypes( xTypeInfoList );
    // self.AddTypes( xTypeInfoList );

    // dynamisch
    TPluginLoader.LoadAllTypes(self);

end;

procedure TRunStepTypeDictionary.RegisterRunStepClasses;
var
    xTypeInfo: TTypeInfo;
begin
    for xTypeInfo in fTypeInfos do
    begin
        (xTypeInfo as TRunStepTypeInfo).RunStepCreator.RegisterRunStepClass;
    end;
end;


end.
