{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk  RegisterRunStepClasses                TN4232    New
  22.07.09 pk  GetAllActions                         TN4668    only get actions that are not hidden
  04.11.09 pk                                	     TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  27.03.13 wl                                        TN6045    uses geändert
  29.11.13 ts  GetAllActions                         TN6316    order changed, no sorting -> sorting already in ReadAllActions
  ----------------------------------------------------------------------------------------------------------------------- }

unit RunStepInfoTypeDictionary;


interface


uses
    TypeDictionary,
    TypeInfo;

type
    TRunStepInfoTypeDictionary = class(TTypeDictionary)
    private
        class var uInstance: TRunStepInfoTypeDictionary;
    protected
        fStaticTypeInfos: TTypeInfoList;
        procedure InitTypeInfoList; override;
        function IsValidType(aTypeInfo: TTypeInfo): boolean; override;
    public
        function ReadAllActions(): TArray<string>;
        function GetAllActions(): TArray<string>;
        procedure RegisterRunStepInfoTypeInfoClasses();
        class procedure CreateInstance();
        class property Instance: TRunStepInfoTypeDictionary read uInstance;
        class procedure DestroyInstance();
    end;


implementation


uses
    Generics.Collections,
    SysUtils,
    RunStepInfoTypeInfo,
    PluginLoader;

{ TRunStepInfoTypeDictionary }

class procedure TRunStepInfoTypeDictionary.CreateInstance();
begin
    if Assigned(uInstance) then
        EXIT;

    uInstance := TRunStepInfoTypeDictionary.Create('RunStepInfo Type Dictionary');
end;

class procedure TRunStepInfoTypeDictionary.DestroyInstance();
begin
    FreeAndNil(uInstance);
end;

function TRunStepInfoTypeDictionary.IsValidType(aTypeInfo: TTypeInfo): boolean;
begin
    result := aTypeInfo is TRunStepInfoTypeInfo;
end;

procedure TRunStepInfoTypeDictionary.InitTypeInfoList;
begin
    TPluginLoader.LoadAllTypes(self);
end;

function TRunStepInfoTypeDictionary.ReadAllActions(): TArray<string>;
begin
    result := self.ReadTypeNames(true);
end;

function TRunStepInfoTypeDictionary.GetAllActions(): TArray<string>;
var
    x: integer;
    xNames: TArray<string>;
    xList: TList<string>;
begin
    xNames := self.ReadAllActions();

    xList := TList<string>.Create();
    try
        for x := 0 to Length(xNames) - 1 do
        begin
            if (GetTypeFromTypeName(xNames[x]) as TRunStepInfoTypeInfo).IsHidden then
                CONTINUE;
            xList.Add(xNames[x]);
        end;

        result := xList.ToArray;
    finally
        FreeAndNil(xList);
    end;
end;

procedure TRunStepInfoTypeDictionary.RegisterRunStepInfoTypeInfoClasses();
var
    xTypeInfo: TTypeInfo;
begin
    for xTypeInfo in fTypeInfos do
    begin
        (xTypeInfo as TRunStepInfoTypeInfo).RunStepInfoCreator.RegisterRunStepInfoClass;
    end;
end;


end.
