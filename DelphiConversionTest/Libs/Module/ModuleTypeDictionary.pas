{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  14.04.08 wl                               TN4060   basic members --> TTypeDictionary
  26.05.08 wl  ReadCompatibleModuleTypeNamesByType  TN4119   TTypeInfo-method name changes
  04.11.09 pk                               	   TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  27.03.13 wl                                TN6045   uses geändert
  ---------------------------------------------------------------------------------------------------------------------- }

unit ModuleTypeDictionary;


interface


uses
    GeneralTypes,
    ModuleTypeInfo,
    ModuleSettings,
    TypeInfo,
    TypeDictionary;

type
    TModuleTypeDictionary = class(TTypeDictionary)
    public
        function ReadCompatibleModuleTypeNamesByType(aModuleID: TModuleID): TStringArray;
    end;


implementation


uses
    Generics.Collections,
    SysUtils;

{ TModuleTypeDictionary }

function TModuleTypeDictionary.ReadCompatibleModuleTypeNamesByType(aModuleID: TModuleID): TStringArray;
var
    xModuleType: TModuleTypeInfo;
    xTypeNames: TList<string>;
    xTypeInfo: TTypeInfo;
begin
    result := nil;
    xTypeNames := TList<string>.Create;
    try
        for xTypeInfo in fTypeInfos do
        begin
            xModuleType := xTypeInfo as TModuleTypeInfo;
            if xModuleType.SupportsModule(aModuleID) then
                xTypeNames.Add(xModuleType.TypeName);
        end;

        xTypeNames.Sort;
        result := xTypeNames.ToArray;

    finally
        FreeAndNil(xTypeNames);
    end;
end;


end.
