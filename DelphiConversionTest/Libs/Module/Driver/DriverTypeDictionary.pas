{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  14.04.08 wl  GetTypeInfoFromTypeName      TN4060   uses TypeInfo
  21.05.08 wl  Create                       TN4119   DictionaryName new for Logging
  ---------------------------------------------------------------------------------------------------------------------- }

unit DriverTypeDictionary;


interface


uses
    Classes,
    Module,
    ModuleTypeDictionary,
    TypeInfo,
    Driver;

type
    TDriverTypeDictionary = class(TModuleTypeDictionary)
    protected
        function IsValidType(aTypeInfo: TTypeInfo): boolean; override;
    end;

var
    gDriverTypeDictionary: TDriverTypeDictionary;

    // ##################################################################################################


implementation


function TDriverTypeDictionary.IsValidType(aTypeInfo: TTypeInfo): boolean;
begin
    result := aTypeInfo is TDriverTypeInfo;
end;


initialization


gDriverTypeDictionary := TDriverTypeDictionary.Create('Driver Type Dictionary');


finalization


gDriverTypeDictionary.Free;


end.
