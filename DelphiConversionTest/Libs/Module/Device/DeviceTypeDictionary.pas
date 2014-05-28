unit DeviceTypeDictionary;
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


interface


uses
    Classes,
    Module,
    ModuleTypeDictionary,
    TypeInfo,
    Device;

type
    TDeviceTypeDictionary = class(TModuleTypeDictionary)
    protected
        function IsValidType(aTypeInfo: TTypeInfo): boolean; override;
    end;

var
    gDeviceTypeDictionary: TDeviceTypeDictionary;

    // ##################################################################################################


implementation


function TDeviceTypeDictionary.IsValidType(aTypeInfo: TTypeInfo): boolean;
begin
    result := aTypeInfo is TDeviceTypeInfo;
end;


initialization


gDeviceTypeDictionary := TDeviceTypeDictionary.Create('Device Type Dictionary');


finalization


gDeviceTypeDictionary.Free;


end.
