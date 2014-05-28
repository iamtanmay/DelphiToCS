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

unit ConnectionTypeDictionary;


interface


uses
    Classes,
    Module,
    ModuleTypeDictionary,
    TypeInfo,
    Connection;

type
    TConnectionTypeDictionary = class(TModuleTypeDictionary)
    protected
        function IsValidType(aTypeInfo: TTypeInfo): boolean; override;
    end;

var
    gConnectionTypeDictionary: TConnectionTypeDictionary;

    // ##################################################################################################


implementation


function TConnectionTypeDictionary.IsValidType(aTypeInfo: TTypeInfo): boolean;
begin
    result := aTypeInfo is TConnectionTypeInfo;
end;


initialization


gConnectionTypeDictionary := TConnectionTypeDictionary.Create('Connection Type Dictionary');


finalization


gConnectionTypeDictionary.Free;


end.
