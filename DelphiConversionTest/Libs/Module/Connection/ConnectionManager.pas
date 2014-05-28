{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  14.04.08 wl  GetTypeInfoFromTypeName      TN4060   uses TypeInfo
  26.05.08 wl  CreateFakeModuleSettings     TN4119   GetModuleSettingsClass replaced
  13.10.08 pk  ReadModuleType               TN4272.2 New
  17.12.08 pk                               TN4374   Code moved to ConnectionSettingsManager
  ---------------------------------------------------------------------------------------------------------------------- }

unit ConnectionManager;


interface


uses
    ModuleManager;

type
    TConnectionManager = class(TModuleManager)
    end;

var
    gConnectionManager: TConnectionManager;


implementation


end.
