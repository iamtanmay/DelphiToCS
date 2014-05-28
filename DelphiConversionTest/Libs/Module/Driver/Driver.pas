{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  07.02.08 wl  TDevResetID                  TN4009   neu
  17.03.08 wl                               TN4043   Überflüssiges entfernt
  26.05.08 wl  AreaName                     TN4119   removed
  27.03.13 wl                               TN6045   uses geändert
  ---------------------------------------------------------------------------------------------------------------------- }

unit Driver;


interface


uses
    Module,
    ModuleSettings,
    DriverSettingList,
    ModuleTypeInfo;

const
    STR_COM_NAME = 'COM';

    INT_INITID_NONE = 0;

type
    TDevInitID = TDateTime;
    TDevResetID = TDateTime;

    TDriverAlias = record
        Area: string;
        Section: string;
    end;

    TDriverTypeInfo = class(TModuleTypeInfo);

    IDriver = interface(IModule)
        ['{346C2F8E-8D49-41D0-B0D3-67E384706853}']
    end;

    TDriver = class(TModule, IDriver)
    public
        constructor Create(const aName: string); override;
    end;


implementation


uses
    CommonTypes;

{ TDriver }

constructor TDriver.Create(const aName: string);
begin
    inherited Create(aName);
end;


end.
