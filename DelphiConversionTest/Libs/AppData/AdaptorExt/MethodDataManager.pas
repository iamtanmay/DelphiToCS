{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  17.04.13 wl                                      TN6106   Initial Revision
  14.05.13 wl                                      TN6095   neu: verwendet MethodVarPagesDataAdaptor
  ----------------------------------------------------------------------------------------------------------- }

unit MethodDataManager;


interface


uses
    DataManager;

type
    TMethodDataManager = class(TDataManager)
    public const
        cMethodFileExtension = 'ZAMeth';
    public
        constructor Create;
    end;


implementation


uses
    SysUtils,
    MethodDataAdaptor,
    MethodVariablesDataAdaptor,
    MethodVarPagesDataAdaptor,
    MethodSettingsDataAdaptor;

{ TMethodFileExport }

constructor TMethodDataManager.Create;
begin
    inherited Create(cMethodFileExtension);

    DataAdaptors.Add(TMethodDataAdaptor.Create);
    DataAdaptors.Add(TMethodVariablesDataAdaptor.Create);
    DataAdaptors.Add(TMethodVarPagesDataAdaptor.Create);
    DataAdaptors.Add(TMethodSettingsDataAdaptor.Create);
end;


end.
