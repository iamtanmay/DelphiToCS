unit RunDisplayComponentManager;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  27.08.09 pk                                        TN4753      Initial Revision
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    DisplayComponentManager,
    DisplayComponentSettingsManager;

type
    TRunDisplayComponentManager = class(TDisplayComponentManager)
    public
        class procedure CreateInstance();
        class procedure DestroyInstance;
        class function Instance: TRunDisplayComponentManager;
    end;


implementation


uses
    DisplayComponentTypeDictionary,
    DisplayComponentDataCache;

var
    uRunDisplayComponentManager: TRunDisplayComponentManager = nil;

class procedure TRunDisplayComponentManager.CreateInstance();
begin
    if Assigned(uRunDisplayComponentManager) then
        EXIT;
    uRunDisplayComponentManager := TRunDisplayComponentManager.Create
        (TRunDisplayComponentTypeDictionary.Instance, TDisplayComponentSettingsManager.Instance);
end;

class procedure TRunDisplayComponentManager.DestroyInstance();
begin
    uRunDisplayComponentManager.Free;
end;

class function TRunDisplayComponentManager.Instance(): TRunDisplayComponentManager;
begin
    result := uRunDisplayComponentManager;
end;


end.
