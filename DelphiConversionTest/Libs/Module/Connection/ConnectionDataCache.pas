{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : wrapper for connection settings cache
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  13.10.08 pk                               TN4272.2  Initial Revision
  ----------------------------------------------------------------------------------------------------------------------- }

unit ConnectionDataCache;


interface


uses
    ModuleDataCache;

type
    TConnectionDataCache = class(TModuleDataCache)

    end;

    TConnectionsDataCache = class(TModulesDataCache)
    protected
        function GetAreaName(): string; override;
    public
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class function Instance(): TConnectionsDataCache;
    end;


implementation


uses
    Connection;

var
    uConnectionsDataCache: TConnectionsDataCache = nil;

    { TAllConnectionDataCache }

class procedure TConnectionsDataCache.CreateInstance;
begin
    uConnectionsDataCache := TConnectionsDataCache.Create();
end;

class procedure TConnectionsDataCache.DestroyInstance;
begin
    uConnectionsDataCache.Free;
end;

function TConnectionsDataCache.GetAreaName: string;
begin
    result := STR_SETTINGS_AREA_CONNECTION;
end;

class function TConnectionsDataCache.Instance: TConnectionsDataCache;
begin
    result := uConnectionsDataCache;
end;


end.
