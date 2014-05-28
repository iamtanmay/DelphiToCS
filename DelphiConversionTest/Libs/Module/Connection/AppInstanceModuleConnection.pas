{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : App Instance for Module Connections
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  13.10.08 pk                               TN4272.2  Initial Revision
  31.08.09 pk  Create                       TN4753    create SettingsMangaer, LoadTypes
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceModuleConnection;


interface


type
    TAppInstanceModuleConnection = class
    private
        constructor Create();
    public
        destructor Destroy(); override;
        class function CreateInstance(): TAppInstanceModuleConnection;
        class procedure DestroyInstance();
        class function Instance(): TAppInstanceModuleConnection;
    end;


implementation


uses
    ConnectionDataCache,
    ConnectionSettingsManager,
    PluginLoader,
    ConnectionTypeDictionary;

var
    uInstModuleConnection: TAppInstanceModuleConnection;

    { TAppInstanceModuleConnection }

constructor TAppInstanceModuleConnection.Create;
begin
    inherited Create();
    TConnectionsDataCache.CreateInstance();
    TConnectionSettingsManager.CreateInstance();
    TPluginLoader.LoadAllTypes(gConnectionTypeDictionary);
end;

destructor TAppInstanceModuleConnection.Destroy;
begin
    TConnectionSettingsManager.DestroyInstance();
    TConnectionsDataCache.DestroyInstance();
    inherited;
end;

class function TAppInstanceModuleConnection.CreateInstance(): TAppInstanceModuleConnection;
begin
    if not Assigned(uInstModuleConnection) then
        uInstModuleConnection := TAppInstanceModuleConnection.Create();

    result := uInstModuleConnection;
end;

class function TAppInstanceModuleConnection.Instance(): TAppInstanceModuleConnection;
begin
    result := uInstModuleConnection;
end;

class procedure TAppInstanceModuleConnection.DestroyInstance;
begin
    uInstModuleConnection.Free;
end;


end.
