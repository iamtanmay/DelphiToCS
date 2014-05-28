{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.08.10 pk                                        TN5218     Initial revision
  19.10.10 pk                                        TN5305     changes needed for CoreClient/Server
  29.11.10 pk                                        TN5368     AppDataPath removed
  11.09.11 wl                                        TN5672   Instance entfernt
  11.09.11 wl  SetTypeDictionary                     TN5672   TypeDictionary wird auﬂerhalb erzeugt
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceLocalDataProviderLib;


interface


uses
    DatabaseProviderManagerTypeDictionary;

type
    TAppInstanceLocalDataProvider = class
    public
        constructor Create;
        destructor Destroy(); override;
        procedure SetTypeDictionary(aTypeDictionary: TDatabaseProviderManagerTypeDictionary);
    end;


implementation


uses
    SysUtils,
    PluginLoader,
    AppInstanceStartupLib,
    LocalDataProviderFactory,
    DataProviderFactory;

{ TAppInstanceLocalDataProvider }

constructor TAppInstanceLocalDataProvider.Create;
begin
    inherited Create();

    TDataProviderFactory.SetInstance(TLocalDataProviderFactory.Create);

    TAppInstanceStartup.CreateInstance();

    TPluginLoader.CreateInstance();
    TPluginLoader.Instance.LoadDatabasePlugins();

    TLocalDataProviderFactory.Instance.ReadDatabaseConfigs();
end;

procedure TAppInstanceLocalDataProvider.SetTypeDictionary(aTypeDictionary
    : TDatabaseProviderManagerTypeDictionary);
begin
    TLocalDataProviderFactory.Instance.SetTypeDictionary(aTypeDictionary);
    TLocalDataProviderFactory.Instance.CreateDataProviderManagerInstances();
end;

destructor TAppInstanceLocalDataProvider.Destroy;
begin
    TLocalDataProviderFactory.Instance.DestroyDataProviderManagerInstances();

    TPluginLoader.DestroyInstance();

    TAppInstanceStartup.DestroyInstance();

    TDataProviderFactory.DestroyInstance();

    inherited;
end;


end.
