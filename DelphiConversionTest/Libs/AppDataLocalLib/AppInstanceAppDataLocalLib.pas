{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  19.10.10 pk                                        TN5305     Initial revision
  11.09.11 wl  fInstancesStarter                     TN5672   Instanz wird hier verwaltet
  11.09.11 wl  uInstance                             TN5672   entfernt
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceAppDataLocalLib;


interface


uses
    CommonTypes,
    AppDataInstancesStarter,
    AppInstanceLocalDataProviderLib;

type
    TLocalAppDataInstancesStarterHelper = class(TAppDataInstancesStarterHelper)
    private
        fAppInstanceLocalDataProvider: TAppInstanceLocalDataProvider;
    public
        procedure DataProviderFactoryCreateInstance(); override;
        procedure DataProviderFactoryDestroyInstance(); override;
    end;

    TAppInstanceAppDataLocal = class
    private
        fInstancesStarter: TAppDataInstancesStarter;
    public
        constructor Create(aPurpose: TAppPurpose; aConnectionType: TMachineConnectionType;
            const aAppHInstance: LongWord);
        destructor Destroy(); override;
    end;


implementation


uses
    SysUtils,
    LocalDataProviderFactory,
    DatabaseProviderManagerTypeDictionary;

{ TAppInstanceAppDataLocal }

constructor TAppInstanceAppDataLocal.Create(aPurpose: TAppPurpose; aConnectionType: TMachineConnectionType;
    const aAppHInstance: LongWord);
begin
    inherited Create();

    fInstancesStarter := TAppDataInstancesStarter.Create(aPurpose, aConnectionType, aAppHInstance,
        TLocalAppDataInstancesStarterHelper.Create());
end;

destructor TAppInstanceAppDataLocal.Destroy;
begin
    FreeAndNil(fInstancesStarter);

    inherited;
end;

{ TLocalAppDataInstancesStarterHelper }

procedure TLocalAppDataInstancesStarterHelper.DataProviderFactoryCreateInstance;
begin
    fAppInstanceLocalDataProvider := TAppInstanceLocalDataProvider.Create;
    fAppInstanceLocalDataProvider.SetTypeDictionary(TDatabaseProviderManagerTypeDictionary.Create(true));
end;

procedure TLocalAppDataInstancesStarterHelper.DataProviderFactoryDestroyInstance;
begin
    FreeAndNil(fAppInstanceLocalDataProvider);
end;


end.
