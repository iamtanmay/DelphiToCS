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
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceAppDataRemoteLib;


interface


uses
    CommonTypes,
    AppDataInstancesStarter;

type
    TRemoteAppDataInstancesStarterHelper = class(TAppDataInstancesStarterHelper)
    public
        procedure DataProviderFactoryCreateInstance(); override;
        procedure DataProviderFactoryDestroyInstance(); override;
    end;

    TAppInstanceAppDataRemoteLib = class
    private
        fInstancesStarter: TAppDataInstancesStarter;

        class var uInstance: TAppInstanceAppDataRemoteLib; // Single Instance
        class function GetInstance(): TAppInstanceAppDataRemoteLib; static;

        constructor Create(aPurpose: TAppPurpose; aConnectionType: TMachineConnectionType;
            const aAppHInstance: LongWord);
    public
        destructor Destroy(); override;
        class procedure CreateInstance(aPurpose: TAppPurpose; aConnectionType: TMachineConnectionType;
            const aAppHInstance: LongWord);
        class procedure DestroyInstance();

        class property Instance: TAppInstanceAppDataRemoteLib read GetInstance;
    end;


implementation


uses
    SysUtils;

{ TAppInstanceAppDataRemoteLib }

class procedure TAppInstanceAppDataRemoteLib.CreateInstance(aPurpose: TAppPurpose;
    aConnectionType: TMachineConnectionType; const aAppHInstance: LongWord);
begin
    // create instance if instance does not exist
    if not Assigned(uInstance) then
        uInstance := TAppInstanceAppDataRemoteLib.Create(aPurpose, aConnectionType, aAppHInstance);
end;

class function TAppInstanceAppDataRemoteLib.GetInstance: TAppInstanceAppDataRemoteLib;
begin
    result := uInstance;
end;

constructor TAppInstanceAppDataRemoteLib.Create(aPurpose: TAppPurpose;
    aConnectionType: TMachineConnectionType; const aAppHInstance: LongWord);
begin
    inherited Create();

    fInstancesStarter := TAppDataInstancesStarter.Create(aPurpose, aConnectionType, aAppHInstance,
        TRemoteAppDataInstancesStarterHelper.Create());
end;

destructor TAppInstanceAppDataRemoteLib.Destroy;
begin
    FreeAndNil(fInstancesStarter);

    inherited;
end;

class procedure TAppInstanceAppDataRemoteLib.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;

{ TRemoteAppDataInstancesStarterHelper }

procedure TRemoteAppDataInstancesStarterHelper.DataProviderFactoryCreateInstance;
begin
end;

procedure TRemoteAppDataInstancesStarterHelper.DataProviderFactoryDestroyInstance;
begin
end;


end.
