{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  27.08.09 pk                                        TN4753    Initial Revision
  28.08.09 pk                                        TN4753    LiquidManager removed
  31.08.09 pk                                        TN4753    Only creates instance of ModuleDevice
  21.07.10 pk                                        TN5203    Create AppInstanceAppDataAdaptor
  03.11.11 wl  Create, Destroy                       TN5725   es wird eine Instanz von TSubstanceLoading erzeugt
  30.08.13 wl                                        TN6236   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstancePeripheryManagement;


interface


type
    TAppInstancePeripheryManagement = class
    private
        class var uInstance: TAppInstancePeripheryManagement;
        constructor Create();
    public
        destructor Destroy(); override;
        class procedure CreateInstance;
        class procedure DestroyInstance();
        class property Instance: TAppInstancePeripheryManagement read uInstance;
    end;


implementation


uses
    SysUtils,
    AppInstanceModuleDevice,
    SubstanceLoading;

{ TAppInstancePeripheryManagement }

constructor TAppInstancePeripheryManagement.Create;
begin
    inherited Create();
    TAppInstanceModuleDevice.CreateInstance();
    TSubstanceLoading.CreateInstance();
end;

destructor TAppInstancePeripheryManagement.Destroy;
begin
    TSubstanceLoading.DestroyInstance();
    TAppInstanceModuleDevice.DestroyInstance();
    inherited;
end;

class procedure TAppInstancePeripheryManagement.CreateInstance();
begin
    // create instance if instance does not exist
    if not Assigned(uInstance) then
        uInstance := TAppInstancePeripheryManagement.Create();
end;

class procedure TAppInstancePeripheryManagement.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;


end.
