{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  27.08.09 pk                                        TN4753    Initial Revision
  09.06.10 pk                                        TN5116    create TSystemLiquidManagerExt with TRunSystemLiquidSettingsFinder
  18.06.12 wl  TAppInstancePeripheryManagementExt    TN5899   TLiquids-Instanz wird unabhängig von gSysLiqManager erzeugt und zerstört
  05.11.12 wl  Destroy                               TN6006   WriteAllVolumes mit Parameter SimulationMode
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstancePeripheryManagementExt;


interface


uses
    TypeInfo;

type
    TAppInstancePeripheryManagementExt = class
    private
        class var uInstance: TAppInstancePeripheryManagementExt;
        constructor Create();
    public
        destructor Destroy(); override;
        class function CreateInstance(): TAppInstancePeripheryManagementExt;
        class procedure DestroyInstance();
        class function Instance(): TAppInstancePeripheryManagementExt;
    end;


implementation


uses
    SysUtils,
    LiquidManager,
    LiquidManagerExt,
    Liquids,
    RunFlow;

{ TAppInstancePeripheryManagementExt }

constructor TAppInstancePeripheryManagementExt.Create;
begin
    inherited Create();
    TLiquids.CreateInstance;
    gSysLiqManager := TSystemLiquidManagerExt.Create(TRunSystemLiquidSettingsFinder.Create());
end;

destructor TAppInstancePeripheryManagementExt.Destroy;
begin
    // etwas spät, das sollte beim Entladen der Devices erfolgen
    TLiquids.Instance.WriteAllVolumes(gRunFlow.SimulationMode);

    FreeAndNil(gSysLiqManager);
    TLiquids.DestroyInstance;
    inherited;
end;

class function TAppInstancePeripheryManagementExt.CreateInstance(): TAppInstancePeripheryManagementExt;
begin
    // create instance if instance does not exist
    if not Assigned(uInstance) then
        uInstance := TAppInstancePeripheryManagementExt.Create();

    // return instance
    result := uInstance;
end;

class function TAppInstancePeripheryManagementExt.Instance(): TAppInstancePeripheryManagementExt;
begin
    result := uInstance;
end;

class procedure TAppInstancePeripheryManagementExt.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;


end.
