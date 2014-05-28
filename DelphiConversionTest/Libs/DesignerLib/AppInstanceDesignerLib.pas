{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  27.08.09 pk                                        TN4753     Initial Revision
  28.08.09 pk  Create                                TN4753     Create instance of TDesignLiquids
  28.08.09 pk                                        TN4753     Uses changed
  31.08.09 pk                                        TN4753     create instance of ModuleDevice instead of PeripheryManagement
  02.09.09 pk                                        TN4753     no longer sets LayoutElementGraphicsDriver.GraphicsType
  28.05.10 wl                                        TN5116     überarbeitet
  07.06.10 wl                                        TN5116     lädt jetzt auch TEditingLayoutManager
  08.08.12 wl                                        TN5946   uses geändert
  29.11.12 wl                                        TN6015.2 DesignDisplayComponents abgeschafft (Es gibt nur noch RunDisplayComponents)
  05.12.12 wl                                        TN6045   DesignLiquids entfernt
  30.08.13 wl                                        TN6236   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceDesignerLib;


interface


uses
    TypeInfo;

type
    TAppInstanceDesignerLib = class
    private
        class var uInstance: TAppInstanceDesignerLib;
        constructor Create();
    public
        destructor Destroy(); override;
        class function CreateInstance(): TAppInstanceDesignerLib;
        class procedure DestroyInstance();
        class property Instance: TAppInstanceDesignerLib read uInstance;
    end;


implementation


uses
    Forms,
    SamGlobe,
    AppSettings,
    LogManager,
    AppInstanceMethodBuilding,
    AppInstanceModuleDevice,
    CommonTypes,
    GUIManager,
    ZADesignLayoutManager,
    GeneralTypes,
    MethodDataCache,
    LiqHDataCache,
    EdExtern,
    EditingLayoutManager,
    LayoutElementGraphicsDriverTypeManager,
    AppInstanceDisplayComponent,
    RunStandardDisplayComponents,
    ViewDisplayComponentPreview,
    ViewConnections;

{ TAppInstanceDesignerLib }

constructor TAppInstanceDesignerLib.Create;
begin
    inherited Create();

    TAppInstanceModuleDevice.CreateInstance();

    TLayoutElementGraphicsDriverTypeManager.CreateInstance();

    TAppInstanceMethodBuilding.CreateInstance();

    TMethodDataCache.CreateInstance();
    TLiqHDataCache.CreateInstance();

    TZADesignLayoutManager.CreateInstance();
    TEditingLayoutManager.CreateInstance();
end;

destructor TAppInstanceDesignerLib.Destroy;
begin
    TEditingLayoutManager.DestroyInstance();
    TZADesignLayoutManager.DestroyInstance();

    TLiqHDataCache.DestroyInstance();
    TMethodDataCache.DestroyInstance();

    TAppInstanceMethodBuilding.DestroyInstance();

    TAppInstanceModuleDevice.DestroyInstance();
    inherited;
end;

class function TAppInstanceDesignerLib.CreateInstance(): TAppInstanceDesignerLib;
begin
    // create instance if instance does not exist
    if not Assigned(uInstance) then
        uInstance := TAppInstanceDesignerLib.Create();

    // return instance
    result := uInstance;
end;

class procedure TAppInstanceDesignerLib.DestroyInstance;
begin
    uInstance.Free;
end;


end.
