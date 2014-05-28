{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : IModule is the minimum interface that must be implemted by all Connection, Driver, and Devices
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.01.08 pk                               TN3864  New Prepare, UnPrepare, Disconnect
  14.04.08 wl                               TN4060   removed: TModuleVersion, TModuleTypeName
  06.05.08 wl  ModuleSettings               TN4068   neue Property
  26.05.08 wl  TModule.Destroy              TN4119   destroys fSettings
  26.05.08 wl  TModule.AreaName             TN4119   removed
  10.07.08 pk  IModule.ModuleSettings       TN4163  SetModuleSetting added
  18.12.08 pk  fConnected                   TN4372  New
  19.11.10 wl  Activate,Deactivate          TN5358  neu
  27.01.11 wl  GetSettingsClass             TN5357   entfernt (wurde schon lange nicht mehr gebraucht)
  -------------------------------------------------------------------------------------------------- }

unit Module;


interface


uses
    GeneralTypes,
    ModuleSettings;

type
    IModule = interface(IInterface)
        ['{0DEBF368-1A21-43B3-9163-3F2AB3BD1568}']
        procedure Prepare();
        procedure UnPrepare();
        procedure Connect();
        procedure Disconnect();
        procedure Activate();
        procedure Deactivate();
        function GetName(): string;
        procedure SetModuleSettings(aSettings: TModuleSettingList);
        function GetModuleSettings(): TModuleSettingList;

        property name: string read GetName;
        property ModuleSettings: TModuleSettingList read GetModuleSettings write SetModuleSettings;
    end;

    TModuleClass = class of TModule;

    TModule = class(TInterfacedObject, IModule)
    protected
        fName: string;
        fSettings: TModuleSettingList;
        fPrepared: boolean;
        fConnected: boolean;
        function GetModuleSettings(): TModuleSettingList;
        procedure SetModuleSettings(aSettings: TModuleSettingList);
        function GetName(): string;
        function GetIsConnectNeeded(): boolean; virtual;
        procedure DoConnect(); virtual;
        procedure DoDisconnect(); virtual;
    public
        constructor Create(const aName: string); virtual;
        destructor Destroy(); override;
        //
        procedure Prepare(); virtual;
        procedure UnPrepare(); virtual;
        procedure Connect();
        procedure Disconnect();
        procedure Activate(); virtual;
        procedure Deactivate(); virtual;

        property name: string read GetName;
        property ModuleSettings: TModuleSettingList read GetModuleSettings write SetModuleSettings;
    end;


implementation


uses
    SysUtils;

{ TModule }

constructor TModule.Create(const aName: string);
begin
    inherited Create();
    fName := aName;
    fSettings := nil;
    fPrepared := false;
    fConnected := false;
end;

destructor TModule.Destroy;
begin
    fSettings.Free;
    inherited;
end;

procedure TModule.SetModuleSettings(aSettings: TModuleSettingList);
begin
    fSettings := aSettings;
end;

function TModule.GetName: string;
begin
    result := fName;
end;

procedure TModule.Prepare();
begin
    if fPrepared then
        EXIT;

    fSettings.Prepare();
    fPrepared := true;
end;

procedure TModule.UnPrepare;
begin
    fPrepared := false;
end;

procedure TModule.Activate;
begin

end;

procedure TModule.Deactivate;
begin

end;

procedure TModule.Connect;
begin
    if not GetIsConnectNeeded() then
        EXIT;
    DoConnect();
    fConnected := true;
end;

procedure TModule.Disconnect;
begin
    DoDisconnect();
    fConnected := false;
end;

function TModule.GetModuleSettings: TModuleSettingList;
begin
    result := fSettings;
end;

function TModule.GetIsConnectNeeded: boolean;
begin
    result := not fConnected;
end;

procedure TModule.DoConnect;
begin

end;

procedure TModule.DoDisconnect;
begin

end;


end.
