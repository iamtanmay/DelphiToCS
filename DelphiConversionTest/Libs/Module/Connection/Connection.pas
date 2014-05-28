{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Base class for all Connection classes
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                           track-no  improvement/change
  -------- --  -------------------------------  --------- ----------------------------------------------
  05.09.07 pk                                             Initial Revision
  09.11.07 pk  fOnCheckSimulated                TN3864    New
  06.05.08 pk  OnCheckSimulated                 TN3864    declared public
  06.05.08 pk  GetIsSimulated                   TN3864    now virtual
  26.05.08 wl  AreaName                         TN4119    removed
  16.11.09 pk  GetIsConnectNeeded	           TN4870	 New: Checks Simulation and enabled
  02.11.10 pk  Init/ResetConnection             TN5235    New
  27.03.13 wl                                   TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit Connection;


interface


uses
    Classes,
    Module,
    ModuleSettings,
    ModuleTypeInfo,
    ConnectionSettingList;

const
    STR_SETTINGS_AREA_CONNECTION = 'CONNECTION';

type
    TConnectionTypeInfo = class(TModuleTypeInfo);

    TOnCheckSimulated = function(aSender: TObject): boolean of object;

    IConnection = interface(IModule)
        ['{21BECE19-4703-4DFE-BBF0-C07A8CA66892}']
        function GetIsSimulated(): boolean;
        procedure SetOnCheckSimulated(aOnCheckSimulated: TOnCheckSimulated);
        procedure InitConnection();
        procedure ResetConnection();
        property IsSimulated: boolean read GetIsSimulated;
        property OnCheckSimulated: TOnCheckSimulated write SetOnCheckSimulated;
    end;

    TConnection = class(TModule, IModule)
    private
        function DoOnCheckSimulated: boolean;
        function GetEnabled: boolean;
    protected
        fOnCheckSimulated: TOnCheckSimulated;
        procedure SetOnCheckSimulated(aOnCheckSimulated: TOnCheckSimulated);
        function GetIsSimulated(): boolean; virtual;
        function GetIsConnectNeeded(): boolean; override;
        function DoGetEnabled(): boolean; virtual;
        procedure DoInitConnection(); virtual;
        procedure DoResetConnection(); virtual;
    public
        constructor Create(const aName: string); override;
        procedure ReadSettings(aSettings: TModuleSettingList); virtual;
        procedure Prepare(); override;
        procedure InitConnection();
        procedure ResetConnection();
        property OnCheckSimulated: TOnCheckSimulated write SetOnCheckSimulated;
        property Enabled: boolean read GetEnabled;
        property IsSimulated: boolean read GetIsSimulated;
    end;


implementation


uses
    CommonTypes;

{ TConnection }

constructor TConnection.Create(const aName: string);
begin
    inherited Create(aName);
    fOnCheckSimulated := nil;
end;

procedure TConnection.ReadSettings(aSettings: TModuleSettingList);
begin

end;

procedure TConnection.DoInitConnection();
begin

end;

procedure TConnection.InitConnection;
begin
    DoInitConnection;
end;

procedure TConnection.ResetConnection;
begin
    DoResetConnection;
end;

function TConnection.DoGetEnabled: boolean;
begin
    result := true;
end;

function TConnection.GetEnabled: boolean;
begin
    result := (fSettings as TConnectionSettingList).Enabled and DoGetEnabled();
end;

function TConnection.DoOnCheckSimulated(): boolean;
begin
    result := true;
    if not Assigned(fOnCheckSimulated) then
        EXIT;
    result := fOnCheckSimulated(self);
end;

procedure TConnection.DoResetConnection;
begin
    // should be implemented if a special reset is required for the entire connection
end;

function TConnection.GetIsConnectNeeded: boolean;
begin
    result := inherited GetIsConnectNeeded();
    result := result and (not self.IsSimulated);
end;

function TConnection.GetIsSimulated: boolean;
begin
    result := (not self.Enabled) or DoOnCheckSimulated();
end;

procedure TConnection.Prepare();
begin
    inherited;
end;

procedure TConnection.SetOnCheckSimulated(aOnCheckSimulated: TOnCheckSimulated);
begin
    fOnCheckSimulated := aOnCheckSimulated;
end;


end.
