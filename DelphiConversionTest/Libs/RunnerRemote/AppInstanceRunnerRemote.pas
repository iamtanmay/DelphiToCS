{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  23.04.13 wl                                      TN6135   Initial Revision
  ----------------------------------------------------------------------------------------------------------- }

  unit AppInstanceRunnerRemote;


interface


uses
    AppInstanceRunnerLib,
    TCPIPRunnerServer;

type
    TAppInstanceRunnerRemote = class
    private
        fAppInstanceRunner: TAppInstanceRunner;
        fServer: TTCPIPRunnerServer;
    public
        constructor Create();
        destructor Destroy(); override;
    end;


implementation


uses
    SysUtils,
    CommonTypes,
    AppSettings;

{ TAppInstanceRunnerLib }

constructor TAppInstanceRunnerRemote.Create();
var
    xIniAccess: IWinlissyIniAccess;
    xUseTCPIP: boolean;
begin
    inherited Create();

    fAppInstanceRunner := TAppInstanceRunner.Create();

    xIniAccess := TAppSettings.CreateAppIni;
    xUseTCPIP := xIniAccess.ReadBool('RemoteControl', 'UseTCPIPInterface');
    if xUseTCPIP then
    begin
        fServer := TTCPIPRunnerServer.Create(7777);
    end;
end;

destructor TAppInstanceRunnerRemote.Destroy;
begin
    FreeAndNil(fServer);
    FreeAndNil(fAppInstanceRunner);

    inherited;
end;


end.
