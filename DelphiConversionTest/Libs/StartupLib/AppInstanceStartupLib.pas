{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.08.10 pk                                        TN5218     Initial revision
  08.09.11 wl  Create                                TN5672   CreateStartupConfig statt ReadStartupConfig
  11.09.11 wl                                        TN5672   TAppInstanceStartup statt TAppInstanceStartupLib
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceStartupLib;


interface


uses
    ListClasses,
    DataProvider,
    DatabaseProvider,
    StartupConfig;

type
    TAppInstanceStartup = class
    private
        fStartupConfig: TStartupConfig;

        class var uInstance: TAppInstanceStartup; // Single Instance
        class function GetInstance(): TAppInstanceStartup; static;

        constructor Create();
        function GetAppDataPath: string;
    public
        destructor Destroy(); override;
        class procedure CreateInstance();
        class procedure DestroyInstance();

        property AppDataPath: string read GetAppDataPath;
        property StartupConfig: TStartupConfig read fStartupConfig;
        class property Instance: TAppInstanceStartup read GetInstance;
    end;


implementation


uses
    SysUtils;

{ TAppInstanceStartup }

class procedure TAppInstanceStartup.CreateInstance();
begin
    // create instance if instance does not exist
    if not Assigned(uInstance) then
        uInstance := TAppInstanceStartup.Create();
end;

function TAppInstanceStartup.GetAppDataPath: string;
begin
    result := fStartupConfig.DataLocation;
end;

class function TAppInstanceStartup.GetInstance: TAppInstanceStartup;
begin
    result := uInstance;
end;

constructor TAppInstanceStartup.Create();
begin
    inherited Create();
    fStartupConfig := TStartupConfigReaderWriter.CreateStartupConfig();
end;

destructor TAppInstanceStartup.Destroy;
begin
    FreeAndNil(fStartupConfig);
    inherited;
end;

class procedure TAppInstanceStartup.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;


end.
