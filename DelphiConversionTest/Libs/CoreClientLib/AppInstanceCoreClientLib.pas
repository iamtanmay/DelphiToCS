{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.06.09 pk                                        TN4585.2    Initial Revision
  06.07.09 pk                                        TN4585.4    Various changes
  30.07.09 pk  TCoreClientSettingsRec                TN4585.4    IsNoController, IsNoEventHost
  10.08.09 wl                                        TN4702   Strings werden jetzt direkt geladen
  27.08.09 pk                                        TN4753   Creates instance of AppCommon instead of RunCommon
  08.09.09 pk                                        TN4753   Does not create AppCommon Instance
  21.09.09 pk  Create                                TN4753   Create TAppInstanceLogging
  26.10.09 wl  ReadClientSettings                    TN4831   IConfigurationSet replaces TIniFile
  20.05.10 wl  Instance                              TN5116   Instance-Funktionen geändert
  17.06.10 pk                                        TN5152.1 uses DataProviderFactory
  12.11.12 wl  ReadClientSettings                    TN6008   wenn keine Adresse angegeben, nimm die lokale IP
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceCoreClientLib;


interface


type
    TCoreClientSettingsRec = record
        ServerAddress: string;
        ServerPort: integer;
        EventHostPort: integer;

        PluginPath: string;

        IsNoController: boolean;
        IsNoEventHost: boolean;
    end;

    TAppInstanceCoreClientLib = class
    private const
        cCoreClientSettingsSectionsApp = 'App';
        cCoreClientSettingsIdentServerAddress = 'ServerAddress';
        cCoreClientSettingsIdentServerPort = 'ServerPort';
        cCoreClientSettingsIdentEventHostPort = 'EventHostPort';

        cStartParamKeyIsNOController = 'NOCONTROLLER';
        cStartParamKeyIsNoEventHost = 'NOEVENTHOST';
    private
        fCoreClientSettingsRec: TCoreClientSettingsRec;
        class var uInstCoreClientLib: TAppInstanceCoreClientLib;
        constructor Create();
        class function ReadClientSettings: TCoreClientSettingsRec; static;
        procedure ReadStartParameters;
    public
        destructor Destroy(); override;
        property CoreClientSettingsRec: TCoreClientSettingsRec read fCoreClientSettingsRec;
        class function CreateInstance(): boolean;
        class procedure DestroyInstance();
        class property Instance: TAppInstanceCoreClientLib read uInstCoreClientLib;
    end;


implementation


uses
    SysUtils,
    Windows,
    DialogUtils,
    UtilLib,
    CoreControllerClientCalls,
    TCPIPCoreClient,
    TCPIPCoreControllerClientCalls,
    AppInstanceLogging,
    DataProviderFactory,
    CoreClientDataProvider,
    ConfigurationFile,
    PluginLoader;

{ TAppInstanceCoreClientLib }

constructor TAppInstanceCoreClientLib.Create();
var
    xCoreClientCalls: TTCPIPCoreControllerClientCalls;
    xCoreClient: TTCPIPCoreClient;
begin
    inherited Create();

    TAppInstanceLogging.CreateInstance(true);
    fCoreClientSettingsRec := ReadClientSettings();
    ReadStartParameters();

    TPluginLoader.SetPluginPath(fCoreClientSettingsRec.PluginPath);

    xCoreClient := TTCPIPCoreClient.Create();
    TCoreClient.Instance := xCoreClient;
    xCoreClientCalls := TTCPIPCoreControllerClientCalls.Create(xCoreClient);
    TCoreControllerClientCalls.Instance := xCoreClientCalls;

    TCoreClient.Instance.Connect(fCoreClientSettingsRec.ServerAddress, fCoreClientSettingsRec.ServerPort);

    TDataProviderFactory.SetInstance(TCoreClientDataProviderFactory.Create
        (TTCPIPCoreClient.Instance.RemoteFunctionCaller));

end;

destructor TAppInstanceCoreClientLib.Destroy;
begin
    TCoreControllerClientCalls.DestroyInstance();
    TTCPIPCoreClient.DestroyInstance();
    TAppInstanceLogging.DestroyInstance();
    inherited;
end;

procedure TAppInstanceCoreClientLib.ReadStartParameters();
var
    xCommandLineParam: string;
    x: integer;
begin
    fCoreClientSettingsRec.IsNoController := false;
    fCoreClientSettingsRec.IsNoEventHost := false;

    for x := 1 to System.ParamCount do
    begin
        xCommandLineParam := UpperCase(System.ParamStr(x));
        if SameText(cStartParamKeyIsNoController, xCommandLineParam) then
            fCoreClientSettingsRec.IsNoController := true
        else if SameText(cStartParamKeyIsNoEventHost, xCommandLineParam) then
            fCoreClientSettingsRec.IsNoEventHost := true
    end;

end;

class function TAppInstanceCoreClientLib.ReadClientSettings(): TCoreClientSettingsRec;

var
    xIniFile: IConfigurationSet;
    xServerAddress: string;
begin
    xIniFile := TConfigurationFile.Create('.\Startup.ini');
    xIniFile.Open(true);
    try
        xServerAddress := xIniFile.ReadString(cCoreClientSettingsSectionsApp,
            cCoreClientSettingsIdentServerAddress, '');

        // wenn keine Adresse angegeben, nimm die lokale IP:
        if (xServerAddress = '') then
        begin
            xServerAddress := TIPInfo.GetLocalIP;
        end;
        result.ServerAddress := xServerAddress;

        result.ServerPort := xIniFile.ReadInteger(cCoreClientSettingsSectionsApp,
            cCoreClientSettingsIdentServerPort, 0);
        result.EventHostPort := xIniFile.ReadInteger(cCoreClientSettingsSectionsApp,
            cCoreClientSettingsIdentEventHostPort, 0);

        result.PluginPath := xIniFile.ReadString(cCoreClientSettingsSectionsApp, 'PluginPath', '');
    finally
        xIniFile.Close;
    end;
end;

class function TAppInstanceCoreClientLib.CreateInstance(): boolean;
begin
    result := true;
    try
        // create instance if instance does not exist
        if not Assigned(uInstCoreClientLib) then
            uInstCoreClientLib := TAppInstanceCoreClientLib.Create();

    except
        on E: exception do
        begin
            result := false;
            TDialogUtils.MessageBox(E.Message, 'Error', MB_ICONERROR);
        end;
    end;
end;

class procedure TAppInstanceCoreClientLib.DestroyInstance;
begin
    uInstCoreClientLib.Free;
end;


end.
