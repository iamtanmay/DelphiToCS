{ --------------------------------------------------------------------------------------------------
  Copyright  2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Pseudo DataProviderFactory for Updater
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  15.02.11 pk                               TN4780   Initial Revision
  03.03.11 wl  CopyTable                    TN5493   aCopyVersionInfo = true, damit VR-Dateien kopiert werden
  19.09.11 wl  CreateDatabaseProvider       TN5672    Neuer Parameter: aOwnsConnectionParams
  -------------------------------------------------------------------------------------------------- }

unit UpdaterDataProviderFactory;


interface


uses
    DataProvider,
    DatabaseProvider,
    LocalDataProviderFactory,
    UpdaterTableProvider,
    DataConnectionParams;

type
    TUpdaterDataProviderFactory = class
    private
        class function CreateStandardDataConnectionParams(const aDataLocation: string): TDataConnectionParams;
    public
        class procedure CopyTable(const aTargetDBPath, aTargetTableName, aSourceDBPath,
            aSourceTableName: string);
        class function CreateStandardDataProvider(const aDataLocation: string): TDataProvider;
        class function CreateStandardDatabaseProvider(const aDataLocation: string): TDatabaseProvider;
    end;


implementation


{ TUpdaterDataProviderFactory }

class function TUpdaterDataProviderFactory.CreateStandardDataConnectionParams(const aDataLocation: string)
    : TDataConnectionParams;
var
    xTypeName: string;
    xConnectionParams: TDataConnectionParams;
begin
    xTypeName := TDataConnectionUtils.StandardPseudoTypeName;
    xConnectionParams := TLocalDataProviderFactory.Instance.CreateDataConnectionParams(xTypeName);
    xConnectionParams.DataLocation := aDataLocation;
    result := xConnectionParams;
end;

class function TUpdaterDataProviderFactory.CreateStandardDataProvider(const aDataLocation: string)
    : TDataProvider;
var
    xConnectionParams: TDataConnectionParams;
begin
    xConnectionParams := CreateStandardDataConnectionParams(aDataLocation);
    result := TLocalDataProviderFactory.Instance.CreateDataProvider(xConnectionParams);
end;

class function TUpdaterDataProviderFactory.CreateStandardDatabaseProvider(const aDataLocation: string)
    : TDatabaseProvider;
var
    xConnectionParams: TDataConnectionParams;
begin
    xConnectionParams := CreateStandardDataConnectionParams(aDataLocation);
    result := TLocalDataProviderFactory.Instance.CreateDatabaseProvider(xConnectionParams, true);
end;

class procedure TUpdaterDataProviderFactory.CopyTable(const aTargetDBPath, aTargetTableName, aSourceDBPath,
    aSourceTableName: string);
var
    xTargetConnectionParams, xSourceConnectionParams: TDataConnectionParams;
begin
    xTargetConnectionParams := CreateStandardDataConnectionParams(aTargetDBPath);
    xSourceConnectionParams := CreateStandardDataConnectionParams(aSourceDBPath);

    TLocalDataProviderFactory.Instance.CopyTable(xTargetConnectionParams, aTargetTableName,
        xSourceConnectionParams, aSourceTableName, true);
end;


end.
