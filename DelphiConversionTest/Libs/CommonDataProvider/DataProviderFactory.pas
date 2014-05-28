{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.06.10 pk                                        TN5152.1  initial revision
  04.08.10 pk                                        TN5218    Changes for Plugin database packages
  19.10.10 pk                                        TN5305    changes needed for CoreClient/Server
  15.02.11 pk  CopyTable                             TN4780    new
  03.03.11 wl  CopyTable                             TN5493    VR-Dateien können jetzt mitkopiert werden
  19.09.11 wl  CreateDatabaseProvider                TN5672    Neuer Parameter: aOwnsConnectionParams
  19.09.11 wl  CopyTable                             TN5672    Sicherstellen, dass die TDataConnectionParams-Instanzen auch zerstört werden
  27.03.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit DataProviderFactory;


interface


uses
    GeneralTypes,
    DataConnectionParams,
    DataProvider,
    DatabaseProvider;

type
    TDataProviderFactory = class
    private
        class var uDataProviderFactoryInstance: TDataProviderFactory;
    protected
        function GetMainDBAlias(): string; virtual; abstract;
        procedure SetMainDBAlias(const aValue: string); virtual; abstract;
        function GetUserDBAlias(): string; virtual; abstract;
        procedure SetUserDBAlias(const aValue: string); virtual; abstract;
        function GetSystemDataPassword(): string; virtual; abstract;
    public
        constructor Create();
        procedure Initialize(const aAliasName: string); virtual;

        function CreateDataProvider(): TDataProvider; overload; virtual;
        function CreateDataProvider(const aConnectionParms: TDataConnectionParams): TDataProvider; overload;
            virtual; abstract;
        function CreateConnectionParamsByAlias(const aAlias: string): TDataConnectionParams; virtual;
            abstract;

        function CreateDataProvider(const aAlias: string): TDataProvider; overload;
        function CreateDatabaseProvider(const aConnectionParms: TDataConnectionParams;
            aOwnsConnectionParams: boolean): TDatabaseProvider; overload; virtual; abstract;
        function CreateDatabaseProvider(const aAlias: string): TDatabaseProvider; overload; virtual; abstract;
        function CreateDataConnectionParams(const aTypeName: string): TDataConnectionParams; virtual;
            abstract;

        function CreateDefaultDatabaseProvider(): TDatabaseProvider;

        procedure CopyTable(var vTargetDataConnectionParams: TDataConnectionParams;
            const aTargetTableName: string; var vSourceDataConnectionParams: TDataConnectionParams;
            const aSourceTableName: string; aCopyVersionInfo: boolean);

        function GetAllAliasNames(): TStringArray; virtual; abstract;
        function AliasNameExists(const aAlias: string): boolean; virtual; abstract;
        function GetAliasPath(const aAlias: string): string; virtual; abstract;
        procedure ResetPassword(); virtual; abstract;
        function ChangeSystemDataPassword(const aAliasNames: TArray<string>): string; virtual; abstract;

        property MainDBAlias: string read GetMainDBAlias write SetMainDBAlias;
        property UserDBAlias: string read GetUserDBAlias write SetUserDBAlias;
        property SystemDataPassword: string read GetSystemDataPassword;

        class procedure SetInstance(const aInstance: TDataProviderFactory);
        class procedure DestroyInstance();
        class function Instance(): TDataProviderFactory;
    end;


implementation


uses
    SysUtils,
    StreamableDatasetClasses,
    TypeMapTranslator,
    FileUtilities;

{ TDataProviderFactory }

constructor TDataProviderFactory.Create();
begin
    inherited Create();
end;

function TDataProviderFactory.CreateDataProvider: TDataProvider;
begin
    result := CreateDataProvider(GetMainDBAlias());
end;

function TDataProviderFactory.CreateDataProvider(const aAlias: string): TDataProvider;
begin
    result := CreateDataProvider(self.CreateConnectionParamsByAlias(aAlias));
end;

function TDataProviderFactory.CreateDefaultDatabaseProvider(): TDatabaseProvider;
begin
    result := CreateDatabaseProvider(GetMainDBAlias());
end;

procedure TDataProviderFactory.CopyTable(var vTargetDataConnectionParams: TDataConnectionParams;
    const aTargetTableName: string; var vSourceDataConnectionParams: TDataConnectionParams;
    const aSourceTableName: string; aCopyVersionInfo: boolean);
var
    xSourceDataProvider, xTargetDataProvider: TDataProvider;
    xTableDef: TStreamableTableDef;
    xSourceDatabaseProvider, xTargetDatabaseProvider: TDatabaseProvider;
begin
    xSourceDatabaseProvider := CreateDatabaseProvider(vSourceDataConnectionParams, false);
    try
        if not xSourceDatabaseProvider.TableExists(aSourceTableName) then
        begin
            FreeAndNil(vSourceDataConnectionParams);
            FreeAndNil(vTargetDataConnectionParams);
            EXIT;
        end;

        xTargetDatabaseProvider := CreateDatabaseProvider(vTargetDataConnectionParams, false);
        xTargetDatabaseProvider.DeleteTable(aTargetTableName);
        try
            xTableDef := xSourceDatabaseProvider.ReadTableDef(aSourceTableName);
            try
                xTableDef.IndexDefs.NameUnnamedIndices();
                xTargetDatabaseProvider.CreateTable(xTableDef);
            finally
                FreeAndNil(xTableDef);
            end;
        finally
            FreeAndNil(xTargetDatabaseProvider);
        end;
    finally
        FreeAndNil(xSourceDatabaseProvider);
    end;

    if (aCopyVersionInfo) then
    begin
        TFileUtilities.CopyFile(TFileUtilities.ConcatPaths(vSourceDataConnectionParams.DataLocation,
            aSourceTableName + '.VR'), TFileUtilities.ConcatPaths(vTargetDataConnectionParams.DataLocation,
            aTargetTableName + '.VR'), false);
    end;

    xSourceDataProvider := CreateDataProvider(vSourceDataConnectionParams);
    try
        xTargetDataProvider := CreateDataProvider(vTargetDataConnectionParams);
        try
            xTargetDataProvider.CopyRecordsFrom(aTargetTableName, xSourceDataProvider, aSourceTableName);
        finally
            FreeAndNil(xTargetDataProvider);
        end;
    finally
        FreeAndNil(xSourceDataProvider);
    end;
end;

class procedure TDataProviderFactory.DestroyInstance;
begin
    FreeAndNil(uDataProviderFactoryInstance);
end;

procedure TDataProviderFactory.Initialize(const aAliasName: string);
begin

end;

class function TDataProviderFactory.Instance: TDataProviderFactory;
begin
    result := uDataProviderFactoryInstance;
end;

class procedure TDataProviderFactory.SetInstance(const aInstance: TDataProviderFactory);
begin
    uDataProviderFactoryInstance := aInstance;
end;


end.
