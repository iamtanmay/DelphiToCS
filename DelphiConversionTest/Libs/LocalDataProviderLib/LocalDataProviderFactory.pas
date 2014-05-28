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
  03.08.11 wl  GetCFR21UserDBAlias                   TN5647    neu
  08.09.11 wl  Destroy                               TN5672   fDatabaseConfigGroup wird zerstört
  11.09.11 wl  SetTypeDictionary                     TN5672   fTypeDictionary wird außerhalb erzeugt
  19.09.11 wl  CreateDatabaseProvider                TN5672   Neuer Parameter: aOwnsConnectionParams
  27.03.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit LocalDataProviderFactory;


interface


uses
    GeneralTypes,
    DataProvider,
    DatabaseProvider,
    DatabaseProviderManager,
    DatabaseProviderManagerTypeInfo,
    DataProviderFactory,
    DatabaseConfig,
    DataConnectionParams,
    TypeMapTranslator,
    DatabaseProviderManagerTypeDictionary,
    DataConnectionParamEditor;

type
    TLocalDataProviderFactory = class(TDataProviderFactory)
    private const
        cCFR21UserDBAlias = 'ZINSSER_APP';
        cCFR21SystemDataPassword = 'Z6871835150A';
    private
        fDatabaseConfigGroup: TDatabaseConfigGroup;
        fDatabaseProvider: TDatabaseProvider;
        fTypeDictionary: TDatabaseProviderManagerTypeDictionary;
        fMainDBAlias: string;
        fUserDBAlias: string;
        fSystemDataPassword: string;
        function FindDataProviderManagerTypeInfo(const aTypeName: string): TDatabaseProviderManagerTypeInfo;
        procedure CreateDataProviderManagerInstance(const aTypeName: string);
        procedure DestroyDataProviderManagerInstance(const aTypeName: string);

        function GetStandardDBTypeName(): string;
        function GetDataProviderManagerTypeFromTypeName(const aTypeName: string)
            : TDatabaseProviderManagerTypeInfo;
        function DetermineRealTypeName(const aPseudoTypeName: string): string;
        function GetDataProviderManagerTypeNames(): TStringArray;

        procedure Password(Sender: TObject; var Continue: Boolean);
        procedure DefineMainAlias(const aAliasName: string);
        procedure InitializeCFR21UserDatabase();
    protected
        function GetMainDBAlias: string; override;
        function GetUserDBAlias: string; override;
        procedure SetMainDBAlias(const aValue: string); override;
        procedure SetUserDBAlias(const aValue: string); override;
        function GetSystemDataPassword(): string; override;
    public
        constructor Create;
        destructor Destroy; override;
        procedure ReadDatabaseConfigs();
        procedure WriteDatabaseConfigs();

        function CreateDataProvider(const aConnectionParams: TDataConnectionParams): TDataProvider; override;
        function CreateConnectionParamsByAlias(const aAlias: string): TDataConnectionParams; override;
        function CreateDatabaseProvider(const aConnectionParams: TDataConnectionParams;
            aOwnsConnectionParams: boolean): TDatabaseProvider; override;
        function CreateDatabaseProvider(const aAlias: string): TDatabaseProvider; override;
        function CreateDataConnectionParams(const aTypeName: string): TDataConnectionParams; override;
        function CreateDataConnectionParamsEditor(const aTypeName: string): TDataConnectionParamsEditor;
        function GetAllAliasNames(): TStringArray; override;
        function AliasNameExists(const aAlias: string): boolean; override;
        function GetAliasPath(const aAlias: string): string; override;

        procedure Initialize(const aAliasName: string); override;

        procedure ResetPassword(); override;
        function ChangeSystemDataPassword(const aAliasNames: TArray<string>): string; override;

        procedure SetTypeDictionary(aTypeDictionary: TDatabaseProviderManagerTypeDictionary);

        function GetDataProviderManagerInstance(const aTypeName: string): TDatabaseProviderManager;
        function GetDataProviderManagerInstanceBySupportedType(const aTypeName: string)
            : TDatabaseProviderManager;
        function GetDataProviderManagerSupportedTypeNames(): TStringArray;

        procedure CreateDataProviderManagerInstances();
        procedure DestroyDataProviderManagerInstances();

        class function Instance(): TLocalDataProviderFactory;
        class function GetCFR21UserDBAlias: string; static;
        property DatabaseConfigGroup: TDatabaseConfigGroup read fDatabaseConfigGroup;
    end;


implementation


uses
    Generics.Collections,
    SysUtils,
    TypeInfo,
    AppInstanceStartupLib,
    DatabaseConfigReaderWriter;

{ TLocalDataProviderFactory }

constructor TLocalDataProviderFactory.Create;
begin
    inherited Create();

    fDatabaseProvider := nil;
    fDatabaseConfigGroup := nil;
    fTypeDictionary := nil;
end;

destructor TLocalDataProviderFactory.Destroy;
begin
    FreeAndNil(fTypeDictionary);
    FreeAndNil(fDatabaseConfigGroup);
    // fDatabaseProvider darf hier nicht zerstört werden! (warum auch immer)

    inherited;
end;

procedure TLocalDataProviderFactory.Initialize(const aAliasName: string);
begin
    DefineMainAlias(aAliasName);
    InitializeCFR21UserDatabase();
end;

procedure TLocalDataProviderFactory.DefineMainAlias(const aAliasName: string);
var
    xAlias: string;
begin
    // define Main Alias
    xAlias := aAliasName;
    if (xAlias = '') then
        xAlias := TAppInstanceStartup.Instance.StartupConfig.MainDBAlias;

    // set TDataAdaptor
    self.MainDBAlias := xAlias;

    FreeAndNil(fDatabaseProvider);
    fDatabaseProvider := self.CreateDefaultDatabaseProvider();
end;

procedure TLocalDataProviderFactory.InitializeCFR21UserDatabase();
begin
    // Check if Alias name exists
    if self.AliasNameExists(cCFR21UserDBAlias) then
    begin
        self.UserDBAlias := cCFR21UserDBAlias;

        fDatabaseProvider.SetOnPassword(Password);

        fSystemDataPassword := cCFR21SystemDataPassword;
    end;
end;

function TLocalDataProviderFactory.GetMainDBAlias(): string;
begin
    result := fMainDBAlias;
end;

procedure TLocalDataProviderFactory.SetMainDBAlias(const aValue: string);
begin
    fMainDBAlias := aValue;
end;

function TLocalDataProviderFactory.GetUserDBAlias: string;
begin
    result := fUserDBAlias;
end;

procedure TLocalDataProviderFactory.SetUserDBAlias(const aValue: string);
begin
    fUserDBAlias := aValue;
end;

procedure TLocalDataProviderFactory.ResetPassword();
begin
    // Nach den ersten Daten-Aktionen: Password sollte aufgetreten sein
    fSystemDataPassword := '';
end;

procedure TLocalDataProviderFactory.Password(Sender: TObject; var Continue: Boolean);
var
    xPassword: string;
begin
    xPassword := fSystemDataPassword;
    if (xPassword <> '') then
    begin
        // Set Session password (use FCurrentUser)
        fDatabaseProvider.AddSystemPassword(xPassword);
        Continue := true;
    end
    else
    begin
        Continue := false;
        raise Exception.Create('Table could not be opened - Password is not known!');
    end;
end;

function TLocalDataProviderFactory.GetSystemDataPassword(): string;
begin
    result := fSystemDataPassword;
end;

function TLocalDataProviderFactory.ChangeSystemDataPassword(const aAliasNames: TArray<string>): string;
var
    xSystemPW: string;
    x: integer;
begin
    result := '';

    // System data is not configurable - password always set to STR_CFR21_SYSTEMDATA_PASSWORD
    xSystemPW := cCFR21SystemDataPassword;

    // find all paradox tables in 'zinsser_app', 'samintf', ... and set password
    for x := 0 to high(aAliasNames) - 1 do
    begin
        result := fDatabaseProvider.SetAllTablesPassword(aAliasNames[x], xSystemPW);
        if (result <> '') then
            break;
    end;

    // if (result <> '') set password back to '' !
    if (result <> '') then
    begin
        for x := 0 to high(aAliasNames) - 1 do
        begin
            fDatabaseProvider.SetAllTablesPassword(aAliasNames[x], '');
        end;
        exit;
    end;

    // add password to session
    fDatabaseProvider.AddSystemPassword(xSystemPW);
end;

procedure TLocalDataProviderFactory.ReadDatabaseConfigs();
begin
    FreeAndNil(fDatabaseConfigGroup);
    fDatabaseConfigGroup := TDatabaseConfigGroupReaderWriter.ReadDatabaseConfigGroup();
end;

procedure TLocalDataProviderFactory.WriteDatabaseConfigs();
begin
    TDatabaseConfigGroupReaderWriter.WriteDatabaseConfigGroup(fDatabaseConfigGroup);
end;

class function TLocalDataProviderFactory.Instance: TLocalDataProviderFactory;
begin
    result := TDataProviderFactory.Instance as TLocalDataProviderFactory;
end;

function TLocalDataProviderFactory.GetStandardDBTypeName(): string;
var
    xDatabaseConfig: TDatabaseConfig;
begin
    xDatabaseConfig := fDatabaseConfigGroup.List.FindByAlias(self.MainDBAlias);
    ASSERT(Assigned(xDatabaseConfig));
    result := xDatabaseConfig.ConnectionParams.TypeName;
end;

function TLocalDataProviderFactory.DetermineRealTypeName(const aPseudoTypeName: string): string;
begin
    result := aPseudoTypeName;
    if SameText(TDataConnectionUtils.StandardPseudoTypeName, result) then
        result := GetStandardDBTypeName();
end;

function TLocalDataProviderFactory.GetDataProviderManagerTypeFromTypeName(const aTypeName: string)
    : TDatabaseProviderManagerTypeInfo;
var
    xTypeName: string;
    xTypeInfo: TTypeInfo;
begin

    xTypeName := DetermineRealTypeName(aTypeName);

    xTypeInfo := fTypeDictionary.GetTypeFromTypeName(xTypeName);
    ASSERT(xTypeInfo is TDatabaseProviderManagerTypeInfo,
        TTypeSafeFormat.Format('DatabaseProviderManager TypeName [ {0} ] not found', [xTypeName]));
    result := xTypeInfo as TDatabaseProviderManagerTypeInfo;
end;

function TLocalDataProviderFactory.CreateConnectionParamsByAlias(const aAlias: string): TDataConnectionParams;
var
    xDatabaseConfig: TDatabaseConfig;
begin
    xDatabaseConfig := fDatabaseConfigGroup.List.FindByAlias(aAlias);
    ASSERT(Assigned(xDatabaseConfig));
    result := TObjectCopy<TDataConnectionParams>.Copy(xDatabaseConfig.ConnectionParams);
end;

function TLocalDataProviderFactory.FindDataProviderManagerTypeInfo(const aTypeName: string)
    : TDatabaseProviderManagerTypeInfo;
var
    xTypeInfo: TTypeInfo;
begin
    xTypeInfo := self.GetDataProviderManagerTypeFromTypeName(aTypeName);
    ASSERT(xTypeInfo is TDatabaseProviderManagerTypeInfo,
        TTypeSafeFormat.Format('DatabaseProviderManager TypeName [ {0} ] not found', [aTypeName]));
    result := xTypeInfo as TDatabaseProviderManagerTypeInfo;
end;

procedure TLocalDataProviderFactory.CreateDataProviderManagerInstance(const aTypeName: string);
var
    xDatabaseProviderManagerTypeInfo: TDatabaseProviderManagerTypeInfo;
begin
    xDatabaseProviderManagerTypeInfo := FindDataProviderManagerTypeInfo(aTypeName);
    xDatabaseProviderManagerTypeInfo.DatabaseProviderManagerCreator.CreateInstance();
    xDatabaseProviderManagerTypeInfo.DatabaseProviderManagerCreator.Instance.Prepare;
end;

procedure TLocalDataProviderFactory.DestroyDataProviderManagerInstance(const aTypeName: string);
var
    xDatabaseProviderManagerTypeInfo: TDatabaseProviderManagerTypeInfo;
begin
    xDatabaseProviderManagerTypeInfo := FindDataProviderManagerTypeInfo(aTypeName);
    xDatabaseProviderManagerTypeInfo.DatabaseProviderManagerCreator.Instance.UnPrepare;
    xDatabaseProviderManagerTypeInfo.DatabaseProviderManagerCreator.DestroyInstance();
end;

function TLocalDataProviderFactory.GetDataProviderManagerTypeNames(): TStringArray;
begin
    result := fTypeDictionary.ReadAllNames();
end;

procedure TLocalDataProviderFactory.CreateDataProviderManagerInstances;
var
    xTypeNames: TStringArray;
    x: integer;
begin
    xTypeNames := GetDataProviderManagerTypeNames();
    for x := 0 to Length(xTypeNames) - 1 do
    begin
        CreateDataProviderManagerInstance(xTypeNames[x]);
    end;
end;

procedure TLocalDataProviderFactory.SetTypeDictionary(aTypeDictionary
    : TDatabaseProviderManagerTypeDictionary);
begin
    fTypeDictionary := aTypeDictionary;
end;

procedure TLocalDataProviderFactory.DestroyDataProviderManagerInstances;
var
    xTypeNames: TStringArray;
    x: integer;
begin
    xTypeNames := GetDataProviderManagerTypeNames();
    for x := 0 to Length(xTypeNames) - 1 do
    begin
        DestroyDataProviderManagerInstance(xTypeNames[x]);
    end;
end;

function TLocalDataProviderFactory.GetDataProviderManagerInstance(const aTypeName: string)
    : TDatabaseProviderManager;
var
    xDatabaseProviderManagerTypeInfo: TDatabaseProviderManagerTypeInfo;
begin
    xDatabaseProviderManagerTypeInfo := FindDataProviderManagerTypeInfo(aTypeName);
    result := xDatabaseProviderManagerTypeInfo.DatabaseProviderManagerCreator.Instance;
end;

function TLocalDataProviderFactory.GetDataProviderManagerSupportedTypeNames(): TStringArray;
var
    xStringKeyList: TList<string>;
    xTypeNames: TStringArray;
    xCurrentTypeNames: TStringArray;
    x: integer;
begin
    xTypeNames := GetDataProviderManagerTypeNames();

    xStringKeyList := TList<string>.Create();
    try

        for x := 0 to Length(xTypeNames) - 1 do
        begin
            xCurrentTypeNames := GetDataProviderManagerInstance(xTypeNames[x]).GetSupportedTypeNames;

            xStringKeyList.AddRange(xCurrentTypeNames);
        end;

        result := xStringKeyList.ToArray;
    finally
        FreeAndNil(xStringKeyList);
    end;
end;

function TLocalDataProviderFactory.GetDataProviderManagerInstanceBySupportedType(const aTypeName: string)
    : TDatabaseProviderManager;

var
    xStringKeyList: TList<string>;
    xTypeNames: TStringArray;
    xCurrentTypeNames: TStringArray;
    x: integer;
    xDatabaseProviderManager: TDatabaseProviderManager;
    xTypeName: string;
begin
    xTypeName := DetermineRealTypeName(aTypeName);

    result := nil;

    xTypeNames := GetDataProviderManagerTypeNames();

    xStringKeyList := TList<string>.Create();
    try

        for x := 0 to Length(xTypeNames) - 1 do
        begin
            xDatabaseProviderManager := GetDataProviderManagerInstance(xTypeNames[x]);
            xStringKeyList.Clear();
            xCurrentTypeNames := xDatabaseProviderManager.GetSupportedTypeNames;
            xStringKeyList.AddRange(xCurrentTypeNames);
            if xStringKeyList.IndexOf(xTypeName) >= 0 then
            begin
                result := xDatabaseProviderManager;
                EXIT;
            end;
        end;
    finally
        FreeAndNil(xStringKeyList);
    end;
end;

function TLocalDataProviderFactory.CreateDataProvider(const aConnectionParams: TDataConnectionParams)
    : TDataProvider;
var
    xDatabaseProviderManager: TDatabaseProviderManager;
begin
    xDatabaseProviderManager := GetDataProviderManagerInstanceBySupportedType(aConnectionParams.TypeName);
    xDatabaseProviderManager.PrepareDatabase(aConnectionParams);
    result := xDatabaseProviderManager.CreateDataProvider(aConnectionParams);
end;

function TLocalDataProviderFactory.CreateDatabaseProvider(const aConnectionParams: TDataConnectionParams;
    aOwnsConnectionParams: boolean): TDatabaseProvider;
var
    xDatabaseProviderManager: TDatabaseProviderManager;
begin
    xDatabaseProviderManager := GetDataProviderManagerInstanceBySupportedType(aConnectionParams.TypeName);
    xDatabaseProviderManager.PrepareDatabase(aConnectionParams);
    result := xDatabaseProviderManager.CreateDatabaseProvider(aConnectionParams, aOwnsConnectionParams);

end;

function TLocalDataProviderFactory.CreateDatabaseProvider(const aAlias: string): TDatabaseProvider;
var
    xConnectionParams: TDataConnectionParams;
begin
    xConnectionParams := CreateConnectionParamsByAlias(aAlias);
    result := CreateDatabaseProvider(xConnectionParams, true);
end;

function TLocalDataProviderFactory.CreateDataConnectionParams(const aTypeName: string): TDataConnectionParams;
var
    xDatabaseProviderManager: TDatabaseProviderManager;
    xTypeName: string;
begin
    xTypeName := DetermineRealTypeName(aTypeName);
    xDatabaseProviderManager := GetDataProviderManagerInstanceBySupportedType(xTypeName);
    result := xDatabaseProviderManager.CreateDataConnectionParams(xTypeName);
end;

function TLocalDataProviderFactory.CreateDataConnectionParamsEditor(const aTypeName: string)
    : TDataConnectionParamsEditor;
var
    xDatabaseProviderManager: TDatabaseProviderManager;
    xTypeName: string;
begin
    xTypeName := DetermineRealTypeName(aTypeName);
    xDatabaseProviderManager := GetDataProviderManagerInstanceBySupportedType(xTypeName);
    result := xDatabaseProviderManager.CreateDataConnectionParamsEditor(xTypeName);
end;

function TLocalDataProviderFactory.GetAllAliasNames(): TStringArray;
begin
    result := fDatabaseConfigGroup.GetDatabaseAliasNames();
end;

class function TLocalDataProviderFactory.GetCFR21UserDBAlias: string;
begin
    EXIT(cCFR21UserDBAlias);
end;

function TLocalDataProviderFactory.AliasNameExists(const aAlias: string): boolean;
begin
    result := Assigned(fDatabaseConfigGroup.List.FindByAlias(aAlias));
end;

function TLocalDataProviderFactory.GetAliasPath(const aAlias: string): string;
var
    xDatabaseConfig: TDatabaseConfig;
begin
    xDatabaseConfig := fDatabaseConfigGroup.List.FindByAlias(aAlias);
    ASSERT(Assigned(xDatabaseConfig));
    result := xDatabaseConfig.ConnectionParams.DataLocation;
end;


end.
