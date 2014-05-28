{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.08.10 pk                                        TN5218     Initial revision
  05.08.10 pk                                        TN5221     Changes needed for Component Ace Absolute Database
  04.08.11 wl  TDataConnectionParamList.this         TN5647    Params heißt jetzt this
  26.09.13 pbp                                       TN6261    ORACLE Verbindung hinzufügen
  29.10.13 pbp                                       TN6291    ORACLE LDAP Verbindungsparameter hinzufügen
  ----------------------------------------------------------------------------------------------------------------------- }

unit DataConnectionParams;


interface


uses
    GeneralTypes,
    Streamable;

type
    TDataConnectionUtils = class
    private const
        cStandardPsuedoTypeName = 'Standard';
        class function GetStandardPseudoTypeName: string; static;
    public
        class property StandardPseudoTypeName: string read GetStandardPseudoTypeName;
    end;

    TDataConnectionParamValue = TStreamableItem;

    TDataConnectionParam = class(TStreamable)
    private
        fKey: string;
        fValue: TDataConnectionParamValue;
    public
        constructor Create(const aKey: string; const aValue: TDataConnectionParamValue); reintroduce;
        destructor Destroy(); override;
    published
        property Key: string read fKey write fKey;
        property Value: TDataConnectionParamValue read fValue write fValue;
    end;

    TDataConnectionParamPathValue = class(TStrStreamableItem);

    TDataConnectionParamList = class(TStreamableObjectList)
    private
        function GetParamAt(aIndex: integer): TDataConnectionParam;
    public
        function FindParamByKey(const aKey: string): TDataConnectionParam;
        property this[aIndex: integer]: TDataConnectionParam read GetParamAt; default;
    end;

    TDataConnectionParams = class(TCustomStreamable)
    private const
        cKeyTypeName = 'TypeName';

    const
        cKeyDataLocation = 'DataLocation';
    protected
        fParams: TDataConnectionParamList;
        procedure AddParam(const aKey: string; const aValue: TDataConnectionParamValue); overload;

        procedure AddStrParam(const aKey: string; const aValue: string); overload;
        procedure AddIntParam(const aKey: string; const aValue: integer);
        procedure AddBoolParam(const aKey: string; const aValue: boolean);

        function GetDataLocation: string;
        function GetTypeName: string;
        procedure SetDataLocation(const aValue: string);
        procedure SetTypeName(const aValue: string);
        procedure SetProps(const aTypeName: string; const aDataLocation: string);
    public
        constructor Create(); overload; override;
        constructor Create(const aTypeName: string; const aDataLocation: string); reintroduce; overload;
        destructor Destroy(); override;
        function ParamValueByName(const aKey: string): TDataConnectionParamValue;
        property TypeName: string read GetTypeName write SetTypeName;
        property DataLocation: string read GetDataLocation write SetDataLocation;
    published
        property Params: TDataConnectionParamList read fParams write fParams;
    end;

    TBDEDataConnectionParams = class(TDataConnectionParams)
    private const

        cKeyMultipleDataLocations = 'MultipleDataLocations';

        cKeyUsername = 'Username';
        cKeyPassword = 'Password';
        cKeySystemDataPassword = 'SystemDataPassword';
        cKeySessionName = 'SessionName';
    private
        function GetDataLocations: TStringArray;
        function GetPassword: string;
        function GetSessionName: string;
        function GetSystemDataPassword: string;
        function GetUsername: string;
        procedure SetDataLocations(const aValue: TStringArray);
        procedure SetPassword(const aValue: string);
        procedure SetSessionName(const aValue: string);
        procedure SetSystemDataPassword(const aValue: string);
        procedure SetUsername(const aValue: string);
    public
        // constructor Create(); overload; override;
        constructor Create(const aTypeName: string; const aDataLocation: string); reintroduce; overload;
        destructor Destroy; override;
        property DataLocations: TStringArray read GetDataLocations write SetDataLocations;
        property Username: string read GetUsername write SetUsername;
        property Password: string read GetPassword write SetPassword;
        property SystemDataPassword: string read GetSystemDataPassword write SetSystemDataPassword;
        property SessionName: string read GetSessionName write SetSessionName;
    end;

    TTDBDataConnectionParams = class(TDataConnectionParams)
    end;

    TACRDBDataConnectionParams = class(TDataConnectionParams);
    TABSDBDataConnectionParams = class(TDataConnectionParams);

    TMSAccessDataConnectionParams = class(TDataConnectionParams)
    end;

    TMSExcelDataConnectionParams = class(TDataConnectionParams)
    private const
        cKeyHasHeader = 'HasHeader';
    private
        function GetHasHeader: boolean;
        procedure SetHasHeader(const aValue: boolean);
    public
        constructor Create(const aTypeName, aDataLocation: string); reintroduce;
        destructor Destroy; override;
        property HasHeader: boolean read GetHasHeader write SetHasHeader;
    end;

    TCSVDataConnectionParams = class(TDataConnectionParams)
    private const
        cKeyTableName = 'TableName';
        cKeyDelimiter = 'Delimiter';
        cKeyHasHeader = 'HasHeader';
    private
        function GetDelimiter: string;
        function GetHasHeader: boolean;
        function GetTableName: string;
        procedure SetDelimiter(const aValue: string);
        procedure SetHasHeader(const aValue: boolean);
        procedure SetTableName(const aValue: string);
    public
        constructor Create(const aTypeName, aDataLocation: string); reintroduce;
        destructor Destroy; override;
        property TableName: string read GetTableName write SetTableName;
        property Delimiter: string read GetDelimiter write SetDelimiter;
        property HasHeader: boolean read GetHasHeader write SetHasHeader;
    end;

    TORACLEDataConnectionParams = class(TDataConnectionParams)
    private const
        cKeyUsername = 'Username';
        cKeyPassword = 'Password';
        cKeyProvider = 'Provider';
        cKeyIP = 'IP';
        cKeyPort = 'Port';
        cKeyDatabaseName = 'Database Name';
        cKeyLdap = 'Ldap Connection';
        cKeyAttributes = 'Attributes CSV';
        cKeyConnect = 'Connection Name';
        cKeyEncrypt = 'Encrypt pw';
        cKeyLdapFilter = 'Ldap Filter';
        cKeyScope = 'Scope';
        cKeyDNnames = 'DN Names';
    private
        function GetUsername: string;
        function GetPassword: string;
        function GetProvider: string;
        function GetIP: string;
        function GetPort: string;
        function GetDatabaseName: string;
        function GetLdapValue: boolean;
        function GetEncryptValue: boolean;
        function GetConnectName: string;
        function GetLdapFilter: string;
        function GetAttributes: string;
        function GetScope: string;
        function GetDNnames: string;
        procedure SetUsername(const aUsername: string);
        procedure SetPassword(const aPassword: string);
        procedure SetProvider(const aProvider: string);
        procedure SetIP(const aIP: string);
        procedure SetPort(const aPort: string);
        procedure SetDatabaseName(const aDatabaseName: string);
        procedure SetLdapValue(const aLdap: boolean);
        procedure SetEncryptValue(const aEncrypt: boolean);
        procedure SetConnectName(const aConnect: string);
        procedure SetLdapFilter(const aLdap: string);
        procedure SetAttributes(const aAttributes: string);
        procedure SetScope(const aScope: string);
        procedure SetDNnames(const aDNnames: string);
    public
        constructor Create(const aTypeName, aDataLocation: string); reintroduce; overload;
        destructor Destroy; override;
        property Username: string read GetUsername write SetUsername;
        property Password: string read GetPassword write SetPassword;
        property Provider: string read GetProvider write SetProvider;
        property IP: string read GetIP write SetIP;
        property Port: string read GetPort write SetPort;
        property DatabaseName: string read GetDatabaseName write SetDatabaseName;
        property Ldap: boolean read GetLdapValue write SetLdapValue;
        property Encrypt: boolean read GetEncryptValue write SetEncryptValue;
        property ConnectName: string read GetConnectName write SetConnectName;
        property LdapFilter: string read GetLdapFilter write SetLdapFilter;
        property Attributes: string read GetAttributes write SetAttributes;
        property Scope: string read GetScope write SetScope;
        property DNnames: string read GetDNnames write SetDNnames;
    end;


implementation


uses
    SysUtils;

{ TDataConnectionParam }

constructor TDataConnectionParam.Create(const aKey: string; const aValue: TDataConnectionParamValue);
begin
    inherited Create();
    fKey := aKey;
    fValue := aValue;
end;

destructor TDataConnectionParam.Destroy;
begin
    FreeAndNil(fValue);
    inherited;
end;

{ TDataConnectionParamList }

function TDataConnectionParamList.FindParamByKey(const aKey: string): TDataConnectionParam;
var
    x: integer;
begin
    result := nil;
    for x := 0 to self.Count - 1 do
    begin
        if SameText(self[x].Key, aKey) then
        begin
            result := self[x];
            EXIT;
        end;
    end;
end;

function TDataConnectionParamList.GetParamAt(aIndex: integer): TDataConnectionParam;
begin
    result := ( inherited Items[aIndex]) as TDataConnectionParam;
end;

{ TDataConnectionParams }

constructor TDataConnectionParams.Create();
begin
    inherited Create();
end;

constructor TDataConnectionParams.Create(const aTypeName: string; const aDataLocation: string);
begin
    inherited Create();
    fParams := TDataConnectionParamList.Create();
    AddParam(cKeyTypeName, TStrStreamableItem.Create());
    AddParam(cKeyDataLocation, TDataConnectionParamPathValue.Create());
    SetProps(aTypeName, aDataLocation);

end;

destructor TDataConnectionParams.Destroy;
begin
    FreeAndNil(fParams);
    inherited;
end;

procedure TDataConnectionParams.SetProps(const aTypeName: string; const aDataLocation: string);
begin
    self.TypeName := aTypeName;
    self.DataLocation := aDataLocation;
end;

procedure TDataConnectionParams.AddParam(const aKey: string; const aValue: TDataConnectionParamValue);
begin
    fParams.Add(TDataConnectionParam.Create(aKey, aValue));
end;

procedure TDataConnectionParams.AddStrParam(const aKey: string; const aValue: string);
begin
    self.AddParam(aKey, TStrStreamableItem.Create(aValue));
end;

procedure TDataConnectionParams.AddIntParam(const aKey: string; const aValue: integer);
begin
    self.AddParam(aKey, TIntStreamableItem.Create(aValue));
end;

procedure TDataConnectionParams.AddBoolParam(const aKey: string; const aValue: boolean);
begin
    self.AddParam(aKey, TBoolStreamableItem.Create(aValue));
end;

function TDataConnectionParams.ParamValueByName(const aKey: string): TDataConnectionParamValue;
begin
    result := fParams.FindParamByKey(aKey).Value;
end;

function TDataConnectionParams.GetDataLocation: string;
begin
    result := ParamValueByName(cKeyDataLocation).AsStr;
end;

function TDataConnectionParams.GetTypeName: string;
begin
    result := ParamValueByName(cKeyTypeName).AsStr;
end;

procedure TDataConnectionParams.SetDataLocation(const aValue: string);
begin
    self.ParamValueByName(cKeyDataLocation).AsStr := aValue
end;

procedure TDataConnectionParams.SetTypeName(const aValue: string);
begin
    self.ParamValueByName(cKeyTypeName).AsStr := aValue;
end;

{ TDataConnectionUtils }

class function TDataConnectionUtils.GetStandardPseudoTypeName: string;
begin
    result := cStandardPsuedoTypeName;
end;

constructor TBDEDataConnectionParams.Create(const aTypeName, aDataLocation: string);
begin
    inherited Create(aTypeName, aDataLocation);
    self.AddParam(cKeyMultipleDataLocations, TStreamableStringList.Create());
    self.AddStrParam(cKeyUsername, '');
    self.AddStrParam(cKeyPassword, '');
    self.AddStrParam(cKeySystemDataPassword, '');
    self.AddStrParam(cKeySessionName, '');
end;

destructor TBDEDataConnectionParams.Destroy();
begin

    inherited;
end;

function TBDEDataConnectionParams.GetDataLocations: TStringArray;
begin
    result := self.ParamValueByName(cKeyMultipleDataLocations).AsStrArray;
end;

function TBDEDataConnectionParams.GetPassword: string;
begin
    result := self.ParamValueByName(cKeyPassword).AsStr;
end;

function TBDEDataConnectionParams.GetSessionName: string;
begin
    result := self.ParamValueByName(cKeySessionName).AsStr;
end;

function TBDEDataConnectionParams.GetSystemDataPassword: string;
begin
    result := self.ParamValueByName(cKeySystemDataPassword).AsStr;
end;

function TBDEDataConnectionParams.GetUsername: string;
begin
    result := self.ParamValueByName(cKeyUsername).AsStr;
end;

procedure TBDEDataConnectionParams.SetDataLocations(const aValue: TStringArray);
begin
    self.ParamValueByName(cKeyMultipleDataLocations).AsStrArray := aValue
end;

procedure TBDEDataConnectionParams.SetPassword(const aValue: string);
begin
    self.ParamValueByName(cKeyPassword).AsStr := aValue;
end;

procedure TBDEDataConnectionParams.SetSessionName(const aValue: string);
begin
    self.ParamValueByName(cKeySessionName).AsStr := aValue;
end;

procedure TBDEDataConnectionParams.SetSystemDataPassword(const aValue: string);
begin
    self.ParamValueByName(cKeySystemDataPassword).AsStr := aValue;
end;

procedure TBDEDataConnectionParams.SetUsername(const aValue: string);
begin
    self.ParamValueByName(cKeyUserName).AsStr := aValue;
end;

{ TCSVDataConnectionParams }

constructor TCSVDataConnectionParams.Create(const aTypeName, aDataLocation: string);
begin
    inherited Create(aTypeName, aDataLocation);

    self.AddStrParam(cKeyTableName, '');
    self.AddStrParam(cKeyDelimiter, '');
    self.AddBoolParam(cKeyHasHeader, true);
end;

destructor TCSVDataConnectionParams.Destroy();
begin

    inherited;
end;

function TCSVDataConnectionParams.GetDelimiter: string;
begin
    result := self.ParamValueByName(cKeyDelimiter).AsStr;
end;

function TCSVDataConnectionParams.GetHasHeader: boolean;
begin
    result := self.ParamValueByName(cKeyHasHeader).AsBool;
end;

function TCSVDataConnectionParams.GetTableName: string;
begin
    result := self.ParamValueByName(cKeyTableName).AsStr;
end;

procedure TCSVDataConnectionParams.SetDelimiter(const aValue: string);
begin
    self.ParamValueByName(cKeyDelimiter).AsStr := aValue;
end;

procedure TCSVDataConnectionParams.SetHasHeader(const aValue: boolean);
begin
    self.ParamValueByName(cKeyHasHeader).AsBool := aValue;
end;

procedure TCSVDataConnectionParams.SetTableName(const aValue: string);
begin
    self.ParamValueByName(cKeyTableName).AsStr := aValue;
end;

{ TMSExcelDataConnectionParams }

constructor TMSExcelDataConnectionParams.Create(const aTypeName, aDataLocation: string);
begin
    inherited Create(aTypeName, aDataLocation);

    self.AddBoolParam(cKeyHasHeader, true);
end;

destructor TMSExcelDataConnectionParams.Destroy();
begin
    inherited;
end;

function TMSExcelDataConnectionParams.GetHasHeader: boolean;
begin
    result := self.ParamValueByName(cKeyHasHeader).AsBool;
end;

procedure TMSExcelDataConnectionParams.SetHasHeader(const aValue: boolean);
begin
    self.ParamValueByName(cKeyHasHeader).AsBool := aValue;
end;

{ TORACLEDataConnectionParams }

constructor TORACLEDataConnectionParams.Create(const aTypeName, aDataLocation: string);
begin
    inherited Create(aTypeName, aDataLocation);
    self.AddStrParam(cKeyUsername, '');
    self.AddStrParam(cKeyPassword, '');
    self.AddStrParam(cKeyProvider, '');
    self.AddStrParam(cKeyIP, '');
    self.AddStrParam(cKeyPort, '');
    self.AddStrParam(cKeyDatabaseName, '');
    self.AddBoolParam(cKeyLdap, false);
    self.AddStrParam(cKeyAttributes, '');
    self.AddStrParam(cKeyConnect, '');
    self.AddBoolParam(cKeyEncrypt, true);
    self.AddStrParam(cKeyLdapFilter, '');
    self.AddStrParam(cKeyScope, '');
    self.AddStrParam(cKeyDNnames, '');
end;

destructor TORACLEDataConnectionParams.Destroy;
begin

    inherited;
end;

function TORACLEDataConnectionParams.GetAttributes: string;
begin
    result := self.ParamValueByName(cKeyAttributes).AsStr;
end;

function TORACLEDataConnectionParams.GetConnectName: string;
begin
    result := self.ParamValueByName(cKeyConnect).AsStr;
end;

function TORACLEDataConnectionParams.GetDatabaseName: string;
begin
    result := self.ParamValueByName(cKeyDatabaseName).AsStr;
end;

function TORACLEDataConnectionParams.GetDNnames: string;
begin
    result := self.ParamValueByName(cKeyDNnames).AsStr;
end;

function TORACLEDataConnectionParams.GetEncryptValue: boolean;
begin
    result := self.ParamValueByName(cKeyEncrypt).AsBool;
end;

function TORACLEDataConnectionParams.GetIP: string;
begin
    result := self.ParamValueByName(cKeyIP).AsStr;
end;

function TORACLEDataConnectionParams.GetLdapFilter: string;
begin
    result := self.ParamValueByName(cKeyLdapFilter).AsStr;
end;

function TORACLEDataConnectionParams.GetLdapValue: boolean;
begin
    result := self.ParamValueByName(cKeyLdap).AsBool;
end;

function TORACLEDataConnectionParams.GetPassword: string;
begin
    result := self.ParamValueByName(cKeyPassword).AsStr;
end;

function TORACLEDataConnectionParams.GetPort: string;
begin
    result := self.ParamValueByName(cKeyPort).AsStr;
end;

function TORACLEDataConnectionParams.GetProvider: string;
begin
    result := self.ParamValueByName(cKeyProvider).AsStr;
end;

function TORACLEDataConnectionParams.GetScope: string;
begin
    result := self.ParamValueByName(cKeyScope).AsStr;
end;

function TORACLEDataConnectionParams.GetUsername: string;
begin
    result := self.ParamValueByName(cKeyUsername).AsStr;
end;

procedure TORACLEDataConnectionParams.SetAttributes(const aAttributes: string);
begin
    self.ParamValueByName(cKeyAttributes).AsStr := aAttributes;
end;

procedure TORACLEDataConnectionParams.SetConnectName(const aConnect: string);
begin
    self.ParamValueByName(cKeyConnect).AsStr := aConnect;
end;

procedure TORACLEDataConnectionParams.SetDatabaseName(const aDatabaseName: string);
begin
    self.ParamValueByName(cKeyDatabaseName).AsStr := aDatabaseName;
end;

procedure TORACLEDataConnectionParams.SetDNnames(const aDNnames: string);
begin
    self.ParamValueByName(cKeyDNnames).AsStr := aDNnames;
end;

procedure TORACLEDataConnectionParams.SetEncryptValue(const aEncrypt: boolean);
begin
    self.ParamValueByName(cKeyEncrypt).AsBool := aEncrypt;
end;

procedure TORACLEDataConnectionParams.SetIP(const aIP: string);
begin
    self.ParamValueByName(cKeyIP).AsStr := aIP;
end;

procedure TORACLEDataConnectionParams.SetLdapFilter(const aLdap: string);
begin
    self.ParamValueByName(cKeyLdapFilter).AsStr := aLdap;
end;

procedure TORACLEDataConnectionParams.SetLdapValue(const aLdap: boolean);
begin
    self.ParamValueByName(cKeyLdap).AsBool := aLdap;
end;

procedure TORACLEDataConnectionParams.SetPassword(const aPassword: string);
begin
    self.ParamValueByName(cKeyPassword).AsStr := aPassword;
end;

procedure TORACLEDataConnectionParams.SetPort(const aPort: string);
begin
    self.ParamValueByName(cKeyPort).AsStr := aPort;
end;

procedure TORACLEDataConnectionParams.SetProvider(const aProvider: string);
begin
    self.ParamValueByName(cKeyProvider).AsStr := aProvider;
end;

procedure TORACLEDataConnectionParams.SetScope(const aScope: string);
begin
    self.ParamValueByName(cKeyScope).AsStr := aScope;
end;

procedure TORACLEDataConnectionParams.SetUsername(const aUsername: string);
begin
    self.ParamValueByName(cKeyUsername).AsStr := aUsername;
end;


end.
