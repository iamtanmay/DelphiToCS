{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.08.10 pk                                        TN5218    Initial revision
  21.03.13 wl                                        TN6045    verwendet Generics.Collections
  26.09.13 pbp                                       TN6261    ORACLE Verbindung hinzufügen
  29.10.13 pbp                                       TN6291    ORACLE LDAP Verbindungsparameter hinzufügen
  ----------------------------------------------------------------------------------------------------------------------- }

unit DataConnectionParamEditor;


interface


uses
    Generics.Collections,
    GeneralTypes,
    DataConnectionParams;

type
    TDataConnectionParamEditor = class
    protected
        fKey: string;
        fParams: TDataConnectionParamList;
        fUserInputRequired: boolean;
        fUserInputAllowed: boolean;
        fUserInputVisible: boolean;
        fRepositoryIndex: integer;
        fData: TObject;
        function GetParam(): TDataConnectionParam;
        function GetValue: TDataConnectionParamValue;
        property Param: TDataConnectionParam read GetParam;
        procedure DoAssignParams(const aValue: TDataConnectionParamList); virtual;
    public
        constructor Create(const aKey: string; const aUserInputRequired: boolean); reintroduce; overload;
        constructor Create(const aKey: string; const aUserInputRequired, aUserInputAllowed,
            aUserInputVisible: boolean); reintroduce; overload;
        destructor Destroy(); override;
        function ReadListItems: TStringArray; virtual;
        function HasEditFunction(): boolean; virtual;
        procedure AssignParams(const aValue: TDataConnectionParamList);
        property Key: string read fKey write fKey;
        property Value: TDataConnectionParamValue read GetValue;
        property UserInputRequired: boolean read fUserInputRequired write fUserInputRequired;
        property UserInputAllowed: boolean read fUserInputAllowed write fUserInputAllowed;
        property UserInputVisible: boolean read fUserInputVisible write fUserInputVisible;
        property RepositoryIndex: integer read fRepositoryIndex write fRepositoryIndex;
        property Data: TObject read fData write fData;

    end;

    TDataConnectionPathParamEditor = class(TDataConnectionParamEditor)
    public
        function HasEditFunction(): boolean; override;
    end;

    TDataConnectionCompositeParamEditor = class(TDataConnectionParamEditor)
    private
        fChildEditors: TObjectList<TDataConnectionParamEditor>;
    protected
        procedure DoAssignParams(const aValue: TDataConnectionParamList); override;
    public
        constructor Create(const aKey: string);
        destructor Destroy(); override;
        procedure AddParam(const aParam: TDataConnectionParamEditor); overload;
        procedure AddParam(const aKey: string; const aUserInputRequired, aUserInputAllowed,
            aUserInputVisible: boolean); overload;
        property ChildEditors: TObjectList<TDataConnectionParamEditor>read fChildEditors;
    end;

    TDataConnectionParamsEditor = class(TDataConnectionCompositeParamEditor)
    private const
        cKeyTypeName = 'TypeName';
        cKeyDataLocation = 'DataLocation';
    public
        constructor Create();
    end;

    TBDEDataConnectionParamsEditor = class(TDataConnectionParamsEditor)
    private const
        cKeyMultipleDataLocations = 'MultipleDataLocations';
        cKeyUsername = 'Username';
        cKeyPassword = 'Password';
        cKeySystemDataPassword = 'SystemDataPassword';
        cKeySessionName = 'SessionName';
    public
        constructor Create();
    end;

    TMSAccessDataConnectionParamsEditor = class(TDataConnectionParamsEditor)
    end;

    TMSExcelDataConnectionParamsEditor = class(TDataConnectionParamsEditor)
    private const
        cKeyHasHeader = 'HasHeader';
    public
        constructor Create();
    end;

    TCSVDataConnectionParamsEditor = class(TDataConnectionParamsEditor)
    private const
        cKeyTableName = 'TableName';
        cKeyDelimiter = 'Delimiter';
        cKeyHasHeader = 'HasHeader';
    public
        constructor Create();
    end;

    TORACLEDataConnectionParamsEditor = class(TDataConnectionParamsEditor)
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
    public
        constructor Create();
    end;


implementation


uses
    SysUtils;

{ TDataConnectionParamEditor }

procedure TDataConnectionParamEditor.DoAssignParams(const aValue: TDataConnectionParamList);
begin
    fParams := aValue;
end;

procedure TDataConnectionParamEditor.AssignParams(const aValue: TDataConnectionParamList);
begin
    DoAssignParams(aValue);
end;

constructor TDataConnectionParamEditor.Create(const aKey: string; const aUserInputRequired, aUserInputAllowed,
    aUserInputVisible: boolean);
begin
    inherited Create();
    fKey := aKey;
    fUserInputRequired := aUserInputRequired;
    fUserInputAllowed := aUserInputAllowed;
    fUserInputVisible := aUserInputVisible;
end;

constructor TDataConnectionParamEditor.Create(const aKey: string; const aUserInputRequired: boolean);
begin
    Create(aKey, aUserInputRequired, aUserInputRequired, aUserInputRequired);
end;

destructor TDataConnectionParamEditor.Destroy;
begin
    inherited;
end;

function TDataConnectionParamEditor.HasEditFunction: boolean;
begin
    result := false;
end;

function TDataConnectionParamEditor.ReadListItems: TStringArray;
begin
    SetLength(result, 0);
end;

function TDataConnectionParamEditor.GetParam: TDataConnectionParam;
begin
    result := fParams.FindParamByKey(fKey);
end;

function TDataConnectionParamEditor.GetValue: TDataConnectionParamValue;
begin
    result := self.Param.Value;
end;

{ TDataConnectionCompositeParamEditor }

constructor TDataConnectionCompositeParamEditor.Create(const aKey: string);
begin
    inherited Create(aKey, true);
    fChildEditors := TObjectList<TDataConnectionParamEditor>.Create();
end;

destructor TDataConnectionCompositeParamEditor.Destroy;
begin
    FreeAndNil(fChildEditors);
    inherited;
end;

procedure TDataConnectionCompositeParamEditor.AddParam(const aParam: TDataConnectionParamEditor);
begin
    fChildEditors.Add(aParam);
end;

procedure TDataConnectionCompositeParamEditor.AddParam(const aKey: string;
    const aUserInputRequired, aUserInputAllowed, aUserInputVisible: boolean);
begin
    AddParam(TDataConnectionParamEditor.Create(aKey, aUserInputRequired, aUserInputAllowed,
        aUserInputVisible));
end;

procedure TDataConnectionCompositeParamEditor.DoAssignParams(const aValue: TDataConnectionParamList);
var
    x: integer;
begin
    fParams := aValue;
    for x := 0 to fChildEditors.Count - 1 do
        fChildEditors[x].AssignParams(aValue);
end;

{ TDataConnectionPathParamEditor }

function TDataConnectionPathParamEditor.HasEditFunction: boolean;
begin
    result := true
end;

constructor TDataConnectionParamsEditor.Create();
begin
    inherited Create('');
    AddParam(cKeyTypeName, true, false, true);
    AddParam(TDataConnectionPathParamEditor.Create(cKeyDataLocation, true, true, true));
end;

{ TBDEDataConnectionParamsEditor }

constructor TBDEDataConnectionParamsEditor.Create();
begin
    inherited Create();
    self.AddParam(cKeyUsername, false, true, true);
    self.AddParam(cKeyPassword, false, true, true);
end;

{ TMSExcelDataConnectionParamsEditor }

constructor TMSExcelDataConnectionParamsEditor.Create;
begin
    inherited Create();
    self.AddParam(cKeyHasHeader, true, true, true);
end;

{ TCSVDataConnectionParamsEditor }

constructor TCSVDataConnectionParamsEditor.Create;
begin
    inherited Create();
    self.AddParam(cKeyTableName, true, true, true);
    self.AddParam(cKeyDelimiter, true, true, true);
    self.AddParam(cKeyHasHeader, true, true, true);
end;

{ TORACLEDataConnectionParamsEditor }

constructor TORACLEDataConnectionParamsEditor.Create;
begin
    inherited Create();
    // Boolean 1.Required, 2.Allowed, 3.Visible
    self.AddParam(cKeyDataLocation, false, false, false);
    self.AddParam(cKeyUsername, true, true, true);
    self.AddParam(cKeyPassword, true, true, true);
    self.AddParam(cKeyProvider, false, true, true);
    self.AddParam(cKeyLdap, true, true, true);
    self.AddParam(cKeyIP, true, true, true);
    self.AddParam(cKeyPort, true, true, true);
    self.AddParam(cKeyDatabaseName, false, true, true);
    self.AddParam(cKeyAttributes, false, true, true);
    self.AddParam(cKeyConnect, false, true, true);
    self.AddParam(cKeyEncrypt, false, true, true);
    self.AddParam(cKeyLdapFilter, false, true, true);
    self.AddParam(cKeyScope, false, true, true);
    self.AddParam(cKeyDNnames, false, true, true);
end;


end.
