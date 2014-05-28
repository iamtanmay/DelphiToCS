{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  06.07.09 pk                                        TN4585.4   Initial revision
  13.07.09 pk  GetRecNo; GetIsInsertState            TN4585.4   New
  04.11.09 pk                               	        TN4843     Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  16.12.09 pk  TCoreClientDatabaseProvider           TN4933     New
  17.06.10 pk                                        TN5152.1   uses DataProviderFactory
  04.08.10 pk                                        TN5218     Changes for Plugin database packages
  19.10.10 pk                                        TN5305     changes needed for CoreClient/Server-
  29.06.11 wl  ExecSQL                               TN5613    result = RowsAffected
  17.08.11 wl  PrepareConnection                     TN5654   entfernt
  30.08.11 wl                                        TN5672   keine Compiler-Fehler mehr
  19.09.11 wl  CreateDatabaseProvider                TN5672   Neuer Parameter: aOwnsConnectionParams
  27.03.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit CoreClientDataProvider;


interface


uses
    GeneralTypes,
    DataConnectionParams,
    DataProvider,
    DatabaseProvider,
    DataProviderFactory,
    StreamableDatasetClasses,
    RemoteQuery,
    SQLParser,
    RemoteFunctionClasses;

type
    TCoreClientDataProvider = class(TDataProvider)
    private
        procedure ReadDataAtCursor;
        procedure WriteDataAtCursor;
    protected
        fQuery: TRemoteClientQuery;
        fFields: TDataFields;
        fActive: boolean;
        function GetActive: boolean; override;
        function GetBof: boolean; override;
        function GetEof: boolean; override;
        function GetRecordCount: Integer; override;
        function GetFieldCount: Integer; override;
        function GetFields: TDataFields; override;
        function GetRecNo: integer; override;
        function GetIsInsertState(): boolean; override;
    public
        constructor Create(const aConnectionParams: TDataConnectionParams;
            const aFunctionCaller: TRemoteFunctionCaller);
        destructor Destroy; override;

        procedure SelectAndOpen(const aSQL: string; aReadOnly: boolean); override;
        procedure SelectAndOpenBySQLRec(const aSQL: TSQLRec; const aReadOnly: boolean); override;
        function ExecSQL(const aSQL: string): integer; override;

        procedure CopyRecordsFrom(const aTableName: string; const aSourceDataProvider: TDataProvider;
            const aSourceTableName: string); override;

        procedure Append; override;
        procedure Close; override;
        procedure Delete; override;
        procedure Edit; override;
        function FieldByName(const aFieldName: string): TDataField; override;
        procedure First; override;
        procedure Insert; override;
        function IsEmpty: Boolean; override;
        procedure Last; override;
        procedure Next; override;
        procedure Post; override;
        procedure Prior; override;
        procedure MoveBy(aOffset: integer); override;
        function GetFieldNames(): TStringArray; override;
        procedure Refresh; override;
        procedure SetCachedUpdates(aCachedUpdates: boolean); override;
        procedure DatasetApplyUpdates(); override;
        property Fields: TDataFields read fFields;
    end;

    TCoreClientDatabaseProvider = class(TDatabaseProvider)
    protected
        fDatabase: TRemoteClientDatabase;
    public
        constructor Create(const aConnectionParams: TDataConnectionParams;
            const aFunctionCaller: TRemoteFunctionCaller);
        destructor Destroy; override;
        function GetAllTableNames(): TStringArray; override;
        procedure AddSystemPassword(const aPassword: string); override;
        procedure SetOnPassword(aOnPassword: TDatabasePasswordEvent); override;
        function SetAllTablesPassword(aAliasName, aPassword: string): string; override;
        procedure OpenSession(const aSessionName: string); override;
        function ReadTableDef(const aTableName: string): TStreamableTableDef; override;
        function CreateTable(const aTableDef: TStreamableTableDef): boolean; override;
        function ReadDatabaseDef(): TStreamableDatabaseDef; override;
        function CreateDatabase(const aDatabaseDef: TStreamableDatabaseDef): boolean; override;
        procedure DeleteTable(const aTableName: string); override;
        function TableExists(const aTableName: string): boolean; override;
    end;

    TCoreClientDataProviderFactory = class(TDataProviderFactory)
    private
        fFunctionCaller: TRemoteFunctionCaller;
    protected
        function GetMainDBAlias: string; override;
        function GetUserDBAlias: string; override;
        procedure SetMainDBAlias(const aValue: string); override;
        procedure SetUserDBAlias(const aValue: string); override;
        function GetSystemDataPassword(): string; override;
    public
        constructor Create(const aFunctionCaller: TRemoteFunctionCaller);
        function CreateDataProvider(const aConnectionParams: TDataConnectionParams): TDataProvider; override;
        function CreateDatabaseProvider(const aConnectionParams: TDataConnectionParams;
            aOwnsConnectionParams: boolean): TDatabaseProvider; override;
        function CreateDataConnectionParams(const aTypeName: string): TDataConnectionParams; override;
        function CreateConnectionParamsByAlias(const aAlias: string): TDataConnectionParams; override;
        function GetAllAliasNames(): TStringArray; override;
        function AliasNameExists(const aAlias: string): boolean; override;
        function GetAliasPath(const aAlias: string): string; override;
        procedure ResetPassword(); override;
        function ChangeSystemDataPassword(const aAliasNames: TArray<string>): string; override;
    end;


implementation


uses
    SysUtils,
    Variants,
    Forms,
    Streamable,
    CoreControllerClientCalls;

{ TDataProvider }

constructor TCoreClientDataProvider.Create(const aConnectionParams: TDataConnectionParams;
    const aFunctionCaller: TRemoteFunctionCaller);
begin
    inherited Create(aConnectionParams);

    fQuery := TRemoteClientQuery.Create(aConnectionParams, aFunctionCaller);
    fFields := nil;
    ASSERT(Assigned(fQuery));
end;

destructor TCoreClientDataProvider.Destroy();
begin
    try
        self.Close;
    finally
        fQuery.Free;

        inherited;
    end;
end;

procedure TCoreClientDataProvider.ReadDataAtCursor();
begin
    fQuery.ReadDataAtCursor();
end;

procedure TCoreClientDataProvider.WriteDataAtCursor();
begin
    fQuery.WriteDataAtCursor();
end;

procedure TCoreClientDataProvider.SelectAndOpen(const aSQL: string; aReadOnly: boolean);
begin
    fFields := fQuery.SelectAndOpen(aSQL, aReadOnly);
    fActive := true;
    ReadDataAtCursor();

    if Assigned(fOnAfterOpen) then
        fOnAfterOpen(self);
end;

procedure TCoreClientDataProvider.SelectAndOpenBySQLRec(const aSQL: TSQLRec; const aReadOnly: boolean);
begin
    inherited;

end;

procedure TCoreClientDataProvider.Append();
begin
    fQuery.Append();
end;

procedure TCoreClientDataProvider.Close();
begin
    fQuery.Close();
    fActive := false;
end;

procedure TCoreClientDataProvider.CopyRecordsFrom(const aTableName: string;
    const aSourceDataProvider: TDataProvider; const aSourceTableName: string);
begin
    inherited;

end;

function TCoreClientDataProvider.ExecSQL(const aSQL: string): integer;
begin
    EXIT(fQuery.ExecSQL(aSQL));
end;

function TCoreClientDataProvider.GetEof(): boolean;
begin
    result := fQuery.IsEOF;
end;

procedure TCoreClientDataProvider.Post;
begin
    WriteDataAtCursor();
    fQuery.Post();
    ReadDataAtCursor();
end;

function TCoreClientDataProvider.GetActive(): boolean;
begin
    result := fActive;
end;

function TCoreClientDataProvider.GetBof: boolean;
begin
    result := fQuery.IsBof;
end;

procedure TCoreClientDataProvider.Delete;
begin
    fQuery.Delete;
    ReadDataAtCursor();
end;

procedure TCoreClientDataProvider.Edit;
begin
    fQuery.Edit;
end;

function TCoreClientDataProvider.FieldByName(const aFieldName: string): TDataField;
begin
    result := fFields.FieldByName(aFieldName);
end;

procedure TCoreClientDataProvider.Next();
begin
    fQuery.Next;
    ReadDataAtCursor();
end;

procedure TCoreClientDataProvider.Prior();
begin
    fQuery.Prior;
    ReadDataAtCursor();
end;

procedure TCoreClientDataProvider.First;
begin
    fQuery.First;
    ReadDataAtCursor();
end;

procedure TCoreClientDataProvider.Last;
begin
    fQuery.Last;
    ReadDataAtCursor();
end;

procedure TCoreClientDataProvider.MoveBy(aOffset: integer);
begin
    fQuery.MoveBy(aOffset);
    self.ReadDataAtCursor();
end;

function TCoreClientDataProvider.GetRecNo: integer;
begin
    result := fQuery.RecNo;
end;

function TCoreClientDataProvider.GetRecordCount: Integer;
begin
    result := fQuery.RecordCount;

    // für Import-DataProvider
    if Assigned(fOnRecordCount) then
        result := fOnRecordCount(result)
end;

procedure TCoreClientDataProvider.Insert;
begin
    fQuery.Insert;
end;

function TCoreClientDataProvider.IsEmpty: Boolean;
begin
    result := fQuery.IsEmpty;
end;

procedure TCoreClientDataProvider.Refresh;
begin
    // fQuery.Refresh;
    // ReadDataAtCursor();
end;

function TCoreClientDataProvider.GetFieldCount: Integer;
begin
    result := fFields.Count;
end;

function TCoreClientDataProvider.GetFields: TDataFields;
begin
    result := fFields;
end;

function TCoreClientDataProvider.GetIsInsertState: boolean;
begin
    result := fQuery.IsInsertState;
end;

function TCoreClientDataProvider.GetFieldNames(): TStringArray;
begin
    result := fFields.GetFieldNames();
end;

procedure TCoreClientDataProvider.DatasetApplyUpdates;
begin
    fQuery.DatasetApplyUpdates();
end;

procedure TCoreClientDataProvider.SetCachedUpdates(aCachedUpdates: boolean);
begin
    fQuery.SetCachedUpdates(aCachedUpdates);
end;

{ TCoreClientDatabaseProvider }

constructor TCoreClientDatabaseProvider.Create(const aConnectionParams: TDataConnectionParams;
    const aFunctionCaller: TRemoteFunctionCaller);
begin
    inherited Create();
    fDatabase := TRemoteClientDatabase.Create(aConnectionParams, aFunctionCaller);
end;

function TCoreClientDatabaseProvider.CreateDatabase(const aDatabaseDef: TStreamableDatabaseDef): boolean;
begin
    result := false;
end;

function TCoreClientDatabaseProvider.CreateTable(const aTableDef: TStreamableTableDef): boolean;
begin
    result := false;
end;

procedure TCoreClientDatabaseProvider.DeleteTable(const aTableName: string);
begin

end;

destructor TCoreClientDatabaseProvider.Destroy();
begin
    fDatabase.Free;
    inherited;
end;

procedure TCoreClientDatabaseProvider.AddSystemPassword(const aPassword: string);
begin
    fDatabase.AddSystemPassword(aPassword);
end;

function TCoreClientDatabaseProvider.GetAllTableNames: TStringArray;
begin
    result := fDatabase.GetAllTableNames();
end;

procedure TCoreClientDatabaseProvider.OpenSession(const aSessionName: string);
begin
    fDatabase.OpenSession(aSessionName);
end;

function TCoreClientDatabaseProvider.ReadDatabaseDef: TStreamableDatabaseDef;
begin
    result := nil;
end;

function TCoreClientDatabaseProvider.ReadTableDef(const aTableName: string): TStreamableTableDef;
begin
    result := nil;
end;

function TCoreClientDatabaseProvider.SetAllTablesPassword(aAliasName, aPassword: string): string;
begin
    result := fDatabase.SetAllTablesPassword(aAliasName, aPassword);
end;

procedure TCoreClientDatabaseProvider.SetOnPassword(aOnPassword: TDatabasePasswordEvent);
begin
    // events cannot be remoted yet?
end;

function TCoreClientDatabaseProvider.TableExists(const aTableName: string): boolean;
begin
    result := false;
end;

{ TCoreClientDataProviderFactory }

function TCoreClientDataProviderFactory.ChangeSystemDataPassword(const aAliasNames: TArray<string>): string;
begin

end;

constructor TCoreClientDataProviderFactory.Create(const aFunctionCaller: TRemoteFunctionCaller);
begin
    inherited Create();
    fFunctionCaller := aFunctionCaller;
end;

function TCoreClientDataProviderFactory.CreateDatabaseProvider(const aConnectionParams: TDataConnectionParams;
    aOwnsConnectionParams: boolean): TDatabaseProvider;
begin
    result := TCoreClientDatabaseProvider.Create(aConnectionParams, fFunctionCaller);
end;

function TCoreClientDataProviderFactory.CreateDataConnectionParams(const aTypeName: string)
    : TDataConnectionParams;
begin
    result := TCoreClient.Instance.CreateDataConnectionParams(aTypeName);
end;

function TCoreClientDataProviderFactory.CreateConnectionParamsByAlias(const aAlias: string)
    : TDataConnectionParams;
begin
    result := TCoreClient.Instance.CreateConnectionParamsByAlias(aAlias);
end;

function TCoreClientDataProviderFactory.CreateDataProvider(const aConnectionParams: TDataConnectionParams)
    : TDataProvider;
begin
    result := TCoreClientDataProvider.Create(aConnectionParams, fFunctionCaller);
end;

function TCoreClientDataProviderFactory.AliasNameExists(const aAlias: string): boolean;
begin
    result := TCoreClient.Instance.AliasNameExists(aAlias);
end;

function TCoreClientDataProviderFactory.GetAliasPath(const aAlias: string): string;
begin
    result := TCoreClient.Instance.GetAliasPath(aAlias);
end;

function TCoreClientDataProviderFactory.GetAllAliasNames: TStringArray;
begin
    result := TCoreClient.Instance.GetAllAliasNames();
end;

function TCoreClientDataProviderFactory.GetMainDBAlias: string;
begin
    result := TCoreClient.Instance.GetMainDBAlias();
end;

function TCoreClientDataProviderFactory.GetSystemDataPassword: string;
begin

end;

function TCoreClientDataProviderFactory.GetUserDBAlias: string;
begin

end;

procedure TCoreClientDataProviderFactory.ResetPassword;
begin
    inherited;

end;

procedure TCoreClientDataProviderFactory.SetMainDBAlias(const aValue: string);
begin
    inherited;

end;

procedure TCoreClientDataProviderFactory.SetUserDBAlias(const aValue: string);
begin
    inherited;

end;


end.
