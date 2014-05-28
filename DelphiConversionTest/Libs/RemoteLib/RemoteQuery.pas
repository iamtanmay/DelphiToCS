{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  06.07.09 pk                                        TN4585.4   Initial revision
  13.07.09 pk                                        TN4585.4   Various changes
  08.09.09 pk  DoHandleCall                          TN4585.4   for SelectAndOpen Parameter 0 was used incorrectly as ReadOnly param
  16.12.09 pk  TRemoteClient/ServerDatabase          TN4933     New
  04.02.10 pk                                        TN4972     Changes for Restart
  17.06.10 pk                                        TN5152.1   uses DataProviderFactory
  04.08.10 pk                                        TN5218     Changes for Plugin database packages
  19.10.10 pk                                        TN5305     changes needed for CoreClient/Server
  29.06.11 wl  ExecSQL                               TN5613    result = RowsAffected
  19.09.11 wl  CreateDatabaseProvider                TN5672   Neuer Parameter: aOwnsConnectionParams
  ----------------------------------------------------------------------------------------------------------------------- }

unit RemoteQuery;


interface


uses
    DataProvider,
    DatabaseProvider,
    DataConnectionPArams,
    GeneralTypes,
    RemoteFunctionClasses,
    StreamableDatasetClasses,
    Streamable;

type
    TCustomRemoteQuery = TObject;

    TRemoteClientQueryMode = (cqmClient, cqmServer);

    TRemoteClientQuery = class(TCustomRemoteQuery)
    private
        fRemoteObjectID: integer;
        fRecords: TStreamableRecordList;
        fCurrentRecord: TStreamableRecord;
        fFunctionCaller: TRemoteFunctionCaller;
        fQueryMode: TRemoteClientQueryMode;
        fClientSideCursorPos: integer;
        fFields: TStreamableFieldList;
        procedure MoveCursorTo(const aCursorPos: integer);
        function ReadFields: TStreamableFieldList;
        procedure ReadData();
        function GetRecNo(): integer;
        function GetIsInsertState(): boolean;
    public
        constructor Create(const aConnectionParams: TDataConnectionParams;
            const aFunctionCaller: TRemoteFunctionCaller);
        destructor Destroy(); override;
        procedure ReadDataAtCursor;
        procedure WriteDataAtCursor();

        function SelectAndOpen(const aSQL: string; const aReadOnly: boolean): TStreamableFieldList;
        function ExecSQL(const aSQL: string): integer;
        procedure DatasetApplyUpdates;
        procedure SetCachedUpdates(aCachedUpdates: boolean);

        procedure Append;
        procedure Close;
        procedure Delete;
        procedure Edit;
        procedure First;
        procedure Insert;
        function IsEmpty: Boolean;
        procedure Last;
        procedure Next;
        procedure Post;
        procedure Prior;
        procedure MoveBy(aOffset: integer);
        function IsEof(): boolean;
        function IsBof(): boolean;
        function RecordCount(): integer;
        property RecNo: integer read GetRecNo;
        property IsInsertState: boolean read GetIsInsertState;
    end;

    TRemoteServerObject = class
    protected
        fFunctionReceiver: TRemoteFunctionReceiver;
        procedure DoHandleCall(); virtual;
    public
        constructor Create(const aFunctionReceiver: TRemoteFunctionReceiver);
        destructor Destroy(); override;
        procedure HandleCall();
    end;

    TRemoteServerQuery = class(TRemoteServerObject)
    private
        fDataProvider: TDataProvider;
        fRecord: TStreamableRecord;
        fRecords: TStreamableRecordList;
        function ReadFields(): TStreamableFieldList;
        procedure ReadDataAtCursor(const aRecord: TStreamableRecord);
        procedure WriteDataAtCursor(const aDataList: TStreamableFieldDataList);
        procedure SelectAndOpen(const aSQL: string; const aReadOnly: boolean);
        function ExecSQL(const aSQL: string): integer;
        procedure Append;
        procedure Close;
        procedure Delete;
        procedure Edit;
        procedure First;
        procedure Insert;
        function IsEmpty: Boolean;
        procedure Last;
        procedure Next;
        procedure Post;
        procedure Prior;
        procedure MoveBy(aOffset: integer);
        function IsEof(): boolean;
        function IsBof(): boolean;
        function RecordCount(): integer;
        function GetRecNo(): integer;
        function GetIsInsertState(): boolean;
        procedure ReadData(const aRecords: TStreamableRecordList);
        procedure DoHandleReadData;
        procedure DoHandleReadDataAtCursor;
        procedure DatasetApplyUpdates;
        procedure SetCachedUpdates(aCachedUpdates: boolean);
    protected
        procedure DoHandleCall(); override;
    public
        constructor Create(const aFunctionReceiver: TRemoteFunctionReceiver);
    end;

    TRemoteClientDatabase = class
    private
        fRemoteObjectID: integer;
        fFunctionCaller: TRemoteFunctionCaller;
        fList: TStreamableStringList;
    public
        constructor Create(const aConnectionParams: TDataConnectionParams;
            const aFunctionCaller: TRemoteFunctionCaller);
        destructor Destroy(); override;
        function GetAllTableNames(): TStringArray;
        function GetAllAliasNames(): TStringArray;
        function AliasNameExists(const aAliasName: string): boolean;
        function GetAliasPath(const aAliasName: string): string;
        procedure CreateNewAlias(const aAliasName, aPath: string);
        procedure AddSystemPassword(const aPassword: string);
        procedure SetOnPassword(aOnPassword: TDatabasePasswordEvent);
        function SetAllTablesPassword(aAliasName, aPassword: string): string;
        procedure OpenSession(const aSessionName: string);
    end;

    TRemoteServerDatabase = class(TRemoteServerObject)
    private
        fDatabaseProvider: TDatabaseProvider;
        fList: TStreamableStringList;
    protected
        procedure DoHandleCall(); override;
    public
        constructor Create(const aFunctionReceiver: TRemoteFunctionReceiver);
        destructor Destroy(); override;
    end;


implementation


uses
    SysUtils,
    UtilLib,
    DataProviderFactory,
    RemoteMessageConstants,
    TypeMapTranslator;

const

    cCursorPosBof = -1;

    cRemoteQueryCallIDReadFields = 'ReadFields';
    cRemoteQueryCallIDReadData = 'ReadData';
    cRemoteQueryCallIDReadDataAtCursor = 'ReadDataAtCursor';
    cRemoteQueryCallIDWriteDataAtCursor = 'WriteDataAtCursor';
    cRemoteQueryCallIDSelectAndOpen = 'SelectAndOpen';
    cRemoteQueryCallIDExecSQL = 'ExecSQL';
    cRemoteQueryCallIDAppend = 'Append';
    cRemoteQueryCallIDClose = 'Close';
    cRemoteQueryCallIDDelete = 'Delete';
    cRemoteQueryCallIDEdit = 'Edit';
    cRemoteQueryCallIDFirst = 'First';
    cRemoteQueryCallIDInsert = 'Insert';
    cRemoteQueryCallIDIsEmpty = 'IsEmpty';
    cRemoteQueryCallIDLast = 'Last';
    cRemoteQueryCallIDNext = 'Next';
    cRemoteQueryCallIDPost = 'Post';
    cRemoteQueryCallIDPrior = 'Prior';
    cRemoteQueryCallIDMoveBy = 'MoveBy';
    cRemoteQueryCallIDIsEof = 'IsEof';
    cRemoteQueryCallIDIsBof = 'IsBof';
    cRemoteQueryCallIDRecordCount = 'RecordCount';
    cRemoteQueryCallIDGetRecNo = 'RecNo';
    cRemoteQueryCallIDIsInsertState = 'IsInsertState';
    cRemoteQueryCallIDSetCachedUpdates = 'SetCachedUpdates';
    cRemoteQueryCallIDApplyUpdates = 'ApplyUpdates';

    cRemoteDatabaseCallIDGetAllTableNames = 'GetAllTableNames';
    cRemoteDatabaseCallIDGetAllAliasNames = 'GetAllAliasNames';
    cRemoteDatabaseCallIDOpenSession = 'OpenSession';
    cRemoteDatabaseCallIDAliasNameExists = 'AliasNameExists';
    cRemoteDatabaseCallIDGetAliasPath = 'GetAliasPath';

    { TRemoteClientQuery }

constructor TRemoteClientQuery.Create(const aConnectionParams: TDataConnectionParams;
    const aFunctionCaller: TRemoteFunctionCaller);
begin
    inherited Create();
    fFunctionCaller := aFunctionCaller;
    fRecords := nil;

    fRemoteObjectID := fFunctionCaller.CallIntFunc(cCoreCallIDCreateObject,
        [TRemoteServerQuery.ClassName, aConnectionParams]);
    fQueryMode := cqmClient;
end;

destructor TRemoteClientQuery.Destroy;
begin
    fFunctionCaller.CallProc(cCoreCallIDDestroyObject, [fRemoteObjectID]);
    fFunctionCaller := nil;
    inherited;
end;

function TRemoteClientQuery.ExecSQL(const aSQL: string): integer;
begin
    EXIT(fFunctionCaller.CallIntFunc(fRemoteObjectID, cRemoteQueryCallIDExecSQL, [aSQL]));
end;

procedure TRemoteClientQuery.ReadDataAtCursor();
begin
    if self.IsBof or self.IsEof then
        EXIT;

    if fQueryMode = cqmClient then
    begin
        fCurrentRecord := fRecords[fClientSideCursorPos]
    end
    else
    begin
        FreeAndNil(fCurrentRecord);
        fFunctionCaller.Call(fRemoteObjectID, cRemoteQueryCallIDReadDataAtCursor, [], [@fCurrentRecord],
            [fCurrentRecord]);
    end;

    fFields.SetData(fCurrentRecord);
    fFields.ClearChanges();
end;

procedure TRemoteClientQuery.ReadData();
begin
    if fQueryMode <> cqmClient then
        EXIT;

    fFunctionCaller.Call(fRemoteObjectID, cRemoteQueryCallIDReadData, [], [@fRecords], [fRecords]);
end;

function TRemoteClientQuery.ReadFields: TStreamableFieldList;
begin
    FreeAndNil(fFields);
    fFunctionCaller.Call(fRemoteObjectID, cRemoteQueryCallIDReadFields, [], [@fFields], [fFields]);
    result := fFields;
end;

procedure TRemoteClientQuery.WriteDataAtCursor();
var
    xRecord: TStreamableRecord;
begin
    xRecord := TStreamableRecord.Create();
    try
        fFields.GetData(xRecord);
        fFunctionCaller.CallProc(fRemoteObjectID, cRemoteQueryCallIDWriteDataAtCursor, [xRecord]);
    finally
        xRecord.Free;
    end;
end;

function TRemoteClientQuery.SelectAndOpen(const aSQL: string; const aReadOnly: boolean): TStreamableFieldList;
begin
    if aReadOnly then
        fQueryMode := cqmClient
    else
        fQueryMode := cqmServer;

    fFunctionCaller.CallProc(fRemoteObjectID, cRemoteQueryCallIDSelectAndOpen, [aSQL, aReadOnly]);

    result := self.ReadFields();
    self.ReadData();
    self.First();
end;

procedure TRemoteClientQuery.SetCachedUpdates(aCachedUpdates: boolean);
begin
    fFunctionCaller.CallProc(fRemoteObjectID, cRemoteQueryCallIDSetCachedUpdates, [aCachedUpdates]);
end;

procedure TRemoteClientQuery.DatasetApplyUpdates;
begin
    fFunctionCaller.CallProc(fRemoteObjectID, cRemoteQueryCallIDApplyUpdates);
end;

procedure TRemoteClientQuery.Close;
begin
    fFunctionCaller.CallProc(fRemoteObjectID, cRemoteQueryCallIDClose);
end;

procedure TRemoteClientQuery.Delete;
begin
    fFunctionCaller.CallProc(fRemoteObjectID, cRemoteQueryCallIDDelete);
end;

procedure TRemoteClientQuery.Append;
begin
    fFunctionCaller.CallProc(fRemoteObjectID, cRemoteQueryCallIDAppend);
end;

procedure TRemoteClientQuery.Edit;
begin
    fFunctionCaller.CallProc(fRemoteObjectID, cRemoteQueryCallIDEdit);
end;

procedure TRemoteClientQuery.Insert;
begin
    fFunctionCaller.CallProc(fRemoteObjectID, cRemoteQueryCallIDInsert);
end;

procedure TRemoteClientQuery.Post;
begin
    fFunctionCaller.CallProc(fRemoteObjectID, cRemoteQueryCallIDPost);
end;

function TRemoteClientQuery.RecordCount: integer;
begin
    if fQueryMode = cqmClient then
    begin
        result := fRecords.Count;
    end
    else
    begin
        result := fFunctionCaller.CallIntFunc(fRemoteObjectID, cRemoteQueryCallIDRecordCount);
    end;
end;

procedure TRemoteClientQuery.MoveCursorTo(const aCursorPos: integer);
begin
    if self.IsEmpty then
    begin
        fClientSideCursorPos := cCursorPosBof;
        EXIT;
    end;

    fClientSideCursorPos := aCursorPos;

    if fClientSideCursorPos < cCursorPosBof then
        fClientSideCursorPos := cCursorPosBof;

    if fClientSideCursorPos > self.RecordCount then
        fClientSideCursorPos := self.RecordCount;

end;

procedure TRemoteClientQuery.First;
begin
    if fQueryMode = cqmClient then
    begin
        MoveCursorTo(0);
    end
    else
        fFunctionCaller.CallProc(fRemoteObjectID, cRemoteQueryCallIDFirst);
end;

function TRemoteClientQuery.GetIsInsertState: boolean;
begin
    result := fFunctionCaller.CallBoolFunc(fRemoteObjectID, cRemoteQueryCallIDIsInsertState, []);
end;

function TRemoteClientQuery.GetRecNo: integer;
begin
    if fQueryMode = cqmClient then
    begin
        result := fClientSideCursorPos + 1;
    end
    else
    begin
        result := fFunctionCaller.CallIntFunc(fRemoteObjectID, cRemoteQueryCallIDGetRecNo, []);
    end;
end;

function TRemoteClientQuery.IsBof: boolean;
begin
    if fQueryMode = cqmClient then
        result := fClientSideCursorPos = cCursorPosBof
    else
        result := fFunctionCaller.CallBoolFunc(fRemoteObjectID, cRemoteQueryCallIDIsBof);
end;

function TRemoteClientQuery.IsEmpty: Boolean;
begin
    if fQueryMode = cqmClient then
        result := self.RecordCount = 0
    else
        result := fFunctionCaller.CallBoolFunc(fRemoteObjectID, cRemoteQueryCallIDIsEmpty);
end;

function TRemoteClientQuery.IsEof: boolean;
begin
    if fQueryMode = cqmClient then
        result := self.IsEmpty or (fClientSideCursorPos = self.RecordCount)
    else
        result := fFunctionCaller.CallBoolFunc(fRemoteObjectID, cRemoteQueryCallIDIsEof);
end;

procedure TRemoteClientQuery.Last;
begin
    if fQueryMode = cqmClient then
        MoveCursorTo(self.RecordCount - 1)
    else
        fFunctionCaller.CallProc(fRemoteObjectID, cRemoteQueryCallIDIsEmpty);
end;

procedure TRemoteClientQuery.MoveBy(aOffset: integer);
begin
    if fQueryMode = cqmClient then
        MoveCursorTo(fClientSideCursorPos + aOffset)
    else
        fFunctionCaller.CallProc(fRemoteObjectID, cRemoteQueryCallIDMoveBy, [aOffset]);
end;

procedure TRemoteClientQuery.Next;
begin
    if fQueryMode = cqmClient then
        MoveBy(1)
    else
        fFunctionCaller.CallProc(fRemoteObjectID, cRemoteQueryCallIDNext);
end;

procedure TRemoteClientQuery.Prior;
begin
    if fQueryMode = cqmClient then
        MoveBy(-1)
    else
        fFunctionCaller.CallProc(fRemoteObjectID, cRemoteQueryCallIDPrior);
end;

{ TRemoteServerObject }

constructor TRemoteServerObject.Create(const aFunctionReceiver: TRemoteFunctionReceiver);
begin
    inherited Create();
    fFunctionReceiver := aFunctionReceiver;
end;

destructor TRemoteServerObject.Destroy();
begin
    fFunctionReceiver := nil;
    inherited;
end;

procedure TRemoteServerObject.DoHandleCall;
begin
end;

procedure TRemoteServerObject.HandleCall;
begin
    DoHandleCall();
end;

{ TRemoteServerQuery }
constructor TRemoteServerQuery.Create(const aFunctionReceiver: TRemoteFunctionReceiver);
var
    xConnectionParams: TDataConnectionParams;
begin
    inherited Create(aFunctionReceiver);
    xConnectionParams := fFunctionReceiver.CallParams[1].AsObj as TDataConnectionParams;

    fDataProvider := TDataProviderFactory.Instance.CreateDataProvider(xConnectionParams);
    fRecord := TStreamableRecord.Create();
    fRecords := TStreamableRecordList.Create();
end;

procedure TRemoteServerQuery.SelectAndOpen(const aSQL: string; const aReadOnly: boolean);
begin
    fDataProvider.SelectAndOpen(aSQL, aReadOnly);
end;

procedure TRemoteServerQuery.WriteDataAtCursor(const aDataList: TStreamableFieldDataList);
begin
    fDataProvider.Fields.SetData(aDataList);
end;

procedure TRemoteServerQuery.ReadDataAtCursor(const aRecord: TStreamableRecord);
begin
    fDataProvider.Fields.GetData(aRecord);
end;

procedure TRemoteServerQuery.ReadData(const aRecords: TStreamableRecordList);
var
    xRecord: TStreamableRecord;
begin
    while not self.IsEof do
    begin
        xRecord := TStreamableRecord.Create();
        aRecords.Add(xRecord);
        fDataProvider.Fields.GetData(xRecord);
        self.Next;
    end;
end;

function TRemoteServerQuery.ReadFields: TStreamableFieldList;
begin
    result := fDataProvider.Fields;
end;

function TRemoteServerQuery.RecordCount: integer;
begin
    result := fDataProvider.RecordCount;
end;

function TRemoteServerQuery.GetIsInsertState: boolean;
begin
    result := fDataProvider.IsInsertState;
end;

function TRemoteServerQuery.GetRecNo(): integer;
begin
    result := fDataProvider.RecNo;
end;

procedure TRemoteServerQuery.Delete;
begin
    fDataProvider.Delete;
end;

procedure TRemoteServerQuery.Append;
begin
    fDataProvider.Append;
end;

procedure TRemoteServerQuery.Close;
begin
    fDataProvider.Close;
end;

procedure TRemoteServerQuery.Edit;
begin
    fDataProvider.Edit;
end;

function TRemoteServerQuery.ExecSQL(const aSQL: string): integer;
begin
    EXIT(fDataProvider.ExecSQL(aSQL));
end;

procedure TRemoteServerQuery.First;
begin
    fDataProvider.First;
end;

procedure TRemoteServerQuery.Insert;
begin
    fDataProvider.Insert;
end;

function TRemoteServerQuery.IsBof: boolean;
begin
    result := fDataProvider.Bof;
end;

function TRemoteServerQuery.IsEmpty: Boolean;
begin
    result := fDataProvider.IsEmpty;
end;

function TRemoteServerQuery.IsEof: boolean;
begin
    result := fDataProvider.Eof;
end;

procedure TRemoteServerQuery.Last;
begin
    fDataProvider.Last;
end;

procedure TRemoteServerQuery.MoveBy(aOffset: integer);
begin
    fDataProvider.MoveBy(aOffset);
end;

procedure TRemoteServerQuery.Next;
begin
    fDataProvider.Next;
end;

procedure TRemoteServerQuery.Post;
begin
    fDataProvider.Post;
end;

procedure TRemoteServerQuery.Prior;
begin
    fDataProvider.Prior;
end;

procedure TRemoteServerQuery.SetCachedUpdates(aCachedUpdates: boolean);
begin
    fDataProvider.SetCachedUpdates(aCachedUpdates);
end;

procedure TRemoteServerQuery.DatasetApplyUpdates;
begin
    fDataProvider.DatasetApplyUpdates();
end;

procedure TRemoteServerQuery.DoHandleReadData();
begin
    fRecords.Clear;
    ReadData(fRecords);
    fFunctionReceiver.ResultParams.Add(TObjectCopy<TStreamableRecordList>.Copy(fRecords));
end;

procedure TRemoteServerQuery.DoHandleReadDataAtCursor();
begin
    fRecord.Clear;
    ReadDataAtCursor(fRecord);
    fFunctionReceiver.ResultParams.Add(TObjectCopy<TStreamableRecord>.Copy(fRecord));
end;

procedure TRemoteServerQuery.DoHandleCall;

begin
    if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDReadData) then
    begin
        DoHandleReadData();
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDReadFields) then
    begin
        fFunctionReceiver.ResultParams.Add(TObjectCopy<TStreamableFieldList>.Copy(ReadFields()));
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDReadDataAtCursor) then
    begin
        DoHandleReadDataAtCursor();
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDWriteDataAtCursor) then
    begin
        WriteDataAtCursor(fFunctionReceiver.CallParams[0].AsObj as TStreamableFieldDataList);
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDSelectAndOpen) then
    begin
        SelectAndOpen(fFunctionReceiver.CallParams[0].AsStr, fFunctionReceiver.CallParams[1].AsBool);
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDExecSQL) then
    begin
        fFunctionReceiver.ResultParams.Add(ExecSQL(fFunctionReceiver.CallParams[0].AsStr));
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDAppend) then
    begin
        Append();
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDClose) then
    begin
        Close();
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDDelete) then
    begin
        Delete();
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDEdit) then
    begin
        Edit();
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDFirst) then
    begin
        First();
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDInsert) then
    begin
        Insert();
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDIsEmpty) then
    begin
        fFunctionReceiver.ResultParams.Add(IsEmpty());
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDLast) then
    begin
        Last();
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDNext) then
    begin
        Next();
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDPost) then
    begin
        Post();
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDPrior) then
    begin
        Prior();
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDMoveBy) then
    begin
        MoveBy(fFunctionReceiver.CallParams[0].AsInt);
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDIsBof) then
    begin
        fFunctionReceiver.ResultParams.Add(IsBof);
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDIsEof) then
    begin
        fFunctionReceiver.ResultParams.Add(IsEof);
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDRecordCount) then
    begin
        fFunctionReceiver.ResultParams.Add(RecordCount);
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDGetRecNo) then
    begin
        fFunctionReceiver.ResultParams.Add(GetRecNo());
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDIsInsertState) then
    begin
        fFunctionReceiver.ResultParams.Add(GetIsInsertState());
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDSetCachedUpdates) then
    begin
        SetCachedUpdates(fFunctionReceiver.CallParams[0].AsBool);
    end
    else if fFunctionReceiver.CallNameMatches(cRemoteQueryCallIDApplyUpdates) then
    begin
        DatasetApplyUpdates();
    end;

end;

{ TRemoteClientDatabase }
constructor TRemoteClientDatabase.Create(const aConnectionParams: TDataConnectionParams;
    const aFunctionCaller: TRemoteFunctionCaller);
begin
    inherited Create();
    fFunctionCaller := aFunctionCaller;

    fRemoteObjectID := fFunctionCaller.CallIntFunc(cCoreCallIDCreateObject,
        [TRemoteServerDatabase.ClassName, aConnectionParams]);
    fList := TStreamableStringList.Create();

end;

destructor TRemoteClientDatabase.Destroy;
begin
    FreeAndNil(fList);
    fFunctionCaller.CallProc(cCoreCallIDDestroyObject, [fRemoteObjectID]);
    fFunctionCaller := nil;
    inherited;
end;

procedure TRemoteClientDatabase.AddSystemPassword(const aPassword: string);
begin

end;

function TRemoteClientDatabase.AliasNameExists(const aAliasName: string): boolean;
begin
    result := fFunctionCaller.CallBoolFunc(fRemoteObjectID, cRemoteDatabaseCallIDAliasNameExists,
        [aAliasName]);
end;

procedure TRemoteClientDatabase.CreateNewAlias(const aAliasName, aPath: string);
begin

end;

function TRemoteClientDatabase.GetAliasPath(const aAliasName: string): string;
var
    xResult: string;
begin
    fFunctionCaller.Call(fRemoteObjectID, cRemoteDatabaseCallIDGetAliasPath, [aAliasName], [@xResult],
        [xResult]);
    result := xResult;
end;

function TRemoteClientDatabase.GetAllAliasNames: TStringArray;
begin
    fFunctionCaller.Call(fRemoteObjectID, cRemoteDatabaseCallIDGetAllAliasNames, [], [@fList], [fList]);
    result := fList.ToStringArray;
end;

function TRemoteClientDatabase.GetAllTableNames: TStringArray;
begin
    fFunctionCaller.Call(fRemoteObjectID, cRemoteDatabaseCallIDGetAllTableNames, [], [@fList], [fList]);
    result := fList.ToStringArray;
end;

procedure TRemoteClientDatabase.OpenSession(const aSessionName: string);
begin
    fFunctionCaller.CallProc(fRemoteObjectID, cRemoteDatabaseCallIDOpenSession, [aSessionName]);
end;

function TRemoteClientDatabase.SetAllTablesPassword(aAliasName, aPassword: string): string;
begin

end;

procedure TRemoteClientDatabase.SetOnPassword(aOnPassword: TDatabasePasswordEvent);
begin

end;

{ TRemoteServerDatabase }

constructor TRemoteServerDatabase.Create(const aFunctionReceiver: TRemoteFunctionReceiver);
var
    xConnectionParams: TDataConnectionParams;
begin
    inherited Create(aFunctionReceiver);
    xConnectionParams := fFunctionReceiver.CallParams[1].AsObj as TDataConnectionParams;

    fDatabaseProvider := TDataProviderFactory.Instance.CreateDatabaseProvider(xConnectionParams, false);
    fList := TStreamableStringList.Create();
end;

destructor TRemoteServerDatabase.Destroy;
begin
    fList.Free;
    inherited;
end;

procedure TRemoteServerDatabase.DoHandleCall;
begin
    if fFunctionReceiver.CallNameMatches(cRemoteDatabaseCallIDGetAllTableNames) then
    begin
        fList.Clear();
        fList.FromStringArray(fDatabaseProvider.GetAllTableNames());
        fFunctionReceiver.ResultParams.Add(fList);
    end
    // else if fFunctionReceiver.CallNameMatches( cRemoteDatabaseCallIDGetAllAliasNames ) then begin
    // fList.Clear();
    // fList.FromStringArray( fDatabaseProvider.GetAllAliasNames() );
    // fFunctionReceiver.ResultParams.Add( fList );
    // end
    else if fFunctionReceiver.CallNameMatches(cRemoteDatabaseCallIDOpenSession) then
    begin
        fDatabaseProvider.OpenSession(fFunctionReceiver.CallParams[0].AsStr);
    end
    // else if fFunctionReceiver.CallNameMatches( cRemoteDatabaseCallIDAliasNameExists ) then begin
    // fFunctionReceiver.ResultParams.Add( fDatabaseProvider.AliasNameExists( fFunctionReceiver.CallParams[0].AsStr ) );
    // end
    // else if fFunctionReceiver.CallNameMatches( cRemoteDatabaseCallIDGetAliasPath ) then begin
    // fFunctionReceiver.ResultParams.Add( fDatabaseProvider.GetAliasPath( fFunctionReceiver.CallParams[0].AsStr ) );
    // end
        ;
end;


end.
