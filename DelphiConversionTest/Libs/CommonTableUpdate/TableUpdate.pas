{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : The main classes which manage the updates of a table
  These classes are abstrac and do NOT contain any logic about what kind of table must be updated.
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  -----------------------------------------------
  06.02.07 pk                                TN3544   Initial Revision
  22.02.07 pk  TTableUpdater.Destroy         TN3583   Bad Bug: call inherited instead of inherited create!
  01.08.07 wl                                TN3811.2 für Logging wird LogManager benutzt
  21.09.07 wl                                TN3811.4 Änderungen für CFR21DataAdapter
  24.06.08 pk                                TN4148   uses UpdateManagerCommonTypes
  11.01.11 wl                                TN5405   verbessertes Logging
  15.02.11 pk                               	TN4780   changes needed to make UpdateManager compatible with TurboDB
  11.04.11 wl                               TN5549   uses ArrayUtils
  27.02.13 wl                                TN6045   uses geändert
  17.04.13 wl                                TN6106   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit TableUpdate;


interface


uses
    Generics.Collections,
    Update,
    TableVersionInfo,
    TableUpdateItem,
    TableChangeAdaptor,
    TableStructDef,
    TableUpdateDataMap;

const
    INT_MINORREVISION_NONE = 0;

type

    TTableUpdate = class(TUpdate)
    protected
        fRevisionNumber: TTableRevisionNumber;
        fStructDef: TTableStructDef;
        fDataDef: TTableUpdateDataDef;
        fStructUpdateList: TObjectList<TTableUpdateStructItem>;
        fDataUpdateList: TObjectList<TTableUpdateDataItem>;
        fTableChangeAdaptor: TTableChangeAdaptor;
        fUpdateReason: string;
        fUpdateSuccessful: boolean;
        // function DisplayVersionError( const aVersionError : string; aJustAbortPossible : boolean ): integer;
        procedure UpdateStructure;
        procedure UpdateData();

        function GetUpdateDescription: string; override;
        function GetUpdateReason: string; override;
        procedure InitializeUpdate(); override;
        procedure PerformUpdate(); override;
        procedure FinalizeUpdate(); override;
        procedure DoVersionCheck(); override;
    public
        constructor Create(aUpdateID: TUpdateID; aStructDefClass: TTableStructDefClass;
            aMajorRevision: integer; aMinorRevision: integer = INT_MINORREVISION_NONE);
        destructor Destroy; override;
        procedure AlterStructure(aNewTableStructDefClass: TTableStructDefClass);
        procedure AddStructItem(aUpdateItem: TTableUpdateStructItem);
        function RevisionNumberAsStr(): string;
        procedure AddDataItem(aUpdateItem: TTableUpdateDataItem);
        procedure CopyMatchingFields(const aExceptions: array of string);
        procedure CopyField(const aSourceField, aDestField: string);
        procedure SetConstField(aDestField: string; const aValue: variant; aUpdateOnlyIfEmpty: boolean);
        procedure CopyFieldWithFunc(const aSourceField, aDestField: string; aFunc: TTableUpdateDataMapFunc);
        procedure CustomDataFunc(aFunc: TTableUpdateDataFunc);
        // procedure PerformUpdate( aTableChangeAdaptor : TTableChangeAdaptor );

        property RevisionNumber: TTableRevisionNumber read fRevisionNumber;
    end;


implementation


uses
    Windows,
    SysUtils,
    Dialogs,
    Controls,
    Forms,
    ArrayUtils,
    UpdateManagerCommonTypes;

{ TTableUpdate }

constructor TTableUpdate.Create(aUpdateID: TUpdateID; aStructDefClass: TTableStructDefClass;
    aMajorRevision, aMinorRevision: integer);
begin
    inherited Create(aUpdateID, '');
    fRevisionNumber := TTableChangeAdaptor.TableRevisionNumberFromInt(aMajorRevision, aMinorRevision);
    fStructUpdateList := TObjectList<TTableUpdateStructItem>.Create();
    fDataUpdateList := TObjectList<TTableUpdateDataItem>.Create();
    fDataDef := TTableUpdateDataDef.Create;
    fUpdateReason := '';
    fStructDef := aStructDefClass.Create();
    fTableChangeAdaptor := TTableChangeAdaptor.Create(TUpdatePaths.Instance.DBPath,
        TUpdatePaths.Instance.TempDBPath, GetArchivePath(), fStructDef, fRevisionNumber);
end;

destructor TTableUpdate.Destroy();
begin
    fTableChangeAdaptor.Free;
    fStructDef.Free;
    inherited;
end;

function TTableUpdate.GetUpdateDescription: string;
begin
    if (fRevisionNumber.Major = 1) and (fRevisionNumber.Minor = INT_MINORREVISION_NONE) then
        result := Format('Create Table %s', [fStructDef.Name])
    else
        result := Format('Update Table %s to Version %s',
            [fStructDef.Name, TTableRevisionNumber.VersionToStr(fRevisionNumber)]);
end;

function TTableUpdate.GetUpdateReason: string;
begin
    result := fUpdateReason;
end;

procedure TTableUpdate.AddStructItem(aUpdateItem: TTableUpdateStructItem);
begin
    fStructUpdateList.Add(aUpdateItem);
end;

procedure TTableUpdate.AlterStructure(aNewTableStructDefClass: TTableStructDefClass);
begin
    self.AddStructItem(TTableUpdateAlterStructItem.Create(aNewTableStructDefClass.Create(false)));
end;

function TTableUpdate.RevisionNumberAsStr(): string;
begin
    result := TTableRevisionNumber.VersionToStr(fRevisionNumber);
end;

procedure TTableUpdate.AddDataItem(aUpdateItem: TTableUpdateDataItem);
begin
    fDataUpdateList.Add(aUpdateItem);
end;

procedure TTableUpdate.UpdateStructure();
var
    x: integer;
    xLogText: string;
begin
    // always update the table strucutre in memory
    for x := 0 to fStructUpdateList.Count - 1 do
    begin
        (fStructUpdateList[x] as TTableUpdateItem).PrepareUpdate(fStructDef);
    end;
    xLogText := 'Updating structure ' + fStructDef.Name + ', Version ' + TTableRevisionNumber.VersionToStr
        (fStructDef.Version);
    TLogManager.Instance.Log(xLogText + ' - BEGIN', 0);
    fTableChangeAdaptor.UpdateStructure();
    TLogManager.Instance.Log(xLogText + ' - END', 0);
end;

{
  procedure TTableUpdate.PerformUpdate( aTableChangeAdaptor : TTableChangeAdaptor );
  var
  xLogStr : string;
  xTempTablePath : string;
  begin
  if aTableChangeAdaptor.CompareVersions( fVersion,  fStructDef.Version ) = rncFirstIsNewer then begin
  xLogStr := Format( 'Updating table %s to Version %s', [ fStructDef.Name, aTableChangeAdaptor.VersionToStr( fVersion ) ] );
  gLogManager.Log( xLogStr + '- BEGIN', true );


  gLogManager.Log( xLogStr + '- END', false );
  end;

  end;
}
procedure TTableUpdate.UpdateData();
var
    x: integer;
    xUpdateItem: TTableUpdateDataItem;
    xLogText: string;
begin
    // Do data updates
    for x := 0 to fDataUpdateList.Count - 1 do
    begin
        if (fDataUpdateList[x] is TTableUpdateDataItem) then
        begin
            xUpdateItem := (fDataUpdateList[x] as TTableUpdateDataItem);
            xUpdateItem.SetDataDef(fDataDef);
            xUpdateItem.PrepareUpdate(fStructDef);
        end;
    end;

    xLogText := 'Updating data ' + fStructDef.Name + ', Version ' + TTableRevisionNumber.VersionToStr
        (fStructDef.Version);

    TLogManager.Instance.Log(xLogText + ' - BEGIN', 0);
    fTableChangeAdaptor.UpdateData(fDataDef);
    TLogManager.Instance.Log(xLogText + ' - END', 0);
end;

procedure TTableUpdate.InitializeUpdate;
begin
    UpdateStructure();
end;

procedure TTableUpdate.PerformUpdate();
begin
    fUpdateSuccessful := false;
    UpdateData();
    fUpdateSuccessful := true;
end;

procedure TTableUpdate.FinalizeUpdate();
begin
    if fUpdateSuccessFul then
    begin
        TLogManager.Instance.Log('Archive existing table and copy new table - BEGIN', 0);
        fTableChangeAdaptor.CommitChanges();
        TLogManager.Instance.Log('Archive existing table and copy new table - END', 0);
    end
    else
    begin
        fTableChangeAdaptor.RollbackChanges();
    end;

end;

{
  function TTableUpdate.DisplayVersionError( const aVersionError : string; aJustAbortPossible : boolean ): integer;
  var
  xErrorInfo : TErrorInfo;
  xErrBoxInstance : TForm;
  xButtons : TErrorInfoButtons;
  begin
  if aJustAbortPossible then
  xButtons := eibAbort
  else
  xButtons := eibAbortIgnore;

  xErrorInfo := TErrorInfo.Create();
  try
  xErrorInfo.Init(  'Update Manager has detected a version issue', 'Update Manager', xButtons );
  xErrorInfo.AddText( aVersionError );
  xErrorInfo.AddText( '' ); // leave a blank line
  if not aJustAbortPossible then begin
  xErrorInfo.IgnoreHint := 'Press Ignore to continue with the update';
  xErrorInfo.AddText( 'If the software version is being updated, ' + xErrorInfo.IgnoreHint );
  end;
  xErrorInfo.AbortHint  := 'Press Abort to exit the application';
  xErrorInfo.AddText( 'Press Abort to exit the application without updating.' );

  xErrBoxInstance := TfrmErrorMessage.Create( Application, xErrorInfo );
  try
  result := xErrBoxInstance.ShowModal;
  finally
  xErrBoxInstance.Free;
  end;
  finally
  xErrorInfo.Free;
  end;
  end;
}
procedure TTableUpdate.DoVersionCheck();
var
    xCurrentRevisionNumber: TTableRevisionNumber;
begin
    xCurrentRevisionNumber := fTableChangeAdaptor.ReadTableVersion();

    fVersionCheckResult := vcrOk;

    if fTableChangeAdaptor.IsTableVersionMissing(xCurrentRevisionNumber) then
    begin
        fUpdateReason := Format('The version information file %s is missing.', [fStructDef.Name + '.VR']);
        fVersionCheckResult := vcrUpdate;
    end
    else
    begin
        case fTableChangeAdaptor.CompareVersions(xCurrentRevisionNumber, fRevisionNumber) of
            rncFirstIsOlder:
                begin
                    fUpdateReason :=
                        Format('The table %s has a version number %s which is older than the expected version number %s',
                        [fStructDef.Name, TTableRevisionNumber.VersionToStr(xCurrentRevisionNumber),
                        TTableRevisionNumber.VersionToStr(fRevisionNumber)]);
                    fVersionCheckResult := vcrUpdate;
                end;
            rncFirstIsNewer:
                begin
                    { xVersionErrorMessage := Format( 'The table %s has a version number %s ' + #13#10 + 'which is newer than the expected version number %s',
                      [ fStructDef.Name, fTableChangeAdaptor.VersionToStr( xCurrentRevisionNumber ),
                      fTableChangeAdaptor.VersionToStr( fRevisionNumber ) ] );
                      xJustAbortPossible := true;
                    }
                end;
            else
                begin
                end;
        end;
    end;


    // if ( DisplayVersionError( xVersionErrorMessage, xJustAbortPossible ) = mrAbort ) then
    // result := vcrError;

end;

procedure TTableUpdate.CopyMatchingFields(const aExceptions: array of string);
begin
    self.AddDataItem(TTableUpdateExecuteDataMapItem.Create(TTableUpdateCopyMatchingDataMap.Create
        (aExceptions)));
end;

procedure TTableUpdate.CopyField(const aSourceField, aDestField: string);
begin
    self.AddDataItem(TTableUpdateExecuteDataMapItem.Create(TTableUpdateCopyFieldDataMap.Create(aSourceField,
        aDestField)));
end;

procedure TTableUpdate.SetConstField(aDestField: string; const aValue: variant; aUpdateOnlyIfEmpty: boolean);
begin
    self.AddDataItem(TTableUpdateExecuteDataMapItem.Create(TTableUpdateConstFieldDataMap.Create(aValue,
        aDestField, aUpdateOnlyIfEmpty)));
end;

procedure TTableUpdate.CopyFieldWithFunc(const aSourceField, aDestField: string;
    aFunc: TTableUpdateDataMapFunc);
begin
    self.AddDataItem(TTableUpdateExecuteDataMapItem.Create(TTableUpdateCopyFieldWithFuncDataMap.Create
        (TArrayUtils.CopyArray<string>([aSourceField]), aDestField, aFunc)));
end;
{
  procedure TTableUpdate.UpdateVersion( aVersionUpdate : TTableUpdate );
  begin
  try
  aVersionUpdate.SetStructDef( fStructDef );
  aVersionUpdate.PerformUpdate( fTableChangeAdaptor );
  except
  on E : Exception do begin
  raise Exception.CreateFmt( 'Error Updating table [%s] to version [%s] -> %s', [ fStructDef.Name, aVersionUpdate.VersionAsStr(), E.Message ] );
  end;
  end;
  end;
}

procedure TTableUpdate.CustomDataFunc(aFunc: TTableUpdateDataFunc);
begin
    AddDataItem(TTableUpdateDataFuncItem.Create(aFunc));
end;


end.
