{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  class.method/member                    track-no improvement/change
  -------- --  -------------------------------------  -------- -------------------------------------
  10.07.08 wl  TUpdatePaths.Create                    TN4157   uses SysUtils.IncludeTrailingPathDelimiter
  15.02.11 pk                              	     	  TN4780   changes needed to make UpdateManager compatible with TurboDB
  27.02.13 wl                                         TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit Update;


interface


uses
    Generics.Collections,
    UpdateStatusDataAdaptor;

type
    TVersionCheckResult = (vcrNone, vcrUpdate, vcrError, vcrOK);

    TUpdateID = integer;

    TUpdatePaths = class
    protected
        fDataPath, fDBPath, fTempDBPath, fUpdatesPath: string;
        constructor Create(const aDataPath, aDBPath, aTempDBPath, aUpdatesPath: string);
    public
        class procedure CreateInstance(const aDataPath, aDBPath, aTempDBPath, aUpdatesPath: string);
        class function Instance(): TUpdatePaths;
        property DataPath: string read fDataPath;
        property DBPath: string read fDBPath;
        property TempDBPath: string read fTempDBPath;
        property UpdatesPath: string read fUpdatesPath;
    end;

    TUpdate = class
    protected
        fUpdateID: TUpdateID;
        fUpdateDescription: string;
        fUpdateStatusDA: TUpdateStatusDataAdaptor;
        fVersionCheckResult: TVersionCheckResult;
        function GetUpdateDescription(): string; virtual;
        function GetUpdateReason(): string; virtual;
        procedure InitializeUpdate(); virtual; abstract;
        procedure FinalizeUpdate(); virtual; abstract;
        procedure PerformUpdate(); virtual; abstract;
        procedure WriteUpdateStatus(); virtual;
        function GetIsUpdateNeeded: boolean;
        procedure DoVersionCheck(); virtual;
        procedure WriteUpdateStatusIfMissing;
    public
        constructor Create(aUpdateID: TUpdateID; const aUpdateDescription: string);
        destructor Destroy(); override;
        procedure Update(); virtual;
        procedure VersionCheck();
        function GetArchivePath: string;

        property UpdateID: TUpdateID read fUpdateID;
        property UpdateDescription: string read GetUpdateDescription;
        property UpdateReason: string read GetUpdateReason;
        property IsUpdateNeeded: boolean read GetIsUpdateNeeded;
    end;

    TMultiUpdate = class(TUpdate)
    private
        fUpdates: TObjectList<TUpdate>;
    protected
        procedure InitializeUpdate(); override;
        procedure FinalizeUpdate(); override;
        procedure PerformUpdate(); override;
    public
        constructor Create(aUpdateID: TUpdateID);
    end;


implementation


uses
    SysUtils;

var
    uUpdatePaths: TUpdatePaths = nil;

    { TUpdate }

constructor TUpdate.Create(aUpdateID: TUpdateID; const aUpdateDescription: string);
begin
    inherited Create();
    fUpdateID := aUpdateID;
    fUpdateDescription := aUpdateDescription;
    fUpdateStatusDA := TUpdateStatusDataAdaptor.Create(TUpdatePaths.Instance.DBPath);
    fVersionCheckResult := vcrNone;
end;

destructor TUpdate.Destroy;
begin
    fUpdateStatusDA.Free;
    inherited;
end;

function TUpdate.GetIsUpdateNeeded: boolean;
begin
    result := fVersionCheckResult = vcrUpdate;
end;

function TUpdate.GetUpdateDescription: string;
begin
    result := fUpdateDescription;
end;

function TUpdate.GetUpdateReason: string;
begin
    result := '';
end;

function TUpdate.GetArchivePath: string;
begin
    result := Format('%s%.9d', [TUpdatePaths.Instance.UpdatesPath, fUpdateID]);
end;

procedure TUpdate.Update();
begin
    VersionCheck();
    if self.IsUpdateNeeded then
    begin
        InitializeUpdate();
        try
            PerformUpdate();
        finally
            FinalizeUpdate();
        end;
        WriteUpdateStatus();
        fVersionCheckResult := vcrOK;
    end
    else
    begin
        WriteUpdateStatusIfMissing();
    end;
end;

procedure TUpdate.DoVersionCheck;
begin
    if fUpdateStatusDA.ReadStatus(fUpdateID) = ustUpdated then
        fVersionCheckResult := vcrOK
    else
        fVersionCheckResult := vcrUpdate;
end;

procedure TUpdate.VersionCheck();
begin
    if fVersionCheckResult <> vcrNone then
        EXIT;
    DoVersionCheck();

    // if an update is not needed write the status into the updatestatus table if it is missing
    // if not IsUpdateNeeded then
    // WriteUpdateStatusIfMissing();
end;

procedure TUpdate.WriteUpdateStatusIfMissing();
begin
    WriteUpdateStatus();
end;

procedure TUpdate.WriteUpdateStatus();
begin
    fUpdateStatusDA.WriteStatus(fUpdateID, GetUpdateDescription(), ustUpdated);
end;

{ TMultiUpdate }

constructor TMultiUpdate.Create(aUpdateID: TUpdateID);
begin
    inherited Create(aUpdateID, '');
    fUpdates := TObjectList<TUpdate>.Create();
end;

procedure TMultiUpdate.FinalizeUpdate;
var
    x: integer;
begin
    for x := 0 to fUpdates.Count - 1 do
        fUpdates[x].FinalizeUpdate();
end;

procedure TMultiUpdate.InitializeUpdate;
var
    x: integer;
begin
    for x := 0 to fUpdates.Count - 1 do
        fUpdates[x].InitializeUpdate();
end;

procedure TMultiUpdate.PerformUpdate;
var
    x: integer;
begin
    for x := 0 to fUpdates.Count - 1 do
        fUpdates[x].PerformUpdate();
end;

{ TUpdatePaths }

constructor TUpdatePaths.Create(const aDataPath, aDBPath, aTempDBPath, aUpdatesPath: string);
begin
    inherited Create();

    fDataPath := SysUtils.IncludeTrailingPathDelimiter(aDataPath);
    fDBPath := SysUtils.IncludeTrailingPathDelimiter(aDBPath);
    fTempDBPath := SysUtils.IncludeTrailingPathDelimiter(aTempDBPath);
    fUpdatesPath := SysUtils.IncludeTrailingPathDelimiter(aUpdatesPath);
end;

class procedure TUpdatePaths.CreateInstance(const aDataPath, aDBPath, aTempDBPath, aUpdatesPath: string);
begin
    uUpdatePaths := TUpdatePaths.Create(aDataPath, aDBPath, aTempDBPath, aUpdatesPath);
end;

class function TUpdatePaths.Instance: TUpdatePaths;
begin
    result := uUpdatePaths;
end;


end.
