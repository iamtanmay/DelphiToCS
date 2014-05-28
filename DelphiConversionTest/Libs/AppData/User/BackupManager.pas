{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : The Backup Manager does an automatic backup of all database files
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  26.03.03 wl                               TN1439.5 initial version
  28.03.03 wl                               TN1439.5 überarbeitet und lauffähig gemacht
  28.03.03 wl  CheckBackupCycle             TN1439.5 noch nicht fertig
  03.04.03 wl  CompleteZipBackup            TN1439.5 benutzt LogCompleteBackup
  10.04.03 wl  SavePathToZipAndDelete       TN1332.4 jetzt als public
  11.04.03 wl  SavePathToZipAndDelete       TN1332.4 Bug bei Pfadnamen beseitigt
  17.04.03 wl  CompleteZipBackup            TN1439.4 benutzt eine ganze Liste mit Aliasnamen
  01.10.03 wl  CompleteZipBackup            TN1610   FileName kann als Parameter übergeben werden
  10.10.05 wl                               TN2007   uses Utility statt FileCtrl
  21.09.06 wl  CompleteZipBackup            TN3325   result ist jetzt Name der Backup-Datei (vollständiger Pfad)
  22.01.08 wl                               TN3972   uses DatabaseProvider
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  16.12.09 pk  CompleteZipBackup            TN4933   TDatabaseProvider functions no longer class functions
  13.04.10 wl                               TN5044   uses FileUtilities
  17.06.10 pk                               TN5152.1 uses DataProviderFactory
  04.08.10 pk                               TN5218   Changes for Plugin database packages
  27.03.13 wl                               TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit BackupManager;


interface


uses
    CommonTypes;

type
    TBackupManager = class
    private
        FBackupPath: string;
        class function CheckBackupCycle(aPath: string; aDayCycle: integer): boolean;
    public
        // constructor
        constructor Create(aCurrentUser: IUser; aAliasNames: TArray<string>; aPath: string;
            aDayCycle: integer);
        // public methods
        function CompleteZipBackup(aCurrentUser: IUser; aAliasNames: TArray<string>;
            aFileName: string): string;
        class function SavePathToZipAndDelete(const aPath, aPassword: string;
            var aZipFileName: string): boolean;
    end;


implementation


uses
    SysUtils,
    Dialogs,
    Classes,
    ZipForge,
    FileUtilities,
    DatabaseProvider,
    DataProvider,
    DataProviderFactory;

{ TBackupManager }

constructor TBackupManager.Create(aCurrentUser: IUser; aAliasNames: TArray<string>; aPath: string;
    aDayCycle: integer);
begin
    inherited Create;

    FBackupPath := aPath;

    // Backup complete if backup cycle is reached
    if CheckBackupCycle(aPath, aDayCycle) then
        CompleteZipBackup(aCurrentUser, aAliasNames, '');
end;

function TBackupManager.CompleteZipBackup(aCurrentUser: IUser; aAliasNames: TArray<string>;
    aFileName: string): string;
const
    cFormatArchiveTimeStamp = 'yyyymmdd-hhnnss';
var
    xZipFileName, xCurrentPath, xTimeString: string;
    x: integer;
begin
    result := '';

    if (aFileName <> '') then
        xTimeString := aFileName
    else
        xTimeString := FormatDateTime(cFormatArchiveTimeStamp, Now);
    xCurrentPath := TFileUtilities.IncludeTrailingPathDelimiter(FBackupPath) +
        TFileUtilities.IncludeTrailingPathDelimiter(xTimeString);

    for x := 0 to high(aAliasNames) do
        if TFileUtilities.CopyAllFiles(TDataProviderFactory.Instance.GetAliasPath(aAliasNames[x]),
            xCurrentPath + aAliasNames[x]) <> '' then
            exit;

    xZipFileName := '';
    if not SavePathToZipAndDelete(xCurrentPath, '', xZipFileName) then
        exit;

    // Add user log for creating log archive
    aCurrentUser.LogCompleteBackup(xZipFileName);

    result := xZipFileName;
end;

class function TBackupManager.SavePathToZipAndDelete(const aPath, aPassword: string;
    var aZipFileName: string): boolean;
var
    xZipArchive: TZipForge;
    xArchiveItem: TZFArchiveItem;
    xFileName: string;
begin
    // create a new zip file
    xZipArchive := TZipForge.Create(nil);
    if (aZipFileName = '') then
        aZipFileName := TFileUtilities.ExcludeTrailingPathDelimiter(aPath) + '.zip';
    xZipArchive.FileName := aZipFileName;
    aZipFileName := xZipArchive.FileName;

    // Create a new archive file
    xZipArchive.BaseDir := aPath;

    // Let's encrypt all files
    if (aPassword <> '') then
        xZipArchive.Password := ansistring(aPassword);

    // save files in zip file
    xZipArchive.OpenArchive(fmCreate);
    xZipArchive.AddFiles('*.*', faAnyFile, '');

    // Search files stored inside the archive and delete the original files
    if (xZipArchive.FindFirst('*.*', xArchiveItem, faAnyFile)) then
        repeat
            xFileName := TFileUtilities.IncludeTrailingPathDelimiter(xZipArchive.BaseDir) +
                xArchiveItem.StoredPath;
            xFileName := TFileUtilities.IncludeTrailingPathDelimiter(xFileName) + xArchiveItem.FileName;
            TFileUtilities.DeleteFile(xFileName);
        until (not xZipArchive.FindNext(xArchiveItem));

    // Close the archive
    xZipArchive.CloseArchive;
    FreeAndNil(xZipArchive);

    // Delete directory
    result := true;
    TFileUtilities.DeleteDirectory(aPath, true);
end;

class function TBackupManager.CheckBackupCycle(aPath: string; aDayCycle: integer): boolean;
begin
    result := false;

    // TBD_WL
end;


end.
