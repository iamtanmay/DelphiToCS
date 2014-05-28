{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Data collection application for support cases
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  02.11.12 wl                                      TN6003   Initial Revision
  27.01.13 wl                                      TN6069   baut jetzt auf WinLissy-Settings auf
  27.01.13 wl                                      TN6069.1 Bugfix
  11.02.13 wl  fIniFileName                        TN6078   entfernt
  ----------------------------------------------------------------------------------------------------------- }

unit DataCollectSettings;


interface


uses
    Generics.Collections,
    CommonTypes;

type
    TPathSettingTypeIntern = (tUnknown, tFile, tFolder);

    TDataCollectSettings = class
    private
        fAddresses: string;
        fPathSettings: TArray<TBackupPathData>;
        fDestinationPath: string;
        fSendMailButton: boolean;
        function ReadPathSettings(aIniAccess: IWinLissyIniAccess): TArray<TBackupPathData>;
    public
        constructor Create();
        procedure ReadAll();
        property Addresses: string read fAddresses;
        property PathSettings: TArray<TBackupPathData>read fPathSettings;
        property DestinationPath: string read fDestinationPath;
        property SendMailButton: boolean read fSendMailButton;
    end;


implementation


uses
    Forms,
    Classes,
    SysUtils,
    AppSettings,
    DataProviderFactory,
    FileUtilities,
    LogManager;

{ TDataCollectSettings }

constructor TDataCollectSettings.Create();
begin
    inherited Create;
end;

function TDataCollectSettings.ReadPathSettings(aIniAccess: IWinLissyIniAccess): TArray<TBackupPathData>;
const
    cNumberOfHardcodedEntries = 4;
    cBasicArchiveName = 'Backup';
var
    xNames: TArray<string>;
    x: integer;
begin
    xNames := aIniAccess.ReadAllowedSection('BackupAddFiles', '');
    SetLength(result, Length(xNames) + cNumberOfHardcodedEntries);

    // hardcoded Entries:
    // 1. Log file
    result[0].IsFolder := false;
    result[0].ArchiveName := 'Log';
    result[0].PathName := TLogManager.Instance.LogFileName;
    result[0].Subfolders := true;
    result[0].CheckDate := false;
    // 2. Data
    result[1].IsFolder := true;
    result[1].ArchiveName := 'Data';
    result[1].PathName := TFileUtilities.ExcludeTrailingPathDelimiter(TAppSettings.DataPath);
    result[1].Subfolders := true;
    result[1].CheckDate := false;
    // 3. DBT
    result[2].IsFolder := true;
    result[2].ArchiveName := 'DBT';
    result[2].PathName := // 'K:\Delphi32\Test\DBT';
        TDataProviderFactory.Instance.GetAliasPath(TDataProviderFactory.Instance.MainDBAlias);
    // TAppSettings.Alias;
    result[2].Subfolders := true;
    result[2].CheckDate := false;
    // 4. Log archive
    result[3].IsFolder := true;
    result[3].ArchiveName := 'Archive';
    result[3].PathName := TLogManager.Instance.LogFilePath;
    result[3].Subfolders := true;
    result[3].CheckDate := true;

    for x := 0 to Length(xNames) - 1 do
    begin
        // Übernehmen beliebig vieler Tool-Menu-Einträge
        result[cNumberOfHardcodedEntries + x] := aIniAccess.ReadBackupPathData('BackupAddFiles', xNames[x]);
    end;
end;

procedure TDataCollectSettings.ReadAll;
var
    xIniAccess: IWinlissyIniAccess;
begin
    xIniAccess := TAppSettings.CreateAppIni();
    fPathSettings := ReadPathSettings(xIniAccess);
    fAddresses := xIniAccess.ReadString('Backup', 'MailAddresses');
    fSendMailButton := xIniAccess.ReadBool('Backup', 'MailEnabled');
    fDestinationPath := xIniAccess.ReadString('Backup', 'Path');
    xIniAccess := nil;
end;


end.
