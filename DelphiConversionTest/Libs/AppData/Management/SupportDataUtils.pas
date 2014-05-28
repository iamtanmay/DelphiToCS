{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Data collection application for support cases
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  02.11.12 wl                                      TN6003   Initial Revision
  27.01.13 wl                                      TN6069   an WinLissy angepasst
  11.02.13 wl  CollectFiles                        TN6083   DestinationPath kann jetzt überschrieben werden
  ----------------------------------------------------------------------------------------------------------- }

unit SupportDataUtils;


interface


uses
    CommonTypes,
    DataCollectSettings;

type
    TZipFileData = record
        FileName: string;
        FileCount: integer;
        Size: int64;
    end;

    TSupportDataUtils = record
    private
        class procedure DeleteFilesWithOtherDate(const aPath: string; aDateTime: TDateTime); static;
    public
        class procedure SendMail(const aSubject, aMailtext, aFromName: Ansistring;
            aRecipients: TArray<Ansistring>; const aAttachedFiles: TArray<string>;
            const aDisplayFileName: Ansistring; aShowDialog: boolean); static;
        class procedure CopyFilesToTempFolder(const aTempPath: string; aAllFiles: boolean;
            aDateTime: TDateTime; aPathSetting: TBackupPathData); static;
        class function SaveFilesToZip(const aZipFileName: string; aAllFiles: boolean; aDateTime: TDateTime;
            const aPassword, aTempPath: string): TZipFileData; static;
        class function StringToStringArray(const aStr, aListSeparator: string): TArray<Ansistring>; static;
    end;

    TSupportDataSetPositionEvent = procedure(aValue: integer) of object;
    TSupportDataAddTextEvent = procedure(const aText: string) of object;

    TSupportDataBackup = class
    private
        fSettings: TDataCollectSettings;
        fOnSetPosition: TSupportDataSetPositionEvent;
        fOnAddText: TSupportDataAddTextEvent;
        procedure DoSetProgressBarPosition(aValue: integer);
        procedure DoAddText(const aText: string);
    public
        constructor Create(aOnSetPosition: TSupportDataSetPositionEvent;
            aOnAddText: TSupportDataAddTextEvent);
        destructor Destroy; override;
        function UseMail: boolean;
        function CollectFiles(aAllFiles: boolean; aDateTime: TDateTime;
            aDestinationPath: string = ''): string;
        function GetAddresses: string;
    end;


implementation


uses
    WinTypes,
    Forms,
    ComCtrls,
    Classes,
    ShellAPI,
    Types,
    IOUtils,
    MAPI,
    SysUtils,
    ZipForge,
    Dialogs;

{ TSupportDataUtils }

class procedure TSupportDataUtils.CopyFilesToTempFolder(const aTempPath: string; aAllFiles: boolean;
    aDateTime: TDateTime; aPathSetting: TBackupPathData);
var
    xArchivePath, xNewDirName: string;
    xDirs: TStringDynArray;
    x: integer;
begin
    xArchivePath := IncludeTrailingPathDelimiter(aTempPath) + aPathSetting.ArchiveName;

    TDirectory.CreateDirectory(xArchivePath);

    // Alles kopieren
    if (not aPathSetting.IsFolder) then
    begin
        TFile.Copy(aPathSetting.PathName, IncludeTrailingPathDelimiter(xArchivePath) +
            ExtractFileName(aPathSetting.PathName));
    end
    else
    begin
        xNewDirName := IncludeTrailingPathDelimiter(xArchivePath) + ExtractFileName(aPathSetting.PathName);
        TDirectory.Copy(aPathSetting.PathName, xNewDirName);
        if (not aPathSetting.Subfolders) then
        begin
            // Dateien einzeln kopieren
            xDirs := TDirectory.GetDirectories(xNewDirName);
            for x := 0 to high(xDirs) do
                TDirectory.Delete(xDirs[x], true);
        end;
    end;

    if (not aAllFiles) and (aPathSetting.CheckDate) then
    begin
        DeleteFilesWithOtherDate(xArchivePath, aDateTime);
    end;
end;

function SameDay(a, b: TDateTime): Boolean;
begin
    result := formatdatetime('dd.mm.yy', a) = formatdatetime('dd.mm.yy', b);
end;

class procedure TSupportDataUtils.DeleteFilesWithOtherDate(const aPath: string; aDateTime: TDateTime);
var
    xFound: TStringDynArray;
    x: integer;
    xLastWriteTime: TDateTime;
begin
    xFound := TDirectory.GetDirectories(aPath);
    for x := 0 to high(xFound) do
        DeleteFilesWithOtherDate(xFound[x], aDateTime); // REKURSION!

    xFound := TDirectory.GetFiles(aPath);
    for x := 0 to high(xFound) do
    begin
        xLastWriteTime := TFile.GetLastWriteTime(xFound[x]);
        if not SameDay(xLastWriteTime, aDateTime) then
            TFile.Delete(xFound[x]);
    end;
end;

class function TSupportDataUtils.SaveFilesToZip(const aZipFileName: string; aAllFiles: boolean;
    aDateTime: TDateTime; const aPassword, aTempPath: string): TZipFileData;
var
    xZipArchive: TZipForge;
    // xArchiveItem: TZFArchiveItem;
begin
    // create a new zip file
    xZipArchive := TZipForge.Create(nil);
    try
        result.FileName := aZipFileName;
        xZipArchive.FileName := result.FileName;
        xZipArchive.BaseDir := aTempPath;

        // Let's encrypt all files
        if (aPassword <> '') then
            xZipArchive.Password := ansistring(aPassword);

        // save files in zip file
        xZipArchive.OpenArchive(fmCreate);
        try
            // xZipArchive.
            xZipArchive.AddFiles('*.*', faAnyFile);

            // Search files stored inside the archive and delete the original files
            { if (xZipArchive.FindFirst('*.*', xArchiveItem, faAnyFile)) then
              repeat
              xFileName := IncludeTrailingPathDelimiter(xZipArchive.BaseDir) + xArchiveItem.StoredPath;
              xFileName := IncludeTrailingPathDelimiter(xFileName) + xArchiveItem.FileName;
              until (not xZipArchive.FindNext(xArchiveItem));
            }
            result.FileCount := xZipArchive.FileCount;
            result.Size := xZipArchive.Size;
        finally
            // Close the archive
            xZipArchive.CloseArchive;
        end;
    finally
        FreeAndNil(xZipArchive);
    end;
end;

class procedure TSupportDataUtils.SendMail(const aSubject, aMailtext, aFromName: Ansistring;
    aRecipients: TArray<Ansistring>; const aAttachedFiles: TArray<string>; const aDisplayFileName: Ansistring;
    aShowDialog: boolean);
var
    MapiMessage: TMapiMessage;
    MError: Cardinal;
    xRecips: array [0 .. 10] of TMapiRecipDesc; // dynamisch geht leider nicht
    xOriginator: TMapiRecipDesc;
    xFiles: array [0 .. 10] of TMapiFileDesc;
    x: integer;
    xFilePaths, xFileNames: TArray<Ansistring>;
begin
    with MapiMessage do
    begin
        ulReserved := 0;

        // Betreff
        lpszSubject := PAnsiChar(aSubject);

        // Body
        lpszNoteText := PAnsiChar(aMailtext);

        lpszMessageType := nil;
        lpszDateReceived := nil;
        lpszConversationID := nil;
        flFlags := 0;

        // Absender festlegen
        xOriginator.ulReserved := 0;
        xOriginator.ulRecipClass := MAPI_ORIG;
        xOriginator.lpszName := PAnsiChar(aFromName);
        xOriginator.lpszAddress := PAnsiChar(aFromName);
        xOriginator.ulEIDSize := 0;
        xOriginator.lpEntryID := nil;
        lpOriginator := @xOriginator;

        // Empfänger festlegen (Hier: nur 1 Empfänger)
        nRecipCount := Length(aRecipients);
        for x := 0 to high(aRecipients) do
        begin
            xRecips[x].ulReserved := 0;
            xRecips[x].ulRecipClass := MAPI_TO;
            xRecips[x].lpszName := PAnsiChar(aRecipients[x]);
            xRecips[x].lpszAddress := PAnsiChar(aRecipients[x]);
            xRecips[x].ulEIDSize := 0;
            xRecips[x].lpEntryID := nil;
        end;
        lpRecips := @xRecips;

        // Dateien anhängen (Hier: nur 1 Datei)
        nFileCount := Length(aAttachedFiles);
        SetLength(xFilePaths, Length(aAttachedFiles));
        SetLength(xFileNames, Length(aAttachedFiles));

        for x := 0 to high(aAttachedFiles) do
        begin
            // Name der Datei auf der Festplatte
            xFilePaths[x] := Ansistring(aAttachedFiles[x]); // jeden AnsiString mit eigenem Pointer speichern
            xFiles[x].lpszPathName := PAnsiChar(xFilePaths[x]);

            // Name, der in der Email angezeigt wird
            xFileNames[x] := Ansistring(ExtractFileName(aAttachedFiles[x]));
            xFiles[x].lpszFileName := PAnsiChar(xFileNames[x]);
            xFiles[x].ulReserved := 0;
            xFiles[x].flFlags := 0;
            xFiles[x].nPosition := Cardinal(-1);
            xFiles[x].lpFileType := nil;
        end;

        lpFiles := @xFiles;
    end;

    // Senden
    if aShowDialog then
        MError := MapiSendMail(0, Application.Handle, MapiMessage, MAPI_DIALOG or MAPI_LOGON_UI, 0)
    else
        // Wenn kein Dialogfeld angezeigt werden soll:
        MError := MapiSendMail(0, Application.Handle, MapiMessage, 0, 0);

    case MError of
        MAPI_E_AMBIGUOUS_RECIPIENT:
            MessageDlg('Ambiguous Recipient/No recipient', mterror, [mbok], 0);

        MAPI_E_ATTACHMENT_NOT_FOUND:
            MessageDlg('Attached file not found', mterror, [mbok], 0);

        MAPI_E_ATTACHMENT_OPEN_FAILURE:
            MessageDlg('Attached file could not be opened.', mterror, [mbok], 0);

        MAPI_E_BAD_RECIPTYPE:
            MessageDlg('recipient type is not MAPI_TO, MAPI_CC or MAPI_BCC.', mterror, [mbok], 0);

        MAPI_E_FAILURE:
            MessageDlg('Unknown failure.', mterror, [mbok], 0);

        MAPI_E_INSUFFICIENT_MEMORY:
            MessageDlg('Insufficient memory.', mterror, [mbok], 0);

        MAPI_E_LOGIN_FAILURE:
            MessageDlg('User login failure (e.g. Outlook).', mterror, [mbok], 0);

        MAPI_E_TEXT_TOO_LARGE:
            MessageDlg('Text too large.', mterror, [mbok], 0);

        MAPI_E_TOO_MANY_FILES:
            MessageDlg('Too many attached files.', mterror, [mbok], 0);

        MAPI_E_TOO_MANY_RECIPIENTS:
            MessageDlg('Too many recipients.', mterror, [mbok], 0);

        MAPI_E_UNKNOWN_RECIPIENT:
            MessageDlg('Recipient not found in address book', mterror, [mbok], 0);

        // MAPI_E_USER_ABORT:
        // MessageDlg('Benutzer hat Senden abgebrochen oder MAPI nicht installiert.',mterror,[mbok],0);

        SUCCESS_SUCCESS:
            MessageDlg('Successful sending is not granted.', mtinformation, [mbok], 0);

    end;
end;

class function TSupportDataUtils.StringToStringArray(const aStr, aListSeparator: string): TArray<Ansistring>;
var
    xCount: integer;
    xStr: string;
    xPos: integer;
    xLen, xDelimLen: integer;
begin
    SetLength(result, 0);
    xStr := aStr;
    xCount := 0;
    xDelimLen := Length(aListSeparator);
    while true do
    begin
        xLen := Length(xStr);
        if xLen = 0 then
            BREAK;

        xPos := Pos(aListSeparator, xStr);
        if xPos = 0 then
            xPos := xLen
        else
            xPos := xPos - 1;

        Inc(xCount);

        // Array um neuen Wert erweitern
        SetLength(result, xCount);
        result[xCount - 1] := AnsiString(Copy(xStr, 1, xPos));

        // String kürzen
        xStr := Copy(xStr, 1 + xPos + xDelimLen, Length(xStr));
    end;
end;

{ TSupportDataBackup }

function TSupportDataBackup.CollectFiles(aAllFiles: boolean; aDateTime: TDateTime;
    aDestinationPath: string): string;
var
    x: integer;
    xTimeStamp, xZipFileName: string;
    xZip: TZipFileData;
    xTempPath: string;
begin
    if (aDestinationPath = '') then
        aDestinationPath := fSettings.DestinationPath;

    xTempPath := IncludeTrailingPathDelimiter(aDestinationPath) + 'TEMP';

    // TEMP-Verzeichnis löschen und neu erzeugen
    if TDirectory.Exists(xTempPath) then
    begin
        TDirectory.Delete(xTempPath, true);
        Sleep(500);
    end;
    TDirectory.CreateDirectory(xTempPath);

    for x := 0 to high(fSettings.PathSettings) do
    begin
        TSupportDataUtils.CopyFilesToTempFolder(xTempPath, aAllFiles, aDateTime, fSettings.PathSettings[x]);
        DoSetProgressBarPosition(Round((x + 1) / Length(fSettings.PathSettings) * 50));
    end;

    DateTimeToString(xTimeStamp, 'yyyymmdd_hhnnss', now);
    xZipFileName := IncludeTrailingPathDelimiter(aDestinationPath) + 'Backup_' + xTimeStamp + '.zip';

    xZip := TSupportDataUtils.SaveFilesToZip(xZipFileName, aAllFiles, aDateTime, '', xTempPath);

    DoAddText(xZip.FileName + ':');
    DoAddText('Number of files: ' + IntToStr(xZip.FileCount));
    DoAddText('Size: ' + IntToStr(xZip.Size) + ' Bytes');

    result := xZip.FileName;

    // TEMP-Verzeichnis löschen
    if TDirectory.Exists(xTempPath) then
    begin
        TDirectory.Delete(xTempPath, true);
        Sleep(500);
    end;
end;

constructor TSupportDataBackup.Create(
    aOnSetPosition: TSupportDataSetPositionEvent; aOnAddText: TSupportDataAddTextEvent);
begin
    inherited Create;
    fOnSetPosition := aOnSetPosition;
    fOnAddText := aOnAddText;

    fSettings := TDataCollectSettings.Create();
    fSettings.ReadAll();
end;

destructor TSupportDataBackup.Destroy;
begin
    FreeAndNil(fSettings);

    inherited;
end;

procedure TSupportDataBackup.DoAddText(const aText: string);
begin
    if Assigned(fOnAddText) then
        fOnAddText(aText);
end;

procedure TSupportDataBackup.DoSetProgressBarPosition(aValue: integer);
begin
    if Assigned(fOnSetPosition) then
        fOnSetPosition(aValue);
end;

function TSupportDataBackup.GetAddresses: string;
begin
    EXIT(fSettings.Addresses);
end;

function TSupportDataBackup.UseMail: boolean;
begin
    EXIT(fSettings.SendMailButton);
end;


end.
