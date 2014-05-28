{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : file handling utility methods
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.03.03 wl  gmDeleteAllFiles             TN1439.4 neue Funktion, die alle Dateien in einem Verzeichnis löscht
  25.03.03 wl  gmCopyAllFiles               TN1439.5 neu: alle Dateien in einem Verzeichnis kopieren
  28.03.03 wl  gmRemoveEmptyDirectories     TN1439.5 eingebucht, obwohl es noch nicht funktioniert!
  11.04.03 wl  gmRemoveEmptyDirectories     TN1439.5 löscht zumindest ein Directory
  25.05.04 wl  gmTakeStringsFromFile        TN1945   für Import-Funktionen: Inhalt einer Textdatei in StringList schreiben
  24.06.04 wl                               TN2007   FileCtrl-Aufrufe durch Utility-Aufrufe ersetzt
  30.08.04 wl  gmTakeAllStringsFromFile     TN2097   neu: liest alle Zeilen aus Datei
  18.01.05 wl  gmGetListOfFiles/WithoutExtension TN2246.4 entspricht gmGetFileList (Utility.pas), erzeugt aber keine Liste
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  // FileUtilities.pas:
  13.04.10 wl                               TN5044   FileUtilities enthält große Teile der ehemaligen units Utility_Files, UtilityWin32 und Utility
  15.04.10 wl  DeleteExtension              TN5044   Rückgabewert = FileName, wenn keine Extension
  17.05.10 wl  TSpecialFoldersInfo          TN5111   Funktionen zum Erkennen der speziellen Verzeichnisse
  17.05.10 wl  GetApplicationDataPath       TN5111   Nimmt den Application Path und ersetzt Program Files durch ProgramData
  08.06.10 pk  GetApplicationName           TN5111   New
  04.08.10 pk                               TN5218   Changes for Plugin database packages
  13.09.10 pk  ExtractFileExtension         TN5218   New
  14.12.10 wl  FileGetSize                  TN5411   neu
  11.04.11 wl  GetProgramDataFolder         TN5549   CSIDL_COMMON_DOCUMENTS statt CSIDL_COMMON_APPDATA
  25.07.12 ts  WriteStringsToFile           TN5944   EndOfLineChar for each line can be set in the settings, SaveToFile used automatically CR+LF
  27.03.13 wl                               TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit FileUtilities;


interface


uses
    GeneralTypes;

type
    TSpecialFoldersInfo = record
    strict private
        class function GetSpecialFolder(const CSIDL: integer): string; static;
    public
        class function GetProgramFolder(): string; static;
        class function GetProgramX86Folder(): string; static;
        class function GetProgramDataFolder(): string; static;
    end;

    TFileUtilities = record
    private const
        STR_READ_ALL_LINES = -999;
    public
        // aus Utility_Files:
        class function DeleteAllFiles(aPath: string): string; static;
        class function CopyAllFiles(aSourcePath, aDestPath: string): string; static;
        class function TakeAllStringsFromFile(const aFileName: string): TStringArray; static;
        class function TakeStringsFromFile(const aFileName: string; aFirstLine, aMaxLines: integer)
            : TStringArray; static;
        class procedure WriteStringsToFile(const aFileName: string; const aStrings: TStringArray); static;
        class function GetListOfFilesWithoutExtension(const aPath, aExtension: string): TStringArray; static;
        class function GetListOfFiles(const aPath, aExtension: string): TStringArray; static;
        class function DeleteExtension(const aFileName: string): string; static;

        // aus Utility:
        class function ExtractFileExtension(const aFileName: string): string; static;
        class function IncludeTrailingPathDelimiter(const S: string): string; static;
        class function ExcludeTrailingPathDelimiter(const S: string): string; static;
        class function ConcatPaths(const aRootPath, aAddedPath: string): string; static;
        class function ForceDirectories(const aDir: string): Boolean; static;
        class function DirectoryExists(const aDir: string): boolean; static;
        class function ExtractFileDir(const aFileName: string): string; static;
        class function CreateExportFileName(aPathName, aFileName, aExtension, aDTStamp: string;
            aAddDateTime, aNumbering: boolean): string; static;
        class procedure CreateExportFile(const aFileName: string); static;

        // aus UtilityWin32:
        class function MoveFile(const aExistingFileName, aNewFileName: string): boolean; static;
        class function CopyFile(const aExistingFileName, aNewFileName: string; aFailIfExists: boolean)
            : boolean; static;
        class function DeleteFile(const aFileName: string): boolean; static;
        class function FileExists(const aFileName: string): boolean; static;
        class function FileGetSize(const aFileName: string): int64; static;

        // neu:
        class procedure DeleteDirectory(const aPath: string; const aRecursive: boolean = false); static;
        class function ReplaceFolder(const aPath, aSourceFolder, aDestFolder: string): string; static;
        class function ReplaceProgramFolderByDataFolder(const aPath: string): string; static;
        class function GetRealApplicationPath(): string; static;
        class function GetApplicationDataPath(): string; static;
        class function GetApplicationName(): string; static;
    end;


implementation


uses
    Forms,
    ShlObj,
    Windows,
    SysUtils,
    Classes,
    IOUtils,
    Generics.Collections,
    UtilLib;

{ TSpecialFoldersInfo }

class function TSpecialFoldersInfo.GetProgramDataFolder: string;
begin
    result := GetSpecialFolder(CSIDL_COMMON_DOCUMENTS);
end;

class function TSpecialFoldersInfo.GetProgramFolder: string;
begin
    result := GetSpecialFolder(CSIDL_PROGRAM_FILES);
end;

class function TSpecialFoldersInfo.GetProgramX86Folder: string;
begin
    result := GetSpecialFolder(CSIDL_PROGRAM_FILESX86);
end;

class function TSpecialFoldersInfo.GetSpecialFolder(const CSIDL: integer): string;
var
    RecPath: PWideChar;
begin
    RecPath := StrAlloc(MAX_PATH);
    try
        FillChar(RecPath^, MAX_PATH, 0);
        if SHGetSpecialFolderPath(0, RecPath, CSIDL, false) then
        begin
            result := RecPath;
        end
        else
            result := '';
    finally
        StrDispose(RecPath);
    end;
end;

{ TFileUtilities }

class function TFileUtilities.DeleteAllFiles(aPath: string): string;
// --------------------------------------------------------------------------------------------------
// delete all files in one directory (without subdirectories)!
// result: if an error occurs - the name of the file thet can not be deleted will be returned
// --------------------------------------------------------------------------------------------------
var
    x: integer;
    xSearchRec: TSearchRec;
begin
    result := '';
    x := FindFirst(TFileUtilities.IncludeTrailingPathDelimiter(aPath) + '*.*', 0, xSearchRec);
    while (x = 0) do
    begin
        if (xSearchRec.Attr and faAnyFile > 0) then
            if not DeleteFile(TFileUtilities.IncludeTrailingPathDelimiter(aPath) + xSearchRec.Name) then
            begin
                result := xSearchRec.Name;
                exit;
            end;
        x := FindNext(xSearchRec);
    end;
    SysUtils.FindClose(xSearchRec);
end;

class function TFileUtilities.CopyAllFiles(aSourcePath, aDestPath: string): string;
// --------------------------------------------------------------------------------------------------
// copy all files from one directory to another (without subdirectories)!
// result: if an error occurs - the name of the file thet can not be copied will be returned
// --------------------------------------------------------------------------------------------------
var
    x: integer;
    xSearchRec: TSearchRec;
begin
    TFileUtilities.ForceDirectories(aDestPath);
    result := '';
    x := FindFirst(TFileUtilities.IncludeTrailingPathDelimiter(aSourcePath) + '*.*', 0, xSearchRec);
    while (x = 0) do
    begin
        if (xSearchRec.Attr and faAnyFile > 0) then
            if not CopyFile(PChar(TFileUtilities.IncludeTrailingPathDelimiter(aSourcePath) + xSearchRec.Name),
                PChar(TFileUtilities.IncludeTrailingPathDelimiter(aDestPath) + xSearchRec.Name), true) then
            begin
                result := xSearchRec.Name;
                exit;
            end;
        x := FindNext(xSearchRec);
    end;
    SysUtils.FindClose(xSearchRec);
end;

class function TFileUtilities.TakeAllStringsFromFile(const aFileName: string): TStringArray;
begin
    result := TakeStringsFromFile(aFileName, 1, STR_READ_ALL_LINES);
end;

class function TFileUtilities.TakeStringsFromFile(const aFileName: string; aFirstLine, aMaxLines: integer)
    : TStringArray;
var
    ImportLine: string;
    ImportFile: TextFile;
    xLine: integer;
    xFileIsOpen: boolean;
    xList: TList<string>;
begin
    xLine := 0;
    xFileIsOpen := false;

    try
        // Existiert die Importdatei?
        if not FileExists(aFileName) then
            raise Exception.Create('Import file ' + aFileName + ' not found!');

        // Import File öffnen
        AssignFile(ImportFile, aFileName);
        Reset(ImportFile);
        xFileIsOpen := true;

        xList := TList<string>.Create();
        try
            // Daten in TStringList einlesen
            while not eof(ImportFile) do
            begin
                inc(xLine);
                ReadLn(ImportFile, ImportLine);

                if (xLine >= aFirstLine) then
                    xList.Add(ImportLine);

                if (aMaxLines <> STR_READ_ALL_LINES) and (xLine > aMaxLines) then
                    exit;
            end;
            result := xList.ToArray();
        finally
            FreeAndNil(xList);
        end;
    finally
        if (xFileIsOpen) then
            CloseFile(ImportFile);
    end;
end;

class procedure TFileUtilities.WriteStringsToFile(const aFileName: string; const aStrings: TStringArray);
var
    x: integer;
    xFile: TextFile;
begin
    AssignFile(xFile, aFileName);
    try
        Rewrite(xFile);
        for x := 0 to Length(aStrings) - 1 do
            write(xFile, aStrings[x]);
    finally
        CloseFile(xFile);
    end;
end;

class function TFileUtilities.GetListOfFiles(const aPath, aExtension: string): TStringArray;
var
    x: integer;
    xSearchRec: TSearchRec;
    xList: TList<string>;
    xSearchPath: string;
begin
    xSearchPath := ConcatPaths(aPath, '*.' + aExtension);
    x := FindFirst(xSearchPath, 0, xSearchRec);

    xList := TList<string>.Create();
    try
        while (x = 0) do
        begin
            xList.Add(xSearchRec.Name);
            x := FindNext(xSearchRec);
        end;
        result := xList.ToArray();
    finally
        FreeAndNil(xList);
    end;

    FindClose(xSearchRec);
end;

class procedure TFileUtilities.DeleteDirectory(const aPath: string; const aRecursive: boolean = false);
begin
    TDirectory.Delete(aPath, aRecursive);
end;

class function TFileUtilities.DeleteExtension(const aFileName: string): string;
var
    aExtension: string;
begin
    aExtension := ExtractFileExt(aFileName);
    if (aExtension = '') then
    begin
        result := aFileName;
        EXIT;
    end;

    result := Copy(aFileName, 1, Length(aFileName) - Length(aExtension));
end;

class function TFileUtilities.GetListOfFilesWithoutExtension(const aPath, aExtension: string): TStringArray;
var
    x: integer;

begin
    result := GetListOfFiles(aPath, aExtension);
    for x := 0 to Length(result) - 1 do
        result[x] := DeleteExtension(result[x]);
end;

class procedure TFileUtilities.CreateExportFile(const aFileName: string);
var
    xPathName: string;
    xFile: Textfile;
begin
    xPathName := ExtractFilePath(aFileName);

    // Pfad prüfen und anlegen
    if not DirectoryExists(xPathName) then
        ForceDirectories(xPathName);

    // existierende Datei plattmachen
    if FileExists(aFileName) then
        DeleteFile(aFileName);

    try
        AssignFile(xFile, aFileName);
        Rewrite(xFile);
    finally
        CloseFile(xFile);
    end;
end;

class function TFileUtilities.CreateExportFileName(aPathName, aFileName, aExtension, aDTStamp: string;
    aAddDateTime, aNumbering: boolean): string;
// ------------------------------------------------------------------------------
// InputParameter :
// Pathname  = Pfadname
// FileName  = Dateiname ohne Extension
// Extension = Extension ohne Punkt
// AddDateTime = wenn True dann wird Datum+Uhrzeit ('JJJJMMTT HHMMSS') an FileName angehängt (+Extension)
// Numbering   = wenn True dann wird eine IntegerZahl beginnend mit 1 anstelle der Extension an FileName angehängt
// Vorher wird geprüft ob eine Datei mit diesem Namen bereits existiert
// Wenn Ja - dann wird die IntegerZahl solange hochgezählt bis ein Dateiname entsteht der noch nicht existiert
// Result :
// Dateiname des TextFiles mit vollständigem Pfad und Extension
// ------------------------------------------------------------------------------
var
    xDateTimeStr: string;
    x: integer;
begin
    if (aPathName = '') then
        aPathName := '.';
    xDateTimeStr := '';
    result := aPathName + '\' + aFileName + '.' + aExtension;

    if aAddDateTime then
    begin
        xDateTimeStr := FormatDateTime(aDTStamp, now);
        result := aPathName + '\' + aFileName + xDateTimeStr + '.' + aExtension;
    end;

    x := 1;
    if (aNumbering) then
    begin
        while FileExists(result) do
        begin
            result := aPathName + '\' + aFileName + xDateTimeStr + IntToStr(x) + '.' + aExtension;
            x := x + 1;
        end;
    end;
end;

class function TFileUtilities.CopyFile(const aExistingFileName, aNewFileName: string;
    aFailIfExists: boolean): boolean;
begin
    result := Windows.CopyFile(PChar(aExistingFileName), PChar(aNewFileName), aFailIfExists);
end;

class function TFileUtilities.DeleteFile(const aFileName: string): boolean;
begin
    result := Windows.DeleteFile(PChar(aFileName));
end;

class function TFileUtilities.ExcludeTrailingPathDelimiter(const S: string): string;
begin
    result := SysUtils.ExcludeTrailingPathDelimiter(S);
end;

class function TFileUtilities.ExtractFileDir(const aFileName: string): string;
begin
    result := SysUtils.ExtractFileDir(aFileName);
end;

class function TFileUtilities.ExtractFileExtension(const aFileName: string): string;
begin
    result := SysUtils.ExtractFileExt(aFileName);
end;

class function TFileUtilities.FileExists(const aFileName: string): boolean;
begin
    result := SysUtils.FileExists(PChar(aFileName));
end;

class function TFileUtilities.ForceDirectories(const aDir: string): boolean;
begin
    result := SysUtils.ForceDirectories(aDir);
end;

class function TFileUtilities.DirectoryExists(const aDir: string): boolean;
begin
    result := SysUtils.DirectoryExists(aDir);
end;

class function TFileUtilities.IncludeTrailingPathDelimiter(const S: string): string;
begin
    result := SysUtils.IncludeTrailingPathDelimiter(S);
end;

class function TFileUtilities.ConcatPaths(const aRootPath, aAddedPath: string): string;
begin
    result := IncludeTrailingPathDelimiter(aRootPath) + aAddedPath;
end;

class function TFileUtilities.MoveFile(const aExistingFileName, aNewFileName: string): boolean;
begin
    result := Windows.MoveFile(PChar(aExistingFileName), PChar(aNewFileName));
end;

class function TFileUtilities.ReplaceFolder(const aPath, aSourceFolder, aDestFolder: string): string;
begin
    result := IncludeTrailingPathDelimiter(aDestFolder) + Copy(aPath, Length(aSourceFolder) + 1,
        Length(aPath) - Length(aSourceFolder));
end;

class function TFileUtilities.ReplaceProgramFolderByDataFolder(const aPath: string): string;
var
    xProgFolder: string;
begin
    result := aPath;

    // Diese Funktion ist nur für Windows Vista und folgende relevant!
    if not TOSInfo.VersionIsVistaOrHigher then
        EXIT;

    // C:\Program files
    xProgFolder := IncludeTrailingPathDelimiter(TSpecialFoldersInfo.GetProgramFolder);
    if Pos(xProgFolder, aPath) = 1 then
    begin
        result := ReplaceFolder(aPath, xProgFolder, TSpecialFoldersInfo.GetProgramDataFolder);
        EXIT;
    end;

    // C:\Program files (x86) [32 bit auf 64-Bit-Systemen]
    xProgFolder := IncludeTrailingPathDelimiter(TSpecialFoldersInfo.GetProgramX86Folder);
    if Pos(xProgFolder, aPath) = 1 then
        result := ReplaceFolder(aPath, xProgFolder, TSpecialFoldersInfo.GetProgramDataFolder);
end;

class function TFileUtilities.GetApplicationDataPath(): string;
begin
    result := ReplaceProgramFolderByDataFolder(GetRealApplicationPath());
end;

class function TFileUtilities.GetApplicationName: string;
begin
    result := DeleteExtension(ExtractFileName(Application.ExeName));
end;

class function TFileUtilities.GetRealApplicationPath(): string;
begin
    result := ExtractFilePath(Application.ExeName);
end;

class function TFileUtilities.FileGetSize(const aFileName: string): int64;
var
    xFileStream: TFileStream;
begin
    xFileStream := TFileStream.Create(aFileName, 0);
    try
        result := xFileStream.Size;
    finally
        FreeAndNil(xFileStream);
    end;
end;


end.
