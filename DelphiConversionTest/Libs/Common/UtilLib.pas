{ --------------------------------------------------------------------------------------------------
  General Purpose Utilities: conversions, string tools, TControl tools
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     track-no   Änderung / Neuerung
  -------- --  -------------------    --------   ----------------------------------------------------
  14.06.02 pk                                    Created
  24.06.02 pk  PadWith                           New
  24.06.02 pk  ReplaceString                     New
  14.07.02 pk  ParseString1                      New
  14.07.02 pk  DefIfNull                         New
  14.07.02 pk  CopyTillEnd                       New
  14.07.02 pk  ListToString                      New
  14.07.02 pk  StringToList                      New
  14.07.02 pk  ReplaceString                     New
  14.07.02 pk  CopyListToUniqueList              New
  02.09.02 pk  FreeList                          New for TStrings
  02.09.02 pk  EncodeToDays                      New similar to EncodeToHours
  02.09.02 pk  StrDateTimeToMSecs                New
  15.09.03 pk  SplitStr                TN1556    New
  15.09.03 pk                          TN1556    Removed warnings
  27.04.04 pk                          TN1880    New RemoveBorderIcons, AskForNewName, OptimizeGridSize, ...
  15.06.04 pk                          TN1980    New   StringArrayOf, ListToStringArray, StringToStringArray
  17.05.04 pk  StrToFloatDef           TN1993    New
  24.06.04 pk  SetControlProperty      TN2009.1  New Property cpColor
  31.08.04 pk  SetControlProperty      TN2112    New Property cpClose
  08.12.04 pk  StringArrayToString     TN2261.1  New write array entries to a delimited string
  19.01.05 pk                          TN2281.0  TStringArray, TIntArray from AppTypes to GeneralTypes
  02.03.05 pk  ObjToInt, IntToObj      TN2328    New : convert TObject to LongInt and visa versa
  02.06.05 pk  FreeList                TN2449    Check if ResultList is Assigned
  23.06.05 pk  DblArrayToString        TN2471.1  New
  23.06.05 pk  StringToDblArry         TN2471.1  New
  23.06.05 pk  RoundBy                 TN2471.1  New
  23.06.05 pk  StringArrayToList       TN2471.1  New
  05.02.07 pk  StringArrayOf           TN3544    move to generaltypes
  13.03.07 wl  gmGetPossibleSeparators TN3634    fügt verschiedene Seperators einer Liste hinzu
  18.02.08 wl  TValueConverter         TN4009    neu: für alle Umwandlungen Double <-> String
  21.02.08 wl  TValueConverter.StrToFloatDef  TN4009  jetzt auch mit Rückgabewert
  15.04.08 wl  gmAskForNewName         TN4060   --> DialogUtils
  15.04.08 wl  WinControl utilities    TN4060   --> DialogUtils (TControlUtils)
  17.04.08 wl  TArrayUtils             TN4060   neu für alle Arrays
  17.04.08 wl  gmGetPossibleSeparators TN4067   removed; function was already implemented in TFileInfo (ImportDataProvider)
  20.09.08 wl  StrToFloatTryBoth       TN4224   neu
  19.11.08 wl  gmAnsiStringToString    TN4312   neu für Delphi 2009
  19.11.08 wl  AddArrayToList          TN4312   entfernt
  11.08.09 wl  TValueConverter         TN4711   arbeitet mit TFormatSettings und produziert nicht mehr so viele Exceptions
  17.08.09 wl  TValueConverter         TN4227   Namensänderungen für Settings-Funktionen
  04.09.09 wl  TArrayUtils             TN4740   -> ArrayUtils
  04.11.09 pk                          TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.04.10 wl  TStringUtils            TN5044   -> StringUtilities
  13.04.10 wl  gmCreateEvent,..        TN5044   aus der aufgelösten unit UtilityWin32
  21.07.10 wl  TOSInfo                 TN5202   Funktionen zum Erkennen der OS-Version - Quelle: http://www.delphidabbler.com/articles?article=23&part=1
  14.12.10 wl  TValueConverter         TN5411   GetFormatSettings-Funktionen --> GeneralTypes
  14.12.10 wl  EncodeTo...             TN5411   --> ObjSchedChart
  14.12.10 wl  Indent                  TN5411   --> SchedMaker
  14.12.10 wl  gmCreateEvent           TN5411   --> LockHandle
  12.11.12 wl  TIPInfo.GetLocalIP      TN6008   ermittelt die lokale IP-Adresse
  -------------------------------------------------------------------------------------------------- }

unit UtilLib;


interface


uses
    SysUtils,
    Windows,
    Graphics,
    GeneralTypes;

type
    TValueConverter = class // TryBoth-Funktionen sollten langsam aussterben!
    public
        class function StrToFloatTryBoth(const aStr: string): extended;
        class function StrToFloatDefTryBoth(const aStr: string; aDefault: extended): extended;
    end;

    TOSInfo = record
        class function IsWOW64: Boolean; static;
        class function GetWinMajorVersion: integer; static;
        class function VersionIsVistaOrHigher: boolean; static;
    end;

    TIPInfo = record
        class function GetLocalIP: string; static;
    end;

    // from UtilityWin32:
function gmGetPictureFromIcon(aIcon: TIcon): TPicture;


implementation


uses
    IdStack;

function gmGetPictureFromIcon(aIcon: TIcon): TPicture;
begin
    result := TPicture(aIcon); // Billiger cast: könnte besser gemacht werden
end;

{ TValueConverter }

class function TValueConverter.StrToFloatDefTryBoth(const aStr: string; aDefault: extended): extended;
var
    xFS: TFormatSettings;
begin
    xFS := TFormatUtils.GetSettingsEnglishUS();
    if TryStrToFloat(aStr, result, xFS) then
        EXIT;

    xFS.DecimalSeparator := xFS.ThousandSeparator;
    if TryStrToFloat(aStr, result, xFS) then
        EXIT;

    result := aDefault;
end;

class function TValueConverter.StrToFloatTryBoth(const aStr: string): extended;
var
    xFS: TFormatSettings;
begin
    xFS := TFormatUtils.GetSettingsEnglishUS();
    if TryStrToFloat(aStr, result, xFS) then
        EXIT;

    xFS.DecimalSeparator := xFS.ThousandSeparator;
    result := StrToFloat(aStr); // hier kann es dann zur Exception kommen
end;

{ TOSInfo }

class function TOSInfo.GetWinMajorVersion: integer;
var
    osVerInfo: TOSVersionInfo;
begin
    result := -1;
    osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);

    if GetVersionEx(osVerInfo) then
    begin
        result := osVerInfo.dwMajorVersion;
        // minorVersion := osVerInfo.dwMinorVersion; // brauchen wir im Moment nicht
    end;
end;

class function TOSInfo.VersionIsVistaOrHigher: boolean;
begin
    result := GetWinMajorVersion() >= 6;
end;

class function TOSInfo.IsWOW64: Boolean;
type
    TIsWow64Process = function( // Type of IsWow64Process API fn
        Handle: THandle; var Res: BOOL): BOOL; stdcall;
var
    IsWow64Result: BOOL; // result from IsWow64Process
    IsWow64Process: TIsWow64Process; // IsWow64Process fn reference
begin
    // Try to load required function from kernel32
    IsWow64Process := GetProcAddress(GetModuleHandle('kernel32'), 'IsWow64Process');
    if Assigned(IsWow64Process) then
    begin
        // Function is implemented: call it
        if not IsWow64Process(GetCurrentProcess, IsWow64Result) then
            raise Exception.Create('Bad process handle');
        // Return result of function
        Result := IsWow64Result;
    end
    else
        // Function not implemented: can't be running on Wow64
        Result := False;
end;

{ TIPInfo }

class function TIPInfo.GetLocalIP: string;
begin
    TIdStack.IncUsage;
    try
        EXIT(GStack.LocalAddress);
    finally
        TIdStack.DecUsage;
    end;
end;


end.
