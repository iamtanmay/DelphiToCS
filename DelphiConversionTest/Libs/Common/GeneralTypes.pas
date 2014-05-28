{ --------------------------------------------------------------------------------------------------
  BASEUNIT!
  ---------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Very General Types that may be used by any dll, or application
  Do NOT put any Non-native Delphi units in the uses clause
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------   -------- -----------------------------------------------
  19.01.05 pk                                 TN2281.0 Initial Revision
  19.01.05 pk                                 TN2281.0  TStringArray, TIntArray from AppTypes to GeneralTypes
  15.02.05 pk  TStringPairRec                 TN2315    New
  28.02.05 pk  TExecutable                    TN2314.1  New : Implements SetThread
  11.03.05 pk  IExceptionHandler              TN2339.2  New
  23.06.05 pk  TDblArray                      TN2471.1  New
  05.02.07 pk  StringArrayOf                  TN3544    New
  18.02.08 wl  TDoubleArray                   TN4009    entfernt
  17.04.08 wl  TDoubleArray,TBoolArray        TN4060    neu
  06.05.08 wl  TTypeSafeFormat.Format                   new
  03.07.08 wl                                 TN4157
  19.11.08 wl  TTypeSafeFormat.Format         TN4312    neu für Delphi 2009: UnicodeString
  16.02.09 wl  TLanguageString                TN4370    einfache Klasse zum Laden von Sprach-Strings
  20.02.09 wl  TLanguageString.Read           TN4370    Klassenfunktion zum Laden eines Strings
  25.02.09 pk  TLanguageString.GetText        TN4370    result was missing
  04.03.09 pk  TKeyValueParamArray            TN4232    from AppTypes
  25.08.09 wl  TGarbage.FreeAndNil            TN4740    soll alle Free/FreeAndNil-Aufrufe ersetzen
  12.10.09 pk  Format                         TN4800    changes for Delphi 2010
  14.11.09 wl  TGeneralUtils.GetCurrentListSeparator  TN4869    Kapselung für SysUtils.ListSeparator
  17.08.10 wl  cFirstStringIndex                      TN5112   eventuell nützlich für String-Funktionen
  29.11.10 wl  TGeneralUtils.GetCurrentListSeparator  TN5370   FormatSettings.ListSeparator statt SysUtils..
  20.12.10 wl  TFormatUtils                           TN5411   GetFormatSettings-Funktionen von UtilsLib hierher
  01.03.11 wl  TStringArray,..                        TN5491   entspricht jetzt TArray<>
  14.03.12 wl  TStringPairRec                         TN5831   entfernt
  26.03.12 wl                                         TN5844   WARN SYMBOL_PLATFORM OFF
  18.04.12 wl  TKeyValueItem                          TN5870   von ListClasses hierher
  10.04.13 wl  TKeyValueListDuplicates                TN6045   von ListClasses hierher
  11.04.13 wl  TKeyValueParam                         TN6045   --> MethodTypes
  24.04.13 wl  TCustom..Comparer                      TN6137   neu: Allgemeine Comparer für string und integer
  -------------------------------------------------------------------------------------------------- }

unit GeneralTypes;


interface


uses
    Generics.Defaults,
    SysUtils;

const
    cFirstStringIndex = 1; // in C#: 0

type
    // General arrays
    TStringArray = TArray<string>;
    TDoubleArray = TArray<double>;
    TIntArray = TArray<integer>;

    TKeyValueItem<TKey, TValue> = class
    private
        fKey: TKey;
        fValue: TValue;
    public
        constructor Create(const aKey: TKey; const aValue: TValue); overload;
        constructor Create(const aKey: TKey); overload;
        property Key: TKey read fKey write fKey;
        property Value: TValue read fValue write fValue;
    end;

    TLanguageString = class
    private
        fText_EN: string;
        fText_DE: string;
        function GetText(): string;
    public
        constructor Create(const aText_EN, aText_DE: string);

        class function read(const aText_EN, aText_DE: string): string; overload;
        class function read(const aText_EN, aText_DE: string; aArgs: array of const ): string; overload;
        class function GetLanguage(): integer;
        class procedure SetLanguage(aValue: integer);

        property Text: string read GetText;
    end;

    TTypeSafeFormat = class // In C# kann TTypeSefeFormat einfach durch String ersetzt werden
    public
        class function Format(aFormat: string; aArgs: array of const ): string;
    end;

    TGarbage = class
    public
        // Kapselung der SamUtil-Funktion FreeAndNil
        class procedure FreeAndNil(aDisposeInDotNet: boolean; var vObj);
    end;

    TFormatUtils = record
    strict private
        class function GetCurrentListSeparator(): string; static;
    public
        class function GetSettingsEnglishUS: TFormatSettings; static;
        class function GetSettingsGermanDE: TFormatSettings; static;
        class function GetLocaleSettings: TFormatSettings; static;
        class property CurrentListSeparator: string read GetCurrentListSeparator;
    end;

    TKeyValueListDuplicates = (dupIgnore, dupAccept, dupError);

    // String-Comparer (CaseSensitive)
    TCustomStringComparer = class(TComparer<string>)
    public
        function Compare(const Left, Right: string): Integer; override;
    end;

    // Text-Comparer (nicht CaseSensitive)
    TCustomTextComparer = class(TComparer<string>)
    public
        function Compare(const Left, Right: string): Integer; override;
    end;

    // Standard-Integer-Comparer
    TCustomIntegerComparer = class(TComparer<Integer>)
    public
        function Compare(const Left, Right: Integer): Integer; override;
    end;


implementation


var
    uLanguage: integer = 0; // alle benutzen die gleiche statische Variable!

    { TLanguageString }

constructor TLanguageString.Create(const aText_EN, aText_DE: string);
begin
    inherited Create();

    fText_EN := aText_EN;
    fText_DE := aText_DE;
end;

class function TLanguageString.GetLanguage: integer;
begin
    result := uLanguage;
end;

function TLanguageString.GetText: string;
begin
    result := read(fText_EN, fText_DE);
end;

class function TLanguageString.Read(const aText_EN, aText_DE: string): string;
begin
    if (uLanguage = 1) then
        result := aText_DE
    else
        result := aText_EN;
end;

class function TLanguageString.Read(const aText_EN, aText_DE: string; aArgs: array of const ): string;
begin
    result := TTypeSafeFormat.Format(read(aText_EN, aText_DE), aArgs);
end;

class procedure TLanguageString.SetLanguage(aValue: integer);
begin
    uLanguage := aValue;
end;

{ TKeyValueItem<TKey,TValue> }

constructor TKeyValueItem<TKey, TValue>.Create(const aKey: TKey);
begin
    inherited Create();
    fKey := aKey;
    fValue := default (TValue);
end;

constructor TKeyValueItem<TKey, TValue>.Create(const aKey: TKey; const aValue: TValue);
begin
    Create(aKey);
    fValue := aValue;
end;

{ TTypeSafeFormat }

class function TTypeSafeFormat.Format(aFormat: string; aArgs: array of const ): string;
const
    cArgStart: char = '{';
    cArgEnd: char = '}';
var
    x: integer;
    xArg, xPattern: string;
begin
    result := aFormat;
    for x := 0 to high(aArgs) do
    begin

        case (aArgs[x].VType) of

            // zunächst mal nur die Standardkonvertierungen für String, Integer und Float-Werte
            // später könnte man auch noch auswerten:
            // {0:F3} - Wieviele Stellen hinter dem Komma
            // {0:D4} - Integer mit führenden Nullen

            vtInteger, vtInt64:
                xArg := IntToStr(aArgs[x].VInteger);
            vtUnicodeString:
                xArg := aArgs[x].VPWideChar;

            vtExtended:
                begin
                    xArg := FloatToStr(aArgs[x].VExtended^);
                end;

            else
                xArg := '|||||||||||||||||||||||||||||||||||||||';
        end;

        xPattern := cArgStart + IntToStr(x) + cArgEnd;
        result := StringReplace(result, xPattern, xArg, [rfReplaceAll]);
    end;
end;

{ TGarbage }

class procedure TGarbage.FreeAndNil(aDisposeInDotNet: boolean; var vObj);
begin
    SysUtils.FreeAndNil(vObj);
end;

{ TFormatUtils }

class function TFormatUtils.GetCurrentListSeparator: string;
begin
    result := FormatSettings.ListSeparator;
end;

class function TFormatUtils.GetSettingsEnglishUS: TFormatSettings;
begin
    result := TFormatSettings.Create('en-US');
end;

class function TFormatUtils.GetSettingsGermanDE: TFormatSettings;
begin
    result := TFormatSettings.Create('de-DE');
end;

class function TFormatUtils.GetLocaleSettings: TFormatSettings;
begin
{$WARN SYMBOL_PLATFORM OFF}
    result := TFormatSettings.Create(SysLocale.DefaultLCID);
{$WARN SYMBOL_PLATFORM ON}
end;

{ TCustomStringComparer }

function TCustomStringComparer.Compare(const Left, Right: string): Integer;
begin
    Result := CompareStr(Left, Right)
end;

{ TCustomTextComparer }

function TCustomTextComparer.Compare(const Left, Right: string): Integer;
begin
    Result := CompareText(Left, Right);
end;

{ TCustomIntegerComparer }

function TCustomIntegerComparer.Compare(const Left, Right: Integer): Integer;
begin
    if Left < Right then
        result := -1
    else if Left > Right then
        result := 1
    else
        result := 0;
end;


end.
