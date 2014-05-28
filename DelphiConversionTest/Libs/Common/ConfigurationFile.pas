{ ----------------------------------------------------------------------------------------------------------------------
  BASEUNIT!
  ---------
  Copyright  2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : TConfigurationFile is a replcement for TIniFile
  TConfigurationFile has a Open and Close method
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  26.10.09 wl                               TN4831   initial revision
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  05.01.11 wl  ReadFloat, WriteFloat        TN5411   TFormatSettings sind jetzt erforderlich
  19.01.11 wl  ReadFloat                    TN5435   benutzt im Zweifel den Default-Wert
  01.03.11 wl  ReadSections, ReadSection    TN5491   benutzt TArray<>
  ---------------------------------------------------------------------------------------------------------------------- }

unit ConfigurationFile;


interface


uses
    Classes,
    SysUtils,
    Types,
    IniFiles;

type
    IConfigurationSet = interface
        ['{19E19F42-BF1B-4769-9540-137722E7FCDE}']
        procedure Open(aReadOnly: boolean);
        procedure Close();
        function ReadString(const Section, Ident, default: string): string;
        procedure WriteString(const Section, Ident, Value: string);
        function ReadInteger(const Section, Ident: string; default: Longint): Longint;
        procedure WriteInteger(const Section, Ident: string; Value: Longint);
        function ReadBool(const Section, Ident: string; default: Boolean): Boolean;
        procedure WriteBool(const Section, Ident: string; Value: Boolean);
        function ReadFloat(const Section, Ident: string; default: Double;
            const aFormatSettings: TFormatSettings): Double;
        procedure WriteFloat(const Section, Ident: string; Value: Double;
            const aFormatSettings: TFormatSettings);
        function ValueExists(const Section, Ident: string): Boolean;
        function ReadSection(const Section: string): TArray<string>;
        procedure DeleteKey(const aSection, aIdent: string);
        function ReadSections(): TArray<string>;
        function SectionExists(const Section: string): Boolean;
        procedure EraseSection(const Section: string);
        function ReadSectionValues(const Section: string): TArray<string>;
    end;

    TConfigurationFile = class(TInterfacedObject, IConfigurationSet)
    private
        fFileName: string;
        fIniFile: TIniFile;
        procedure CreateFileIfNeeded(const aFileName: string);
        function GetFileName: string;
        function StringsToStringArray(const aStrings: TStrings): TArray<string>;
    public
        constructor Create(const aFileName: string);
        destructor Destroy; override;
        procedure Open(aReadOnly: boolean);
        procedure Close();
        function ReadString(const Section, Ident, default: string): string;
        procedure WriteString(const Section, Ident, Value: string);
        function ReadInteger(const Section, Ident: string; default: Longint): Longint;
        procedure WriteInteger(const Section, Ident: string; Value: Longint);
        function ReadBool(const Section, Ident: string; default: Boolean): Boolean;
        procedure WriteBool(const Section, Ident: string; Value: Boolean);
        function ReadFloat(const Section, Ident: string; default: Double;
            const aFormatSettings: TFormatSettings): Double;
        procedure WriteFloat(const Section, Ident: string; Value: Double;
            const aFormatSettings: TFormatSettings);
        function ValueExists(const Section, Ident: string): Boolean;
        function ReadSection(const Section: string): TArray<string>;
        procedure DeleteKey(const aSection, aIdent: string);
        function ReadSections(): TArray<string>;
        function SectionExists(const Section: string): Boolean;
        procedure EraseSection(const Section: string);
        function ReadSectionValues(const Section: string): TArray<string>;
        property FileName: string read GetFileName;
    end;


implementation


uses
    Windows;

{ TConfigurationFile }

procedure TConfigurationFile.Close;
begin
    FreeAndNil(fIniFile);
end;

constructor TConfigurationFile.Create(const aFileName: string);
begin
    inherited Create;
    fFileName := aFileName;
end;

procedure TConfigurationFile.DeleteKey(const aSection, aIdent: string);
begin
    if FIniFile.ValueExists(aSection, aIdent) then // TBD_WL kracht, wenn der erste Wert nicht vorhanden ist!
        FIniFile.DeleteKey(aSection, aIdent);
end;

destructor TConfigurationFile.Destroy;
begin
    if (fIniFile <> nil) then
        FreeAndNil(fIniFile);

    inherited;
end;

// These are delphi-specific functions
function TConfigurationFile.StringsToStringArray(const aStrings: TStrings): TArray<string>;
var
    x: integer;
begin
    SetLength(result, aStrings.Count);
    for x := 0 to aStrings.Count - 1 do
        result[x] := aStrings[x];
end;

procedure TConfigurationFile.EraseSection(const Section: string);
begin
    fIniFile.EraseSection(Section);
end;

function TConfigurationFile.GetFileName: string;
begin
    result := '';
    if (fIniFile <> nil) then
        result := fIniFile.FileName;
end;

procedure TConfigurationFile.CreateFileIfNeeded(const aFileName: string);
var
    xFileHandle: integer;
begin
    if not FileExists(aFileName) then
    begin
        xFileHandle := FileCreate(aFileName);
        FileClose(xFileHandle);
    end;
end;

procedure TConfigurationFile.Open(aReadOnly: boolean);
begin
    if (not aReadOnly) then
        self.CreateFileIfNeeded(fFileName);

    if (fIniFile = nil) then
        fIniFile := TIniFile.Create(fFileName);
end;

function TConfigurationFile.ReadBool(const Section, Ident: string; default: Boolean): Boolean;
begin
    result := fIniFile.ReadBool(Section, Ident, default);
end;

function TConfigurationFile.ReadFloat(const Section, Ident: string; default: Double;
    const aFormatSettings: TFormatSettings): Double;
var
    xStrValue: string;
begin
    xStrValue := fIniFile.ReadString(Section, Ident, '');
    result := StrToFloatDef(xStrValue, default, aFormatSettings);
end;

function TConfigurationFile.ReadInteger(const Section, Ident: string; default: Integer): Longint;
begin
    result := fIniFile.ReadInteger(Section, Ident, default);
end;

function TConfigurationFile.ReadSection(const Section: string): TArray<string>;
var
    xStrings: TStringList;
begin
    xStrings := TStringList.Create();
    try
        fIniFile.ReadSection(Section, xStrings);
        result := self.StringsToStringArray(xStrings);
    finally
        FreeAndNil(xStrings);
    end;
end;

function TConfigurationFile.ReadSections(): TArray<string>;
var
    xStrings: TStringList;
begin
    xStrings := TStringList.Create();
    try
        fIniFile.ReadSections(xStrings);
        result := self.StringsToStringArray(xStrings);
    finally
        FreeAndNil(xStrings);
    end;

end;

function TConfigurationFile.ReadSectionValues(const Section: string): TArray<string>;
var
    xStrings: TStringList;
begin
    xStrings := TStringList.Create();
    try
        fIniFile.ReadSectionValues(Section, xStrings);
        result := self.StringsToStringArray(xStrings);
    finally
        FreeAndNil(xStrings);
    end;
end;

function TConfigurationFile.ReadString(const Section, Ident, default: string): string;
begin
    result := fIniFile.ReadString(Section, Ident, default);
end;

function TConfigurationFile.SectionExists(const Section: string): Boolean;
begin
    result := fIniFile.SectionExists(Section);
end;

function TConfigurationFile.ValueExists(const Section, Ident: string): Boolean;
begin
    result := fIniFile.ValueExists(Section, Ident);
end;

procedure TConfigurationFile.WriteBool(const Section, Ident: string; Value: Boolean);
begin
    fIniFile.WriteBool(Section, Ident, Value);
end;

procedure TConfigurationFile.WriteFloat(const Section, Ident: string; Value: Double;
    const aFormatSettings: TFormatSettings);
var
    xStrValue: string;
begin
    xStrValue := FloatToStr(Value, aFormatSettings);
    fIniFile.WriteString(Section, Ident, xStrValue);
end;

procedure TConfigurationFile.WriteInteger(const Section, Ident: string; Value: Integer);
begin
    fIniFile.WriteInteger(Section, Ident, Value);
end;

procedure TConfigurationFile.WriteString(const Section, Ident, Value: string);
begin
    fIniFile.WriteString(Section, Ident, Value);
end;


end.
