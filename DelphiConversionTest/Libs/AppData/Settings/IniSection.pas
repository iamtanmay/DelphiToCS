{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : An object derived from TIniEntryList that represents a ini file section
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  15.01.03 wl                               TN1293.1 separated from IniEntryList.pas
  16.01.03 wl  IdentExists                  TN1293.3 ValueExists muﬂ gesetzt sein, sonst false
  11.02.03 wl  TIniSection.Create           TN1293.3 L‰nge von Section & DefaultIdent wird begrenzt
  12.03.03 wl                               TN1293.5 uses ge‰ndert
  31.08.07 wl                               TN3811.4 uses ge‰ndert
  06.08.09 wl                               TN4702   Strings werden direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  11.11.09 pk  PrepareIdentList	           TN4843   result was not set correctly
  12.11.09 pk  PrepareIdentList             TN4843   count downwards
  27.03.13 wl                               TN6045   verwendet Generics.Collections
  -------------------------------------------------------------------------------------------------- }

unit IniSection;


interface


uses
    GeneralTypes,
    IniEntry,
    Generics.Collections;

type
    TIniSection = class abstract
    private
        function GetCount: integer;
        function GetIniEntry(index: integer): TIniEntry;
    protected
        fList: TObjectList<TIniEntry>;
        FSection: string;
        FDefaultIdent: string;
        FDescription: string;
        function GetDefaultValue: string; virtual; abstract;
        function GetCompleteIdent: string;
        function FitToDefaultIdent(aIdent: string): boolean;
        function GetSection: string;
        function GetDefaultIdent: string;
    public
        // Constructor/Destructor
        constructor Create(const aSection, aDefaultIdent, aDescription: string; aOwnsObjects: boolean);
        destructor Destroy; override;
        // public methods
        function PrepareIdentList(const aStrings: TStringArray): TStringArray;
        function Add(aIdent: string): TIniEntry; virtual; abstract;
        function IdentExists(aIdent: string): boolean;
        function IsAllowed(aSection, aIdent: string): boolean;
        function GetNextPossibleIdent: string;
        procedure Clear;
        // properties
        property Section: string read GetSection;
        property DefaultIdent: string read GetDefaultIdent;
        property CompleteIdent: string read GetCompleteIdent; // Section & Ident as one string
        property DefaultValue: string read GetDefaultValue;
        property Count: integer read GetCount;
        property this[index: integer]: TIniEntry read GetIniEntry; default;
    end;

    TTypeIniSection = class(TIniSection)
    private
        FPrototypeEntry: TIniEntry;
    protected
        function GetDefaultValue: string; override;
        function GetIndexString(aIndex: integer): string; virtual;
        function CreatePrototypeEntry(aIdent: string): TIniEntry; virtual; abstract;
    public
        // Constructor/Destructor
        constructor Create(const aSection, aDefaultIdent, aDescription: string);
        destructor Destroy; override;
        function Add(aIdent: string): TIniEntry; override;
    end;

    TStringIniSection = class(TTypeIniSection)
    public
        function CreatePrototypeEntry(aIdent: string): TIniEntry; override;
    end;

    TIntegerIniSection = class(TTypeIniSection)
    public
        function CreatePrototypeEntry(aIdent: string): TIniEntry; override;
    end;


implementation


uses
    SysUtils,
    Utility2,
    IniDataWrapper;

{ TIniSection }

procedure TIniSection.Clear;
begin
    fList.Clear;
end;

constructor TIniSection.Create(const aSection, aDefaultIdent, aDescription: string; aOwnsObjects: boolean);
begin
    inherited Create;
    fList := TObjectList<TIniEntry>.Create(aOwnsObjects);
    FSection := Copy(aSection, 1, TIniDataWrapper.GetFieldLength_Section);
    FDefaultIdent := Copy(aDefaultIdent, 1, TIniDataWrapper.GetFieldLength_Ident);
    FDescription := aDescription;
end;

destructor TIniSection.Destroy;
begin
    FreeAndNil(fList);
    inherited;
end;

function TIniSection.GetSection: string;
begin
    result := FSection;
end;

function TIniSection.GetDefaultIdent: string;
begin
    result := FDefaultIdent;
end;

function TIniSection.GetIniEntry(index: integer): TIniEntry;
begin
    EXIT(fList[index]);
end;

function TIniSection.PrepareIdentList(const aStrings: TStringArray): TStringArray;
var
    x: integer;
    xList: TList<string>;
begin
    // alle entfernen, die nicht einen bestimmeten Anfang (DefaultIdent) haben
    if (FDefaultIdent = '') then
    begin
        result := aStrings;
        EXIT;
    end;

    xList := TList<string>.Create();
    try
        xList.AddRange(aStrings);
        xList.Sort();
        for x := xList.Count - 1 downto 0 do
        begin
            if not FitToDefaultIdent(aStrings[x]) then
                xList.Delete(x);
        end;
        result := xList.ToArray();
    finally
        FreeAndNil(xList);
    end;
end;

function TIniSection.FitToDefaultIdent(aIdent: string): boolean;
var
    xIntPos, xValue, xValueLength: integer;
    xStrPart, xDefPart: string;
begin
    result := false;

    // 1. FDefaultIdent ist nicht definiert
    if (FDefaultIdent = '') then
    begin
        result := true;
        exit;
    end;

    // 2. DefaultIdent ist der Beginn von Ident
    if (Pos(FDefaultIdent, aIdent) = 1) then
    begin
        result := true;
        exit;
    end;

    // 3. Der Bezeichner %d muss durch eine Zahl ersetzt werden
    xIntPos := Pos('%d', FDefaultIdent);
    if (xIntPos > 0) then
    begin

        xDefPart := Copy(FDefaultIdent, 1, xIntPos - 1);
        xStrPart := Copy(aIdent, 1, xIntPos - 1);
        if (xStrPart <> xDefPart) then
            exit;

        xValueLength := 2 + Length(aIdent) - Length(FDefaultIdent);
        xStrPart := Copy(aIdent, xIntPos, xValueLength);
        if not gmStrToIntTry(xValue, xStrPart) then
            exit;

        xDefPart := Copy(FDefaultIdent, xIntPos + 2, Length(FDefaultIdent) - xIntPos - 1);
        if (xDefPart <> '') then
        begin
            xStrPart := Copy(aIdent, xIntPos + xValueLength, Length(aIdent) - xIntPos - xValueLength + 1);
            if (xStrPart <> xDefPart) then
                exit;
        end;

        result := true;
    end;
end;

function TIniSection.GetCompleteIdent: string;
begin
    result := '[' + FSection + '] ' + FDefaultIdent; // differs from TIniEntry.CompleteIdent
end;

function TIniSection.GetCount: integer;
begin
    EXIT(fList.Count);
end;

function TIniSection.IdentExists(aIdent: string): boolean;
var
    x: integer;
begin
    result := false;
    for x := 0 to fList.Count - 1 do
        if (fList[x].Ident = aIdent) // Ident fits
            and (fList[x].ValueExists) then // value must exist !!!!
            result := true;
end;

function TIniSection.IsAllowed(aSection, aIdent: string): boolean;
begin
    result := false;
    if (FSection = aSection) and FitToDefaultIdent(aIdent) then
        result := true;
end;

function TIniSection.GetNextPossibleIdent: string;
var
    xIntPos, x: integer;
begin
    result := '';
    x := 1;
    xIntPos := Pos('%d', FDefaultIdent);

    repeat
        if (xIntPos > 0) then
            result := Format(FDefaultIdent, [x])
        else
            result := Format(FDefaultIdent + '%d', [x]);

        inc(x);
    until (not IdentExists(result));
end;

{ TTypeIniSection }

constructor TTypeIniSection.Create(const aSection, aDefaultIdent, aDescription: string);
begin
    inherited Create(aSection, aDefaultIdent, aDescription, true);
    FPrototypeEntry := CreatePrototypeEntry(FDefaultIdent);
end;

destructor TTypeIniSection.Destroy;
begin
    FreeAndNil(FPrototypeEntry);
    inherited Destroy;
end;

function TTypeIniSection.GetIndexString(aIndex: integer): string;
begin
    result := IntToStr(aIndex);
end;

function TTypeIniSection.Add(aIdent: string): TIniEntry;
var
    xIndex: integer;
begin
    xIndex := fList.Add(CreatePrototypeEntry(aIdent));
    result := fList[xIndex];
end;

function TTypeIniSection.GetDefaultValue: string;
begin
    result := FPrototypeEntry.DefaultValue;
end;

{ TStringIniSection }

function TStringIniSection.CreatePrototypeEntry(aIdent: string): TIniEntry;
begin
    result := TStringEntry.Create(FSection, aIdent, '', '', FDescription);
end;

{ TIntegerIniSection }

function TIntegerIniSection.CreatePrototypeEntry(aIdent: string): TIniEntry;
begin
    result := TIntegerEntry.Create(FSection, aIdent, 0, '', FDescription);
end;


end.
