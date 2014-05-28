{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : TSimpleIniAccess: Provides ini access with simple read and write methods
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  10.10.02 wl                               TN1293.1 initial version
  25.10.02 wl                               TN1293.1 überarbeitet: wird für device.ini und balance.ini benutzt
  06.11.02 wl                               TN1293.1 uses AppSettings
  20.12.02 wl  Create, CreateFileAccess     TN1293.5 geänderte constructors, in Abhängigkeit von TIniAccess
  20.12.02 wl  ChangeAccess                 TN1293.5 entfernt
  30.12.02 wl                               TN1293.5 jetzt auch von IIniAccess abgeleitet
  15.01.03 wl  Create                       TN1295.5 IniDataAdapter-Instanz
  15.01.03 wl  AddDialog                    TN1295.5 neu: für Settings Editor
  16.01.03 wl  AddDialog                    TN1295.5 --> TIniAccess
  05.02.03 wl  DeleteAllValues              TN1293.3 neu: löscht alle Werte
  06.02.03 wl  SaveAllValues                TN1334.3 Integration von GetReason
  16.08.05 wl  DeleteAllAndSave             TN2558.4  --> IniAccess
  16.08.05 wl  DeleteAll                    TN2558.4  enthält nur die Lösch-Schleife
  31.08.07 wl  Items                        TN3811.4 Items = fItems (TIniEntryList)
  13.02.08 wl  ReadMotor,ReadDilutor        TN4009   entfernt
  06.08.08 pk  LoadAllValues,SaveAllValues  TN4165   uses settingsdatacache methods for reading and writing
  06.08.09 wl                               TN4702   Strings werden direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  21.03.13 wl                               TN6045   verwendet Generics.Collections
  -------------------------------------------------------------------------------------------------- }

unit SimpleIniAccess;


interface


uses
    IniAccess,
    Generics.Collections,
    IniEntry,
    CommonTypes,
    GeneralTypes,
    IniDataWrapper,
    WinlissyIniEntry;

type
    TSimpleIniAccess = class(TIniAccess, ISimpleIniAccess)
    protected
        function GetAreaEmpty: boolean; override;
        function GetCount: integer; override;
        function GetItem(aIndex: Integer): TIniEntry; override;
        function DeleteAll(): boolean; override;
    public
        // Constructor/Destructor
        constructor Create(aIniDataWrapper: TIniDataWrapper);
        destructor Destroy; override;

        // Read Methods
        function ReadBool(aSection, aIdent: string; aDefault: Boolean): Boolean;
        function ReadString(aSection, aIdent, aDefault: string): string;
        function ReadInteger(aSection, aIdent: string; aDefault: Longint): Longint;
        function ReadFloat(aSection, aIdent: string; aDefault: double): double;

        // Write Methods
        procedure WriteBool(aSection, aIdent: string; aValue: Boolean);
        procedure WriteString(aSection, aIdent, aValue: string);
        procedure WriteInteger(aSection, aIdent: string; aValue: Longint);
        procedure WriteFloat(aSection, aIdent: string; aValue: double);
        // Load all / save all
        procedure LoadAllValues; override;
        function SaveAllValues(aCurrentUser: IUser): boolean; override;
        // other public methods
        function ReadSection(aSection: string): TStringArray;
        function GetItemByIdent(aSection, aIdent: string; aAddIfPossible: boolean): TIniEntry; override;
        function IsCheckingAccess: boolean; override;
    end;


implementation


uses
    Windows,
    SysUtils;

// --------------------------------------------------------------------------------------------------
// TSimpleIniAccess -> TIniAccess
// --------------------------------------------------------------------------------------------------
constructor TSimpleIniAccess.Create(aIniDataWrapper: TIniDataWrapper);
begin
    inherited Create(aIniDataWrapper, true);
end;

destructor TSimpleIniAccess.Destroy;
begin
    inherited Destroy;
end;

function TSimpleIniAccess.GetCount: integer;
begin
    result := fItems.Count;
end;

function TSimpleIniAccess.GetItem(aIndex: Integer): TIniEntry;
begin
    result := nil;
    if (aIndex < fItems.Count) then
        result := fItems[aIndex]
end;

procedure TSimpleIniAccess.WriteBool(aSection, aIdent: string; aValue: Boolean);
var
    xIniEntry: TBoolEntry;
begin
    try
        xIniEntry := TBoolEntry.CreateWithoutDefault(aSection, aIdent);
        xIniEntry.BoolValue := aValue;
        WriteValue(nil, '', xIniEntry);
        FreeAndNil(xIniEntry);
    except
        on E: Exception do
            ErrorDialog(E.Message);
    end
end;

procedure TSimpleIniAccess.WriteInteger(aSection, aIdent: string; aValue: Longint);
var
    xIniEntry: TIntegerEntry;
begin
    try
        xIniEntry := TIntegerEntry.CreateWithoutDefault(aSection, aIdent);
        xIniEntry.IntegerValue := aValue;
        WriteValue(nil, '', xIniEntry);
        FreeAndNil(xIniEntry);
    except
        on E: Exception do
            ErrorDialog(E.Message);
    end
end;

procedure TSimpleIniAccess.WriteString(aSection, aIdent, aValue: string);
var
    xIniEntry: TStringEntry;
begin
    try
        xIniEntry := TStringEntry.CreateWithoutDefault(aSection, aIdent);
        xIniEntry.Value := aValue; // hier kann's krachen!
        WriteValue(nil, '', xIniEntry); // hier auch!
        FreeAndNil(xIniEntry);
    except
        on E: Exception do
            ErrorDialog(E.Message);
    end
end;

procedure TSimpleIniAccess.WriteFloat(aSection, aIdent: string; aValue: double);
var
    xIniEntry: TFloatEntry;
begin
    try
        xIniEntry := TFloatEntry.CreateWithoutDefault(aSection, aIdent);
        xIniEntry.FloatValue := aValue; // hier kann's krachen!
        WriteValue(nil, '', xIniEntry); // hier auch!
        FreeAndNil(xIniEntry);
    except
        on E: Exception do
            ErrorDialog(E.Message);
    end
end;

function TSimpleIniAccess.ReadBool(aSection, aIdent: string; aDefault: Boolean): Boolean;
var
    xIniEntry: TBoolEntry;
begin
    result := aDefault;
    try
        xIniEntry := TBoolEntry.Create(aSection, aIdent, aDefault, '', '');
        ReadValue(xIniEntry);
        result := xIniEntry.BoolValue;
        FreeAndNil(xIniEntry);
    except
        on E: Exception do
            ErrorDialog(E.Message);
    end
end;

function TSimpleIniAccess.ReadString(aSection, aIdent, aDefault: string): string;
var
    xIniEntry: TStringEntry;
begin
    result := aDefault;
    try
        xIniEntry := TStringEntry.Create(aSection, aIdent, aDefault, '', '');
        ReadValue(xIniEntry);
        result := xIniEntry.Value;
        FreeAndNil(xIniEntry);
    except
        on E: Exception do
            ErrorDialog(E.Message);
    end
end;

function TSimpleIniAccess.ReadInteger(aSection, aIdent: string; aDefault: Longint): Longint;
var
    xIniEntry: TIntegerEntry;
begin
    result := aDefault;
    try
        xIniEntry := TIntegerEntry.Create(aSection, aIdent, aDefault, '', '');
        ReadValue(xIniEntry);
        result := xIniEntry.IntegerValue;
        FreeAndNil(xIniEntry);
    except
        on E: Exception do
            ErrorDialog(E.Message);
    end
end;

function TSimpleIniAccess.ReadFloat(aSection, aIdent: string; aDefault: double): double;
var
    xIniEntry: TFloatEntry;
begin
    result := aDefault;
    try
        xIniEntry := TFloatEntry.Create(aSection, aIdent, aDefault, '', '');
        ReadValue(xIniEntry);
        result := xIniEntry.FloatValue;
        FreeAndNil(xIniEntry);
    except
        on E: Exception do
            ErrorDialog(E.Message);
    end
end;

procedure TSimpleIniAccess.LoadAllValues;
var
    xSections, xIdents: TStringArray;
    xS, xI: integer;
    xIniEntry: TIniEntry;
begin

    ReadIntoCache();

    fItems.Clear;
    try
        xSections := FIniDataWrapper.ReadSections();

        for xS := 0 to high(xSections) do
        begin
            xIdents := FIniDataWrapper.ReadSection(xSections[xS]);

            for xI := 0 to high(xIdents) do
            begin
                xIniEntry := TStringEntry.Create(xSections[xS], xIdents[xI], '', '', '');
                ReadValue(xIniEntry);
                fItems.Add(xIniEntry);
            end;
        end;
    except
        on E: Exception do
            ErrorDialog(E.Message);
    end;

end;

function TSimpleIniAccess.SaveAllValues(aCurrentUser: IUser): boolean;
var
    x: integer;
    xReason: string;
begin
    result := GetReason(aCurrentUser, xReason);
    if not(result) then
        EXIT;
    try
        // Write all single values
        for x := 0 to fItems.Count - 1 do
            WriteValue(aCurrentUser, xReason, fItems[x]);
    except
        on E: Exception do
            ErrorDialog(E.Message);
    end;

    WriteFromCache();
end;

function TSimpleIniAccess.ReadSection(aSection: string): TStringArray;
begin
    result := FIniDataWrapper.ReadSection(aSection);
end;

function TSimpleIniAccess.GetItemByIdent(aSection, aIdent: string; aAddIfPossible: boolean): TIniEntry;
var
    x: integer;
    xIniEntry: TStringEntry;
begin
    // get item from FAllowed
    for x := 0 to fItems.Count - 1 do
    begin
        if (fItems[x].Ident = aIdent) and (fItems[x].Section = aSection) then
            EXIT(fItems[x]);
    end;

    if (aAddIfPossible) then
    begin
        xIniEntry := TStringEntry.Create(aSection, aIdent, '', '', '');
        xIniEntry.ValueExists := true;
        fItems.Add(xIniEntry);
        EXIT(xIniEntry);
    end;

    EXIT(nil);
end;

function TSimpleIniAccess.GetAreaEmpty: boolean;
var
    x: integer;
begin
    result := true;
    for x := 0 to fItems.Count - 1 do
        if fItems[x].ValueExists then
            result := false;
end;

function TSimpleIniAccess.IsCheckingAccess: boolean;
begin
    result := false;
end;

function TSimpleIniAccess.DeleteAll(): boolean;
var
    x: integer;
begin
    result := false;

    for x := 0 to fItems.Count - 1 do
    begin
        if (not fItems[x].ValueExists) then
            CONTINUE;

        fItems[x].ValueExists := false;
        result := true;
    end;
end;


end.
