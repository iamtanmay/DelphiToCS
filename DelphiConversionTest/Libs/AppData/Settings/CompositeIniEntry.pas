{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : An Ini Entry that consists of different sub entries
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.09.02 wl                               TN1293.1 initial version
  09.10.02 wl  SetValue                     TN1293.2 Last string (InitStr) is read with delimiters
  11.10.02 wl                               TN1293.2 Neu: optional values (noch nicht fertig TBD_WL)
  25.10.02 wl                               TN1293.1 Strings für IniAccess komplett
  20.12.02 wl                               TN1293.5 von TIniEntryImpl abgeleitet, uses geändert
  14.01.03 wl  TIniEntry                    TN1293.5 Namen geändert TIniTypeImpl -> TIniType, TIniType -> IIniType
  16.01.03 wl  Get/SetValueExists           TN1295.3 wird aus SubEntries gelesen
  16.01.03 wl  GetDescription               TN1295.3 wird jetzt aus allen SubEntries gelesen
  16.01.03 wl  Add...                       TN1295.3 Ressource-Nr wird an Subentries übergeben
  12.03.03 wl                               TN1293.5 uses geändert
  19.03.03 wl  SetValue                     TN1293.5 benutzt neue Funktion gmReadSubString
  25.04.03 wl  SetValue                     TN1470   Erstes Zeichen wurde nicht gelesen bei FReadLastStringWithDelimiters
  28.01.05 wl  AddBool,AddBoolWithoutDefault TN2297.4  für TTipType.DryAfterInit
  28.02.05 wl  SetValue                     TN2326   ein Eintrag mit zu vielen Subentries gibt keine Exception mehr
  03.09.07 wl  SubEntries                   TN3811.4  = fSubEntries
  06.08.09 wl                               TN4702   Strings werden direkt geladen
  09.02.10 pk                               TN4973   UnitName changed to ValueUnitName to avoid overwriting TObject.UnitName
  13.04.10 wl                               TN5044      uses StringUtilities
  21.03.13 wl                               TN6045   verwendet Generics.Collections
  -------------------------------------------------------------------------------------------------- }

unit CompositeIniEntry;


interface


uses
    IniEntry,
    Generics.Collections;

type
    TCompositeIniEntry = class(TIniEntry)
    protected
        FSubEntries: TObjectList<TIniEntry>;
        FDelimiter: string;
        FReadLastStringWithDelimiters: boolean;
        FOptionalValues: boolean;
        FStartOfOptionalValues: integer;
    protected
        procedure SetValue(aValue: string); override;
        function GetDescription: string; override;
        function GetDefaultValue: string; override;
        function GetValue: string; override;
        function GetValueUnitName: string; override;
        function GetValueExists: boolean; override;
        procedure SetValueExists(aValueExists: boolean); override;
        // methods to add subentries
        procedure AddIntegerWithoutDefault;
        procedure AddStringWithoutDefault;
        procedure AddFloatWithoutDefault;
        procedure AddBoolWithoutDefault;
        procedure StartOptionalValues;
        procedure AddInteger(aDefault: Longint; aUnitName, aDescription: string); overload;
        procedure AddInteger(aDefault: Longint; aUnitName, aDescription: string;
            aMinValue, aMaxValue: Longint); overload;
        procedure AddFloat(aDefault: double; aUnitName, aDescription: string); overload;
        procedure AddFloat(aDefault: double; aUnitName, aDescription: string;
            aMinValue, aMaxValue: double); overload;
        procedure AddString(aDefault, aUnitName, aDescription: string);
        procedure AddBool(aDefault: boolean; aDescription: string);
    public
        // Constructor/Destructor
        constructor Create(aSection, aIdent, aDelimiter, aDescription: string);
        constructor CreateWithoutDefault(aSection, aIdent, aDelimiter: string);
        destructor Destroy; override;
        // Properties
        property SubEntries: TObjectList<TIniEntry>read FSubEntries;
    end;


implementation


uses
    StringUtilities,
    Utility2;

// --------------------------------------------------------------------------------------------------
// TCompositeEntry -> TIniEntry
// --------------------------------------------------------------------------------------------------
constructor TCompositeIniEntry.Create(aSection, aIdent, aDelimiter, aDescription: string);
begin
    inherited Create(aSection, aIdent, aDescription);

    FDelimiter := aDelimiter;
    FSubEntries := TObjectList<TIniEntry>.Create;
    FSubEntries.OwnsObjects := true;
    FReadLastStringWithDelimiters := false;
    FOptionalValues := false;
end;

constructor TCompositeIniEntry.CreateWithoutDefault(aSection, aIdent, aDelimiter: string);
begin
    Create(aSection, aIdent, aDelimiter, '');
end;

destructor TCompositeIniEntry.Destroy;
begin
    FSubEntries.Free;
    inherited Destroy;
end;

procedure TCompositeIniEntry.SetValue(aValue: string);
var
    x, xCurrentChar: integer;
    xSubValue: string;
    xValueLength: integer;
begin
    CheckValueLength(aValue);
    xValueLength := Length(aValue);

    // set the values of all subentries
    xCurrentChar := 1;
    for x := 0 to FSubEntries.Count - 1 do
    begin
        if (FReadLastStringWithDelimiters) and (x = FSubEntries.Count - 1) then
            xSubValue := TStringUtilities.GetFromString(aValue, xCurrentChar, xValueLength)
            // Delphi 6: RightStr()
        else
            xSubValue := gmReadSubString(xCurrentChar, aValue, FDelimiter);
        FSubEntries[x].Value := xSubValue;
    end;

    // check if the value has been read completely
    // if (not FReadLastStringWithDelimiters) and (xCurrentChar <= xValueLength) then
    // raise EInvalidIniValue.Create(TResLoader.GetResFString(15870{Identifier [%s] %s: Too much subentries!}, [GetSection,GetIdent]));

end;

function TCompositeIniEntry.GetValueExists: boolean;
var
    x: integer;
begin
    result := true;
    for x := 0 to FSubEntries.Count - 1 do
    begin
        if (FSubEntries[x].ValueExists = false) then
            result := false;
    end;
end;

procedure TCompositeIniEntry.SetValueExists(aValueExists: boolean);
var
    x: integer;
begin
    for x := 0 to FSubEntries.Count - 1 do
    begin
        FSubEntries[x].ValueExists := aValueExists;
    end;
end;

function TCompositeIniEntry.GetDescription: string;
var
    x: integer;
begin
    result := inherited GetDescription;

    result := result + ' - ';

    for x := 0 to FSubEntries.Count - 1 do
    begin
        if (FSubEntries[x].Description <> '') then
            result := result + FSubEntries[x].Description + ', ';
    end;

    Delete(result, length(result) - 1, 2);
end;

function TCompositeIniEntry.GetDefaultValue: string;
var
    x: integer;
begin
    result := '';
    for x := 0 to FSubEntries.Count - 1 do
    begin
        if (x <> 0) then
            result := result + FDelimiter;
        result := result + FSubEntries[x].DefaultValue;
    end;
end;

function TCompositeIniEntry.GetValue: string;
var
    x: integer;
begin
    result := '';
    for x := 0 to FSubEntries.Count - 1 do
    begin
        if (x <> 0) then
            result := result + FDelimiter;
        result := result + FSubEntries[x].Value;
    end;
end;

function TCompositeIniEntry.GetValueUnitName: string;
var
    x: integer;
begin
    result := '';
    for x := 0 to FSubEntries.Count - 1 do
    begin
        if (x <> 0) then
            result := result + FDelimiter;
        result := result + FSubEntries[x].ValueUnitName;
    end;
end;

procedure TCompositeIniEntry.AddIntegerWithoutDefault;
begin
    FSubEntries.Add(TIntegerEntry.CreateWithoutDefault(Section, Ident));
end;

procedure TCompositeIniEntry.AddFloatWithoutDefault;
begin
    FSubEntries.Add(TFloatEntry.CreateWithoutDefault(Section, Ident));
end;

procedure TCompositeIniEntry.AddStringWithoutDefault;
begin
    FSubEntries.Add(TStringEntry.CreateWithoutDefault(Section, Ident));
end;

procedure TCompositeIniEntry.AddBoolWithoutDefault;
begin
    FSubEntries.Add(TBoolEntry.CreateWithoutDefault(Section, Ident));
end;

procedure TCompositeIniEntry.StartOptionalValues;
begin
    FOptionalValues := true;
    FStartOfOptionalValues := FSubEntries.Count;
end;

procedure TCompositeIniEntry.AddInteger(aDefault: Longint; aUnitName, aDescription: string);
var
    xIniEntry: TIntegerEntry;
begin
    xIniEntry := TIntegerEntry.Create(Section, Ident, aDefault, aUnitName, aDescription);
    xIniEntry.ParentDescription := FDescription;
    FSubEntries.Add(xIniEntry);
end;

procedure TCompositeIniEntry.AddInteger(aDefault: Longint; aUnitName, aDescription: string;
    aMinValue, aMaxValue: Longint);
var
    xIniEntry: TIntegerEntry;
begin
    xIniEntry := TIntegerEntry.Create(Section, Ident, aDefault, aUnitName, aDescription, aMinValue,
        aMaxValue);
    xIniEntry.ParentDescription := FDescription;
    FSubEntries.Add(xIniEntry);
end;

procedure TCompositeIniEntry.AddFloat(aDefault: double; aUnitName, aDescription: string);
var
    xIniEntry: TFloatEntry;
begin
    xIniEntry := TFloatEntry.Create(Section, Ident, aDefault, aUnitName, aDescription);
    xIniEntry.ParentDescription := FDescription;
    FSubEntries.Add(xIniEntry);
end;

procedure TCompositeIniEntry.AddFloat(aDefault: double; aUnitName, aDescription: string;
    aMinValue, aMaxValue: double);
var
    xIniEntry: TFloatEntry;
begin
    xIniEntry := TFloatEntry.Create(Section, Ident, aDefault, aUnitName, aDescription, aMinValue, aMaxValue);
    xIniEntry.ParentDescription := FDescription;
    FSubEntries.Add(xIniEntry);
end;

procedure TCompositeIniEntry.AddString(aDefault, aUnitName, aDescription: string);
var
    xIniEntry: TStringEntry;
begin
    xIniEntry := TStringEntry.Create(Section, Ident, aDefault, aUnitName, aDescription);
    xIniEntry.ParentDescription := FDescription;
    FSubEntries.Add(xIniEntry);
end;

procedure TCompositeIniEntry.AddBool(aDefault: boolean; aDescription: string);
var
    xIniEntry: TBoolEntry;
begin
    xIniEntry := TBoolEntry.Create(Section, Ident, aDefault, '', aDescription);
    xIniEntry.ParentDescription := FDescription;
    FSubEntries.Add(xIniEntry);
end;


end.
