{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : A TObjectList-wrapper for ini entries.
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.09.02 wl                               TN1293.1 initial version
  15.10.02 wl                               TN1293.2 neu: TIniSection, TTypeIniSection, TIniSectionList
  16.10.02 wl  TIniSectionList.GetSection   TN1293.2 neu
  21.10.02 wl  TIniSection.PrepareIdentList TN1293.1 Bugfix: funktioniert jetzt auch mit x=0
  21.10.02 wl  TIniEntryList.GetEntry       TN1293.1 vereinfacht
  21.10.02 wl  TIniSection.IsAllowed        TN1293.1 neu
  22.10.02 wl  TIniSection.IsAllowed        TN1293.1 Bug beseitigt
  22.10.02 wl                               TN1293.1 neu: TStringIniSection, TIntegerIniSection
  25.10.02 wl TIniSection.FitToDefaultIdent TN1293.1 neu: überprüft auch strings mit Bezeichner %d
  06.11.02 wl  TIniSection.PrepareIdentList TN1293.1 Bug beseitigt
  20.12.02 wl  TIniSection                  TN1293.5 abgeleitet von interface IIniSection, definiert in CommonTypes
  20.12.02 wl                               TN1293.5 neu: Zugriff auf TResLoader
  15.01.03 wl  TIniSection                  TN1293.1 --> IniSection.pas
  15.01.03 wl  TIniSectionList              TN1293.1 --> IniSectionList.pas
  31.08.07 wl                               TN3811.4 Index-property ist jetzt default;
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  -------------------------------------------------------------------------------------------------- }

unit IniEntryList;


interface


uses
    ListClasses,
    IniEntry;

type
    TIniEntryList = class
    protected
        FList: TGeneralObjectList;
    private
        function GetCount: integer;
        function GetItem(aIndex: Integer): TIniEntry;
        procedure SetOwnsObjects(aOwnsObjects: boolean);
    public
        // Constructor/Destructor
        constructor Create;
        destructor Destroy; override;
        // Public Methods
        function GetEntry(aSection, aIdent: string): TIniEntry;
        procedure Clear;
        function Add(aItem: TIniEntry): Integer;
        // Properties
        property Count: integer read GetCount;
        property CSharpIsKing[index: Integer]: TIniEntry read GetItem; default;
        property OwnsObjects: boolean write SetOwnsObjects;
    end;


    // ##################################################################################################


implementation


uses
    SysUtils;

// --------------------------------------------------------------------------------------------------
// TIniEntryList
// --------------------------------------------------------------------------------------------------
constructor TIniEntryList.Create;
begin
    inherited Create;
    FList := TGeneralObjectList.Create;
end;

destructor TIniEntryList.Destroy;
begin
    FreeAndNil(FList);
    inherited Destroy;
end;

function TIniEntryList.GetCount: integer;
begin
    result := FList.Count;
end;

function TIniEntryList.GetItem(aIndex: Integer): TIniEntry;
begin
    result := FList.Items[aIndex] as TIniEntry;
end;

function TIniEntryList.GetEntry(aSection, aIdent: string): TIniEntry;
var
    x: integer;
    xIniEntry: TIniEntry;
begin
    result := nil;

    // serch in the list for the right IniEntry
    for x := 0 to FList.Count - 1 do
    begin
        xIniEntry := (FList.Items[x] as TIniEntry);
        if (xIniEntry.Ident = aIdent) and (xIniEntry.Section = aSection) then
            result := xIniEntry;
    end;
end;

procedure TIniEntryList.SetOwnsObjects(aOwnsObjects: boolean);
begin
    FList.OwnsObjects := aOwnsObjects;
end;

procedure TIniEntryList.Clear;
begin
    FList.Clear;
end;

function TIniEntryList.Add(aItem: TIniEntry): Integer;
begin
    result := FList.Add(aItem);
end;


end.
