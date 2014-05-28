{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : A TObjectList-wrapper for ini sections.
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  15.01.03 wl                               TN1293.1 separated from IniEntryList.pas
  31.08.07 wl                               TN3811.4 Index-property ist jetzt default;
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  -------------------------------------------------------------------------------------------------- }

unit IniSectionList;


interface


uses
    ListClasses,
    IniSection;

type
    TIniSectionList = class
    protected
        FList: TGeneralObjectList;
    private
        function GetCount: integer;
        function GetItem(aIndex: Integer): TIniSection;
        procedure SetOwnsObjects(aOwnsObjects: boolean);
    public
        // Constructor/Destructor
        constructor Create;
        destructor Destroy; override;
        // Public Methods
        function Add(aItem: TIniSection): Integer;
        function GetSection(aSection, aDefaultIdent: string): TIniSection;
        // Properties
        property Count: integer read GetCount;
        property CSharpIsKing[index: Integer]: TIniSection read GetItem; default;
        property OwnsObjects: boolean write SetOwnsObjects;
    end;


implementation


uses
    SysUtils;

// --------------------------------------------------------------------------------------------------
// TIniSectionList
// --------------------------------------------------------------------------------------------------
constructor TIniSectionList.Create;
begin
    inherited Create;
    FList := TGeneralObjectList.Create;
end;

destructor TIniSectionList.Destroy;
begin
    FreeAndNil(FList);
    inherited Destroy;
end;

function TIniSectionList.GetCount: integer;
begin
    result := FList.Count;
end;

function TIniSectionList.GetItem(aIndex: Integer): TIniSection;
begin
    result := FList.Items[aIndex] as TIniSection;
end;

procedure TIniSectionList.SetOwnsObjects(aOwnsObjects: boolean);
begin
    FList.OwnsObjects := aOwnsObjects;
end;

function TIniSectionList.Add(aItem: TIniSection): Integer;
begin
    result := FList.Add(aItem);
end;

function TIniSectionList.GetSection(aSection, aDefaultIdent: string): TIniSection;
var
    x: integer;
    xIniSection: TIniSection;
begin
    result := nil;

    // serch in the list for the right IniEntry
    for x := 0 to FList.Count - 1 do
    begin
        xIniSection := GetItem(x);
        if (xIniSection.Section = aSection) and (xIniSection.DefaultIdent = aDefaultIdent) then
            result := xIniSection;
    end;
end;


end.
