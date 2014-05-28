{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : cache for settings table
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  06.08.08 pk                                TN4165.1 initial revision
  13.08.08 pk TSettingsDataCache.ReadIntoCache TN4165.1 New
  13.10.08 pk                                TN4272.2 various changes
  14.10.08 pk OrigValueIsNull                TN4272.2 New
  09.12.08 pk DeleteItemByIdentName          TN4328   Call MakeKey
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  15.11.10 pk                                TN5340   Changes to prevent memory leak
  27.03.13 wl                                TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit SettingsDataCache;


interface


uses
    GeneralTypes,
    ListClasses,
    SettingsDataAdaptor,
    CommonTypes;

type
    TSettingsDataCacheItem = class
    private
        fArea: string;
        fSection: string;
        fIdent: string;
        fOrigValueIsNull: boolean;
        fOrigValue: string;
        fValue: string;
        fDataAdaptor: TSettingsDataAdaptor;
        fChanged: boolean;
        procedure SetValue(const aValue: string);
        function GetValue: string;
        procedure SetOrigValue(const aValue: string);

    public
        constructor Create(aDataAdaptor: TSettingsDataAdaptor); overload;
        constructor Create(const aArea: string; const aSection: string; const aIdent: string;
            aDataAdaptor: TSettingsDataAdaptor); overload;
        procedure ValuePosted();
        procedure ReadIntoCache;
        procedure WriteFromCache;

        function IsSameAsOrigValue(const aValue: string): boolean;
        property Area: string read fArea write fArea;
        property Section: string read fSection write fSection;
        property Ident: string read fIdent write fIdent;
        property Value: string read GetValue write SetValue;
        property OrigValueIsNull: boolean read fOrigValueIsNull;
        property OrigValue: string read fOrigValue write SetOrigValue;
        property Changed: boolean read fChanged;
    end;

    TSettingsDataCacheItemList = class
    private
        fList: TStringKeyObjectValueList;
        function GetCacheItem(aIndex: integer): TSettingsDataCacheItem;
        function GetCount(): integer;
    public
        constructor Create(aOwnsObjects: boolean; aSorted: boolean);
        destructor Destroy(); override;
        function MakeItemKey(const aAreaName, aSectionName, aIdentName: string): string;
        function ItemByKey(const aKey: string): TSettingsDataCacheItem;
        procedure DeleteByKey(const aKey: string);
        procedure Clear();
        procedure AddItem(aItem: TSettingsDataCacheItem);
        procedure GetItems(aItems: TSettingsDataCacheItemList);
        property this[aIndex: integer]: TSettingsDataCacheItem read GetCacheItem; default;
        property Count: integer read GetCount;
    end;

    TSettingsDataCacheReaderWriter = class
    protected
        fDataAdaptor: TSettingsDataAdaptor;
        procedure DoOpen(const aReadOnly: boolean); virtual; abstract;
    public
        constructor Create(const aDataAdaptor: TSettingsDataAdaptor);
        procedure Close;
        procedure Open(const aReadOnly: boolean);
        procedure WriteItemToDB(const aItem: TSettingsDataCacheItem; aAppend: boolean);
        procedure WriteItemsToDB(const aItems: TSettingsDataCacheItemList);
        procedure ReadItemFromDB(const aItem: TSettingsDataCacheItem);
        procedure ReadItemsFromDB(const aItems: TSettingsDataCacheItemList);
    end;

    TSettingsSectionDataCacheReaderWriter = class(TSettingsDataCacheReaderWriter)
    private
        fAreaName: string;
        fSectionName: string;
    protected
        procedure DoOpen(const aReadOnly: boolean); override;
    public
        constructor Create(const aAreaName: string; const aSectionName: string;
            const aDataAdaptor: TSettingsDataAdaptor);
    end;

    TSettingsAreaDataCacheReaderWriter = class(TSettingsDataCacheReaderWriter)
    private
        fAreaName: string;
    protected
        procedure DoOpen(const aReadOnly: boolean); override;
    public
        constructor Create(const aAreaName: string; const aDataAdaptor: TSettingsDataAdaptor);
    end;

    TSettingsAllDataCacheReaderWriter = class(TSettingsDataCacheReaderWriter)
    protected
        procedure DoOpen(const aReadOnly: boolean); override;
    end;

    TSettingsDataCacheSection = class
    private
        fAreaName: string;
        fSectionName: string;
        fDataAdaptor: TSettingsDataAdaptor;
        fItems: TSettingsDataCacheItemList;
        fReaderWriter: TSettingsSectionDataCacheReaderWriter;
    public
        constructor Create(const aAreaName, aSectionName: string; aDataAdaptor: TSettingsDataAdaptor);
        destructor Destroy; override;
        function MakeItemKey(const aIdentName: string): string;
        procedure ReadIntoCache;
        procedure WriteFromCache;
        function FindOrCreateIdent(const aIdentName: string): TSettingsDataCacheItem;
        function FindItemByIdentName(const aIdentName: string): TSettingsDataCacheItem;
        procedure DeleteItemByIdentName(const aIdentName: string);
        procedure DeleteAllIdents();
        procedure AddItem(const aItem: TSettingsDataCacheItem); overload;
        function GetIdentNames(): TStringArray;
        procedure GetItems(aItems: TSettingsDataCacheItemList);
        property AreaName: string read fAreaName;
        property SectionName: string read fSectionName;
        property Items: TSettingsDataCacheItemList read fItems;
        // procedure ReadAll();
    end;

    TSettingsDataCacheSectionList = class
    private
        fList: TStringKeyObjectValueList;
        function GetCacheSection(aIndex: integer): TSettingsDataCacheSection;
        function GetCount(): integer;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Add(const aSection: TSettingsDataCacheSection);
        procedure DeleteBySectionName(const aSectionName: string);
        procedure Clear();
        function SectionByName(const aSection: string): TSettingsDataCacheSection;
        property this[aIndex: integer]: TSettingsDataCacheSection read GetCacheSection; default;
        property Count: integer read GetCount;

    end;

    TSettingsDataCacheArea = class
    private
        fAreaName: string;
        fDataAdaptor: TSettingsDataAdaptor;
        fSections: TSettingsDataCacheSectionList;
        fReaderWriter: TSettingsAreaDataCacheReaderWriter;
        function FindItemBySectionNameAndIndentName(const aSection, aIdent: string): TSettingsDataCacheItem;
        procedure AddItem(const aItem: TSettingsDataCacheItem); overload;
        procedure DeleteItemBySectionNameAndIndentName(const aSection, aIdent: string);
    public
        constructor Create(const aAreaName: string; aDataAdaptor: TSettingsDataAdaptor);
        destructor Destroy; override;
        function FindOrCreateSection(const aSectionName: string): TSettingsDataCacheSection;
        function GetSectionNames(): TStringArray;
        function SectionExists(const aSectionName: string): boolean;
        function GetValue(const aSectionName, aIdent: string; var aValue: string): boolean;
        procedure SetValue(aCurrentUser: IUser; const aReason, aSectionName, aIdent, aValue: string);
        function GetIdentNamesBySection(const aSectionName: string): TStringArray;
        procedure DeleteSectionBySectionName(const aSectionName: string);
        function IsEmpty(): boolean;
        procedure AddSection(aSection: TSettingsDataCacheSection);
        procedure RemoveItem(aCurrentUser: IUser; const aReason, aSectionName, aIdent: string);
        procedure ReadIntoCache();
        procedure WriteFromCache();
        procedure ReadSectionIntoCache(const aSectionName: string);
        procedure WriteSectionFromCache(const aSectionName: string);
        property AreaName: string read fAreaName;
        property Sections: TSettingsDataCacheSectionList read fSections;
    end;

    TSettingsDataCacheAreaList = class
    private
        fList: TStringKeyObjectValueList;
        function GetCacheArea(aIndex: integer): TSettingsDataCacheArea;
        function GetCount(): integer;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Clear();
        procedure Add(const aArea: TSettingsDataCacheArea);
        function AreaByName(const aAreaName: string): TSettingsDataCacheArea;
        property this[aIndex: integer]: TSettingsDataCacheArea read GetCacheArea; default;
        property Count: integer read GetCount;
    end;

    TSettingsDataCache = class
    private
        fDataAdaptor: TSettingsDataAdaptor;
        fAreas: TSettingsDataCacheAreaList;
        fReaderWriter: TSettingsAllDataCacheReaderWriter;
        procedure AddArea(aArea: TSettingsDataCacheArea);
        procedure AddItem(const aItem: TSettingsDataCacheItem); overload;
    public
        constructor Create(aDataAdaptor: TSettingsDataAdaptor);
        destructor Destroy; override;
        function FindOrCreateArea(const aAreaName: string): TSettingsDataCacheArea;
        procedure ReadAreaIntoCache(const aArea: string);
        procedure ReadIntoCache;
    end;


implementation


uses
    Generics.Collections,
    SysUtils;

{ TSettingsDataCacheItem }

constructor TSettingsDataCacheItem.Create(const aArea: string; const aSection: string; const aIdent: string;
    aDataAdaptor: TSettingsDataAdaptor);
begin
    inherited Create();
    fArea := aArea;
    fSection := aSection;
    fIdent := aIdent;
    fDataAdaptor := aDataAdaptor;
    fOrigValueIsNull := true;
    fOrigValue := '';
    fValue := '';
end;

constructor TSettingsDataCacheItem.Create(aDataAdaptor: TSettingsDataAdaptor);
begin
    Create('', '', '', aDataAdaptor);
end;

function TSettingsDataCacheItem.GetValue: string;
begin
    result := fValue;
end;

function TSettingsDataCacheItem.IsSameAsOrigValue(const aValue: string): boolean;
begin
    result := (not fOrigValueIsNull) and (aValue = fOrigValue);
end;

procedure TSettingsDataCacheItem.SetValue(const aValue: string);
begin
    if IsSameAsOrigValue(aValue) then
        EXIT;
    fValue := aValue;
    fChanged := true;
end;

procedure TSettingsDataCacheItem.ValuePosted;
begin
    fChanged := false;
    fOrigValue := fValue;
end;

procedure TSettingsDataCacheItem.SetOrigValue(const aValue: string);
begin
    fOrigValue := aValue;
    fOrigValueIsNull := false;

    fValue := fOrigValue;
    fChanged := false;
end;

procedure TSettingsDataCacheItem.ReadIntoCache;
begin
end;

procedure TSettingsDataCacheItem.WriteFromCache();

begin
end;

{ TSettingsDataCacheItemList }
constructor TSettingsDataCacheItemList.Create(aOwnsObjects: boolean; aSorted: boolean);
begin
    inherited Create();
    // if aSorted then
    fList := TStringKeyObjectValueList.Create(aOwnsObjects, dupError, true);
    // else
    // fList := TStringKeyObjectValueList.Create( aOwnsObjects );

end;

destructor TSettingsDataCacheItemList.Destroy;
begin
    Clear();
    FreeAndNil(fList);
    inherited;
end;

function TSettingsDataCacheItemList.GetCount(): integer;
begin
    result := fList.Count;
end;

procedure TSettingsDataCacheItemList.Clear;
begin
    fList.Clear();
end;

function TSettingsDataCacheItemList.ItemByKey(const aKey: string): TSettingsDataCacheItem;
var
    xIndex: integer;
begin
    result := nil;
    xIndex := fList.IndexOf(aKey);
    if xIndex < 0 then
        EXIT;
    result := self[xIndex];
end;

procedure TSettingsDataCacheItemList.DeleteByKey(const aKey: string);
var
    xIndex: integer;
begin
    xIndex := fList.IndexOf(aKey);
    if xIndex < 0 then
        EXIT;
    fList.Delete(xIndex);
end;

function TSettingsDataCacheItemList.GetCacheItem(aIndex: integer): TSettingsDataCacheItem;
begin
    result := fList.Objects[aIndex] as TSettingsDataCacheItem;
end;

procedure TSettingsDataCacheItemList.GetItems(aItems: TSettingsDataCacheItemList);
var
    x: integer;
begin
    for x := 0 to self.Count - 1 do
        aItems.AddItem(self[x]);
end;

procedure TSettingsDataCacheItemList.AddItem(aItem: TSettingsDataCacheItem);
begin
    try
        fList.AddObject(MakeItemKey(aItem.Area, aItem.Section, aItem.Ident), aItem);
    except
        on e: exception do
            raise Exception.CreateFmt('Could not add Ident %s', [aItem.Ident]);
    end;
end;

function TSettingsDataCacheItemList.MakeItemKey(const aAreaName, aSectionName, aIdentName: string): string;
begin
    result := Format('%s, %s, %s', [aAreaName, aSectionName, aIdentName]);
end;

{ TSettingsDataCacheSection }

constructor TSettingsDataCacheSection.Create(const aAreaName, aSectionName: string;
    aDataAdaptor: TSettingsDataAdaptor);
begin
    inherited Create();
    fAreaName := aAreaName;
    fSectionName := aSectionName;
    fDataAdaptor := aDataAdaptor;
    fItems := TSettingsDataCacheItemList.Create(true, true);
    fReaderWriter := TSettingsSectionDataCacheReaderWriter.Create(fAreaName, fSectionName, fDataAdaptor);
end;

destructor TSettingsDataCacheSection.Destroy;
begin
    FreeAndNil(fReaderWriter);
    FreeAndNil(fItems);
    inherited;
end;
{
  procedure TSettingsDataCacheSection.ReadAll;
  var
  xRecs : TSettingsRecArray;
  x : integer;
  begin
  self.Clear();
  fDataAdaptor.ReadRecsBySection( fArea, fSection, xRecs );
  for x := 0 to High( xRecs ) do begin
  self.AddItem( TSettingsDataCacheItem.Create( xRecs[x].Section, xRecs[x].Section, xRecs[x].Ident, xRecs[x].Value ) );
  end;
  end;
}

function TSettingsDataCacheSection.GetIdentNames(): TStringArray;
var
    x: integer;
begin
    SetLength(result, fItems.Count);
    for x := 0 to fItems.Count - 1 do
    begin
        result[x] := fItems[x].Ident;
    end;
end;

function TSettingsDataCacheSection.MakeItemKey(const aIdentName: string): string;
begin
    result := fItems.MakeItemKey(fAreaName, fSectionName, aIdentName);
end;

function TSettingsDataCacheSection.FindItemByIdentName(const aIdentName: string): TSettingsDataCacheItem;
begin
    result := fItems.ItemByKey(fItems.MakeItemKey(fAreaName, fSectionName, aIdentName));
end;

function TSettingsDataCacheSection.FindOrCreateIdent(const aIdentName: string): TSettingsDataCacheItem;
begin
    result := FindItemByIdentName(aIdentName);
    if not Assigned(result) then
    begin
        result := TSettingsDataCacheItem.Create(fAreaName, fSectionName, aIdentName, fDataAdaptor);
        fItems.AddItem(result);
    end;
end;

procedure TSettingsDataCacheSection.AddItem(const aItem: TSettingsDataCacheItem);
begin
    fItems.AddItem(aItem);
end;

procedure TSettingsDataCacheSection.DeleteItemByIdentName(const aIdentName: string);
begin
    fItems.DeleteByKey(fItems.MakeItemKey(fAreaName, fSectionName, aIdentName));
    // if (aCurrentUser <> nil) then
    // aCurrentUser.LogDataChanged( self.TableName, aArea + ', ' + aSection + ', ' + aIdent + ' deleted', aReason, lctRemove);
end;

procedure TSettingsDataCacheSection.GetItems(aItems: TSettingsDataCacheItemList);
begin
    fItems.GetItems(aItems);
end;

procedure TSettingsDataCacheSection.ReadIntoCache;
begin
    fItems.Clear();
    fReaderWriter.ReadItemsFromDB(fItems);
end;

procedure TSettingsDataCacheSection.WriteFromCache();
begin
    fReaderWriter.WriteItemsToDB(fItems);
end;

procedure TSettingsDataCacheSection.DeleteAllIdents;
begin
    fItems.Clear();
end;

{ TSettingsDataCacheSectionList }
constructor TSettingsDataCacheSectionList.Create;
begin
    inherited Create();
    fList := TStringKeyObjectValueList.Create(true, dupError, true);
end;

destructor TSettingsDataCacheSectionList.Destroy;
begin
    Clear();
    FreeAndNil(fList);
    inherited;
end;

function TSettingsDataCacheSectionList.GetCount(): integer;
begin
    result := fList.Count;
end;

function TSettingsDataCacheSectionList.SectionByName(const aSection: string): TSettingsDataCacheSection;
var
    xIndex: integer;
begin
    result := nil;
    xIndex := fList.IndexOf(aSection);
    if xIndex < 0 then
        EXIT;
    result := self[xIndex];
end;

function TSettingsDataCacheSectionList.GetCacheSection(aIndex: integer): TSettingsDataCacheSection;
begin
    result := fList.Objects[aIndex] as TSettingsDataCacheSection;
end;

procedure TSettingsDataCacheSectionList.Clear;
begin
    fList.Clear();
end;

procedure TSettingsDataCacheSectionList.Add(const aSection: TSettingsDataCacheSection);
begin
    fList.AddObject(aSection.SectionName, aSection);
end;

procedure TSettingsDataCacheSectionList.DeleteBySectionName(const aSectionName: string);
var
    xIndex: integer;
begin
    xIndex := fList.IndexOf(aSectionName);
    if xIndex < 0 then
        EXIT;
    fList.Delete(xIndex);
end;

{ TSettingsDataCacheArea }

procedure TSettingsDataCacheArea.AddSection(aSection: TSettingsDataCacheSection);
begin
    try
        fSections.Add(aSection);
    except
        on e: exception do
            raise Exception.CreateFmt('Could not add section %s', [aSection.SectionName]);
    end;
end;

constructor TSettingsDataCacheArea.Create(const aAreaName: string; aDataAdaptor: TSettingsDataAdaptor);
begin
    inherited Create();
    fAreaName := aAreaName;
    fSections := TSettingsDataCacheSectionList.Create();
    fDataAdaptor := aDataAdaptor;
    fReaderWriter := TSettingsAreaDataCacheReaderWriter.Create(fAreaName, fDataAdaptor);
end;

destructor TSettingsDataCacheArea.Destroy();
begin
    FreeAndNil(fReaderWriter);
    FreeAndNil(fSections);
    inherited;
end;

function TSettingsDataCacheArea.GetSectionNames(): TStringArray;
var
    x: integer;
begin
    SetLength(result, fSections.Count);
    for x := 0 to fSections.Count - 1 do
        result[x] := fSections[x].SectionName;
end;

function TSettingsDataCacheArea.SectionExists(const aSectionName: string): boolean;
var
    xSectionNames: TList<string>;
    xNames: TStringArray;
begin
    xSectionNames := TList<string>.Create();
    try
        xNames := GetSectionNames();
        xSectionNames.AddRange(xNames);
        result := xSectionNames.IndexOf(aSectionName) >= 0;
    finally
        FreeAndNil(xSectionNames);
    end;
end;

function TSettingsDataCacheArea.FindItemBySectionNameAndIndentName(const aSection, aIdent: string)
    : TSettingsDataCacheItem;
var
    xSection: TSettingsDataCacheSection;

begin
    result := nil;
    xSection := fSections.SectionByName(aSection);
    if not Assigned(xSection) then
        EXIT;
    result := xSection.FindItemByIdentName(aIdent);
end;

procedure TSettingsDataCacheArea.DeleteItemBySectionNameAndIndentName(const aSection, aIdent: string);
var
    xSection: TSettingsDataCacheSection;
begin
    xSection := fSections.SectionByName(aSection);
    if not Assigned(xSection) then
        EXIT;
    xSection.DeleteItemByIdentName(aIdent);
end;

function TSettingsDataCacheArea.GetValue(const aSectionName, aIdent: string; var aValue: string): boolean;
var
    xItem: TSettingsDataCacheItem;
begin
    xItem := FindItemBySectionNameAndIndentName(aSectionName, aIdent);
    result := Assigned(xItem);
    if not result then
        EXIT;
    aValue := xItem.Value;
end;

function TSettingsDataCacheArea.GetIdentNamesBySection(const aSectionName: string): TStringArray;
var
    xSection: TSettingsDataCacheSection;
begin
    xSection := fSections.SectionByName(aSectionName);
    if not Assigned(xSection) then
        EXIT;
    result := xSection.GetIdentNames();
end;

function TSettingsDataCacheArea.IsEmpty: boolean;
begin
    result := fSections.Count = 0;
end;

procedure TSettingsDataCacheArea.SetValue(aCurrentUser: IUser;
    const aReason, aSectionName, aIdent, aValue: string);
var
    xItem: TSettingsDataCacheItem;
begin
    xItem := FindItemBySectionNameAndIndentName(aSectionName, aIdent);
    if not Assigned(xItem) then
    begin
        xItem := TSettingsDataCacheItem.Create(fAreaName, aSectionName, aIdent, fDataAdaptor);
        self.AddItem(xItem);
    end;

    xItem.Value := aValue;
end;

procedure TSettingsDataCacheArea.RemoveItem(aCurrentUser: IUser; const aReason, aSectionName, aIdent: string);
begin
    self.DeleteItemBySectionNameAndIndentName(aSectionName, aIdent);
end;

function TSettingsDataCacheArea.FindOrCreateSection(const aSectionName: string): TSettingsDataCacheSection;
begin
    result := fSections.SectionByName(aSectionName);
    if not Assigned(result) then
    begin
        result := TSettingsDataCacheSection.Create(fAreaName, aSectionName, fDataAdaptor);
        self.AddSection(result);
    end;
end;

procedure TSettingsDataCacheArea.AddItem(const aItem: TSettingsDataCacheItem);
var
    xSection: TSettingsDataCacheSection;
begin
    xSection := FindOrCreateSection(aItem.Section);
    xSection.AddItem(aItem);
end;

procedure TSettingsDataCacheArea.ReadIntoCache;
var
    x: integer;
    xItems: TSettingsDataCacheItemList;
begin
    fSections.Clear();
    xItems := TSettingsDataCacheItemList.Create(false, true);
    try
        fReaderWriter.ReadItemsFromDB(xItems);

        for x := 0 to xItems.Count - 1 do
        begin
            AddItem(xItems[x]);
        end;

    finally
        FreeAndNil(xItems);
    end;
end;

procedure TSettingsDataCacheArea.WriteFromCache();
var
    x: integer;
    xItems: TSettingsDataCacheItemList;
begin
    xItems := TSettingsDataCacheItemList.Create(false, false);
    try
        for x := 0 to fSections.Count - 1 do
        begin
            fSections[x].GetItems(xItems);
        end;

        fReaderWriter.WriteItemsToDB(xItems);

    finally
        FreeAndNil(xItems);
    end;

end;

procedure TSettingsDataCacheArea.ReadSectionIntoCache(const aSectionName: string);
var
    xSection: TSettingsDataCacheSection;
begin
    xSection := FindOrCreateSection(aSectionName);
    ASSERT(Assigned(xSection));
    xSection.ReadIntoCache();
end;

procedure TSettingsDataCacheArea.WriteSectionFromCache(const aSectionName: string);
var
    xSection: TSettingsDataCacheSection;
begin
    xSection := fSections.SectionByName(aSectionName);
    if not Assigned(xSection) then
        EXIT;
    xSection.WriteFromCache();
end;

procedure TSettingsDataCacheArea.DeleteSectionBySectionName(const aSectionName: string);
begin
    fSections.DeleteBySectionName(aSectionName);
end;

{ TSettingsDataCacheAreaList }

function TSettingsDataCacheAreaList.AreaByName(const aAreaName: string): TSettingsDataCacheArea;
var
    xIndex: integer;
begin
    result := nil;
    xIndex := fList.IndexOf(aAreaName);
    if xIndex < 0 then
        EXIT;
    result := self[xIndex];
end;

constructor TSettingsDataCacheAreaList.Create;
begin
    inherited Create();
    fList := TStringKeyObjectValueList.Create(true, dupError, true);
end;

destructor TSettingsDataCacheAreaList.Destroy;
begin
    Clear();
    FreeAndNil(fList);
    inherited;
end;

procedure TSettingsDataCacheAreaList.Clear();
begin
    fList.Clear();
end;

function TSettingsDataCacheAreaList.GetCacheArea(aIndex: integer): TSettingsDataCacheArea;
begin
    result := fList.Objects[aIndex] as TSettingsDataCacheArea;
end;

function TSettingsDataCacheAreaList.GetCount: integer;
begin
    result := fList.Count;
end;

procedure TSettingsDataCacheAreaList.Add(const aArea: TSettingsDataCacheArea);
begin
    fList.AddObject(aArea.AreaName, aArea);
end;

{ TSettingsDataCache }

constructor TSettingsDataCache.Create(aDataAdaptor: TSettingsDataAdaptor);
begin
    inherited Create();
    fAreas := TSettingsDataCacheAreaList.Create();
    fDataAdaptor := aDataAdaptor;
    fReaderWriter := TSettingsAllDataCacheReaderWriter.Create(fDataAdaptor);
end;

destructor TSettingsDataCache.Destroy();
begin
    FreeAndNil(fReaderWriter);
    FreeAndNil(fAreas);
    inherited;
end;

procedure TSettingsDataCache.AddArea(aArea: TSettingsDataCacheArea);
begin
    try
        fAreas.Add(aArea);
    except
        on e: exception do
            raise Exception.CreateFmt('Could not add area %s', [aArea.AreaName]);
    end;
end;

function TSettingsDataCache.FindOrCreateArea(const aAreaName: string): TSettingsDataCacheArea;
begin
    result := fAreas.AreaByName(aAreaName);
    if not Assigned(result) then
    begin
        result := TSettingsDataCacheArea.Create(aAreaName, fDataAdaptor);
        self.AddArea(result);
    end;
end;

procedure TSettingsDataCache.ReadAreaIntoCache(const aArea: string);
var
    xCacheArea: TSettingsDataCacheArea;
begin
    xCacheArea := FindOrCreateArea(aArea);
    xCacheArea.ReadIntoCache();
end;

procedure TSettingsDataCache.AddItem(const aItem: TSettingsDataCacheItem);
var
    xArea: TSettingsDataCacheArea;
begin
    xArea := FindOrCreateArea(aItem.Area);
    xArea.AddItem(aItem);
end;

procedure TSettingsDataCache.ReadIntoCache();
var
    x: integer;
    xItems: TSettingsDataCacheItemList;
begin
    fAreas.Clear();
    xItems := TSettingsDataCacheItemList.Create(false, true);
    try
        fReaderWriter.ReadItemsFromDB(xItems);

        for x := 0 to xItems.Count - 1 do
        begin
            AddItem(xItems[x]);
        end;
    finally
        FreeAndNil(xItems);
    end;
end;

constructor TSettingsDataCacheReaderWriter.Create(const aDataAdaptor: TSettingsDataAdaptor);
begin
    inherited Create();
    fDataAdaptor := aDataAdaptor;
end;

procedure TSettingsDataCacheReaderWriter.WriteItemToDB(const aItem: TSettingsDataCacheItem; aAppend: boolean);
begin
    if aAppend then
    begin
        fDataAdaptor.DataProvider.Append();
        fDataAdaptor.WriteAreaField(aItem.Area);
        fDataAdaptor.WriteSectionField(aItem.Section);
        fDataAdaptor.WriteIdentField(aItem.Ident);
    end
    else
    begin
        fDataAdaptor.DataProvider.Edit();
    end;

    fDataAdaptor.WriteValueField(aItem.Value);

    fDataAdaptor.DataProvider.Post();
    aItem.ValuePosted();
end;

procedure TSettingsDataCacheReaderWriter.ReadItemFromDB(const aItem: TSettingsDataCacheItem);
begin
    aItem.Area := fDataAdaptor.ReadAreaField();
    aItem.Section := fDataAdaptor.ReadSectionField();
    aItem.Ident := fDataAdaptor.ReadIdentField();
    aItem.OrigValue := fDataAdaptor.ReadValueField();
end;

procedure TSettingsDataCacheReaderWriter.ReadItemsFromDB(const aItems: TSettingsDataCacheItemList);
var
    xItem: TSettingsDataCacheItem;
begin
    self.Open(true);
    try
        while not fDataAdaptor.DataProvider.Eof do
        begin
            xItem := TSettingsDataCacheItem.Create(fDataAdaptor);
            ReadItemFromDB(xItem);
            aItems.AddItem(xItem);
            fDataAdaptor.DataProvider.Next();
        end;
    finally
        self.Close();
    end;
end;

procedure TSettingsDataCacheReaderWriter.Open(const aReadOnly: boolean);
begin
    DoOpen(aReadOnly);
end;

procedure TSettingsDataCacheReaderWriter.Close();
begin
    fDataAdaptor.Close();
end;

procedure TSettingsDataCacheReaderWriter.WriteItemsToDB(const aItems: TSettingsDataCacheItemList);
var
    xItem: TSettingsDataCacheItem;
    x: integer;
    xDBItem: TSettingsDataCacheItem;
begin

    self.Open(false);
    try
        xDBItem := TSettingsDataCacheItem.Create(nil);
        // Phase 1: Edit existing entries, Delete entries that are no longer in Cache
        while not fDataAdaptor.DataProvider.Eof do
        begin
            ReadItemFromDB(xDBItem);
            xItem := aItems.ItemByKey(aItems.MakeItemKey(xDBItem.Area, xDBItem.Section, xDBItem.Ident));
            if not Assigned(xItem) then
            begin
                // assumption: delete command automatically moves to next line
                fDataAdaptor.DataProvider.Delete();
            end
            else
            begin
                WriteItemToDB(xItem, false);
                fDataAdaptor.DataProvider.Next();
            end;
        end;
        FreeAndNil(xDBItem);

        // Phase 2: Add entires which do not yet exist
        for x := 0 to aItems.Count - 1 do
        begin
            xItem := aItems[x];
            if not xItem.Changed then
                CONTINUE;
            WriteItemToDB(xItem, true);
        end;
    finally
        self.Close();
    end;

end;

{ TSettingsSectionDataCacheReaderWriter }

constructor TSettingsSectionDataCacheReaderWriter.Create(const aAreaName, aSectionName: string;
    const aDataAdaptor: TSettingsDataAdaptor);
begin
    inherited Create(aDataAdaptor);
    fAreaName := aAreaName;
    fSectionName := aSectionName;
end;

procedure TSettingsSectionDataCacheReaderWriter.DoOpen(const aReadOnly: boolean);
begin
    fDataAdaptor.SelectAndOpenSection(fAreaName, fSectionName, aReadOnly);
end;

{ TSettingsAreaDataCacheReaderWriter }

constructor TSettingsAreaDataCacheReaderWriter.Create(const aAreaName: string;
    const aDataAdaptor: TSettingsDataAdaptor);
begin
    inherited Create(aDataAdaptor);
    fAreaName := aAreaName;
end;

procedure TSettingsAreaDataCacheReaderWriter.DoOpen(const aReadOnly: boolean);
begin
    fDataAdaptor.SelectAndOpenArea(fAreaName, aReadOnly);

end;

{ TSettingsAllDataCacheReaderWriter }

procedure TSettingsAllDataCacheReaderWriter.DoOpen(const aReadOnly: boolean);
begin
    fDataAdaptor.SelectAndOpenAll(aReadOnly);
end;


end.
