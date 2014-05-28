{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  04.12.09 pk  TDisplayComponentItemList     TN4919  Now based on GenericObjectList
  13.11.12 wl                                TN6015   überarbeitet für DisplayComponentsDataAdaptor
  28.02.13 wl  TDSBool, TDSFloat             TN6096   wieder eingeführt
  ----------------------------------------------------------------------------------------------------------------------- }

unit DisplayComponentSettings;


interface


uses
    Generics.Collections,
    DisplayComponentsDataAdaptor;

type
    TDisplayComponentID = TGUID;

    TOnEditDisplayComponentSetting = procedure(aSender: TObject) of object;

    TDisplayComponentSetting = class
    strict private
        fOwner: TObject;
        fValue: string;
        fDefaultValue: string;
        fSettingName: string;
        fDescription: string;
        fOnEditValue: TOnEditDisplayComponentSetting;
    strict protected
        fSkipRead: boolean;
        fPossibleValues: TList<string>;
        procedure SetValue(const aValue: string); virtual;
        function GetValue: string; virtual;
        function GetSkipWrite: boolean; virtual;
        function GetAsInt(): integer;
        function GetAsBool(): boolean;
        function GetAsFloat: double;
        function GetAsStr: string;
    public const
        cSettingAlignClient = 'Client';
        cSettingAlignNone = 'None';
        cSettingAlignTop = 'Top';
        cSettingAlignBottom = 'Bottom';
        cSettingAlignLeft = 'Left';
        cSettingAlignRight = 'Right';
        cSettingNameChildComponents = 'ChildComponents';
    public
        constructor Create(const aSettingName: string; const aDescription: string;
            const aDefaultValue: string);
        destructor Destroy(); override;

        class function ItemReadFromRecs(const aRecs: TArray<TDisplayComponentRec>; const aSettingName: string;
            const aDefault: string): string;
        class procedure ItemAddToList(aList: TList<TDisplayComponentRec>; const aSettingName: string;
            const aValue: string);
        class procedure ItemWriteToList(aList: TList<TDisplayComponentRec>; const aSettingName: string;
            const aValue: string; aDelete: boolean);
        class procedure ItemDeleteFromList(aList: TList<TDisplayComponentRec>; const aSettingName: string);

        class function Find(aList: TObjectList<TDisplayComponentSetting>; const aSettingName: string;
            aMustFind: boolean): TDisplayComponentSetting; static;

        procedure ReadFromRecs(const aRecs: TArray<TDisplayComponentRec>); virtual;
        procedure WriteToList(aList: TList<TDisplayComponentRec>); virtual;
        procedure ChangeSettingName(const aNewSetttingName: string);
        property SettingName: string read fSettingName;
        property Value: string read GetValue write SetValue;
        property DefaultValue: string read fDefaultValue write fDefaultValue;
        property Description: string read fDescription write fDescription;
        property OnEditValue: TOnEditDisplayComponentSetting read fOnEditValue write fOnEditValue;
        property PossibleValues: TList<string>read fPossibleValues;
        property AsInt: integer read GetAsInt;
        property AsBool: boolean read GetAsBool;
        property AsFloat: double read GetAsFloat;
        property AsStr: string read GetAsStr;
        property Owner: TObject read fOwner write fOwner;
        property SkipRead: boolean read fSkipRead;
        property SkipWrite: boolean read GetSkipWrite;
    end;

    TDSStr = class(TDisplayComponentSetting)
    private
        procedure SetPossibleValues(const aPossibleValues: array of string);
    public
        constructor Create(const aSettingName: string; const aDescription: string;
            const aDefaultValue: string; const aPossibleValues: array of string);
    end;

    TDSInt = class(TDisplayComponentSetting)
    private
        procedure SetPossibleValues(const aPossibleValues: array of integer);
    public
        constructor Create(const aSettingName: string; const aDescription: string;
            const aDefaultValue: integer; const aPossibleValues: array of integer);
    end;

    TDSFloat = class(TDisplayComponentSetting)
    private
        procedure SetPossibleValues(const aPossibleValues: array of double);
    public
        constructor Create(const aSettingName: string; const aDescription: string;
            const aDefaultValue: double; const aPossibleValues: array of double);
    end;

    TDSBool = class(TDisplayComponentSetting)
    private
        procedure SetPossibleValues(const aPossibleValues: array of boolean);
    public
        constructor Create(const aSettingName: string; const aDescription: string;
            const aDefaultValue: boolean; const aPossibleValues: array of boolean);
        class function BoolToStr(aValue: boolean): string;
    end;

    TDSAlign = class(TDSStr)
    public
        constructor Create(const aSettingName: string; const aDescription: string;
            const aDefaultValue: string);
    end;

    TDSEvent = class(TDSStr)
    public
        constructor Create(const aSettingName: string; const aDescription: string);
    end;

    TDSDisplayComponentType = class(TDisplayComponentSetting)
    protected
        function GetSkipWrite(): boolean; override;
    public
        constructor Create(const aDefaultValue: string);
    end;

    TDSDisplayComponent = class(TDisplayComponentSetting)
    private
        fDisplayComponentID: TDisplayComponentID;
    public
        constructor Create(const aSettingName: string; const aDescription: string;
            const aDefaultValue: string; aID: TDisplayComponentID);
        property DisplayComponentID: TDisplayComponentID read fDisplayComponentID;
    end;

    TDSMulti = class(TDisplayComponentSetting)
    private
        fChildSettingNamePrefix: string;
        fChildSettings: TObjectList<TDisplayComponentSetting>;
        fListSizeChanged: boolean;
        function GetChildSettingName(const aChildNumber: integer): string;
        function GetChildCountSettingName(): string;
        function ReadChildCountSetting(const aRecs: TArray<TDisplayComponentRec>): integer;
        procedure WriteChildCountSetting(aList: TList<TDisplayComponentRec>; const aValue: integer);
    protected
        function CreateChildSetting(const aSettingName: string): TDisplayComponentSetting; virtual;
        function GetSkipWrite(): boolean; override;
    public
        constructor Create(const aSettingName: string; const aChildSettingNamePrefix: string);
        destructor Destroy(); override;
        function AddNewChildSetting(): TDisplayComponentSetting;
        procedure DeleteChildSetting(const aChildSettingName: string);
        procedure ReadFromRecs(const aRecs: TArray<TDisplayComponentRec>); override;
        procedure WriteToList(aList: TList<TDisplayComponentRec>); override;
        property ChildSettings: TObjectList<TDisplayComponentSetting>read fChildSettings;
    end;

    TDSChildComponents = class(TDSMulti)
    protected
        function CreateChildSetting(const aSettingName: string): TDisplayComponentSetting; override;
    public
        constructor Create();
        function GetAllChildNames(): TArray<string>;
    end;

    TDisplayComponentSettingsClass = class of TDisplayComponentSettingList;

    TDisplayComponentSettingList = class
    protected
        fList: TObjectList<TDisplayComponentSetting>;
        fComponentName: string;
        fTypeName: string;
        function GetSetting(aIndex: integer): TDisplayComponentSetting;
        function GetCount: integer;
        procedure AddSettings(); virtual;
        procedure LoadSettings(); virtual;
        function GetCanHaveChildren(): boolean; virtual;
    public
        constructor Create(const aComponentName, aTypeName: string); virtual;
        procedure Add(aSetting: TDisplayComponentSetting);
        procedure AddFromList(aSettings: TDisplayComponentSettingList);
        procedure AddStr(const aSettingName: string; const aDefaultValue: string); overload;
        procedure AddStr(const aSettingName: string; const aDescription: string; const aDefaultValue: string;
            const aPossibleValues: array of string); overload;

        procedure AddFloat(const aSettingName: string; const aDefaultValue: double); overload;
        procedure AddFloat(const aSettingName: string; const aDescription: string;
            const aDefaultValue: double; const aPossibleValues: array of double); overload;

        procedure AddInt(const aSettingName: string; const aDefaultValue: integer); overload;
        procedure AddInt(const aSettingName: string; const aDescription: string; const aDefaultValue: integer;
            const aPossibleValues: array of integer); overload;

        procedure AddBool(const aSettingName: string; const aDefaultValue: boolean); overload;
        procedure AddBool(const aSettingName: string; const aDescription: string;
            const aDefaultValue: boolean; const aPossibleValues: array of boolean); overload;

        procedure AddEvent(const aSettingName: string; const aDescription: string);

        procedure AddAlign(const aSettingName: string; const aDescription: string;
            const aDefaultValue: string);
        procedure AddDisplayComponentType(const aDefaultValue: string); virtual;
        procedure AddDisplayComponent(const aSettingName, aDescription: string; aID: TDisplayComponentID);

        function Find(const aSettingName: string; aMustFind: boolean = true): TDisplayComponentSetting;
        procedure ReadAll();
        procedure WriteAll();
        procedure DeleteAll();
        procedure Prepare();
        function HasOneToManyOption(): boolean; virtual;
        //
        property Count: integer read GetCount;
        property CanHaveChildren: boolean read GetCanHaveChildren;
        property this[aIndex: integer]: TDisplayComponentSetting read GetSetting; default;
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    DisplayComponentIntf;

{ TDisplayComponentSetting }

constructor TDisplayComponentSetting.Create(const aSettingName: string; const aDescription: string;
    const aDefaultValue: string);
begin
    inherited Create();
    fPossibleValues := TList<string>.Create();
    fSettingName := aSettingName;
    fDescription := aDescription;
    fDefaultValue := aDefaultValue;
    fValue := fDefaultValue;
    fSkipRead := false;
end;

destructor TDisplayComponentSetting.Destroy();
begin
    FreeAndNil(fPossibleValues);
    inherited;
end;

function TDisplayComponentSetting.GetValue: string;
begin
    result := fValue;
end;

function TDisplayComponentSetting.GetSkipWrite(): boolean;
begin
    result := fValue = fDefaultValue
end;

class function TDisplayComponentSetting.ItemReadFromRecs(const aRecs: TArray<TDisplayComponentRec>;
    const aSettingName: string; const aDefault: string): string;
var
    x: integer;
begin
    for x := 0 to high(aRecs) do
    begin
        if (aRecs[x].Ident = aSettingName) then
            EXIT(aRecs[x].Value);
    end;
    EXIT(aDefault);
end;

class procedure TDisplayComponentSetting.ItemWriteToList(aList: TList<TDisplayComponentRec>;
    const aSettingName, aValue: string; aDelete: boolean);
begin
    if aDelete then
        ItemDeleteFromList(aList, aSettingName)
    else
        ItemAddToList(aList, aSettingName, aValue);
end;

class procedure TDisplayComponentSetting.ItemAddToList(aList: TList<TDisplayComponentRec>;
    const aSettingName: string; const aValue: string);
var
    xRec: TDisplayComponentRec;
begin
    // keine doppelten Einträge:
    ItemDeleteFromList(aList, aSettingName);

    // neu hinzufügen
    xRec.Ident := aSettingName;
    xRec.Value := aValue;
    aList.Add(xRec);
end;

class procedure TDisplayComponentSetting.ItemDeleteFromList(aList: TList<TDisplayComponentRec>;
    const aSettingName: string);
var
    x: integer;
begin
    for x := 0 to aList.Count - 1 do
    begin
        if (aList[x].Ident = aSettingName) then
        begin
            aList.Delete(x);
            EXIT;
        end;
    end;
end;

procedure TDisplayComponentSetting.ReadFromRecs(const aRecs: TArray<TDisplayComponentRec>);
begin
    if not fSkipRead then
        fValue := ItemReadFromRecs(aRecs, fSettingName, fDefaultValue);
end;

procedure TDisplayComponentSetting.WriteToList(aList: TList<TDisplayComponentRec>);
begin
    if SkipWrite then
        ItemDeleteFromList(aList, fSettingName)
    else
        ItemAddToList(aList, fSettingName, fValue);
end;

procedure TDisplayComponentSetting.SetValue(const aValue: string);
begin
    fValue := aValue;
end;

function TDisplayComponentSetting.GetAsInt: integer;
begin
    result := StrToInt(fValue);
end;

function TDisplayComponentSetting.GetAsBool: boolean;
begin
    if fValue = 'YES' then
        result := true
    else
        result := false;
end;

function TDisplayComponentSetting.GetAsStr: string;
begin
    result := fValue;
end;

function TDisplayComponentSetting.GetAsFloat: double;
begin
    result := SysUtils.StrToFloatDef(fValue, 0, TFormatUtils.GetSettingsEnglishUS);
end;

procedure TDisplayComponentSetting.ChangeSettingName(const aNewSetttingName: string);
begin
    fSettingName := aNewSetttingName;
end;

class function TDisplayComponentSetting.Find(aList: TObjectList<TDisplayComponentSetting>;
    const aSettingName: string; aMustFind: boolean): TDisplayComponentSetting;
var
    x: integer;
begin
    for x := 0 to aList.Count - 1 do
    begin
        if SameText(aSettingName, aList[x].SettingName) then
            EXIT(aList[x]);
    end;

    if not aMustFind then
        EXIT(nil);

    raise Exception.CreateFmt('Setting %s not found', [aSettingName])
end;

{ TDisplayComponentSettingList }

constructor TDisplayComponentSettingList.Create(const aComponentName, aTypeName: string);
begin
    inherited Create();
    fComponentName := aComponentName;
    fTypeName := aTypeName;
    fList := TObjectList<TDisplayComponentSetting>.Create();
    AddSettings();
end;

function TDisplayComponentSettingList.GetCanHaveChildren: boolean;
begin
    result := false;
end;

procedure TDisplayComponentSettingList.Add(aSetting: TDisplayComponentSetting);
begin
    fList.Add(aSetting);
end;

function TDisplayComponentSettingList.GetSetting(aIndex: integer): TDisplayComponentSetting;
begin
    result := fList[aIndex];
end;

procedure TDisplayComponentSettingList.AddStr(const aSettingName: string; const aDescription: string;
    const aDefaultValue: string; const aPossibleValues: array of string);
begin
    self.Add(TDSStr.Create(aSettingName, aDescription, aDefaultValue, aPossibleValues));
end;

procedure TDisplayComponentSettingList.AddStr(const aSettingName, aDefaultValue: string);
begin
    AddStr(aSettingName, '', aDefaultValue, []);
end;

procedure TDisplayComponentSettingList.AddInt(const aSettingName: string; const aDescription: string;
    const aDefaultValue: integer; const aPossibleValues: array of integer);
begin
    self.Add(TDSInt.Create(aSettingName, aDescription, aDefaultValue, aPossibleValues));
end;

procedure TDisplayComponentSettingList.AddInt(const aSettingName: string; const aDefaultValue: integer);
begin
    AddInt(aSettingName, '', aDefaultValue, []);
end;

procedure TDisplayComponentSettingList.AddFloat(const aSettingName: string; const aDescription: string;
    const aDefaultValue: double; const aPossibleValues: array of double);
begin
    self.Add(TDSFLoat.Create(aSettingName, aDescription, aDefaultValue, aPossibleValues));
end;

procedure TDisplayComponentSettingList.AddFloat(const aSettingName: string; const aDefaultValue: double);
begin
    AddFloat(aSettingName, '', aDefaultValue, []);
end;

procedure TDisplayComponentSettingList.AddBool(const aSettingName: string; const aDescription: string;
    const aDefaultValue: boolean; const aPossibleValues: array of boolean);
begin
    self.Add(TDSBool.Create(aSettingName, aDescription, aDefaultValue, aPossibleValues));
end;

procedure TDisplayComponentSettingList.AddBool(const aSettingName: string; const aDefaultValue: boolean);
begin
    AddBool(aSettingName, '', aDefaultValue, []);
end;

procedure TDisplayComponentSettingList.AddEvent(const aSettingName: string; const aDescription: string);
begin
    self.Add(TDSEvent.Create(aSettingName, aDescription));
end;

procedure TDisplayComponentSettingList.AddDisplayComponentType(const aDefaultValue: string);
begin
    self.Add(TDSDisplayComponentType.Create(aDefaultValue));
end;

procedure TDisplayComponentSettingList.AddDisplayComponent(const aSettingName, aDescription: string;
    aID: TDisplayComponentID);
begin
    self.Add(TDSDisplayComponent.Create(aSettingName, aDescription, aDescription, aID));
end;

procedure TDisplayComponentSettingList.AddAlign(const aSettingName, aDescription, aDefaultValue: string);
begin
    self.Add(TDSAlign.Create(aSettingName, aDescription, aDefaultValue));
end;

procedure TDisplayComponentSettingList.ReadAll();
var
    x: integer;
    xDA: TDisplayComponentsDataAdaptor;
    xRecs: TArray<TDisplayComponentRec>;
begin
    xDA := TDisplayComponentsDataAdaptor.Create;
    try
        xRecs := xDA.ReadRecsByComponentName(fComponentName);
    finally
        FreeAndNil(xDA);
    end;

    for x := 0 to fList.Count - 1 do
        self[x].ReadFromRecs(xRecs);
end;

procedure TDisplayComponentSettingList.WriteAll();
var
    x: integer;
    xDA: TDisplayComponentsDataAdaptor;
    xList: TList<TDisplayComponentRec>;
begin
    xList := TList<TDisplayComponentRec>.Create;
    try
        for x := 0 to fList.Count - 1 do
            self[x].WriteToList(xList);

        xDA := TDisplayComponentsDataAdaptor.Create;
        try
            xDA.WriteRecsByComponentName(fComponentName, xList.ToArray);
        finally
            FreeAndNil(xDA);
        end;
    finally
        FreeAndNil(xList);
    end;
end;

procedure TDisplayComponentSettingList.DeleteAll();
var
    xDA: TDisplayComponentsDataAdaptor;
begin
    xDA := TDisplayComponentsDataAdaptor.Create;
    try
        xDA.DeleteByComponentName(fComponentName);
    finally
        FreeAndNil(xDA);
    end;
end;

function TDisplayComponentSettingList.GetCount: integer;
begin
    result := fList.Count;
end;

function TDisplayComponentSettingList.Find(const aSettingName: string; aMustFind: boolean = true)
    : TDisplayComponentSetting;
begin
    result := TDisplayComponentSetting.Find(fList, aSettingName, true);
end;

procedure TDisplayComponentSettingList.AddSettings();
begin
    self.AddDisplayComponentType(fTypeName);
end;

procedure TDisplayComponentSettingList.LoadSettings;
begin

end;

procedure TDisplayComponentSettingList.AddFromList(aSettings: TDisplayComponentSettingList);
var
    x: integer;
begin
    for x := 0 to aSettings.Count - 1 do
    begin
        fList.Add(aSettings[x]);
    end;
end;

procedure TDisplayComponentSettingList.Prepare;
begin
    ReadAll();
    LoadSettings();
end;

function TDisplayComponentSettingList.HasOneToManyOption(): boolean;
begin
    result := false;
end;

{ TDSStr }

constructor TDSStr.Create(const aSettingName: string; const aDescription: string; const aDefaultValue: string;
    const aPossibleValues: array of string);
begin
    inherited Create(aSettingName, aDescription, aDefaultValue);
    SetPossibleValues(aPossibleValues);
end;

procedure TDSStr.SetPossibleValues(const aPossibleValues: array of string);
var
    x: integer;
begin
    fPossibleValues.Clear;
    for x := 0 to high(aPossibleValues) do
    begin
        fPossibleValues.Add(aPossibleValues[x]);
    end;
end;

{ TDSInt }

constructor TDSInt.Create(const aSettingName: string; const aDescription: string;
    const aDefaultValue: integer; const aPossibleValues: array of integer);
begin
    inherited Create(aSettingName, aDescription, IntToStr(aDefaultValue));
    SetPossibleValues(aPossibleValues);
end;

procedure TDSInt.SetPossibleValues(const aPossibleValues: array of integer);
var
    x: integer;
begin
    fPossibleValues.Clear;
    for x := 0 to high(aPossibleValues) do
    begin
        fPossibleValues.Add(IntToStr(aPossibleValues[x]));
    end;
end;

{ TDSBool }

function InternBoolToStr(aValue: boolean): string;
begin
    if aValue then
        result := 'YES'
    else
        result := 'NO';
end;

constructor TDSBool.Create(const aSettingName: string; const aDescription: string;
    const aDefaultValue: boolean; const aPossibleValues: array of boolean);
begin
    inherited Create(aSettingName, aDescription, BoolToStr(aDefaultValue));
    SetPossibleValues(aPossibleValues);
end;

procedure TDSBool.SetPossibleValues(const aPossibleValues: array of boolean);
var
    x: integer;
begin
    fPossibleValues.Clear;
    for x := 0 to high(aPossibleValues) do
    begin
        fPossibleValues.Add(BoolToStr(aPossibleValues[x]));
    end;
end;

class function TDSBool.BoolToStr(aValue: boolean): string;
begin
    result := InternBoolToStr(aValue);
end;

{ TDSFloat }

constructor TDSFloat.Create(const aSettingName: string; const aDescription: string;
    const aDefaultValue: double; const aPossibleValues: array of double);
begin
    inherited Create(aSettingName, aDescription, FloatToStr(aDefaultValue,
        TFormatUtils.GetSettingsEnglishUS));
    SetPossibleValues(aPossibleValues);
end;

procedure TDSFloat.SetPossibleValues(const aPossibleValues: array of double);
var
    x: integer;
begin
    fPossibleValues.Clear;
    for x := 0 to high(aPossibleValues) do
    begin
        fPossibleValues.Add(FloatToStr(aPossibleValues[x], TFormatUtils.GetSettingsEnglishUS));
    end;
end;

{ TDSEvent }

constructor TDSEvent.Create(const aSettingName, aDescription: string);
begin
    inherited Create(aSettingName, aDescription, '', []);
end;

{ TDSDisplayComponentType }

constructor TDSDisplayComponentType.Create(const aDefaultValue: string);
begin
    inherited Create(TDisplayComponentsDataAdaptor.cDisplayComponentSettingType, '', aDefaultValue);
    fSkipRead := true;
end;

function TDSDisplayComponentType.GetSkipWrite(): boolean;
begin
    // always write type
    result := false;
end;

{ TDSDisplayComponent }

constructor TDSDisplayComponent.Create(const aSettingName, aDescription, aDefaultValue: string;
    aID: TDisplayComponentID);
begin
    inherited Create(aSettingName, aDescription, aDefaultValue);
    fDisplayComponentID := aID;
end;

{ TDSAlign }

constructor TDSAlign.Create(const aSettingName, aDescription: string; const aDefaultValue: string);
begin
    inherited Create(aSettingName, aDescription, aDefaultValue, [cSettingAlignClient, cSettingAlignNone,
        cSettingAlignTop, cSettingAlignBottom, cSettingAlignLeft, cSettingAlignRight]);
end;

{ TDSMulti }

constructor TDSMulti.Create(const aSettingName: string; const aChildSettingNamePrefix: string);
begin
    inherited Create(aSettingName, '', '');
    fChildSettingNamePrefix := aChildSettingNamePrefix;
    fChildSettings := TObjectList<TDisplayComponentSetting>.Create();
    fListSizeChanged := false;
end;

destructor TDSMulti.Destroy;
begin
    fChildSettings.Free;
    inherited;
end;

function TDSMulti.GetChildSettingName(const aChildNumber: integer): string;
begin
    result := Format('%sItem%d', [fChildSettingNamePrefix, aChildNumber]);
end;

function TDSMulti.GetChildCountSettingName(): string;
begin
    result := Format('%sCount', [fChildSettingNamePrefix]);
end;

function TDSMulti.ReadChildCountSetting(const aRecs: TArray<TDisplayComponentRec>): integer;
begin
    EXIT(StrToInt(ItemReadFromRecs(aRecs, GetChildCountSettingName(), '0')));
end;

procedure TDSMulti.WriteChildCountSetting(aList: TList<TDisplayComponentRec>; const aValue: integer);
begin
    ItemAddToList(aList, GetChildCountSettingName(), IntToStr(aValue));
end;

function TDSMulti.CreateChildSetting(const aSettingName: string): TDisplayComponentSetting;
begin
    result := TDSStr.Create(aSettingName, '', '', []);
end;

procedure TDSMulti.ReadFromRecs(const aRecs: TArray<TDisplayComponentRec>);
var
    x, xChildCount: integer;
    xSettingName: string;
    xChildSetting: TDisplayComponentSetting;
begin
    if fSkipRead then
        EXIT;
    fChildSettings.Clear();

    xChildCount := ReadChildCountSetting(aRecs);
    for x := 1 to xChildCount do
    begin
        xSettingName := GetChildSettingName(x);
        xChildSetting := CreateChildSetting(xSettingName);
        fChildSettings.Add(xChildSetting);
        xChildSetting.ReadFromRecs(aRecs);
    end;
end;

function TDSMulti.GetSkipWrite(): boolean;
var
    x: integer;
begin
    result := not fListSizeChanged;
    if not result then
        EXIT;
    for x := 0 to fChildSettings.Count - 1 do
    begin
        if not fChildSettings[x].SkipWrite then
        begin
            result := false;
            EXIT;
        end;
    end;
end;

procedure TDSMulti.WriteToList(aList: TList<TDisplayComponentRec>);
var
    x: integer;
begin
    if SkipWrite then
        EXIT;

    for x := 0 to fChildSettings.Count - 1 do
    begin
        fChildSettings[x].WriteToList(aList);
    end;

    WriteChildCountSetting(aList, fChildSettings.Count);

    fListSizeChanged := false;
end;

function TDSMulti.AddNewChildSetting: TDisplayComponentSetting;
begin
    result := CreateChildSetting(GetChildSettingName(fChildSettings.Count + 1));
    fChildSettings.Add(result);
    fListSizeChanged := true;
end;

procedure TDSMulti.DeleteChildSetting(const aChildSettingName: string);
var
    xChildSetting: TDisplayComponentSetting;
    x: integer;
begin
    xChildSetting := TDisplayComponentSetting.Find(fChildSettings, aChildSettingName, false);
    if not Assigned(xChildSetting) then
        EXIT;

    fChildSettings.Remove(xChildSetting);
    fListSizeChanged := true;

    for x := 0 to fChildSettings.Count - 1 do
    begin
        fChildSettings[x].ChangeSettingName(self.GetChildSettingName(x + 1));
    end;
end;

{ TDSChildComponents }

constructor TDSChildComponents.Create();
begin
    inherited Create('ChildComponents', 'Child');
end;

function TDSChildComponents.CreateChildSetting(const aSettingName: string): TDisplayComponentSetting;
begin
    result := TDSDisplayComponent.Create(aSettingName, '', '', IDisplayComponent);
end;

function TDSChildComponents.GetAllChildNames: TArray<string>;
var
    x: integer;
begin
    SetLength(result, fChildSettings.Count);
    for x := 0 to fChildSettings.Count - 1 do
    begin
        result[x] := fChildSettings[x].Value;
    end;
end;


end.
