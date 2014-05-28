{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  07.02.08 wl  TModuleSetting.DefaultValue  TN4003   kann jetzt auch beschrieben werden
  18.02.08 wl  TModuleSetting.GetAsFloat    TN4009   benutzt TValueConverter, konvertiert also von . und ,
  06.05.08 wl  TModuleSettingList           TN4068   Settings als default-Property
  26.05.08 wl  TModuleSettingList.Create    TN4119   Parameters changed
  13.10.08 pk  TModuleSettingList           TN4272.2 DataAdaptor replaced by DataCache
  17.12.08 pk                               TN4243   Various changes
  28.01.09 wl  TModuleSetting.Description   TN4243   Description als string statt DescriptionResNo
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  15.11.10 pk                               TN5340    Changes to prevent memory leak
  03.05.11 wl  TMSBool.SetPossibleValues    TN5566   entfernt (Was soll das denn? Ja, Nein, Vielleicht?)
  03.05.11 wl  AddInt,AddFloat              TN5566   auch ohne PossibleValues möglich
  29.03.12 wl  AddStr                       TN5845   auch ohne PossibleValues möglich
  27.03.13 wl                               TN6045   uses geändert
  ---------------------------------------------------------------------------------------------------------------------- }

unit ModuleSettings;


interface


uses
    Generics.Collections,
    SysUtils,
    CommonTypes,
    GeneralTypes,
    ModuleDataCache;

type
    TModuleID = TGUID;

    TOnEditModuleSetting = procedure(aSender: TObject) of object;

    TModuleSetting = class
    protected
        fOwner: TObject;
        fValue: string;
        fDefaultValue: string;
        fSettingName: string;
        fDescription: string;
        fSectionName: string;
        fDataCache: TModuleDataCache;
        fPossibleValues: TList<string>;
        fOnEditValue: TOnEditModuleSetting;
        procedure SetValue(const aValue: string); virtual;
        function GetValue: string; virtual;
        function SkipRead(): boolean; virtual;
        function SkipWrite(): boolean; virtual;
        function GetAsInt(): integer;
        function GetAsBool(): boolean;
        function GetAsFloat: double;
        function GetAsStr: string;
    public
        constructor Create(const aSettingName: string; const aDescription: string;
            const aDefaultValue: string);
        destructor Destroy(); override;

        procedure read();
        procedure write();
        property SettingName: string read fSettingName;
        property Value: string read GetValue write SetValue;
        property DefaultValue: string read fDefaultValue write fDefaultValue;
        property Description: string read fDescription write fDescription;
        property OnEditValue: TOnEditModuleSetting read fOnEditValue write fOnEditValue;
        property PossibleValues: TList<string>read fPossibleValues;
        property SectionName: string read fSectionName write fSectionName;
        property DataAdaptor: TModuleDataCache read fDataCache write fDataCache;
        property AsInt: integer read GetAsInt;
        property AsBool: boolean read GetAsBool;
        property AsFloat: double read GetAsFloat;
        property AsStr: string read GetAsStr;
        property Owner: TObject read fOwner write fOwner;
    end;

    TMSStr = class(TModuleSetting)
    private
        procedure SetPossibleValues(const aPossibleValues: array of string);
    public
        constructor Create(const aSettingName: string; const aDescription: string;
            const aDefaultValue: string; const aPossibleValues: array of string);
    end;

    TMSInt = class(TModuleSetting)
    private
        procedure SetPossibleValues(const aPossibleValues: array of integer);
    public
        constructor Create(const aSettingName: string; const aDescription: string;
            const aDefaultValue: integer; const aPossibleValues: array of integer);
    end;

    TMSFloat = class(TModuleSetting)
    private
        procedure SetPossibleValues(const aPossibleValues: array of double);
    public
        constructor Create(const aSettingName: string; const aDescription: string;
            const aDefaultValue: double; const aPossibleValues: array of double);
    end;

    TMSBool = class(TModuleSetting)
    public
        constructor Create(const aSettingName: string; const aDescription: string;
            const aDefaultValue: boolean);
        class function BoolToStr(aValue: boolean): string;
    end;

    TMSPort = class(TMSInt)
    public
        constructor Create(const aDescription: string; const aDefaultValue: integer;
            const aPossibleValues: array of integer);
    end;

    TMSModuleType = class(TModuleSetting)
    protected
        function SkipRead(): boolean; override;
        function SkipWrite(): boolean; override;
    public
        constructor Create(const aDefaultValue: string);
    end;

    TMSModule = class(TModuleSetting)
    private
        fModuleID: TModuleID;
    public
        constructor Create(const aSettingName: string; const aDescription: string;
            const aDefaultValue: string; aID: TModuleID);
        property ModuleID: TModuleID read fModuleID;
    end;

    TModuleSettingsClass = class of TModuleSettingList;

    TModuleSettingList = class
    protected
        fOwnsSettings: boolean;
        fList: TObjectList<TModuleSetting>;
        fDataCache: TModuleDataCache;
        fSectionName: string;
        fTypeName: string;
        function GetSetting(aIndex: integer): TModuleSetting;
        function GetCount: integer;
        function CreateDataCache(): TModuleDataCache; virtual; abstract;
        procedure Add_Intern(aSetting: TModuleSetting);
        procedure AddSettings(); virtual;
        procedure LoadSettings(); virtual;
    public
        constructor Create(const aArea, aSection, aTypeName: string); virtual;
        destructor Destroy(); override;
        procedure Add(aSetting: TModuleSetting);
        procedure AddFromList(aSettings: TModuleSettingList);
        procedure AddStr(const aSettingName: string; const aDefaultValue: string); overload;
        procedure AddStr(const aSettingName: string; const aDescription: string;
            const aDefaultValue: string); overload;
        procedure AddStr(const aSettingName: string; const aDescription: string; const aDefaultValue: string;
            const aPossibleValues: array of string); overload;

        procedure AddFloat(const aSettingName: string; const aDefaultValue: double); overload;
        procedure AddFloat(const aSettingName: string; const aDescription: string;
            const aDefaultValue: double); overload;
        procedure AddFloat(const aSettingName: string; const aDescription: string;
            const aDefaultValue: double; const aPossibleValues: array of double); overload;

        procedure AddInt(const aSettingName: string; const aDefaultValue: integer); overload;
        procedure AddInt(const aSettingName: string; const aDescription: string;
            const aDefaultValue: integer); overload;
        procedure AddInt(const aSettingName: string; const aDescription: string; const aDefaultValue: integer;
            const aPossibleValues: array of integer); overload;

        procedure AddBool(const aSettingName: string; const aDefaultValue: boolean); overload;
        procedure AddBool(const aSettingName: string; const aDescription: string;
            const aDefaultValue: boolean); overload;

        procedure AddPort(const aDescription: string; const aDefaultValue: integer;
            const aPossibleValues: array of integer); overload;
        procedure AddPort(const aDefaultValue: integer); overload;

        procedure AddModuleType(const aDefaultValue: string); virtual;

        function Find(const aSettingName: string): TModuleSetting;
        procedure ReadAll();
        procedure WriteAll();
        procedure DeleteAll();
        procedure WriteFromCache();
        procedure Prepare();
        function HasOneToManyOption(): boolean; virtual;
        //
        property DataCache: TModuleDataCache read fDataCache write fDataCache;
        property Count: integer read GetCount;
        property Settings[aIndex: integer]: TModuleSetting read GetSetting; default;
    end;


implementation


uses
    UtilLib;

{ TModuleSetting }

constructor TModuleSetting.Create(const aSettingName: string; const aDescription: string;
    const aDefaultValue: string);
begin
    inherited Create();
    fPossibleValues := TList<string>.Create();
    fSettingName := aSettingName;
    fDescription := aDescription;
    fDefaultValue := aDefaultValue;
    fValue := fDefaultValue;
end;

destructor TModuleSetting.Destroy();
begin
    FreeAndNil(fPossibleValues);
    inherited;
end;

function TModuleSetting.GetValue: string;
begin
    result := fValue;
end;

function TModuleSetting.SkipRead(): boolean;
begin
    result := false;
end;

function TModuleSetting.SkipWrite(): boolean;
begin
    result := fValue = fDefaultValue
end;

procedure TModuleSetting.Read;
begin
    if not Assigned(fDataCache) then
        EXIT;
    if SkipRead then
        EXIT;
    fValue := fDataCache.ReadSetting(fSettingName, fDefaultValue);
end;

procedure TModuleSetting.Write;
begin
    if not Assigned(fDataCache) then
        EXIT;
    if SkipWrite then
    begin
        fDataCache.DeleteSetting(fSettingName);
        EXIT;
    end;
    fDataCache.WriteSetting(fSettingName, fValue);
end;

procedure TModuleSetting.SetValue(const aValue: string);
begin
    fValue := aValue;
end;

function TModuleSetting.GetAsInt: integer;
begin
    result := StrToInt(fValue);
end;

function TModuleSetting.GetAsBool: boolean;
begin
    if fValue = 'YES' then
        result := true
    else
        result := false;
end;

function TModuleSetting.GetAsStr: string;
begin
    result := fValue;
end;

function TModuleSetting.GetAsFloat: double;
begin
    result := TValueConverter.StrToFloatDefTryBoth(fValue, 0);
end;

{ TModuleSettingList }

constructor TModuleSettingList.Create(const aArea, aSection, aTypeName: string);
begin
    inherited Create();
    fOwnsSettings := true;
    fSectionName := aSection;
    fTypeName := aTypeName;
    fList := TObjectList<TModuleSetting>.Create;
    fDataCache := CreateDataCache();
    AddSettings();
end;

destructor TModuleSettingList.Destroy();
begin
    FreeAndNil(fDataCache);
    FreeAndNil(fList);
    inherited;
end;

procedure TModuleSettingList.Add_Intern(aSetting: TModuleSetting);
begin
    fList.Add(aSetting);
end;

procedure TModuleSettingList.Add(aSetting: TModuleSetting);
begin
    aSetting.DataAdaptor := fDataCache;
    aSetting.SectionName := fSectionName;
    Add_Intern(aSetting);
end;

function TModuleSettingList.GetSetting(aIndex: integer): TModuleSetting;
begin
    result := fList[aIndex];
end;

procedure TModuleSettingList.AddStr(const aSettingName: string; const aDescription: string;
    const aDefaultValue: string; const aPossibleValues: array of string);
begin
    self.Add(TMSStr.Create(aSettingName, aDescription, aDefaultValue, aPossibleValues));
end;

procedure TModuleSettingList.AddStr(const aSettingName, aDescription, aDefaultValue: string);
begin
    AddStr(aSettingName, aDescription, aDefaultValue, []);
end;

procedure TModuleSettingList.AddStr(const aSettingName, aDefaultValue: string);
begin
    AddStr(aSettingName, '', aDefaultValue, []);
end;

procedure TModuleSettingList.AddInt(const aSettingName: string; const aDescription: string;
    const aDefaultValue: integer; const aPossibleValues: array of integer);
begin
    self.Add(TMSInt.Create(aSettingName, aDescription, aDefaultValue, aPossibleValues));
end;

procedure TModuleSettingList.AddInt(const aSettingName: string; const aDescription: string;
    const aDefaultValue: integer);
begin
    AddInt(aSettingName, aDescription, aDefaultValue, []);
end;

procedure TModuleSettingList.AddInt(const aSettingName: string; const aDefaultValue: integer);
begin
    AddInt(aSettingName, '', aDefaultValue, []);
end;

procedure TModuleSettingList.AddFloat(const aSettingName: string; const aDescription: string;
    const aDefaultValue: double; const aPossibleValues: array of double);
begin
    self.Add(TMSFLoat.Create(aSettingName, aDescription, aDefaultValue, aPossibleValues));
end;

procedure TModuleSettingList.AddFloat(const aSettingName: string; const aDescription: string;
    const aDefaultValue: double);
begin
    AddFloat(aSettingName, aDescription, aDefaultValue, []);
end;

procedure TModuleSettingList.AddFloat(const aSettingName: string; const aDefaultValue: double);
begin
    AddFloat(aSettingName, '', aDefaultValue, []);
end;

procedure TModuleSettingList.AddBool(const aSettingName: string; const aDescription: string;
    const aDefaultValue: boolean);
begin
    self.Add(TMSBool.Create(aSettingName, aDescription, aDefaultValue));
end;

procedure TModuleSettingList.AddBool(const aSettingName: string; const aDefaultValue: boolean);
begin
    AddBool(aSettingName, '', aDefaultValue);
end;

procedure TModuleSettingList.AddPort(const aDescription: string; const aDefaultValue: integer;
    const aPossibleValues: array of integer);
begin
    self.Add(TMSPort.Create(aDescription, aDefaultValue, aPossibleValues));
end;

procedure TModuleSettingList.AddPort(const aDefaultValue: integer);
begin
    self.AddPort('', aDefaultValue, []);
end;

procedure TModuleSettingList.AddModuleType(const aDefaultValue: string);
begin
    self.Add(TMSModuleType.Create(aDefaultValue));
end;

procedure TModuleSettingList.ReadAll();
var
    xSetting: TModuleSetting;
begin
    for xSetting in fList do
        xSetting.Read;
end;

procedure TModuleSettingList.WriteAll();
var
    xSetting: TModuleSetting;
begin
    for xSetting in fList do
        xSetting.Write;
end;

procedure TModuleSettingList.DeleteAll();
begin
    fDataCache.DeleteAllSettings();
end;

procedure TModuleSettingList.WriteFromCache();
begin
    fDataCache.WriteFromCache(fSectionName);
end;

function TModuleSettingList.GetCount: integer;
begin
    result := fList.Count;
end;

function TModuleSettingList.Find(const aSettingName: string): TModuleSetting;
var
    xSetting: TModuleSetting;
begin
    for xSetting in fList do
    begin
        if SameText(aSettingName, xSetting.SettingName) then
        begin
            result := xSetting;
            EXIT;
        end;
    end;
    raise Exception.CreateFmt('Setting %s not found', [aSettingName]);
end;

procedure TModuleSettingList.AddSettings();
begin
    self.AddModuleType(fTypeName);
end;

procedure TModuleSettingList.LoadSettings;
begin

end;

procedure TModuleSettingList.AddFromList(aSettings: TModuleSettingList);
var
    x: integer;
begin
    for x := 0 to aSettings.Count - 1 do
    begin
        self.Add_Intern(aSettings.Settings[x]);
    end;
end;

procedure TModuleSettingList.Prepare;
begin
    ReadAll();
    LoadSettings();
end;

function TModuleSettingList.HasOneToManyOption(): boolean;
begin
    result := false;
end;

{ TMSStr }
constructor TMSStr.Create(const aSettingName: string; const aDescription: string; const aDefaultValue: string;
    const aPossibleValues: array of string);
begin
    inherited Create(aSettingName, aDescription, aDefaultValue);
    SetPossibleValues(aPossibleValues);
end;

procedure TMSStr.SetPossibleValues(const aPossibleValues: array of string);
var
    x: integer;
begin
    fPossibleValues.Clear;
    for x := 0 to high(aPossibleValues) do
    begin
        fPossibleValues.Add(aPossibleValues[x]);
    end;
end;

{ TMSInt }

constructor TMSInt.Create(const aSettingName: string; const aDescription: string;
    const aDefaultValue: integer; const aPossibleValues: array of integer);
begin
    inherited Create(aSettingName, aDescription, IntToStr(aDefaultValue));
    SetPossibleValues(aPossibleValues);
end;

procedure TMSInt.SetPossibleValues(const aPossibleValues: array of integer);
var
    x: integer;
begin
    fPossibleValues.Clear;
    for x := 0 to high(aPossibleValues) do
    begin
        fPossibleValues.Add(IntToStr(aPossibleValues[x]));

    end;
end;

{ TMSPort }

constructor TMSPort.Create(const aDescription: string; const aDefaultValue: integer;
    const aPossibleValues: array of integer);
begin
    inherited Create('Port', aDescription, aDefaultValue, aPossibleValues);
end;

{ TMSBool }

function InternBoolToStr(aValue: boolean): string;
begin
    if aValue then
        result := 'YES'
    else
        result := 'NO';
end;

constructor TMSBool.Create(const aSettingName: string; const aDescription: string;
    const aDefaultValue: boolean);
begin
    inherited Create(aSettingName, aDescription, BoolToStr(aDefaultValue));
end;

class function TMSBool.BoolToStr(aValue: boolean): string;
begin
    result := InternBoolToStr(aValue);
end;

{ TMSFloat }

constructor TMSFloat.Create(const aSettingName: string; const aDescription: string;
    const aDefaultValue: double; const aPossibleValues: array of double);
begin
    inherited Create(aSettingName, aDescription, FloatToStr(aDefaultValue));
    SetPossibleValues(aPossibleValues);
end;

procedure TMSFloat.SetPossibleValues(const aPossibleValues: array of double);
var
    x: integer;
begin
    fPossibleValues.Clear;
    for x := 0 to high(aPossibleValues) do
    begin
        fPossibleValues.Add(FloatToStr(aPossibleValues[x]));
    end;
end;

{ TMSModuleType }

constructor TMSModuleType.Create(const aDefaultValue: string);
begin
    inherited Create('Type', '', aDefaultValue);
end;

function TMSModuleType.SkipRead: boolean;
begin
    // never read type
    result := true;
end;

function TMSModuleType.SkipWrite(): boolean;
begin
    // always write type
    result := false;
end;

{ TMSModule }

constructor TMSModule.Create(const aSettingName, aDescription, aDefaultValue: string; aID: TModuleID);
begin
    inherited Create(aSettingName, aDescription, aDefaultValue);
    fModuleID := aID;
end;


end.
