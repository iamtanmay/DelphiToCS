unit StepsToMMUpdate;
{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  11.06.08 wl                               TN4143   fRStepsPerMM auskommentiert
  24.06.08 pk                               TN4148   uses UpdateManagerDataProvider instead of dataprovider
  10.12.10 wl  TSettingsTableUpdateV1_2.Convert  TN5405  neu: DeviceConversion
  13.12.10 wl  TStepsToMMConverter          TN5405   --> DeviceConversion
  31.01.11 wl  TLayoutTableUpdateV2         TN5440   liest und benutzt Korrektur-Werte aus den Settings, die TSettingsTableUpdateV1_2 vorher schreibt
  15.02.11 pk                               TN4780   changes needed to make UpdateManager compatible with TurboDB
  21.02.11 wl                               TN5455   an DeviceConversion angepasst
  ---------------------------------------------------------------------------------------------------------------------- }


interface


uses
    Generics.Collections,
    Types,
    Update,
    TableUpdate,
    DataProvider,
    DeviceConversion;

type
    TLayoutTableUpdateV2 = class(TTableUpdate)
    private
        fStepsToMMConv: TStepsToMMConverter;
        procedure ConvertAny(aType: integer; const aSourceVal: variant; var vDestVal: variant);
        procedure ConvertX(const aSourceVal: variant; var vDestVal: variant);
        procedure ConvertY(const aSourceVal: variant; var vDestVal: variant);
        procedure ConvertZ(const aSourceVal: variant; var vDestVal: variant);
        procedure ConvertCarrierYData(aSender: TObject);
        function GetGlobalZTravel_mm(): double;
        function GetXOffsetLeftToTip1_mm(): double;
        function GetYOffsetFrontToTip1_mm(): double;
    public
        constructor Create(aUpdateID: TUpdateID);
        destructor Destroy; override;
    end;

    TRacksTableUpdateV2 = class(TTableUpdate)
    private
        fStepsToMMConv: TStepsToMMConverter;
        procedure Convert(const aSourceVal: variant; var vDestVal: variant);
    public
        constructor Create(aUpdateID: TUpdateID);
    end;

    TSettingsTableUpdateV1_2 = class(TTableUpdate)
    private
        fStepsToMMConv: TStepsToMMConverter;
        fSettingDP: TDataProvider;
        procedure Convert(aSender: TObject);

        // StepsToMM Update
        procedure ConvertValue(const aSection, aIdent: string; aType: integer);
        procedure ConvertTakeTubesValue(const aIdent: string; aType: integer);
        procedure ConvertTubeToolEntry(const aIdent: string);
        procedure ConvertTubeToolEntries;
        procedure ConvertAllTubeTool;
    public
        constructor Create(aUpdateID: TUpdateID);
        destructor Destroy(); override;
    end;


implementation


uses
    SysUtils,
    Variants,
    Classes,
    Math,
    StrUtils,

    LayoutTableUpdate,
    RackTableUpdate,
    SettingsTableUpdate,
    TableUpdateItem,
    TableUpdateDataMap;

{ TLayoutTableUpdateV2 }

constructor TLayoutTableUpdateV2.Create(aUpdateID: TUpdateID);
begin
    inherited Create(aUpdateID, TLayoutTableStructDefV1, INT_LAYOUT_MAJORREVISION_2);
    AlterStructure(TLayoutTableStructDefV2);
    CopyMatchingFields([]);
    self.CopyFieldWithFunc('CARR_X_STEPS', 'CARR_X', ConvertX);
    self.CopyFieldWithFunc('CARR_Y_STEPS', 'CARR_Y', ConvertY);
    self.CopyFieldWithFunc('CARR_Z_STEPS', 'CARR_Z', ConvertZ);
    self.CustomDataFunc(ConvertCarrierYData);
    fStepsToMMConv := TStepsToMMConverter.Create();
end;

destructor TLayoutTableUpdateV2.Destroy();
begin
    fStepsToMMConv.Free;
    inherited;
end;

function TLayoutTableUpdateV2.GetXOffsetLeftToTip1_mm(): double;
var
    xSourceDP: TDataProvider;
begin
    xSourceDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        // neuer Wert, der für das Layout-Update V2 benötigt wird
        xSourceDP.SelectAndOpen('select * from settings where settings."AREA" = ''ROBOT''' +
            ' and settings."SECTION" = ''TempConversion'' and settings."IDENT" = ''XOffsetLeftToTip1_mm''',
            true);
        result := StrToFloatDef(xSourceDP.FieldByName('VALUE').AsString, 0);
        xSourceDP.Close;
    finally
        xSourceDP.Free;
    end;
end;

function TLayoutTableUpdateV2.GetYOffsetFrontToTip1_mm(): double;
var
    xSourceDP: TDataProvider;
begin
    xSourceDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        // neuer Wert, der für das Layout-Update V2 benötigt wird
        xSourceDP.SelectAndOpen('select * from settings where settings."AREA" = ''ROBOT''' +
            ' and settings."SECTION" = ''TempConversion'' and settings."IDENT" = ''YOffsetFrontToTip1_mm''',
            true);
        result := StrToFloatDef(xSourceDP.FieldByName('VALUE').AsString, 0);
        xSourceDP.Close;
    finally
        xSourceDP.Free;
    end;
end;

function TLayoutTableUpdateV2.GetGlobalZTravel_mm(): double;
var
    xSourceDP: TDataProvider;
begin
    xSourceDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        // neuer Wert, der für das Layout-Update V2 benötigt wird
        xSourceDP.SelectAndOpen('select * from settings where settings."AREA" = ''ROBOT''' +
            ' and settings."SECTION" = ''TempConversion'' and settings."IDENT" = ''GlobalZTravel_mm''', true);
        result := StrToFloatDef(xSourceDP.FieldByName('VALUE').AsString, 0);
        xSourceDP.Close;
    finally
        xSourceDP.Free;
    end;
end;

procedure TLayoutTableUpdateV2.ConvertCarrierYData(aSender: TObject);
var
    xDestDP: TDataProvider;
    xGlobalZTravel_mm: double;
    xXOffsetLeftToTip1_mm: double;
    xYOffsetFrontToTip1_mm: double;
begin
    xXOffsetLeftToTip1_mm := GetXOffsetLeftToTip1_mm();
    xYOffsetFrontToTip1_mm := GetYOffsetFrontToTip1_mm();
    xGlobalZTravel_mm := GetGlobalZTravel_mm();
    xDestDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDestDP.SelectAndOpen('SELECT * FROM Layout where CARRIER is not null', false);
        while not xDestDP.Eof do
        begin
            xDestDP.Edit;
            xDestDP.FieldByName('CARR_Y').AsFloat := xYOffsetFrontToTip1_mm -
                xDestDP.FieldByName('CARR_Y').AsFloat;
            xDestDP.FieldByName('CARR_X').AsFloat := xXOffsetLeftToTip1_mm +
                xDestDP.FieldByName('CARR_X').AsFloat;
            xDestDP.FieldByName('CARR_Z').AsFloat := xGlobalZTravel_mm -
                xDestDP.FieldByName('CARR_Z').AsFloat;
            xDestDP.Post;

            xDestDP.Next;
        end;
        xDestDP.Close;
    finally
        xDestDP.Free;
    end;
end;

procedure TLayoutTableUpdateV2.ConvertAny(aType: integer; const aSourceVal: variant; var vDestVal: variant);
var
    xSteps: integer;
begin
    fStepsToMMConv.ReadXYZSettings();

    if aSourceVal = null then
        EXIT;
    xSteps := aSourceVal;
    vDestVal := 0;

    if xSteps = 0 then
        EXIT;

    vDestVal := fStepsToMMConv.StepsToMMByType(aType, xSteps);
end;

procedure TLayoutTableUpdateV2.ConvertX(const aSourceVal: variant; var vDestVal: variant);
begin
    ConvertAny(TStepsToMMConverter.INT_CONV_X, aSourceVal, vDestVal);
end;

procedure TLayoutTableUpdateV2.ConvertY(const aSourceVal: variant; var vDestVal: variant);
begin
    ConvertAny(TStepsToMMConverter.INT_CONV_Y, aSourceVal, vDestVal);
end;

procedure TLayoutTableUpdateV2.ConvertZ(const aSourceVal: variant; var vDestVal: variant);
begin
    ConvertAny(TStepsToMMConverter.INT_CONV_Z, aSourceVal, vDestVal);
end;

{ TRacksTableUpdateV2 }

constructor TRacksTableUpdateV2.Create(aUpdateID: TUpdateID);
begin
    inherited Create(aUpdateID, TRacksTableStructDefV1, INT_RACKS_MAJORREVISION_2);
    AlterStructure(TRacksTableStructDefV2);
    CopyMatchingFields([]);
    self.CopyFieldWithFunc('H_VOpen', 'H_VOpen_mm', Convert);
    self.CopyFieldWithFunc('H_VClose', 'H_VClose_mm', Convert);
    self.CopyFieldWithFunc('TubeGetOpen_Steps', 'TubeGetOpen_mm', Convert);
    self.CopyFieldWithFunc('TubeGetClose_Steps', 'TubeGetClose_mm', Convert);
    self.CopyFieldWithFunc('TubePutOpen_Steps', 'TubePutOpen_mm', Convert);
    fStepsToMMConv := TStepsToMMConverter.Create();
end;

procedure TRacksTableUpdateV2.Convert(const aSourceVal: variant; var vDestVal: variant);
var
    xSteps: integer;
begin
    fStepsToMMConv.ReadGSettings;

    if aSourceVal = null then
        EXIT;
    xSteps := aSourceVal;
    vDestVal := 0;

    if xSteps = 0 then
        EXIT;

    vDestVal := fStepsToMMConv.GStepsToMM(xSteps);

end;

constructor TSettingsTableUpdateV1_2.Create(aUpdateID: TUpdateID);
begin
    inherited Create(aUpdateID, TSettingsTableStructDefV1, INT_SETTINGS_MAJORREVISION_1,
        INT_SETTINGS_MINORREVISION_2);
    self.AlterStructure(TSettingsTableStructDefV1);
    CopyMatchingFields([]);
    self.CustomDataFunc(Convert);
    fStepsToMMConv := TStepsToMMConverter.Create();
end;

destructor TSettingsTableUpdateV1_2.Destroy();
begin
    fStepsToMMConv.Free;
    inherited;
end;

procedure TSettingsTableUpdateV1_2.ConvertValue(const aSection, aIdent: string; aType: integer);
var
    xValueInSteps: integer;
    xValue: double;
begin
    fSettingDP.SelectAndOpen('SELECT * FROM Settings WHERE ' + TSettingsSQL.AreaEq('Settings', 'ROBOT') +
        ' AND ' + TSettingsSQL.SectionEq('Settings', aSection) + ' AND ' + TSettingsSQL.IdentEq('Settings',
        aIdent), false);
    try

        if fSettingDP.IsEmpty then
            EXIT;
        xValueInSteps := StrToIntDef(fSettingDP.FieldByName('Value').AsString, 0);

        xValue := RoundTo(fStepsToMMConv.StepsToMMByType(aType, xValueInSteps), -4);
        fSettingDP.Edit;
        fSettingDP.FieldByName('Value').AsString := FloatToStr(xValue);
        fSettingDP.Post;
    finally
        fSettingDP.Close;
    end;

end;

procedure TSettingsTableUpdateV1_2.ConvertTakeTubesValue(const aIdent: string; aType: integer);
begin
    ConvertValue('TakeTubes', aIdent, aType);
end;

procedure TSettingsTableUpdateV1_2.ConvertTubeToolEntry(const aIdent: string);
const
    INT_INDEX_XOFFSET = 0;
    INT_INDEX_YOFFSET = 1;
    INT_INDEX_ZOFFSET = 2;
    INT_INDEX_ZDROP = 3;
    INT_INDEX_VOPEN = 4;
    INT_INDEX_VDROP = 5;
    INT_INDEX_TUBEDY = 6;
    INT_INDEX_SAVEMOVEOFFSET = 7;
    INT_INDEX_SHAKEHEIGHT = 9;
    INT_INDEX_SHAKEZOFFSET = 10;
    INT_INDEX_SHAKEYOFFSET = 11;
    INT_INDEX_BRINGBACKVOFFSET = 12;
var
    xValueAsCommaText: string;
    xList: TStringList;

    procedure ConvSubEntry(aIndex: integer; aType: integer);
    begin
        if aIndex >= xList.Count then
            EXIT;
        xList[aIndex] := FloatToStr(RoundTo(fStepsToMMConv.StepsToMMByType(aType, StrToIntDef(xList[aIndex],
            0)), -4));
    end;

begin
    fSettingDP.SelectAndOpen('SELECT * FROM Settings WHERE ' + TSettingsSQL.AreaEq('Settings', 'ROBOT') +
        ' AND ' + TSettingsSQL.SectionEq('Settings', 'TubeToolData') + ' AND ' +
        TSettingsSQL.IdentEq('Settings', aIdent), false);
    try
        if fSettingDP.IsEmpty then
            EXIT;

        xValueAsCommaText := fSettingDP.FieldByName('Value').AsString;
        xList := TStringList.Create;
        try
            // the commatext function will automatically parse the comma separted TubeTool Entry
            xList.CommaText := xValueAsCommaText;

            ConvSubEntry(INT_INDEX_XOFFSET, TStepsToMMConverter.INT_CONV_X);
            ConvSubEntry(INT_INDEX_YOFFSET, TStepsToMMConverter.INT_CONV_Y);
            ConvSubEntry(INT_INDEX_ZOFFSET, TStepsToMMConverter.INT_CONV_HZ);
            ConvSubEntry(INT_INDEX_ZDROP, TStepsToMMConverter.INT_CONV_HZ);
            ConvSubEntry(INT_INDEX_VOPEN, TStepsToMMConverter.INT_CONV_G);
            ConvSubEntry(INT_INDEX_VDROP, TStepsToMMConverter.INT_CONV_G);
            ConvSubEntry(INT_INDEX_TUBEDY, TStepsToMMConverter.INT_CONV_G);
            ConvSubEntry(INT_INDEX_SAVEMOVEOFFSET, TStepsToMMConverter.INT_CONV_HZ);
            ConvSubEntry(INT_INDEX_SHAKEHEIGHT, TStepsToMMConverter.INT_CONV_HZ);
            ConvSubEntry(INT_INDEX_SHAKEZOFFSET, TStepsToMMConverter.INT_CONV_HZ);
            ConvSubEntry(INT_INDEX_SHAKEYOFFSET, TStepsToMMConverter.INT_CONV_Y);
            ConvSubEntry(INT_INDEX_BRINGBACKVOFFSET, TStepsToMMConverter.INT_CONV_G);

            fSettingDP.Edit;
            fSettingDP.FieldByName('Value').AsString := xList.CommaText;
            fSettingDP.Post;

        finally
            xList.Free;
        end;
    finally
        fSettingDP.Close();
    end;

end;

procedure TSettingsTableUpdateV1_2.ConvertTubeToolEntries();
var
    xList: TStringList;
    x: integer;
begin
    xList := TStringList.Create();
    try
        fSettingDP.SelectAndOpen('SELECT * FROM Settings WHERE ' + TSettingsSQL.AreaEq('Settings', 'ROBOT') +
            ' AND ' + TSettingsSQL.SectionEq('Settings', 'TubeToolData'), true);
        try
            while not fSettingDP.Eof do
            begin
                xList.Add(fSettingDP.FieldByName('Ident').AsString);
                fSettingDP.Next();
            end;
        finally
            fSettingDP.Close();
        end;

        for x := 0 to xList.Count - 1 do
        begin
            ConvertTubeToolEntry(xList[x]);
        end;
    finally
        xList.Free;
    end;
end;

procedure TSettingsTableUpdateV1_2.ConvertAllTubeTool();
begin
    ConvertTakeTubesValue('XOffset', TStepsToMMConverter.INT_CONV_X);
    ConvertTakeTubesValue('YOffset', TStepsToMMConverter.INT_CONV_Y);
    ConvertTakeTubesValue('ZOffset', TStepsToMMConverter.INT_CONV_HZ);
    ConvertTakeTubesValue('ZDrop', TStepsToMMConverter.INT_CONV_HZ);
    ConvertTakeTubesValue('VOpen', TStepsToMMConverter.INT_CONV_G);
    ConvertTakeTubesValue('VDrop', TStepsToMMConverter.INT_CONV_G);
    ConvertTakeTubesValue('TubeDY', TStepsToMMConverter.INT_CONV_G);
    ConvertTakeTubesValue('SaveMoveOffset', TStepsToMMConverter.INT_CONV_HZ);
    ConvertTakeTubesValue('ShakeHeight', TStepsToMMConverter.INT_CONV_HZ);
    ConvertTakeTubesValue('ShakeZOffset', TStepsToMMConverter.INT_CONV_HZ);
    ConvertTakeTubesValue('ShakeYOffset', TStepsToMMConverter.INT_CONV_Y);
    ConvertTakeTubesValue('BringBackVOffset', TStepsToMMConverter.INT_CONV_G);

    ConvertTubeToolEntries();
end;

procedure TSettingsTableUpdateV1_2.Convert(aSender: TObject);
var
    xDeviceConversion: TDeviceConversion;
    xSourceDP: TDataProvider;
begin
    fStepsToMMConv.ReadXYZSettings;
    fStepsToMMConv.ReadGSettings;

    fSettingDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        // Neu: Alle Devices von V 7.1.7 oder weniger konvertieren
        xSourceDP := fTableChangeAdaptor.CreateSourceDataProvider();
        try
            xDeviceConversion := TDeviceConversion.Create(xSourceDP, fSettingDP, fStepsToMMConv);
            try
                xDeviceConversion.Convert;
            finally
                FreeAndNil(xDeviceConversion);
            end;

        finally
            FreeAndNil(xSourceDP);
        end;

        // Bisheriges Update: Steps to mm
        ConvertAllTubeTool();
    finally
        fSettingDP.Free;
    end;
end;


end.
