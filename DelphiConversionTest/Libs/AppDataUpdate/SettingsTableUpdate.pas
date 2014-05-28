{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.06.08 pk                               TN4148   FieldNames unit removed
  10.07.08 wl  TSettingsTablePipDeviceLoader          TN4157    Reads all Pip Devices from the settings
  20.01.09 wl  INT_SETTINGS_MINORREVISION_3           TN4358   TSettingsTableUpdateV1_3 für ZP04/LinearMotor01-Trennung
  19.06.09 ts  INT_SETTINGS_MINORREVISION_4           TN4618   TSettingsTableUpdateV1_4 Spalte SECTION auf 80 Zeichen vergrößert
  05.08.09 wl  TSettingsTableUpdateV2_1               TN4705   Regelung "Kein Offset wenn InitOffset = Min" durch Parameter UseInitOffset ersetzt
  23.02.10 ts  TSettingsTableUpdateV2_2               TN4992   PipPump04 Settings von Connection- in Driver-Area
  16.08.10 wl  Achtung: TSettingsTableUpdate V2_3, V2_4, V2_5 gibt es nicht!
  16.08.10 wl  TSettingsTableUpdateV2_6               TN5222.1 FixAfterVortexing: YES -> NO, NO -> YES
  16.08.10 wl  TSettingsTableUpdateV2_6               TN5222.2 CATMix wird zu Mixer01
  08.09.10 pk  ConvertFixAfterVortexingSetting        TN5261   Append caused Index out of bounds because select only had selected the Value field instead of select *
  25.11.10 wl  TSettingsTableUpdateV2_7               TN5355   Delay-Settings haben jetzt section 'Delay'
  31.01.11 wl  TSettingsTableUpdateV2_1               TN5453   InitOffset wurde als int gelesen, muss aber als float gelesen werden
  15.02.11 pk                               	     	 TN4780   changes needed to make UpdateManager compatible with TurboDB
  15.02.12 wl  TSettingsTableUpdateV2_8               TN5803   Updater für BCTurntable-Device
  13.11.12 wl  TSettingsTableUpdateV2_9               TN6015   neu: DisplayComponents von Settings trennen
  20.02.13 wl  TSettingsTableUpdateV2_10              TN6055   neu: MethodParams von Settings trennen
  07.11.13 wl  TSettingsTableUpdateV2_11              TN6274   REMPIPETTE wird durch ATTACHEDPIPETTE ersetzt
  -------------------------------------------------------------------------------------------------- }

unit SettingsTableUpdate;


interface


uses
    Classes,
    Update,
    TableStructDef,
    TableUpdate,
    TableChangeAdaptor,
    DataProvider;

const
    INT_SETTINGS_MAJORREVISION_1 = 1;
    INT_SETTINGS_MAJORREVISION_2 = 2;

    INT_SETTINGS_MINORREVISION_1 = 1;
    INT_SETTINGS_MINORREVISION_2 = 2;
    INT_SETTINGS_MINORREVISION_3 = 3;
    INT_SETTINGS_MINORREVISION_4 = 4;
    INT_SETTINGS_MINORREVISION_5 = 5;
    INT_SETTINGS_MINORREVISION_6 = 6;
    INT_SETTINGS_MINORREVISION_7 = 7;
    INT_SETTINGS_MINORREVISION_8 = 8;
    INT_SETTINGS_MINORREVISION_9 = 9;
    INT_SETTINGS_MINORREVISION_10 = 10;
    INT_SETTINGS_MINORREVISION_11 = 11;

type
    // Utilities that uses the settings table
    TPipDeviceRec = record
        PipDeviceName: string;
        TipCount: integer;
    end;

    TSettingsTablePipDeviceLoader = class
    private
        fPipDevices: array of TPipDeviceRec;
        procedure ReadPipDeviceData(aTableChangeAdaptor: TTableChangeAdaptor);
    public
        constructor Create(aTableChangeAdaptor: TTableChangeAdaptor);
        destructor Destroy(); override;
        function GetArmTipFromTotalTip(const aTotalTip: integer; out oPipDeviceName: string): integer;
        function GetArmTipmapFromTotalTipmap(const aTotalTipmap: integer; out oPipDeviceName: string)
            : integer;
    end;

    TSettingsTableProviderUtilsV0 = class
    public
        class procedure AppendRecord(const aDataProvider: TDataProvider; const aArea: string;
            const aSection: string; const aIdent: string; const aValue: string);

    end;

    TSettingsSQL = class
    public
        class function AreaEq(const aTableAlias: string; const aValue: string): string;
        class function SectionEq(const aTableAlias: string; const aValue: string): string;
        class function IdentEq(const aTableAlias: string; const aValue: string): string;
        class function ValueEq(const aTableAlias: string; const aValue: string): string;
    end;

    // table structure definitions
    TSettingsTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TSettingsTableStructDefV1 = class(TSettingsTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TSettingsTableStructDefV2 = class(TSettingsTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TSettingsTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateID: TUpdateID);
    end;

    TSettingsTableUpdateV2 = class(TTableUpdate)
    public
        constructor Create(aUpdateID: TUpdateID);
    end;

    TSettingsTableUpdateV2_1 = class(TTableUpdate)
    private
        procedure WriteMotorsUsingInitOffset(aDeviceNames: TStrings);
        procedure SearchMotorsUsingInitOffset(aDeviceNames: TStrings);
        procedure DeviceSettingsUpdateV2_1CustomFunc(aSender: TObject);
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateID: TUpdateID);
    end;

    TSettingsTableUpdateV2_2 = class(TTableUpdate)
    private
        procedure SettingsUpdateV2_2CustomFunc(aSender: TObject);
        procedure WriteSettingFromConnectionToDriver(aSettingName: string);
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TSettingsTableUpdateV2_6 = class(TTableUpdate)
    private
        procedure SettingsUpdateV2_6CustomFunc(aSender: TObject);
        procedure ConvertMixer01Settings(aSettingDP: TDataProvider);
        procedure ConvertFixAfterVortexingSetting(aSettingDP: TDataProvider);
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TSettingsTableUpdateV2_7 = class(TTableUpdate)
    private
        procedure SettingsUpdateV2_7CustomFunc(aSender: TObject);
        procedure ConvertDelayActionSettings(aSettingDP: TDataProvider);
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TSettingsTableUpdateV2_8 = class(TTableUpdate)
    private
        procedure SettingsUpdateV2_8CustomFunc(aSender: TObject);
        procedure ConvertSettings(aSettingDP: TDataProvider);
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TSettingsTableUpdateV2_9 = class(TTableUpdate)
    private
        procedure SettingsUpdateV2_9CustomFunc(aSender: TObject);
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TSettingsTableUpdateV2_10 = class(TTableUpdate)
    private
        procedure DeleteMethodParamSection(aSender: TObject);
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TSettingsTableUpdateV2_11 = class(TTableUpdate)
    private
        procedure DeleteMethodParamSection(aSender: TObject);
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils;

const
    STR_SETTINGS_V0_TBL = 'SETTINGS';

    STR_SETTINGS_V0_FLD_AREA = 'AREA';
    STR_SETTINGS_V0_FLD_SECTION = 'SECTION';
    STR_SETTINGS_V0_FLD_IDENT = 'IDENT';
    STR_SETTINGS_V0_FLD_VALUE = 'VALUE';

    INT_SETTINGS_V0_FLDLEN_AREA = 40;
    INT_SETTINGS_V0_FLDLEN_SECTION = 40;
    INT_SETTINGS_V0_FLDLEN_IDENT = 60;
    INT_SETTINGS_V0_FLDLEN_VALUE = 200;

    INT_SETTINGS_V2_FLDLEN_SECTION = 80;

class procedure TSettingsTableProviderUtilsV0.AppendRecord(const aDataProvider: TDataProvider;
    const aArea: string; const aSection: string; const aIdent: string; const aValue: string);
begin
    aDataProvider.Append;
    aDataProvider.FieldByName(STR_SETTINGS_V0_FLD_AREA).AsString := aArea;
    aDataProvider.FieldByName(STR_SETTINGS_V0_FLD_SECTION).AsString := aSection;
    aDataProvider.FieldByName(STR_SETTINGS_V0_FLD_IDENT).AsString := aIdent;
    aDataProvider.FieldByName(STR_SETTINGS_V0_FLD_VALUE).AsString := aValue;
    aDataProvider.Post;

end;

class function TSettingsSQL.AreaEq(const aTableAlias: string; const aValue: string): string;
begin
    result := TSQL.FieldEq(aTableAlias, STR_SETTINGS_V0_FLD_AREA, aValue);
end;

class function TSettingsSQL.SectionEq(const aTableAlias: string; const aValue: string): string;
begin
    result := TSQL.FieldEq(aTableAlias, STR_SETTINGS_V0_FLD_SECTION, aValue);
end;

class function TSettingsSQL.IdentEq(const aTableAlias: string; const aValue: string): string;
begin
    result := TSQL.FieldEq(aTableAlias, STR_SETTINGS_V0_FLD_IDENT, aValue);
end;

class function TSettingsSQL.ValueEq(const aTableAlias: string; const aValue: string): string;
begin
    result := TSQL.FieldEq(aTableAlias, STR_SETTINGS_V0_FLD_VALUE, aValue);
end;

{ TSettingsTablePipDeviceLoader }

constructor TSettingsTablePipDeviceLoader.Create(aTableChangeAdaptor: TTableChangeAdaptor);
begin
    inherited Create();

    // Settings lesen
    ReadPipDeviceData(aTableChangeAdaptor);
end;

destructor TSettingsTablePipDeviceLoader.Destroy;
begin

    inherited;
end;

procedure TSettingsTablePipDeviceLoader.ReadPipDeviceData(aTableChangeAdaptor: TTableChangeAdaptor);
var
    xDP: TDataProvider;
    x: integer;
    xSQL: string;
begin
    xDP := aTableChangeAdaptor.CreateSourceDataProvider();
    try
        xSQL := 'Select sa.Section as ARMNAME, st.SECTION as DEVNAME, st."VALUE" as TIPCOUNT ' +
            ' from Settings sn, Settings st, Settings sa ' + ' where ' + TSettingsSQL.AreaEq('sn', 'DEVICE') +
            ' and ' + TSettingsSQL.AreaEq('st', 'DEVICE') + ' and sn.SECTION = st.SECTION ' + ' and ' +
            TSettingsSQL.IdentEq('st', 'Tips') + ' and ' + TSettingsSQL.IdentEq('sn', 'Type') + ' and ' + '( '
            + TSettingsSQL.ValueEq('sn', 'ATTACHEDPIPETTE') + ' or ' +
            TSettingsSQL.ValueEq('sn', 'REMPIPETTE') + ' ) ' + ' and sa."VALUE" = st.SECTION ' +
            ' order by ARMNAME';
        xDP.SelectAndOpen(xSQL, true);
        try
            x := 0;
            SetLength(fPipDevices, xDP.RecordCount);
            while not xDP.Eof do
            begin
                fPipDevices[x].PipDeviceName := xDP.FieldByName('DEVNAME').AsString;
                fPipDevices[x].TipCount := StrToIntDef(xDP.FieldByName('TIPCOUNT').AsString, 0);
                xDP.Next;
                Inc(x);
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

function TSettingsTablePipDeviceLoader.GetArmTipFromTotalTip(const aTotalTip: integer;
    out oPipDeviceName: string): integer;
var
    x, xPreviousTipCount: integer;
begin
    xPreviousTipCount := 0;

    for x := 0 to high(fPipDevices) do
    begin

        if (aTotalTip - xPreviousTipCount <= fPipDevices[x].TipCount) then
        begin
            oPipDeviceName := fPipDevices[x].PipDeviceName;
            result := aTotalTip - xPreviousTipCount;
            EXIT;
        end;

        xPreviousTipCount := xPreviousTipCount + fPipDevices[x].TipCount;
    end;

    // hier sollte man nicht hinkommen
    oPipDeviceName := '';
    result := aTotalTip;
end;

function TSettingsTablePipDeviceLoader.GetArmTipmapFromTotalTipmap(const aTotalTipmap: integer;
    out oPipDeviceName: string): integer;
var
    x, xArmTipIndex, xPreviousTipCount, xTotalTipIndex: integer;
begin
    xPreviousTipCount := 0;
    oPipDeviceName := '';
    result := 0;

    for x := 0 to high(fPipDevices) do
    begin

        for xArmTipIndex := 0 to fPipDevices[x].TipCount - 1 do
        begin

            xTotalTipIndex := xPreviousTipCount + xArmTipIndex;
            if (((aTotalTipmap shr (xTotalTipIndex)) and 1) <> 1) then
                CONTINUE;

            oPipDeviceName := fPipDevices[x].PipDeviceName;
            result := result or (1 shl xArmTipIndex);
        end;

        // Wenn für diesen Arm Tips gefunden wurden --> RETURN
        if (oPipDeviceName <> '') then
            EXIT;

        xPreviousTipCount := xPreviousTipCount + fPipDevices[x].TipCount;
    end;

    // hier sollte man nicht hinkommen
    oPipDeviceName := '';
    result := aTotalTipmap;
end;

{ TSettingsTableStructDefV0 }

procedure TSettingsTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := STR_SETTINGS_V0_TBL;
end;

{ TSettingsTableStructDefV1 }

procedure TSettingsTableStructDefV1.DoDefineStruct();
begin
    inherited;
    AddField(STR_SETTINGS_V0_FLD_AREA, tftString, INT_SETTINGS_V0_FLDLEN_AREA);
    AddField(STR_SETTINGS_V0_FLD_SECTION, tftString, INT_SETTINGS_V0_FLDLEN_SECTION);
    AddField(STR_SETTINGS_V0_FLD_IDENT, tftString, INT_SETTINGS_V0_FLDLEN_IDENT);
    AddField(STR_SETTINGS_V0_FLD_VALUE, tftString, INT_SETTINGS_V0_FLDLEN_VALUE);

    AddIndex(STR_SETTINGS_V0_FLD_AREA + ';' + STR_SETTINGS_V0_FLD_SECTION + ';' + STR_SETTINGS_V0_FLD_IDENT);
end;

{ TSettingsTableStructDefV2 }

procedure TSettingsTableStructDefV2.DoDefineStruct;
begin
    inherited;
    AddField(STR_SETTINGS_V0_FLD_AREA, tftString, INT_SETTINGS_V0_FLDLEN_AREA);
    AddField(STR_SETTINGS_V0_FLD_SECTION, tftString, INT_SETTINGS_V2_FLDLEN_SECTION);
    AddField(STR_SETTINGS_V0_FLD_IDENT, tftString, INT_SETTINGS_V0_FLDLEN_IDENT);
    AddField(STR_SETTINGS_V0_FLD_VALUE, tftString, INT_SETTINGS_V0_FLDLEN_VALUE);

    AddIndex(STR_SETTINGS_V0_FLD_AREA + ';' + STR_SETTINGS_V0_FLD_SECTION + ';' + STR_SETTINGS_V0_FLD_IDENT);
end;

{ TSettingsTableUpdateV1 }

constructor TSettingsTableUpdateV1.Create(aUpdateID: TUpdateID);
begin
    inherited Create(aUpdateID, TSettingsTableStructDefV0, INT_SETTINGS_MAJORREVISION_1);
    AlterStructure(TSettingsTableStructDefV1);
    CopyMatchingFields([]);
end;

{ TSettingsTableUpdateV2 }

constructor TSettingsTableUpdateV2.Create(aUpdateID: TUpdateID);
begin
    inherited Create(aUpdateID, TSettingsTableStructDefV2, INT_SETTINGS_MAJORREVISION_2);
    AlterStructure(TSettingsTableStructDefV2);
    CopyMatchingFields([]);
end;

{ TSettingsTableUpdateV2_1 }

constructor TSettingsTableUpdateV2_1.Create(aUpdateID: TUpdateID);
begin
    inherited Create(aUpdateID, TSettingsTableStructDefV2, INT_SETTINGS_MAJORREVISION_2,
        INT_SETTINGS_MINORREVISION_1);
    AlterStructure(TSettingsTableStructDefV2);
    CopyMatchingFields([]);

    self.CustomDataFunc(DeviceSettingsUpdateV2_1CustomFunc);
end;

procedure TSettingsTableUpdateV2_1.SearchMotorsUsingInitOffset(aDeviceNames: TStrings);
var
    xDP: TDataProvider;
    xSQL: string;
    xLastDev: string;
    xVal1: double;
begin
    xDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        xSQL := 'Select sn.Section as MOTORNAME, sv.IDENT as VIDENT, sv."VALUE" as VVALUE' +
            ' from Settings sn, Settings sv' + ' where ' + TSettingsSQL.AreaEq('sn', 'DEVICE') + ' and ' +
            TSettingsSQL.AreaEq('sv', 'DEVICE') + ' and sv.SECTION = sn.SECTION' + ' and ' +
            TSettingsSQL.IdentEq('sn', 'Type') + ' and (' + TSettingsSQL.ValueEq('sn', 'XMOTOR') + ' or ' +
            TSettingsSQL.ValueEq('sn', 'YMOTOR') + ' or ' + TSettingsSQL.ValueEq('sn', 'RMOTOR') + ')' +
            ' and (' + TSettingsSQL.IdentEq('sv', 'Min') + ' or ' + TSettingsSQL.IdentEq('sv', 'InitOffset') +
            ' )' + ' order by MOTORNAME';

        // testen, bei welchen Motoren ( Min <> InitOffset ), d.h. InitOffset wird benutzt
        xDP.SelectAndOpen(xSQL, true);
        try
            xLastDev := '';
            xVal1 := 0;
            while not xDP.Eof do
            begin
                if (xLastDev <> xDP.FieldByName('MOTORNAME').AsString) then
                begin

                    // Device eintragen
                    if (xVal1 <> 0) then
                        aDeviceNames.Add(xLastDev);

                    // Neues Device lesen
                    xLastDev := xDP.FieldByName('MOTORNAME').AsString;
                    xVal1 := StrToFloatDef(xDP.FieldByName('VVALUE').AsString, 0);
                end
                else
                begin
                    // die beiden Werte vergleichen
                    if (xVal1 <> StrToFloatDef(xDP.FieldByName('VVALUE').AsString, 0)) then
                        xVal1 := 1
                    else
                        xVal1 := 0;
                end;
                xDP.Next;
            end;

            // letztes Device eintragen
            if (xLastDev <> '') then
            begin
                if (xVal1 <> 0) then
                    aDeviceNames.Add(xLastDev);
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

procedure TSettingsTableUpdateV2_1.WriteMotorsUsingInitOffset(aDeviceNames: TStrings);
var
    xDP: TDataProvider;
    x: integer;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        // "UseInitOffset" für alle erkannten Motoren schreiben
        xDP.SelectAndOpen('select * from settings', false);
        try
            for x := 0 to aDeviceNames.Count - 1 do
            begin
                xDP.Append;
                xDP.FieldByName('AREA').AsString := 'DEVICE';
                xDP.FieldByName('SECTION').AsString := aDeviceNames[x];
                xDP.FieldByName('IDENT').AsString := 'UseInitOffset';
                xDP.FieldByName('VALUE').AsString := 'YES';
                xDP.Post;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

procedure TSettingsTableUpdateV2_1.DeviceSettingsUpdateV2_1CustomFunc(aSender: TObject);
var
    xDeviceNames: TStringList;
begin
    xDeviceNames := TStringList.Create;
    try
        self.SearchMotorsUsingInitOffset(xDeviceNames);
        self.WriteMotorsUsingInitOffset(xDeviceNames);
    finally
        xDeviceNames.Free;
    end;
end;

function TSettingsTableUpdateV2_1.GetUpdateDescription: string;
begin
    result := 'Writes Motor device setting "UseInitOffset" if necessary';
end;

{ TSettingsTableUpdateV2_2 }

constructor TSettingsTableUpdateV2_2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TSettingsTableStructDefV2, INT_SETTINGS_MAJORREVISION_2,
        INT_SETTINGS_MINORREVISION_5);
    self.AlterStructure(TSettingsTableStructDefV2);
    CopyMatchingFields([]);

    self.CustomDataFunc(SettingsUpdateV2_2CustomFunc);
end;

function TSettingsTableUpdateV2_2.GetUpdateDescription: string;
begin
    result := 'Settings "PumpHead", "MaxVolume", "DispSpeed" and "StartPercent" of PipPump04 devices will be stored in Driver area instead of Connection';
end;

procedure TSettingsTableUpdateV2_2.SettingsUpdateV2_2CustomFunc(aSender: TObject);
begin
    self.WriteSettingFromConnectionToDriver('PumpHead');
    self.WriteSettingFromConnectionToDriver('DispSpeed');
    self.WriteSettingFromConnectionToDriver('MaxVolume');
    self.WriteSettingFromConnectionToDriver('StartPercent');
end;

procedure TSettingsTableUpdateV2_2.WriteSettingFromConnectionToDriver(aSettingName: string);
var
    xDPConnection, xDPDriver: TDataProvider;
    xConnectionName, xDriverName: string;
begin
    xDPConnection := fTableChangeAdaptor.CreateDestDataProvider();
    xDPDriver := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        xDPConnection.SelectAndOpen('Select * from SETTINGS s WHERE ' + TSettingsSQL.IdentEq('s',
            aSettingName), false);
        if xDPConnection.RecordCount = 0 then
            EXIT;

        while xDPConnection.FieldByName('AREA').AsString = 'CONNECTION' do
        begin
            xConnectionName := xDPConnection.FieldByName('SECTION').AsString;
            xDPDriver.SelectAndOpen('Select * from SETTINGS s where ' + TSettingsSQL.ValueEq('s',
                xConnectionName), true);
            xDriverName := xDPDriver.FieldByName('SECTION').AsString;

            xDPConnection.Edit;
            xDPConnection.FieldByName('AREA').AsString := 'DRIVER';
            xDPConnection.FieldByName('SECTION').AsString := xDriverName;
            xDPConnection.Post;

            xDPConnection.First;

            xDPDriver.Close;
        end;

        xDPConnection.Close;
    finally
        xDPDriver.Free;
        xDPConnection.Free;
    end;
end;

{ TSettingsTableUpdateV2_6 }

constructor TSettingsTableUpdateV2_6.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TSettingsTableStructDefV2, INT_SETTINGS_MAJORREVISION_2,

        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        INT_SETTINGS_MINORREVISION_6);
    // Achtung: MINORREVISION ist zu hoch, aber da das letzte Update die Version auf 5 gesetzt hat, musste das sein

    self.AlterStructure(TSettingsTableStructDefV2);
    CopyMatchingFields([]);

    self.CustomDataFunc(SettingsUpdateV2_6CustomFunc);
end;

function TSettingsTableUpdateV2_6.GetUpdateDescription: string;
begin
    result := 'Package Mixer01.bpl renamed (former CATMix.bpl)';
end;

procedure TSettingsTableUpdateV2_6.SettingsUpdateV2_6CustomFunc(aSender: TObject);
var
    xSettingDP: TDataProvider;
begin
    xSettingDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        self.ConvertMixer01Settings(xSettingDP);
        self.ConvertFixAfterVortexingSetting(xSettingDP);
    finally
        xSettingDP.Free;
    end;
end;

procedure TSettingsTableUpdateV2_6.ConvertFixAfterVortexingSetting(aSettingDP: TDataProvider);
var
    xNames: array of string;
    x: integer;
    xAppend: boolean;
begin
    aSettingDP.SelectAndOpen('select distinct s."Section" from settings s where ' + TSettingsSQL.AreaEq('s',
        'DEVICE') + ' and ' + TSettingsSQL.IdentEq('s', 'Type') + ' and ' + TSettingsSQL.ValueEq('s',
        'MIXER'), true);
    try
        SetLength(xNames, aSettingDP.RecordCount);
        x := 0;
        while not aSettingDP.Eof do
        begin
            xNames[x] := aSettingDP.FieldByName('Section').AsString;
            aSettingDP.Next;
            inc(x);
        end;
    finally
        aSettingDP.Close;
    end;

    for x := 0 to high(xNames) do
    begin
        aSettingDP.SelectAndOpen('select s."Value" from settings s where ' + TSettingsSQL.AreaEq('s',
            'DEVICE') + ' and ' + TSettingsSQL.IdentEq('s', 'FixAfterVortexing') + ' and ' +
            TSettingsSQL.SectionEq('s', xNames[x]), true);
        try
            xAppend := (aSettingDP.Eof);
        finally
            aSettingDP.Close;
        end;

        aSettingDP.SelectAndOpen('select * from settings', false);
        try
            if xAppend then
            begin
                aSettingDP.Append;
                aSettingDP.FieldByName('AREA').AsString := 'DEVICE';
                aSettingDP.FieldByName('SECTION').AsString := xNames[x];
                aSettingDP.FieldByName('IDENT').AsString := 'FixAfterVortexing';
                aSettingDP.FieldByName('VALUE').AsString := 'NO';
                aSettingDP.Post;
            end
            else
            begin
                aSettingDP.Edit;
                if (aSettingDP.FieldByName('Value').AsString = 'NO') then
                    aSettingDP.FieldByName('Value').AsString := 'YES'
                else
                    aSettingDP.FieldByName('Value').AsString := 'NO';
                aSettingDP.Post;
            end;
        finally
            aSettingDP.Close;
        end;
    end;
end;

procedure TSettingsTableUpdateV2_6.ConvertMixer01Settings(aSettingDP: TDataProvider);
const
    cAlias = 'Settings';
begin
    // Namensänderungen

    aSettingDP.ExecSQL('Update settings Set ' + TSettingsSQL.ValueEq(cAlias, 'Mixer01Connection') + ' where '
        + TSettingsSQL.AreaEq(cAlias, 'CONNECTION') + ' and ' + TSettingsSQL.IdentEq(cAlias, 'Type') + ' and '
        + TSettingsSQL.ValueEq(cAlias, 'CATMIXCONNECTION'));

    aSettingDP.ExecSQL('Update settings Set ' + TSettingsSQL.ValueEq(cAlias, 'Mixer01FixationSwitch') +
        ' where ' + TSettingsSQL.AreaEq(cAlias, 'DRIVER') + ' and ' + TSettingsSQL.IdentEq(cAlias, 'Type') +
        ' and ' + TSettingsSQL.ValueEq(cAlias, 'CATFIXATION'));

    aSettingDP.ExecSQL('Update settings Set ' + TSettingsSQL.ValueEq(cAlias, 'Mixer01Thermostat') + ' where '
        + TSettingsSQL.AreaEq(cAlias, 'DRIVER') + ' and ' + TSettingsSQL.IdentEq(cAlias, 'Type') + ' and ' +
        TSettingsSQL.ValueEq(cAlias, 'CATTHERMOSTAT'));

    aSettingDP.ExecSQL('Update settings Set ' + TSettingsSQL.ValueEq(cAlias, 'Mixer01Vortexer') + ' where ' +
        TSettingsSQL.AreaEq(cAlias, 'DRIVER') + ' and ' + TSettingsSQL.IdentEq(cAlias, 'Type') + ' and ' +
        TSettingsSQL.ValueEq(cAlias, 'CATVORTEXER'));
end;

{ TSettingsTableUpdateV2_7 }

constructor TSettingsTableUpdateV2_7.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TSettingsTableStructDefV2, INT_SETTINGS_MAJORREVISION_2,
        INT_SETTINGS_MINORREVISION_7);

    self.AlterStructure(TSettingsTableStructDefV2);
    CopyMatchingFields([]);

    self.CustomDataFunc(SettingsUpdateV2_7CustomFunc);
end;

function TSettingsTableUpdateV2_7.GetUpdateDescription: string;
begin
    result := 'Change settings for delay action';
end;

procedure TSettingsTableUpdateV2_7.SettingsUpdateV2_7CustomFunc(aSender: TObject);
var
    xSettingDP: TDataProvider;
begin
    xSettingDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        self.ConvertDelayActionSettings(xSettingDP);
    finally
        xSettingDP.Free;
    end;
end;

procedure TSettingsTableUpdateV2_7.ConvertDelayActionSettings(aSettingDP: TDataProvider);
begin
    // Section für SkipDelaysOnSimulation geändert
    aSettingDP.SelectAndOpen('select * from settings s where ' + TSettingsSQL.AreaEq('s', 'APPLICATION') +
        ' and ' + TSettingsSQL.SectionEq('s', 'Run') + ' and ' + TSettingsSQL.IdentEq('s',
        'SkipDelaysOnSimulation'), false);
    try
        if not aSettingDP.Eof then
        begin
            aSettingDP.Edit;
            aSettingDP.FieldByName('SECTION').AsString := 'Delay';
            aSettingDP.Post;
        end;
    finally
        aSettingDP.Close;
    end;

    // statt diese Altlasten zu importieren, setzen wir lieber die Default-Werte
    {
      // Section, Identname und Value für ShowDialogForDelayLongerThan geändert
      aSettingDP.SelectAndOpen( 'select * from settings s where s."Area" = ''APPLICATION'' and s."Section" = ''Run'' and s."Ident" = ''ShowDialogForDelayLongerThan''', false );
      try
      if not aSettingDP.Eof then begin
      aSettingDP.Edit;
      aSettingDP.FieldByName( 'SECTION' ).AsString := 'Delay';
      aSettingDP.FieldByName( 'IDENT' ).AsString := 'ShowDelayLongerThanXMilliSeconds';
      aSettingDP.FieldByName( 'VALUE' ).AsString := aSettingDP.FieldByName( 'VALUE' ).AsString + '000';
      aSettingDP.Post;
      end;
      finally
      aSettingDP.Close;
      end;

      // Eintrag SkipXSeconds mit dem Wert 1 einfügen (nur aus Kompatibilitätsgründen)
      aSettingDP.SelectAndOpen( 'select * from settings', false );
      try
      aSettingDP.Append;
      aSettingDP.FieldByName( 'AREA' ).AsString := 'APPLICATION';
      aSettingDP.FieldByName( 'SECTION' ).AsString := 'Delay';
      aSettingDP.FieldByName( 'IDENT' ).AsString := 'SkipXMilliSeconds';
      aSettingDP.FieldByName( 'VALUE' ).AsString := '1000';
      aSettingDP.Post;
      finally
      aSettingDP.Close;
      end;
    }

end;

{ TSettingsTableUpdateV2_8 }

constructor TSettingsTableUpdateV2_8.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TSettingsTableStructDefV2, INT_SETTINGS_MAJORREVISION_2,
        INT_SETTINGS_MINORREVISION_8);

    self.AlterStructure(TSettingsTableStructDefV2);
    CopyMatchingFields([]);

    self.CustomDataFunc(SettingsUpdateV2_8CustomFunc);
end;

function TSettingsTableUpdateV2_8.GetUpdateDescription: string;
begin
    result := 'Device BCTurnatble as Switch or Motor';
end;

procedure TSettingsTableUpdateV2_8.SettingsUpdateV2_8CustomFunc(aSender: TObject);
var
    xSettingDP: TDataProvider;
begin
    xSettingDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        self.ConvertSettings(xSettingDP);
    finally
        xSettingDP.Free;
    end;
end;

procedure TSettingsTableUpdateV2_8.ConvertSettings(aSettingDP: TDataProvider);
const
    cAlias = 'Settings';
begin
    // Namensänderungen (es wird nur auf BCTurntableMotor umgestellt, Switch wird nicht berücksichtigt

    aSettingDP.ExecSQL('Update settings Set ' + TSettingsSQL.ValueEq(cAlias, 'BCTurntableMotor') + ' where ' +
        TSettingsSQL.AreaEq(cAlias, 'DEVICE') + ' and ' + TSettingsSQL.IdentEq(cAlias, 'Type') + ' and ' +
        TSettingsSQL.ValueEq(cAlias, 'BCTURNTABLE'));

    aSettingDP.ExecSQL('Update settings Set ' + TSettingsSQL.ValueEq(cAlias, 'ZP01MOTOR') + ' where ' +
        TSettingsSQL.AreaEq(cAlias, 'DRIVER') + ' and ' + TSettingsSQL.IdentEq(cAlias, 'Type') + ' and ' +
        TSettingsSQL.ValueEq(cAlias, 'ZP01BCTURNTABLE'));

    aSettingDP.ExecSQL('Update settings Set ' + TSettingsSQL.ValueEq(cAlias, 'ZP02MOTOR') + ' where ' +
        TSettingsSQL.AreaEq(cAlias, 'DRIVER') + ' and ' + TSettingsSQL.IdentEq(cAlias, 'Type') + ' and ' +
        TSettingsSQL.ValueEq(cAlias, 'ZP02BCTURNTABLE'));
end;

{ TSettingsTableUpdateV2_9 }

constructor TSettingsTableUpdateV2_9.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TSettingsTableStructDefV2, INT_SETTINGS_MAJORREVISION_2,
        INT_SETTINGS_MINORREVISION_9);

    self.AlterStructure(TSettingsTableStructDefV2);
    CopyMatchingFields([]);

    self.CustomDataFunc(SettingsUpdateV2_9CustomFunc);
end;

function TSettingsTableUpdateV2_9.GetUpdateDescription: string;
begin
    result := 'Delete DisplayComponent section from Settings';
end;

procedure TSettingsTableUpdateV2_9.SettingsUpdateV2_9CustomFunc(aSender: TObject);
var
    xDP: TDataProvider;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.ExecSQL('delete from SETTINGS where [AREA]=''DISPLAYCOMPONENTS''');
    finally
        xDP.Free;
    end;
end;

{ TSettingsTableUpdateV2_10 }

constructor TSettingsTableUpdateV2_10.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TSettingsTableStructDefV2, INT_SETTINGS_MAJORREVISION_2,
        INT_SETTINGS_MINORREVISION_10);

    self.AlterStructure(TSettingsTableStructDefV2);
    CopyMatchingFields([]);

    self.CustomDataFunc(DeleteMethodParamSection);
end;

function TSettingsTableUpdateV2_10.GetUpdateDescription: string;
begin
    result := 'Delete MethodParameters section from Settings';
end;

procedure TSettingsTableUpdateV2_10.DeleteMethodParamSection(aSender: TObject);
var
    xDP: TDataProvider;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.ExecSQL('delete from SETTINGS where [AREA]=''APPLICATION'' and  [Section]=''MethodParameters''');
    finally
        xDP.Free;
    end;
end;

{ TSettingsTableUpdateV2_11 }

constructor TSettingsTableUpdateV2_11.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TSettingsTableStructDefV2, INT_SETTINGS_MAJORREVISION_2,
        INT_SETTINGS_MINORREVISION_11);

    self.AlterStructure(TSettingsTableStructDefV2);
    CopyMatchingFields([]);

    self.CustomDataFunc(DeleteMethodParamSection);
end;

function TSettingsTableUpdateV2_11.GetUpdateDescription: string;
begin
    result := 'Replace REMPIPETTE in Settings';
end;

procedure TSettingsTableUpdateV2_11.DeleteMethodParamSection(aSender: TObject);
var
    xDP: TDataProvider;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.ExecSQL('update SETTINGS set [Value]=''ATTACHEDPIPETTE''' +
            ' where [AREA]=''DEVICE'' and [Ident]=''Type'' and [Value]=''REMPIPETTE''');
    finally
        xDP.Free;
    end;
end;


end.
