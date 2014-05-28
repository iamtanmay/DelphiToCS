{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  10.12.10 wl  ConvertZP01Config            TN5405   konvertiert ZP01CONFIG in Devices (einige Angaben fehlen noch)
  13.12.10 wl  TStepsToMMConverter          TN5405   von StepsToMMUpdate hierher
  21.01.11 wl                               TN5405   Device-Konvertierung für ZP01 mit V7.0.9 und ZP02 mit V7.1.7
  31.01.11 wl  WriteDeviceIniFile           TN5440   schreibt jetzt auch x-,y- und z-Offsets für Layout-Konvertierung
  15.02.11 pk                               TN4780   changes needed to make UpdateManager compatible with TurboDB
  21.02.11 wl  ConvertZP01Config            TN5455   Step-Werte werden jetzt in mm-Werte umgewandelt
  09.06.11 wl  CreateMTPReader              TN5597   Name von 2D BC Rack & Tube Reader geändert
  13.02.12 wl  CreateBCReaders              TN5801   Kleine Änderungen bei BCTurntable
  30.03.13 wl                               TN6121   Bugfix Switch-Konvertierung
  10.12.13 wl                               TN6326   entfernt: 'Carrier', 'ReverseY'
  ---------------------------------------------------------------------------------------------------------------------- }

unit DeviceConversion;


interface


uses
    Generics.Collections,
    Types,
    SysUtils,
    Update,
    TableUpdate,
    DeviceClasses,
    DataProvider;

type
    TStepsToMMConverter = class
    strict private
        fXYZSettingsReadDone: boolean;
        fGSettingsReadDone: boolean;
        fXStepsPerMM: double;
        fYStepsPerMM: double;
        fZStepsPerMM: double;
        fHZStepsPerMM: double;
        // fRStepsPerMM : double;
        fGStepsPerMM: double;
        fGOffsetMM: double;
        fSettingsDP: TDataProvider;
        function ReadStepsPerMM(const aIdent: string): double;

    const
        DBL_ZP01_GSTEPSPERMM = 30.9476;

    const
        DBL_ZP01_GOFFSETMM = 8.5;

    const
        DBL_ZP02_GOFFSETMM = 42.0;
    public
        constructor Create();
        destructor Destroy; override;

    const
        INT_CONV_X = 0;

    const
        INT_CONV_Y = 1;

    const
        INT_CONV_Z = 2;

    const
        INT_CONV_HZ = 3;

    const
        INT_CONV_G = 4;

        procedure ReadXYZSettings();
        function StepsToMMByType(aType, aSteps: integer): double;
        procedure ReadGSettings();

        function XStepsToMM(aSteps: integer): double;
        function YStepsToMM(aSteps: integer): double;
        function ZStepsToMM(aSteps: integer): double;
        function HZStepsToMM(aSteps: integer): double;
        function GStepsToMM(aSteps: integer): double;

        property XStepsPerMM: double read fXStepsPerMM;
        property YStepsPerMM: double read fYStepsPerMM;
        property ZStepsPerMM: double read fZStepsPerMM;
        property HZStepsPerMM: double read fHZStepsPerMM;
        property GStepsPerMM: double read fGStepsPerMM;
        property GOffsetMM: double read fGOffsetMM;
    end;

    // neu: Device-Konvertierung
    TDeviceConversion = class
    strict private
        fSourceDP: TDataProvider;
        fDestDP: TDataProvider;
        fStepsToMMConv: TStepsToMMConverter;

        class procedure ConvertAll(aSourceDP, aDestDP: TDataProvider; aStepsToMMConv: TStepsToMMConverter);
        class function ReadRediDeviceNames(aSourceDP: TDataProvider): TArray<string>;
    public
        constructor Create(aSourceDP: TDataProvider; aDestDP: TDataProvider;
            aStepsToMMConv: TStepsToMMConverter);
        procedure Convert();
    end;

    TActionModules = class(TCompositeDevice)
    strict private
        BCRackLeadZero: Byte; // 15.10.98 neu wl: Führende Nullen
        BCTubeLeadZero: Byte; // wird für Tubes noch nicht ausgewertet
        class function FindMotorExecID(aDevice: TDevice; aAdr: integer): integer;
        function Find_ByName(aName: string): TDevice; reintroduce;
        procedure CreateMTPReader(aSettingDP: TDataProvider; const aDLLName: string;
            const aTubeBCRDev: TBCReaderDevice);
        procedure AddSystemLiquidValve(aSettingDP: TDataProvider; const aArea: string);
        procedure CreateBCReaders(aSettingDP: TDataProvider);
        procedure CreateComModules(aSettingDP: TDataProvider; const aDevArea: string);
        procedure LoadArmDevices(aSettingDP: TDataProvider; const aDevArea: string);
        procedure ReadDeviceIniFile(aSettingDP: TDataProvider; const aArea: string;
            aRediDeviceNames: TArray<string>);
        procedure CreateBalance(aSettingDP: TDataProvider);
        procedure CreateSystemDevices(aSettingDP: TDataProvider; const aDevArea: string);
        class procedure ReadZP01DilutorData(aSourceDP: TDataProvider; aIndex: integer; out oSysInput: string;
            out oMaxSteps: integer);
        class function ZP01ModuleExists(const aText: string; out oSettings: TStringDynArray;
            out oAdr: integer): boolean;
        class procedure ReadZP01GripperArmWorldoffs(aSourceDP: TDataProvider;
            out oXOffs, oYOffs, oZOffs: integer);
    public
        procedure Prepare(aSourceDP: TDataProvider; aRediDeviceNames: TArray<string>);
        procedure WriteDeviceIniFile(aSourceDP, aDestDP: TDataProvider; aIsZP01: boolean;
            aZP01Config: TDictionary<string, string>; aStepsToMMConv: TStepsToMMConverter;
            aWashRetrSpeed: integer; aEnglFormatSettings: TFormatSettings);
        class procedure WriteZP01Config(aSourceDP, aDestDP: TDataProvider;
            aZP01Config: TDictionary<string, string>; aStepsToMMConv: TStepsToMMConverter;
            aWashRetrSpeed: integer; out oZP01GlobalZ: double; aEnglFormatSettings: TFormatSettings);
    end;


implementation


uses
    Variants,
    Classes,
    Math,
    StrUtils,
    Dialogs,
    UpdateManagerCommonTypes,
    SettingsTableUpdate,
    UpdaterDataProviderFactory;

{ TStepsToMMConverter }

constructor TStepsToMMConverter.Create;
begin
    inherited Create();
    fSettingsDP := TUpdaterDataProviderFactory.CreateStandardDataProvider(TUpdatePaths.Instance.DBPath);
    fXYZSettingsReadDone := false;
    fGSettingsReadDone := false;
end;

destructor TStepsToMMConverter.Destroy();
begin
    fSettingsDP.Free;
    inherited;
end;

function TStepsToMMConverter.ReadStepsPerMM(const aIdent: string): double;
begin
    result := -1;
    fSettingsDP.SelectAndOpen('SELECT * FROM Settings s WHERE ' + TSettingsSQL.IdentEq('s', aIdent), true);
    try
        if fSettingsDP.IsEmpty then
            EXIT;
        result := StrToFloatDef(fSettingsDP.FieldByName('VALUE').AsString, 0);
    finally
        fSettingsDP.Close();
    end;
end;

procedure TStepsToMMConverter.ReadXYZSettings;
begin
    if fXYZSettingsReadDone then
        EXIT;
    fXStepsPerMM := ReadStepsPerMM('XSTEPperMM');
    fYStepsPerMM := ReadStepsPerMM('YSTEPperMM');
    fZStepsPerMM := ReadStepsPerMM('ZSTEPperMM');

    fHZStepsPerMM := ReadStepsPerMM('HSTEPperMM');
    fXYZSettingsReadDone := true;
end;

function TStepsToMMConverter.XStepsToMM(aSteps: integer): double;
begin
    result := aSteps / fXStepsPerMM;
end;

function TStepsToMMConverter.YStepsToMM(aSteps: integer): double;
begin
    result := aSteps / fYStepsPerMM;
end;

function TStepsToMMConverter.ZStepsToMM(aSteps: integer): double;
begin
    result := aSteps / fZStepsPerMM;
end;

function TStepsToMMConverter.GStepsToMM(aSteps: integer): double;
begin
    if aSteps = 0 then
        result := 0
    else
        result := (aSteps / fGStepsPerMM) + fGOffsetMM;
end;

function TStepsToMMConverter.HZStepsToMM(aSteps: integer): double;
begin
    result := aSteps / fHZStepsPerMM;
end;

function TStepsToMMConverter.StepsToMMByType(aType: integer; aSteps: integer): double;
begin
    if aType = INT_CONV_X then
        result := XStepsToMM(aSteps)
    else if aType = INT_CONV_Y then
        result := YStepsToMM(aSteps)
    else if aType = INT_CONV_Z then
        result := ZStepsToMM(aSteps)
    else if aType = INT_CONV_HZ then
        result := HZStepsToMM(aSteps)
    else if aType = INT_CONV_G then
        result := GStepsToMM(aSteps)
    else
        result := 0;
end;

procedure TStepsToMMConverter.ReadGSettings;
var
    xIsZP01: boolean;
begin
    if fGSettingsReadDone then
        EXIT;

    fSettingsDP.SelectAndOpen('SELECT * FROM Settings s WHERE ' + TSettingsSQL.AreaEq('s',
        'ZP01CONFIG'), true);
    try
        xIsZP01 := not fSettingsDP.IsEmpty;
    finally
        fSettingsDP.Close();
    end;

    if xIsZP01 then
    begin
        fGStepsPerMM := DBL_ZP01_GSTEPSPERMM;
        fGOffsetMM := DBL_ZP01_GOFFSETMM;
    end
    else
    begin
        fGStepsPerMM := ReadStepsPerMM('HVSTEPperMM');
        fGOffsetMM := DBL_ZP02_GOFFSETMM;
    end;

    fGSettingsReadDone := true;
end;

class function TActionModules.ZP01ModuleExists(const aText: string; out oSettings: TStringDynArray;
    out oAdr: integer): boolean;
begin
    result := false;
    oSettings := TAppSettings.StringToStringArray(aText);
    oAdr := -1;

    if Length(oSettings) > 0 then
    begin
        oAdr := StrToIntDef(oSettings[0], -1);
        result := (oAdr > 0) and (oAdr < ZP01_MAX_ADR);
    end;
end;

class procedure TActionModules.ReadZP01DilutorData(aSourceDP: TDataProvider; aIndex: integer;
    out oSysInput: string; out oMaxSteps: integer);
begin
    oSysInput := TIniSettings.ReadString(aSourceDP, 'DEVICE', 'Dilutor ' + IntToStr(aIndex + 1),
        'SysInput', '');
    oMaxSteps := TIniSettings.ReadInteger(aSourceDP, 'DEVICE', 'Dilutor ' + IntToStr(aIndex + 1),
        'MaxMotorStep', 2000);
end;

class procedure TActionModules.ReadZP01GripperArmWorldoffs(aSourceDP: TDataProvider;
    out oXOffs, oYOffs, oZOffs: integer);
var
    xIniAccess: ISimpleIniAccess;
begin
    xIniAccess := TDevSimpleIniAccess.Create(aSourceDP, 'ROBOT');
    oXOffs := xIniAccess.ReadInteger('Handler_ReferenceTip_Distances_Steps', 'DistX_H_Tip1', -1);
    oYOffs := xIniAccess.ReadInteger('Handler_ReferenceTip_Distances_Steps', 'DistY_H_Tip1', -1);
    oZOffs := xIniAccess.ReadInteger('Handler_ReferenceTip_Distances_Steps', 'DistZ_H_Tip1', -1);
end;

class procedure TActionModules.WriteZP01Config(aSourceDP, aDestDP: TDataProvider;
    aZP01Config: TDictionary<string, string>; aStepsToMMConv: TStepsToMMConverter; aWashRetrSpeed: integer;
    out oZP01GlobalZ: double; aEnglFormatSettings: TFormatSettings);
const
    cLGripArm = 'LGripArm';
    cLPipArm = 'LPipArm';
var
    xCurrentSetting: TStringDynArray;
    xAnyZMotorExists: boolean;
    x, xNTips: integer;
    xAdr: integer;
    xMax_mm, xZTravel_mm: double;
    xSysInput: string;
    xMaxSteps: integer;
    xIniAccess: ISimpleIniAccess;
    xhXOffset, xhYOffset, xhZOffset: integer;
begin
    // nTips bestimmen
    xNTips := 4;
    if aZP01Config.ContainsKey('nTips') then
        xNTips := StrToInt(aZP01Config['nTips']);

    // Global Z-Travel
    oZP01GlobalZ := 0;
    if aZP01Config.ContainsKey('ZMax') then
        oZP01GlobalZ := Round(StrToIntDef(aZP01Config['ZMax'], 0) / aStepsToMMConv.ZStepsPerMM * 100) / 100;

    // Connection anlegen
    TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'CONNECTION', TRoboticInterfaceZP01.cZP01Connection,
        'Type', 'ZP01CONNECTION');
    if aZP01Config.ContainsKey('Port') then
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'CONNECTION',
            TRoboticInterfaceZP01.cZP01Connection, 'ComPort', Copy(aZP01Config['Port'], 4, 2));
    if aZP01Config.ContainsKey('Baud') then
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'CONNECTION',
            TRoboticInterfaceZP01.cZP01Connection, 'Baud', aZP01Config['Baud']);
    if aZP01Config.ContainsKey('RS232MaxRetry') then
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'CONNECTION',
            TRoboticInterfaceZP01.cZP01Connection, 'RS232MaxRetry', aZP01Config['RS232MaxRetry']);
    TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'CONNECTION', TRoboticInterfaceZP01.cZP01Connection,
        'RelayBoardAdr', IntToStr(31));
    if aZP01Config.ContainsKey('MaxPorts') then
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'CONNECTION',
            TRoboticInterfaceZP01.cZP01Connection, 'RelayBoardMaxPorts', aZP01Config['MaxPorts']);
    if aZP01Config.ContainsKey('RelayBoard5') then
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'CONNECTION',
            TRoboticInterfaceZP01.cZP01Connection, 'AdditionalRelayBoardAdr', aZP01Config['RelayBoard5']);

    // X,Y,V-Motor-Driver (für beide Arme)
    if aZP01Config.ContainsKey('XMotor') and self.ZP01ModuleExists(aZP01Config['XMotor'], xCurrentSetting,
        xAdr) then
    begin
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'XMotorDriver', 'Type', 'ZP01MOTOR');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'XMotorDriver', 'Connection',
            TRoboticInterfaceZP01.cZP01Connection);
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'XMotorDriver', 'Adr', IntToStr(xAdr));
        if Length(xCurrentSetting) > 1 then
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'XMotorDriver', 'Speed',
                xCurrentSetting[1]);
        if Length(xCurrentSetting) > 2 then
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'XMotorDriver', 'Ramp',
                xCurrentSetting[2]);
    end;
    if aZP01Config.ContainsKey('YMotor') and self.ZP01ModuleExists(aZP01Config['YMotor'], xCurrentSetting,
        xAdr) then
    begin
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'YMotorDriver', 'Type', 'ZP01MOTOR');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'YMotorDriver', 'Connection',
            TRoboticInterfaceZP01.cZP01Connection);
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'YMotorDriver', 'Adr', IntToStr(xAdr));
        if Length(xCurrentSetting) > 1 then
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'YMotorDriver', 'Speed',
                xCurrentSetting[1]);
        if Length(xCurrentSetting) > 2 then
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'YMotorDriver', 'Ramp',
                xCurrentSetting[2]);
    end;
    if (aZP01Config.ContainsKey('VMotor') and self.ZP01ModuleExists(aZP01Config['VMotor'], xCurrentSetting,
        xAdr)) or (aZP01Config.ContainsKey('hVMotor') and self.ZP01ModuleExists(aZP01Config['hVMotor'],
        xCurrentSetting, xAdr)) then
    begin
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'VMotorDriver', 'Type', 'ZP01MOTOR');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'VMotorDriver', 'Connection',
            TRoboticInterfaceZP01.cZP01Connection);
        if Length(xCurrentSetting) > 0 then
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'VMotorDriver', 'Adr',
                xCurrentSetting[0]);
        if Length(xCurrentSetting) > 1 then
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'VMotorDriver', 'Speed',
                xCurrentSetting[1]);
        if Length(xCurrentSetting) > 2 then
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'VMotorDriver', 'Ramp',
                xCurrentSetting[2]);
    end;

    // Welche Arme sollen angelegt werden?
    xAnyZMotorExists := false;
    for x := 0 to xNTips - 1 do
    begin
        if aZP01Config.ContainsKey('ZMotor' + IntToStr(x + 1)) and
            self.ZP01ModuleExists(aZP01Config['ZMotor' + IntToStr(x + 1)], xCurrentSetting, xAdr) then
        begin
            xAnyZMotorExists := true;
            BREAK;
        end;
    end;

    // Pip-Arm anlegen
    if xAnyZMotorExists then
    begin

        // Allgemeine Arm-Daten
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm, 'Type', 'ROBOTARM');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm, 'ArmGroupID', '1');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm, 'Color', 'green');

        xZTravel_mm := 0;
        if aZP01Config.ContainsKey('ZTravel') then
            xZTravel_mm := oZP01GlobalZ - Round(StrToIntDef(aZP01Config['ZTravel'], 0) /
                aStepsToMMConv.ZStepsPerMM * 100) / 100;

        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm, 'ZTravel',
            FloatToStr(xZTravel_mm, aEnglFormatSettings));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm, 'ToolZTravel',
            FloatToStr(xZTravel_mm, aEnglFormatSettings));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm, 'PipToolZTravel',
            FloatToStr(xZTravel_mm, aEnglFormatSettings));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm, 'PipToolMinZTravel',
            FloatToStr(xZTravel_mm, aEnglFormatSettings));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm, 'TubeMinZTravel',
            FloatToStr(xZTravel_mm, aEnglFormatSettings));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm, 'RackMoveZTravel',
            FloatToStr(xZTravel_mm, aEnglFormatSettings));

        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm, 'Pip', cLPipArm + 'Pip');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'Pip', 'Type',
            'ATTACHEDPIPETTE');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'Pip', 'Tips',
            IntToStr(xNTips));

        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm, 'Motion',
            cLPipArm + 'Motion');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'Motion', 'Type',
            'MOTORBASEDMOTION');

        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'Motion', 'ZMotors',
            cLPipArm + 'MotionZMotors');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionZMotors', 'Type',
            'MULTIZMOTOR');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionZMotors',
            'ZMotorsHaveBlockMoveOption', 'NO');

        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'Motion', 'YMotors',
            cLPipArm + 'MotionYMotors');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotors', 'Type',
            'VSPANMULTIYMOTOR');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotors', 'NTips',
            IntToStr(xNTips));
        if aZP01Config.ContainsKey('CloseVfor1Tip') and (aZP01Config['CloseVfor1Tip'] = '0') then
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotors',
                'CloseVFor1Tip', 'NO');
        if aZP01Config.ContainsKey('RefPos') then
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotors',
                'RefPos', aZP01Config['RefPos']);
        {
          if aZP01Config.ContainsKey('VFactor') then
          TSettingsTableProviderUtilsV0.AppendRecord( aDestDP, 'DEVICE', cLPipArm + 'MotionYMotors', 'VFactor', aZP01Config['VFactor']);
          if aZP01Config.ContainsKey('VOffset') then
          TSettingsTableProviderUtilsV0.AppendRecord( aDestDP, 'DEVICE', cLPipArm + 'MotionYMotors', 'VOffset', aZP01Config['VOffset']);
          if aZP01Config.ContainsKey('YOffset') then
          TSettingsTableProviderUtilsV0.AppendRecord( aDestDP, 'DEVICE', cLPipArm + 'MotionYMotors', 'YOffset', aZP01Config['YOffset']);
        }

        // wo auch immer diese Werte herkommen:
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotors', 'VFactor',
            FloatToStr(31, aEnglFormatSettings));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotors', 'VOffset',
            FloatToStr(24.23, aEnglFormatSettings));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotors', 'YOffset',
            FloatToStr(8.6, aEnglFormatSettings));

        // X-Motor
        if aZP01Config.ContainsKey('XMotor') and self.ZP01ModuleExists(aZP01Config['XMotor'], xCurrentSetting,
            xAdr) then
        begin
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'Motion', 'XMotor',
                cLPipArm + 'MotionXMotor');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionXMotor', 'Type',
                'XMOTOR');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionXMotor', 'Driver',
                'XMotorDriver');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionXMotor',
                'StepsPerUnit', FloatToStr(aStepsToMMConv.XStepsPerMM, aEnglFormatSettings));
            xMax_mm := 0;
            if aZP01Config.ContainsKey('XMax') then
                xMax_mm := Round(StrToIntDef(aZP01Config['XMax'], 0) / aStepsToMMConv.XStepsPerMM *
                    100) / 100;
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionXMotor', 'Min',
                FloatToStr(0, aEnglFormatSettings));
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionXMotor', 'Max',
                FloatToStr(xMax_mm, aEnglFormatSettings));
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionXMotor',
                'WorldOffset', FloatToStr(TAppSettings.XOffsetLeftToTip1, aEnglFormatSettings));
        end;

        // Y-Motor
        if aZP01Config.ContainsKey('YMotor') and self.ZP01ModuleExists(aZP01Config['YMotor'], xCurrentSetting,
            xAdr) then
        begin
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotors',
                'YMotor', cLPipArm + 'MotionYMotorsYMotor');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotorsYMotor',
                'Type', 'YMOTOR');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotorsYMotor',
                'Reverse', 'YES');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotorsYMotor',
                'Driver', 'YMotorDriver');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotorsYMotor',
                'StepsPerUnit', FloatToStr(aStepsToMMConv.YStepsPerMM, aEnglFormatSettings));

            xMax_mm := 0;
            if aZP01Config.ContainsKey('YMax') then
                xMax_mm := Round(StrToIntDef(aZP01Config['YMax'], 0) / aStepsToMMConv.YStepsPerMM *
                    100) / 100;

            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotorsYMotor',
                'Min', FloatToStr(0, aEnglFormatSettings));
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotorsYMotor',
                'Max', FloatToStr(xMax_mm, aEnglFormatSettings));
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotorsYMotor',
                'WorldOffset', FloatToStr(TAppSettings.YOffsetFrontToTip1, aEnglFormatSettings));
        end;

        // V-Motor
        if aZP01Config.ContainsKey('VMotor') and self.ZP01ModuleExists(aZP01Config['VMotor'], xCurrentSetting,
            xAdr) then
        begin
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotors',
                'VMotor', cLPipArm + 'MotionYMotorsVMotor');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotorsVMotor',
                'Type', 'VMOTOR');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotorsVMotor',
                'Driver', 'VMotorDriver');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotorsVMotor',
                'StepsPerUnit', FloatToStr(aStepsToMMConv.YStepsPerMM * 2, aEnglFormatSettings));
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotorsVMotor',
                'WorldOffset', '0');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotorsVMotor',
                'Min', '0');
            xMax_mm := 0;
            if aZP01Config.ContainsKey('VMax') then
                xMax_mm := Round(StrToIntDef(aZP01Config['VMax'], 0) / aStepsToMMConv.YStepsPerMM / 2 *
                    100) / 100;
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionYMotorsVMotor',
                'Max', FloatToStr(xMax_mm, aEnglFormatSettings));
        end;

        // Z-Motors
        for x := 0 to xNTips - 1 do
        begin
            if aZP01Config.ContainsKey('ZMotor' + IntToStr(x + 1)) and
                self.ZP01ModuleExists(aZP01Config['ZMotor' + IntToStr(x + 1)], xCurrentSetting, xAdr) then
            begin
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'MotionZMotors',
                    'Motor' + IntToStr(x + 1), cLPipArm + 'MotionZMotorsMotor' + IntToStr(x + 1));
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE',
                    cLPipArm + 'MotionZMotorsMotor' + IntToStr(x + 1), 'Type', 'ZMOTOR');
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE',
                    cLPipArm + 'MotionZMotorsMotor' + IntToStr(x + 1), 'TipNo', IntToStr(x + 1));

                if aZP01Config.ContainsKey('ZScanSpeed') then
                    TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE',
                        cLPipArm + 'MotionZMotorsMotor' + IntToStr(x + 1), 'ScanSpeed',
                        aZP01Config['ZScanSpeed']);
                if aZP01Config.ContainsKey('ZRetrSpeed') then
                    TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE',
                        cLPipArm + 'MotionZMotorsMotor' + IntToStr(x + 1), 'RetractSpeed',
                        aZP01Config['ZRetrSpeed']);

                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE',
                    cLPipArm + 'MotionZMotorsMotor' + IntToStr(x + 1), 'WashRetractSpeed',
                    IntToStr(aWashRetrSpeed));
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE',
                    cLPipArm + 'MotionZMotorsMotor' + IntToStr(x + 1), 'StepsPerUnit',
                    FloatToStr(aStepsToMMConv.ZStepsPerMM, aEnglFormatSettings));
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE',
                    cLPipArm + 'MotionZMotorsMotor' + IntToStr(x + 1), 'WorldOffset',
                    FloatToStr(oZP01GlobalZ, aEnglFormatSettings));
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE',
                    cLPipArm + 'MotionZMotorsMotor' + IntToStr(x + 1), 'Min', '0');
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE',
                    cLPipArm + 'MotionZMotorsMotor' + IntToStr(x + 1), 'Max',
                    FloatToStr(oZP01GlobalZ, aEnglFormatSettings));

                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE',
                    cLPipArm + 'MotionZMotorsMotor' + IntToStr(x + 1), 'Driver',
                    'ZMotor' + IntToStr(x + 1) + 'Driver');

                // Driver für jeden Motor hinzufügen
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                    'ZMotor' + IntToStr(x + 1) + 'Driver', 'Type', 'ZP01MOTOR');
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                    'ZMotor' + IntToStr(x + 1) + 'Driver', 'Connection',
                    TRoboticInterfaceZP01.cZP01Connection);
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                    'ZMotor' + IntToStr(x + 1) + 'Driver', 'Adr', IntToStr(xAdr));
                if Length(xCurrentSetting) > 1 then
                    TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                        'ZMotor' + IntToStr(x + 1) + 'Driver', 'Speed', xCurrentSetting[1]);
                if Length(xCurrentSetting) > 2 then
                    TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                        'ZMotor' + IntToStr(x + 1) + 'Driver', 'Ramp', xCurrentSetting[2]);
            end;
        end;

        // Dilutoren (PipPumps)
        for x := 0 to 8 - 1 do
        begin
            if aZP01Config.ContainsKey('Diluter' + IntToStr(x + 1)) and
                self.ZP01ModuleExists(aZP01Config['Diluter' + IntToStr(x + 1)], xCurrentSetting, xAdr) then
            begin
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLPipArm + 'Pip',
                    'Pump' + IntToStr(x + 1), cLPipArm + 'PipPump' + IntToStr(x + 1));
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE',
                    cLPipArm + 'PipPump' + IntToStr(x + 1), 'Type', 'SIMPLEPIPPUMP');

                // Tip-Zuordnung: z.B. PipPump5 zu Tip 1 bei 4 Tips
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE',
                    cLPipArm + 'PipPump' + IntToStr(x + 1), 'TipNo', IntToStr((x mod xNTips) + 1));

                ReadZP01DilutorData(aSourceDP, x, xSysInput, xMaxSteps);
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE',
                    cLPipArm + 'PipPump' + IntToStr(x + 1), 'SysInput', xSysInput);
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE',
                    cLPipArm + 'PipPump' + IntToStr(x + 1), 'Driver', cLPipArm + 'PipPump' + IntToStr(x + 1) +
                    'Driver');

                // PipPumpDriver
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                    cLPipArm + 'PipPump' + IntToStr(x + 1) + 'Driver', 'Type', 'ZP01PIPPUMP');
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                    cLPipArm + 'PipPump' + IntToStr(x + 1) + 'Driver', 'Connection',
                    TRoboticInterfaceZP01.cZP01Connection);
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                    cLPipArm + 'PipPump' + IntToStr(x + 1) + 'Driver', 'MaxSteps', IntToStr(xMaxSteps));
                TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                    cLPipArm + 'PipPump' + IntToStr(x + 1) + 'Driver', 'Adr', IntToStr(xAdr));
                if Length(xCurrentSetting) > 1 then
                    TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                        cLPipArm + 'PipPump' + IntToStr(x + 1) + 'Driver', 'AspSpeed', xCurrentSetting[1]);
                if Length(xCurrentSetting) > 2 then
                    TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                        cLPipArm + 'PipPump' + IntToStr(x + 1) + 'Driver', 'AspRamp', xCurrentSetting[2]);
                if Length(xCurrentSetting) > 3 then
                begin
                    TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                        cLPipArm + 'PipPump' + IntToStr(x + 1) + 'Driver', 'DispSpeed', xCurrentSetting[3]);
                    TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                        cLPipArm + 'PipPump' + IntToStr(x + 1) + 'Driver', 'InitSpeed', xCurrentSetting[3]);
                end;
                if Length(xCurrentSetting) > 4 then
                    TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                        cLPipArm + 'PipPump' + IntToStr(x + 1) + 'Driver', 'DispRamp', xCurrentSetting[4]);

                if Length(xCurrentSetting) > 5 then
                    TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                        cLPipArm + 'PipPump' + IntToStr(x + 1) + 'Driver', 'ValvePos1', xCurrentSetting[5]);
                if Length(xCurrentSetting) > 6 then
                    TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                        cLPipArm + 'PipPump' + IntToStr(x + 1) + 'Driver', 'ValvePos2', xCurrentSetting[6]);
                if Length(xCurrentSetting) > 7 then
                    TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                        cLPipArm + 'PipPump' + IntToStr(x + 1) + 'Driver', 'ValvePos3', xCurrentSetting[7]);
                if Length(xCurrentSetting) > 8 then
                    TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                        cLPipArm + 'PipPump' + IntToStr(x + 1) + 'Driver', 'ValvePos4', xCurrentSetting[8]);
                // Parameter 9 (ValvePos5) ist Quatsch und wird nicht gelesen

                if Length(xCurrentSetting) > 10 then
                    TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER',
                        cLPipArm + 'PipPump' + IntToStr(x + 1) + 'Driver', 'MaxVolume', xCurrentSetting[10]);
            end;
        end;
    end;

    // Grip-Arm anlegen
    if aZP01Config.ContainsKey('hZMotor') and self.ZP01ModuleExists(aZP01Config['hZMotor'], xCurrentSetting,
        xAdr) then
    begin

        ReadZP01GripperArmWorldoffs(aSourceDP, xhXOffset, xhYOffset, xhZOffset);

        // Allgemeine Arm-Daten
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm, 'Type', 'ROBOTARM');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm, 'ArmGroupID', '1');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm, 'Color', 'red');

        xMax_mm := 0;
        if aZP01Config.ContainsKey('hZMax') then
            xMax_mm := Round(StrToIntDef(aZP01Config['hZMax'], 0) / aStepsToMMConv.hZStepsPerMM * 100) / 100;
        xZTravel_mm := 0;
        if aZP01Config.ContainsKey('hZTravel') then // dies ist kein Fehler: ZSteps statt hZSteps!
            xZTravel_mm := xMax_mm - Round(StrToIntDef(aZP01Config['hZTravel'], 0) /
                aStepsToMMConv.ZStepsPerMM * 100) / 100;

        xIniAccess := TDevSimpleIniAccess.Create(aSourceDP, 'ROBOT');

        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm, 'ZTravel',
            FloatToStr(xZTravel_mm, aEnglFormatSettings));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm, 'ToolZTravel',
            FloatToStr(xMax_mm - Round(xIniAccess.ReadInteger('Module', 'ToolZTravel',
            0) / aStepsToMMConv.ZStepsPerMM * 100) / 100, aEnglFormatSettings));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm, 'PipToolZTravel',
            FloatToStr(xMax_mm - Round(xIniAccess.ReadInteger('Module', 'PipToolZTravel',
            0) / aStepsToMMConv.ZStepsPerMM * 100) / 100, aEnglFormatSettings));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm, 'PipToolMinZTravel',
            FloatToStr(xMax_mm - Round(xIniAccess.ReadInteger('Module', 'MinZTravelPipTool',
            0) / aStepsToMMConv.ZStepsPerMM * 100) / 100, aEnglFormatSettings));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm, 'TubeMinZTravel',
            FloatToStr(xMax_mm - Round(xIniAccess.ReadInteger('Module', 'MinZTravelTubes',
            0) / aStepsToMMConv.ZStepsPerMM * 100) / 100, aEnglFormatSettings));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm, 'RackMoveZTravel',
            FloatToStr(xZTravel_mm, aEnglFormatSettings));

        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm, 'Pip', cLGripArm + 'Pip');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'Pip', 'Type',
            'ATTACHEDPIPETTE');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'Pip', 'Tips', '1');

        { TODO : Additional Dilutors }

        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm, 'Motion',
            cLGripArm + 'Motion');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'Motion', 'Type',
            'MOTORBASEDMOTION');

        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'Motion', 'ZMotors',
            cLGripArm + 'MotionZMotors');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionZMotors', 'Type',
            'MULTIZMOTOR');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionZMotors',
            'ZMotorsHaveBlockMoveOption', 'NO');

        // HZ-Motor
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionZMotors', 'Motor1',
            cLGripArm + 'MotionZMotorsMotor1');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionZMotorsMotor1',
            'Type', 'ZMOTOR');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionZMotorsMotor1',
            'TipNo', '1');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionZMotorsMotor1',
            'LiquidDetect', 'NO');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionZMotorsMotor1',
            'Driver', 'HZMotorDriver');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionZMotorsMotor1',
            'StepsPerUnit', FloatToStr(aStepsToMMConv.HZStepsPerMM, aEnglFormatSettings));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionZMotorsMotor1',
            'Min', '0');
        xMax_mm := 0;
        if aZP01Config.ContainsKey('hZMax') then
            xMax_mm := Round(StrToIntDef(aZP01Config['hZMax'], 0) / aStepsToMMConv.hZStepsPerMM * 100) / 100;
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionZMotorsMotor1',
            'Max', FloatToStr(xMax_mm, aEnglFormatSettings));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionZMotorsMotor1',
            'WorldOffset', FloatToStr(xMax_mm + (Round(xhZOffset / aStepsToMMConv.HZStepsPerMM * 100) / 100),
            aEnglFormatSettings));

        // HZMotorDriver
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'HZMotorDriver', 'Type', 'ZP01MOTOR');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'HZMotorDriver', 'Connection',
            TRoboticInterfaceZP01.cZP01Connection);
        if Length(xCurrentSetting) > 0 then
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'HZMotorDriver', 'Adr',
                xCurrentSetting[0]);
        if Length(xCurrentSetting) > 1 then
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'HZMotorDriver', 'Speed',
                xCurrentSetting[1]);
        if Length(xCurrentSetting) > 2 then
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'HZMotorDriver', 'Ramp',
                xCurrentSetting[2]);

        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'Motion', 'YMotors',
            cLGripArm + 'MotionYMotors');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotors', 'Type',
            'VSPANMULTIYMOTOR');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotors',
            'NTips', '1');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotors',
            'RefPos', '0');
        if aZP01Config.ContainsKey('CloseVfor1Tip') and (aZP01Config['CloseVfor1Tip'] = '0') then
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotors',
                'CloseVFor1Tip', 'NO');
        {
          TSettingsTableProviderUtilsV0.AppendRecord( aDestDP, 'DEVICE', cLGripArm + 'MotionYMotors', 'YOffset', '0');
          if aZP01Config.ContainsKey('VFactor') then
          TSettingsTableProviderUtilsV0.AppendRecord( aDestDP, 'DEVICE', cLGripArm + 'MotionYMotors', 'VFactor', aZP01Config['VFactor']);
          if aZP01Config.ContainsKey('VOffset') then
          TSettingsTableProviderUtilsV0.AppendRecord( aDestDP, 'DEVICE', cLGripArm + 'MotionYMotors', 'VOffset', aZP01Config['VOffset']);
        }
        // wo auch immer diese Werte herkommen:
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotors', 'VFactor',
            FloatToStr(31, aEnglFormatSettings));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotors', 'VOffset',
            FloatToStr(24.23, aEnglFormatSettings));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotors', 'YOffset',
            FloatToStr(0, aEnglFormatSettings));

        // X-Motor
        if aZP01Config.ContainsKey('XMotor') and self.ZP01ModuleExists(aZP01Config['XMotor'], xCurrentSetting,
            xAdr) then
        begin
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'Motion', 'XMotor',
                cLGripArm + 'MotionXMotor');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionXMotor', 'Type',
                'XMOTOR');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionXMotor',
                'Driver', 'XMotorDriver');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionXMotor',
                'StepsPerUnit', FloatToStr(aStepsToMMConv.XStepsPerMM, aEnglFormatSettings));
            xMax_mm := 0;
            if aZP01Config.ContainsKey('XMax') then
                xMax_mm := Round(StrToIntDef(aZP01Config['XMax'], 0) / aStepsToMMConv.XStepsPerMM *
                    100) / 100;
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionXMotor', 'Min',
                FloatToStr(0, aEnglFormatSettings));
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionXMotor', 'Max',
                FloatToStr(xMax_mm, aEnglFormatSettings));
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionXMotor',
                'WorldOffset', FloatToStr(TAppSettings.XOffsetLeftToTip1 +
                (Round(xhXOffset / aStepsToMMConv.XStepsPerMM * 100) / 100), aEnglFormatSettings));
        end;

        // Y-Motor
        if aZP01Config.ContainsKey('YMotor') and self.ZP01ModuleExists(aZP01Config['YMotor'], xCurrentSetting,
            xAdr) then
        begin
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotors',
                'YMotor', cLGripArm + 'MotionYMotorsYMotor');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotorsYMotor',
                'Type', 'YMOTOR');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotorsYMotor',
                'Reverse', 'YES');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotorsYMotor',
                'Driver', 'YMotorDriver');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotorsYMotor',
                'StepsPerUnit', FloatToStr(aStepsToMMConv.YStepsPerMM, aEnglFormatSettings));
            xMax_mm := 0;
            if aZP01Config.ContainsKey('YMax') then
                xMax_mm := Round(StrToIntDef(aZP01Config['YMax'], 0) / aStepsToMMConv.YStepsPerMM *
                    100) / 100;
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotorsYMotor',
                'Min', FloatToStr(0, aEnglFormatSettings));
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotorsYMotor',
                'Max', FloatToStr(xMax_mm, aEnglFormatSettings));
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotorsYMotor',
                'WorldOffset', FloatToStr(TAppSettings.YOffsetFrontToTip1 +
                (Round(xhYOffset / aStepsToMMConv.YStepsPerMM * 100) / 100), aEnglFormatSettings));
        end;

        // V-Motor
        if (aZP01Config.ContainsKey('hVMotor') and self.ZP01ModuleExists(aZP01Config['hVMotor'],
            xCurrentSetting, xAdr)) or (aZP01Config.ContainsKey('VMotor') and
            self.ZP01ModuleExists(aZP01Config['VMotor'], xCurrentSetting, xAdr)) then
        begin
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotors',
                'VMotor', cLGripArm + 'MotionYMotorsVMotor');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotorsVMotor',
                'Type', 'VMOTOR');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotorsVMotor',
                'Driver', 'VMotorDriver');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotorsVMotor',
                'StepsPerUnit', FloatToStr(aStepsToMMConv.YStepsPerMM * 2, aEnglFormatSettings));
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotorsVMotor',
                'WorldOffset', '0');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotorsVMotor',
                'Min', '0');
            xMax_mm := 0;
            if aZP01Config.ContainsKey('VMax') then
                xMax_mm := Round(StrToIntDef(aZP01Config['VMax'], 0) / aStepsToMMConv.YStepsPerMM / 2 *
                    100) / 100;
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'MotionYMotorsVMotor',
                'Max', FloatToStr(xMax_mm, aEnglFormatSettings));

            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm, 'Grip',
                cLGripArm + 'Grip');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'Grip', 'Type',
                'MOTORGRIP');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'Grip', 'Motor',
                cLGripArm + 'GripMotor');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'GripMotor', 'Type',
                'GMOTOR');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'GripMotor', 'Driver',
                'VMotorDriver');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'GripMotor',
                'StepsPerUnit', FloatToStr(aStepsToMMConv.YStepsPerMM * 2, aEnglFormatSettings));
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'GripMotor',
                'WorldOffset', '0');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'GripMotor',
                'Min', '0');
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', cLGripArm + 'GripMotor', 'Max',
                FloatToStr(xMax_mm, aEnglFormatSettings));
        end;
    end;

    // Washer und Reader werden nicht konvertiert

    // Für jeden Port einen Driver und ein Device anlegen
    if aZP01Config.ContainsKey('PeriPump') and self.ZP01ModuleExists(aZP01Config['PeriPump'], xCurrentSetting,
        xAdr) then
    begin
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', 'PeriPump', 'Type', 'PERIPUMP');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', 'PeriPump', 'Driver', 'PeriPumpDriver');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'PeriPumpDriver', 'Type',
            'ZP01PERIPUMP');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'PeriPumpDriver', 'Connection',
            TRoboticInterfaceZP01.cZP01Connection);
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'PeriPumpDriver', 'Adr',
            IntToStr(xAdr));
        if Length(xCurrentSetting) > 1 then
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'PeriPumpDriver', 'Bit',
                xCurrentSetting[1]);
        if aZP01Config.ContainsKey('PeriSpeed') then
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'PeriPumpDriver', 'PeriSpeed',
                aZP01Config['PeriSpeed'])
        else
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'PeriPumpDriver',
                'PeriSpeed', '2000');
    end;

    if aZP01Config.ContainsKey('Beeper') and self.ZP01ModuleExists(aZP01Config['Beeper'], xCurrentSetting,
        xAdr) then
    begin
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', 'Beeper', 'Type', 'SWITCH');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', 'Beeper', 'Driver', 'BeeperDriver');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'BeeperDriver', 'Type', 'ZP01SWITCH');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'BeeperDriver', 'Connection',
            TRoboticInterfaceZP01.cZP01Connection);
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'BeeperDriver', 'Adr', IntToStr(xAdr));
        if Length(xCurrentSetting) > 1 then
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'BeeperDriver', 'Port',
                xCurrentSetting[1]);
    end;

    if aZP01Config.ContainsKey('Stirer') and self.ZP01ModuleExists(aZP01Config['Stirer'], xCurrentSetting,
        xAdr) then
    begin
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', 'Stirrer01', 'Type', 'SWITCH');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DEVICE', 'Stirrer01', 'Driver',
            'Stirrer01Driver');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'Stirrer01Driver', 'Type',
            'ZP01SWITCH');
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'Stirrer01Driver', 'Connection',
            TRoboticInterfaceZP01.cZP01Connection);
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'Stirrer01Driver', 'Adr',
            IntToStr(xAdr));
        if Length(xCurrentSetting) > 1 then
            TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'DRIVER', 'Stirrer01Driver', 'Port',
                xCurrentSetting[1]);
    end;
end;

constructor TDeviceConversion.Create(aSourceDP: TDataProvider; aDestDP: TDataProvider;
    aStepsToMMConv: TStepsToMMConverter);
begin
    fDestDP := aDestDP;
    fStepsToMMConv := aStepsToMMConv; // nur übernehmen: werden nicht zerstört
    fSourceDP := aSourceDP;
end;

class procedure TDeviceConversion.ConvertAll(aSourceDP, aDestDP: TDataProvider;
    aStepsToMMConv: TStepsToMMConverter);
var
    xZP01Config: TDictionary<string, string>;
    xWashRetrSpeed: integer;
    xAllDevices: TActionModules;
    xIsZP01: boolean;
    xEnglFormatSettings: TFormatSettings;
begin
    xEnglFormatSettings := TFormatSettings.Create('en-US');

    // WashRetrSpeed lesen
    xWashRetrSpeed := TIniSettings.ReadInteger(aSourceDP, 'APPLICATION', 'Pipetting', 'WashRetractSpeed', 0);

    xZP01Config := TDictionary<string, string>.Create;
    try
        // ZP01 oder ZP02
        aSourceDP.SelectAndOpen('select * from settings s where ' + TSettingsSQL.AreaEq('s',
            'ZP01CONFIG'), true);
        xIsZP01 := not aSourceDP.Eof;
        while not aSourceDP.Eof do
        begin
            xZP01Config.Add(aSourceDP.FieldByName('IDENT').AsString, aSourceDP.FieldByName('VALUE').AsString);
            aSourceDP.Next;
        end;
        aSourceDP.Close;

        TAppSettings.Init(not xIsZP01);

        gCommManager := TCommunicationRealManager.Create(aSourceDP);

        xAllDevices := TActionModules.Create('AllDevices', dptNone);
        try
            // Device-Daten lesen
            xAllDevices.Prepare(aSourceDP, ReadRediDeviceNames(aSourceDP));

            // Device-Daten schreiben
            xAllDevices.WriteDeviceIniFile(aSourceDP, aDestDP, xIsZP01, xZP01Config, aStepsToMMConv,
                xWashRetrSpeed, xEnglFormatSettings);
        finally
            xAllDevices.Free;
        end;
    finally
        xZP01Config.Free;
    end;
end;

class function TDeviceConversion.ReadRediDeviceNames(aSourceDP: TDataProvider): TArray<string>;
var
    xNames: TList<string>;
begin
    xNames := TList<string>.Create;
    try
        aSourceDP.SelectAndOpen('select distinct REDIDEVICENAME from "TIPTYPES"', true);
        try
            while not aSourceDP.Eof do
            begin
                if (aSourceDP.FieldByName('REDIDEVICENAME').AsString <> '') then
                    xNames.Add(aSourceDP.FieldByName('REDIDEVICENAME').AsString);

                aSourceDP.Next;
            end;
        finally
            aSourceDP.Close;
        end;

        result := xNames.ToArray();
    finally
        xNames.Free;
    end;
end;

procedure TDeviceConversion.Convert();
begin
    // Area DEVICE_OLD löschen
    fDestDP.ExecSQL('DELETE from "settings" where ' + TSettingsSQL.AreaEq('settings', 'DEVICE'));

    // Devices extrahieren und umschreiben
    self.ConvertAll(fSourceDP, fDestDP, fStepsToMMConv);
end;

{ TActionModules }

procedure TActionModules.Prepare(aSourceDP: TDataProvider; aRediDeviceNames: TArray<string>);
const
    cDevArea = 'DEVICE';
begin
    // aus Create:

    ReadDeviceIniFile(aSourceDP, cDevArea, aRediDeviceNames);

    AddSystemLiquidValve(aSourceDP, cDevArea);

    CreateComModules(aSourceDP, cDevArea);

    // sub-devices laden
    LoadArmDevices(aSourceDP, cDevArea);
end;

procedure TActionModules.CreateComModules(aSettingDP: TDataProvider; const aDevArea: string);
begin
    CreateBCReaders(aSettingDP);
    CreateBalance(aSettingDP);
    CreateSystemDevices(aSettingDP, aDevArea);
end;

procedure TActionModules.ReadDeviceIniFile(aSettingDP: TDataProvider; const aArea: string;
    aRediDeviceNames: TArray<string>);
var
    iDevNo: integer;
    iDeviceName: string;
begin
    iDevNo := 0;
    repeat
        // ------------------------------------------------------------------------ Device-Namen lesen
        inc(iDevNo);
        iDeviceName := TIniSettings.ReadString(aSettingDP, aArea, 'DEVICES', 'DEVICE' + inttostr(iDevNo), '');
        // --------------------------------------------------------------- Daten der DeviceParts lesen
        if not ReadAndAddComposedDevice(aSettingDP, aArea, iDeviceName, INT_FIRST_DEVICE_LEVEL,
            aRediDeviceNames) then
            BREAK;

    until (iDeviceName = '');
end;

procedure TActionModules.WriteDeviceIniFile(aSourceDP, aDestDP: TDataProvider; aIsZP01: boolean;
    aZP01Config: TDictionary<string, string>; aStepsToMMConv: TStepsToMMConverter; aWashRetrSpeed: integer;
    aEnglFormatSettings: TFormatSettings);
var
    x: integer;
    xTempStr: string;
    xCurZMax: double;
    xGlobalZMax_mm: double;
    xXOffsetLeftToTip1_mm: double;
    xYOffsetFrontToTip1_mm: double;
    xFirstPipDeviceName: string;
begin
    // Diese Werte sollen mal abgefragt werden:
    xTempStr := InputBox('X Offset: Distance between the left edge of the workbench to Tip 1 [mm]',
        'Conversion values', '100');
    xXOffsetLeftToTip1_mm := StrToFloatDef(xTempStr, -1, TFormatSettings.Create('en-US'));
    if xXOffsetLeftToTip1_mm = -1 then
        xXOffsetLeftToTip1_mm := StrToFloatDef(xTempStr, -1, TFormatSettings.Create('de-DE'));

    xTempStr := InputBox('Y Offset: Distance between the front edge of the workbench to Tip 1 [mm]',
        'Conversion values', '510');
    xYOffsetFrontToTip1_mm := StrToFloatDef(xTempStr, -1, TFormatSettings.Create('en-US'));
    if xYOffsetFrontToTip1_mm = -1 then
        xYOffsetFrontToTip1_mm := StrToFloatDef(xTempStr, -1, TFormatSettings.Create('de-DE'));

    TAppSettings.XOffsetLeftToTip1 := xXOffsetLeftToTip1_mm;
    TAppSettings.YOffsetFrontToTip1 := xYOffsetFrontToTip1_mm;

    // Settings neu erstellen
    aDestDP.SelectAndOpen('select * from settings', false);
    try
        xGlobalZMax_mm := 0;

        // ZP01CONFIG konvertieren und schreiben
        if aIsZP01 then
        begin
            TActionModules.WriteZP01Config(aSourceDP, aDestDP, aZP01Config, aStepsToMMConv, aWashRetrSpeed,
                xGlobalZMax_mm, aEnglFormatSettings);
            xFirstPipDeviceName := 'LPipArmPip';
        end;

        // Alle Sub-Devices schreiben
        for x := 0 to fDeviceList.Count - 1 do
        begin
            fDeviceList[x].WriteDeviceData(aDestDP);

            if (fDeviceList[x] is TRobotArmDevice) then
            begin
                xCurZMax := (fDeviceList[x] as TRobotArmDevice).BasicZTravelValue;
                if (xCurZMax > xGlobalZMax_mm) then
                    xGlobalZMax_mm := xCurZMax;
            end;
        end;

        // Werte speichern, der für das Layout-Update V2 benötigt werden
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'ROBOT', 'TempConversion', 'GlobalZTravel_mm',
            FloatToStr(xGlobalZMax_mm));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'ROBOT', 'TempConversion', 'XOffsetLeftToTip1_mm',
            FloatToStr(xXOffsetLeftToTip1_mm));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'ROBOT', 'TempConversion',
            'YOffsetFrontToTip1_mm', FloatToStr(xYOffsetFrontToTip1_mm));
        TSettingsTableProviderUtilsV0.AppendRecord(aDestDP, 'ROBOT', 'TempConversion', 'FlushPipDevice',
            xFirstPipDeviceName);
    finally
        aDestDP.Close;
    end;
end;

procedure TActionModules.LoadArmDevices(aSettingDP: TDataProvider; const aDevArea: string);
var
    x: integer;
begin
    for x := 0 to fDeviceList.Count - 1 do
    begin
        if (fDeviceList[x] is TRobotArmDevice) then
            (fDeviceList[x] as TRobotArmDevice).LoadArmDevices(aSettingDP, aDevArea);
    end;
end;

procedure TActionModules.AddSystemLiquidValve(aSettingDP: TDataProvider; const aArea: string);
const
    STR_RELAYVALVE = 'RelayValve';
    STR_AREA_LIQUIDSYSTEM = 'LiquidSystem';
    STR_ISEC_SYSLIQVALVE = 'SystemLiquidValve';
    STR_NAME_SIXWAYVALVE_1 = 'SixWayValve1';
    // Name of the first 6-Way-Valve (defined in Application Settings)
var
    xIniAccess: ISimpleIniAccess;
    xDevice: TXWayValveDevice;
    x, xCom, xNoOfPorts: integer;
    xFirstRelaysValveName, xRelaysValveName, xInPort1Name, xInPort2Name: string;
begin
    xFirstRelaysValveName := TLiquids.EncodeSystemLiquidIdent(5);
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, 'APPLICATION');

    // RelaisVentil
    xCom := xIniAccess.ReadInteger(STR_ISEC_SYSLIQVALVE, 'RelayValveComPort', 0);
    if (xCom > 0) then
    begin
        xNoOfPorts := xIniAccess.ReadInteger(STR_ISEC_SYSLIQVALVE, 'RelayValveNoOfLiqPorts', 0);
        for x := 0 to xNoOfPorts - 1 do
        begin
            xRelaysValveName := STR_RELAYVALVE + IntToStr(x + 1);
            if (x = 0) then
                xFirstRelaysValveName := xRelaysValveName;

            xInPort1Name := TLiquids.EncodeSystemLiquidIdent(x + 5);
            if (x = xNoOfPorts - 1) then
                xInPort2Name := TLiquids.EncodeSystemLiquidIdent(x + 6)
            else
                xInPort2Name := STR_RELAYVALVE + IntToStr(x + 2);
            xDevice := TXWayValveSwitchDevice.CreateOldStyle(aSettingDP, aArea, xRelaysValveName,
                STR_AREA_LIQUIDSYSTEM, xCom, x + 1, xInPort1Name, xInPort2Name);
            FDeviceList.Add(xDevice);
        end;
    end;

    // Omnifit 6-Wege-Ventil
    xCom := xIniAccess.ReadInteger(STR_ISEC_SYSLIQVALVE, 'ComPort', -1);
    if (xCom > 0) then
    begin
        xDevice := T6WayValveDevice.CreateOldStyle(STR_NAME_SIXWAYVALVE_1, STR_AREA_LIQUIDSYSTEM, xCom,
            xIniAccess.ReadInteger(STR_ISEC_SYSLIQVALVE, 'Timeout', 20),
            xIniAccess.ReadInteger(STR_ISEC_SYSLIQVALVE, 'SleepTime', 500),
            xIniAccess.ReadInteger(STR_ISEC_SYSLIQVALVE, 'RetryCnt', 5), xFirstRelaysValveName);
        FDeviceList.Add(xDevice);
    end;
end;

class function TActionModules.FindMotorExecID(aDevice: TDevice; aAdr: integer): integer;
var
    x: integer;
    xMotor: TMotorDevice;
    xCompDev: TCompositeDevice;
begin
    result := 0;
    if (aDevice is TMotorDevice) then
    begin
        xMotor := aDevice as TMotorDevice;
        if xMotor.Adr = aAdr then
        begin
            result := xMotor.ExecID;
            EXIT;
        end;
        EXIT;
    end;

    if not(aDevice is TCompositeDevice) then
        EXIT;
    xCompDev := (aDevice as TCompositeDevice);

    for x := 0 to xCompDev.Devices.Count - 1 do
    begin
        result := FindMotorExecID(xCompDev.Devices[x], aAdr);
        if (result <> 0) then
            EXIT;
    end;
end;

procedure TActionModules.CreateBCReaders(aSettingDP: TDataProvider);
var
    xIniAccess: ISimpleIniAccess;
    xBCRDev: TBCReaderDevice;
    xCDevice: TReadingDevice;
    xTurntable: TBCTurntableDevice;
    xTTMotor: MOTOR;
    xBCRData: BARCODEREC;
    xBCPosition: BCRPOSITION;
    xBCScanRange: BCSCANRANGE;
    xDelLastPos: integer;
    xTurntableBit: integer;
    xDllName, xInitFunc, xDllReadBcFunc, xDllReadBcPara, xTurnSwitchName: string;
    xTurnSwitchDev: TDevice;
    xTurnSwitch: TSwitchDevice;
    xExecID: integer;
begin
    xBCRDev := nil;
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, 'ROBOT');
    BCRackLeadZero := xIniAccess.ReadInteger(STR_ISEC_BARCODE, 'MTP_BCR_LeadZero', 0);
    BCTubeLeadZero := xIniAccess.ReadInteger(STR_ISEC_BARCODE, 'TUBE_BCR_LeadZero', 0);

    // external dll rack reader (Action ReadE)
    xDllName := xIniAccess.ReadString(STR_ISEC_BARCODE, 'ReadE_DLLName', '');
    xDllReadBcFunc := xIniAccess.ReadString(STR_ISEC_BARCODE, 'ReadE_DLLFunc', '');
    xDllReadBcPara := xIniAccess.ReadString(STR_ISEC_BARCODE, 'ReadE_DLLParam', '');
    if (xDllName <> '') and (xDllReadBcFunc <> '') then
        FDeviceList.Add(TRackDllBCReaderDevice.Create(xDllName, xDllReadBcFunc, xDllReadBcPara));

    // Tube Barcode Reader
    xBCRData := xIniAccess.ReadBarcoderec(STR_ISEC_BARCODE, 'TUBE_BCR');
    TReadingDevice.IniReadBCRPosition(aSettingDP, 'TUBE_BCR_Pos', false, xBCPosition);
    xBCScanRange := xIniAccess.ReadBcscanrange(STR_ISEC_BARCODE, 'TUBE_BCR_ScanXYZRange');
    xDelLastPos := xIniAccess.ReadInteger(STR_ISEC_BARCODE, 'TUBE_BCR_DelLastPos', 0);
    if (xBCRData.Adr > 0) and (xBCRData.Adr < ZP01_MAX_ADR) then
    begin

        // create BC reader device
        xBCRDev := TBCReaderDevice.CreateOldStyle('BCReaderTubes', STR_AREANAME_ROBOT, xBCRData.Pattern);
        xBCRDev.Intf := gCommManager.GetZP01BCReaderIntf(xBCRData.Adr, xBCRData.BcType, xBCRData.InitStr, 0);
        // create reading device
        xCDevice := TTubeReadingDevice.Create(xBCRDev, xBCPosition, xBCScanRange, xDelLastPos, nil, 0, nil);
        FDeviceList.Add(xCDevice);
    end
    else
    begin
        // Create External tube reader
        xDllName := xIniAccess.ReadString(STR_ISEC_BARCODE, 'TUBE_BCR_DllName', '');
        xInitFunc := xIniAccess.ReadString(STR_ISEC_BARCODE, 'TUBE_BCR_DllInit', 'Init');
        if (xDllName <> '') and (xInitFunc <> '') then
        begin

            // Create Turntable motor device
            xTurntable := nil;
            xTTMotor := xIniAccess.ReadMotor(STR_ISEC_BARCODE, 'TUBE_BCR_TurnTable', 300, 2);
            if (xTTMotor.Adr > 0) then
            begin
                if (xIniAccess.ReadBool(STR_ISEC_BARCODE, 'TUBE_BCR_TurnRelay', false)) then
                    xTurntableBit := 1 // Port 1 ist festgelegt!!!!
                else
                    xTurntableBit := -1;

                xTurntable := TBCTurntableDevice.Create('TurntableTubeBCR', 'TubeBCR', xTTMotor,
                    xIniAccess.ReadInteger(STR_ISEC_BARCODE, 'TUBE_BCR_TurnSteps', 5000),
                    xIniAccess.ReadInteger(STR_ISEC_BARCODE, 'TUBE_BCR_TurnStepsWrapAt', 0), xTurntableBit);
                xExecID := FindMotorExecID(self, xTTMotor.Adr);
                if xExecID > 0 then
                    xTurnTable.ExecID := xExecID;
            end;

            // Create BC reader device
            xBCRDev := TBCReaderDevice.CreateOldStyle('ExternalTubeBCReader', 'TubeBCR',
                xIniAccess.ReadString(STR_ISEC_BARCODE, 'TUBE_BCR_CodeFilter', '*'));
            xBCRDev.Intf := gCommManager.GetDLLBCReaderIntf(aSettingDP, xDllName);

            // neu: TurnSwitchDevice (quick & dirty Schrott) - muss später anders gemacht werden
            xTurnSwitchName := xIniAccess.ReadString(STR_ISEC_BARCODE, 'TUBE_BCR_TurnSwitchDevice', '');
            xTurnSwitch := nil;
            if (xTurnSwitchName <> '') then
            begin
                xTurnSwitchDev := self.Find_ByName(xTurnSwitchName);
                if (xTurnSwitchDev is TSwitchDevice) then
                    xTurnSwitch := xTurnSwitchDev as TSwitchDevice;
            end;

            // create reading device
            xCDevice := TTubeReadingDevice.Create(xBCRDev, xBCPosition, xBCScanRange,
                xIniAccess.ReadInteger(STR_ISEC_BARCODE, 'TUBE_BCR_DelLastPos', 0), xTurntable,
                xIniAccess.ReadInteger(STR_ISEC_BARCODE, 'TUBE_BCR_NoOfTurns', 5), xTurnSwitch);
            FDeviceList.Add(xCDevice);
        end;
    end;

    CreateMTPReader(aSettingDP, xDLLName, xBCRDev);

end;

procedure TActionModules.CreateMTPReader(aSettingDP: TDataProvider; const aDLLName: string;
    const aTubeBCRDev: TBCReaderDevice);
var
    xIniAccess: ISimpleIniAccess;
    xBCRData: BARCODEREC;
    xRackCodeFilter: string;
    xUseTubeDevice: boolean;
    xUseDevice: boolean;
    xUseDeviceName: string;
    xDevice: TDevice;
    xBCRDev: TBCReaderDevice;
    xBCPosition: BCRPOSITION;
    xBCScanRange: BCSCANRANGE;
    xDelLastPos: integer;
    xCDevice: TReadingDevice;
begin

    // MTP Barcode Reader
    xRackCodeFilter := STR_USE_BCR_FILTER;
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, 'ROBOT');
    xBCRData := xIniAccess.ReadBarcoderec(STR_ISEC_BARCODE, 'MTP_BCR');

    xBCRDev := nil;
    xUseTubeDevice := false;
    xUseDevice := false;

    if aDllName <> '' then
    begin
        if (UpperCase(xBCRData.InitStr) = 'USETUBEBCR') then
        begin
            xUseDevice := true;
            xUseTubeDevice := true;
            xBCRDev := aTubeBCRDev;
        end
        else if Pos('USEDEVICEBCR=', UpperCase(xBCRData.InitStr)) = 1 then
        begin
            xUseDevice := true;
            xUseDeviceName := Copy(xBCRData.InitStr, Length('USEDEVICEBCR=') + 1, Length(xBCRData.InitStr));
            xDevice := self.Find_ByName(xUseDeviceName);
            if xDevice is TBCReaderDevice then
                xBCRDev := xDevice as TBCReaderDevice;
        end;
    end;

    if xUseDevice then
    begin
        xRackCodeFilter := xIniAccess.ReadString(STR_ISEC_BARCODE, 'MTP_BCR_CodeFilter', '*');
        if xUseTubeDevice then
        begin
            // reader can be used for rack & tubes
            xBCRDev.Name := 'BCReaderRackTube2D';
        end;
    end
    else
    begin
        xBCRDev := nil;
        if (xBCRData.Adr > 0) and (xBCRData.Adr < ZP01_MAX_ADR) then
        begin // create BC reader device
            // create BC reader device
            xBCRDev := TBCReaderDevice.CreateOldStyle('BCReaderRacks', STR_AREANAME_ROBOT, xBCRData.Pattern);
            xBCRDev.Intf := gCommManager.GetZP01BCReaderIntf(xBCRData.Adr, xBCRData.BcType,
                xBCRData.InitStr, 0);
        end;
    end;
    // create rack reading device
    if (xBCRDev <> nil) then
    begin
        TReadingDevice.IniReadBCRPosition(aSettingDP, 'MTP_BCR_Pos', false, xBCPosition);
        xBCScanRange := xIniAccess.ReadBcscanrange(STR_ISEC_BARCODE, 'MTP_BCR_ScanXYZRange');
        xDelLastPos := xIniAccess.ReadInteger(STR_ISEC_BARCODE, 'MTP_BCR_DelLastPos', 0);
        xCDevice := TRackReadingDevice.Create(xBCRDev, xBCPosition, xBCScanRange, xRackCodeFilter,
            xDelLastPos);
        FDeviceList.Add(xCDevice);
    end;
end;

procedure TActionModules.CreateBalance(aSettingDP: TDataProvider);
var
    xIniAccess: ISimpleIniAccess;
    xBalName: string;
    xBalDev: TBalanceDevice;
begin
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, 'APPLICATION');
    xBalName := xIniAccess.ReadString('Balance', 'Balance1', '');
    if (xBalName <> '') then
    begin
        xBalDev := TBalanceDeviceFactory.CreateBalanceDeviceOldStyle(aSettingDP, xBalName,
            xIniAccess.ReadInteger('Balance', 'Balance1_Left', 0), xIniAccess.ReadInteger('Balance',
            'Balance1_Top', 0), xIniAccess.ReadInteger('Balance', 'TaraAfterXTimes', 75),
            xIniAccess.ReadBool('Balance', 'CloseDoorAfterInit', false));
        FDeviceList.Add(TWeighingDevice.CreateOldStyle('WEIGHING', xBalDev));

        // ReadInteger('Balance','TaraAfterXMin');  wird zur Zeit nicht benutzt!
    end;
end;

procedure TActionModules.CreateSystemDevices(aSettingDP: TDataProvider; const aDevArea: string);
var
    xComPort1, xComPort2: integer;
    xStateSignal: TStateSignalDevice;
    xIniAccess: ISimpleIniAccess;
    xDevice: TCompositeDevice;
begin
    // Create System State Device
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, 'ROBOT');
    xComPort1 := xIniAccess.ReadInteger(STR_ISEC_STATEMONITOR, 'ComPort', -1);
    if (xComPort1 > 0) then
    begin
        xStateSignal := TStateSignalDevice.Create('State Signal');
        FDeviceList.Add(xStateSignal);
        xStateSignal.AddSwitch(aSettingDP, aDevArea, 'Active 1', sStateActive, xComPort1,
            xIniAccess.ReadString(STR_ISEC_STATEMONITOR, 'StateActivePort', '-1'));
        xStateSignal.AddSwitch(aSettingDP, aDevArea, 'Active 2', sStateActive, xComPort1,
            xIniAccess.ReadString(STR_ISEC_STATEMONITOR, 'AddActivePort', '-1'));
        xStateSignal.AddSwitch(aSettingDP, aDevArea, 'Setup 1', sStateSetup, xComPort1,
            xIniAccess.ReadString(STR_ISEC_STATEMONITOR, 'StateSetupPort', '-1'));
        xStateSignal.AddSwitch(aSettingDP, aDevArea, 'Error 1', sStateError, xComPort1,
            xIniAccess.ReadString(STR_ISEC_STATEMONITOR, 'StateErrorPort', '-1'));
    end;

    // Create User Protection Request Port
    xComPort2 := xIniAccess.ReadInteger(STR_ISEC_USERPROTECT, 'RequestComPort', -1);
    if (xComPort2 > 0) then
    begin
        xDevice := TUserProtectionDevice.CreateOldStyle(aSettingDP, aDevArea, STR_ISEC_USERPROTECT,
            'DEVICE_' + STR_ISEC_USERPROTECT, false);
        fDeviceList.Add(xDevice);
    end;
end;

function TActionModules.Find_ByName(aName: string): TDevice;
begin
    result := nil;

    inherited Find_ByName(result, aName);

    // Log if device is not found
    if (result = nil) then
        TLogManager.Instance.Log('Device ' + aName + ' not found!', 0);
end;


end.
