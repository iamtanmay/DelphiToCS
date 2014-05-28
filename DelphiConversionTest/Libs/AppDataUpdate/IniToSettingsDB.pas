{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  30.01.08 wl  ConvertSamplerIni            TN4003   OpenFixationAtInit --> CatMixConnection
  24.06.08 pk                               TN4148   uses UpdateManagerDataProvider instead of dataprovider
  15.02.11 pk                               TN4780   changes needed to make UpdateManager compatible with TurboDB
  11.04.11 wl                               TN5549   uses Fileutilities
  ---------------------------------------------------------------------------------------------------------------------- }

unit IniToSettingsDB;


interface


uses
    Update,
    TableUpdate;

type
    TIniFieldType = (iftString, iftBoolean, iftInteger, iftFloat);

    TIniPlatformType = (iptNone, iptZP01, iptZP02, iptCRS);

    // table updates
    TSettingsTableUpdateV1_1 = class(TTableUpdate)
    private
        procedure Convert(aSender: TObject);
        procedure ConvertMachineIni();
        procedure ConvertSamplerIni();
        procedure ConvertPlatformIni;
        procedure ConvertSiasRobotIni;
        function DeterminePlatform: TIniPlatformType;
    protected
        procedure DoVersionCheck(); override;
    public
        constructor Create(aUpdateID: TUpdateID);
    end;


implementation


uses
    SysUtils,
    IniFiles,
    graphics,
    DataProvider,
    TableChangeAdaptor,
    SettingsTableUpdate,
    FileUtilities;

const
    STR_SETTING_AREA_ZP01CONFIG = 'ZP01CONFIG';
    STR_SETTING_AREA_ROBOT = 'ROBOT';
    STR_SETTING_AREA_APP = 'APPLICATION';
    STR_SETTINGS_SELECT_ALL = 'SELECT * FROM SETTINGS';

    STR_ISEC_VORTEXER = 'VORTEXER';
    STR_ISEC_MODULE = 'Module';
    STR_ISEC_BARCODE = 'BARCODE';
    STR_ISEC_SYSLIQVALVE = 'SystemLiquidValve';
    STR_ISEC_LASTRBLOCK = 'LastReactionBlock';
    STR_ISEC_TUBETOOLDATA = 'TubeToolData';
    STR_ISEC_ACTIONDEPENDENT = 'ActionDependent';
    STR_ISEC_METHODPARAMS = 'MethodParameters';

procedure ConvertSettingExt(aDataProvider: TDataProvider; aIniFile: TIniFile;
    const aIniSection, aIniIdent: string; const aSettingArea, aSettingSection, aSettingIdent,
    aDefault: string; aIniFieldType: TIniFieldType; aMustWriteDefault: boolean);
var
    xIniValue: string;
    xIntDummy: integer;
    xFloatDummy: double;

    procedure RaiseTypeException(const aTypeAsStr: string);
    begin
        raise Exception.CreateFmt('Could not convert value %s to %s type', [xIniValue, aTypeAsStr]);
    end;

begin
    try
        if not aMustWriteDefault then
            if not aIniFile.ValueExists(aIniSection, aIniIdent) then
                EXIT;

        xIniValue := aIniFile.ReadString(aIniSection, aIniIdent, aDefault);

        // check type
        case aIniFieldType of
            iftBoolean:
                if (xIniValue <> '1') and (xIniValue <> '0') then
                    RaiseTypeException('Boolean');

            iftInteger:
                if not TryStrToInt(xIniValue, xIntDummy) then
                    RaiseTypeException('Integer');

            iftFloat:
                if not TryStrToFloat(xIniValue, xFloatDummy) then
                    RaiseTypeException('Float');
        end;

        aDataProvider.Append;
        aDataProvider.FieldByName('AREA').AsString := aSettingArea;
        aDataProvider.FieldByName('SECTION').AsString := aSettingSection;
        aDataProvider.FieldByName('IDENT').AsString := aSettingIdent;
        aDataProvider.FieldByName('VALUE').AsString := xIniValue;
        aDataProvider.Post;
    except
        on E: Exception do
            raise Exception.CreateFmt('Could not convert Section %s, Ident %s --> %s',
                [aIniSection, aIniIdent, E.Message]);
    end;
end;

procedure ConvertSetting(aDataProvider: TDataProvider; aIniFile: TIniFile;
    const aArea, aSection, aIdent: string; aDefault: string; aIniFieldType: TIniFieldType;
    aMustWriteDefault: boolean);
begin
    ConvertSettingExt(aDataProvider, aIniFile, aSection, aIdent, aArea, aSection, aIdent, aDefault,
        aIniFieldType, aMustWriteDefault);
end;

{ TSettingsTableUpdateV1_1 }

constructor TSettingsTableUpdateV1_1.Create(aUpdateID: TUpdateID);
begin
    inherited Create(aUpdateID, TSettingsTableStructDefV1, INT_SETTINGS_MAJORREVISION_1,
        INT_SETTINGS_MINORREVISION_1);
    self.AlterStructure(TSettingsTableStructDefV1);
    CopyMatchingFields([]);
    self.CustomDataFunc(Convert);
end;

procedure TSettingsTableUpdateV1_1.ConvertMachineIni();
var
    xSettingDP: TDataProvider;
    xIniFile: TIniFile;
    xIniPath: string;
    x: integer;

    procedure ConvertZP01Setting(const aSection, aIdent, aDefault: string; aIniFieldType: TIniFieldType);
    begin
        ConvertSetting(xSettingDP, xIniFile, STR_SETTING_AREA_ZP01CONFIG, aSection, aIdent, aDefault,
            aIniFieldType, false);
    end;
    procedure ConvertRobotSetting(const aSection, aIdent, aDefault: string; aIniFieldType: TIniFieldType);
    begin
        ConvertSetting(xSettingDP, xIniFile, STR_SETTING_AREA_ROBOT, aSection, aIdent, aDefault,
            aIniFieldType, false);
    end;

begin
    xIniPath := TUpdatePaths.Instance.DataPath + 'Machine.ini';
    if (not TFileUtilities.FileExists(xIniPath)) then
        raise Exception.CreateFmt('Ini file %s does not exist', [xIniPath]);

    xSettingDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xIniFile := TIniFile.Create(xIniPath);
        try
            xSettingDP.SelectAndOpen(STR_SETTINGS_SELECT_ALL, false);
            try
                ConvertZP01Setting('Machine', 'TipWash', '0', iftInteger);
                ConvertZP01Setting('Machine', 'nTips', '4', iftInteger);
                ConvertZP01Setting('Machine', 'Options', '0', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'XMotor', '-16,5000,5', iftString);
                ConvertZP01Setting(STR_ISEC_MODULE, 'XMax', '6000', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'YMotor', '-15,5000,5', iftString);
                ConvertZP01Setting(STR_ISEC_MODULE, 'YMax', '3000', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'VMotor', '-14,5000,5', iftString);
                ConvertZP01Setting(STR_ISEC_MODULE, 'VMax', '2500', iftInteger);

                for x := 1 to 4 do
                    ConvertZP01Setting(STR_ISEC_MODULE, 'ZMotor' + IntToStr(x),
                        '-1' + IntToStr(x) + ',5000,5', iftString);

                ConvertZP01Setting(STR_ISEC_MODULE, 'ZMax', '2500', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'ZTravel', '0', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'ZScanSpeed', '1000', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'ZRetrSpeed', '1000', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'DTScanSpeed', '1000', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'ZRange', '150', iftInteger);

                ConvertZP01Setting(STR_ISEC_MODULE, 'hZMotor', '-17,5000,5', iftString);
                ConvertZP01Setting(STR_ISEC_MODULE, 'hZMax', '2000', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'hZTravel', '0', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'hVMotor', '-14,5000,5', iftString);
                ConvertZP01Setting(STR_ISEC_MODULE, 'hVMax', '2000', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'hVCheck', '50', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'HandTrk', '0', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'TubeHandTrk', '0', iftInteger);
                for x := 1 to 8 do
                    ConvertZP01Setting(STR_ISEC_MODULE, 'Diluter' + IntToStr(x),
                        '-' + IntToStr(x) + ',1200,3,1200,3,0,90,270,180,999,1000', iftString);

                ConvertZP01Setting(STR_ISEC_MODULE, 'DMax', '2000', iftInteger);
                //
                ConvertZP01Setting(STR_ISEC_MODULE, 'PeriPump', '-16,1,0', iftString);
                ConvertZP01Setting(STR_ISEC_MODULE, 'PeriSpeed', '2000', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'Stirer', '-16,2,0', iftString);
                ConvertZP01Setting(STR_ISEC_MODULE, 'Beeper', '-16,5,0', iftString);
                //
                ConvertZP01Setting(STR_ISEC_MODULE, 'Washer', '-5,0,1,0,Xh0d0', iftString);
                ConvertZP01Setting(STR_ISEC_MODULE, 'Reader', '-22,0,0,0,0,0,0', iftString);
                ConvertZP01Setting(STR_ISEC_MODULE, 'Shaker', '-9', iftInteger);
                //
                ConvertZP01Setting(STR_ISEC_MODULE, 'RefPos', '3', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'VFactor', '62', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'YOffset', '135', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'VOffset', '1000', iftInteger);
                //
                ConvertZP01Setting('Port', 'Port', 'COM1', iftString);
                ConvertZP01Setting('Port', 'Baud', '9600', iftInteger);
                ConvertZP01Setting('Port', 'Port2', '', iftString);
                //
                // //------------------------------------------------------- sonstige Parameter aus Machine.ini
                ConvertZP01Setting('Options', 'CloseVfor1Tip', '1', iftInteger);
                ConvertZP01Setting('Options', 'OptimizeTipCouple', '0', iftInteger);
                ConvertZP01Setting('Options', 'RS232MaxRetry', '3', iftInteger);
                ConvertZP01Setting('Options', 'InitXBeforeY', '0', iftInteger);

                ConvertZP01Setting(STR_ISEC_MODULE, 'MaxPorts', '13', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'DilutorType', '0', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'RelayBoard5', '-5', iftInteger);
                ConvertZP01Setting(STR_ISEC_MODULE, 'MinMotorSpeed', '6', iftInteger);

                // Part 2: All entries needed in machine.ini
                ConvertRobotSetting('OPTIONS', 'WaitAfterPickSys', '0', iftInteger);
                ConvertRobotSetting('OPTIONS', 'HToolYCorrect', '1', iftInteger);
                ConvertRobotSetting('OPTIONS', 'MaxSBError', '20', iftInteger);

                ConvertRobotSetting('XYZStepsPerMillimeter', 'XSTEPperMM', '-1', iftFloat);
                ConvertRobotSetting('XYZStepsPerMillimeter', 'YSTEPperMM', '-1', iftFloat);
                ConvertRobotSetting('XYZStepsPerMillimeter', 'ZSTEPperMM', '-1', iftFloat);

                // ZP01
                ConvertRobotSetting('XYZStepsPerMillimeter', 'HSTEPperMM', '-1', iftFloat);
                // ZP02
                ConvertRobotSetting('XYZStepsPerMillimeter', 'HXSTEPperMM', '-1', iftFloat);
                ConvertRobotSetting('XYZStepsPerMillimeter', 'HYSTEPperMM', '-1', iftFloat);
                ConvertRobotSetting('XYZStepsPerMillimeter', 'HZSTEPperMM', '-1', iftFloat);
                ConvertRobotSetting('XYZStepsPerMillimeter', 'HRSTEPperDegrees', '-1', iftFloat);
                ConvertRobotSetting('XYZStepsPerMillimeter', 'HVSTEPperMM', '-1', iftFloat);

                ConvertRobotSetting('Handler_ReferenceTip_Distances_Steps', 'DistX_H_Tip1', '-1', iftInteger);
                ConvertRobotSetting('Handler_ReferenceTip_Distances_Steps', 'DistY_H_Tip1', '-1', iftInteger);
                ConvertRobotSetting('Handler_ReferenceTip_Distances_Steps', 'DistZ_H_Tip1', '-1', iftInteger);

                ConvertRobotSetting('Handler_ReferenceTip_Distances_Steps', 'DistX_HandlerEnd_Tip1_mm', '-1',
                    iftFloat);
                ConvertRobotSetting('Handler_ReferenceTip_Distances_Steps', 'DistY_HandlerEnd_Tip1_mm', '-1',
                    iftFloat);
                ConvertRobotSetting('Handler_ReferenceTip_Distances_Steps', 'DistX_HTip1_Tip1_mm', '-1',
                    iftFloat);
                ConvertRobotSetting('Handler_ReferenceTip_Distances_Steps', 'DistY_HTip1_Tip1_mm', '-1',
                    iftFloat);
                ConvertRobotSetting('Handler_ReferenceTip_Distances_Steps', 'DistZ_HTip1_Tip1_mm', '-1',
                    iftFloat);

                ConvertRobotSetting('VisualFactor', 'zoom', '-1', iftInteger);
                ConvertRobotSetting('VisualFactor', 'BorderWidthTop', '0', iftInteger);
                ConvertRobotSetting('VisualFactor', 'BorderWidthLeft', '0', iftInteger);
                ConvertRobotSetting('VisualFactor', 'BorderWidthRight', '0', iftInteger);
                ConvertRobotSetting('VisualFactor', 'BorderWidthBottom', '0', iftInteger);

                ConvertRobotSetting('LiquidSpeed', 'DefaultAspSpeed', '0', iftInteger);
                ConvertRobotSetting('LiquidSpeed', 'DefaultDispSpeed', '0', iftInteger);
                ConvertRobotSetting('LiquidSpeed', 'MaxAspSpeed', '0', iftInteger);
                ConvertRobotSetting('LiquidSpeed', 'AspSpeedCalcFactor', '100', iftInteger);
                ConvertRobotSetting('LiquidSpeed', 'DispSpeedCalcFactor', '100', iftInteger);

                ConvertRobotSetting(STR_ISEC_MODULE, 'DilutorAspSpeedPerCent', '0', iftInteger);
                ConvertRobotSetting(STR_ISEC_MODULE, 'ToolZTravel', '0', iftInteger);
                ConvertRobotSetting(STR_ISEC_MODULE, 'MinZTravelTubes', '0', iftInteger);
                ConvertRobotSetting(STR_ISEC_MODULE, 'PipToolZTravel', '0', iftInteger);
                ConvertRobotSetting(STR_ISEC_MODULE, 'MinZTravelPipTool', '0', iftInteger);
                ConvertRobotSetting(STR_ISEC_MODULE, 'RediZRetrSpeed', '0', iftInteger);

                ConvertRobotSetting('WriteLogs', 'WRITE_LOG_INFO', '1', iftInteger);
                ConvertRobotSetting('WriteLogs', 'WRITE_LOG_ERROR', '1', iftInteger);
                ConvertRobotSetting('WriteLogs', 'WRITE_LOG_DEBUG', '0', iftInteger);
                ConvertRobotSetting('WriteLogs', 'WRITE_LOG_SEND', '1', iftInteger);
                ConvertRobotSetting('WriteLogs', 'WRITE_LOG_NO_NEW', '1', iftInteger);

                ConvertRobotSetting('DISPOSAL_TIPS', 'DT_CHECK_BEFORE', '0', iftBoolean);
                ConvertRobotSetting('DISPOSAL_TIPS', 'DT_DISP_BEFORE', '0', iftBoolean);
                ConvertRobotSetting('DISPOSAL_TIPS', 'DT_DISP_ERROR', '0', iftBoolean);
                ConvertRobotSetting('DISPOSAL_TIPS', 'DT_CHECK_AFTER', '0', iftBoolean);
                ConvertRobotSetting('DISPOSAL_TIPS', 'DT_DISP_AFTER', '0', iftBoolean);
                ConvertRobotSetting('DISPOSAL_TIPS', 'DT_REMOVE_LIQ', '0', iftBoolean);

                ConvertRobotSetting('DISPOSAL_TIPS', 'GT_DISP_ERROR', '0', iftBoolean);
                ConvertRobotSetting('DISPOSAL_TIPS', 'GT_SKIP_ERROR', '0', iftBoolean);
                ConvertRobotSetting('DISPOSAL_TIPS', 'GT_RETRY_ERROR', '0', iftBoolean);
                ConvertRobotSetting('DISPOSAL_TIPS', 'GT_CHECK_AFTER', '0', iftBoolean);
                ConvertRobotSetting('DISPOSAL_TIPS', 'GT_DISP_AFTER', '0', iftBoolean);
                ConvertRobotSetting('DISPOSAL_TIPS', 'GT_SKIP_AFTER', '0', iftBoolean);
                ConvertRobotSetting('DISPOSAL_TIPS', 'GT_RETRY_AFTER', '0', iftBoolean);
                ConvertRobotSetting('DISPOSAL_TIPS', 'GT_SINGLE_TIP', '0', iftBoolean);

                ConvertRobotSetting('DISPOSAL_TIPS', 'IgnoreRestTips', '1', iftBoolean);
                ConvertRobotSetting('DISPOSAL_TIPS', 'GT_ScanSpeed', '500', iftInteger);
                ConvertRobotSetting('DISPOSAL_TIPS', 'GT_ScanRamp', '300', iftInteger);

                ConvertRobotSetting('PlateCheck', 'PutPlate', '0', iftInteger);
                ConvertRobotSetting('PlateCheck', 'GetPlate', '0', iftInteger);

                ConvertRobotSetting(STR_ISEC_BARCODE, 'ReadE_DLLName', '', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'ReadE_DLLFunc', '', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'ReadE_DLLParam', '', iftString);

                ConvertRobotSetting('TakeTubes', 'XOffset', '0', iftInteger);
                ConvertRobotSetting('TakeTubes', 'YOffset', '0', iftInteger);
                ConvertRobotSetting('TakeTubes', 'ZOffset', '0', iftInteger);
                ConvertRobotSetting('TakeTubes', 'VOpen', '0', iftInteger);
                ConvertRobotSetting('TakeTubes', 'VDrop', '0', iftInteger);
                ConvertRobotSetting('TakeTubes', 'ZDrop', '0', iftInteger);
                ConvertRobotSetting('TakeTubes', 'TubeDY', '0', iftInteger);
                ConvertRobotSetting('TakeTubes', 'SaveMoveOffset', '0', iftInteger);
                ConvertRobotSetting('TakeTubes', 'SaveMoveSpeed', '0', iftInteger);
                ConvertRobotSetting('TakeTubes', 'ToolBringBackVOffset', '0', iftInteger);
                ConvertRobotSetting('TakeTubes', 'ShakeHeight', '1600', iftInteger);
                ConvertRobotSetting('TakeTubes', 'ShakeZOffset', '700', iftInteger);
                ConvertRobotSetting('TakeTubes', 'ShakeYOffset', '30', iftInteger);

                ConvertRobotSetting('ConradRelayBoard', 'ComPort', '-1', iftInteger);

                ConvertRobotSetting(STR_ISEC_BARCODE, 'MTP_BCR', '-16,0,???*,AcZz', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'MTP_BCR_Pos', '0,0,0', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'MTP_BCR_ScanXYZRange', '0,0,0,0,0,0', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'MTP_BCR_LeadZero', '0', iftInteger);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'MTP_BCR_DelLastPos', '0', iftInteger);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'MTP_BCR_CodeFilter', '*', iftString);

                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR', '-8,0,???*,AcZz', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_Pos', '0,0,0', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_ScanXYZRange', '0,0,0,0,0,0', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_LeadZero', '0', iftInteger);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_DelLastPos', '0', iftInteger);

                ConvertRobotSetting(STR_ISEC_MODULE, 'SetupSpeedFactor', '100', iftInteger);

                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_DllName', '', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_DllRead', 'ReadBC', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_DllInit', 'Init', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_DllInitPar', '', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_DllTrigOnFunc', 'WASerCam_TriggerOnOff',
                    iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_DllTrigOnPara', '1', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_DllTrigOffFunc', 'WASerCam_TriggerOnOff',
                    iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_DllTrigOffPara', '0', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_DllGetBcFunc', 'GetBC', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_DllGetBcPara', '0', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_GetBcDelay', '100', iftInteger);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_CodeFilter', '*', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_TurnTable', '-9,300,2', iftString);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_NoOfTurns', '5', iftInteger);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_TurnSteps', '5000', iftInteger);
                ConvertRobotSetting(STR_ISEC_BARCODE, 'TUBE_BCR_TurnRelay', '0', iftBoolean);

            finally
                xSettingDP.Close();
            end;
        finally
            xIniFile.Free;
        end;
    finally
        xSettingDP.Free;
    end;
end;

procedure TSettingsTableUpdateV1_1.ConvertSiasRobotIni();
var
    xSettingDP: TDataProvider;
    xIniFile: TIniFile;
    xIniPath: string;

    procedure ConvertRobotSetting(const aSection, aIdent, aDefault: string; aIniFieldType: TIniFieldType;
        aMustWriteDefault: boolean);
    begin
        ConvertSetting(xSettingDP, xIniFile, STR_SETTING_AREA_ROBOT, aSection, aIdent, aDefault,
            aIniFieldType, aMustWriteDefault);
    end;

begin
    xIniPath := TUpdatePaths.Instance.DataPath + 'SiasRobot.ini';
    if (not TFileUtilities.FileExists(xIniPath)) then
        raise Exception.CreateFmt('Ini file %s does not exist', [xIniPath]);

    xSettingDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xIniFile := TIniFile.Create(xIniPath);
        try
            xSettingDP.SelectAndOpen(STR_SETTINGS_SELECT_ALL, false);
            try
                ConvertRobotSetting('XYZStepsPerMillimeter', 'XSTEPperMM', '10', iftFloat, true);
                ConvertRobotSetting('XYZStepsPerMillimeter', 'YSTEPperMM', '10', iftFloat, true);
                ConvertRobotSetting('XYZStepsPerMillimeter', 'ZSTEPperMM', '10', iftFloat, true);

                // ZP02
                ConvertRobotSetting('XYZStepsPerMillimeter', 'HXSTEPperMM', '10', iftFloat, true);
                ConvertRobotSetting('XYZStepsPerMillimeter', 'HYSTEPperMM', '10', iftFloat, true);
                ConvertRobotSetting('XYZStepsPerMillimeter', 'HZSTEPperMM', '10', iftFloat, true);
                ConvertRobotSetting('XYZStepsPerMillimeter', 'HRSTEPperDegrees', '10', iftFloat, true);
                ConvertRobotSetting('XYZStepsPerMillimeter', 'HVSTEPperMM', '10', iftFloat, true);

                ConvertRobotSetting('Handler_ReferenceTip_Distances_Steps', 'DistX_H_Tip1', '-1',
                    iftInteger, false);
                ConvertRobotSetting('Handler_ReferenceTip_Distances_Steps', 'DistY_H_Tip1', '-1',
                    iftInteger, false);
                ConvertRobotSetting('Handler_ReferenceTip_Distances_Steps', 'DistZ_H_Tip1', '-1',
                    iftInteger, false);

                ConvertRobotSetting('Handler_ReferenceTip_Distances_Steps', 'DistX_HandlerEnd_Tip1_mm', '-1',
                    iftFloat, false);
                ConvertRobotSetting('Handler_ReferenceTip_Distances_Steps', 'DistY_HandlerEnd_Tip1_mm', '-1',
                    iftFloat, false);
                ConvertRobotSetting('Handler_ReferenceTip_Distances_Steps', 'DistX_HTip1_Tip1_mm', '-1',
                    iftFloat, false);
                ConvertRobotSetting('Handler_ReferenceTip_Distances_Steps', 'DistY_HTip1_Tip1_mm', '-1',
                    iftFloat, false);
                ConvertRobotSetting('Handler_ReferenceTip_Distances_Steps', 'DistZ_HTip1_Tip1_mm', '-1',
                    iftFloat, false);

                // 02.11.07 pk this is not complete...

            finally
                xSettingDP.Close();
            end;
        finally
            xIniFile.Free;
        end;
    finally
        xSettingDP.Free;
    end;
end;

function TSettingsTableUpdateV1_1.DeterminePlatform(): TIniPlatformType;
var
    xIniPath: string;
begin
    result := iptNone;

    xIniPath := TUpdatePaths.Instance.DataPath + 'SiasRobot.ini';
    if TFileUtilities.FileExists(xIniPath) then
    begin
        result := iptZP02;
        EXIT;
    end;

    xIniPath := TUpdatePaths.Instance.DataPath + 'Machine.ini';
    if TFileUtilities.FileExists(xIniPath) then
    begin
        result := iptZP01;
        EXIT;
    end;
end;

procedure TSettingsTableUpdateV1_1.ConvertPlatformIni();
var
    xPlatformType: TIniPlatformType;
begin
    xPlatformType := DeterminePlatform();
    if xPlatformType = iptZP01 then
        ConvertMachineIni()
    else if xPlatformType = iptZP02 then
        ConvertSiasRobotIni();
end;

procedure TSettingsTableUpdateV1_1.ConvertSamplerIni();
var
    xSettingDP: TDataProvider;
    xIniFile: TIniFile;
    xIniPath: string;

    procedure ConvertAppSetting(const aSection, aIdent, aDefault: string; aIniFieldType: TIniFieldType);
    begin
        ConvertSetting(xSettingDP, xIniFile, STR_SETTING_AREA_APP, aSection, aIdent, aDefault,
            aIniFieldType, false);
    end;

begin
    xIniPath := TUpdatePaths.Instance.DataPath + 'Sampler.ini';
    if (not TFileUtilities.FileExists(xIniPath)) then
        raise Exception.CreateFmt('Ini file %s does not exist', [xIniPath]);

    xSettingDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xIniFile := TIniFile.Create(xIniPath);
        try
            xSettingDP.SelectAndOpen(STR_SETTINGS_SELECT_ALL, false);
            try

                // Part 3: All entries needed in sampler.ini
                ConvertAppSetting('Info', 'SerialNo', '', iftString);

                ConvertAppSetting('Oem', 'AppTitle', '', iftString);
                ConvertAppSetting('Oem', 'OemTitle', '', iftString);
                ConvertAppSetting('Oem', 'JobTitle', '', iftString);
                ConvertAppSetting('Oem', 'Support', '', iftString);
                ConvertAppSetting('Oem', 'Copyr1', '', iftString);
                ConvertAppSetting('Oem', 'Copyr2', '', iftString);
                ConvertAppSetting('Oem', 'www', '', iftString);

                ConvertAppSetting('Display', 'UseScript', '0', iftBoolean);
                ConvertAppSetting('Display', 'Language', '0', iftInteger);
                ConvertAppSetting('Display', 'ConfirmIgnore', '0', iftBoolean);
                ConvertAppSetting('Display', '24h-Mode', '0', iftBoolean);
                ConvertAppSetting('Display', 'ChangePosinfo', '0', iftBoolean);
                ConvertAppSetting('Display', 'AskForRackPlacement', '0', iftInteger);
                ConvertAppSetting('Display', 'TubeDispColor', IntToStr($0000FF) { clRed } , iftInteger);
                ConvertAppSetting('Display', 'MethEditActionOnly', '0', iftInteger);
                ConvertAppSetting('Display', 'UseScheduler', '0', iftInteger);

                ConvertAppSetting('Logfiles', 'Path', '', iftString);
                ConvertAppSetting('Logfiles', 'StoreDays', '28', iftInteger);
                ConvertAppSetting('Logfiles', 'Archives', '', iftString);

                ConvertAppSetting('Documentation', 'WritePosinfo', '1', iftInteger);

                ConvertAppSetting('Pipetting', 'MultiCheckXYPos', '1', iftBoolean);
                ConvertAppSetting('Pipetting', 'SwitchPortOnRestart', '', iftString);
                ConvertAppSetting('Pipetting', 'WashDryDelay', '1000', iftInteger);
                ConvertAppSetting('Pipetting', 'WashOnlyUsedTips', '0', iftBoolean);
                ConvertAppSetting('Pipetting', 'DispSubmergeActive', '0', iftBoolean);
                ConvertAppSetting('Pipetting', 'ChangeTipTypes', '0', iftBoolean);
                ConvertAppSetting('Pipetting', 'ChkLHPByGeneratePipSeq', '1', iftBoolean);

                ConvertAppSetting('Pipetting', 'WashRetractSpeed', '0', iftInteger);
                ConvertAppSetting('Pipetting', 'FlushVolAfterInit', '0', iftInteger);
                ConvertAppSetting('Pipetting', 'AirVolAfterInit', '0', iftInteger);
                ConvertAppSetting('Pipetting', 'RetractStepsAfterInit', '0', iftInteger);
                ConvertAppSetting('Pipetting', 'SaveSolvent', '0', iftInteger);
                ConvertAppSetting('Pipetting', 'EnableVolumeControl', '1', iftBoolean);
                ConvertAppSetting('Pipetting', 'WashRemRediTips', '1', iftBoolean);

                ConvertAppSetting('Corridor', 'XMoveOffset', '-1', iftInteger);

                ConvertAppSetting('Liquids', 'WasteVol', '0', iftInteger);
                ConvertAppSetting('Liquids', 'WasteMaxVol', '1000', iftInteger);
                ConvertAppSetting('Liquids', 'FlushCycles', '10', iftInteger);
                ConvertAppSetting('Liquids', 'SysLiqMinVol', '100', iftInteger);

                ConvertAppSetting('Balance', 'CloseDoorAfterInit', '0', iftBoolean);
                ConvertAppSetting('Balance', 'TaraAfterXMin', '120', iftInteger);
                ConvertAppSetting('Balance', 'TaraAfterXTimes', '75', iftInteger);
                ConvertAppSetting('Balance', 'Balance1', '', iftString);
                ConvertAppSetting('Balance', 'Balance1_Left', '15', iftInteger);
                ConvertAppSetting('Balance', 'Balance1_Top', '5', iftInteger);
                ConvertAppSetting('Balance', 'MoveTubeToBalance', '0', iftInteger);
                ConvertAppSetting('Balance', 'MoveTubeFromBalance', '0', iftInteger);
                ConvertAppSetting('Balance', 'MoveTubesAfterRestart', '0', iftInteger);
                ConvertAppSetting('Balance', 'UseSeveralBalanceRacks', '1', iftBoolean);

                ConvertAppSetting('Calli', 'CheckNetWeight', '0', iftBoolean);
                ConvertAppSetting('Calli', 'TaraAfterRedi', '0', iftBoolean);
                ConvertAppSetting('Calli', 'UseRackIDPos', '0', iftBoolean);
                ConvertAppSetting('Calli', 'DestOnBalance', '0', iftBoolean);
                ConvertAppSetting('Calli', 'TargetApproach', '1', iftInteger);
                ConvertAppSetting('Calli', 'WeighLogicDbName', '', iftString);
                ConvertAppSetting('Calli', 'WeighLogicDbUser', 'CALLIW', iftString);
                ConvertAppSetting('Calli', 'WeighLogicDbPassword', '', iftString);

                ConvertAppSetting('Redi', 'ShakeTimeAdjustable', '0', iftBoolean);

                ConvertAppSetting('DatabaseToolDll', 'Name', '', iftString);
                ConvertAppSetting('DatabaseToolDll', 'RequestWriteBackData', '1', iftInteger);
                ConvertAppSetting('DatabaseToolDll', 'ShowPlatesBeforeBCRead', '0', iftInteger);

                ConvertAppSetting('DLLFiles', 'cDLLFiles', 'CRSRobot', iftString);
                ConvertAppSetting('DLLFiles', 'DetachDLL', 'CRSRobot', iftString);

                ConvertAppSetting('Device', 'Name', 'Sampler', iftString);

                ConvertAppSetting('Run', 'MoveRackSlotDelimiter', ';', iftString);
                ConvertAppSetting('Run', 'RunTableExclusive', '1', iftBoolean);

                ConvertAppSetting('ExportRunFile', 'ExpAfterRun', '0', iftBoolean);
                ConvertAppSetting('ExportRunFile', 'ExportScript', '0', iftBoolean);
                ConvertAppSetting('ExportRunFile', 'Path', '.\ExportRn', iftString);
                ConvertAppSetting('ExportRunFile', 'Fields', '', iftString);
                ConvertAppSetting('ExportRunFile', 'Separator', ',', iftString);
                ConvertAppSetting('ExportRunFile', 'ExpActions', '1', iftBoolean);
                ConvertAppSetting('ExportRunFile', 'Numbering', '1', iftBoolean);
                ConvertAppSetting('ExportRunFile', 'DateTimeStamp', '0', iftBoolean);
                ConvertAppSetting('ExportRunFile', 'SeprAtEndOfLine', '1', iftBoolean);
                ConvertAppSetting('ExportRunFile', 'Extension', 'txt', iftString);
                ConvertAppSetting('ExportRunFile', 'xlsDelCSV', '1', iftBoolean);
                ConvertAppSetting('ExportRunFile', 'DateTimeStampStr', 'yyyymmdd-hhnnsszzz', iftString);

                ConvertAppSetting('ScriptReset', 'AskForKeepContents', '0', iftBoolean);
                ConvertAppSetting('ScriptReset', 'ResetScriptAfterDDEStart', '0', iftBoolean);
                ConvertAppSetting('ScriptReset', 'ArchiveScript', '1', iftBoolean);
                ConvertAppSetting('ScriptReset', 'ArchiveScriptPath', '.\ScriptArchive', iftString);
                ConvertAppSetting('ScriptReset', 'AutoResetScript', '0', iftBoolean);

                ConvertAppSetting('Schedule', 'ShowTempRequestTime', '0', iftInteger);
                ConvertAppSetting('Schedule', 'VortRequestTime', '1', iftInteger);
                ConvertAppSetting('Schedule', 'TangoRequestTime', '1', iftInteger);
                ConvertAppSetting('Schedule', 'LogVortInfo', '1', iftBoolean);
                // ConvertAppSetting( 'Schedule','FixAfterVortexing', '0', iftBoolean );     --> ShakerDevice
                // ConvertAppSetting( 'Schedule','OpenFixationAtInit', '0', iftBoolean );    --> in CATMix.bpl - CATMixConnection
                // ConvertAppSetting( 'Schedule','NoVortexerStopAtInit', '0', iftBoolean );  --> in CATMix.bpl - CATMixConnection

                ConvertAppSetting('ImportFile', 'Delimiter', ';', iftString);

                ConvertAppSetting('Washprogram', 'Sample', '1', iftInteger);
                ConvertAppSetting('Washprogram', 'Cycle', '1', iftInteger);

                ConvertAppSetting('Display', 'ShowAnimation', '0', iftBoolean);
                ConvertAppSetting('Display', 'ShowRunTable', '0', iftBoolean);

                ConvertAppSetting('Beep', 'DLLName', '', iftString);
                ConvertAppSetting('Beep', 'DLLFunction', '', iftString);
                ConvertAppSetting('Beep', 'DLLOffFunction', '', iftString);
                ConvertAppSetting('Beep', 'Time', '50', iftInteger);
                ConvertAppSetting('Beep', 'Sound', '16', iftInteger);

                ConvertAppSetting('Display', 'SimBeforeStart', '0', iftBoolean);
                ConvertAppSetting('Run', 'RunCreateSeqAuto', '0', iftBoolean);
                ConvertAppSetting('Run', 'RunCreatePaint', '1', iftBoolean);
                ConvertAppSetting('Display', 'InitAtMethodEnd', '0', iftBoolean);
                ConvertAppSetting('Display', 'InitAtScriptEnd', '0', iftBoolean);

                ConvertAppSetting('Export', 'Racks', '.\RACKS.DAT', iftString);
                ConvertAppSetting('Export', 'Slots', '.\SLOTS.DAT', iftString);

                ConvertAppSetting('PosPressCtrl', 'RequestTime', '500', iftInteger);
                ConvertAppSetting('PosPressCtrl', 'StartDelay', '500', iftInteger);
                ConvertAppSetting('PosPressCtrl', 'MoveZDelay', '500', iftInteger);
                ConvertAppSetting('PosPressCtrl', 'EndDelay', '0', iftInteger);
                ConvertAppSetting('PosPressCtrl', 'ScanSpeed', '0', iftInteger);
                ConvertAppSetting('PosPressCtrl', 'RetractSpeed', '0', iftInteger);

                ConvertAppSetting(STR_ISEC_VORTEXER, 'Adr', '-1', iftInteger);
                ConvertAppSetting(STR_ISEC_VORTEXER, 'VortexerPort', '-1', iftInteger);
                ConvertAppSetting(STR_ISEC_VORTEXER, 'MagnetPort', '-1', iftInteger);
                ConvertAppSetting(STR_ISEC_VORTEXER, 'VortexerOn', '-1', iftInteger);
                ConvertAppSetting(STR_ISEC_VORTEXER, 'MagnetOn', '-1', iftInteger);
                ConvertAppSetting(STR_ISEC_VORTEXER, 'SwitchOffDelay', '-1', iftInteger);
                ConvertAppSetting(STR_ISEC_VORTEXER, 'VortexTimeinSec', '0', iftInteger);

                ConvertAppSetting(STR_ISEC_LASTRBLOCK, 'Columns', '12', iftInteger);
                ConvertAppSetting(STR_ISEC_LASTRBLOCK, 'Rows', '8', iftInteger);
                ConvertAppSetting(STR_ISEC_LASTRBLOCK, 'VolumeF1000', '100000', iftInteger);

                ConvertAppSetting(STR_ISEC_SYSLIQVALVE, 'UsePeriAtWash', '1', iftBoolean);
                ConvertAppSetting(STR_ISEC_SYSLIQVALVE, 'FlushVol', '-31', iftInteger);
                ConvertAppSetting(STR_ISEC_SYSLIQVALVE, 'ComPort', '0', iftInteger);
                ConvertAppSetting(STR_ISEC_SYSLIQVALVE, 'NoValveRead', '0', iftBoolean);
                ConvertAppSetting(STR_ISEC_SYSLIQVALVE, 'Timeout', '20', iftInteger);
                ConvertAppSetting(STR_ISEC_SYSLIQVALVE, 'SleepTime', '500', iftInteger);
                ConvertAppSetting(STR_ISEC_SYSLIQVALVE, 'RetryCnt', '5', iftInteger);
                ConvertAppSetting(STR_ISEC_SYSLIQVALVE, 'RelayValveComPort', '0', iftInteger);
                ConvertAppSetting(STR_ISEC_SYSLIQVALVE, 'RelayValveFirstLiqPort', '0', iftInteger);
                ConvertAppSetting(STR_ISEC_SYSLIQVALVE, 'RelayValveNoOfLiqPorts', '0', iftInteger);
                ConvertAppSetting(STR_ISEC_SYSLIQVALVE, 'Adress', '-31', iftInteger);

            finally
                xSettingDP.Close();
            end;
        finally
            xIniFile.Free;
        end;
    finally
        xSettingDP.Free;
    end;
end;

procedure TSettingsTableUpdateV1_1.Convert(aSender: TObject);
begin
    ConvertPlatformIni();
    ConvertSamplerIni();
end;

procedure TSettingsTableUpdateV1_1.DoVersionCheck;
var
    xSettingDP: TDataProvider;
begin
    fVersionCheckResult := vcrOK;

    // if settings table does not exist, we will do the update
    if not fTableChangeAdaptor.DBPathTableExists('SETTINGS') then
    begin
        fVersionCheckResult := vcrUpdate;
        EXIT;
    end;

    // if settings table exists but is empty we will do the update
    xSettingDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        xSettingDP.SelectAndOpen(STR_SETTINGS_SELECT_ALL, true);
        try
            if not xSettingDP.IsEmpty then
                EXIT;
            fVersionCheckResult := vcrUpdate;
        finally
            xSettingDP.Close();
        end;
    finally
        xSettingDP.Free;
    end;
end;


end.
