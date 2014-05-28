{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  04.01.11 wl                               TN5405   initial revision
  21.01.11 wl                               TN5405   Device-Konvertierung für ZP01 mit V7.0.9 und ZP02 mit V7.1.7
  26.01.11 wl                               TN5448   Arm: Geänderte Parameter
  31.01.11 wl  TMotorDevice                 TN5440   Umwandlung aller Step-Werte in mm, Y- und Z-Motoren Reverse
  15.02.11 pk                               TN4780   changes needed to make UpdateManager compatible with TurboDB
  21.02.11 wl  TCommunicationRealManager.AddShaker   TN5455   Interface wird auch für negative COM-Werte erzeugt
  09.06.11 wl  TBCReaderDevice.WriteDeviceData       TN5597   Wenn es den Namen schon gab, nichts schreiben
  09.06.11 wl  TCompositeDevice.Destroy              TN5597   Ignorieren, wenn nicht alle Elemente zerstört werden können
  29.11.11 wl  TCompositeDevice.WriteDeviceData         TN5752   Schreiben jedes Subdevices in try..except_block mit Tabellen-Reset
  29.11.11 wl  TIntfRelayKolter.Sensor_WriteDriverData  TN5752   für Kolter wurde schon mal ein PLC05 vorgesehen
  13.02.12 wl  TBCTurntableDevice                       TN5801   Neu: Unterscheidung zwischen TMotorBCTurntableDevice und TSwitchBCTurntableDevice
  30.03.13 wl                                           TN6121   Bugfix Switch-Konvertierung
  10.06.13 wl                                           TN6171   ein paar try-except-Blöcke eingebaut, damit die Konvertierung nicht abbricht
  08.11.13 wl                                           TN6298   SIXWAYWALVE_1 entfernt
  ---------------------------------------------------------------------------------------------------------------------- }

unit DeviceClasses;


interface


uses
    Generics.Collections,
    DataProvider,
    Classes,
    ComCtrls,
    Types;

const
    MAX_TIPS = 9;
    SIZE_RACK_NAME = 39;
    STR_DEV_INI_DEFAULT_BIT_UNDEFINED = '-1';
    STR_DEV_INI_DEFAULT_DLLNAME_UNDEFINED = '';
    MAX_VORTEXERS = 30;
    STR_ISEC_BARCODE = 'BARCODE';
    STR_USE_BCR_FILTER = 'USE_BCREADER_FILTER';
    STR_ISEC_USERPROTECT = 'UserProtection';
    STR_ISEC_STATEMONITOR = 'StateMonitor';

    ZP01_MAX_ADR = 64;

    STR_AREANAME_ROBOT = 'Robot';

type
    TIniSettings = class
    private
        class procedure SelectAndOpenIdent(aSettingDP: TDataProvider; const aArea, aSection, aIdent: string);
    public
        class function SectionExists(aSettingDP: TDataProvider; const aArea, aSection: string)
            : boolean; static;
        class function ReadInteger(aSettingDP: TDataProvider; const aArea, aSection, aIdent: string;
            aDefault: integer): integer; static;
        class function ReadString(aSettingDP: TDataProvider; const aArea, aSection, aIdent: string;
            const aDefault: string): string; static;
        class function ReadBool(aSettingDP: TDataProvider; const aArea, aSection, aIdent: string;
            aDefault: boolean): boolean; static;
        class function ReadFloat(aSettingDP: TDataProvider; const aArea, aSection, aIdent: string;
            aDefault: double): double; static;
        class procedure ReadSection(aSettingDP: TDataProvider; const aArea, aSection: string;
            aStrings: TStrings); static;
    end;

    MAdr = smallInt; // 16 Bit Modul Adressen;

    MOTOR = record
        Adr: MAdr; // Module Adress
        Speed, Ramp, // Default Speed and Ramp
        InitSpeed: integer;
    end;

    BARCODEREC = record
        Adr: integer;
        BcType: integer;
        Pattern: string;
        InitStr: string;
    end;

    BCSCANRANGE = record
        XScanRange: integer;
        XScanSteps: integer;
        YScanRange: integer;
        YScanSteps: integer;
        ZScanRange: integer;
        ZScanSteps: integer;
    end;

    BCRPOSITION = record
        XPos, YPos, ZPos, RPos: integer;
        PosName: string;
    end;

    TXYRangeData = record
        XRel1, XRel2, YRel1, YRel2: integer;
    end;

    ISimpleIniAccess = interface
        ['{2AEE79F3-3760-44C6-A6F7-19916A7FCD16}']
        function SectionExists(const aSection: string): boolean;
        function ReadInteger(const aSection, aIdent: string; aDefault: integer): integer;
        function ReadString(const aSection, aIdent: string; const aDefault: string): string;
        function ReadBool(const aSection, aIdent: string; aDefault: boolean): boolean;
        function ReadFloat(const aSection, aIdent: string; aDefault: double): double;
        procedure ReadSection(const aSection: string; aStrings: TStrings);
        function ReadBcrPosition(const aSection, aIdent: string): BCRPOSITION;
        function ReadBcScanrange(const aSection, aIdent: string): BCSCANRANGE;
        function ReadBarcodeRec(const aSection, aIdent: string): BARCODEREC;
        function ReadMotor(const aSection, aIdent: string; aDefSpeed, aDefRamp: integer): MOTOR;
        function ReadXYRangeData(aSection, aIdent: string): TXYRangeData;
    end;

    TDevSimpleIniAccess = class(TInterfacedObject, ISimpleIniAccess)
    private
        fSettingDP: TDataProvider;
        fArea: string;
    public
        constructor Create(aSettingDP: TDataProvider; const aArea: string);
        function SectionExists(const aSection: string): boolean;
        function ReadInteger(const aSection, aIdent: string; aDefault: integer): integer;
        function ReadString(const aSection, aIdent: string; const aDefault: string): string;
        function ReadBool(const aSection, aIdent: string; aDefault: boolean): boolean;
        function ReadFloat(const aSection, aIdent: string; aDefault: double): double;
        procedure ReadSection(const aSection: string; aStrings: TStrings);
        function ReadBcrPosition(const aSection, aIdent: string): BCRPOSITION;
        function ReadBcScanrange(const aSection, aIdent: string): BCSCANRANGE;
        function ReadBarcodeRec(const aSection, aIdent: string): BARCODEREC;
        function ReadMotor(const aSection, aIdent: string; aDefSpeed, aDefRamp: integer): MOTOR;
        function ReadXYRangeData(aSection, aIdent: string): TXYRangeData;
    end;

    TPosMM = double; // Motor Position oder Längenangabe in mm
    TPosMMArray = array [0 .. MAX_TIPS - 1] of TPosMM;

    TAppSettings = class
    private
        class var fIsSias: boolean;
        class var uXOffsetLeftToTip1: TPosMM;
        class var uYOffsetFrontToTip1: TPosMM;
        class function CopyTillEnd(aStrText: string; aIntIndex: integer): string; static;
        class function Extract(var S: string; index, Count, Skip: Integer): string; static;
    public
        class procedure Init(aIsSias: boolean);
        class function WriteBoolToStr(aValue: boolean): string;
        class function StringToStringArray(aStrText: string): TStringDynArray;
        class function GetFixationNameFromDeviceName(aDeviceName: string): string;

        class function IsSias(): boolean;
        class property XOffsetLeftToTip1: TPosMM read uXOffsetLeftToTip1 write uXOffsetLeftToTip1;
        class property YOffsetFrontToTip1: TPosMM read uYOffsetFrontToTip1 write uYOffsetFrontToTip1;
    end;

    TLiquids = class
        class function EncodeSystemLiquidIdent(aIndex: integer): string;
        class function DecodeSystemLiquidIdent(aIdent: string): integer;
    end;

    TArmMotorType = (amtXMotor, amtYMotor, amtZMotor, amtGMotor, amtRMotor, amtCombinedZMotor);

    EXEC_MODE = (m_NO_EXEC, m_EXECUTE);
    TIPMAP = integer;

    TDeviceType = (dptNone, dptShakerCAT, dptShakerIKA, dptShakerIKAOld, dptSwitchShaker, dptSamShaker,
        dptVacuum, dptBlower, dptPump, dptAnySwitch, // irgendwelche Switch-Devices z.b.Feeding
        dptEntireGripper, dptPipPump, dptPhotometer, dptBCReader, dptTurntable, dptSensor, dptStateSwitch,
        dptSamPeriPump, dptWasher, dptThermostat, dptBalance, dptXWayValve, dptVarRediMotor, dptGripper,
        dptXMotor, dptYMotor, dptZMotor, dptPipZMotor, dptRMotor, dptGMotor, dptPneumaticGripOpenCloseSwitch,
        dptPneumaticGripDistanceSwitch, dptSimpleWatchDog,
        // composite devices
        dptCmpAny, dptCmpOverview, dptCmpWeigh, dptCmpPipArm, dptCmpGrpArm, dptCmpStateSignal,
        dptCmpSoftwareProtect, dptCmpBCReadSystem, dptCmpDecapper, dptCmpUserProtect, dptCmpArm,
        dptCmpMotorMotion, dptCmpLocationMotion, dptCmpYMotorsIndiv, dptCmpZMotorsIndiv, dptCmpYMotorsVSpan,
        dptCmpPipette, dptCmpRemPipette, dptCmpMotorGrip, dptCmpPneumaticGrip, dptCmpRedi);
    TProtocol = (cdtNone, cdtRosys, cdtCANBus, cdtOmnifit, cdtHuber, cdtConradRelay, cdtCompulab,
        cdtCATVortexer, cdtIKAVortexer, cdtDll, cdtRelayValve, cdtDLLBCReader, btMettlerAT, btMettlerAX,
        btSartGenius, btSartGeniusBin, btSartGeniusCellBin, cdtKolterOpto, cdtDLLEntireGripper, cdtSignalLoop,
        cdtEDOS, cdtCATPump, cdtDLLRelay, cdtPeerless);

    TDeviceTypes = set of TDeviceType;

    TThermoDeviceDataRec = record
        Adr: integer;
        Com: integer;
        Port: integer;
        ReadPort: integer;
        Protocol: TProtocol;
        SleepTime: integer;
    end;

    TIntfCommunication = class(TInterfacedObject)
    protected
        FProtocol: TProtocol;
        function GetProtocol: TProtocol;
    public
        constructor Create(aProtocol: TProtocol);
    end;

    TIntfCommPort = class(TIntfCommunication)
    protected
        function GetComPortNo: integer; virtual; abstract;
    public
        property ComPortNo: integer read GetComPortNo;
    end;

    TIntfDll = class(TIntfCommunication)
    protected
        FDllName: string;
    public
        constructor Create(aDllName: string; aProtocol: TProtocol);

        property DllName: string read FDllName;
    end;

    IIntfBCReader = interface
        procedure BCReader_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aAdr: integer; const aInitStr: string; aBCType: integer; aGetBCDelay: integer);
    end;

    IIntfSensor = interface
        procedure Sensor_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string; aAdr: integer;
            const aBit: string; aDefaultValue: integer);
    end;

    IIntfSwitch = interface
        procedure Switch_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string; aAdr: integer;
            const aBit, aDefaultBit, aDelayedBit: string; aReverse: boolean;
            aSwitchOffDelay, aSwitchOnDelay: integer; aNeverResetAtInit, aNeverResetAtReset: boolean);
    end;

    IIntfVortexer = interface
        procedure Mixer_WriteDriverData(aSettingDP: TDataProvider; aPort: integer; const aDeviceName: string);
    end;

    IIntfThermostat = interface
        procedure Thermostat_WriteDriverData(aSettingDP: TDataProvider; const aDeviceName: string;
            aData: TThermoDeviceDataRec);
    end;

    IIntfBCTurntable = interface
        procedure Switch_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string; aAdr: integer;
            const aBit, aDefaultBit, aDelayedBit: string; aReverse: boolean;
            aSwitchOffDelay, aSwitchOnDelay: integer; aNeverResetAtInit, aNeverResetAtReset: boolean);
        procedure Motor_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aExecID, aAdr, aSpeed, aRamp, aInitSpeed: integer);
    end;

    // Motor
    IIntfMotor = interface
        procedure Motor_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aExecID, aAdr, aSpeed, aRamp, aInitSpeed: integer);
    end;

    // Entire Gripper Arm
    IIntfLocationBasedMotion = interface
    end;

    IIntfGripper = interface
    end;

    // Relay-Board
    TIntfRelay = class(TIntfCommPort, IIntfSwitch)
    private
        fConnectionName: string;

    public
        constructor Create(aProtocol: TProtocol);
        procedure Switch_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string; aAdr: integer;
            const aBit, aDefaultBit, aDelayedBit: string; aReverse: boolean;
            aSwitchOffDelay, aSwitchOnDelay: integer; aNeverResetAtInit, aNeverResetAtReset: boolean);
            virtual; abstract;
    end;

type
    // Relay-Board
    TIntfRelayWithSensor = class(TIntfRelay, IIntfSensor)
    public
        procedure Sensor_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string; aAdr: integer;
            const aBit: string; aDefaultValue: integer); virtual; abstract;
    end;

    PDevice = ^TDevice;
    TDeviceClass = class of TDevice;

    TDevice = class(TInterfacedObject)
    protected
        FName: string;
        FDType: TDeviceType;
        //
        function GetPortAdrBit: string; virtual;
        class function ContainsTypeName(const aTypeName: string; const aTypeStr: string): boolean;
        //
        constructor Create(aName: string; aDType: TDeviceType);
    public
        // public methods
        class function GetTypeFromTypeName(aTypeStr: string; aAdr, aCom: integer; aBit: string): TDeviceType;
        class function BitValueIsDefined(const aBit: string; aZeroBased: boolean = true): boolean;
        class function GetSamplerProtocol(): TProtocol;
        class function GetGripperArmName(aArmNumber: integer): string;
        class function GetPipetteArmName(aArmNumber: integer): string;

        // public methods
        procedure Find_ByName(var aDevice: TDevice; aName: string); virtual;
        procedure Find_ByClass(var aDevice: TDevice; aClass: TDeviceClass); virtual;
        procedure Find_ByNameAndClass(var aDevice: TDevice; aName: string; aClass: TDeviceClass); virtual;
        procedure Find_ByNameCut(var aDevice: TDevice; aSubString: string; aClass: TDeviceClass); virtual;
        procedure FindName_ByClass(aList: TStrings; aClass: TDeviceClass); virtual;

        procedure WriteDeviceData(aSettingDP: TDataProvider); virtual; abstract;

        // properties
        property name: string read FName write FName;
        property DType: TDeviceType read FDType;
        property PortAdrBit: string read GetPortAdrBit;
    end;

    TLeafDeviceClass = class of TLeafDevice;

    TLeafDevice = class(TDevice)
    protected
        FCarrierName: string;
        FRackName: string;
        FDelay: integer;
        FRow, FCol: integer;
        //
        class function GetProtocolFromType(aDType: TDeviceType; aCom, aBit: integer): TProtocol;
        class function GetProtocolFromProtName(aProtName: string): TProtocol;
        // read from device ini methods:
        function Settings_ReadProtocol(aSettingDP: TDataProvider; const aDevArea: string): TProtocol;
        function Settings_ReadBit(aSettingDP: TDataProvider; const aDevArea: string): string;
        function Settings_ReadDefaultValue(aSettingDP: TDataProvider; const aDevArea: string): integer;

    public
        constructor Create(aName, aAreaName: string; aDType: TDeviceType);
        // public methods
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
        // properties (read/write)
        property Delay: integer read FDelay write FDelay;
        property CarrierName: string read FCarrierName write FCarrierName;
        property RackName: string read FRackName write FRackName;
        property Row: integer read FRow write FRow;
        property Col: integer read FCol write FCol;
    end;

    TSwitchDevice = class(TLeafDevice)
    private
        FIntf: IIntfSwitch;

        fReverse: boolean;
        fSwitchOffDelay: integer;
        fSwitchOnDelay: integer;

        fAdr: integer;
        fBit: string;
        fDefaultBit: string;
        fDelayedBit: string;
        fNeverResetAtInit: boolean;
        fNeverResetAtReset: boolean;

    public
        // constructor
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
            aDType: TDeviceType; aAdr, aCom: integer; const aBit, aDefaultBit, aDllName: string;
            aNeverResetDefault: boolean = false; aProtocolDefault: TProtocol = cdtNone);

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TCompositeDevice = class(TDevice)
    private
        function FindCompositeDevice(aName: string): TCompositeDevice;
        function ReadAndAddLeafDevice(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName, aTypeName,
            aCarrierName, aRackName: string; aRow, aColumn: integer): TLeafDevice;
        function AddSwitchDevice(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
            aDevType: TDeviceType; aAdr, aCom: integer; aBit, aDefaultBit: string; const aDllName: string)
            : TLeafDevice;
    protected
        fDeviceList: TObjectList<TDevice>;

    const
        INT_FIRST_DEVICE_LEVEL = 1;
    public
        constructor Create(aName: string; aDType: TDeviceType);
        destructor Destroy; override;

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;

        procedure Find_ByNameCut(var aDevice: TDevice; aSubString: string; aClass: TDeviceClass); override;
        function Find_SwitchByNameCut(aSubString: string): TSwitchDevice;
        procedure Find_ByClass(var aDevice: TDevice; aClass: TDeviceClass); override;

        function AddCompositeDevice(aSettingDP: TDataProvider; const aDevArea: string;
            const aSection, aName: string; aRediDeviceNames: TArray<string>): TCompositeDevice;
        function ReadAndAddComposedDevice(aSettingDP: TDataProvider; const aDevArea: string;
            const aDeviceName: string; aLevel: integer; aRediDeviceNames: TArray<string>): boolean;
        function AddLeafDevice(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName, aTypeName,
            aCarrierName, aRackName: string; aAdr, aCom: integer; aBit, aDefaultBit: string;
            aRow, aColumn: integer; aTipIndex: integer = 0; aMType: TArmMotorType = amtXMotor;
            const aDllName: string = ''): TLeafDevice;
        property Devices: TObjectList<TDevice>read fDeviceList;
    end;

    TSystemState = (sStateActive, sStateSetup, sStateError, sStateUnknown);

    // State Switch Device - Ein Switch-Device, das bei einem System-Zustand (Active, Error, Setup)
    // geschaltet wird
    TStateSwitchDevice = class(TSwitchDevice)
    private
        fActiveState: TSystemState;
    public
        constructor CreateOldStyle(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
            aAdr, aCom: integer; aBit: string; aActiveState: TSystemState);
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
            aDType: TDeviceType; aAdr, aCom: integer; const aBit, aDefaultBit, aDllName: string);

        class function GetStateFromStateName(const aStateName: string): TSystemState;
        class function GetStateNameFromState(aState: TSystemState): string;
        property ActiveState: TSystemState read fActiveState;
    end;

    TSensorDevice = class(TLeafDevice)
    private
        FIntf: IIntfSensor;
        FTipNumber: integer;
        fAdr: integer;
        fBit: string;
        FDefaultValue: integer;
    public
        // constructor/destructor
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
            aCom, aAdr: integer; aBit, aDllName: string; aPreferSignalLoop: boolean = false);

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TBCReaderDevice = class(TLeafDevice)
    private
        FIntf: IIntfBCReader;
        FCodeFilter: string;
        fInitFunc: string;
        fAdr: integer;
        fBCType: integer;
        fInitStr: string;
        fGetBCDelay: integer;
    public
        // constructor/destructor
        constructor CreateOldStyle(aName, aAreaName, aCodeFilter: string);
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string);

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
        property CodeFilter: string read FCodeFilter;
        property Intf: IIntfBCReader write FIntf;
    end;

    MPOS = integer; // Motor Position oder Längenangabe in Steps (auch Speed oder Ramp)
    MposArray = array [0 .. MAX_TIPS - 1] of MPOS;

    ACTION_TYPE = (AT_MOVE_ABS, AT_BLOCK_MOVE1, AT_BLOCK_MOVE2, AT_BLOCK_MOVE3, AT_SCAN_MOVE, AT_MOVE_REL,
        AT_VCLOSE_MOVE);

    TMovoThruPathPos = (pposAll, pposFirstPosOnly, pposLastPosOnly, pposAllWithoutFirstPos,
        pposAllWithoutLastPos);

    TMotorUnit = (mu_MM, mu_Degree);

    TMotorDevice = class(TLeafDevice)
    private
        function GetMaxPos_Unit: TPosMM;
        function GetMinPos_Unit: TPosMM;
        function GetMaxPos_Steps: MPos;
        function GetMinPos_Steps: MPos;
        function GetMotorUnit: TMotorUnit;
        function GetStepsPerUnit: Extended; // (degrees for rotation motor)
        function GetDefaultRamp: integer;
        function GetDefaultSpeed: integer;

    protected
        FIntf: IIntfMotor;
        fExecID: integer;
        fAdr: integer;
        FMType: TArmMotorType;
        fWorldOffset_Steps: MPos; // At which world position does the motor initialize
        fReverseM: boolean; // is the motor reversed
        fReverseChanged: boolean;
        fMin_Steps, fMax_Steps: MPos; // in Local motor steps
        fSpeed, fRamp, fInitSpeed: MPos;
        fInitOffset: MPos;
        fStepsPerUnit: Extended;
        fTrackID: integer;

        function GetActive(): boolean;
        function CalcReverse_Steps(aMotorPos: MPOS): MPOS;
        function CalcReverse_MM(aMotorPos_mm: TPosMM): TPosMM;
        function GetNewWorldOffset_mm: TPosMM; virtual;
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string; aAdr: integer;
            aDType: TDeviceType; aMType: TArmMotorType);

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;

        // public functions
        function GetUnitFromSteps(aValue: MPos): TPosMM;
        function GetStepsFromUnit(aValue: TPosMM): MPos;
        function DistanceToMin_Steps(aWorldDest: MPos): MPos;
        function DistanceToMax_Steps(aWorldDest: MPos): MPos;
        function IsBeyondMin_Steps(aWorldDest: MPos): boolean;
        function IsBeyondMax_Steps(aWorldDest: MPos): boolean;
        function IsBeyondMinOrMax_Steps(aWorldDest: MPos): boolean;
        function GetWorldInitOffset_Steps(): MPos;

        procedure SetDepracatedProperties(aWorldOffset_Steps: MPos; aReverse: boolean);
        // Use of this function is strictly forbidden!
        function MotorToWorld_Steps(aMotorPos: MPOS): MPOS;
        function WorldToMotor_Steps(aWorldPos: MPOS): MPOS;

        // properties
        property MotorType: TArmMotorType read FMType;
        property MinPos_Unit: TPosMM read GetMinPos_Unit;
        property MaxPos_Unit: TPosMM read GetMaxPos_Unit;
        property MotorUnit: TMotorUnit read GetMotorUnit;
        property DefaultSpeed: integer read GetDefaultSpeed;
        property DefaultRamp: integer read GetDefaultRamp;
        property ExecID: integer read fExecID write fExecID;
        property InitOffset: MPos read fInitOffset write fInitOffset;
        property WorldOffset: MPos read fWorldOffset_Steps;
        property Adr: integer read fAdr;
    end;

    TGMotorDevice = class(TMotorDevice)
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string; aAdr: integer;
            aDType: TDeviceType);
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TXMotorDevice = class(TMotorDevice)
    protected
        fConflictBufferMinus_MM, fConflictBufferPlus_MM: TPosMM;
        function GetNewWorldOffset_mm: TPosMM; override;
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string; aAdr: integer;
            aDType: TDeviceType);
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TYMotorDevice = class(TMotorDevice)
    protected
        fMotorIndex: integer;
        function GetNewWorldOffset_mm: TPosMM; override;
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string; aAdr: integer;
            aDType: TDeviceType);
        property MotorIndex: integer read fMotorIndex;
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TZMotorDevice = class(TMotorDevice)
    protected
        fMotorIndex: integer;
        fUseRealBlockMove: boolean;
        fLiquidDetection: boolean;
        fScanSpeed, fRetractSpeed, fWashRetractSpeed: MPos;
        function GetNewWorldOffset_mm: TPosMM; override;
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string; aAdr: integer;
            aDType: TDeviceType);
        property MotorIndex: integer read fMotorIndex;
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
        function GetNewZTavelValue(): TPosMM;
    end;

    TRMotorDevice = class(TMotorDevice)
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string; aAdr: integer;
            aDType: TDeviceType);
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TMultiYMotorDevice = class(TCompositeDevice)
    protected
        fYOffset: MPos;
        fInitOffset: MPos;
        fExecID: integer;
        procedure SetExecID(aExecID: integer); virtual; abstract;
    public
        procedure LoadDevices(); virtual; abstract;
        property ExecID: integer read fExecID write SetExecID;
    end;

    TMotorDeviceArray = array [0 .. MAX_TIPS - 1] of TMotorDevice;

    TIndividualMultiYMotorDevice = class(TMultiYMotorDevice)
    protected
        fMotors: TMotorDeviceArray;
        fMapAllToFirst: boolean;
        function GetMotor(aIndex: Integer): TMotorDevice;

        procedure SetExecID(aExecID: integer); override;

    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aSection: string);
        procedure LoadDevices(); override;

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
        procedure MultiY_WriteDeviceData(aSettingDP: TDataProvider; const aName: string);
    end;

    TSingleMultiYMotorDevice = class(TIndividualMultiYMotorDevice)
    protected
        function GetYMotor(): TYMotorDevice;
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName: string; aYMotor: TYMotorDevice);
        property YMotorDevice: TYMotorDevice read GetYMotor;
    end;

    TMultiZMotorDevice = class(TCompositeDevice)
    private
        fMotors: TMotorDeviceArray;
        fExecID: integer;
        fMapAllToFirst: boolean;
        procedure SetExecID(aExecID: integer);
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aSection: string);
        procedure LoadDevices();
        function GetMotor(aIndex: Integer): TMotorDevice;
        property ExecID: integer read fExecID write SetExecID;

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
        procedure MultiZ_WriteDeviceData(aSettingDP: TDataProvider; const aName: string);
        function GetNewZTravelValue(): TPosMM;
        function GetStepsPerUnit(): extended;
    end;

    TSingleMultiZMotorDevice = class(TMultiZMotorDevice)
    protected
        function GetZMotor(): TZMotorDevice;
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aSection: string;
            aZMotor: TZMotorDevice);
        property ZMotorDevice: TZMotorDevice read GetZMotor;
    end;

    TMultiAndCombinedZMotorDevice = class(TMultiZMotorDevice)
        fMultiZMotorDevice: TMultiZMotorDevice;
    end;

    TStateSignalDevice = class(TCompositeDevice)
    public
        constructor Create(const aName: string);
        function AddSwitch(aSettingDP: TDataProvider; const aDevArea, aName: string;
            aActiveState: TSystemState; aCom: integer; aBit: string): TSwitchDevice;
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TMotionDevice = class(TCompositeDevice)
    public
        function GetNewZTravelValue(): TPosMM; virtual; abstract;
        function GetStepsPerUnit(): extended; virtual; abstract;
    end;

    TMotorBasedMotionDevice = class(TMotionDevice)
    private
        function FindRMotor: TRMotorDevice;
        function FindXMotor: TXMotorDevice;
        function FindYMotor: TYMotorDevice;
        function FindYMotors: TMultiYMotorDevice;
        function FindZMotor: TZMotorDevice;
        function FindZMotors: TMultiZMotorDevice;
    protected
        fExecID: integer;
        fXMotor: TXMotorDevice;
        fYMotors: TMultiYMotorDevice;
        fOwnsYMotors: boolean;
        fZMotors: TMultiZMotorDevice;
        fOwnsZMotors: boolean;
        fRMotor: TRMotorDevice;

        function GetYMotor: TYMotorDevice;
        function GetZMotor: TZMotorDevice;

        procedure SetYMotor(aSettingDP: TDataProvider; const aDevArea: string; aYMotor: TYMotorDevice);
        procedure SetZMotor(aSettingDP: TDataProvider; const aDevArea: string; aZMotor: TZMotorDevice);
    public
        constructor Create(aName: string);
        destructor Destroy(); override;
        procedure LoadDevices(aSettingDP: TDataProvider; const aDevArea: string);

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
        function GetNewZTravelValue(): TPosMM; override;
        function GetStepsPerUnit(): extended; override;

        property XMotor: TXMotorDevice read fXMotor write fXMotor;
        property YMotor: TYMotorDevice read GetYMotor;
        property YMotors: TMultiYMotorDevice read fYMotors write fYMotors;
        property ZMotor: TZMotorDevice read GetZMotor;
        property ZMotors: TMultiZMotorDevice read fZMotors write fZMotors;
        property RMotor: TRMotorDevice read fRMotor write fRMotor;
        property ExecID: integer read fExecID write fExecID;

    end;

    TGripDevice = class(TCompositeDevice)
    public
        constructor Create(aName: string; aDType: TDeviceType);
        procedure LoadDevices(); virtual;
    end;

    TSimpleGripDevice = class(TGripDevice)
    protected
        fIntf: IIntfGripper;
    end;

    TMotorGripDevice = class(TGripDevice)
    protected
        fGMotor: TGMotorDevice;
    public
        constructor Create(const aName, aSection: string);
        procedure LoadDevices(); override;

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;

        property GMotor: TGMotorDevice read fGMotor;
    end;

    TPneumaticGripOpenCloseSwitchDevice = class(TSwitchDevice)
    public
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TPneumaticGripDistanceSwitchDevice = class(TSwitchDevice)
    protected
        fDistance: TPosMM;
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
            aDType: TDeviceType; aAdr, aCom: integer; const aBit, aDefaultBit, aDllName: string);

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
        property Distance: TPosMM read fDistance;
    end;

    TPneumaticGripDevice = class(TGripDevice)
    protected
        fMin, fMax: TPosMM;
        fCloseDelay, fOpenDelay: integer;
        fOpenCloseSwitch: TPneumaticGripOpenCloseSwitchDevice;
        fDistSwitch1: TPneumaticGripDistanceSwitchDevice;
        fDistSwitch2: TPneumaticGripDistanceSwitchDevice;
        fDistSwitch3: TPneumaticGripDistanceSwitchDevice;
        function DetermineClosestDestFromDistSwitch(aDest: TPosMM; aDefault: TPosMM;
            aDirection: integer): TPosMM;
        function DetermineClosestDest(aDest: TPosMM; aDirection: integer): TPosMM;
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aSection: string);
        procedure LoadDevices(); override;
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TValvePos = (V_SYR_TIP, V_PER_TIP, V_SYR_SYS, V_PER_SYS);
    TVPosArray = array [0 .. 4] of Word;

    // PipPump: allgemeine Formulierung für SyringePump und GearPump (, PeriPump)
    IIntfPipPump = interface
        function GetProtocol: TProtocol;
        procedure PipPump_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aExecID, aAdr, aMaxVol_uL, aMaxVolSteps, aASpeed, aARamp, aDSpeed, aDRamp,
            aScalingFactor: integer; aValvePos: TVPosArray);
    end;

    TLiquidPumpDevice = class(TLeafDevice)
    end;

    TPipPumpDevice = class(TLiquidPumpDevice)
    private
        FIntf: IIntfPipPump;
        fAdr: integer;
        fBit: integer;
        fDllName: string;
        FTipIndex: integer;
        fInputPortName: string;
        fMaxVol_uL: integer;
        fMaxVolSteps: integer;
        fAspSpeed, fDispSpeed: integer;
        fAspRamp, fDispRamp: integer;
        fScalingFactor: integer;
        fValvePos: TVPosArray;
        fVolULToStepFactor: extended;
        fCurrentRoundedVolumeRest: extended;
        fExecID: integer;

    public
        // constructor/destructor
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName, aDllName: string;
            aAdr, aBit: integer; aTipIndex: integer);

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
        //
        function VolumeULToVolumeSteps(aVolumeUL: extended): extended;
        procedure LoadDevice(aSettingDP: TDataProvider; const aDevArea: string);

        property TipIndex: integer read FTipIndex;
        property ExecID: integer read fExecID write fExecID;
    end;

    TPipPumpDeviceArray = array of TPipPumpDevice;

    TPipPumpDeviceFactory = class
    public
        class function CreatePipPumpDevice(aSettingDP: TDataProvider;
            const aDevArea, aName, aAreaName, aDllName: string; aAdr, aBit: integer; aTipIndex: integer)
            : TPipPumpDevice;
        class function CreateIntfAsPipPump(aSettingDP: TDataProvider; const aName: string;
            aDevType: TDeviceType; const aDLLName: string; aAdr, aBit: integer;
            aAspSpeed, aAspRamp, aDispSpeed, aDispRamp: integer; aValvePos: TVPosArray;
            aMaxVol_uL, aMaxVolSteps, aScalingFactor: integer; aExecID: integer): IIntfPipPump;
    end;

    TPipDevice = class(TCompositeDevice)

    private
        // from customarm
        fUsesAddDilutors: boolean;
        fRackNamePrefix: string;
        fPipPumpsExecID: integer;

    protected
        // from customarm
        FTipCount: integer;

    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aSection: string;
            aDevType: TDeviceType);
        procedure LoadDevices(aSettingDP: TDataProvider; const aDevArea: string);

        property TipCount: integer read FTipCount;

        property RackNamePrefix: string read fRackNamePrefix write fRackNamePrefix;
        property ExecID: integer read fPipPumpsExecID write fPipPumpsExecID;
    end;

    TRemovablePipDevice = class(TPipDevice)
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aSection: string);
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TAttachedPipDevice = class(TPipDevice)
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aSection: string);
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TTurntableAndBCReader = class(TCompositeDevice)
    protected
        FTurntable: TSwitchDevice;
        FBCReader: TBCReaderDevice;
        FNoOfTurns: integer;
        FDevLoaded: boolean;
    end;

    TDecapperDevice = class(TTurntableAndBCReader)
    private
        FState1Rotation: integer;
        FState2Rotation: integer;
        FTubeReleaseDelay: integer;
        FCapTime: integer;
        FDecapTime: integer;
        FSwiwelLifter: TSwitchDevice;
        FCapLifter: TSwitchDevice;
        FCapGripper: TSwitchDevice;
        FTubeHolder: TSwitchDevice;
        FPrepare: TSwitchDevice;
        FCapSensor: TSensorDevice;
        FTubeSensor: TSensorDevice;
        FUseCapSensor: boolean;
        FUseTubeSensor: boolean;
        FTubeDropBeforeIntake: boolean;
        FCapDropBeforeIntake: boolean;
        FSwiwelShortSwitch: boolean;
        FRepeatDecapping: integer;
        fSwiwelUpBeforeIntake: boolean;
        fSwiwelDownBeforeTakeCap: boolean;
        fSwiwelSwitchBoth: boolean;
        fSkipCapCheckAtInit: boolean;
        fTurnLeftBeforeCapTime: integer;
        fCapWasteBelowCapLifter: boolean;
        fNoCapliftWithoutCap: boolean;
        fCappingTurntableFirst: boolean;

        procedure LoadDevices;
    public
        //
        constructor Create(aSettingDP: TDataProvider; const aArea, aName, aIdentName: string);

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TMoveTubeMode = (mtmNoStartPosBeforeGetTube, mtmNoStartPosAfterGetTube, mtmNoStartPosBeforePutTube,
        mtmNoStartPosAfterPutTube);
    TMoveTubeModes = set of TMoveTubeMode;

    TArmWithVarispanValues = record
        VMax, RefPos, VFactor, VOffset, OptimizeTipCouple, CloseVfor1Tip: integer;
        CalcYMaxUsingVOffset: boolean;
    end;

    TZTravelManager = class
    protected
        // ZTravel Steps : the values stored in these variables MUST be in Local motor steps NOT Global motor steps
        fDefault: MPos;
        function GetNewValue(aValue_Steps: MPos; aBasicZTavelValue: TPosMM; aStepsPerUnit: extended): TPosMM;
    public
        constructor Create(aDefault: MPos);
        procedure ZTravelManager_WriteDeviceData(const aName: string; aSettingDP: TDataProvider;
            aBasicZTavelValue: TPosMM; aStepsPerUnit: extended); virtual;
    end;

    TZTravelMode = (ztNormal, ztPipTool, ztTool, ztTube);

    TGripZTravelManager = class(TZTravelManager)
        // ZTravel Steps : the values stored in these variables MUST be in Local motor steps NOT World motor steps
    protected
        fPipTool: MPos;
        fPipToolMin: MPos;
        fTool: MPos;
        fTubeMin: MPos;
    public
        constructor Create(aSettingDP: TDataProvider; aDefault: MPos);
        procedure ZTravelManager_WriteDeviceData(const aName: string; aSettingDP: TDataProvider;
            aBasicZTavelValue: TPosMM; aStepsPerUnit: extended); override;
    end;

    // Robotic arm devices
    TRobotArmDevice = class(TCompositeDevice)
    private
        fPipDevice: TPipDevice;
        fGripDevice: TGripDevice;
        fMotionDevice: TMotionDevice;

        fZTravelManager: TZTravelManager;
        fBasicZTravelValue: TPosMM;

        fXYRange: TXYRangeData;
        fRediZRetrSpeed: MPos;
        fZTravel: MPos;

        fMoveZTravelIfNoXYMove: boolean;
        fHandlerXYIndependetFromRotation: boolean;
        fExecID: integer;
        fArmGroupID: integer;
        fColor: string;
        fInitXBeforeY: boolean;
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aSection: string;
            aDType: TDeviceType);

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
        procedure LoadArmDevices(aSettingDP: TDataProvider; const aDevArea: string);
        property MotionDevice: TMotionDevice read fMotionDevice;
        property BasicZTravelValue: double read fBasicZTravelValue write fBasicZTravelValue;
    end;

    // CRS-Arm
    TGripperArmDevice = class(TCompositeDevice)
    private
        // aus ToolHandling:
        FToolVarispanFactor: double;
        fIsXBasedMotorSystem: boolean;
        fDepracatedXWorldOffset: MPos;
        fDepracatedYWorldOffset: MPos;
        fDepracatedZWorldOffset: MPos;
        fDepracatedXReverse: boolean;
    public
        // constructor
        constructor Create(aSettingDP: TDataProvider; aName: string);
    end;

    IIntfBalance = interface
        procedure Balance_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aBalanceDoorBlocked: Boolean; aDoorNoOfRequests: integer; aDoorType: byte;
            aSetupFuncs: TStringList);
    end;

    TIntfBalance = class(TIntfCommPort, IIntfBalance)
    public
        procedure Balance_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aBalanceDoorBlocked: Boolean; aDoorNoOfRequests: integer; aDoorType: byte;
            aSetupFuncs: TStringList); virtual; abstract;
    end;

    TBalanceDevice = class(TLeafDevice)
        // unnütz
    end;

    TBalanceDevice_1 = class(TBalanceDevice) // uses the TBalance class
    private
        fBalanceDoorBlocked: Boolean;
        FIntf: IIntfBalance;

        // const
        FTaraAfterXTimes: integer;
        FCloseDoorAfterInit: boolean;
        FUseSoftwareTara: Boolean;
        FDoorNoOfRequests: integer;
        FDoorType: byte;
        FStableReset: boolean;
        FStableTare: boolean;
        FTimeout: integer;
        FSetupFuncs: TStringList;

        procedure CreateSetupFuncsList(aSettingDP: TDataProvider; aName: string);
        procedure ConfigInterface(aSettingDP: TDataProvider; aName: string); virtual;
    public
        // constructor / destructor
        constructor CreateOldStyle(aSettingDP: TDataProvider; aIntf: IIntfBalance; aModuleIndex: integer;
            aName: string; aTimeout, aTaraAfterXTimes: integer; aCloseDoorAfterInit: boolean);
        class function ReadAndCreate(aSettingDP: TDataProvider; const aDevArea: string; aIntf: IIntfBalance;
            aModuleIndex: integer; aName: string): TBalanceDevice_1;

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TBalanceDeviceSartGenius = class(TBalanceDevice_1)
    private
        FIonizerTimeInSec: Shortint; // How long to keep the ionizer on (in seconds)
    protected
        procedure ConfigInterface(aSettingDP: TDataProvider; aName: string); override;
    end;

    TBalanceDeviceFactory = class
        class function CreateBalanceDeviceOldStyle(aSettingDP: TDataProvider; aName: string;
            aLeft, aTop, aTaraAfterXTimes: integer; aCloseDoorAfterInit: boolean): TBalanceDevice_1;
        class function CreateBalanceDevice(aSettingDP: TDataProvider; const aDevArea, aName: string)
            : TBalanceDevice_1;
        class procedure CreateBalanceDeviceInterface(aSettingDP: TDataProvider; aName: string;
            var aIntf: IIntfBalance; var aProtocol: tProtocol; var aModuleIndex: integer);

    end;

    TUserProtectionDevice = class(TCompositeDevice)
    protected
        fSensor: TSensorDevice;
        fProtectionOn: boolean;
        fProtectionPaused: boolean;
        fSwitchDLLName: string; // this should really be a switch device!
        fCheckDllName: string;
        fPollDelay: integer;
        fAutoReactivate: boolean;
        fAutoReactivatePollDelay: integer;
        fMessageText: string;
        function GetSensor: TSensorDevice;
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName: string; aIdentName: string);
        constructor CreateOldStyle(aSettingDP: TDataProvider; const aDevArea, aName: string;
            aIdentName: string; aDummy: boolean);
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TWeighingDevice = class(TCompositeDevice)
    private
        FUseTubeSensor: boolean;
        function GetBalance: TBalanceDevice;
        function GetDoorSwitch: TSwitchDevice;
        function GetTubeSensor: TSensorDevice;
    public
        constructor CreateOldStyle(aName: string; aBalance: TBalanceDevice);
        constructor Create(aName: string);
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TVarRediMotorDevice = class(TLeafDevice)
    private
        fComPort: integer;
    public
        // constructor / destructor
        constructor Create(aName, aAreaName: string; aCom: integer);

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TRediDevice = class(TCompositeDevice)
    private
        fPump: TSwitchDevice;
        fVacuum: TSwitchDevice;
        fBlower: TSwitchDevice;
        fMotor: TVarRediMotorDevice;
        fFeeding: TSwitchDevice;
        procedure LoadDevices();
    public
        constructor Create(aName: string; aSection: string);

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    IIntfWatchDog = interface
        procedure WatchDog_AddPort(aPort: integer; aTriggerInterval: integer);
    end;

    TIntfDLLWatchDog = class(TIntfDll, IIntfWatchDog)
    public
        constructor Create(const aDllName: string);
        procedure WatchDog_AddPort(aPort: integer; aTriggerInterval: integer);
    end;

    TWatchDogDevice = class(TLeafDevice)
    end;

    TSimpleWatchDogDevice = class(TWatchDogDevice)
    protected
        fIntf: IIntfWatchDog;
        fIntervalInS: integer;
        fDllName: string;
        fPort: integer;
        fWaitTimeInMS: integer;
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aSection: string;
            aDevType: TDeviceType);
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TSoftwareProtectionDevice = class(TCompositeDevice)
    protected
        fDevLoaded: boolean;
        fWatchDogDevice: TWatchDogDevice;
    public
        constructor Create(aName: string; aSectionName: string);
        procedure LoadDevices;
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TIntfCommunicationList = class
    protected
        FList: TObjectList<TIntfCommunication>;
    private
        function GetCount: integer;
        function GetItem(aIndex: Integer): TIntfCommunication;
        procedure SetOwnsObjects(aOwnsObjects: boolean);
    public
        // constructor/destructor
        constructor Create;
        destructor Destroy; override;
        // Public Methods
        function Add(aItem: TIntfCommunication): Integer;
        function GetNextIndex: Integer;
        // Properties
        property Count: integer read GetCount;
        property Items[index: Integer]: TIntfCommunication read GetItem;
        property OwnsObjects: boolean write SetOwnsObjects;
    end;

    TGlobalErrorRec = record
        Value: integer;
        Reason: string;
    end;

    TXWayValveDevice = class(TLeafDevice)
    private
        fInputPortNames: TStringList;
        function GetInputPortCount: integer;
        function GetInputPortName(index: Integer): string;
        procedure ReadInputPorts(aSettingDP: TDataProvider; const aDevArea: string; aNoOfPorts: integer);
    public
        constructor Create(aName, aAreaName: string; aDType: TDeviceType);
        destructor Destroy; override;
        // public methods
        procedure GetPortNames(aPortNames: TStrings);
        // properties
        property InputPortCount: integer read GetInputPortCount;
        property InputPortName[index: Integer]: string read GetInputPortName;
    end;

    T6WayValveDevice = class(TXWayValveDevice)
    private
        FComPortNo: integer;
        FTimeout: Integer;
        FSleepTime: Integer;
        FRetryCnt: integer;
        fResetSwitchDevice: string;
    public
        // constructor / destructor
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
            aCom: integer);
        constructor CreateOldStyle(aName, aAreaName: string; aCom: integer;
            aTimeout, aSleepTime, aRetryCnt: integer; aInPort6Name: string);

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TXWayValveSwitchDevice = class(TXWayValveDevice)
    private
        FIntf: IIntfSwitch;
        fAdr: integer;
        fBit: string;
    public
        // constructor / destructor
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
            aAdr, aCom: integer; const aBit, aDllName: string);
        constructor CreateOldStyle(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
            aCom, aBit: integer; aInPort1Name, aInPort2Name: string);

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TShakerDevice = class(TLeafDevice)
    public
    end;

    TSimpleShakerDevice = class(TShakerDevice) // nur an/aus
    private
        fIntf: IIntfSwitch;
        fShaking: boolean;
        FSwitchOffDelay: integer;
        FReverse: boolean;
        FMagnetReverse: boolean;
        fAdr: integer;
        fBit: string;
        fDelayedBit: string;
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
            aDType: TDeviceType; aAdr, aCom: integer; const aBit, aMagnetBit, aDllName: string);
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TSpeedShakerDevice = class(TShakerDevice) // mit regelbarer Geschwindigkeit
    protected
        fIntf: IIntfVortexer;
        fFixAfterVortexing: boolean;
        fBit: integer;
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
            aDType: TDeviceType; aCom, aBit: integer);
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    TThermoDevice = class(TLeafDevice)
    protected
        fIntf: IIntfThermostat;
        fDeviceData: TThermoDeviceDataRec;
    public
        constructor Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName, aTypeName: string;
            aAdr, aCom, aBit: integer);
        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    // ------------------------------------------------------------------------------ Turntable Motor
    TBCTurntableDevice = class(TLeafDevice)
    private
        FIntf: IIntfBCTurntable;
        FTurnSteps: MPos;
        fTurnStepsWrapAt: MPos;
        fExecID: integer;
        fMotor: MOTOR;
        fBit: integer;
    public
        // constructor
        constructor Create(aName, aAreaName: string; aMotor: MOTOR; aTurnSteps: integer;
            aTurnStepsWrapAt: integer; aBit: integer);

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
        property ExecID: integer read fExecID write fExecID;
    end;

    // -----------------------------------------------------------------------------------------------
    // ------------------------------------------------------------------------------- Reading Devices
    TReadingDevice = class(TCompositeDevice)
    protected
        FBCReader: TBCReaderDevice;
        FBCPosition: BCRPOSITION;
        FBCScanRange: BCSCANRANGE;
        FBCDelLastPos: Byte; // 15.10.98 neu wl: Prüfziffer(n) abschneiden
    public
        class function IniReadBCRPosition(aSettingDP: TDataProvider; const aIniKey: string;
            aCheckValueExists: boolean; var vPos: BCRPOSITION): boolean;

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;

        property BCReader: TBCReaderDevice read fBCReader;
        property BCScanRange: BCSCANRANGE read fBCScanRange;
        property BCPosition: BCRPOSITION read fBCPosition;
        property BCDelLastPos: byte read fBCDelLastPos;
    end;

    TTubeReadingDevice = class(TReadingDevice)
    protected
        FNoOfTurns: integer;
        FTurnDevice: TBCTurntableDevice;
        fTurnSwitch: TSwitchDevice;
    public
        constructor Create(aBCReader: TBCReaderDevice; aBCPosition: BCRPOSITION; aBCScanRange: BCSCANRANGE;
            aBCDelLastPos: Byte; aTurntable: TBCTurntableDevice; aNoOfTurns: integer;
            aTurnSwitch: TSwitchDevice);
        function HasTurnDevice: boolean;

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;

        property TurnDevice: TBCTurnTableDevice read fTurnDevice;
        property NoOfTurns: integer read fNoOfTurns;
        property TurnSwitch: TSwitchDevice read fTurnSwitch;
    end;

    TRackReadingDevice = class(TReadingDevice)
    private
        FCodeFilter: string;
    public
        constructor Create(aBCReader: TBCReaderDevice; aBCPosition: BCRPOSITION; aBCScanRange: BCSCANRANGE;
            aCodeFilter: string; aBCDelLastPos: Byte);
        function GetCodeFilter: string;

        procedure WriteDeviceData(aSettingDP: TDataProvider); override;
    end;

    // -----------------------------------------------------------------------------------------------
    // Reader, der nur bei Action "ReadE" für Racks benutzt wird
    TRackDllBCReaderDevice = class(TLeafDevice)
    private
        FDLLReadBcFunc, FDLLReadBcPara: string;
    public
        constructor Create(aReadExtDLLName, aReadExtDLLFunc, aReadExtDLLParam: string);
    end;

    TComInterfaceErrorInfoType = (cieCheckActionTimeout, cieType1, cieType5, cieType7, cieType8,
        cieOmnifitTimeout);

    BSWITCH = record
        Adr: MAdr;
        Bit: Word;
        Off: Byte; // Value for off
        res: array [1 .. 1] of shortInt;
    end;

    TRoboticInterface = class(TIntfDll)
    end;

    // ----------------------------------------------------- SignalPort
    TIntfSignalLoop = class(TIntfCommPort, IIntfSensor)
    private
        fComPortNo: integer;
    protected
        function GetComPortNo: integer; override;
    public
        constructor Create(aComPort: integer; aSimulated: boolean);
        procedure AddSensor(out oModuleIndex: integer; aBit: string);
        procedure Sensor_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string; aAdr: integer;
            const aBit: string; aDefaultValue: integer);
    end;

    // ------------------------------------------------------------------ Huber Unistat Tango
    TIntfNewHuberTango = class(TIntfCommPort, IIntfThermostat)
    private
        fComPortNo: integer;
        FUseExternSensor: boolean;
    protected
        function GetComPortNo: integer; override;
    public
        constructor Create(aComPort: integer; aSimulated, aUseExternSensor: boolean);
        function Thermostat_GetInterfaceData(aIndex: integer; out aAdr: integer;
            out aBitName: string): boolean;
        procedure Thermostat_WriteDriverData(aSettingDP: TDataProvider; const aDeviceName: string;
            aData: TThermoDeviceDataRec);
    end;

    TDataVortexerCAT = record
        IsShaker: boolean;
        IsHeater: Boolean;
        Speed: integer;
        Temp: integer;
        ONPulse: integer; // Pulse-Funktion: Aktivzeit [s]
        OFFPulse: integer; // Pulse-Funktion: Wartezeit [s]
    end;

    // ------------------------------------------------------------------ CAT (Zipperer) Vortexer
    TIntfVortexerCAT = class(TIntfCommPort, IIntfThermostat, IIntfVortexer)
    private
        FVort: array [0 .. MAX_VORTEXERS] of TDataVortexerCAT;
        FNoOfVortexer: integer;
        FOpenFixationAtInit: boolean;
        FExtraSleepTime: integer;
        fComPort: integer;
        fComSleepTime: word;
        fConnectionWritten: boolean;
        fConnectionName: string;
        procedure WriteConnection(aSettingDP: TDataProvider);
    protected
        function GetComPortNo: integer; override;
    public
        // constructor/destructor
        constructor Create(aComPort: integer; aComSleepTime: word; aOpenFixationAtInit, aSimulated: boolean;
            const cSleepTime: Integer);
        //
        procedure InitHeater(aIndex: integer);
        procedure Mixer_WriteDriverData(aSettingDP: TDataProvider; aPort: integer; const aDeviceName: string);
        //
        function Thermostat_GetInterfaceData(aIndex: integer; out aAdr: integer;
            out aBitName: string): boolean;
        procedure Thermostat_WriteDriverData(aSettingDP: TDataProvider; const aDeviceName: string;
            aData: TThermoDeviceDataRec);
    end;

    // ------------------------------------------------------------ neuer IKA Kreisschüttler (Vortexer)
    TIntfVortexerIKA = class(TIntfCommPort, IIntfVortexer)
    private
        fComPort: integer;
    protected
        function GetComPortNo: integer; override;
    public
        // constructor/destructor
        constructor Create(aComPort: integer; aSimulated: boolean);
        //
        procedure Mixer_WriteDriverData(aSettingDP: TDataProvider; aPort: integer; const aDeviceName: string);
    end;

    TIntfDllBCReader = class(TIntfDll, IIntfBCReader)
    private
        fDllName: string;
        fConnectionWritten: boolean;
    public
        // constructor/destructor
        constructor Create(const aDllName: string);
        procedure BCReader_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aAdr: integer; const aInitStr: string; aBCType: integer; aGetBCDelay: integer); virtual; abstract;
    end;

    TIntfDllBCReader_IT = class(TIntfDllBCReader)
    private
        fComPort: integer;
        fBaudRate: integer;
        fTimeout: integer;
        fDoBeep: boolean;
        fTriggerOffIfNoRead: boolean;
    public
        constructor Create(aSettingDP: TDataProvider; const aDllName: string);
        procedure BCReader_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aAdr: integer; const aInitStr: string; aBCType: integer; aGetBCDelay: integer); override;
    end;

    TIntfDllBCReader_WAIT = class(TIntfDllBCReader)
    private
        fComPort: integer;
        fSendConfig: boolean;
        fTimeout: integer;
        fTrigOnCode: string;
        fTrigOffCode: string;
    public
        constructor Create(aSettingDP: TDataProvider; const aDllName: string);
        procedure BCReader_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aAdr: integer; const aInitStr: string; aBCType: integer; aGetBCDelay: integer); override;
    end;

    TIntfDllBCReader_LEUZE = class(TIntfDllBCReader)
    private
        fComPort: integer;
        fSendConfig: boolean;
        fTimeout: integer;
    public
        constructor Create(aSettingDP: TDataProvider; const aDllName: string);
        procedure BCReader_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aAdr: integer; const aInitStr: string; aBCType: integer; aGetBCDelay: integer); override;
    end;

    TSGBalanceConfig = record
        IntCalcNumValues: integer;
        IntCalcDeviation: extended;
        IntCalcDo: boolean;
    end;

    TIntfDllBalance = class(TIntfBalance)
    private
        fName: string;
        fComPort: integer;
        fMaxSendTries: integer;
        fWeighValueResolution: integer;
        fConfig: TSGBalanceConfig;
        function ReadCalcValuesConfig(aSettingDP: TDataProvider; const aBalanceName: string;
            var vConfig: TSGBalanceConfig): boolean;
    protected
        function GetComPortNo: integer; override;
    public
        // public
        constructor Create(aSettingDP: TDataProvider; aDllName: string; aName: string; aComPort: integer);

        procedure Balance_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aBalanceDoorBlocked: Boolean; aDoorNoOfRequests: integer; aDoorType: byte;
            aSetupFuncs: TStringList); override;
    end;

    TIntfDllEntireGripper = class(TIntfDll, IIntfLocationBasedMotion, IIntfGripper)
    public
        // constructor/destructor
        constructor Create(aDllName: string; aSimulated: boolean);
    end;

    TIntfDllPipPump = class(TIntfDLL, IIntfPipPump)
    private
        fDllName: string;
        fConnectionWritten: boolean;
    public
        constructor Create(const aDllName: string);
        procedure PipPump_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aExecID, aAdr, aMaxVol_uL, aMaxVolSteps, aASpeed, aARamp, aDSpeed, aDRamp,
            aScalingFactor: integer; aValvePos: TVPosArray); virtual; abstract;
    end;

    TEdosConfigRec = record
        ComPort: integer;
        Timeoutsecs: integer;
        Baud: integer;
        DataBits: byte;
        StopBits: byte;
        Parity: char;
        SleepTime: integer;
        MaxSendRetry: integer;
        MaxMotorMoveTime: integer;
        MotorRequestDelay: integer;
        TypeAdress: byte;
        SubAdress: byte;
        ResetRepeat: integer;
        MinDropRepeat: integer;
        MaxDropRepeat: integer;
        MaxVolume: integer;
        DoRelax: integer;
        SilentPickDispErrors: integer;
    end;

    TIntfDllPipPump_CADI = class(TIntfDllPipPump)
    private
        fConfigRec: TEdosConfigRec;
    public
        // public
        constructor Create(aSettingDP: TDataProvider; const aDllName: string);
        procedure PipPump_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aExecID, aAdr, aMaxVol_uL, aMaxVolSteps, aASpeed, aARamp, aDSpeed, aDRamp,
            aScalingFactor: integer; aValvePos: TVPosArray); override;
    end;

    TCatPumpConfigRec = record
        StartPercent: integer;
        ComPort: integer;
        Answerdelay: integer;
        Timeout: integer;
        Baud: integer;
        TitSpeed: integer;
        DispSpeed: integer;
        MaxVolume: integer;
    end;

    TIntfDllPipPump_CATPUMP = class(TIntfDllPipPump)
    private
        fConfigRec: TCatPumpConfigRec;
        fPumpHead: integer;
    public
        // public
        constructor Create(aSettingDP: TDataProvider; const aDllName: string; aPumpHead: integer);
        procedure PipPump_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aExecID, aAdr, aMaxVol_uL, aMaxVolSteps, aASpeed, aARamp, aDSpeed, aDRamp,
            aScalingFactor: integer; aValvePos: TVPosArray); override;
    end;

    TIntfDllRelayWithSensor = class(TIntfRelayWithSensor)
    protected
        fDllName: string;
        fComPort: integer;
        fConnectionName: string;
        function GetComPortNo: integer; override;
    public
        // public
        class function FindOrCreate(const aDllName: string; aCom: integer): TIntfDllRelayWithSensor;
        constructor Create(const aDllName: string; aCom: integer);
    end;

    TIntfDllRelayWithSensor_Easy800 = class(TIntfDllRelayWithSensor)
    private
        fLinkConnections: TList<string>;
    public
        constructor Create(const aDllName: string; aCom: integer);
        destructor Destroy; override;

        procedure Switch_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string; aAdr: integer;
            const aBit, aDefaultBit, aDelayedBit: string; aReverse: boolean;
            aSwitchOffDelay, aSwitchOnDelay: integer;
            aNeverResetAtInit, aNeverResetAtReset: boolean); override;
        procedure Sensor_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string; aAdr: integer;
            const aBit: string; aDefaultValue: integer); override;
    end;

    TIntfBalanceOld = class(TIntfBalance)
    private
        FSendDelay: Integer;
        FMaxSendRetry: Integer;
        FDoorMoveDelay: Integer;
        FSerPortName: string;
        FSerPortComPort: integer;
        FSerPortBaud: integer;
        FSerPortDataBits: integer;
        FSerPortStopBits: integer;
        FSerPortParity: string;
        FSerPortTimeout: integer;
        FSerPortSleepTime: integer;
    protected
        function GetComPortNo: integer; override;
    public
        constructor Create(aSettingDP: TDataProvider; aSectionName: string; aProtocol: TProtocol;
            aComPort: integer; aSimulation: boolean);
        procedure Balance_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aBalanceDoorBlocked: Boolean; aDoorNoOfRequests: integer; aDoorType: byte;
            aSetupFuncs: TStringList); override;
    end;

    TIntfBitmapRelay = class(TIntfRelay)
    private
        FPortStatus: int64; // bis zu 64 ports möglich
        FNextPortStatus: int64;
        FInitPortStatus: int64;
        FResetPortStatus: int64;
        FNextDelay: integer;
    protected
        FMaxOutPorts: integer;
        FMaxInPorts: integer;
        //
        constructor Create(aProtocol: TProtocol; aMaxOutPorts, aMaxInPorts: integer);
    end;

    TIntfRelayConrad = class(TIntfBitmapRelay)
    private
        FSerPortComPort: integer;
        FBoardCount: integer;
        FBoardMaxPorts: integer;
    protected
        //
        function GetComPortNo: integer; override;
    public
        constructor Create(aComPortNo: integer; aSimulated: boolean);
        procedure Switch_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string; aAdr: integer;
            const aBit, aDefaultBit, aDelayedBit: string; aReverse: boolean;
            aSwitchOffDelay, aSwitchOnDelay: integer;
            aNeverResetAtInit, aNeverResetAtReset: boolean); override;
    end;

    // ------------------------------------------------------------------------ Compulab-Board
    TIntfRelayCompulab = class(TIntfBitmapRelay, IIntfSensor)
    private
        FComPortNo: integer;
    protected

        function GetComPortNo: integer; override;
    public
        //
        constructor Create(aComPortNo: integer; aSimulated: boolean);
        // public methods
        procedure Switch_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string; aAdr: integer;
            const aBit, aDefaultBit, aDelayedBit: string; aReverse: boolean;
            aSwitchOffDelay, aSwitchOnDelay: integer;
            aNeverResetAtInit, aNeverResetAtReset: boolean); override;
        procedure Sensor_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string; aAdr: integer;
            const aBit: string; aDefaultValue: integer);
    end;

    // -------------------------------- Kolter-Module: OPTO-4 USB, PROTO-2 USB 16/16, VModul I/O 16/16
    TIntfRelayKolter = class(TIntfBitmapRelay, IIntfSensor)
    private
        FComPortNo: integer;
    protected

        function GetComPortNo: integer; override;
    public
        // constructor / destructor
        constructor Create(aComPortNo: integer; aSimulated: boolean);
        procedure Switch_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string; aAdr: integer;
            const aBit, aDefaultBit, aDelayedBit: string; aReverse: boolean;
            aSwitchOffDelay, aSwitchOnDelay: integer;
            aNeverResetAtInit, aNeverResetAtReset: boolean); override;
        procedure Sensor_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string; aAdr: integer;
            const aBit: string; aDefaultValue: integer);
    end;

    TZinsserPlatform = (appZpUndefined, appZp01, // Rosys Plato
        appZp02); // Sias Xanthos - distributed by Zinsser

    TCommunicationManager = class
    protected
        FConradBoardPort: integer;
        FCommList: TIntfCommunicationList;
        FLastErrorRec: TGlobalErrorRec;
        FIsSafeExitRequested: boolean;
        fOnError: TNotifyEvent;
        fSimulationSpeed_Percent: integer;
        fSimulationAskWeight: boolean;
        //
        function GetSimulationMode: boolean; virtual;
        function FindCommPort(aSerialPort: integer): TIntfCommPort;
        function FindCommDll(aDllName: string): TIntfDll;
    public
        // constructor/destructor
        constructor Create(aSettingDP: TDataProvider);
        destructor Destroy; override;
        // Public Methods
        function GetDLLIntf(aDllName: string; aProtocol: TProtocol): TIntfDLL; virtual;

        class function GetBalanceProtocolFromType(aType: string): TProtocol;
        // properties
        property ConradBoardPort: integer read FConradBoardPort;
        property IsSafeExitRequested: boolean read fIsSafeExitRequested write fIsSafeExitRequested;
    end;

    TCommunicationRealManager = class(TCommunicationManager)
    private
        fCVortexSleepTime: integer;
        fOpenFixationAtInit: boolean;
        function GetRoboticInterface: TRoboticInterface;
        function CreateSwitchRelay(aCom: integer; const aBit, aDllName: string; aProtocol: TProtocol)
            : IIntfSwitch;
        function AddSamplerSwitch(aAdr, aBit: integer): IIntfSwitch;
        function AddRelaySwitch(aCom: integer; const aBit, aDllName: string; aProtocol: TProtocol)
            : IIntfSwitch;
        function CreateSensorRelay(aCom: integer; const aBit, aDllName: string; aProtocol: TProtocol)
            : IIntfSensor;
        function AddSamplerSensor(aAdr, aBit: integer): IIntfSensor;
        function AddRelaySensor(aCom: integer; const aBit, aDllName: string; aProtocol: TProtocol)
            : IIntfSensor;
        function AddPipPumpDLL(aSettingDP: TDataProvider; const aDllName: string; aBit: integer)
            : IIntfPipPump;
        function AddHeaterAsHuberTango(aSettingDP: TDataProvider; const aDevArea: string; aCom: integer)
            : IIntfThermostat;
        function AddHeaterAsCATVortexer(aCom, aBit: integer; const cSleepTime: integer): IIntfThermostat;
        function AddHeaterAsCANBusDevice(aAdr, aWritePort, aReadPort: integer): IIntfThermostat;
    public
        // constructor
        constructor Create(aSettingDP: TDataProvider);
        // public methods
        function AddBalance(aSettingDP: TDataProvider; aProtocol: TProtocol;
            var aModuleIndex, aComPort: integer; aName, aType: string; aDLLName: string): IIntfBalance;
        function AddSwitch(aAdr, aCom: integer; const aBit, aDllName: string; aProtocol: TProtocol)
            : IIntfSwitch;
        function AddShaker(aCom, aBit: integer; aProtocol: TProtocol; const cSleepTime: Integer)
            : IIntfVortexer;
        function AddHeater(aSettingDP: TDataProvider; const aDevArea: string; aDevData: TThermoDeviceDataRec)
            : IIntfThermostat;
        function AddPipPump(aSettingDP: TDataProvider; const aName: string; aDevType: TDeviceType;
            const aDLLName: string; aAdr, aBit: integer; aAspSpeed, aAspRamp, aDispSpeed, aDispRamp: integer;
            aValvePos: TVPosArray; aMaxVol_uL, aMaxVolSteps, aScalingFactor: integer; aExecID: integer)
            : IIntfPipPump;
        function AddMotor(const aName: string; aDevType: TDeviceType; aAdr: integer;
            aSpeed, aRamp, aInitSpeed: MPos; aProtocol: TProtocol): IIntfMotor;
        function GetDLLBCReaderIntf(aSettingDP: TDataProvider; const aDllName: string): IIntfBCReader;
        function GetZP01BCReaderIntf(aAdr: MADR; aBcType: word; aInitStr: string; aGetBCDelay: integer)
            : IIntfBCReader;
        function AddSensor(aAdr, aCom: integer; const aBit, aDllName: string; aProtocol: TProtocol)
            : IIntfSensor;
        function AddEntireGripper(aDllName: string): IIntfLocationBasedMotion;
        function AddBCTurntable(aName: string; aMotor: MOTOR; aBit: integer): IIntfBCTurntable;
        function AddWatchDog(aDllName: string; aPort: integer; aTriggerInterval: integer): IIntfWatchDog;
        // properties
        property RoboticInterface: TRoboticInterface read GetRoboticInterface;
        property CVortexSleepTime: integer read fCVortexSleepTime;
        property OpenFixationAtInit: boolean read fOpenFixationAtInit;
    end;

    TRoboticInterfaceZP01 = class(TRoboticInterface, IIntfMotor, IIntfPipPump, IIntfBCReader, IIntfSensor,
        IIntfSwitch, IIntfBCTurntable)
    public const
        cZP01Connection = 'ZP01Connection';

        constructor Create(var aSimulation: boolean);

        procedure PipPump_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aExecID, aAdr, aMaxVol_uL, aMaxVolSteps, aASpeed, aARamp, aDSpeed, aDRamp,
            aScalingFactor: integer; aValvePos: TVPosArray);
        procedure Motor_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aExecID, aAdr, aSpeed, aRamp, aInitSpeed: integer);
        procedure Switch_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string; aAdr: integer;
            const aBit, aDefaultBit, aDelayedBit: string; aReverse: boolean;
            aSwitchOffDelay, aSwitchOnDelay: integer; aNeverResetAtInit, aNeverResetAtReset: boolean);
        procedure BCReader_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aAdr: integer; const aInitStr: string; aBCType: integer; aGetBCDelay: integer);
        procedure Sensor_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string; aAdr: integer;
            const aBit: string; aDefaultValue: integer);
    end;

    TRoboticInterfaceZP02 = class(TRoboticInterface, IIntfMotor, IIntfPipPump, IIntfBCTurntable,
        IIntfThermostat)
    private
        fConnectionName: string;
        fConnectionWritten: boolean;
        fExecIDList: TList<integer>;
        procedure WriteConnection(aSettingDP: TDataProvider; aExecID: integer; const aDriverName: string;
            aAdr: integer);
    public
        constructor Create(var aSimulation: boolean);
        destructor Destroy; override;

        procedure PipPump_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aExecID, aAdr, aMaxVol_uL, aMaxVolSteps, aASpeed, aARamp, aDSpeed, aDRamp,
            aScalingFactor: integer; aValvePos: TVPosArray);
        procedure Motor_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
            aExecID, aAdr, aSpeed, aRamp, aInitSpeed: integer);
        procedure Thermostat_WriteDriverData(aSettingDP: TDataProvider; const aDeviceName: string;
            aData: TThermoDeviceDataRec);
        procedure Switch_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string; aAdr: integer;
            const aBit, aDefaultBit, aDelayedBit: string; aReverse: boolean;
            aSwitchOffDelay, aSwitchOnDelay: integer; aNeverResetAtInit, aNeverResetAtReset: boolean);
    end;

var
    gCommManager: TCommunicationRealManager;

function CalcWorldPosFromPos(aSettingDP: TDataProvider; aPos: MPos; aMotorType: TArmMotorType): MPos;


implementation


uses
    Math,
    SysUtils,
    Graphics,
    UpdateManagerCommonTypes,
    SettingsTableUpdate;

const
    MODULENAME_LENGTH = 25;

    STR_LEAFDEVICE_VACUUM = 'VACUUM';
    STR_LEAFDEVICE_SWITCH = 'SWITCH';
    STR_LEAFDEVICE_BLOWER = 'BLOWER';
    STR_LEAFDEVICE_PUMP = 'PUMP';

    STR_LEAFDEVICE_MOTOR = 'MOTOR';
    STR_LEAFDEVICE_VARREDIMOTOR = 'VARREDIMOTOR';
    STR_LEAFDEVICE_XMOTOR = 'XMOTOR';
    STR_LEAFDEVICE_YMOTOR = 'YMOTOR';
    STR_LEAFDEVICE_ZMOTOR = 'ZMOTOR';
    STR_LEAFDEVICE_RMOTOR = 'RMOTOR';
    STR_LEAFDEVICE_GMOTOR = 'GMOTOR';

    STR_LEAFDEVICE_PNEUGRIPOPENCLOSESWITCH = 'PNEUGRIPOPENCLOSESWITCH';
    STR_LEAFDEVICE_PNEUGRIPDISTSWITCH = 'PNEUGRIPDISTSWITCH';

    STR_LEAFDEVICE_ISHAK = 'ISHAK';
    STR_LEAFDEVICE_ASHAK = 'ASHAK';
    STR_LEAFDEVICE_THERMOSTAT = 'THERMOSTAT';
    STR_LEAFDEVICE_BCREADER = 'BCREADER';
    STR_LEAFDEVICE_BALANCE = 'BALANCE';
    STR_LEAFDEVICE_PIPPUMP = 'PIPPUMP';
    STR_LEAFDEVICE_PIPMOTOR = 'PIPMOTOR';
    STR_LEAFDEVICE_GRPMOTOR = 'GRPMOTOR';
    STR_LEAFDEVICE_GRIPPER = 'GRIPPER';

    STR_LEAFDEVICE_SENSOR = 'SENSOR';
    STR_LEAFDEVICE_SHAKER = 'SHAKER';
    STR_LEAFDEVICE_XWAYVALVE = 'XWAYVALVE';
    STR_LEAFDEVICE_2IN1VALVE = '2IN1VALVE';

    STR_LEAFDEVICE_STATESWITCH = 'STATESWITCH';
    STR_LEAFDEVICE_SIMPLEWATCHDOG = 'SIMPLEWATCHDOG';

    // composed:
    STR_COMPDEVICE_MACHINE = 'MACHINE';
    STR_COMPDEVICE_DECAPPER = 'DECAPPER';
    STR_COMPDEVICE_GRIPPERARM = 'GRIPPERARM';
    STR_COMPDEVICE_ARM = 'ARM';
    STR_COMPDEVICE_WEIGH = 'WEIGH';
    STR_COMPDEVICE_USERPROTECT = 'USERPROTECT';
    STR_COMPDEVICE_MOTORMOTION = 'MOTORMOTION';
    STR_COMPDEVICE_LOCATIONMOTION = 'LOCATIONMOTION';
    STR_COMPDEVICE_YMOTORSINDIV = 'YMOTORSINDIV';
    STR_COMPDEVICE_ZMOTORSINDIV = 'ZMOTORSINDIV';
    STR_COMPDEVICE_PIP = 'PIP';
    STR_COMPDEVICE_REMOVABLEPIP = 'REMOVABLEPIP';
    STR_COMPDEVICE_MOTORGRIP = 'MOTORGRIP';
    STR_COMPDEVICE_PNEUMATICGRIP = 'PNEUMATICGRIP';
    STR_COMPDEVICE_STATESIGNAL = 'STATESIGNAL';
    STR_COMPDEVICE_REDI = 'REDI';
    STR_COMPDEVICE_SOFTWAREPROTECT = 'SOFTWAREPROTECT';

    // alt:
    STR_LEAFDEVICE_OLD_LINEIN = 'LINEIN';
    STR_LEAFDEVICE_OLD_HEAT = 'HEAT';
    STR_LEAFDEVICE_OLD_COOL = 'COOL';

    // noch nicht benutzt:
    STR_LEAFDEVICE_SAMSHAKER = 'SAMSHAKER';
    STR_LEAFDEVICE_PHOTOMETER = 'PHOTOMETER';
    STR_LEAFDEVICE_TURNTABLE = 'TURNTABLE';
    STR_LEAFDEVICE_PERIPUMP = 'PERIPUMP';
    STR_LEAFDEVICE_WASHER = 'WASHER';
    STR_COMPDEVICE_BCREADSYSTEM = 'BCREADSYSTEM';
    STR_COMPDEVICE_COMPOSED = 'COMPOSED';
    STR_COMPDEVICE_PIPARM = 'PIPARM';
    STR_COMPDEVICE_OVERVIEW = 'OVERVIEW';

    // Protocols:
    STR_DEV_PROTOCOL_COMPULAB = 'COMPULAB';
    STR_DEV_PROTOCOL_KOLTER = 'KOLTER';
    STR_DEV_PROTOCOL_CONRAD = 'CONRAD';
    STR_DEV_PROTOCOL_SIGNALLOOP = 'SIGNALLOOP';
    STR_DEV_PROTOCOL_ROSYS = 'SAMPLER';
    STR_DEV_PROTOCOL_OMNIFIT = 'OMNIFIT';
    STR_DEV_PROTOCOL_HUBER = 'HUBER';
    STR_DEV_PROTOCOL_PEERLESS = 'PEERLESS';
    STR_DEV_PROTOCOL_CAT = 'CAT';
    STR_DEV_PROTOCOL_IKA = 'IKA';
    STR_DEV_PROTOCOL_DLL = 'DLL';
    STR_DEV_PROTOCOL_CONRAD_V = 'CONRADV';
    STR_DEV_PROTOCOL_DLL_BCR = 'DLLBCR';
    STR_DEV_PROTOCOL_METTLER_AT = 'METTLERAT';
    STR_DEV_PROTOCOL_METTLER_AX = 'METTLERAX';
    STR_DEV_PROTOCOL_GENIUS = 'GENIUS';
    STR_DEV_PROTOCOL_GENIUS_BIN = 'GENIUSBIN';
    STR_DEV_PROTOCOL_GENIUSCELL_BIN = 'GENIUSCELLBIN';
    STR_DEV_PROTOCOL_DLL_GRIPPER = 'DLLGRIPPER';
    STR_DEV_PROTOCOL_CANBUS = 'CANBUS';

    // Ini section in DEVICE ini settings
    STR_DEV_INI_IDENT_TYPE = 'Type';
    STR_DEV_INI_IDENT_CARRIERNAME = 'CarrierName';
    STR_DEV_INI_IDENT_RACKNAME = 'RackName';
    STR_DEV_INI_IDENT_ROW = 'Row';
    STR_DEV_INI_IDENT_COLUMN = 'Column';
    STR_DEV_INI_IDENT_ADR = 'Adr';
    STR_DEV_INI_IDENT_COMPORT = 'COMMPORT';
    STR_DEV_INI_IDENT_BIT = 'Port';
    STR_DEV_INI_IDENT_DEFAULTPORT = 'DefaultPort';
    STR_DEV_INI_IDENT_PROTOCOL = 'Protocol';
    STR_DEV_INI_IDENT_DELAY = 'Delay';
    STR_DEV_INI_IDENT_DLLNAME = 'DllName';
    STR_DEV_INI_IDENT_DEFAULTVALUE = 'DefaultValue'; // für Sensoren

    STR_SETTINGS_AREA_BALANCE = 'BALANCE'; // balance.ini
    STR_COM_NAME = 'COM';
    STR_WEIGHING_DOORSWITCH_IDENTIFIER = 'DOOR';

    STR_STATENAME_UNKNOWN = 'UNKNOWN';
    STR_STATENAME_ACTIVE = 'ACTIVE';
    STR_STATENAME_SETUP = 'SETUP';
    STR_STATENAME_ERROR = 'ERROR';

function DepractedWorldOffsetAllowed(aOffset: MPos; aMotorType: TArmMotorType): boolean;
begin
    // -1 : Default setting.  User has not entered a custom offset
    result := (aOffset <> -1) and (aOffset <> 0);
    if aMotorType = amtYMotor then
        result := result and (TAppSettings.IsSias);
end;

function CalcDepracatedHZStepsFromZSteps(aZSteps: MPos; aHZSTEPS_PER_ZSTEPS: Extended): MPos;
const
    INT_CORRECTED_LOGIC = -1; // the Z-Offset was always entered as a positive value although
    // in relative length it was shorter than the standard tip length
begin
    result := Round(aZSteps * aHZSTEPS_PER_ZSTEPS) * INT_CORRECTED_LOGIC;
end;

function CalcWorldPosFromPos(aSettingDP: TDataProvider; aPos: MPos; aMotorType: TArmMotorType): MPos;
var
    xIniAccess: ISimpleIniAccess;
    xZSTEPS_PER_MM, // darf nur noch an Stellen benutzt werden, wo der Hardcore-Modus keinen Einfluß hatte
    xHZSTEPS_PER_MM: Extended;
    xHZSTEPS_PER_ZSTEPS: Extended;
    // some of the settings are defined in Z_Steps but are needed as HZ_Steps : multiply by this factor to get HZSteps
    xDist_H_Tip1: MPos;
    xIdent: string;
begin
    result := aPos;
    xIdent := '';
    case aMotorType of
        amtXMotor:
            xIdent := 'DistX_H_Tip1';
        amtYMotor:
            xIdent := 'DistY_H_Tip1';
        amtZMotor:
            xIdent := 'DistZ_H_Tip1';
        else
            ASSERT(false, 'Motor type not supported');
    end;
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, 'ROBOT');
    xZSTEPS_PER_MM := xIniAccess.ReadFloat('XYZStepsPerMillimeter', 'ZSTEPperMM', -1);
    xHZSTEPS_PER_MM := xIniAccess.ReadFloat('XYZStepsPerMillimeter', 'HZSTEPperMM', -1);
    xHZSTEPS_PER_ZSTEPS := xHZSTEPS_PER_MM / xZSTEPS_PER_MM;

    xDist_H_Tip1 := xIniAccess.ReadInteger('Handler_ReferenceTip_Distances_Steps', xIdent, -1);
    if aMotorType = amtZMotor then
        xDist_H_Tip1 := CalcDepracatedHZStepsFromZSteps(xDist_H_Tip1, xHZSTEPS_PER_ZSTEPS);

    if not DepractedWorldOffsetAllowed(xDist_H_Tip1, aMotorType) then
        EXIT;

    if (aMotorType = amtXMotor) and TAppSettings.IsSias then
    begin
        result := -result; // There was always a reversed X motor for ZP02 Gripper Arm
    end;

    result := result + xDist_H_Tip1;

end;

// ------------------------------------------------------------------------------
function gmStrToIntTry(var IntResult: integer; Text: string): boolean;
// ------------------------------------------------------------------------------
var
    d: integer;
begin
    val(Text, IntResult, d);
    if (d = 0) then
        result := true
    else
        result := false;
end;

const
    STR_SYSTEMLIQUID_PREFIX = 'SYS';

class function TLiquids.DecodeSystemLiquidIdent(aIdent: string): integer;
var
    xIndex, xErr: integer;
begin
    result := -1;
    if (Pos(STR_SYSTEMLIQUID_PREFIX, aIdent) <> 1) then
        EXIT;
    Val(Copy(aIdent, 4, 2), xIndex, xErr);
    result := xIndex - 1;
end;

class function TLiquids.EncodeSystemLiquidIdent(aIndex: integer): string;
begin
    result := Format('%s%.2d', [STR_SYSTEMLIQUID_PREFIX, (aIndex + 1)]);
end;

{ TCompositeDevice }

function TCompositeDevice.FindCompositeDevice(aName: string): TCompositeDevice;
var
    x: integer;
begin
    result := nil;

    for x := 0 to FDeviceList.Count - 1 do
    begin
        if (FDeviceList.Items[x] is TCompositeDevice) and (FDeviceList.Items[x].Name = aName) then
        begin
            result := (FDeviceList.Items[x] as TCompositeDevice);
            EXIT;
        end;
    end;
end;

procedure TCompositeDevice.Find_ByClass(var aDevice: TDevice; aClass: TDeviceClass);
var
    x: integer;
begin
    inherited Find_ByClass(aDevice, aClass);

    // do this for all sub devices
    for x := 0 to FDeviceList.Count - 1 do
        FDeviceList.Items[x].Find_ByClass(aDevice, aClass);
end;

procedure TCompositeDevice.Find_ByNameCut(var aDevice: TDevice; aSubString: string; aClass: TDeviceClass);
var
    x: integer;
begin
    inherited Find_ByNameCut(aDevice, aSubString, aClass);

    // do this for all sub devices
    for x := 0 to FDeviceList.Count - 1 do
        FDeviceList.Items[x].Find_ByNameCut(aDevice, aSubString, aClass);
end;

function TCompositeDevice.Find_SwitchByNameCut(aSubString: string): TSwitchDevice;
var
    xDevice: TDevice;
begin
    xDevice := nil;
    self.Find_ByNameCut(xDevice, aSubString, TSwitchDevice);

    result := nil;
    if (xDevice is TSwitchDevice) then
        result := xDevice as TSwitchDevice;
end;

function TCompositeDevice.AddCompositeDevice(aSettingDP: TDataProvider; const aDevArea: string;
    const aSection, aName: string; aRediDeviceNames: TArray<string>): TCompositeDevice;
var
    xIniFile: ISimpleIniAccess;
    xTypeName: string;
    x: integer;
begin
    result := nil;
    xIniFile := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    if not xIniFile.SectionExists(aSection) then
        EXIT;

    result := FindCompositeDevice(aName);
    if Assigned(result) then
        EXIT;

    xTypeName := xIniFile.Readstring(aSection, 'Type', '');

    // Decapper device
    if (Uppercase(xTypeName) = STR_COMPDEVICE_DECAPPER) then
    begin
        result := TDecapperDevice.Create(aSettingDP, aDevArea, aName, aSection);
        FDeviceList.Add(result);
        EXIT;
    end
    // Weighing device
    else if (Uppercase(xTypeName) = STR_COMPDEVICE_WEIGH) then
    begin
        result := TWeighingDevice.Create(aName);
        FDeviceList.Add(result);
        EXIT;
    end
    // Gripper Arm Device
    else if (Uppercase(xTypeName) = STR_COMPDEVICE_GRIPPERARM) then
    begin
        result := TGripperArmDevice.Create(aSettingDP, aName);
        FDeviceList.Add(result);
        EXIT;
    end
    // User Protection device
    else if (Uppercase(xTypeName) = STR_COMPDEVICE_USERPROTECT) then
    begin
        result := TUserProtectionDevice.Create(aSettingDP, aDevArea, aName, aSection);
        FDeviceList.Add(result);
        EXIT;
    end
    // Gripper Arm Device
    else if (Uppercase(xTypeName) = STR_COMPDEVICE_ARM) then
    begin
        result := TRobotArmDevice.Create(aSettingDP, aDevArea, aName, aSection, dptCmpArm);
        FDeviceList.Add(result);
        EXIT;
    end
    // Weighing device
    else if (Uppercase(xTypeName) = STR_COMPDEVICE_YMOTORSINDIV) then
    begin
        result := TIndividualMultiYMotorDevice.Create(aSettingDP, aDevArea, aName, aSection);
        FDeviceList.Add(result);
        EXIT;
    end
    else if (Uppercase(xTypeName) = STR_COMPDEVICE_ZMOTORSINDIV) then
    begin
        result := TMultiZMotorDevice.Create(aSettingDP, aDevArea, aName, aSection);
        FDeviceList.Add(result);
        EXIT;
    end
    else if (Uppercase(xTypeName) = STR_COMPDEVICE_MOTORMOTION) then
    begin
        result := TMotorBasedMotionDevice.Create(aName);
        FDeviceList.Add(result);
        EXIT;
    end
    else if (Uppercase(xTypeName) = STR_COMPDEVICE_PIP) then
    begin
        result := TAttachedPipDevice.Create(aSettingDP, aDevArea, aName, aSection);
        FDeviceList.Add(result);
        EXIT;
    end
    else if (Uppercase(xTypeName) = STR_COMPDEVICE_REMOVABLEPIP) then
    begin
        result := TRemovablePipDevice.Create(aSettingDP, aDevArea, aName, aSection);
        FDeviceList.Add(result);
        EXIT;
    end
    else if (Uppercase(xTypeName) = STR_COMPDEVICE_MOTORGRIP) then
    begin
        result := TMotorGripDevice.Create(aName, aSection);
        FDeviceList.Add(result);
        EXIT;
    end
    else if (Uppercase(xTypeName) = STR_COMPDEVICE_PNEUMATICGRIP) then
    begin
        result := TPneumaticGripDevice.Create(aSettingDP, aDevArea, aName, aSection);
        FDeviceList.Add(result);
        EXIT;
    end
    else if (Uppercase(xTypeName) = STR_COMPDEVICE_STATESIGNAL) then
    begin
        result := TStateSignalDevice.Create(aName);
        FDeviceList.Add(result);
        EXIT;
    end
    else if (Uppercase(xTypeName) = STR_COMPDEVICE_REDI) then
    begin
        result := TRediDevice.Create(aName, aSection);
        FDeviceList.Add(result);
        EXIT;
    end
    else if (Uppercase(xTypeName) = STR_COMPDEVICE_SOFTWAREPROTECT) then
    begin
        result := TSoftwareProtectionDevice.Create(aName, aSection);
        FDeviceList.Add(result);
        EXIT;
    end;

    // REDI-Device muss nicht als solches gekennzeichnet sein:
    for x := 0 to high(aRediDeviceNames) do
    begin
        if (Pos(aRediDeviceNames[x], aName) = 1) and
            (StrToIntDef(Copy(aName, Length(aRediDeviceNames[x]) + 1,
            Length(aName) - Length(aRediDeviceNames[x])), -1) <> -1) then
        begin
            result := TRediDevice.Create(aName, aSection);
            FDeviceList.Add(result);
            EXIT;
        end;
    end;
end;

constructor TCompositeDevice.Create(aName: string; aDType: TDeviceType);
begin
    inherited Create(aName, aDType);

    FDeviceList := TObjectList<TDevice>.Create(true);
end;

destructor TCompositeDevice.Destroy;
begin
    try
        FDeviceList.Free;
    except
        // Kann passieren, wenn Devices doppelt verwendet werden
    end;
    inherited Destroy;
end;

function TCompositeDevice.ReadAndAddComposedDevice(aSettingDP: TDataProvider; const aDevArea: string;
    const aDeviceName: string; aLevel: integer; aRediDeviceNames: TArray<string>): boolean;
var
    xDPNo: integer;
    NPos, xCol, xRow: integer;
    xSection, xDPName, xDevTypeString, xCarrierName, xRackName: string;
    iDevPart: TLeafDevice;
    xCompositeDevice: TCompositeDevice;
    xIniAccess: ISimpleIniAccess;
begin
    result := false;

    xDPNo := 0;

    // testweise für Decapper
    if (aDeviceName = '') then
        EXIT;

    // testweise für Decapper: Type für composite device
    xSection := 'DEVICE_' + aDeviceName;
    xCompositeDevice := AddCompositeDevice(aSettingDP, aDevArea, xSection, aDeviceName, aRediDeviceNames);

    if not Assigned(xCompositeDevice) then
    begin
        // default - set to self for MACHINE, Sophas Heater and Cooler and REDI
        if aLevel = INT_FIRST_DEVICE_LEVEL then
            xCompositeDevice := self;
    end;

    if not Assigned(xCompositeDevice) then
        EXIT;

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    repeat
        inc(xDPNo);
        xDPName := xIniAccess.Readstring(xSection, 'DEVICEPART' + inttostr(xDPNo), '');

        if xCompositeDevice.ReadAndAddComposedDevice(aSettingDP, aDevArea, xDPName, aLevel + 1,
            aRediDeviceNames) then
            CONTINUE;
        ;

        // Typ-Bestimmung
        xDevTypeString := xIniAccess.ReadString(xDPName, STR_DEV_INI_IDENT_TYPE, '');
        // neu: Angabe des Typs in 'Type'
        if (xDevTypeString = '') then
            xDevTypeString := xDPName; // alt: Typ aus dem Namen lesen
        if (aDeviceName = 'MACHINE') then
            xDevTypeString := 'SWITCH'; // SOPHAS-Altlast

        // Bestimmung von Rackname, Row & Column
        xCarrierName := xIniAccess.Readstring(xSection, STR_DEV_INI_IDENT_CARRIERNAME, '');
        xRackName := xIniAccess.Readstring(xSection, STR_DEV_INI_IDENT_RACKNAME, '');
        if (xRackName = '') then
            xRackName := xIniAccess.Readstring(xDPName, STR_DEV_INI_IDENT_RACKNAME, '');
        xRow := xIniAccess.ReadInteger(xDPName, STR_DEV_INI_IDENT_ROW, 0);
        xCol := xIniAccess.ReadInteger(xDPName, STR_DEV_INI_IDENT_COLUMN, 0);

        // Relikt aus V 4.3 für Oxford Assymetry
        if Pos('SH_', UpperCase(aDeviceName)) = 1 then
        begin
            xDevTypeString := 'SHAKER';
            xRackName := Copy(aDeviceName, 4, SIZE_RACK_NAME);
            NPos := Pos('COL', UpperCase(xDPName));
            if NPos > 0 then
                gmStrToIntTry(xCol, Copy(xDPName, NPos + 3, 1));
            NPos := Pos('ROW', UpperCase(xDPName));
            if NPos > 0 then
                gmStrToIntTry(xRow, Copy(xDPName, NPos + 3, 1));
        end;

        // Add leaf device to list!
        iDevPart := xCompositeDevice.ReadAndAddLeafDevice(aSettingDP, aDevArea, xDPName, aDeviceName,
            xDevTypeString, xCarrierName, xRackName, xRow, xCol);

        if (iDevPart <> nil) then
        begin
            // ------------------------------------------------------------- Daten laden aus Devicepart
            iDevPart.Delay := xIniAccess.ReadInteger(xDPName, STR_DEV_INI_IDENT_DELAY, 0);
        end;

    until (xDPName = '');

    result := true;
end;

function TCompositeDevice.AddSwitchDevice(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
    aDevType: TDeviceType; aAdr, aCom: integer; aBit, aDefaultBit: string; const aDllName: string)
    : TLeafDevice;
begin
    result := nil;
    if (aAdr > -1) then
    begin
        result := TSwitchDevice.Create(aSettingDP, aDevArea, aName, aAreaName, aDevType, aAdr, -1, aBit,
            aDefaultBit, aDllName);
    end
    else if (aCom > -1) then
    begin
        result := TSwitchDevice.Create(aSettingDP, aDevArea, aName, aAreaName, aDevType, -1, aCom, aBit,
            aDefaultBit, aDllName);
    end;
end;

function TCompositeDevice.ReadAndAddLeafDevice(aSettingDP: TDataProvider;
    const aDevArea, aName, aAreaName, aTypeName, aCarrierName, aRackName: string; aRow, aColumn: integer)
    : TLeafDevice;
var
    xIniAccess: ISimpleIniAccess;
    xAdr, xCom, xTipIndex: integer;
    xBit, xDefaultBit, xDllName: string;
begin
    result := nil;
    if (aName = '') then
        EXIT;

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    try

        xTipIndex := xIniAccess.ReadInteger(aName, 'TipNo', 0);
        xAdr := xIniAccess.ReadInteger(aName, STR_DEV_INI_IDENT_ADR, -1);
        xCom := xIniAccess.ReadInteger(aName, STR_DEV_INI_IDENT_COMPORT, -1);
        xBit := xIniAccess.ReadString(aName, STR_DEV_INI_IDENT_BIT, STR_DEV_INI_DEFAULT_BIT_UNDEFINED);
        xDefaultBit := xIniAccess.ReadString(aName, STR_DEV_INI_IDENT_DEFAULTPORT,
            STR_DEV_INI_DEFAULT_BIT_UNDEFINED);
        xDllName := xIniAccess.ReadString(aName, STR_DEV_INI_IDENT_DLLNAME,
            STR_DEV_INI_DEFAULT_DLLNAME_UNDEFINED);

        // Add leaf device to list!
        result := AddLeafDevice(aSettingDP, aDevArea, aName, aAreaName, aTypeName, aCarrierName, aRackName,
            xAdr, xCom, xBit, xDefaultBit, aRow, aColumn, xTipIndex, amtXMotor, xDllName);
    except
        // ShowMessage('Startup error creating device ' + aName );
    end;
end;

procedure TCompositeDevice.WriteDeviceData(aSettingDP: TDataProvider);
var
    x: integer;
begin
    for x := 0 to fDeviceList.Count - 1 do
    begin
        try
            fDeviceList[x].WriteDeviceData(aSettingDP);
        except
            TLogManager.Instance.Log('WARNING: Device ' + fDeviceList[x].name +
                ', Conversion failed (maybe this device is used twice)', 1);

            // Dieser Fehler passiert beim POST
            // Ein Reset der Tabelle ist erforderlich
            aSettingDP.Close;
            aSettingDP.SelectAndOpen('select * from settings', false);
        end;
    end;
end;

function TCompositeDevice.AddLeafDevice(aSettingDP: TDataProvider;
    const aDevArea, aName, aAreaName, aTypeName, aCarrierName, aRackName: string; aAdr, aCom: integer;
    aBit, aDefaultBit: string; aRow, aColumn, aTipIndex: integer; aMType: TArmMotorType;
    const aDllName: string): TLeafDevice;
var
    xDevType: TDeviceType;
    xIniAccess: ISimpleIniAccess;
    xDllName: string;
    xIntBit: integer;
begin
    result := nil;
    if (aName <> '') then
    begin

        // Get DevicePartType
        xDevType := GetTypeFromTypeName(aTypeName, aAdr, aCom, aBit);

        // Get Bit integer value from aBit
        xIntBit := StrToIntDef(aBit, -1);

        // Create leaf device
        case (xDevType) of
            // BCReader
            dptBCReader:
                result := TBCReaderDevice.Create(aSettingDP, aDevArea, aName, aAreaName);

            // Shaker aller Art
            dptShakerIKA:
                result := TSpeedShakerDevice.Create(aSettingDP, aDevArea, aName, aAreaName, xDevType,
                    aCom, xIntBit);
            dptShakerIKAOld:
                result := TSimpleShakerDevice.Create(aSettingDP, aDevArea, aName, aAreaName, xDevType, aAdr,
                    aCom, aBit, aDefaultBit, aDllName);
            dptShakerCAT:
                result := TSpeedShakerDevice.Create(aSettingDP, aDevArea, aName, aAreaName, xDevType,
                    aCom, xIntBit);

            // Thermostat
            dptThermostat:
                result := TThermoDevice.Create(aSettingDP, aDevArea, aName, aAreaName, aTypeName, aAdr,
                    aCom, xIntBit);
            // Sensor
            dptSensor:
                result := TSensorDevice.Create(aSettingDP, aDevArea, aName, aAreaName, aCom, aAdr, aBit,
                    aDllName);
            // Balance
            dptBalance:
                result := TBalanceDeviceFactory.CreateBalanceDevice(aSettingDP, aDevArea, aName);
            // PipPump
            dptPipPump:
                result := TPipPumpDeviceFactory.CreatePipPumpDevice(aSettingDP, aDevArea, aName, aAreaName,
                    aDllName, aAdr, xIntBit, aTipIndex - 1);
            // Motors:
            dptXMotor:
                result := TXMotorDevice.Create(aSettingDP, aDevArea, aName, aAreaName, aAdr, xDevType);

            dptYMotor:
                result := TYMotorDevice.Create(aSettingDP, aDevArea, aName, aAreaName, aAdr, xDevType);

            dptZMotor:
                result := TZMotorDevice.Create(aSettingDP, aDevArea, aName, aAreaName, aAdr, xDevType);

            dptGMotor:
                result := TGMotorDevice.Create(aSettingDP, aDevArea, aName, aAreaName, aAdr, xDevType);

            dptRMotor:
                result := TRMotorDevice.Create(aSettingDP, aDevArea, aName, aAreaName, aAdr, xDevType);

            // Thermostat
            dptXWayValve:
                if TDevice.BitValueIsDefined(aBit) then
                    result := TXWayValveSwitchDevice.Create(aSettingDP, aDevArea, aName, aAreaName, aAdr,
                        aCom, aBit, aDllName)
                else
                    result := T6WayValveDevice.Create(aSettingDP, aDevArea, aName, aAreaName, aCom);

            // EntireGripper (z.B. CRS):
            dptEntireGripper:
                begin
                    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
                    xDllName := xIniAccess.ReadString(aName, 'DLLName', '');
                end;

            dptPneumaticGripOpenCloseSwitch:
                result := TPneumaticGripOpenCloseSwitchDevice.Create(aSettingDP, aDevArea, aName, aAreaName,
                    xDevType, aAdr, aCom, aBit, aDefaultBit, aDLLName);

            dptPneumaticGripDistanceSwitch:
                result := TPneumaticGripDistanceSwitchDevice.Create(aSettingDP, aDevArea, aName, aAreaName,
                    xDevType, aAdr, aCom, aBit, aDefaultBit, aDLLName);

            dptStateSwitch:
                result := TStateSwitchDevice.Create(aSettingDP, aDevArea, aName, aAreaName, xDevType, aAdr,
                    aCom, aBit, aDefaultBit, aDLLName);

            dptVarRediMotor:
                result := TVarRediMotorDevice.Create(aName, aAreaName, aCom);

            dptSimpleWatchDog:
                result := TSimpleWatchDogDevice.Create(aSettingDP, aDevArea, aName, aAreaName,
                    dptSimpleWatchDog);
            // other Types
            else
                begin
                    result := AddSwitchDevice(aSettingDP, aDevArea, aName, aAreaName, xDevType, aAdr, aCom,
                        aBit, aDefaultBit, aDLLName);
                end;
        end;
    end;
    // Add device to list
    if (result <> nil) then
    begin
        result.CarrierName := aCarrierName;
        result.RackName := aRackName;
        result.Row := aRow;
        result.Col := aColumn;
        FDeviceList.Add(result);
    end;
end;

{ TDecapperDevice }

constructor TDecapperDevice.Create(aSettingDP: TDataProvider; const aArea, aName, aIdentName: string);
begin
    inherited Create(aName, dptCmpDecapper);

    FDevLoaded := false;
    FUseCapSensor := true;
    FUseTubeSensor := true;

    fCapTime := TIniSettings.ReadInteger(aSettingDP, aArea, aIdentName, 'CapTime', 0);
    fDecapTime := TIniSettings.ReadInteger(aSettingDP, aArea, aIdentName, 'DecapTime', 0);
    fTubeReleaseDelay := TIniSettings.ReadInteger(aSettingDP, aArea, aIdentName, 'TubeReleaseDelay', 50);
    fNoOfTurns := TIniSettings.ReadInteger(aSettingDP, aArea, aIdentName, 'NoOfTurns', 3);
    fTubeDropBeforeIntake := TIniSettings.ReadBool(aSettingDP, aArea, aIdentName, 'DropBeforeIntake', false);
    fCapDropBeforeIntake := TIniSettings.ReadBool(aSettingDP, aArea, aIdentName,
        'CapDropBeforeIntake', false);
    fState1Rotation := TIniSettings.ReadInteger(aSettingDP, aArea, aIdentName, 'State1Rotation', 0);
    fState2Rotation := TIniSettings.ReadInteger(aSettingDP, aArea, aIdentName, 'State2Rotation', 0);
    fSwiwelShortSwitch := TIniSettings.ReadBool(aSettingDP, aArea, aIdentName, 'SwiwelShortSwitch', false);
    fRepeatDecapping := TIniSettings.ReadInteger(aSettingDP, aArea, aIdentName, 'RepeatDecapping', 0);
    fSwiwelUpBeforeIntake := TIniSettings.ReadBool(aSettingDP, aArea, aIdentName,
        'SwiwelUpBeforeIntake', false);
    fSwiwelDownBeforeTakeCap := TIniSettings.ReadBool(aSettingDP, aArea, aIdentName,
        'SwiwelDownBeforeTakeCap', false);
    fSwiwelSwitchBoth := TIniSettings.ReadBool(aSettingDP, aArea, aIdentName, 'SwiwelSwitchBoth', false);
    fSkipCapCheckAtInit := TIniSettings.ReadBool(aSettingDP, aArea, aIdentName, 'SkipCapCheckAtInit', false);
    fTurnLeftBeforeCapTime := TIniSettings.ReadInteger(aSettingDP, aArea, aIdentName,
        'TurnLeftBeforeCapTime', 0);
    fCapWasteBelowCapLifter := TIniSettings.ReadBool(aSettingDP, aArea, aIdentName,
        'CapWasteBelowCapLifter', false);
    fNoCapliftWithoutCap := TIniSettings.ReadBool(aSettingDP, aArea, aIdentName,
        'NoCapliftWithoutCap', false);
    fCappingTurntableFirst := TIniSettings.ReadBool(aSettingDP, aArea, aIdentName,
        'CappingTurntableFirst', false);
end;

procedure TDecapperDevice.LoadDevices();
const
    STR_DECAP_SWIWELLIFTER = 'SWIWELLIFTER';
    STR_DECAP_CAPLIFTER = 'CAPLIFTER';
    STR_DECAP_CAPGRIPPER = 'CAPGRIPPER';
    STR_DECAP_TUBEHOLDER = 'TUBEHOLDER';
    STR_DECAP_TURNTABLE = 'TURNTABLE';
    STR_DECAP_TUBESENSOR = 'TUBESENSOR';
    STR_DECAP_CAPSENSOR = 'CAPSENSOR';
    STR_DECAP_PREPARE = 'PREPARE';
var
    xDevice: TDevice;
begin
    // Load all devices
    FCapGripper := self.Find_SwitchByNameCut(STR_DECAP_CAPGRIPPER);
    FCapLifter := self.Find_SwitchByNameCut(STR_DECAP_CAPLIFTER);
    FSwiwelLifter := self.Find_SwitchByNameCut(STR_DECAP_SWIWELLIFTER);
    FTubeHolder := self.Find_SwitchByNameCut(STR_DECAP_TUBEHOLDER);
    FTurntable := self.Find_SwitchByNameCut(STR_DECAP_TURNTABLE);
    FPrepare := self.Find_SwitchByNameCut(STR_DECAP_PREPARE);

    xDevice := nil;
    self.Find_ByClass(xDevice, TBCReaderDevice);
    if (xDevice is TBCReaderDevice) then
        FBCReader := xDevice as TBCReaderDevice;

    xDevice := nil;
    self.Find_ByNameCut(xDevice, STR_DECAP_TUBESENSOR, TSensorDevice);
    if (xDevice is TSensorDevice) then
        FTubeSensor := xDevice as TSensorDevice;

    xDevice := nil;
    self.Find_ByNameCut(xDevice, STR_DECAP_CAPSENSOR, TSensorDevice);
    if (xDevice is TSensorDevice) then
        FCapSensor := xDevice as TSensorDevice;

    FDevLoaded := true;
end;

procedure TDecapperDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    inherited;

    // sub-devices laden
    LoadDevices();

    // schreiben der Daten
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'DECAPPER');
    if Assigned(fTurntable) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Turntable', fTurntable.Name);
    if Assigned(fBCReader) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'BCReader', fBCReader.Name);
    if Assigned(fSwiwelLifter) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'SwiwelLifter',
            fSwiwelLifter.Name);
    if Assigned(fCapLifter) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'CapLifter', fCapLifter.Name);
    if Assigned(fCapGripper) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'CapGripper',
            fCapGripper.Name);
    if Assigned(fTubeHolder) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'TubeHolder',
            fTubeHolder.Name);
    if Assigned(fPrepare) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Prepare', fPrepare.Name);
    if Assigned(fCapSensor) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'CapSensor', fCapSensor.Name);
    if Assigned(fTubeSensor) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'TubeSensor',
            fTubeSensor.Name);

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'CapTime', IntToStr(fCapTime));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'DecapTime',
        IntToStr(fDecapTime));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'TubeReleaseDelay',
        IntToStr(fTubeReleaseDelay));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'NoOfTurns',
        IntToStr(fNoOfTurns));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'DropBeforeIntake',
        TAppSettings.WriteBoolToStr(fTubeDropBeforeIntake));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'CapDropBeforeIntake',
        TAppSettings.WriteBoolToStr(fCapDropBeforeIntake));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'State1Rotation',
        IntToStr(fState1Rotation));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'State2Rotation',
        IntToStr(fState2Rotation));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'SwiwelShortSwitch',
        TAppSettings.WriteBoolToStr(fSwiwelShortSwitch));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'RepeatDecapping',
        IntToStr(fRepeatDecapping));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'SwiwelUpBeforeIntake',
        TAppSettings.WriteBoolToStr(fSwiwelUpBeforeIntake));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'SwiwelDownBeforeTakeCap',
        TAppSettings.WriteBoolToStr(fSwiwelDownBeforeTakeCap));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'SwiwelSwitchBoth',
        TAppSettings.WriteBoolToStr(fSwiwelSwitchBoth));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'SkipCapCheckAtInit',
        TAppSettings.WriteBoolToStr(fSkipCapCheckAtInit));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'TurnLeftBeforeCapTime',
        IntToStr(fTurnLeftBeforeCapTime));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'CapWasteBelowCapLifter',
        TAppSettings.WriteBoolToStr(fCapWasteBelowCapLifter));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'NoCapliftWithoutCap',
        TAppSettings.WriteBoolToStr(fNoCapliftWithoutCap));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'CappingTurntableFirst',
        TAppSettings.WriteBoolToStr(fCappingTurntableFirst));
end;

{ TSwitchDevice }

constructor TSwitchDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
    aDType: TDeviceType; aAdr, aCom: integer; const aBit, aDefaultBit, aDllName: string;
    aNeverResetDefault: boolean; aProtocolDefault: TProtocol);
var
    xIniAccess: ISimpleIniAccess;
    xProtocol: TProtocol;
begin
    inherited Create(aName, aAreaName, aDType);

    fAdr := aAdr;
    fBit := aBit;
    fDefaultBit := aDefaultBit;
    if fDefaultBit = '' then
        fDefaultBit := STR_DEV_INI_DEFAULT_BIT_UNDEFINED;

    // Bestimmung von Protocol
    xProtocol := Settings_ReadProtocol(aSettingDP, aDevArea);
    if (xProtocol = cdtNone) then
        xProtocol := aProtocolDefault;

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    fNeverResetAtReset := xIniAccess.ReadBool(aName, 'NeverResetAtReset', aNeverResetDefault);
    // Default = true nur für TStateSwitchDevice
    fNeverResetAtInit := xIniAccess.ReadBool(aName, 'NeverResetAtInit', aNeverResetDefault);
    // Default = true nur für TStateSwitchDevice
    FSwitchOffDelay := xIniAccess.ReadInteger(aName, 'SwitchOffDelay', 0);
    FSwitchOnDelay := xIniAccess.ReadInteger(aName, 'SwitchOnDelay', 0);
    fReverse := (xIniAccess.ReadInteger(aName, 'On', 1) = 0) or (xIniAccess.ReadInteger(aName, 'Off', 0) = 1);

    // (Main) Port
    FIntf := gCommManager.AddSwitch(aAdr, aCom, aBit, aDllName, xProtocol);

    // Default Port
    if TDevice.BitValueIsDefined(fDefaultBit) then
        gCommManager.AddSwitch(aAdr, aCom, fDefaultBit, aDllName, xProtocol);

    // Delayed Port
    fDelayedBit := xIniAccess.ReadString(aName, 'DelayedPort', STR_DEV_INI_DEFAULT_BIT_UNDEFINED);
    if fDelayedBit = '' then
        fDelayedBit := STR_DEV_INI_DEFAULT_BIT_UNDEFINED;

    if TDevice.BitValueIsDefined(fDelayedBit) then
        gCommManager.AddSwitch(aAdr, aCom, fDelayedBit, aDllName, xProtocol);
end;

procedure TSwitchDevice.WriteDeviceData(aSettingDP: TDataProvider);
var
    xDriverName: string;
begin
    try
        xDriverName := fName + 'Driver';
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'SWITCH');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Driver', xDriverName);
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'RackName', fRackName);
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Delay', IntToStr(fDelay));

        fIntf.Switch_WriteDriverData(aSettingDP, xDriverName, fAdr, fBit, fDefaultBit, fDelayedBit, fReverse,
            fSwitchOffDelay, fSwitchOnDelay, fNeverResetAtInit, fNeverResetAtReset);
    except
        //
    end;
end;

{ TBCReaderDevice }

constructor TBCReaderDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string);
var
    xIniAccess: ISimpleIniAccess;
    xDllName: string;
begin
    inherited Create(aName, aAreaName, dptBCReader);

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);

    FCodeFilter := xIniAccess.ReadString(aName, 'CodeFilter', '*');
    fAdr := xIniAccess.ReadInteger(aName, STR_DEV_INI_IDENT_ADR, -1);
    xDLLName := xIniAccess.ReadString(aName, STR_DEV_INI_IDENT_DLLNAME, '');
    fInitFunc := xIniAccess.ReadString(aName, 'DllInitFunc', 'Init');

    // DllName zu AREA-Namen machen:
    xDllName := StringReplace(xDllName, '.DLL', '', [rfIgnoreCase]);

    if (fAdr > 0) then
    begin
        fBCType := xIniAccess.ReadInteger(aName, 'BCType', 0);
        fInitStr := xIniAccess.ReadString(aName, 'InitStr', 'AcZz');
        fGetBCDelay := xIniAccess.ReadInteger(aName, 'GetBCDelay', 0);
        FIntf := gCommManager.GetZP01BCReaderIntf(fAdr, fBCType, fInitStr, fGetBCDelay);
    end
    else if (xDLLName <> '') then
    begin
        fGetBCDelay := xIniAccess.ReadInteger(aName, 'GetBCDelay', 200);
        FIntf := gCommManager.GetDLLBCReaderIntf(aSettingDP, xDLLName);
    end;
end;

constructor TBCReaderDevice.CreateOldStyle(aName, aAreaName, aCodeFilter: string);
begin
    inherited Create(aName, aAreaName, dptBCReader);

    FCodeFilter := aCodeFilter;
end;

var
    gLastWrittenBCR: string = '';

procedure TBCReaderDevice.WriteDeviceData(aSettingDP: TDataProvider);
var
    xDriverName: string;
begin
    // Keine so gute Lösung: Gibt es 3 Reader in der Reihenfolge BCR1 - BCR2 - BCR1, funktioniert es nicht.
    // Es sollte einen allgemeinen Ansatz geben, um doppelt verwendetete Devices nur einmal zu schreiben.
    if fName = gLastWrittenBCR then
        EXIT;
    gLastWrittenBCR := fName;

    xDriverName := fName + 'Driver';
    try
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'BCREADER');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Driver', xDriverName);
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'CodeFilter',
            self.FCodeFilter);

        fIntf.BCReader_WriteDriverData(aSettingDP, xDriverName, fAdr, fInitStr, fBCType, fGetBCDelay);
    except

    end;
end;

{ TWeighingDevice }

constructor TWeighingDevice.Create(aName: string);
begin
    inherited Create(aName, dptCmpWeigh);

    FUseTubeSensor := true;
end;

constructor TWeighingDevice.CreateOldStyle(aName: string; aBalance: TBalanceDevice);
begin
    inherited Create(aName, dptCmpWeigh);

    FDeviceList.Add(aBalance);
end;

procedure TWeighingDevice.WriteDeviceData(aSettingDP: TDataProvider);
var
    xBalance: TBalanceDevice;
    xTubeSensor: TSensorDevice;
    xDoorSwitch: TSwitchDevice;
begin
    inherited;

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'WEIGHING');

    xBalance := self.GetBalance;
    if Assigned(xBalance) then
    begin
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Balance', xBalance.Name);

        // DoorSwitch an das Balance-Device abgeben:
        xDoorSwitch := self.GetDoorSwitch;
        if Assigned(xDoorSwitch) then
            TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', xBalance.Name, 'DoorSwitch',
                xDoorSwitch.Name);
    end;

    xTubeSensor := self.GetTubeSensor;
    if Assigned(xTubeSensor) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'TubeSensor',
            xTubeSensor.Name);
end;

function TWeighingDevice.GetBalance: TBalanceDevice;
var
    x: integer;
begin
    result := nil;
    for x := 0 to FDeviceList.Count - 1 do
        if (FDeviceList.Items[x] is TBalanceDevice) then
            result := FDeviceList.Items[x] as TBalanceDevice;
end;

function TWeighingDevice.GetTubeSensor: TSensorDevice;
var
    x: integer;
begin
    result := nil;
    if not(FUseTubeSensor) then
        exit;

    for x := 0 to FDeviceList.Count - 1 do
        if (FDeviceList.Items[x] is TSensorDevice) then
            result := FDeviceList.Items[x] as TSensorDevice;
end;

function TWeighingDevice.GetDoorSwitch: TSwitchDevice;
var
    x: integer;
begin
    result := nil;
    for x := 0 to FDeviceList.Count - 1 do
        if (FDeviceList.Items[x] is TSwitchDevice) and
            (pos(STR_WEIGHING_DOORSWITCH_IDENTIFIER, uppercase(FDeviceList.Items[x].Name)) > 0) then
            result := FDeviceList.Items[x] as TSwitchDevice;
end;

{ TBalanceDevice }

class function TBalanceDevice_1.ReadAndCreate(aSettingDP: TDataProvider; const aDevArea: string;
    aIntf: IIntfBalance; aModuleIndex: integer; aName: string): TBalanceDevice_1;
var
    xIniAccess: ISimpleIniAccess;
    xTimeout, xTaraAfterXTimes: integer;
    xCloseDoorAfterInit: boolean;
begin
    // read from Device.ini:
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    xTaraAfterXTimes := xIniAccess.ReadInteger(aName, 'TaraAfterXTimes', 75);
    xCloseDoorAfterInit := xIniAccess.ReadBool(aName, 'CloseDoorAfterInit', false);
    xTimeout := xIniAccess.ReadInteger(aName, 'Timeout', 300);
    result := CreateOldStyle(aSettingDP, aIntf, aModuleIndex, aName, xTimeout, xTaraAfterXTimes,
        xCloseDoorAfterInit);
end;

constructor TBalanceDevice_1.CreateOldStyle(aSettingDP: TDataProvider; aIntf: IIntfBalance;
    aModuleIndex: integer; aName: string; aTimeout, aTaraAfterXTimes: integer; aCloseDoorAfterInit: boolean);
begin
    inherited Create(aName, '', dptBalance);

    FName := aName;
    FIntf := aIntf;
    // set member variables
    FTaraAfterXTimes := aTaraAfterXTimes;
    FTimeout := aTimeout;
    FCloseDoorAfterInit := aCloseDoorAfterInit;
    fBalanceDoorBlocked := False;

    // Create everithing else for balance
    ConfigInterface(aSettingDP, aName);
    CreateSetupFuncsList(aSettingDP, aName);
end;

const
    INT_BALDEV_IONIZER_AFTER_OFF_IN_MILLISEC = 5000;
    INT_BALDEV_DOOR_REQUEST_DELAY_IN_MILLISEC = 200;
    INT_BALDEV_MIN_DOORNOOFREQUESTS = 1;

    // --------------------------------------------------------------------------------------------------
procedure TBalanceDevice_1.ConfigInterface(aSettingDP: TDataProvider; aName: string);
// --------------------------------------------------------------------------------------------------

// --------------------------------------------------------------------------------------------------
var
    xIniAccess: ISimpleIniAccess;

begin
    // read from Balance.ini:
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, STR_SETTINGS_AREA_BALANCE);

    FUseSoftwareTara := xIniAccess.ReadBool(aName, 'UseSoftwareTara', false);
    FDoorNoOfRequests := Max(xIniAccess.ReadInteger(aName, 'DoorNoOfRequests', 10000),
        INT_BALDEV_MIN_DOORNOOFREQUESTS);

    FStableReset := xIniAccess.ReadBool(aName, 'StableReset', false);
    FStableTare := xIniAccess.ReadBool(aName, 'StableTare', true);
    FDoorType := xIniAccess.ReadInteger(aName, 'DoorType', 0);
end;

// --------------------------------------------------------------------------------------------------
procedure TBalanceDevice_1.CreateSetupFuncsList(aSettingDP: TDataProvider; aName: string);
// --------------------------------------------------------------------------------------------------
var
    xIdentList: TStringList;
    xIniAccess: ISimpleIniAccess;
    i: integer;
begin
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, STR_SETTINGS_AREA_BALANCE);

    FSetupFuncs := TStringList.Create;
    xIdentList := TStringList.Create;
    xIniAccess.ReadSection(aName + '_PARAMETER', xIdentList);
    for i := 0 to xIdentList.Count - 1 do
        FSetupFuncs.Add(xIniAccess.ReadString(aName + '_PARAMETER', xIdentList[i], ''));
    xIdentList.Free;
end;

procedure TBalanceDevice_1.WriteDeviceData(aSettingDP: TDataProvider);
var
    xDriverName: string;
begin
    xDriverName := fName + 'Driver';
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'BALANCE');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Driver', xDriverName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'TaraAfterXTimes',
        IntToStr(FTaraAfterXTimes));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'CloseDoorAfterInit',
        TAppSettings.WriteBoolToStr(FCloseDoorAfterInit));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'UseSoftwareTara',
        TAppSettings.WriteBoolToStr(FUseSoftwareTara));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'StableReset',
        TAppSettings.WriteBoolToStr(FStableReset));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'StableTare',
        TAppSettings.WriteBoolToStr(FStableTare));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Timeout', IntToStr(FTimeout));

    fIntf.Balance_WriteDriverData(aSettingDP, xDriverName, fBalanceDoorBlocked, FDoorNoOfRequests, FDoorType,
        FSetupFuncs);

    // AddInt( 'DoorDelay', 2000 );

end;

// --------------------------------------------------------------------------------------------------
procedure TBalanceDeviceSartGenius.ConfigInterface(aSettingDP: TDataProvider; aName: string);
// --------------------------------------------------------------------------------------------------
var
    xIniAccess: ISimpleIniAccess;

begin
    inherited;
    // read from Balance.ini:
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, STR_SETTINGS_AREA_BALANCE);
    FIonizerTimeInSec := xIniAccess.ReadInteger(aName, 'IonizerTime', 0);

end;

// ---------------------------------------- TBalanceDeviceFactory ----------------------------------
// The BalanceDeviceFactory has the purpose of differentiation between different implementations of the
// BalanceDevice at a higher level ( At the Sampler Device Level ).
// At the interface level, there is a further differentiation depending on in which DLL the
// interface is implemented.
// At the interface level, the implementation of the interface in the DLLs must be a basic
// implementation of the functions that the actual hardware is able to perform.  Remember that the purpose
// of the Balance Interface is to make all balance hardwares  look the same to the Sampler Balance Device object.
//
// At the Sampler Device level ( TBalanceDevice ), the actual manner in which the basic interface/DLL functions
// are used, and in which order they are used should be implemented
// Therefore at the Sampler Device level, different implementations of the TBalanceDevice can use
// the exact same interface/DLL in totally different manners.  Thus, this is why we need the differentiation here.
// The parameter aName is used to create the interface.  Upon creation, the interface is assinged a protocol
// based on the aName string.  The protocol is then used here to decide which TBalanceDevice class must
// be used to create the Balance Device Object.

// --------------------------------------------------------------------------------------------------
class function TBalanceDeviceFactory.CreateBalanceDeviceOldStyle(aSettingDP: TDataProvider; aName: string;
    aLeft, aTop, aTaraAfterXTimes: integer; aCloseDoorAfterInit: boolean): TBalanceDevice_1;
// --------------------------------------------------------------------------------------------------
var
    xIntf: IIntfBalance;
    xModuleIndex: integer;
    xProtocol: TProtocol;
begin
    result := nil;
    xIntf := nil;
    // Create the Interface. Return the interface and the module index
    CreateBalanceDeviceInterface(aSettingDP, aName, xIntf, xProtocol, xModuleIndex);
    if not Assigned(xIntf) then
    begin
        // gCommManager.WriteLog( format( 'TBalanceDeviceFactory.CreateBalanceDeviceOldStyle -> Could not create Balance Device [%s]', [ aName ]), LOG_ERROR, true);
        Exit;
    end;
    // Get the class of the Balance device to be created according to the interface.
    // xClass := GetBalanceDeviceClass( xProtocol );

    // Create the balance class, giving it the interface and the moduleindex.
    result := TBalanceDevice_1.CreateOldStyle(aSettingDP, xIntf, xModuleIndex, aName, 300, aTaraAfterXTimes,
        aCloseDoorAfterInit);
end;

// --------------------------------------------------------------------------------------------------
class function TBalanceDeviceFactory.CreateBalanceDevice(aSettingDP: TDataProvider;
    const aDevArea, aName: string): TBalanceDevice_1;
// --------------------------------------------------------------------------------------------------
var
    xIntf: IIntfBalance;
    xModuleIndex: integer;
    xProtocol: TProtocol;
begin
    result := nil;
    xIntf := nil;
    // Create the Interface. Return the interface and the module index
    CreateBalanceDeviceInterface(aSettingDP, aName, xIntf, xProtocol, xModuleIndex);
    if not Assigned(xIntf) then
    begin
        // gCommManager.WriteLog( format( 'TBalanceDeviceFactory.CreateBalanceDevice -> Could not create Balance Device [%s]', [ aName ]), LOG_ERROR, true );
        exit;
    end;
    // Get the class of the Balance device to be created according to the interface.
    // xClass := GetBalanceDeviceClass( xProtocol );
    // if not Assigned( xClass ) then Exit;
    // Create the balance class, giving it the interface and the moduleindex.
    result := TBalanceDevice_1.ReadAndCreate(aSettingDP, aDevArea, xIntf, xModuleIndex, aName);
end;

// --------------------------------------------------------------------------------------------------
class procedure TBalanceDeviceFactory.CreateBalanceDeviceInterface(aSettingDP: TDataProvider; aName: string;
    var aIntf: IIntfBalance; var aProtocol: tProtocol; var aModuleIndex: integer);
// --------------------------------------------------------------------------------------------------
var
    xIniAccess: ISimpleIniAccess;
    xType: string;
    xComPort: integer;
    xDLLName: string;

begin
    // read from Balance.ini:
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, STR_SETTINGS_AREA_BALANCE);
    if (not xIniAccess.SectionExists(aName)) then
        EXIT;

    // Create Interface
    xComPort := xIniAccess.ReadInteger(aName, 'ComPort', -1);
    xType := xIniAccess.ReadString(aName, 'Type', 'AT261');
    xDLLName := xIniAccess.ReadString(aName, 'DllName', 'Balance DllName not found in Settings');
    aProtocol := gCommManager.GetBalanceProtocolFromType(xType);

    aIntf := gCommManager.AddBalance(aSettingDP, aProtocol, aModuleIndex, xComPort, aName, xType, xDLLName);

end;

{ TUserProtectionDevice }

constructor TUserProtectionDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName: string;
    aIdentName: string);
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(aName, dptCmpUserProtect);

    fProtectionOn := false;
    fProtectionPaused := false;

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    fSwitchDLLName := xIniAccess.ReadString(aIdentName, 'DLLName', '');
    fPollDelay := xIniAccess.ReadInteger(aIdentName, 'PollDelay', -1);
    fAutoReactivate := xIniAccess.ReadBool(aIdentName, 'AutoReactivate', false);
    fMessageText := 'Please close all doors for to continue robot action !';
    fMessageText := xIniAccess.ReadString(aIdentName, 'MessageText', fMessageText);
    fCheckDllName := xIniAccess.ReadString(aIdentName, 'CheckDLLName', '');

    fAutoReactivatePollDelay := 1000;
    if fPollDelay > fAutoReactivatePollDelay then
        fAutoReactivatePollDelay := fPollDelay;

end;

constructor TUserProtectionDevice.CreateOldStyle(aSettingDP: TDataProvider; const aDevArea, aName: string;
    aIdentName: string; aDummy: boolean);
var
    xIniAccess: ISimpleIniAccess;
    xSensorComPort: integer;
begin
    Create(aSettingDP, aDevArea, aName, aIdentName);
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, 'ROBOT');

    xSensorComPort := xIniAccess.ReadInteger(STR_ISEC_USERPROTECT, 'RequestComPort', -1);
    if xSensorComPort > 0 then
    begin
        fSensor := TSensorDevice.Create(aSettingDP, aDevArea, STR_ISEC_USERPROTECT, STR_ISEC_USERPROTECT,
            xSensorComPort, -1, STR_DEV_INI_DEFAULT_BIT_UNDEFINED, '', true);
        fDeviceList.Add(fSensor);
    end;

    // fActive         := (xIniAccess.ReadInteger(STR_ISEC_USERPROTECT,'Active', -1)=1);

    fSwitchDLLName := xIniAccess.ReadString(STR_ISEC_USERPROTECT, 'DLLName', '');

end;

function TUserProtectionDevice.GetSensor: TSensorDevice;
var
    x: integer;
begin
    result := nil;

    for x := 0 to FDeviceList.Count - 1 do
        if (FDeviceList.Items[x] is TSensorDevice) then
            result := FDeviceList.Items[x] as TSensorDevice;
end;

procedure TUserProtectionDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    inherited;

    // sub-devices laden
    if (not Assigned(fSensor)) then
        fSensor := GetSensor();

    // schreiben der Daten
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'USERPROTECTION');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'SwitchDLLName', fSwitchDLLName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'PollDelay',
        IntToStr(fPollDelay));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'AutoReactivate',
        TAppSettings.WriteBoolToStr(fAutoReactivate));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'MessageText', fMessageText);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'CheckDLLName', fCheckDLLName);
    if Assigned(fSensor) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Sensor', fSensor.Name);
end;

{ TSensorDevice }

constructor TSensorDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
    aCom, aAdr: integer; aBit, aDllName: string; aPreferSignalLoop: boolean);
var
    xProtocol: TProtocol;
begin
    inherited Create(aName, aAreaName, dptSensor);

    gmStrToIntTry(FTipNumber, Copy(aName, 7, 1)); // für PosPress-Sensor bei Speedy - 7.Buchstabe = Tipnumber

    fAdr := aAdr;
    fBit := aBit;
    FDefaultValue := Settings_ReadDefaultValue(aSettingDP, aDevArea);

    // Bestimmung von Bit (wenn nötig)
    if not TDevice.BitValueIsDefined(aBit) then
        aBit := Settings_ReadBit(aSettingDP, aDevArea);

    // Bestimmung von Protocol
    xProtocol := Settings_ReadProtocol(aSettingDP, aDevArea);
    if (aPreferSignalLoop) and (xProtocol = cdtNone) then
        xProtocol := cdtSignalLoop;
    FIntf := gCommManager.AddSensor(aAdr, aCom, aBit, aDllName, xProtocol);
end;

procedure TSensorDevice.WriteDeviceData(aSettingDP: TDataProvider);
var
    xDriverName: string;
begin
    xDriverName := fName + 'Driver';
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'SENSOR');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Driver', xDriverName);

    fIntf.Sensor_WriteDriverData(aSettingDP, xDriverName, fAdr, fBit, fDefaultValue);
end;

{ TDevSimpleIniAccess }

constructor TDevSimpleIniAccess.Create(aSettingDP: TDataProvider; const aArea: string);
begin
    inherited Create;
    fSettingDP := aSettingDP;
    fArea := aArea;
end;

function TDevSimpleIniAccess.ReadBcrPosition(const aSection, aIdent: string): BCRPOSITION;
var
    xText: string;
    xCurrentSetting: TStringDynArray;
begin
    result.XPos := 0;
    result.YPos := 0;
    result.ZPos := 0;
    result.RPos := 0;
    result.PosName := '';

    xText := self.ReadString(aSection, aIdent, '');

    xCurrentSetting := TAppSettings.StringToStringArray(xText);
    if Length(xCurrentSetting) > 0 then
        result.XPos := StrToIntDef(xCurrentSetting[0], 0);
    if Length(xCurrentSetting) > 1 then
        result.YPos := StrToIntDef(xCurrentSetting[1], 0);
    if Length(xCurrentSetting) > 2 then
        result.ZPos := StrToIntDef(xCurrentSetting[2], 0);
    if Length(xCurrentSetting) > 3 then
        result.RPos := StrToIntDef(xCurrentSetting[3], 0);
    if Length(xCurrentSetting) > 4 then
        result.PosName := xCurrentSetting[4];
end;

function TDevSimpleIniAccess.ReadBarcodeRec(const aSection, aIdent: string): BARCODEREC;
var
    xText: string;
    xCurrentSetting: TStringDynArray;
begin
    result.Adr := -1;
    result.BcType := 0;
    result.Pattern := '???*';
    result.InitStr := 'AcZz';

    xText := self.ReadString(aSection, aIdent, '');

    xCurrentSetting := TAppSettings.StringToStringArray(xText);
    if Length(xCurrentSetting) > 0 then
        result.Adr := StrToIntDef(xCurrentSetting[0], -1);
    if Length(xCurrentSetting) > 1 then
        result.BcType := StrToIntDef(xCurrentSetting[1], 0);
    if Length(xCurrentSetting) > 2 then
        result.Pattern := xCurrentSetting[2];
    if Length(xCurrentSetting) > 3 then
        result.InitStr := xCurrentSetting[3];
end;

function TDevSimpleIniAccess.ReadBcScanrange(const aSection, aIdent: string): BCSCANRANGE;
var
    xText: string;
    xCurrentSetting: TStringDynArray;
begin
    result.XScanRange := 0;
    result.XScanSteps := 0;
    result.YScanRange := 0;
    result.YScanSteps := 0;
    result.ZScanRange := 0;
    result.ZScanSteps := 0;

    xText := self.ReadString(aSection, aIdent, '');

    xCurrentSetting := TAppSettings.StringToStringArray(xText);
    if Length(xCurrentSetting) > 0 then
        result.XScanRange := StrToIntDef(xCurrentSetting[0], 0);
    if Length(xCurrentSetting) > 1 then
        result.XScanSteps := StrToIntDef(xCurrentSetting[1], 0);
    if Length(xCurrentSetting) > 2 then
        result.YScanRange := StrToIntDef(xCurrentSetting[2], 0);
    if Length(xCurrentSetting) > 3 then
        result.YScanSteps := StrToIntDef(xCurrentSetting[3], 0);
    if Length(xCurrentSetting) > 4 then
        result.ZScanRange := StrToIntDef(xCurrentSetting[4], 0);
    if Length(xCurrentSetting) > 5 then
        result.ZScanSteps := StrToIntDef(xCurrentSetting[5], 0);
end;

function TDevSimpleIniAccess.ReadBool(const aSection, aIdent: string; aDefault: boolean): boolean;
begin
    result := TIniSettings.ReadBool(fSettingDP, fArea, aSection, aIdent, aDefault);
end;

function TDevSimpleIniAccess.ReadFloat(const aSection, aIdent: string; aDefault: double): double;
begin
    result := TIniSettings.ReadFloat(fSettingDP, fArea, aSection, aIdent, aDefault);
end;

function TDevSimpleIniAccess.ReadInteger(const aSection, aIdent: string; aDefault: integer): integer;
begin
    result := TIniSettings.ReadInteger(fSettingDP, fArea, aSection, aIdent, aDefault);
end;

function TDevSimpleIniAccess.ReadMotor(const aSection, aIdent: string; aDefSpeed, aDefRamp: integer): MOTOR;
var
    xText: string;
    xCurrentSetting: TStringDynArray;
begin
    result.Adr := -1;
    result.Speed := aDefSpeed;
    result.Ramp := aDefRamp;
    result.InitSpeed := 0;

    xText := self.ReadString(aSection, aIdent, '');

    xCurrentSetting := TAppSettings.StringToStringArray(xText);
    if Length(xCurrentSetting) > 0 then
        result.Adr := StrToIntDef(xCurrentSetting[0], -1);
    if Length(xCurrentSetting) > 1 then
        result.Speed := StrToIntDef(xCurrentSetting[1], aDefSpeed);
    if Length(xCurrentSetting) > 2 then
        result.Ramp := StrToIntDef(xCurrentSetting[2], aDefRamp);
    if Length(xCurrentSetting) > 3 then
        result.InitSpeed := StrToIntDef(xCurrentSetting[3], 0);
end;

procedure TDevSimpleIniAccess.ReadSection(const aSection: string; aStrings: TStrings);
begin
    TIniSettings.ReadSection(fSettingDP, fArea, aSection, aStrings);
end;

function TDevSimpleIniAccess.ReadString(const aSection, aIdent, aDefault: string): string;
begin
    result := TIniSettings.ReadString(fSettingDP, fArea, aSection, aIdent, aDefault);
end;

function TDevSimpleIniAccess.ReadXYRangeData(aSection, aIdent: string): TXYRangeData;
var
    xText: string;
    xCurrentSetting: TStringDynArray;
begin
    result.XRel1 := 0;
    result.XRel2 := 0;
    result.YRel1 := 0;
    result.YRel2 := 0;

    xText := self.ReadString(aSection, aIdent, '');

    xCurrentSetting := TAppSettings.StringToStringArray(xText);
    if Length(xCurrentSetting) > 0 then
        result.XRel1 := StrToIntDef(xCurrentSetting[0], 0);
    if Length(xCurrentSetting) > 1 then
        result.XRel2 := StrToIntDef(xCurrentSetting[1], 0);
    if Length(xCurrentSetting) > 2 then
        result.YRel1 := StrToIntDef(xCurrentSetting[2], 0);
    if Length(xCurrentSetting) > 3 then
        result.YRel2 := StrToIntDef(xCurrentSetting[3], 0);
end;

function TDevSimpleIniAccess.SectionExists(const aSection: string): boolean;
begin
    result := TIniSettings.SectionExists(fSettingDP, fArea, aSection);
end;

{ TIniSettings }
class procedure TIniSettings.SelectAndOpenIdent(aSettingDP: TDataProvider;
    const aArea, aSection, aIdent: string);
begin
    aSettingDP.SelectAndOpen('select * from settings where ' + TSettingsSQL.AreaEq('settings', aArea) +
        ' and ' + TSettingsSQL.SectionEq('settings', aSection) + ' and ' + TSettingsSQL.IdentEq('settings',
        aIdent), true);
end;

class function TIniSettings.ReadBool(aSettingDP: TDataProvider; const aArea, aSection, aIdent: string;
    aDefault: boolean): boolean;
const
    STR_BOOLENTRY_FALSE = '0';
    STR_BOOLENTRY_TRUE = '1';
begin
    result := aDefault;

    SelectAndOpenIdent(aSettingDP, aArea, aSection, aIdent);

    try
        if not aSettingDP.Eof then
        begin
            if (aSettingDP.FieldByName('Value').AsString = STR_BOOLENTRY_TRUE) then
                result := true;

            if (aSettingDP.FieldByName('Value').AsString = '') or
                (aSettingDP.FieldByName('Value').AsString = STR_BOOLENTRY_FALSE) then
                result := false;
        end;
    finally
        aSettingDP.Close;
    end;
end;

class function TIniSettings.ReadFloat(aSettingDP: TDataProvider; const aArea, aSection, aIdent: string;
    aDefault: double): double;

begin
    result := aDefault;

    SelectAndOpenIdent(aSettingDP, aArea, aSection, aIdent);
    try
        if not aSettingDP.Eof then
        begin
            result := StrToFloatDef(aSettingDP.FieldByName('Value').AsString, aDefault);
        end;
    finally
        aSettingDP.Close;
    end;
end;

class function TIniSettings.ReadInteger(aSettingDP: TDataProvider; const aArea, aSection, aIdent: string;
    aDefault: integer): integer;
begin
    result := aDefault;

    SelectAndOpenIdent(aSettingDP, aArea, aSection, aIdent);
    try
        if not aSettingDP.Eof then
        begin
            result := StrToIntDef(aSettingDP.FieldByName('Value').AsString, aDefault);
        end;
    finally
        aSettingDP.Close;
    end;
end;

class procedure TIniSettings.ReadSection(aSettingDP: TDataProvider; const aArea, aSection: string;
    aStrings: TStrings);
begin

end;

class function TIniSettings.ReadString(aSettingDP: TDataProvider;
    const aArea, aSection, aIdent, aDefault: string): string;
begin
    SelectAndOpenIdent(aSettingDP, aArea, aSection, aIdent);
    try
        if aSettingDP.Eof then
            result := aDefault
        else
            result := aSettingDP.FieldByName('Value').AsString;
    finally
        aSettingDP.Close;
    end;
end;

class function TIniSettings.SectionExists(aSettingDP: TDataProvider; const aArea, aSection: string): boolean;
begin
    aSettingDP.SelectAndOpen('select * from settings where ' + TSettingsSQL.AreaEq('settings', aArea) +
        ' and ' + TSettingsSQL.SectionEq('settings', aSection), true);
    try
        result := not aSettingDP.Eof;
    finally
        aSettingDP.Close;
    end;
end;

{ TIntfCommunication }

constructor TIntfCommunication.Create(aProtocol: TProtocol);
begin
    inherited Create;

    FProtocol := aProtocol;
end;

function TIntfCommunication.GetProtocol: TProtocol;
begin
    result := FProtocol;
end;

{ TIntfDll }

constructor TIntfDll.Create(aDllName: string; aProtocol: TProtocol);
begin
    inherited Create(aProtocol);

    FDllName := aDllName;
end;

const
    INT_DEFAULT_RESOURCE_LOCK_COUNT = 1;

    // --------------------------------------------------------------------------------------------------
    // TDevice
    // --------------------------------------------------------------------------------------------------
constructor TDevice.Create(aName: string; aDType: TDeviceType);
begin
    inherited Create;
    // ---------------------------------------------- Setzen der internen Variablen
    FName := Copy(aName, 1, MODULENAME_LENGTH);
    FDType := aDType;
end;

function TDevice.GetPortAdrBit: string;
begin
    result := '';
end;

class function TDevice.ContainsTypeName(const aTypeName: string; const aTypeStr: string): boolean;
begin
    result := Pos(aTypeName, aTypeStr) = 1;
end;

class function TDevice.GetTypeFromTypeName(aTypeStr: string; aAdr, aCom: integer; aBit: string): TDeviceType;
var
    xTypeStr: string;
begin
    result := dptNone;
    xTypeStr := UpperCase(aTypeStr);

    if ContainsTypeName(STR_LEAFDEVICE_VACUUM, xTypeStr) then
        result := dptVacuum;
    if ContainsTypeName(STR_LEAFDEVICE_SWITCH, xTypeStr) then
        result := dptAnySwitch;
    if ContainsTypeName(STR_LEAFDEVICE_BLOWER, xTypeStr) then
        result := dptBlower;
    if ContainsTypeName(STR_LEAFDEVICE_PUMP, xTypeStr) then
        result := dptPump;
    if ContainsTypeName(STR_LEAFDEVICE_MOTOR, xTypeStr) then
        result := dptVarRediMotor;
    if ContainsTypeName(STR_LEAFDEVICE_VARREDIMOTOR, xTypeStr) then
        result := dptVarRediMotor;
    if ContainsTypeName(STR_LEAFDEVICE_XMOTOR, xTypeStr) then
        result := dptXMotor;
    if ContainsTypeName(STR_LEAFDEVICE_YMOTOR, xTypeStr) then
        result := dptYMotor;
    if ContainsTypeName(STR_LEAFDEVICE_ZMOTOR, xTypeStr) then
        result := dptZMotor;
    if ContainsTypeName(STR_LEAFDEVICE_RMOTOR, xTypeStr) then
        result := dptRMotor;
    if ContainsTypeName(STR_LEAFDEVICE_GMOTOR, xTypeStr) then
        result := dptGMotor;
    if ContainsTypeName(STR_LEAFDEVICE_PNEUGRIPOPENCLOSESWITCH, xTypeStr) then
        result := dptPneumaticGripOpenCloseSwitch;
    if ContainsTypeName(STR_LEAFDEVICE_PNEUGRIPDISTSWITCH, xTypeStr) then
        result := dptPneumaticGripDistanceSwitch;
    if ContainsTypeName(STR_LEAFDEVICE_STATESWITCH, xTypeStr) then
        result := dptStateSwitch;

    if ContainsTypeName(STR_LEAFDEVICE_THERMOSTAT, xTypeStr) then
        result := dptThermostat;
    if ContainsTypeName(STR_LEAFDEVICE_BCREADER, xTypeStr) then
        result := dptBCReader;
    if ContainsTypeName(STR_LEAFDEVICE_BALANCE, xTypeStr) then
        result := dptBalance;
    if ContainsTypeName(STR_LEAFDEVICE_PIPPUMP, xTypeStr) then
        result := dptPipPump;
    if ContainsTypeName(STR_LEAFDEVICE_GRIPPER, xTypeStr) then
        result := dptEntireGripper;
    if ContainsTypeName(STR_LEAFDEVICE_SENSOR, xTypeStr) then
        result := dptSensor;
    if ContainsTypeName(STR_LEAFDEVICE_XWAYVALVE, xTypeStr) then
        result := dptXWayValve;

    if ContainsTypeName(STR_LEAFDEVICE_SIMPLEWATCHDOG, xTypeStr) then
        result := dptSimpleWatchDog;

    // alte Bezeichnungen (nur beim Einlesen)
    if ContainsTypeName(STR_LEAFDEVICE_OLD_LINEIN, xTypeStr) then
        result := dptSensor;
    if ContainsTypeName(STR_LEAFDEVICE_OLD_HEAT, xTypeStr) then
        result := dptThermostat;
    if ContainsTypeName(STR_LEAFDEVICE_OLD_COOL, xTypeStr) then
        result := dptThermostat;

    // komplizierte Shaker-Protokoll-Bestimmung sollte hier nicht stehen:
    if ContainsTypeName(STR_LEAFDEVICE_ISHAK, xTypeStr) then
        result := dptShakerIKA; // neuer IKA Vortexer
    if ContainsTypeName(STR_LEAFDEVICE_ASHAK, xTypeStr) then
        result := dptShakerIKAOld; // alter MOSS IKA Vortexer
    if ContainsTypeName(STR_LEAFDEVICE_SHAKER, xTypeStr) then
    begin
        if (aAdr <= 0) and (aCom <> gCommManager.ConradBoardPort) then
            result := dptShakerCAT // CAT-Vortexer
        else
            result := dptSwitchShaker; // Redi-Shaker
    end;
end;

procedure TDevice.Find_ByName(var aDevice: TDevice; aName: string);
begin
    if (aDevice <> nil) then
        EXIT;
    if (Uppercase(FName) <> Uppercase(aName)) then
        EXIT; // nicht mehr Case-sensitive!

    aDevice := self;
end;

procedure TDevice.Find_ByClass(var aDevice: TDevice; aClass: TDeviceClass);
begin
    if (aDevice <> nil) then
        EXIT;
    if not(self is aClass) then
        EXIT;

    aDevice := self;
end;

procedure TDevice.Find_ByNameAndClass(var aDevice: TDevice; aName: string; aClass: TDeviceClass);
begin
    if (aDevice <> nil) then
        EXIT;
    if not(self is aClass) then
        EXIT;
    if (Uppercase(FName) <> Uppercase(aName)) then
        EXIT; // nicht mehr Case-sensitive!

    aDevice := self;
end;

procedure TDevice.Find_ByNameCut(var aDevice: TDevice; aSubString: string; aClass: TDeviceClass);
begin
    if (aDevice <> nil) then
        EXIT;
    if not(self is aClass) then
        EXIT;
    if (Pos(Uppercase(aSubString), Uppercase(FName)) <> 1) then
        EXIT; // nicht mehr Case-sensitive!

    aDevice := self;
end;

procedure TDevice.FindName_ByClass(aList: TStrings; aClass: TDeviceClass);
begin
    if self is aClass then
        aList.Add(self.Name);
end;

class function TDevice.BitValueIsDefined(const aBit: string; aZeroBased: boolean): boolean;
begin
    result := false;
    if (aBit[1] = '-') then
        EXIT; // wenn erster Buchstabe ein minus ist -> undefiniert
    if (aBit = '') then
        EXIT; // wenn aBit = '' -> undefiniert
    if (not aZeroBased) and (aBit = '0') then
        EXIT; // wenn 0, und 0 nicht erlaubt ist -> undefiniert
    result := true;
end;

class function TDevice.GetSamplerProtocol: TProtocol;
begin
    if TAppSettings.IsSias() then
        result := cdtCANBus
    else
        result := cdtRosys;
end;

class function TDevice.GetGripperArmName(aArmNumber: integer): string;
const
    STR_NAME_GRIPPERARM = 'Gripper Arm';
begin
    if (aArmNumber = 1) then // Kompatibilität: 1. Arm heißt "Gripper Arm"
        result := STR_NAME_GRIPPERARM
    else
        result := STR_NAME_GRIPPERARM + ' ' + IntToStr(aArmNumber);
end;

class function TDevice.GetPipetteArmName(aArmNumber: integer): string;
const
    STR_NAME_PIPETTEARM = 'Pipetting Arm ';
begin
    result := STR_NAME_PIPETTEARM + IntToStr(aArmNumber);
end;

// --------------------------------------------------------------------------------------------------
// TLeafDevice -> TDevice
// --------------------------------------------------------------------------------------------------
constructor TLeafDevice.Create(aName, aAreaName: string; aDType: TDeviceType);
begin
    inherited Create(aName, aDType);

    FDelay := 0;
    FCarrierName := '';
    FRackName := '';
    FRow := 0;
    FCol := 0;
end;

class function TLeafDevice.GetProtocolFromType(aDType: TDeviceType; aCom, aBit: integer): TProtocol;
begin
    result := cdtNone;
    if (aDType = dptVarRediMotor) then
        result := cdtPeerless;
    if (aDType = dptShakerCAT) then
        result := cdtCATVortexer;
    if (aDType = dptShakerIKA) then
        result := cdtIKAVortexer;
end;

class function TLeafDevice.GetProtocolFromProtName(aProtName: string): TProtocol;
begin
    result := cdtNone;
    if (UpperCase(aProtName) = STR_DEV_PROTOCOL_COMPULAB) then
        result := cdtCompulab;
    if (UpperCase(aProtName) = STR_DEV_PROTOCOL_CONRAD) then
        result := cdtConradRelay;
    if (UpperCase(aProtName) = STR_DEV_PROTOCOL_KOLTER) then
        result := cdtKolterOpto;
    if (UpperCase(aProtName) = STR_DEV_PROTOCOL_SIGNALLOOP) then
        result := cdtSignalloop;
    if (UpperCase(aProtName) = STR_DEV_PROTOCOL_HUBER) then
        result := cdtHuber;
    if (UpperCase(aProtName) = STR_DEV_PROTOCOL_CAT) then
        result := cdtCATVortexer;
    if (UpperCase(aProtName) = STR_DEV_PROTOCOL_CANBUS) then
        result := cdtCANBus;

end;

// Read from device ini methods:
function TLeafDevice.Settings_ReadProtocol(aSettingDP: TDataProvider; const aDevArea: string): TProtocol;
var
    xIniAccess: ISimpleIniAccess;
begin
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    result := GetProtocolFromProtName(xIniAccess.ReadString(FName, STR_DEV_INI_IDENT_PROTOCOL, ''));
end;

function TLeafDevice.Settings_ReadBit(aSettingDP: TDataProvider; const aDevArea: string): string;
var
    xIniAccess: ISimpleIniAccess;
begin
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    result := xIniAccess.ReadString(FName, STR_DEV_INI_IDENT_BIT, STR_DEV_INI_DEFAULT_BIT_UNDEFINED)
end;

function TLeafDevice.Settings_ReadDefaultValue(aSettingDP: TDataProvider; const aDevArea: string): integer;
var
    xIniAccess: ISimpleIniAccess;
begin
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    result := xIniAccess.ReadInteger(FName, STR_DEV_INI_IDENT_DEFAULTVALUE, 0)
end;

procedure TLeafDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    TLogManager.Instance.Log('WARNING: Device ' + fName + ', Conversion is not yet implemented!', 1);
end;

{ TAppSettings }

class procedure TAppSettings.Init(aIsSias: boolean);
begin
    fIsSias := aIsSias;
end;

class function TAppSettings.IsSias: boolean;
begin
    result := fIsSias;
end;

class function TAppSettings.CopyTillEnd(aStrText: string; aIntIndex: integer): string;
begin
    result := Copy(aStrText, aIntIndex, Length(aStrText));
end;

class function TAppSettings.Extract(var S: string; index, Count, Skip: Integer): string;
// result is index upto and including Count
// S is right side from index + count + skip until end of string
begin
    result := Copy(S, index, Count);
    S := CopyTillEnd(S, index + Count + Skip);
end;

class function TAppSettings.GetFixationNameFromDeviceName(aDeviceName: string): string;
begin
    result := aDeviceName + 'Fixation';
end;

class function TAppSettings.StringToStringArray(aStrText: string): TStringDynArray;
const
    cDelimiter = ',';
var
    xList: TList<string>;
    intPos, i: integer;
begin

    SetLength(result, 0);
    if Length(aStrText) = 0 then
        Exit;

    xList := TList<string>.Create;
    try
        while true do
        begin
            intPos := Pos(cDelimiter, aStrText);
            if intPos = 0 then
            begin
                xList.Add(aStrText);
                Break;
            end;
            xList.Add(Extract(aStrText, 1, intPos - 1, Length(cDelimiter)));
        end;

        SetLength(result, xList.Count);

        for i := 0 to xList.Count - 1 do
            result[i] := xList[i];

    finally
        FreeAndNil(xList);
    end;
end;

class function TAppSettings.WriteBoolToStr(aValue: boolean): string;
begin
    if aValue then
        result := 'YES'
    else
        result := 'NO';
end;

{ TStateSwitchDevice }

constructor TStateSwitchDevice.CreateOldStyle(aSettingDP: TDataProvider;
    const aDevArea, aName, aAreaName: string; aAdr, aCom: integer; aBit: string; aActiveState: TSystemState);
begin
    inherited Create(aSettingDP, aDevArea, aName, aAreaName, dptStateSwitch, aAdr, aCom, aBit,
        STR_DEV_INI_DEFAULT_BIT_UNDEFINED, '', true, cdtConradRelay);

    FActiveState := aActiveState;
end;

constructor TStateSwitchDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
    aDType: TDeviceType; aAdr, aCom: integer; const aBit, aDefaultBit, aDllName: string);
var
    xIniAccess: ISimpleIniAccess;
    xStateName: string;
begin
    inherited Create(aSettingDP, aDevArea, aName, aAreaName, aDType, aAdr, aCom, aBit, aDefaultBit,
        aDllName, true);

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    xStateName := xIniAccess.ReadString(aName, 'StateName', '');

    fActiveState := GetStateFromStateName(xStateName);

    if fActiveState = sStateUnknown then
    begin
        // ShowMessage( format( 'State Switch [%s]: State name [%s] is not valid', [ aName, xStateName ] ) );
    end;
end;

class function TStateSwitchDevice.GetStateFromStateName(const aStateName: string): TSystemState;
var
    xUpperStateName: string;
begin
    result := sStateUnknown;

    xUpperStateName := UpperCase(aStateName);
    if Pos('ACTIVE', xUpperStateName) = 1 then
        result := sStateActive
    else if Pos('SETUP', xUpperStateName) = 1 then
        result := sStateSetup
    else if Pos('ERROR', xUpperStateName) = 1 then
        result := sStateError;
end;

class function TStateSwitchDevice.GetStateNameFromState(aState: TSystemState): string;
begin
    result := STR_STATENAME_UNKNOWN;
    case aState of
        sStateActive:
            result := STR_STATENAME_ACTIVE;
        sStateSetup:
            result := STR_STATENAME_SETUP;
        sStateError:
            result := STR_STATENAME_ERROR;
    end;
end;

// --------------------------------------------------------------------------------------------------
// TMotorDevice -> TLeafDevice -> TDevice
// --------------------------------------------------------------------------------------------------
constructor TMotorDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
    aAdr: integer; aDType: TDeviceType; aMType: TArmMotorType);
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(aName, aAreaName, aDType);
    fExecID := 0;

    FMType := aMType;
    FReverseChanged := false;

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    fWorldOffset_Steps := xIniAccess.ReadInteger(aName, 'WorldOffset', 0);
    fAdr := aAdr;
    fReverseM := xIniAccess.ReadBool(aName, 'Reverse', false);
    fSpeed := xIniAccess.ReadInteger(aName, 'Speed', 0);
    fRamp := xIniAccess.ReadInteger(aName, 'Ramp', 0);
    fInitSpeed := xIniAccess.ReadInteger(aName, 'InitSpeed', 0);
    fStepsPerUnit := xIniAccess.ReadFloat(aName, 'StepsPerUnit', 10);
    fMin_Steps := xIniAccess.ReadInteger(aName, 'Min', 0);
    fMax_Steps := xIniAccess.ReadInteger(aName, 'Max', 0);
    fInitOffset := xIniAccess.ReadInteger(aName, 'InitOffset', 0);
    fTrackID := xIniAccess.ReadInteger(aname, 'TrackID', 0);
    fIntf := gCommManager.AddMotor(aName, aDType, aAdr, fSpeed, fRamp, fInitSpeed, self.GetSamplerProtocol());
end;

procedure TMotorDevice.SetDepracatedProperties(aWorldOffset_Steps: MPos; aReverse: boolean);
// Use of this function is strictly forbidden!
begin
    fWorldOffset_Steps := aWorldOffset_Steps;
    fReverseM := aReverse;
end;

function TMotorDevice.GetActive(): boolean;
begin
    result := Assigned(fIntf);
end;

function TMotorDevice.CalcReverse_Steps(aMotorPos: MPOS): MPOS;
begin
    if fReverseM then
        result := -aMotorPos
    else
        result := aMotorPos
end;

function TMotorDevice.CalcReverse_MM(aMotorPos_mm: TPosMM): TPosMM;
begin
    if fReverseM then
        result := -aMotorPos_mm
    else
        result := aMotorPos_mm
end;

function TMotorDevice.MotorToWorld_Steps(aMotorPos: MPOS): MPOS;
begin
    result := CalcReverse_Steps(aMotorPos) + fWorldOffset_Steps;
end;

function TMotorDevice.WorldToMotor_Steps(aWorldPos: MPOS): MPOS;
begin
    result := CalcReverse_Steps(aWorldPos - fWorldOffset_Steps);
end;

procedure TMotorDevice.WriteDeviceData(aSettingDP: TDataProvider);
var
    xDriverName: string;
begin
    xDriverName := fName + 'Driver';
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'WorldOffset',
        FloatToStr(self.GetNewWorldOffset_mm));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Reverse',
        TAppSettings.WriteBoolToStr(fReverseM));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'StepsPerUnit',
        FloatToStr(fStepsPerUnit));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Min', IntToStr(fMin_Steps));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Max', IntToStr(fMax_Steps));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'InitOffset',
        IntToStr(fInitOffset));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Driver', xDriverName);

    fIntf.Motor_WriteDriverData(aSettingDP, xDriverName, fExecID, fAdr, fSpeed, fRamp, fInitSpeed);
end;

function TMotorDevice.GetMaxPos_Steps: MPos;
begin
    result := MotorToWorld_Steps(fMax_Steps);
end;

function TMotorDevice.GetMinPos_Steps: MPos;
begin
    result := MotorToWorld_Steps(fMin_Steps);
end;

function TMotorDevice.DistanceToMin_Steps(aWorldDest: MPos): MPos;
// How many steps are left from the current pos to min pos
// returns negative value if aWorldDest would be less than fMinSteps
begin
    result := WorldToMotor_Steps(aWorldDest) - fMin_Steps;
end;

function TMotorDevice.DistanceToMax_Steps(aWorldDest: MPos): MPos;
// How many steps are left from the current pos to Max pos
// returns negative value if aWorldDest would be greater than fMaxSteps
begin
    result := fMax_Steps - WorldToMotor_Steps(aWorldDest);
end;

function TMotorDevice.IsBeyondMin_Steps(aWorldDest: MPos): boolean;
begin
    result := DistanceToMin_Steps(aWorldDest) < 0;
end;

function TMotorDevice.IsBeyondMax_Steps(aWorldDest: MPos): boolean;
begin
    result := DistanceToMax_Steps(aWorldDest) < 0;
end;

function TMotorDevice.IsBeyondMinOrMax_Steps(aWorldDest: MPos): boolean;
begin
    result := IsBeyondMin_Steps(aWorldDest) or IsBeyondMax_Steps(aWorldDest);
end;

function TMotorDevice.GetStepsPerUnit: Extended;
begin
    result := fStepsPerUnit;
end;

function TMotorDevice.GetMotorUnit: TMotorUnit;
begin
    result := mu_MM;
    if (FMType = amtRMotor) then
        result := mu_Degree;
end;

function TMotorDevice.GetNewWorldOffset_mm: TPosMM;
var
    xNewWorldOffset_Steps: MPos;
begin
    if fReverseChanged then
        xNewWorldOffset_Steps := self.CalcReverse_Steps(self.fWorldOffset_Steps)
    else
        xNewWorldOffset_Steps := self.fWorldOffset_Steps;

    result := GetUnitFromSteps(xNewWorldOffset_Steps);
end;

function TMotorDevice.GetMaxPos_Unit: TPosMM;
begin
    result := GetUnitFromSteps(GetMaxPos_Steps);
end;

function TMotorDevice.GetMinPos_Unit: TPosMM;
begin
    result := GetUnitFromSteps(GetMinPos_Steps);
end;

function TMotorDevice.GetStepsFromUnit(aValue: TPosMM): MPos;
begin
    result := Round(aValue * GetStepsPerUnit);
end;

function TMotorDevice.GetUnitFromSteps(aValue: MPos): TPosMM;
begin
    result := aValue / GetStepsPerUnit;
end;

function TMotorDevice.GetDefaultRamp: integer;
begin
    result := fRamp;
end;

function TMotorDevice.GetDefaultSpeed: integer;
begin
    result := fSpeed;
end;

function TMotorDevice.GetWorldInitOffset_Steps(): MPos;
begin
    result := MotorToWorld_Steps(fInitOffset);
end;

constructor TXMotorDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
    aAdr: integer; aDType: TDeviceType);
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(aSettingDP, aDevArea, aName, aAreaName, aAdr, aDType, amtXMotor);
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    fConflictBufferMinus_MM := xIniAccess.ReadFloat(aName, 'ConflictBufferMinus', 0);
    fConflictBufferPlus_MM := xIniAccess.ReadFloat(aName, 'ConflictBufferPlus', 0);
end;

function TXMotorDevice.GetNewWorldOffset_mm: TPosMM;
begin
    result := inherited GetNewWorldOffset_mm() + TAppSettings.XOffsetLeftToTip1;
end;

procedure TXMotorDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    inherited;
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'XMOTOR');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'ConflictBufferMinus',
        FloatToStr(fConflictBufferMinus_MM));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'ConflictBufferPlus',
        FloatToStr(fConflictBufferPlus_MM));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'TrackID', IntToStr(fTrackID));
end;

constructor TYMotorDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
    aAdr: integer; aDType: TDeviceType);
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(aSettingDP, aDevArea, aName, aAreaName, aAdr, aDType, amtYMotor);
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    fMotorIndex := xIniAccess.ReadInteger(aName, 'TipNo', 1) - 1;
end;

function TYMotorDevice.GetNewWorldOffset_mm: TPosMM;
begin
    result := inherited GetNewWorldOffset_mm() + TAppSettings.YOffsetFrontToTip1;
end;

procedure TYMotorDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    // Y-Motoren: Reverse wird immer auf YES gesetzt!
    if not fReverseM then
    begin
        fReverseM := true;
        fReverseChanged := true;
    end;

    inherited;
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'YMOTOR');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'TipNo',
        IntToStr(fMotorIndex + 1));
end;

{ TZMotorDevice }

constructor TZMotorDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
    aAdr: integer; aDType: TDeviceType);
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(aSettingDP, aDevArea, aName, aAreaName, aAdr, aDType, amtZMotor);
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    fMotorIndex := xIniAccess.ReadInteger(aName, 'TipNo', 1) - 1;
    fUseRealBlockMove := (xIniAccess.ReadInteger(aName, 'BlockMove', 1) = 1);
    fLiquidDetection := (xIniAccess.ReadInteger(aName, 'LiquidDetect', 1) = 1);
    fScanSpeed := xIniAccess.ReadInteger(aName, 'ScanSpeed', 500);
    fRetractSpeed := Min(self.DefaultSpeed, xIniAccess.ReadInteger(aName, 'RetractSpeed', 500));
    fWashRetractSpeed := Min(self.DefaultSpeed, xIniAccess.ReadInteger(aName, 'WashRetractSpeed', 0));
end;

function TZMotorDevice.GetNewWorldOffset_mm: TPosMM;
begin
    // für Z-Motoren ist alles anders:
    result := GetUnitFromSteps(fMax_Steps);
end;

function TZMotorDevice.GetNewZTavelValue: TPosMM;
begin
    result := GetUnitFromSteps(fMax_Steps) + GetUnitFromSteps(fWorldOffset_Steps);
end;

procedure TZMotorDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    // Z-Motoren: Reverse wird immer auf YES gesetzt!
    if not fReverseM then
    begin
        fReverseM := true;
        fReverseChanged := true;
    end;

    inherited;
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'ZMOTOR');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'TipNo',
        IntToStr(fMotorIndex + 1));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'BlockMove',
        TAppSettings.WriteBoolToStr(fUseRealBlockMove));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'LiquidDetect',
        TAppSettings.WriteBoolToStr(fLiquidDetection));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'ScanSpeed',
        IntToStr(fScanSpeed));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'RetractSpeed',
        IntToStr(fRetractSpeed));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'WashRetractSpeed',
        IntToStr(fWashRetractSpeed));
end;

{ TIndividualMultiYMotorDevice }

constructor TIndividualMultiYMotorDevice.Create(aSettingDP: TDataProvider;
    const aDevArea, aName, aSection: string);
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(aName, dptCmpYMotorsIndiv);

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    fYOffset := xIniAccess.ReadInteger(aSection, 'Offset', 0);
    fInitOffset := xIniAccess.ReadInteger(aSection, 'InitOffset', 0);
    fMapAllToFirst := xIniAccess.ReadBool(aSection, 'MapAllToFirst', false);
end;

function TIndividualMultiYMotorDevice.GetMotor(aIndex: integer): TMotorDevice;
begin
    if fMapAllToFirst then
        result := fMotors[0]
    else
        result := fMotors[aIndex];
end;

procedure TIndividualMultiYMotorDevice.LoadDevices();
var
    x: integer;
    xYMotor: TYMotorDevice;
begin
    for x := 0 to fDeviceList.Count - 1 do
    begin
        if not(fDeviceList.Items[x] is TYMotorDevice) then
            CONTINUE;
        xYMotor := fDeviceList.Items[x] as TYMotorDevice;
        fMotors[xYMotor.MotorIndex] := xYMotor;
    end;

    for x := 0 to high(fMotors) do
    begin
        if not Assigned(fMotors[x]) then
            CONTINUE;
        if fYOffset > 0 then
            fMotors[x].SetDepracatedProperties(fYOffset * x, false);
        if fInitOffset > 0 then
            fMotors[x].InitOffset := self.fInitOffset;
    end;
end;

procedure TIndividualMultiYMotorDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    inherited;

    MultiY_WriteDeviceData(aSettingDP, fName)
end;

procedure TIndividualMultiYMotorDevice.SetExecID(aExecID: integer);
var
    x: integer;
begin
    fExecID := aExecID;
    for x := 0 to high(fMotors) do
    begin
        if Assigned(GetMotor(x)) then
        begin
            GetMotor(x).ExecID := aExecID;
        end;
    end;
end;

procedure TIndividualMultiYMotorDevice.MultiY_WriteDeviceData(aSettingDP: TDataProvider; const aName: string);
var
    x: integer;
begin
    // schreiben der Daten
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'Type', 'INDIVIDUALMULTIYMOTOR');
    for x := 1 to MAX_TIPS do
    begin
        if Assigned(fMotors[x - 1]) then
            TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'Motor' + IntToStr(x),
                fMotors[x - 1].Name);
    end;
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'Offset', IntToStr(fYOffset));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'InitOffset',
        IntToStr(fInitOffset));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'MapAllToFirst',
        TAppSettings.WriteBoolToStr(fMapAllToFirst));
end;

{ TSingleMultiYMotorDevice }

constructor TSingleMultiYMotorDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName: string;
    aYMotor: TYMotorDevice);
begin
    inherited Create(aSettingDP, aDevArea, aName, aName);
    fMotors[0] := aYMotor;
end;

function TSingleMultiYMotorDevice.GetYMotor(): TYMotorDevice;
begin
    result := GetMotor(0) as TYMotorDevice;
end;

{ TRMotorDevice }

constructor TRMotorDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
    aAdr: integer; aDType: TDeviceType);
begin
    inherited Create(aSettingDP, aDevArea, aName, aAreaName, aAdr, aDType, amtRMotor);
end;

procedure TRMotorDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    inherited;
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'RMOTOR');
end;

{ TGMotorDevice }

constructor TGMotorDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
    aAdr: integer; aDType: TDeviceType);
begin
    inherited Create(aSettingDP, aDevArea, aName, aAreaName, aAdr, aDType, amtRMotor);
end;

procedure TGMotorDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    inherited;
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'GMOTOR');
end;

{ TMultiZMotorDevice }

constructor TMultiZMotorDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aSection: string);
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(aName, dptCmpZMotorsIndiv);
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    fMapAllToFirst := xIniAccess.ReadBool(aSection, 'MapAllToFirst', false);
end;

procedure TMultiZMotorDevice.LoadDevices();
var
    x: integer;
    xZMotor: TZMotorDevice;
begin
    for x := 0 to fDeviceList.Count - 1 do
    begin
        if not(fDeviceList.Items[x] is TZMotorDevice) then
            CONTINUE;
        xZMotor := fDeviceList.Items[x] as TZMotorDevice;
        fMotors[xZMotor.MotorIndex] := xZMotor;
    end;
end;

function TMultiZMotorDevice.GetMotor(aIndex: Integer): TMotorDevice;
begin
    result := nil;
    if fMapAllToFirst then
        result := fMotors[0]
    else if (aIndex >= 0) and (aIndex <= high(fMotors)) then
        result := FMotors[aIndex]
end;

function TMultiZMotorDevice.GetNewZTravelValue: TPosMM;
var
    x: integer;
    xZTravel, xLastZTravel: TPosMM;
begin
    xZTravel := 0;
    xLastZTravel := -9999;
    for x := 0 to high(fMotors) do
    begin
        if Assigned(GetMotor(x)) and (GetMotor(x) is TZMotorDevice) then
        begin

            if (xLastZTravel <> -9999) and (xLastZTravel <> xZTravel) then
            begin
                TLogManager.Instance.Log('WARNING: Z-Travel values are different', 1);
            end;

            xZTravel := (GetMotor(x) as TZMotorDevice).GetNewZTavelValue;
            xLastZTravel := xZTravel;
        end;
    end;
    result := xZTravel;
end;

function TMultiZMotorDevice.GetStepsPerUnit: extended;
var
    x: integer;
    xStepsPerUnit, xLastStepsPerUnit: extended;
begin
    xStepsPerUnit := 1;
    xLastStepsPerUnit := -9999;
    for x := 0 to high(fMotors) do
    begin
        if Assigned(GetMotor(x)) and (GetMotor(x) is TZMotorDevice) then
        begin

            if (xLastStepsPerUnit <> -9999) and (xLastStepsPerUnit <> xStepsPerUnit) then
            begin
                TLogManager.Instance.Log('WARNING: ZMotors: StepsPerUnit values are different', 1);
            end;

            xStepsPerUnit := (GetMotor(x) as TZMotorDevice).GetStepsPerUnit;
            xLastStepsPerUnit := xStepsPerUnit;
        end;
    end;
    result := xStepsPerUnit;
end;

procedure TMultiZMotorDevice.SetExecID(aExecID: integer);
var
    x: integer;
begin
    fExecID := aExecID;
    for x := 0 to high(fMotors) do
    begin
        if Assigned(GetMotor(x)) then
        begin
            GetMotor(x).ExecID := aExecID;
        end;
    end;
end;

procedure TMultiZMotorDevice.MultiZ_WriteDeviceData(aSettingDP: TDataProvider; const aName: string);
var
    x: integer;
begin
    inherited;

    // schreiben der Daten
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'Type', 'MULTIZMOTOR');
    for x := 1 to MAX_TIPS do
    begin
        if Assigned(fMotors[x - 1]) then
            TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'Motor' + IntToStr(x),
                fMotors[x - 1].Name);
    end;
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'MapAllToFirst',
        TAppSettings.WriteBoolToStr(fMapAllToFirst));
end;

procedure TMultiZMotorDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    inherited;

    MultiZ_WriteDeviceData(aSettingDP, fName);
end;

{ TSingleMultiZMotorDevice }

constructor TSingleMultiZMotorDevice.Create(aSettingDP: TDataProvider;
    const aDevArea, aName, aSection: string; aZMotor: TZMotorDevice);
begin
    inherited Create(aSettingDP, aDevArea, aName, aSection);
    fMotors[0] := aZMotor;
end;

function TSingleMultiZMotorDevice.GetZMotor(): TZMotorDevice;
begin
    result := fMotors[0] as TZMotorDevice;
end;

{ TStateSignalDevice }

function TStateSignalDevice.AddSwitch(aSettingDP: TDataProvider; const aDevArea, aName: string;
    aActiveState: TSystemState; aCom: integer; aBit: string): TSwitchDevice;
begin
    result := nil;
    if not TDevice.BitValueIsDefined(aBit) then
        EXIT;

    result := TStateSwitchDevice.CreateOldStyle(aSettingDP, aDevArea, aName, FName, -1, aCom, aBit,
        aActiveState);
    FDeviceList.Add(result);
end;

constructor TStateSignalDevice.Create(const aName: string);
begin
    inherited Create(aName, dptCmpStateSignal);
end;

procedure TStateSignalDevice.WriteDeviceData(aSettingDP: TDataProvider);
var
    x, xActIndex: integer;
begin
    inherited;

    // schreiben der Daten
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'STATESIGNAL');

    xActIndex := 1;
    for x := 0 to FDeviceList.Count - 1 do
    begin
        if (FDeviceList.Items[x] is TStateSwitchDevice) then
        begin
            case ((FDeviceList.Items[x] as TStateSwitchDevice).ActiveState) of
                sStateActive:
                    begin
                        if (xActIndex = 1) then
                            TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName,
                                'ActiveSignal', FDeviceList.Items[x].Name)
                        else
                            TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName,
                                'ActiveSignal2', FDeviceList.Items[x].Name);
                        inc(xActIndex);
                    end;
                sStateSetup:
                    begin
                        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'SetupSignal',
                            FDeviceList.Items[x].Name);
                    end;
                sStateError:
                    begin
                        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'ErrorSignal',
                            FDeviceList.Items[x].Name);
                    end;
            end;
        end;
    end;
end;

constructor TRediDevice.Create(aName: string; aSection: string);
begin
    inherited Create(aName, dptCmpRedi);
    fPump := nil;
    fVacuum := nil;
    fBlower := nil;
    fMotor := nil;
    fFeeding := nil;
end;

procedure TRediDevice.LoadDevices();
var
    x: integer;
begin
    for x := 0 to FDeviceList.Count - 1 do
    begin
        if (FDeviceList.Items[x].DType = dptPump) then
            fPump := FDeviceList.Items[x] as TSwitchDevice;
        if (FDeviceList.Items[x].DType = dptVacuum) then
            fVacuum := FDeviceList.Items[x] as TSwitchDevice;
        if (FDeviceList.Items[x].DType = dptBlower) then
            fBlower := FDeviceList.Items[x] as TSwitchDevice;
        if (FDeviceList.Items[x] is TVarRediMotorDevice) then
            fMotor := FDeviceList.Items[x] as TVarRediMotorDevice;
        if (FDeviceList.Items[x].DType = dptAnySwitch) then
            fFeeding := FDeviceList.Items[x] as TSwitchDevice;
    end;
end;

procedure TRediDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    inherited;

    // sub-devices laden
    LoadDevices();

    // schreiben der Daten
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'REDI');
    if Assigned(fPump) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Pump', fPump.Name);
    if Assigned(fVacuum) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Vacuum', fVacuum.Name);
    if Assigned(fBlower) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Blower', fBlower.Name);
    if Assigned(fMotor) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Motor', fMotor.Name);
    if Assigned(fFeeding) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Feeding', fFeeding.Name);
end;

{ TMotorBasedMotionDevice }

constructor TMotorBasedMotionDevice.Create(aName: string);
begin
    inherited Create(aName, dptCmpMotorMotion);
    fOwnsYMotors := false;
    fYMotors := nil;
    fOwnsZMotors := false;
    fZMotors := nil;
end;

destructor TMotorBasedMotionDevice.Destroy();
begin
    if fOwnsYMotors then
        fYMotors.Free;

    if fOwnsZMotors then
        fZMotors.Free;
    inherited;
end;

procedure TMotorBasedMotionDevice.LoadDevices(aSettingDP: TDataProvider; const aDevArea: string);
begin
    self.XMotor := FindXMotor();
    if Assigned(self.XMotor) then
        self.XMotor.ExecID := self.ExecID;

    self.YMotors := FindYMotors();
    if Assigned(self.YMotors) then
        self.YMotors.LoadDevices();

    if not Assigned(self.YMotors) then
        self.SetYMotor(aSettingDP, aDevArea, FindYMotor());

    self.YMotors.ExecID := self.ExecID;

    self.ZMotors := FindZMotors();
    if Assigned(self.ZMotors) then
        self.ZMotors.LoadDevices();

    if not Assigned(self.ZMotors) then
        self.SetZMotor(aSettingDP, aDevArea, FindZMotor());

    self.ZMotors.ExecID := self.ExecID;

    self.RMotor := FindRMotor();
    if Assigned(self.RMotor) then
        self.RMotor.ExecID := self.ExecID;
end;

function TMotorBasedMotionDevice.GetNewZTravelValue: TPosMM;
begin
    result := 0;
    if Assigned(self.ZMotors) then
    begin
        result := self.ZMotors.GetNewZTravelValue;
    end;
end;

function TMotorBasedMotionDevice.GetStepsPerUnit: extended;
begin
    result := 1;
    if Assigned(self.ZMotors) then
    begin
        result := self.ZMotors.GetStepsPerUnit;
    end;
end;

function TMotorBasedMotionDevice.GetYMotor: TYMotorDevice;
begin
    result := nil;
    if not(fYMotors is TSingleMultiYMotorDevice) then
        EXIT;
    result := (fYMotors as TSingleMultiYMotorDevice).YMotorDevice;
end;

function TMotorBasedMotionDevice.GetZMotor: TZMotorDevice;
begin
    result := nil;
    if not(fZMotors is TSingleMultiZMotorDevice) then
        EXIT;
    result := (fZMotors as TSingleMultiZMotorDevice).ZMotorDevice;
end;

procedure TMotorBasedMotionDevice.SetYMotor(aSettingDP: TDataProvider; const aDevArea: string;
    aYMotor: TYMotorDevice);
begin
    if not Assigned(aYMotor) then
        EXIT;
    fYMotors := TSingleMultiYMotorDevice.Create(aSettingDP, aDevArea, aYMotor.Name, aYMotor);
    fOwnsYMotors := true;
end;

procedure TMotorBasedMotionDevice.SetZMotor(aSettingDP: TDataProvider; const aDevArea: string;
    aZMotor: TZMotorDevice);
begin
    if not Assigned(aZMotor) then
        EXIT;
    fZMotors := TSingleMultiZMotorDevice.Create(aSettingDP, aDevArea, aZMotor.Name, aZMotor.Name, aZMotor);
    fOwnsZMotors := true;
end;

procedure TMotorBasedMotionDevice.WriteDeviceData(aSettingDP: TDataProvider);
var
    xYMotorsName, xZMotorsName: string;
begin
    inherited;

    xYMotorsName := fYMotors.Name;
    if (fOwnsYMotors) and (fYMotors is TSingleMultiYMotorDevice) then
    begin
        xYMotorsName := fName + 'YMotors';
        (fYMotors as TSingleMultiYMotorDevice).MultiY_WriteDeviceData(aSettingDP, xYMotorsName);
    end;

    xZMotorsName := fZMotors.Name;
    if (fOwnsZMotors) and (fZMotors is TSingleMultiZMotorDevice) then
    begin
        xZMotorsName := fName + 'ZMotors';
        (fZMotors as TSingleMultiZMotorDevice).MultiZ_WriteDeviceData(aSettingDP, xZMotorsName);
    end;

    // schreiben der Daten
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'MOTORBASEDMOTION');

    if Assigned(fXMotor) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'XMotor', fXMotor.Name);
    if Assigned(fYMotors) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'YMotors', xYMotorsName);
    if Assigned(fZMotors) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'ZMotors', xZMotorsName);
    if Assigned(fRMotor) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'RMotor', fRMotor.Name);
end;

function TMotorBasedMotionDevice.FindXMotor: TXMotorDevice;
var
    xDevice: TDevice;
begin
    xDevice := nil;
    result := nil;
    self.Find_ByClass(xDevice, TXMotorDevice);
    if not Assigned(xDevice) then
        EXIT;
    result := xDevice as TXMotorDevice;
end;

function TMotorBasedMotionDevice.FindYMotor: TYMotorDevice;
var
    xDevice: TDevice;
begin
    xDevice := nil;
    result := nil;
    self.Find_ByClass(xDevice, TYMotorDevice);
    if not Assigned(xDevice) then
        EXIT;
    result := xDevice as TYMotorDevice;
end;

function TMotorBasedMotionDevice.FindZMotor: TZMotorDevice;
var
    xDevice: TDevice;
begin
    xDevice := nil;
    result := nil;
    self.Find_ByClass(xDevice, TZMotorDevice);
    if not Assigned(xDevice) then
        EXIT;
    result := xDevice as TZMotorDevice;
end;

function TMotorBasedMotionDevice.FindRMotor: TRMotorDevice;
var
    xDevice: TDevice;
begin
    xDevice := nil;
    result := nil;
    self.Find_ByClass(xDevice, TRMotorDevice);
    if not Assigned(xDevice) then
        EXIT;
    result := xDevice as TRMotorDevice;
end;

function TMotorBasedMotionDevice.FindYMotors: TMultiYMotorDevice;
var
    xDevice: TDevice;
begin
    xDevice := nil;
    result := nil;
    self.Find_ByClass(xDevice, TMultiYMotorDevice);
    if not Assigned(xDevice) then
        EXIT;
    result := xDevice as TMultiYMotorDevice;
end;

function TMotorBasedMotionDevice.FindZMotors: TMultiZMotorDevice;
var
    xDevice: TDevice;
begin
    xDevice := nil;
    result := nil;
    self.Find_ByClass(xDevice, TMultiZMotorDevice);
    if not Assigned(xDevice) then
        EXIT;
    result := xDevice as TMultiZMotorDevice;
end;

const
    INT_DISTANCE_UNKNWOWN = -1;

constructor TGripDevice.Create(aName: string; aDType: TDeviceType);
begin
    inherited Create(aName, aDType);
end;

procedure TGripDevice.LoadDevices();
begin
end;

{ TMotorGripDevice }

constructor TMotorGripDevice.Create(const aName, aSection: string);
begin
    inherited Create(aName, dptCmpMotorGrip);
end;

procedure TMotorGripDevice.LoadDevices();
var
    xDevice: TDevice;
begin
    xDevice := nil;
    self.Find_ByClass(xDevice, TGMotorDevice);
    if Assigned(xDevice) then
    begin
        fGMotor := xDevice as TGMotorDevice;
    end;
end;

procedure TMotorGripDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    inherited;

    // schreiben der Daten
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'MOTORGRIP');
    if Assigned(fGMotor) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Motor', fGMotor.Name);
end;

{ TPneumaticGripDistanceSwitchDevice }

constructor TPneumaticGripDistanceSwitchDevice.Create(aSettingDP: TDataProvider;
    const aDevArea, aName, aAreaName: string; aDType: TDeviceType; aAdr, aCom: integer;
    const aBit, aDefaultBit, aDllName: string);
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(aSettingDP, aDevArea, aName, aAreaName, aDType, aAdr, aCom, aBit, aDefaultBit, aDllName);
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    fDistance := xIniAccess.ReadFloat(aName, 'Distance', -1);
end;

procedure TPneumaticGripDistanceSwitchDevice.WriteDeviceData(aSettingDP: TDataProvider);
var
    xDriverName: string;
begin
    xDriverName := fName + 'Driver';

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type',
        'PNUEMATICGRIPDISTSWITCH');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Driver', xDriverName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'RackName', fRackName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Delay', IntToStr(fDelay));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Distance',
        FloatToStr(fDistance));

    fIntf.Switch_WriteDriverData(aSettingDP, xDriverName, fAdr, fBit, fDefaultBit, fDelayedBit, fReverse,
        fSwitchOffDelay, fSwitchOnDelay, fNeverResetAtInit, fNeverResetAtReset);
end;

{ TPneumaticGripDevice }

constructor TPneumaticGripDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aSection: string);
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(aName, dptCmpPneumaticGrip);
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    fMin := xIniAccess.ReadFloat(aSection, 'Min', -1);
    fMax := xIniAccess.ReadFloat(aSection, 'Max', -1);
    fCloseDelay := xIniAccess.ReadInteger(aSection, 'CloseDelay', 0);
    fOpenDelay := xIniAccess.ReadInteger(aSection, 'OpenDelay', 0);
    fOpenCloseSwitch := nil;
end;

procedure TPneumaticGripDevice.LoadDevices();
var
    i: integer;
    xDistSwitchCount: integer;
begin
    xDistSwitchCount := 0;
    for i := 0 to fDeviceList.Count - 1 do
    begin
        if (fDeviceList.Items[i] is TPneumaticGripOpenCloseSwitchDevice) then
        begin
            fOpenCloseSwitch := fDeviceList.Items[i] as TPneumaticGripOpenCloseSwitchDevice;
            CONTINUE;
        end;
        if (fDeviceList.Items[i] is TPneumaticGripDistanceSwitchDevice) then
        begin
            inc(xDistSwitchCount);
            case xDistSwitchCount of
                1:
                    fDistSwitch1 := fDeviceList.Items[i] as TPneumaticGripDistanceSwitchDevice;
                2:
                    fDistSwitch2 := fDeviceList.Items[i] as TPneumaticGripDistanceSwitchDevice;
                3:
                    fDistSwitch3 := fDeviceList.Items[i] as TPneumaticGripDistanceSwitchDevice;
            end;
        end;
    end;
end;

procedure TPneumaticGripDevice.WriteDeviceData(aSettingDP: TDataProvider);
const
    INT_MAX_DISTSWITCH = 3;
begin
    inherited;

    // schreiben der Daten
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'PNUEMATICGRIP');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Min', FloatToStr(fMin));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Max', FloatToStr(fMax));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'CloseDelay',
        IntToStr(fCloseDelay));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'OpenDelay',
        IntToStr(fOpenDelay));
    if Assigned(fOpenCloseSwitch) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'OpenCloseSwitch',
            fOpenCloseSwitch.Name);
    if Assigned(fDistSwitch1) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName,
            'PneumaticDistanceSwitchDevice1', fDistSwitch1.Name);
    if Assigned(fDistSwitch2) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName,
            'PneumaticDistanceSwitchDevice2', fDistSwitch2.Name);
    if Assigned(fDistSwitch3) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName,
            'PneumaticDistanceSwitchDevice3', fDistSwitch3.Name);
end;

function TPneumaticGripDevice.DetermineClosestDestFromDistSwitch(aDest: TPosMM; aDefault: TPosMM;
    aDirection: integer): TPosMM;
var
    i: integer;
    xSwitchDist: TPosMM;
begin
    result := aDefault;
    for i := 0 to fDeviceList.Count - 1 do
    begin
        if not(fDeviceList.Items[i] is TPneumaticGripDistanceSwitchDevice) then
            CONTINUE;
        xSwitchDist := (fDeviceList.Items[i] as TPneumaticGripDistanceSwitchDevice).Distance;
        if ((aDirection < 0) and (aDest < xSwitchDist)) or ((aDirection > 0) and (aDest > xSwitchDist)) then
            CONTINUE;

        if (aDirection < 0) then
        begin
            if (xSwitchDist > result) then
                result := xSwitchDist;
        end
        else if (aDirection > 0) then
        begin
            if (xSwitchDist < result) then
                result := xSwitchDist;
        end;
    end;
end;

function TPneumaticGripDevice.DetermineClosestDest(aDest: TPosMM; aDirection: integer): TPosMM;
begin
    if aDirection < 0 then
    begin
        result := fMin;
        if (aDest <> INT_DISTANCE_UNKNWOWN) and (aDest >= fMax) then
        begin
            result := fMax;
            EXIT;
        end;
    end
    else
    begin
        result := fMax;
        if (aDest <> INT_DISTANCE_UNKNWOWN) and (aDest <= fMin) then
        begin
            result := fMin;
            EXIT;
        end;
    end;

    if (aDest <> INT_DISTANCE_UNKNWOWN) then
        result := DetermineClosestDestFromDistSwitch(aDest, result, aDirection);
end;

// --------------------------------------------------------------------------------------------------
// TPipPumpDevice -> TDispenserDevice -> TLeafDevice -> TDevice
// --------------------------------------------------------------------------------------------------
constructor TPipPumpDevice.Create(aSettingDP: TDataProvider;
    const aDevArea, aName, aAreaName, aDllName: string; aAdr, aBit: integer; aTipIndex: integer);
var
    xIniAccess: ISimpleIniAccess;
    x: integer;

begin
    inherited Create(aName, aAreaName, dptPipPump);

    fCurrentRoundedVolumeRest := 0;
    FTipIndex := aTipIndex;
    fAdr := aAdr;
    fBit := aBit;
    fDllName := aDllName;

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    fInputPortName := xIniAccess.ReadString(aName, 'SysInput', '');
    fMaxVolSteps := xIniAccess.ReadInteger(aName, 'MaxMotorStep', 0);
    fMaxVol_uL := xIniAccess.ReadInteger(aName, 'MaxVolume', 0);
    fAspSpeed := xIniAccess.ReadInteger(aName, 'AspSpeed', 0);
    fAspRamp := xIniAccess.ReadInteger(aName, 'AspRamp', 0);
    fDispSpeed := xIniAccess.ReadInteger(aName, 'DispSpeed', 0);
    fDispRamp := xIniAccess.ReadInteger(aName, 'DispRamp', 0);
    fScalingFactor := xIniAccess.ReadInteger(aName, 'ScalingFactor', 0);
    for x := 0 to high(fValvePos) do
        fValvePos[x] := xIniAccess.ReadInteger(aName, 'ValvePos' + IntToStr(x + 1), 0);
end;

procedure TPipPumpDevice.LoadDevice(aSettingDP: TDataProvider; const aDevArea: string);
var
    xMaxVol_ul: extended;
begin
    FIntf := TPipPumpDeviceFactory.CreateintfAsPipPump(aSettingDP, fName, fDType, fDllName, fAdr, fBit,
        fAspSpeed, fAspRamp, fDispSpeed, fDispRamp, fValvePos, fMaxVol_uL, fMaxVolSteps,
        fScalingFactor, fExecID);

    fVolULToStepFactor := 1;
    try
        xMaxVol_ul := fMaxVol_uL;
        if xMaxVol_ul > 0 then
            fVolULToStepFactor := fMaxVolSteps / xMaxVol_ul;
    except
    end;
end;

function TPipPumpDevice.VolumeULToVolumeSteps(aVolumeUL: extended): extended;
begin
    try
        result := aVolumeUL * fVolULToStepFactor; // an dieser Stelle kein Runden!!
    except
        result := 0;
    end;
end;

procedure TPipPumpDevice.WriteDeviceData(aSettingDP: TDataProvider);
var
    xDriverName: string;
begin
    xDriverName := fName + 'Driver';
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'SIMPLEPIPPUMP');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'TipNo', IntToStr(fTipIndex + 1));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'SysInput', fInputPortName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Driver', xDriverName);

    fIntf.PipPump_WriteDriverData(aSettingDP, xDriverName, fExecID, fAdr, fMaxVol_uL, fMaxVolSteps, fAspSpeed,
        fAspRamp, fDispSpeed, fDispRamp, fScalingFactor, fValvePos);
end;

{ TPipPumpDeviceFactory }

class function TPipPumpDeviceFactory.CreatePipPumpDevice(aSettingDP: TDataProvider;
    const aDevArea, aName, aAreaName, aDllName: string; aAdr, aBit, aTipIndex: integer): TPipPumpDevice;
begin
    result := TPipPumpDevice.Create(aSettingDP, aDevArea, aName, aAreaName, aDllName, aAdr, aBit, aTipIndex);
end;

class function TPipPumpDeviceFactory.CreateIntfAsPipPump(aSettingDP: TDataProvider; const aName: string;
    aDevType: TDeviceType; const aDLLName: string; aAdr, aBit: integer;
    aAspSpeed, aAspRamp, aDispSpeed, aDispRamp: integer; aValvePos: TVPosArray;
    aMaxVol_uL, aMaxVolSteps, aScalingFactor: integer; aExecID: integer): IIntfPipPump;
begin
    result := gCommManager.AddPipPump(aSettingDP, aName, aDevType, aDLLName, aAdr, aBit, aAspSpeed, aAspRamp,
        aDispSpeed, aDispRamp, aValvePos, aMaxVol_uL, aMaxVolSteps, aScalingFactor, aExecID);

end;

constructor TPipDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aSection: string;
    aDevType: TDeviceType);
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(aName, dptCmpPipette);

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    fTipCount := xIniAccess.ReadInteger(aSection, 'Tips', 0);

    fUsesAddDilutors := false;
end;

procedure TPipDevice.LoadDevices(aSettingDP: TDataProvider; const aDevArea: string);
var
    x: integer;
begin

    // alle zusätzlichen Dilutoren werden initialisiert
    for x := 0 to FDeviceList.Count - 1 do
    begin
        if (FDeviceList.Items[x] is TPipPumpDevice) then
        begin
            (FDeviceList.Items[x] as TPipPumpDevice).ExecID := fPipPumpsExecID;
            (FDeviceList.Items[x] as TPipPumpDevice).LoadDevice(aSettingDP, aDevArea);
        end;
    end;
end;

constructor TAttachedPipDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aSection: string);
begin
    inherited Create(aSettingDP, aDevArea, aName, aSection, dptCmpPipette);

end;

procedure TAttachedPipDevice.WriteDeviceData(aSettingDP: TDataProvider);
const
    cMaxPumpCount = 16;
var
    xDev, xTipIndex, xDilIndex: integer;
begin
    inherited;

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'ATTACHEDPIPETTE');

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'RackNameSuffix',
        self.RackNamePrefix); // ?????
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Tips', IntToStr(self.TipCount));

    for xTipIndex := 0 to 7 do
    begin
        xDilIndex := 0;
        for xDev := 0 to FDeviceList.Count - 1 do
        begin
            if (FDeviceList.Items[xDev] is TPipPumpDevice) and
                ((FDeviceList.Items[xDev] as TPipPumpDevice).TipIndex = xTipIndex) then
            begin

                if (xDilIndex = 0) then
                    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName,
                        'Pump' + IntToStr(xTipIndex + 1), FDeviceList.Items[xDev].Name);
                if (xDilIndex = 1) then
                    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName,
                        'Pump' + IntToStr(self.TipCount + xTipIndex + 1), FDeviceList.Items[xDev].Name);
                inc(xDilIndex);
            end;
        end;
    end;
end;

{ TRemovablePipDevice }

constructor TRemovablePipDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aSection: string);
begin
    inherited Create(aSettingDP, aDevArea, aName, aSection, dptCmpRemPipette);

end;

procedure TRemovablePipDevice.WriteDeviceData(aSettingDP: TDataProvider);
var
    xDev, xDilIndex: integer;
begin
    inherited;

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'REMPIPETTE');

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'RackNameSuffix',
        self.RackNamePrefix); // ?????
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Tips', IntToStr(self.TipCount));

    xDilIndex := 0;
    for xDev := 0 to FDeviceList.Count - 1 do
    begin
        if (FDeviceList.Items[xDev] is TPipPumpDevice) then
        begin

            TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName,
                'Pump' + IntToStr(xDilIndex), FDeviceList.Items[xDev].Name);
        end;
    end;
end;

{ TVarRediMotorDevice }

constructor TVarRediMotorDevice.Create(aName, aAreaName: string; aCom: integer);
begin
    inherited Create(aName, aAreaName, dptVarRediMotor);

    fComPort := aCom;
end;

procedure TVarRediMotorDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    // ist immer Peerless 1:1, also brauche ich fIntf nicht

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'VARREDIMOTOR');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Driver', fName + 'Driver');

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', fName + 'Driver', 'Type',
        'Varix01VolumeMotor');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', fName + 'Driver', 'Connection',
        fName + 'Connection');

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fName + 'Connection', 'Type',
        'Varix01Connection');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fName + 'Connection', 'ComPort',
        IntToStr(FComPort));
end;

const
    INT_INTERVAL_IN_S_MIN = 5;
    // the interval should be atleast 5 seconds, otherwise we are going to take up too much cpu time
    INT_INTERVAL_IN_S_DEFAULT = 10;
    INT_TRIGGER_RESET_DIVIDER = 5; // we want to try to trigger after 1/5th of the interval time has passed

    { TSimpleWatchDogDevice }

constructor TSimpleWatchDogDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aSection: string;
    aDevType: TDeviceType);
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(aName, aSection, aDevType);
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    fIntervalInS := xIniAccess.ReadInteger(aName, 'Interval', INT_INTERVAL_IN_S_DEFAULT);
    if fIntervalInS < INT_INTERVAL_IN_S_MIN then
        fIntervalInS := INT_INTERVAL_IN_S_MIN;

    fDllName := xIniAccess.ReadString(aName, STR_DEV_INI_IDENT_DLLNAME, '');
    fPort := xIniAccess.ReadInteger(aName, 'Port', -1);
    fIntf := gCommManager.AddWatchDog(fDllName, fPort, fIntervalInS);
    fWaitTimeInMS := Trunc((fIntervalInS * 1000) / INT_TRIGGER_RESET_DIVIDER);
end;

procedure TSimpleWatchDogDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'SIMPLEWATCHDOG');

    TLogManager.Instance.Log('WARNING: Device ' + fName +
        ' is a SimpleWatchDogDevice. There is no driver implemented!', 1);
end;

{ TSoftwareProtectionDevice }

constructor TSoftwareProtectionDevice.Create(aName: string; aSectionName: string);
begin
    inherited Create(aName, dptCmpSoftwareProtect);
    fDevLoaded := false;
    fWatchDogDevice := nil;
end;

procedure TSoftwareProtectionDevice.LoadDevices;
var
    x: integer;
begin
    if fDevLoaded then
        EXIT;
    for x := 0 to FDeviceList.Count - 1 do
        if (FDeviceList.Items[x] is TWatchDogDevice) then
        begin
            fWatchDogDevice := FDeviceList.Items[x] as TWatchDogDevice;
        end;

    fDevLoaded := true;
end;

procedure TSoftwareProtectionDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    inherited;

    // sub-devices laden
    LoadDevices();

    // schreiben der Daten
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'SIMPLESOFTWAREPROTECT');
    if Assigned(fWatchDogDevice) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'WatchDogDevice',
            fWatchDogDevice.Name);
end;

{ TXWayValveDevice }

constructor TXWayValveDevice.Create(aName, aAreaName: string; aDType: TDeviceType);
begin
    inherited;

    fInputPortNames := TStringList.Create;
end;

destructor TXWayValveDevice.Destroy;
begin
    fInputPortNames.Free;

    inherited;
end;

function TXWayValveDevice.GetInputPortCount: integer;
begin
    result := fInputPortNames.Count;
end;

function TXWayValveDevice.GetInputPortName(index: Integer): string;
begin
    result := fInputPortNames[index];
end;

procedure TXWayValveDevice.ReadInputPorts(aSettingDP: TDataProvider; const aDevArea: string;
    aNoOfPorts: integer);
var
    x: integer;
    xIniAccess: ISimpleIniAccess;
    xIdent: string;
begin
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);

    for x := 0 to aNoOfPorts - 1 do
    begin
        xIdent := xIniAccess.ReadString(fName, 'In' + IntToStr(x + 1), '');
        if (xIdent = '') then
            xIdent := TLiquids.EncodeSystemLiquidIdent(x);
        fInputPortNames.Add(xIdent);
    end;
end;

procedure TXWayValveDevice.GetPortNames(aPortNames: TStrings);
begin
    aPortNames.AddStrings(fInputPortNames);
end;

{ T6WayValveDevice }

constructor T6WayValveDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
    aCom: integer);
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(aName, aAreaName, dptXWayValve);

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);

    FComPortNo := aCom;
    FTimeout := xIniAccess.ReadInteger(aName, 'Timeout', 20);
    FSleepTime := xIniAccess.ReadInteger(aName, 'SleepTime', 500);
    FRetryCnt := xIniAccess.ReadInteger(aName, 'RetryCnt', 5);
    fResetSwitchDevice := xIniAccess.ReadString(aName, 'ResetSwitchDevice', '');

    ReadInputPorts(aSettingDP, aDevArea, 6)
end;

constructor T6WayValveDevice.CreateOldStyle(aName, aAreaName: string;
    aCom, aTimeout, aSleepTime, aRetryCnt: integer; aInPort6Name: string);
begin
    inherited Create(aName, 'LiquidSystem', dptXWayValve);

    FComPortNo := aCom;
    FTimeout := aTimeout;
    FSleepTime := aSleepTime;
    FRetryCnt := aRetryCnt;
    fResetSwitchDevice := '';

    fInputPortNames.Add(TLiquids.EncodeSystemLiquidIdent(0));
    fInputPortNames.Add(TLiquids.EncodeSystemLiquidIdent(1));
    fInputPortNames.Add(TLiquids.EncodeSystemLiquidIdent(2));
    fInputPortNames.Add(TLiquids.EncodeSystemLiquidIdent(3));
    fInputPortNames.Add(TLiquids.EncodeSystemLiquidIdent(4));
    fInputPortNames.Add(aInPort6Name);
end;

procedure T6WayValveDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    try
        // ist immer Omnifit 1:1, also brauche ich fIntf nicht

        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'COMPACTXWAYVALVE');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Driver', fName + 'Driver');

        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', fName + 'Driver', 'Type',
            'XWAYVALVE01XWAYVALVE');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', fName + 'Driver', 'Connection',
            fName + 'Connection');

        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fName + 'Connection', 'Type',
            'XWAYVALVE01CONNECTION');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fName + 'Connection', 'ComPort',
            IntToStr(FComPortNo));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fName + 'Connection',
            'ComPortTimeout', IntToStr(FTimeout));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fName + 'Connection',
            'SleepTime', IntToStr(FSleepTime));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fName + 'Connection', 'Retry',
            IntToStr(FRetryCnt));

        if (fResetSwitchDevice <> '') then
            TLogManager.Instance.Log('WARNING: Device ' + fName +
                ', Parameter ResetSwitchDevice is not implemented in the current version!', 1);
    except

    end;
end;

{ TXWayValveSwitchDevice }

constructor TXWayValveSwitchDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
    aAdr, aCom: integer; const aBit, aDllName: string);
var
    xProtocol: TProtocol;
begin
    inherited Create(aName, aAreaName, dptXWayValve);

    fAdr := aAdr;
    fBit := aBit;
    xProtocol := Settings_ReadProtocol(aSettingDP, aDevArea);
    fIntf := gCommManager.AddSwitch(fAdr, aCom, fBit, aDllName, xProtocol);

    ReadInputPorts(aSettingDP, aDevArea, 2)
end;

constructor TXWayValveSwitchDevice.CreateOldStyle(aSettingDP: TDataProvider;
    const aDevArea, aName, aAreaName: string; aCom, aBit: integer; aInPort1Name, aInPort2Name: string);
var
    xProtocol: TProtocol;
begin
    inherited Create(aName, aAreaName, dptXWayValve);

    fAdr := -1;
    fBit := IntToStr(aBit);
    xProtocol := Settings_ReadProtocol(aSettingDP, aDevArea);
    fIntf := gCommManager.AddSwitch(fAdr, aCom, fBit, '', xProtocol);

    fInputPortNames.Add(aInPort1Name);
    fInputPortNames.Add(aInPort2Name);
end;

procedure TXWayValveSwitchDevice.WriteDeviceData(aSettingDP: TDataProvider);
var
    xDriverName: string;
begin
    xDriverName := fName + 'Driver';
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'SWITCHXWAYVALVE');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Driver', xDriverName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Delay', IntToStr(fDelay));

    fIntf.Switch_WriteDriverData(aSettingDP, xDriverName, fAdr, fBit, STR_DEV_INI_DEFAULT_BIT_UNDEFINED,
        STR_DEV_INI_DEFAULT_BIT_UNDEFINED, false, 0, 0, false, false);
end;

// --------------------------------------------------------------------------------------------------
// TSimpleShakerDevice -> TShakerDevice -> TDisplayableDevice -> TLeafDevice -> TDevice
// --------------------------------------------------------------------------------------------------
constructor TSimpleShakerDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
    aDType: TDeviceType; aAdr, aCom: integer; const aBit, aMagnetBit, aDllName: string);
var
    xIniAccess: ISimpleIniAccess;
    xProtocol: TProtocol;
begin
    inherited Create(aName, aAreaName, aDType);

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    fSwitchOffDelay := xIniAccess.ReadInteger(aName, 'SwitchOffDelay', 10);
    fReverse := xIniAccess.ReadBool(aName, 'Off', false);
    fMagnetReverse := xIniAccess.ReadBool(aName, 'MagnetOff', false);

    fAdr := aAdr;
    fBit := aBit;
    fDelayedBit := aMagnetBit;

    xProtocol := Settings_ReadProtocol(aSettingDP, aDevArea);
    fShaking := false;
    FIntf := gCommManager.AddSwitch(aAdr, aCom, aBit, aDllName, xProtocol);
    gCommManager.AddSwitch(aAdr, aCom, xIniAccess.ReadString(aName, 'MagnetPort',
        STR_DEV_INI_DEFAULT_BIT_UNDEFINED), aDllName, xProtocol)
end;

procedure TSimpleShakerDevice.WriteDeviceData(aSettingDP: TDataProvider);
// var
// xDriverName: string;
begin
    TLogManager.Instance.Log('WARNING: Device ' + fName +
        ', Type SimpleShakerDevice is not implemented in the current version!', 1);

    { xDriverName := fName + 'Driver';
      TSettingsTableProviderUtilsV0.AppendRecord( aSettingDP, 'DEVICE', fName, 'Type', 'SWITCH' );
      TSettingsTableProviderUtilsV0.AppendRecord( aSettingDP, 'DEVICE', fName, 'Driver', xDriverName );
      TSettingsTableProviderUtilsV0.AppendRecord( aSettingDP, 'DEVICE', fName, 'RackName', fRackName 1);
      TSettingsTableProviderUtilsV0.AppendRecord( aSettingDP, 'DEVICE', fName, 'Delay', IntToStr( fDelay ) );

      fIntf.Switch_WriteDriverData( aSettingDP, xDriverName, fAdr, fBit, '', '',
      false, 0, 0, false, false );
    }
end;

// --------------------------------------------------------------------------------------------------
// TSpeedShakerDevice -> TShakerDevice -> TDisplayableDevice -> TLeafDevice -> TDevice
// --------------------------------------------------------------------------------------------------
constructor TSpeedShakerDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aAreaName: string;
    aDType: TDeviceType; aCom, aBit: integer);
var
    xProtocol: TProtocol;
    xIniAccess: ISimpleIniAccess;
    xSleepTime: Integer;
begin
    inherited Create(aName, aAreaName, aDType);

    fBit := aBit;
    xProtocol := cdtNone;
    if (aDType = dptShakerCAT) then
        xProtocol := cdtCATVortexer;
    if (aDType = dptShakerIKA) then
        xProtocol := cdtIKAVortexer;

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    xSleepTime := xIniAccess.ReadInteger(aName, 'SleepTime', 400);
    fIntf := gCommManager.AddShaker(aCom, fBit, xProtocol, xSleepTime);

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, 'APPLICATION');
    fFixAfterVortexing := xIniAccess.ReadBool('Schedule', 'FixAfterVortexing', false);
end;

procedure TSpeedShakerDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'MIXER');

    // FixAfterVortexing war in alten 7.4.x-Versionen falschrum - deshalb NOT fFixAfterVortexing
    // mit dem Updater TSettingsTableUpdateV2_6.ConvertFixAfterVortexingSetting wird das hinterher korrigiert!
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'FixAfterVortexing',
        TAppSettings.WriteBoolToStr(not fFixAfterVortexing));

    fIntf.Mixer_WriteDriverData(aSettingDP, fBit, fName);
end;

// --------------------------------------------------------------------------------------------------
// TThermoDevice -> TDisplayableDevice -> TLeafDevice -> TDevice
// --------------------------------------------------------------------------------------------------
constructor TThermoDevice.Create(aSettingDP: TDataProvider;
    const aDevArea, aName, aAreaName, aTypeName: string; aAdr, aCom, aBit: integer);
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(aName, aAreaName, dptThermostat);

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);

    fDeviceData.Adr := aAdr;
    fDeviceData.Com := aCom;
    fDeviceData.Port := aBit;
    fDeviceData.ReadPort := xIniAccess.ReadInteger(aName, 'ReadPort', -1);
    fDeviceData.Protocol := self.Settings_ReadProtocol(aSettingDP, aDevArea);
    fDeviceData.SleepTime := xIniAccess.ReadInteger(aName, 'SleepTime', 400);

    if (fDeviceData.Protocol = cdtNone) then
    begin
        if (Pos(STR_LEAFDEVICE_OLD_COOL, UpperCase(aTypeName)) = 1) then
            fDeviceData.Protocol := cdtHuber;
        if (Pos(STR_LEAFDEVICE_OLD_HEAT, UpperCase(aTypeName)) = 1) then
            fDeviceData.Protocol := cdtCATVortexer;
    end;

    fIntf := gCommManager.AddHeater(aSettingDP, aDevArea, fDeviceData);
end;

procedure TThermoDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'THERMO');

    fIntf.Thermostat_WriteDriverData(aSettingDP, fName, fDeviceData);
end;

// --------------------------------------------------------------------------------------------------
// TIntfCommunicationList
// --------------------------------------------------------------------------------------------------
constructor TIntfCommunicationList.Create;
begin
    inherited Create;
    FList := TObjectList<TIntfCommunication>.Create;
    FList.OwnsObjects := false;
end;

destructor TIntfCommunicationList.Destroy;
begin
    FList.Free;
    inherited Destroy;
end;

function TIntfCommunicationList.GetCount: integer;
begin
    result := FList.Count;
end;

function TIntfCommunicationList.GetItem(aIndex: Integer): TIntfCommunication;
begin
    result := FList.Items[aIndex] as TIntfCommunication;
end;

procedure TIntfCommunicationList.SetOwnsObjects(aOwnsObjects: boolean);
begin
    FList.OwnsObjects := aOwnsObjects;
end;

function TIntfCommunicationList.Add(aItem: TIntfCommunication): Integer;
begin
    result := FList.Add(aItem);
end;

// --------------------------------------------------------------------------------------------------
// TCommunicationManager
// --------------------------------------------------------------------------------------------------
constructor TCommunicationManager.Create(aSettingDP: TDataProvider);
begin
    inherited Create;
    FIsSafeExitRequested := false;
    fSimulationSpeed_Percent := 0;

    // read conrad port from ini
    FConradBoardPort := TIniSettings.ReadInteger(aSettingDP, 'ROBOT', 'ConradRelayBoard', 'ComPort', -1);

    FCommList := TIntfCommunicationList.Create;
end;

destructor TCommunicationManager.Destroy;
begin
    FCommList.Free;

    inherited;
end;

class function TCommunicationManager.GetBalanceProtocolFromType(aType: string): TProtocol;
begin
    // Typbestimmung
    result := btMettlerAT;
    if (pos('AX', aType) > 0) then
        result := btMettlerAX;
    if (pos('GENIUS', uppercase(aType)) > 0) then
        result := btSartGenius;
    if (pos('GENIUSB', uppercase(aType)) > 0) then
        result := btSartGeniusBin;
    if (pos('GENIUSCELLB', uppercase(aType)) > 0) then
        result := btSartGeniusCellBin;
end;

function TCommunicationManager.FindCommPort(aSerialPort: integer): TIntfCommPort;
var
    x: integer;
begin
    result := nil;

    // find communication port in list
    for x := 0 to FCommList.Count - 1 do
        if (FCommList.Items[x] is TIntfCommPort) and
            ((FCommList.Items[x] as TIntfCommPort).ComPortNo = aSerialPort) then
            result := FCommList.Items[x] as TIntfCommPort;
end;

function TCommunicationManager.FindCommDll(aDllName: string): TIntfDll;
var
    x: integer;
begin
    result := nil;

    // find communication port in list
    for x := 0 to FCommList.Count - 1 do
        if (FCommList.Items[x] is TIntfDll) and ((FCommList.Items[x] as TIntfDll).DllName = aDllName) then
            result := FCommList.Items[x] as TIntfDll;
end;

function TCommunicationManager.GetDLLIntf(aDllName: string; aProtocol: TProtocol): TIntfDLL;
begin
    result := FindCommDll(aDllName);

    if (result = nil) then
    begin
        result := TIntfDll.Create(aDllName, cdtDLL);
        FCommList.Add(result);
    end;
end;

function TCommunicationManager.GetSimulationMode: boolean;
begin
    result := true; // im Editor immer Simulationsmodus!
end;

function TIntfCommunicationList.GetNextIndex: Integer;
begin
    result := count;
end;

// --------------------------------------------------------------------------------------------------
// TCommunicationRealManager --> TCommunicationManager
// --------------------------------------------------------------------------------------------------
constructor TCommunicationRealManager.Create(aSettingDP: TDataProvider);
var
    xInterface: TRoboticInterface;
    xSimulation: boolean;
begin
    inherited Create(aSettingDP);

    fCVortexSleepTime := TIniSettings.ReadInteger(aSettingDP, 'DEVICE', 'RS232', 'CVortexSleepTime', 10);
    fOpenFixationAtInit := TIniSettings.ReadBool(aSettingDP, 'APPLICATION', 'Schedule',
        'OpenFixationAtInit', false);

    xSimulation := true;
    if (TAppSettings.IsSias) then
        xInterface := TRoboticInterfaceZP02.Create(xSimulation)
    else
        xInterface := TRoboticInterfaceZP01.Create(xSimulation);
    // der Simulationsmodus kann hier gesetzt werden

    if (xInterface <> nil) then
        FCommList.Add(xInterface);
end;

function TCommunicationRealManager.AddBalance(aSettingDP: TDataProvider; aProtocol: TProtocol;
    var aModuleIndex, aComPort: integer; aName, aType: string; aDLLName: string): IIntfBalance;
var
    xIntf1: TIntfBalance;
begin
    if (aDLLName <> '') then
        xIntf1 := TIntfDllBalance.Create(aSettingDP, aDLLName, aName, aComPort)
    else
        xIntf1 := TIntfBalanceOld.Create(aSettingDP, aName, aProtocol, aComPort, true);

    aModuleIndex := 0;
    if Assigned(xIntf1) then
        FCommList.Add(xIntf1);
    result := xIntf1;
    // xIntf1.Balance_Init();
end;

function TCommunicationRealManager.CreateSwitchRelay(aCom: integer; const aBit, aDllName: string;
    aProtocol: TProtocol): IIntfSwitch;
var
    xRelay: TIntfRelay;
begin
    result := nil;
    if (aDllName <> '') then
    begin
        xRelay := TIntfDllRelayWithSensor.FindOrCreate(aDllName, aCom);
        if not Assigned(xRelay) then
            EXIT;
        FCommList.Add(xRelay);
        result := xRelay;
        EXIT;
    end;
    if (aCom = FConradBoardPort) or (aProtocol = cdtConradRelay) then
    begin
        xRelay := TIntfRelayConrad.Create(aCom, true);
        FCommList.Add(xRelay);
        result := xRelay;
    end;
    if (aProtocol = cdtKolterOpto) then
    begin
        xRelay := TIntfRelayKolter.Create(aCom, true);
        FCommList.Add(xRelay);
        result := xRelay;
    end;
    if (result = nil) then
    begin
        xRelay := TIntfRelayCompulab.Create(aCom, true); // wenn nicht definiert: Compulab (historisch)
        FCommList.Add(xRelay);
        result := xRelay;
    end;
end;

function TCommunicationRealManager.CreateSensorRelay(aCom: integer; const aBit, aDllName: string;
    aProtocol: TProtocol): IIntfSensor;
var
    xSignalLoop: TIntfSignalLoop;
    xRelayCompulab: TIntfRelayCompulab;
    xRelayKolter: TIntfRelayKolter;
    xDllRelay: TIntfDllRelayWithSensor;
begin
    result := nil;
    if (aDllName <> '') then
    begin
        xDllRelay := TIntfDllRelayWithSensor.FindOrCreate(aDllName, aCom);
        if not Assigned(xDllRelay) then
            EXIT;
        FCommList.Add(xDllRelay);
        result := xDllRelay;
        EXIT;
    end;
    if (aCom = FConradBoardPort) then
    begin
        EXIT;
    end;
    if (aProtocol = cdtSignalLoop) then
    begin
        xSignalLoop := TIntfSignalLoop.Create(aCom, true);
        FCommList.Add(xSignalLoop);
        result := xSignalLoop;
    end;
    if (aProtocol = cdtCompulab) then
    begin
        xRelayCompulab := TIntfRelayCompulab.Create(aCom, true);
        FCommList.Add(xRelayCompulab);
        result := xRelayCompulab;
    end;
    if (result = nil) then
    begin
        xRelayKolter := TIntfRelayKolter.Create(aCom, true); // wenn nicht definiert: Kolter
        FCommList.Add(xRelayKolter);
        result := xRelayKolter;
    end;
end;

function TCommunicationRealManager.AddRelaySwitch(aCom: integer; const aBit, aDllName: string;
    aProtocol: TProtocol): IIntfSwitch;
var
    xCommPort: TIntfCommPort;
begin
    result := nil;
    xCommPort := FindCommPort(aCom);

    if Assigned(xCommPort) then
    begin // COM-Port existiert bereits!
        if (xCommPort is TIntfRelay) then
        begin
            result := xCommPort as TIntfRelay;
        end
        else
        begin
            EXIT;
        end;
    end
    else
        result := CreateSwitchRelay(aCom, aBit, aDllName, aProtocol);
    // create communication port and add to list
end;

function TCommunicationRealManager.AddRelaySensor(aCom: integer; const aBit, aDllName: string;
    aProtocol: TProtocol): IIntfSensor;
var
    xCommPort: TIntfCommPort;
begin
    result := nil;
    xCommPort := FindCommPort(aCom);

    if Assigned(xCommPort) then
    begin // COM-Port existiert bereits!
        if (xCommPort is TIntfDllRelayWithSensor) then
        begin
            result := xCommPort as TIntfDllRelayWithSensor;
        end;
        if (xCommPort is TIntfRelayKolter) then
        begin
            result := xCommPort as TIntfRelayKolter;
        end;
        if (xCommPort is TIntfRelayCompulab) then
        begin
            result := xCommPort as TIntfRelayCompulab;
        end;
        if (xCommPort is TIntfSignalLoop) then
        begin
            result := xCommPort as TIntfSignalLoop;
        end;
        if (result = nil) then
        begin
            EXIT;
        end;
    end
    else
        result := CreateSensorRelay(aCom, aBit, aDllName, aProtocol);
    // create communication port and add to list
end;

function TCommunicationRealManager.AddSamplerSwitch(aAdr, aBit: integer): IIntfSwitch;
var
    xIntf: TRoboticInterfaceZP01;
begin
    if (FCommList.Items[0] is TRoboticInterfaceZP01) then
    begin
        xIntf := (FCommList.Items[0] as TRoboticInterfaceZP01);
        result := xIntf;
    end;
end;

function TCommunicationRealManager.AddSamplerSensor(aAdr, aBit: integer): IIntfSensor;
var
    xIntf: TRoboticInterfaceZP01;
begin
    if (FCommList.Items[0] is TRoboticInterfaceZP01) then
    begin
        xIntf := (FCommList.Items[0] as TRoboticInterfaceZP01);
        result := xIntf;
    end;
end;

function TCommunicationRealManager.AddSwitch(aAdr, aCom: integer; const aBit, aDllName: string;
    aProtocol: TProtocol): IIntfSwitch;
begin
    result := nil;

    if (aCom > -1) then
    begin
        result := self.AddRelaySwitch(aCom, aBit, aDllName, aProtocol) // Relay
    end
    else
    begin
        result := self.AddSamplerSwitch(aAdr, StrToInt(aBit)); // ZP01
    end;
end;

function TCommunicationRealManager.AddSensor(aAdr, aCom: integer; const aBit, aDllName: string;
    aProtocol: TProtocol): IIntfSensor;
begin
    result := nil;

    if (aCom > -1) then
    begin
        result := self.AddRelaySensor(aCom, aBit, aDllName, aProtocol); // Relay
    end
    else
    begin
        result := self.AddSamplerSensor(aAdr, StrToInt(aBit)); // ZP01
    end;
end;

function TCommunicationRealManager.GetRoboticInterface: TRoboticInterface;
begin
    result := nil;
    if (FCommList.Items[0] is TRoboticInterface) then
        result := (FCommList.Items[0] as TRoboticInterface);
end;

function TCommunicationRealManager.GetDLLBCReaderIntf(aSettingDP: TDataProvider; const aDllName: string)
    : IIntfBCReader;
var
    xIntf: TIntfDLLBCReader;
    xIntfDll: TIntfDLL;
begin
    xIntfDll := FindCommDll(aDllName);
    if (xIntfDll <> nil) and (xIntfDll is TIntfDLLBCReader) then
    begin
        result := xIntfDll as TIntfDLLBCReader;
    end
    else if (Pos('WAIT', UpperCase(aDllName)) = 1) then
    begin
        xIntf := TIntfDllBCReader_WAIT.Create(aSettingDP, aDllName);
        result := xIntf;
        FCommList.Add(xIntf);
    end
    else if (Pos('IT', UpperCase(aDllName)) = 1) then
    begin
        xIntf := TIntfDllBCReader_IT.Create(aSettingDP, aDllName);
        result := xIntf;
        FCommList.Add(xIntf);
    end
    else if (Pos('LEUZE', UpperCase(aDllName)) = 1) then
    begin
        xIntf := TIntfDllBCReader_LEUZE.Create(aSettingDP, aDllName);
        result := xIntf;
        FCommList.Add(xIntf);
    end
    else
    begin
        { TODO : Welche Dll's gibt noch? }
        TLogManager.Instance.Log('WARNING: Driver ' + aDllName + ': DLL Name unknown!', 1);
    end;
end;

function TCommunicationRealManager.GetZP01BCReaderIntf(aAdr: MADR; aBcType: word; aInitStr: string;
    aGetBCDelay: integer): IIntfBCReader;
var
    xIntf: TRoboticInterfaceZP01;
begin
    result := nil;
    if (FCommList.Items[0] is TRoboticInterfaceZP01) then
    begin
        xIntf := (FCommList.Items[0] as TRoboticInterfaceZP01);
        result := xIntf;
    end;
end;

function TCommunicationRealManager.AddPipPumpDLL(aSettingDP: TDataProvider; const aDllName: string;
    aBit: integer): IIntfPipPump;
var
    xIntf: TIntfDllPipPump;
    xIntfDll: TIntfDLL;
begin
    xIntfDll := FindCommDll(aDllName);
    if (xIntfDll <> nil) and (xIntfDll is TIntfDllPipPump) then
    begin
        result := xIntfDll as TIntfDllPipPump;
    end
    else if (Pos('CADI', UpperCase(aDllName)) = 1) then
    begin
        xIntf := TIntfDllPipPump_CADI.Create(aSettingDP, aDllName);
        result := xIntf;
        FCommList.Add(xIntf);
    end
    else if (Pos('CAT', UpperCase(aDllName)) = 1) then
    begin
        xIntf := TIntfDllPipPump_CATPUMP.Create(aSettingDP, aDllName, aBit);
        result := xIntf;
        FCommList.Add(xIntf);
    end
    else
    begin
        { TODO : Welche Dll's gibt noch? }
        TLogManager.Instance.Log('WARNING: Driver ' + aDllName + ': DLL Name unknown!', 1);
    end;
end;

function TCommunicationRealManager.AddPipPump(aSettingDP: TDataProvider; const aName: string;
    aDevType: TDeviceType; const aDLLName: string; aAdr, aBit: integer;
    aAspSpeed, aAspRamp, aDispSpeed, aDispRamp: integer; aValvePos: TVPosArray;
    aMaxVol_uL, aMaxVolSteps, aScalingFactor: integer; aExecID: integer): IIntfPipPump;

begin
    result := nil;
    if (aDLLName <> '') then
    begin
        result := AddPipPumpDLL(aSettingDP, aDLLName, aBit);
    end
    else if (FCommList.Items[0] is TRoboticInterfaceZP01) then
    begin
        result := (FCommList.Items[0] as TRoboticInterfaceZP01);
    end
    else if (FCommList.Items[0] is TRoboticInterfaceZP02) then
    begin
        result := (FCommList.Items[0] as TRoboticInterfaceZP02);
    end;
end;

function TCommunicationRealManager.AddMotor(const aName: string; aDevType: TDeviceType; aAdr: integer;
    aSpeed, aRamp, aInitSpeed: MPos; aProtocol: TProtocol): IIntfMotor;
begin
    result := nil;

    if (FCommList.Items[0] is TRoboticInterfaceZP01) then
    begin
        result := (FCommList.Items[0] as TRoboticInterfaceZP01);
    end
    else
    begin
        result := (FCommList.Items[0] as TRoboticInterfaceZP02);
    end;
end;

function TCommunicationRealManager.AddHeaterAsHuberTango(aSettingDP: TDataProvider; const aDevArea: string;
    aCom: integer): IIntfThermostat;
var
    xTango: TIntfNewHuberTango;
    xSimpleSettingAccess: ISimpleIniAccess;
    xUseNewProtocol, xUseExternSensor: boolean;
begin
    xTango := nil;
    xSimpleSettingAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    xUseNewProtocol := xSimpleSettingAccess.ReadBool('Thermostat', 'UseNewProtocol', false);
    // Es wird NIEMALS 2 Huber gleichzeitig geben! tbh
    xUseExternSensor := xSimpleSettingAccess.ReadBool('Thermostat', 'UseExternSensor', false);

    if (FindCommPort(aCom) <> nil) and (FindCommPort(aCom) is TIntfNewHuberTango) then
    begin
        xTango := FindCommPort(aCom) as TIntfNewHuberTango;
    end;

    if (xTango = nil) then
    begin // create communication port and add to list

        if (not xUseNewProtocol) then
            TLogManager.Instance.Log
                ('WARNING: Old Huber Protocol is not implemented in the current version!', 1);
        // xTango := TIntfOldHuberTango.Create(aCom, true)

        xTango := TIntfNewHuberTango.Create(aCom, true, xUseExternSensor);

        FCommList.Add(xTango);
    end;

    result := xTango; // TBD_ wieder zurücksetzen
end;

function TCommunicationRealManager.AddHeaterAsCATVortexer(aCom, aBit: integer; const cSleepTime: integer)
    : IIntfThermostat;
var
    xVortexerCAT: TIntfVortexerCAT;
begin
    xVortexerCAT := nil;
    if (FindCommPort(aCom) <> nil) and (FindCommPort(aCom) is TIntfVortexerCAT) then
        xVortexerCAT := FindCommPort(aCom) as TIntfVortexerCAT;

    if (xVortexerCAT = nil) then
    begin // create communication port and add to list
        xVortexerCAT := TIntfVortexerCAT.Create(aCom, fCVortexSleepTime, fOpenFixationAtInit, true,
            cSleepTime);
        FCommList.Add(xVortexerCAT);
    end;

    if (xVortexerCAT <> nil) then
        xVortexerCAT.InitHeater(aBit);

    result := xVortexerCAT; // TBD_ wieder zurücksetzen
end;

function TCommunicationRealManager.AddHeaterAsCANBusDevice(aAdr, aWritePort, aReadPort: integer)
    : IIntfThermostat;
var
    xIntf: TRoboticInterfaceZP02;
begin
    result := nil;
    if (FCommList.Items[0] is TRoboticInterfaceZP02) then
    begin // zur Zeit nur SIAS als CANBus-Modul möglich
        xIntf := (FCommList.Items[0] as TRoboticInterfaceZP02);
        result := xIntf;
    end;
end;

function TCommunicationRealManager.AddHeater(aSettingDP: TDataProvider; const aDevArea: string;
    aDevData: TThermoDeviceDataRec): IIntfThermostat;
begin
    result := nil;
    case (aDevData.Protocol) of

        cdtHuber:
            begin // Huber Tango
                result := AddHeaterAsHuberTango(aSettingDP, aDevArea, aDevData.Com);
            end;

        cdtCATVortexer:
            begin // CAT Vortexer
                result := AddHeaterAsCATVortexer(aDevData.Com, aDevData.Port, aDevData.SleepTime);
            end;

        cdtCANBus:
            begin // CAT Vortexer
                result := AddHeaterAsCANBusDevice(aDevData.Adr, aDevData.Port, aDevData.ReadPort);
            end;
    end;
end;

function TCommunicationRealManager.AddShaker(aCom, aBit: integer; aProtocol: TProtocol;
    const cSleepTime: Integer): IIntfVortexer;
var
    xVortexerIKA: TIntfVortexerIKA;
    xVortexerCAT: TIntfVortexerCAT;
begin
    result := nil;

    case aProtocol of
        cdtIKAVortexer:
            begin // IKA Vortexer
                xVortexerIKA := nil;
                if (FindCommPort(aCom) <> nil) and (FindCommPort(aCom) is TIntfVortexerIKA) then
                    xVortexerIKA := FindCommPort(aCom) as TIntfVortexerIKA;

                if (xVortexerIKA = nil) then
                begin // create communication port and add to list
                    xVortexerIKA := TIntfVortexerIKA.Create(aCom, true);
                    FCommList.Add(xVortexerIKA);
                end;
                result := xVortexerIKA;
            end;
        cdtCATVortexer:
            begin // CAT Vortexer
                xVortexerCAT := nil;
                if (aProtocol = cdtCATVortexer) and (FindCommPort(aCom) <> nil) and
                    (FindCommPort(aCom) is TIntfVortexerCAT) then
                    xVortexerCAT := FindCommPort(aCom) as TIntfVortexerCAT;

                if (xVortexerCAT = nil) then
                begin // create communication port and add to list
                    xVortexerCAT := TIntfVortexerCAT.Create(aCom, fCVortexSleepTime, fOpenFixationAtInit,
                        true, cSleepTIme);
                    FCommList.Add(xVortexerCAT);
                end;
                result := xVortexerCAT;
            end;
    end;
end;

function TCommunicationRealManager.AddEntireGripper(aDllName: string): IIntfLocationBasedMotion;
var
    xIntf: TIntfDLLEntireGripper;
    xIntfDll: TIntfDLL;
begin
    // aModuleIndex wird zur Zeit nicht gesetzt

    xIntfDll := FindCommDll(aDllName);
    if (xIntfDll <> nil) and (xIntfDll is TIntfDLLEntireGripper) then
    begin
        result := xIntfDll as TIntfDLLEntireGripper;
    end
    else
    begin
        xIntf := TIntfDLLEntireGripper.Create(aDllName, true);
        result := xIntf;
        FCommList.Add(xIntf);
    end;
end;

function TCommunicationRealManager.AddBCTurntable(aName: string; aMotor: MOTOR; aBit: integer)
    : IIntfBCTurntable;
var
    xIntf01: TRoboticInterfaceZP01;
    xIntf02: TRoboticInterfaceZP02;
begin
    result := nil;
    if (FCommList.Items[0] is TRoboticInterfaceZP01) then
    begin
        xIntf01 := (FCommList.Items[0] as TRoboticInterfaceZP01);
        result := xIntf01;
    end;
    if (FCommList.Items[0] is TRoboticInterfaceZP02) then
    begin
        xIntf02 := (FCommList.Items[0] as TRoboticInterfaceZP02);
        result := xIntf02;
    end;
end;

function TCommunicationRealManager.AddWatchDog(aDllName: string; aPort: integer; aTriggerInterval: integer)
    : IIntfWatchDog;
var
    xIntf: TIntfDLLWatchDog;
    xIntfDll: TIntfDLL;
begin

    xIntfDll := FindCommDll(aDllName);
    if (xIntfDll <> nil) and (xIntfDll is TIntfDLLWatchDog) then
    begin
        xIntf := xIntfDll as TIntfDLLWatchDog;
    end
    else
    begin
        xIntf := TIntfDLLWatchDog.Create(aDllName);
        if Assigned(xIntf) then
        begin
            FCommList.Add(xIntf);
        end;
    end;
    xIntf.WatchDog_AddPort(aPort, aTriggerInterval);

    result := xIntf;
end;

{ TIntfSignalLoop }

constructor TIntfSignalLoop.Create(aComPort: integer; aSimulated: boolean);
begin
    inherited Create(cdtSignalLoop);
    fComPortNo := aComPort;
end;

function TIntfSignalLoop.GetComPortNo: integer;
begin
    result := fComPortNo;
end;

procedure TIntfSignalLoop.Sensor_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aAdr: integer; const aBit: string; aDefaultValue: integer);
begin
    TLogManager.Instance.Log('WARNING: Driver ' + aDriverName +
        ', Type SignalLoop is not implemented in the current version!', 1);
end;

procedure TIntfSignalLoop.AddSensor(out oModuleIndex: integer; aBit: string);
begin
    oModuleIndex := -1;
end;

{ TIntfNewHuberTango }

function TIntfNewHuberTango.GetComPortNo: integer;
begin
    result := fComPortNo;
end;

function TIntfNewHuberTango.Thermostat_GetInterfaceData(aIndex: integer; out aAdr: integer;
    out aBitName: string): boolean;
begin
    result := true;
    aAdr := -1;
    aBitName := '';
end;

procedure TIntfNewHuberTango.Thermostat_WriteDriverData(aSettingDP: TDataProvider; const aDeviceName: string;
    aData: TThermoDeviceDataRec);
var
    xDriverName, xConnectionName: string;
begin
    xDriverName := aDeviceName + 'Driver';
    xConnectionName := aDeviceName + 'Connection';
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aDeviceName, 'Driver', xDriverName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', xDriverName, 'Type',
        'THERMOSTAT01THERMOSTAT');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', xDriverName, 'Connection',
        xConnectionName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', xConnectionName, 'Type',
        'THERMOSTAT01CONNECTION');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', xConnectionName, 'ComPort',
        IntToStr(fComPortNo));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', xConnectionName, 'UseExternSensor',
        TAppSettings.WriteBoolToStr(fUseExternSensor));
end;

constructor TIntfNewHuberTango.Create(aComPort: integer; aSimulated, aUseExternSensor: boolean);
begin
    inherited Create(cdtHuber);

    fComPortNo := aComPort;
    fUseExternSensor := aUseExternSensor;
end;

// --------------------------------------------------------------------------------------------------
// TIntfVortexerCAT --> TIntfCommPort --> TIntfCommunication
// --------------------------------------------------------------------------------------------------
constructor TIntfVortexerCAT.Create(aComPort: integer; aComSleepTime: word;
    aOpenFixationAtInit, aSimulated: boolean; const cSleepTime: Integer);
var
    i: integer;
begin
    inherited Create(cdtCATVortexer);

    fComPort := aComPort;
    fComSleepTime := aComSleepTime;
    FExtraSleepTime := cSleepTime;
    FOpenFixationAtInit := aOpenFixationAtInit;
    for i := 0 to MAX_VORTEXERS do
    begin // FVort[0] ist immer leer
        FVort[i].IsShaker := false;
        FVort[i].IsHeater := false;
        FVort[i].Speed := 0;
        FVort[i].Temp := 0;
        FVort[i].ONPulse := 0;
        FVort[i].OFFPulse := 0;
    end;
    FNoOFVortexer := 1;
    fConnectionWritten := false;
    fConnectionName := 'Vortexer' + IntToStr(fComPort);

end;

function TIntfVortexerCAT.GetComPortNo: integer;
begin
    result := FComPort;
end;

function TIntfVortexerCAT.Thermostat_GetInterfaceData(aIndex: integer; out aAdr: integer;
    out aBitName: string): boolean;
begin
    result := false;
    if (aIndex > 0) and (aIndex <= MAX_VORTEXERS) then
    begin
        FVort[aIndex].IsHeater := true;
        result := true;
        aBitName := IntToStr(aIndex); // ModuleIndex = Port(Bit)
    end;
end;

procedure TIntfVortexerCAT.WriteConnection(aSettingDP: TDataProvider);
begin
    if (not fConnectionWritten) then
    begin
        fConnectionWritten := true;

        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fConnectionName, 'Type',
            'Mixer01Connection');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fConnectionName, 'ComPort',
            IntToStr(fComPort));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fConnectionName, 'ComSleepTime',
            IntToStr(fComSleepTime));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fConnectionName,
            'ExtraSleepTime', IntToStr(FExtraSleepTime));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fConnectionName,
            'OpenFixationAtInit', TAppSettings.WriteBoolToStr(fOpenFixationAtInit));
    end;
end;

procedure TIntfVortexerCAT.InitHeater(aIndex: integer);
begin
    if (aIndex > 0) and (aIndex <= MAX_VORTEXERS) then
    begin
        FVort[aIndex].IsHeater := true;
    end;
end;

procedure TIntfVortexerCAT.Thermostat_WriteDriverData(aSettingDP: TDataProvider; const aDeviceName: string;
    aData: TThermoDeviceDataRec);
var
    xDriverName: string;
begin
    xDriverName := aDeviceName + 'Driver';
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aDeviceName, 'Driver', xDriverName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', xDriverName, 'Type',
        'Mixer01Thermostat');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', xDriverName, 'Connection',
        fConnectionName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', xDriverName, 'Port',
        IntToStr(aData.Port));

    WriteConnection(aSettingDP);
end;

procedure TIntfVortexerCAT.Mixer_WriteDriverData(aSettingDP: TDataProvider; aPort: integer;
    const aDeviceName: string);
var
    xFixDeviceName, xDriverName: string;
begin
    xDriverName := aDeviceName + 'Driver';
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aDeviceName, 'Driver', xDriverName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', xDriverName, 'Type', 'Mixer01Vortexer');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', xDriverName, 'Connection',
        fConnectionName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', xDriverName, 'Port', IntToStr(aPort));

    // für DEVICE-Actions muss es einen Fixation-SwitchDevice geben!
    // (als Ersatz für FIX-Action)
    xFixDeviceName := TAppSettings.GetFixationNameFromDeviceName(aDeviceName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', xFixDeviceName, 'Type', 'SWITCH');
    xDriverName := aDeviceName + 'FixDriver';
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', xFixDeviceName, 'Driver', xDriverName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', xDriverName, 'Type',
        'Mixer01FixationSwitch');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', xDriverName, 'Connection',
        fConnectionName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', xDriverName, 'Port', IntToStr(aPort));

    WriteConnection(aSettingDP);
end;
{
  function TIntfVortexerCAT.GetShakerExists(aIndex: integer; var aBit: integer): boolean;
  begin
  result := false;
  if (aIndex>0) and (aIndex<=MAX_VORTEXERS) then begin
  if (not FVort[aIndex].IsShaker) then inc(FNoOFVortexer);
  FVort[aIndex].IsShaker:=true;
  result := true;
  aBit := aIndex; // ModuleIndex = Port(Bit)
  end;
  end;
}

// --------------------------------------------------------------------------------------------------
// TIntfVortexerIKA --> TIntfCommPort --> TIntfCommunication
// --------------------------------------------------------------------------------------------------
const
    STR_IKA_STATE_RESULT = '0 4;1 4;10;11;12'; // result strings für altes und neues IKA Protokoll
    STR_IKA_STATE_REQUEST_OLD = 'STATUS_4'; // request string for status old protocol
    STR_IKA_STATE_REQUEST_NEW = 'STATUS'; // request string for status new protocol

constructor TIntfVortexerIKA.Create(aComPort: integer; aSimulated: boolean);
begin
    inherited Create(cdtIKAVortexer);

    FComPort := aComPort;
end;

procedure TIntfVortexerIKA.Mixer_WriteDriverData(aSettingDP: TDataProvider; aPort: integer;
    const aDeviceName: string);
var
    xDriverName, xConnectionName: string;
begin
    xDriverName := aDeviceName + 'Driver';
    xConnectionName := aDeviceName + 'Connection';

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aDeviceName, 'Driver', xDriverName);

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', xDriverName, 'Type', 'HEAVYDUTYSHAKER');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', xDriverName, 'Connection',
        xConnectionName);

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', xConnectionName, 'Type',
        'HEAVYDUTYSHAKERCONNECTION');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', xConnectionName, 'ComPort',
        IntToStr(fComPort));

    // früher unter DEVICE: 'RS232', 'CVortexSleepTime'
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', xConnectionName, 'ComSleepTime',
        IntToStr(gCommManager.CVortexSleepTime));
end;

function TIntfVortexerIKA.GetComPortNo: integer;
begin
    result := FComPort;
end;

{ TIntfDllBCReader }

constructor TIntfDllBCReader.Create(const aDllName: string);
begin
    inherited Create(aDllName, cdtDLLBCReader);

    fDllName := aDllName;
    fConnectionWritten := false;
end;

{ TIntfDllBCReader_IT }

constructor TIntfDllBCReader_IT.Create(aSettingDP: TDataProvider; const aDllName: string);
const
    STR_SECTION_NAME = 'Parameters';
    INT_DEF_TIMEOUT = 1000;
    INT_DEF_BAUDRATE = 115200;
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(aDllName);

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDllName);

    fComPort := xIniAccess.ReadInteger(STR_SECTION_NAME, 'ComPort', -1);
    fBaudrate := xIniAccess.ReadInteger(STR_SECTION_NAME, 'Baudrate', INT_DEF_BAUDRATE);
    fTimeout := xIniAccess.ReadInteger(STR_SECTION_NAME, 'Timeout', INT_DEF_TIMEOUT);
    fDoBeep := xIniAccess.ReadBool(STR_SECTION_NAME, 'Beep', true);
    fTriggerOffIfNoRead := xIniAccess.ReadBool(STR_SECTION_NAME, 'TriggerOffIfNoRead', true);

    // UseDynamicPort ist in der aktuellen Version immer der Fall:
    // fUseDynamicPort := xIniAccess.ReadBool( STR_SECTION_NAME, 'UseDynamicPort', false);
end;

procedure TIntfDllBCReader_IT.BCReader_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aAdr: integer; const aInitStr: string; aBCType: integer; aGetBCDelay: integer);
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type',
        'BCREADER01BCREADER');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Connection', fDllName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'GetBCDelay',
        IntToStr(aGetBCDelay));

    if (not fConnectionWritten) then
    begin
        fConnectionWritten := true;

        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'Type',
            'BCREADER01CONNECTION');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'ComPort',
            IntToStr(fComPort));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'Baudrate',
            IntToStr(fBaudrate));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'Timeout',
            IntToStr(fTimeout));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'Beep',
            TAppSettings.WriteBoolToStr(fDoBeep));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'TriggerOffIfNoRead',
            TAppSettings.WriteBoolToStr(fTriggerOffIfNoRead));
    end;
end;

{ TIntfDllBCReader_WAIT }

constructor TIntfDllBCReader_WAIT.Create(aSettingDP: TDataProvider; const aDllName: string);
const
    STR_SECTION_NAME = 'Parameters';
    INT_DEF_TIMEOUT = 10;
    INT_WA_DEF_TRIGGER_ON_CODE = '18';
    INT_WA_DEF_TRIGGER_OFF_CODE = '20';
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(aDllName);

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDllName);
    fComPort := xIniAccess.ReadInteger(STR_SECTION_NAME, 'ComPort', -1);
    fSendConfig := xIniAccess.ReadBool(STR_SECTION_NAME, 'SendConfig', true);
    fTimeout := xIniAccess.ReadInteger(STR_SECTION_NAME, 'Timeout', INT_DEF_TIMEOUT);
    fTrigOnCode := xIniAccess.ReadString(STR_SECTION_NAME, 'TriggerOn', INT_WA_DEF_TRIGGER_ON_CODE);
    fTrigOffCode := xIniAccess.ReadString(STR_SECTION_NAME, 'TriggerOff', INT_WA_DEF_TRIGGER_OFF_CODE);

    // UseDynamicPort ist in der aktuellen Version immer der Fall:
    // fUseDynamicPort := xIniAccess.ReadBool( STR_SECTION_NAME, 'UseDynamicPort', false);
end;

procedure TIntfDllBCReader_WAIT.BCReader_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aAdr: integer; const aInitStr: string; aBCType: integer; aGetBCDelay: integer);
const
    INT_WA_DEF_BAUDRATE = 38400;
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type',
        'BCREADER02BCREADER');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Connection', fDllName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'GetBCDelay',
        IntToStr(aGetBCDelay));

    if (not fConnectionWritten) then
    begin
        fConnectionWritten := true;
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'Type',
            'BCREADER02CONNECTION');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'ComPort',
            IntToStr(fComPort));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'Baudrate',
            IntToStr(INT_WA_DEF_BAUDRATE));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'Timeout',
            IntToStr(fTimeout));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'SendConfig',
            TAppSettings.WriteBoolToStr(fSendConfig));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'TriggerOn',
            fTrigOnCode);
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'TriggerOff',
            fTrigOffCode);
    end;
end;

{ TIntfDllBCReader_LEUZE }

constructor TIntfDllBCReader_LEUZE.Create(aSettingDP: TDataProvider; const aDllName: string);
const
    STR_SECTION_NAME = 'Parameters';
    INT_DEF_TIMEOUT = 5;
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(aDllName);

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDllName);
    fComPort := xIniAccess.ReadInteger(STR_SECTION_NAME, 'ComPort', -1);
    fSendConfig := xIniAccess.ReadBool(STR_SECTION_NAME, 'SendConfig', true);
    fTimeout := xIniAccess.ReadInteger(STR_SECTION_NAME, 'Timeout', INT_DEF_TIMEOUT);
end;

procedure TIntfDllBCReader_LEUZE.BCReader_WriteDriverData(aSettingDP: TDataProvider;
    const aDriverName: string; aAdr: integer; const aInitStr: string; aBCType: integer; aGetBCDelay: integer);
const
    INT_LEUZE_DEF_BAUDRATE = 9600;
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type',
        'BCREADER04BCREADER');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Connection', fDllName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'GetBCDelay',
        IntToStr(aGetBCDelay));

    if (not fConnectionWritten) then
    begin
        fConnectionWritten := true;
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'Type',
            'BCREADER04CONNECTION');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'ComPort',
            IntToStr(fComPort));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'Baudrate',
            IntToStr(INT_LEUZE_DEF_BAUDRATE));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'Timeout',
            IntToStr(fTimeout));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'SendConfig',
            TAppSettings.WriteBoolToStr(fSendConfig));
    end;
end;

{ TIntfDllBalance }

procedure TIntfDllBalance.Balance_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aBalanceDoorBlocked: Boolean; aDoorNoOfRequests: integer; aDoorType: byte; aSetupFuncs: TStringList);
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type', 'BALANCE01BALANCE');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Connection', fName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'IntCalcNumValues',
        IntToStr(fConfig.IntCalcNumValues));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'IntCalcDeviation',
        FloatToStr(fConfig.IntCalcDeviation));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'IntCalcEnabled',
        TAppSettings.WriteBoolToStr(fConfig.IntCalcDo));

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fName, 'Type',
        'BALANCE01CONNECTION');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fName, 'CommPort',
        IntToStr(fComPort));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fName, 'MaxSendTries',
        IntToStr(fMaxSendTries));
end;

constructor TIntfDllBalance.Create(aSettingDP: TDataProvider; aDllName: string; aName: string;
    aComPort: integer);
const
    STRBALANCE = 'BALANCE';
    INT_NUM_SENDANDRECEIVE_TRIES_DEF = 4;
    INT_SGBAL_WEIGHVALUE_RESOLUTION_STANDARD = 0;
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(cdtDll);

    fName := aName;
    fComPort := aComPort;

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, STRBALANCE);
    fMaxSendTries := xIniAccess.ReadInteger(aName, 'MaxSendTries', INT_NUM_SENDANDRECEIVE_TRIES_DEF);
    fWeighValueResolution := xIniAccess.ReadInteger(aName, 'WeighValueResolution',
        INT_SGBAL_WEIGHVALUE_RESOLUTION_STANDARD);
    ReadCalcValuesConfig(aSettingDP, aName, fConfig);
end;

function TIntfDllBalance.GetComPortNo: integer;
begin
    result := fComPort;
end;

function TIntfDllBalance.ReadCalcValuesConfig(aSettingDP: TDataProvider; const aBalanceName: string;
    var vConfig: TSGBalanceConfig): boolean;
const
    STRBALANCE = 'BALANCE';
var
    xNumValues: integer;
    xDeviation: extended;
    xDoInternalCalc: boolean;
    xIniAccess: ISimpleIniAccess;
begin
    result := false;
    try
        xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, STRBALANCE);

        xNumValues := xIniAccess.ReadInteger(fName, 'NumValues', 10);
        xDoInternalCalc := (xIniAccess.ReadInteger(fName, 'InternalCalc', 0) <> 0);
        try
            xDeviation := StrToFloat(xIniAccess.ReadString(fName, 'Deviation', '0.5'),
                TFormatSettings.Create('en-US'));
        except
            try
                xDeviation := StrToFloat(xIniAccess.ReadString(fName, 'Deviation', '0,5'),
                    TFormatSettings.Create('de-DE'));
            except
                xDeviation := 0.5;
            end;
        end;
        if not xDoInternalCalc then
        begin
            xDeviation := 0;
            xNumValues := 0;
        end;
        vConfig.IntCalcNumValues := xNumValues;
        vConfig.IntCalcDeviation := xDeviation;
        vConfig.IntCalcDo := xDoInternalCalc;

        result := true;
    except
    end;
end;

{ TIntfDllEntireGripper }

constructor TIntfDllEntireGripper.Create(aDllName: string; aSimulated: boolean);
begin

end;

{ TIntfDllPipPump }

constructor TIntfDllPipPump.Create(const aDllName: string);
begin
    inherited Create(aDllName, cdtDLL);

    fDllName := aDllName;
    fConnectionWritten := false;
end;

{ TIntfDllPipPump_CADI }

constructor TIntfDllPipPump_CADI.Create(aSettingDP: TDataProvider; const aDllName: string);
const
    STR_INISECTION = 'CADI';
var
    xIniAccess: ISimpleIniAccess;
    xTmpStr: string;
begin
    inherited Create(aDllName);

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDllName);

    fConfigRec.DoRelax := xIniAccess.ReadInteger(STR_INISECTION, 'DoRelax', 1);
    fConfigRec.MaxDropRepeat := xIniAccess.ReadInteger(STR_INISECTION, 'MaxDropRepeat', 5);
    fConfigRec.MinDropRepeat := xIniAccess.ReadInteger(STR_INISECTION, 'MinDropRepeat', 2);
    fConfigRec.ResetRepeat := xIniAccess.ReadInteger(STR_INISECTION, 'ResetRepeat', 2);
    fConfigRec.ComPort := xIniAccess.ReadInteger(STR_INISECTION, 'ComPort', 1);
    fConfigRec.Timeoutsecs := xIniAccess.ReadInteger(STR_INISECTION, 'TimeoutSecs', 5);
    fConfigRec.Baud := xIniAccess.ReadInteger(STR_INISECTION, 'Baud', 9600);
    fConfigRec.DataBits := xIniAccess.ReadInteger(STR_INISECTION, 'DataBits', 8);
    fConfigRec.StopBits := xIniAccess.ReadInteger(STR_INISECTION, 'StopBits', 0);
    xTmpstr := xIniAccess.ReadString(STR_INISECTION, 'Parity', 'E');
    fConfigRec.Parity := xTmpstr[1];
    fConfigRec.SleepTime := xIniAccess.ReadInteger(STR_INISECTION, 'SleepTime', 350);
    fConfigRec.MaxSendRetry := xIniAccess.ReadInteger(STR_INISECTION, 'MaxSendRetry', 3);
    fConfigRec.MaxMotorMoveTime := xIniAccess.ReadInteger(STR_INISECTION, 'MaxMotorMoveTime', 60);
    fConfigRec.MotorRequestDelay := xIniAccess.ReadInteger(STR_INISECTION, 'MotorRequestDelay', 100);
    fConfigRec.TypeAdress := xIniAccess.ReadInteger(STR_INISECTION, 'TypeAdress', 0);
    fConfigRec.SubAdress := xIniAccess.ReadInteger(STR_INISECTION, 'SubAdress', 0);
    fConfigRec.MaxVolume := xIniAccess.ReadInteger(STR_INISECTION, 'MaxVolume', 10000);
    fConfigRec.SilentPickDispErrors := xIniAccess.ReadInteger(STR_INISECTION, 'SilentPickDispErrors', 0);
end;

procedure TIntfDllPipPump_CADI.PipPump_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aExecID, aAdr, aMaxVol_uL, aMaxVolSteps, aASpeed, aARamp, aDSpeed, aDRamp, aScalingFactor: integer;
    aValvePos: TVPosArray);
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type', 'PIPPUMP03PIPPUMP');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Connection', fDllName);

    if (not fConnectionWritten) then
    begin
        fConnectionWritten := true;
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'Type',
            'PIPPUMP03CONNECTION');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'ComPort',
            IntToStr(fConfigRec.ComPort));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'Baud',
            IntToStr(fConfigRec.Baud));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'ComSleepTime',
            IntToStr(fConfigRec.SleepTime));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'MaxMotorMoveTime',
            IntToStr(fConfigRec.MaxMotorMoveTime));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'MaxSendRetry',
            IntToStr(fConfigRec.MaxSendRetry));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'MotorRequestDelay',
            IntToStr(fConfigRec.MotorRequestDelay));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'TypeAddress',
            IntToStr(fConfigRec.TypeAdress));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'SubAddress',
            IntToStr(fConfigRec.SubAdress));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'TimeoutSecs',
            IntToStr(fConfigRec.TimeoutSecs));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'DoRelax',
            IntToStr(fConfigRec.DoRelax));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'SilentPickDispErrors',
            TAppSettings.WriteBoolToStr(fConfigRec.SilentPickDispErrors <> 0));
    end;
end;

{ TIntfDllPipPump_CATPUMP }

constructor TIntfDllPipPump_CATPUMP.Create(aSettingDP: TDataProvider; const aDllName: string;
    aPumpHead: integer);
const
    CSection = 'PumpSettings';
var
    xIniAccess: ISimpleIniAccess;
    xTmp: integer;
begin
    inherited Create(aDllName);

    fPumpHead := aPumpHead;

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDllName);

    fConfigRec.ComPort := xIniAccess.ReadInteger(cSection, 'ComPort', -1);
    fConfigRec.Timeout := xIniAccess.ReadInteger(cSection, 'Timeout', 2);
    xTmp := xIniAccess.ReadInteger(cSection, 'Answerdelay', -99);
    if xTmp = -99 then
        fConfigRec.Answerdelay := xIniAccess.ReadInteger(cSection, 'AnswerDelay', 10)
    else
        fConfigRec.Answerdelay := xTmp;
    fConfigRec.Baud := xIniAccess.ReadInteger(cSection, 'Baud', 4800);

    fConfigRec.DispSpeed := xIniAccess.ReadInteger(cSection, 'DispSpeed', 0);
    fConfigRec.TitSpeed := xIniAccess.ReadInteger(cSection, 'Speed', 1);
    fConfigRec.StartPercent := xIniAccess.ReadInteger(cSection, 'Startpercent', 0);
    fConfigRec.MaxVolume := xIniAccess.ReadInteger(cSection, 'MaxVolume', 10000);
end;

procedure TIntfDllPipPump_CATPUMP.PipPump_WriteDriverData(aSettingDP: TDataProvider;
    const aDriverName: string; aExecID, aAdr, aMaxVol_uL, aMaxVolSteps, aASpeed, aARamp, aDSpeed, aDRamp,
    aScalingFactor: integer; aValvePos: TVPosArray);
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type', 'PIPPUMP04PIPPUMP');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Connection', fDllName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'PumpHead',
        IntToStr(fPumpHead));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'DispSpeed',
        IntToStr(fConfigRec.DispSpeed));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'MaxVolume',
        IntToStr(fConfigRec.MaxVolume));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'StartPercent',
        IntToStr(fConfigRec.StartPercent));

    if (not fConnectionWritten) then
    begin
        fConnectionWritten := true;
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'Type',
            'PIPPUMP04CONNECTION');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'ComPort',
            IntToStr(fConfigRec.ComPort));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'Baudrate',
            IntToStr(fConfigRec.Baud));
        // TSettingsTableProviderUtilsV0.AppendRecord( aSettingDP, 'CONNECTION', fDllName, 'WaitSleepTime', IntToStr( fConfigRec. ) );
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'AnswerDelay',
            IntToStr(fConfigRec.Answerdelay));
        // TSettingsTableProviderUtilsV0.AppendRecord( aSettingDP, 'CONNECTION', fDllName, 'Timeout', IntToStr( fConfigRec. ) );
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fDllName, 'ComportTimeout',
            IntToStr(fConfigRec.Timeout));
    end;
end;

{ TIntfDllRelayWithSensor }

class function TIntfDllRelayWithSensor.FindOrCreate(const aDllName: string; aCom: integer)
    : TIntfDllRelayWithSensor;
begin
    if (Pos('EASY', UpperCase(aDllName)) = 1) then
    begin
        result := TIntfDllRelayWithSensor_Easy800.Create(aDllName, aCom);
    end
    else
    begin
        result := nil;
        TLogManager.Instance.Log('WARNING: Driver ' + aDllName + ': DLL Name unknown!', 1);
    end;
end;

constructor TIntfDllRelayWithSensor.Create(const aDllName: string; aCom: integer);
var
    xPos: integer;
begin
    inherited Create(cdtDll);

    xPos := Pos('.', aDllName);
    if xPos > 0 then
        fDllName := Copy(aDllName, 1, xPos - 1)
    else
        fDllName := aDllName;

    fComPort := aCom;
    fConnectionName := '';
end;

function TIntfDllRelayWithSensor.GetComPortNo: integer;
begin
    result := fComPort;
end;

{ TIntfDllRelayWithSensor_Easy800 }

constructor TIntfDllRelayWithSensor_Easy800.Create(const aDllName: string; aCom: integer);
begin
    inherited Create(aDllName, aCom);

    fLinkConnections := TList<string>.Create;
end;

destructor TIntfDllRelayWithSensor_Easy800.Destroy;
begin
    fLinkConnections.Free;

    inherited;
end;

procedure TIntfDllRelayWithSensor_Easy800.Sensor_WriteDriverData(aSettingDP: TDataProvider;
    const aDriverName: string; aAdr: integer; const aBit: string; aDefaultValue: integer);
const
    INT_PARAM_NETID = 0;
    INT_PARAM_OBJECTNAME = 1;
    INT_PARAM_OBJECTINDEX = 2;
    INT_PARAM_OBJECTPORT = 3; // braucht man nicht
    INT_PARAM_MWNAME = 4;
    INT_PARAM_TRIGGER = 5;
    INT_PARAM_TRIGGERONATRESET = 6;
var
    xLinkConnectionName: string;
    xBit: TStringDynArray;
begin
    xBit := TAppSettings.StringToStringArray(aBit);

    if Length(xBit) < 3 then
        EXIT;

    // Connection
    if (fConnectionName = '') then
    begin
        fConnectionName := fDllName + IntToStr(fComPort);
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fConnectionName, 'Type',
            'PLC01CONNECTION');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fConnectionName, 'ComPort',
            IntToStr(fComPort));
    end;

    // LinkConnecttion
    xLinkConnectionName := fConnectionName + 'NetID' + xBit[INT_PARAM_NETID];
    if not fLinkConnections.Contains(xLinkConnectionName) then
    begin
        fLinkConnections.Add(xLinkConnectionName);
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', xLinkConnectionName, 'Type',
            'PLC01LINKCONNECTION');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', xLinkConnectionName,
            'Connection', fConnectionName);
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', xLinkConnectionName, 'NetID',
            xBit[INT_PARAM_NETID]);
    end;

    // Sensor Driver
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type', 'PLC01SENSOR');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Connection',
        xLinkConnectionName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'MarkerType',
        xBit[INT_PARAM_OBJECTNAME]);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'MarkerIndex',
        xBit[INT_PARAM_OBJECTINDEX]);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'DefaultValue',
        IntToStr(aDefaultValue));
end;

procedure TIntfDllRelayWithSensor_Easy800.Switch_WriteDriverData(aSettingDP: TDataProvider;
    const aDriverName: string; aAdr: integer; const aBit, aDefaultBit, aDelayedBit: string; aReverse: boolean;
    aSwitchOffDelay, aSwitchOnDelay: integer; aNeverResetAtInit, aNeverResetAtReset: boolean);
const
    INT_PARAM_NETID = 0;
    INT_PARAM_OBJECTNAME = 1;
    INT_PARAM_OBJECTINDEX = 2;
    INT_PARAM_OBJECTPORT = 3; // braucht man nicht
    INT_PARAM_MWNAME = 4;
    INT_PARAM_TRIGGER = 5;
    INT_PARAM_TRIGGERONATRESET = 6;
var
    xLinkConnectionName: string;
    xBit, xDefaultBit: TStringDynArray;
begin
    xBit := TAppSettings.StringToStringArray(aBit);

    if Length(xBit) < 3 then
        EXIT;

    // Connection
    if (fConnectionName = '') then
    begin
        fConnectionName := fDllName + IntToStr(fComPort);
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fConnectionName, 'Type',
            'PLC01CONNECTION');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fConnectionName, 'ComPort',
            IntToStr(fComPort));
    end;

    // LinkConnecttion
    xLinkConnectionName := fConnectionName + 'NetID' + xBit[INT_PARAM_NETID];
    if not fLinkConnections.Contains(xLinkConnectionName) then
    begin
        fLinkConnections.Add(xLinkConnectionName);
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', xLinkConnectionName, 'Type',
            'PLC01LINKCONNECTION');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', xLinkConnectionName,
            'Connection', fConnectionName);
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', xLinkConnectionName, 'NetID',
            xBit[INT_PARAM_NETID]);
    end;

    xDefaultBit := TAppSettings.StringToStringArray(aDefaultBit);
    if (Length(xDefaultBit) >= 3) and (xBit[INT_PARAM_NETID] = xDefaultBit[INT_PARAM_NETID]) and
        (xDefaultBit[INT_PARAM_OBJECTNAME] <> '') then
    begin
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'SecondMarkerType',
            xDefaultBit[INT_PARAM_OBJECTNAME]);
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'SecondMarkerIndex',
            xDefaultBit[INT_PARAM_OBJECTINDEX]);
    end;

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type', 'PLC01SWITCH');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'LinkConnection',
        xLinkConnectionName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'MarkerType',
        xBit[INT_PARAM_OBJECTNAME]);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'MarkerIndex',
        xBit[INT_PARAM_OBJECTINDEX]);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'SwitchOffDelay',
        IntToStr(aSwitchOffDelay));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'SwitchOnDelay',
        IntToStr(aSwitchOnDelay));

    if (aReverse) then
    begin
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'OnValue', '0');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'OffValue', '1');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'ResetValue', '1');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'InitValue', '1');
    end;

    // Trigger
    if (Length(xBit) > INT_PARAM_TRIGGER) and (StrToIntDef(xBit[INT_PARAM_TRIGGER], 0) = 1) then
    begin

        TLogManager.Instance.Log('WARNING: Driver ' + aDriverName +
            ' is defined as trigger, but used as switch!', 1);

        // ein Trigger wird bei Init nicht zurückgesetzt
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName,
            'NeverResetAtInit', 'YES');

        // spezielle Trigger-Einstellung für ResetAtReset
        if (Length(xBit) > INT_PARAM_TRIGGERONATRESET) and
            (StrToIntDef(xBit[INT_PARAM_TRIGGERONATRESET], 0) = 1) then
            TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName,
                'NeverResetAtReset', 'NO')
        else
            TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName,
                'NeverResetAtReset', 'YES');
    end
    else
    begin // Switch
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'NeverResetAtInit',
            TAppSettings.WriteBoolToStr(aNeverResetAtInit));
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'NeverResetAtReset',
            TAppSettings.WriteBoolToStr(aNeverResetAtReset));
    end;
end;

{ TIntfDLLWatchDog }

constructor TIntfDLLWatchDog.Create(const aDllName: string);
begin
    inherited Create(aDllName, cdtDll);
end;

// --------------------------------------------------------------------------------------------------
// TIntfBalanceOld
// --------------------------------------------------------------------------------------------------
procedure TIntfBalanceOld.Balance_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aBalanceDoorBlocked: Boolean; aDoorNoOfRequests: integer; aDoorType: byte; aSetupFuncs: TStringList);
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type', 'BALANCE01BALANCE');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Connection', FSerPortName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', FSerPortName, 'Type',
        'BALANCE01CONNECTION');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', FSerPortName, 'ComPort',
        IntToStr(FSerPortComPort));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', FSerPortName, 'MaxSendTries',
        IntToStr(FMaxSendRetry));
end;

constructor TIntfBalanceOld.Create(aSettingDP: TDataProvider; aSectionName: string; aProtocol: TProtocol;
    aComPort: integer; aSimulation: boolean);
var
    xIniAccess: ISimpleIniAccess;
begin

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, STR_SETTINGS_AREA_BALANCE);

    inherited Create(aProtocol);

    FSerPortName := aSectionName;
    FSerPortComPort := aComPort;
    FSerPortBaud := xIniAccess.ReadInteger(aSectionName, 'Baud', 9600);
    FSerPortDataBits := xIniAccess.ReadInteger(aSectionName, 'DataBits', 7);
    FSerPortStopBits := xIniAccess.ReadInteger(aSectionName, 'StopBits', 1);
    FSerPortParity := xIniAccess.ReadString(aSectionName, 'Parity', 'E');
    FSerPortTimeout := xIniAccess.ReadInteger(aSectionName, 'Timeout', 10);
    FSerPortSleepTime := xIniAccess.ReadInteger(aSectionName, 'SleepTime', 10);

    FMaxSendRetry := xIniAccess.ReadInteger(aSectionName, 'MaxSendRetry', 10);
    FSendDelay := xIniAccess.ReadInteger(aSectionName, 'SendDelay', 400);
    FDoorMoveDelay := xIniAccess.ReadInteger(aSectionName, 'DoorMoveDelay', 500); // only in Mettler

end;

function TIntfBalanceOld.GetComPortNo: integer;
begin
    result := FSerPortComPort;
end;

procedure TIntfDLLWatchDog.WatchDog_AddPort(aPort, aTriggerInterval: integer);
begin

end;

{ TIntfBitmapRelay }

constructor TIntfBitmapRelay.Create(aProtocol: TProtocol; aMaxOutPorts, aMaxInPorts: integer);
begin
    inherited Create(aProtocol);

    FMaxOutPorts := aMaxOutPorts;
    FMaxInPorts := aMaxInPorts;

    FPortStatus := 0;
    FNextPortStatus := 0;
    FResetPortStatus := 0;
    FInitPortStatus := 0;
    FNextDelay := 0;
end;

{ TIntfRelayConrad }

constructor TIntfRelayConrad.Create(aComPortNo: integer; aSimulated: boolean);
begin
    inherited Create(cdtConradRelay, 32, 0);

    FBoardCount := 4;
    FBoardMaxPorts := 8;
    FSerPortComPort := aComPortNo;
end;

function TIntfRelayConrad.GetComPortNo: Integer;
begin
    result := FSerPortComPort;
end;

procedure TIntfRelayConrad.Switch_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aAdr: integer; const aBit, aDefaultBit, aDelayedBit: string; aReverse: boolean;
    aSwitchOffDelay, aSwitchOnDelay: integer; aNeverResetAtInit, aNeverResetAtReset: boolean);
begin
    if (fConnectionName = '') then
    begin
        fConnectionName := 'RelayBoard' + IntToStr(FSerPortComPort);
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fConnectionName, 'Type',
            'CONRADRELAY');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fConnectionName, 'ComPort',
            IntToStr(FSerPortComPort));
    end;

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type', 'CONRADRELAY');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Connection',
        fConnectionName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Port', aBit);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'DefaultPort', aDefaultBit);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'DelayedPort', aDelayedBit);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'SwitchOffDelay',
        IntToStr(aSwitchOffDelay));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'SwitchOnDelay',
        IntToStr(aSwitchOnDelay));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'NeverResetAtInit',
        TAppSettings.WriteBoolToStr(aNeverResetAtInit));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'NeverResetAtReset',
        TAppSettings.WriteBoolToStr(aNeverResetAtReset));
end;

// --------------------------------------------------------------------------------------------------
// TIntfRelayCompulab --> TIntfRelay --> TIntfCommPort --> TIntfCommunication
// --------------------------------------------------------------------------------------------------
constructor TIntfRelayCompulab.Create(aComPortNo: integer; aSimulated: boolean);
begin
    inherited Create(cdtCompulab, 8, 8);

    FComPortNo := aComPortNo;
end;

function TIntfRelayCompulab.GetComPortNo: integer;
begin
    result := FComPortNo;
end;

procedure TIntfRelayCompulab.Sensor_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aAdr: integer; const aBit: string; aDefaultValue: integer);
begin
    TLogManager.Instance.Log('WARNING: Sensor Driver ' + aDriverName +
        ', Type RelayCompulab is not implemented in the current version!', 1);
end;

procedure TIntfRelayCompulab.Switch_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aAdr: integer; const aBit, aDefaultBit, aDelayedBit: string; aReverse: boolean;
    aSwitchOffDelay, aSwitchOnDelay: integer; aNeverResetAtInit, aNeverResetAtReset: boolean);
begin
    TLogManager.Instance.Log('WARNING: Switch Driver ' + aDriverName +
        ', Type RelayCompulab is not implemented in the current version!', 1);
end;

// --------------------------------------------------------------------------------------------------
// TIntfRelayKolter --> TIntfRelay --> TIntfCommPort --> TIntfCommunication
// --------------------------------------------------------------------------------------------------
constructor TIntfRelayKolter.Create(aComPortNo: integer; aSimulated: boolean);
begin
    inherited Create(cdtKolterOpto, 16, 16); // auch das Modell OPTO-4-IN funktioniert mit diesen Einstellugen

    FComPortNo := aComPortNo;
end;

function TIntfRelayKolter.GetComPortNo: integer;
begin
    result := FComPortNo;
end;

procedure TIntfRelayKolter.Sensor_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aAdr: integer; const aBit: string; aDefaultValue: integer);
begin
    if (fConnectionName = '') then
    begin
        fConnectionName := 'KolterOpto' + IntToStr(FComPortNo);
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fConnectionName, 'Type',
            'PLC05Connection');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fConnectionName, 'ComPort',
            IntToStr(FComPortNo));
    end;

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type', 'PLC05Sensor');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Connection',
        fConnectionName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Port', aBit);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'DefaultValue',
        IntToStr(aDefaultValue));
end;

procedure TIntfRelayKolter.Switch_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aAdr: integer; const aBit, aDefaultBit, aDelayedBit: string; aReverse: boolean;
    aSwitchOffDelay, aSwitchOnDelay: integer; aNeverResetAtInit, aNeverResetAtReset: boolean);
begin
    if (fConnectionName = '') then
    begin
        fConnectionName := 'KolterOpto' + IntToStr(FComPortNo);
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fConnectionName, 'Type',
            'PLC05Connection');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fConnectionName, 'ComPort',
            IntToStr(FComPortNo));
    end;

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type', 'PLC05Switch');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Connection',
        fConnectionName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Port', aBit);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'DefaultPort', aDefaultBit);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'DelayedPort', aDelayedBit);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'SwitchOffDelay',
        IntToStr(aSwitchOffDelay));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'SwitchOnDelay',
        IntToStr(aSwitchOnDelay));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'NeverResetAtInit',
        TAppSettings.WriteBoolToStr(aNeverResetAtInit));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'NeverResetAtReset',
        TAppSettings.WriteBoolToStr(aNeverResetAtReset));
end;

{ TRoboticInterfaceZP01 }

procedure TRoboticInterfaceZP01.BCReader_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aAdr: integer; const aInitStr: string; aBCType: integer; aGetBCDelay: integer);
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type', 'ZP01BCREADER');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Connection',
        cZP01Connection);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Adr', IntToStr(aAdr));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'InitStr', aInitStr);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'BCType',
        IntToStr(aBCType));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'GetBCDelay',
        IntToStr(aGetBCDelay));
end;

constructor TRoboticInterfaceZP01.Create(var aSimulation: boolean);
begin

end;

procedure TRoboticInterfaceZP01.Motor_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aExecID, aAdr, aSpeed, aRamp, aInitSpeed: integer);
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type', 'ZP01MOTOR');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Adr', IntToStr(aAdr));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Speed', IntToStr(aSpeed));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Ramp', IntToStr(aRamp));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Connection',
        cZP01Connection);
end;

procedure TRoboticInterfaceZP01.PipPump_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aExecID, aAdr, aMaxVol_uL, aMaxVolSteps, aASpeed, aARamp, aDSpeed, aDRamp, aScalingFactor: integer;
    aValvePos: TVPosArray);
begin

end;

procedure TRoboticInterfaceZP01.Sensor_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aAdr: integer; const aBit: string; aDefaultValue: integer);
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type', 'ZP01SENSOR');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Connection',
        cZP01Connection);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Adr', IntToStr(aAdr));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Port', aBit);
end;

procedure TRoboticInterfaceZP01.Switch_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aAdr: integer; const aBit, aDefaultBit, aDelayedBit: string; aReverse: boolean;
    aSwitchOffDelay, aSwitchOnDelay: integer; aNeverResetAtInit, aNeverResetAtReset: boolean);
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type', 'ZP01SWITCH');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Connection',
        cZP01Connection);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Adr', IntToStr(aAdr));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Port', aBit);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'DefaultPort', aDefaultBit);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'DelayedPort', aDelayedBit);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'SwitchOffDelay',
        IntToStr(aSwitchOffDelay));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'SwitchOnDelay',
        IntToStr(aSwitchOnDelay));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'NeverResetAtInit',
        TAppSettings.WriteBoolToStr(aNeverResetAtInit));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'NeverResetAtReset',
        TAppSettings.WriteBoolToStr(aNeverResetAtReset));
end;

{ TRoboticInterfaceZP02 }

procedure TRoboticInterfaceZP02.Switch_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aAdr: integer; const aBit, aDefaultBit, aDelayedBit: string; aReverse: boolean;
    aSwitchOffDelay, aSwitchOnDelay: integer; aNeverResetAtInit, aNeverResetAtReset: boolean);
begin
    // kann nicht sein!
end;

constructor TRoboticInterfaceZP02.Create(var aSimulation: boolean);
begin
    inherited Create('', cdtDll);

    fConnectionName := 'ZP02Connection';
    fConnectionWritten := false;

    fExecIDList := TList<integer>.Create();

end;

destructor TRoboticInterfaceZP02.Destroy;
begin
    FreeAndNil(fExecIDList);

    inherited;
end;

procedure TRoboticInterfaceZP02.Motor_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aExecID, aAdr, aSpeed, aRamp, aInitSpeed: integer);
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type', 'ZP02MOTOR');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Speed', IntToStr(aSpeed));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Ramp', IntToStr(aRamp));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'InitSpeed',
        IntToStr(aInitSpeed));

    WriteConnection(aSettingDP, aExecID, aDriverName, aAdr);
end;

procedure TRoboticInterfaceZP02.PipPump_WriteDriverData(aSettingDP: TDataProvider; const aDriverName: string;
    aExecID, aAdr, aMaxVol_uL, aMaxVolSteps, aASpeed, aARamp, aDSpeed, aDRamp, aScalingFactor: integer;
    aValvePos: TVPosArray);
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Type', 'ZP02PIPPUMP');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'AspSpeed',
        IntToStr(aASpeed));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'AspRamp',
        IntToStr(aARamp));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'DispSpeed',
        IntToStr(aDSpeed));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'DispRamp',
        IntToStr(aDRamp));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'MaxVolume',
        IntToStr(aMaxVol_uL));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'MaxSteps',
        IntToStr(aMaxVolSteps));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'ScalingFactor',
        IntToStr(aScalingFactor));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'ValvePos1',
        IntToStr(aValvePos[0]));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'ValvePos2',
        IntToStr(aValvePos[1]));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'ValvePos3',
        IntToStr(aValvePos[2]));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'ValvePos4',
        IntToStr(aValvePos[3]));

    WriteConnection(aSettingDP, aExecID, aDriverName, aAdr);
end;

procedure TRoboticInterfaceZP02.Thermostat_WriteDriverData(aSettingDP: TDataProvider;
    const aDeviceName: string; aData: TThermoDeviceDataRec);
var
    xDriverName: string;
begin
    xDriverName := aDeviceName + 'Driver';

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aDeviceName, 'Driver', xDriverName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', xDriverName, 'Type', 'ZP02THERMOSTAT');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', xDriverName, 'WritePort',
        IntToStr(aData.Port));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', xDriverName, 'ReadPort',
        IntToStr(aData.ReadPort));

    WriteConnection(aSettingDP, 0, xDriverName, aData.Adr);
end;

procedure TRoboticInterfaceZP02.WriteConnection(aSettingDP: TDataProvider; aExecID: integer;
    const aDriverName: string; aAdr: integer);
var
    xLinkConnName: string;
begin
    xLinkConnName := fConnectionName + 'Group' + IntToStr(aExecID);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Connection',
        xLinkConnName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DRIVER', aDriverName, 'Adr', IntToStr(aAdr));

    // LinkConnecttion
    if not fExecIDList.Contains(aExecID) then
    begin
        fExecIDList.Add(aExecID);
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', xLinkConnName, 'Type',
            'ZP02LINKCONNECTION');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', xLinkConnName, 'Connection',
            fConnectionName);
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', xLinkConnName, 'GroupID',
            IntToStr(aExecID));
    end;

    // Connecttion
    if (not fConnectionWritten) then
    begin
        fConnectionWritten := true;
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fConnectionName, 'Type',
            'ZP02CONNECTION');
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'CONNECTION', fConnectionName,
            'CANNetName', '1MBit');
    end;
end;

{ TIntfRelay }

constructor TIntfRelay.Create(aProtocol: TProtocol);
begin
    inherited Create(aProtocol);

    fConnectionName := ''; // wird erst später definiert
end;

{ TRobotArmDevice }

constructor TRobotArmDevice.Create(aSettingDP: TDataProvider; const aDevArea, aName, aSection: string;
    aDType: TDeviceType);

var
    xIniAccess: ISimpleIniAccess;
    xDeviceIniAccess: ISimpleIniAccess;
begin
    inherited Create(aName, aDType);

    // fRackMoveManager := TRackMoveManager.Create();
    // "ROBOT"
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, 'ROBOT');
    fMoveZTravelIfNoXYMove := xIniAccess.ReadBool('Module', 'MoveZTravelIfNoXYMove', true);
    fRediZRetrSpeed := xIniAccess.ReadInteger('Module', 'RediZRetrSpeed', 0);
    fHandlerXYIndependetFromRotation := xIniAccess.ReadBool('MoveRack', 'HXYIndependetFromRotation', false);

    // ZP02
    // xIniAccess := gCommonDll.CreateZP02CfgIni;  // noch nicht iatDatabaseAccess
    // xIniAccess := gCommonDll.CreateAppIni;

    xDeviceIniAccess := TDevSimpleIniAccess.Create(aSettingDP, aDevArea);
    fExecID := xDeviceIniAccess.ReadInteger(aSection, 'ExecID', 0);
    fArmGroupID := xDeviceIniAccess.ReadInteger(aSection, 'ArmGroupID', -1);
    fColor := xDeviceIniAccess.ReadString(aSection, 'Color', '');
    fZTravel := xDeviceIniAccess.ReadInteger(aSection, 'ZTravel', 0);
    fInitXBeforeY := xDeviceIniAccess.ReadBool(aSection, 'InitXBeforeY', false);

    fPipDevice := nil;
    fGripDevice := nil;
    fMotionDevice := nil;
end;

procedure TRobotArmDevice.LoadArmDevices(aSettingDP: TDataProvider; const aDevArea: string);
var
    xDevice: TDevice;
    xCrashIdent: string;
    xIniAccess: ISimpleIniAccess;
begin
    xDevice := nil;
    self.Find_ByClass(xDevice, TPipDevice);
    if Assigned(xDevice) then
    begin
        fPipDevice := xDevice as TPipDevice;
        fPipDevice.ExecID := fExecID;
        // Racknameprefix is = armname
        fPipDevice.RackNamePrefix := self.Name;
        fPipDevice.LoadDevices(aSettingDP, aDevArea);
    end;

    xDevice := nil;
    self.Find_ByClass(xDevice, TGripDevice);
    if Assigned(xDevice) then
    begin
        fGripDevice := xDevice as TGripDevice;
        fGripDevice.LoadDevices;
        if fGripDevice is TMotorGripDevice then
        begin
            (fGripDevice as TMotorGripDevice).GMotor.ExecID := fExecID;
        end;
    end;

    if Assigned(fGripDevice) then
        xCrashIdent := 'GripperArm'
    else
        xCrashIdent := 'PipArm';

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, 'ROBOT');
    fXYRange := xIniAccess.ReadXYRangeData('Carrier_Crash_Avoidance', xCrashIdent);

    if Assigned(fGripDevice) then
        fZTravelManager := TGripZTravelManager.Create(aSettingDP, fZTravel)
    else
        fZTravelManager := TZTravelManager.Create(fZTravel);

    xDevice := nil;
    self.Find_ByClass(xDevice, TMotionDevice);
    if Assigned(xDevice) then
    begin
        fMotionDevice := xDevice as TMotionDevice;
        if fMotionDevice is TMotorBasedMotionDevice then
        begin
            (fMotionDevice as TMotorBasedMotionDevice).ExecID := fExecID;
            (fMotionDevice as TMotorBasedMotionDevice).LoadDevices(aSettingDP, aDevArea);
            // ( fMotionDevice as TMotorBasedMotionDevice ).SetZProps( fZTravelManager, fArmGroupID );
        end;
    end;
end;

procedure TRobotArmDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    inherited;

    // schreiben der Daten
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'ROBOTARM');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'ArmGroupID',
        IntToStr(fArmGroupID));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Color', fColor);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'InitXBeforeY',
        TAppSettings.WriteBoolToStr(fInitXBeforeY));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'CarrierCrashAvoidanceXRel1',
        IntToStr(fXYRange.XRel1));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'CarrierCrashAvoidanceXRel2',
        IntToStr(fXYRange.XRel2));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'CarrierCrashAvoidanceYRel1',
        IntToStr(fXYRange.YRel1));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'CarrierCrashAvoidanceYRel2',
        IntToStr(fXYRange.YRel2));

    if Assigned(fMotionDevice) then
    begin
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Motion', fMotionDevice.Name);
        self.BasicZTravelValue := fMotionDevice.GetNewZTravelValue();
        fZTravelManager.ZTravelManager_WriteDeviceData(fName, aSettingDP, self.BasicZTravelValue,
            fMotionDevice.GetStepsPerUnit);
    end;
    if Assigned(fGripDevice) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Grip', fGripDevice.Name);
    if Assigned(fPipDevice) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Pip', fPipDevice.Name);

end;

// --------------------------------------------------------------------------------------------------
// TGripperArmDevice -> TCompositeDevice -> TDevice
// --------------------------------------------------------------------------------------------------
constructor TGripperArmDevice.Create(aSettingDP: TDataProvider; aName: string);
var
    xIniAccess: ISimpleIniAccess;
    xDistX_H_Tip1, xDistY_H_Tip1, xDistZ_H_Tip1: MPos;
    xZSTEPS_PER_MM, // darf nur noch an Stellen benutzt werden, wo der Hardcore-Modus keinen Einfluß hatte
    xHZSTEPS_PER_MM: Extended;
    xHZSTEPS_PER_ZSTEPS: Extended;
    // some of the settings are defined in Z_Steps but are needed as HZ_Steps : multiply by this factor to get HZSteps
begin
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, 'ROBOT');
    fDepracatedXWorldOffset := -1;
    fDepracatedXReverse := false;
    fDepracatedYWorldOffset := -1;
    fDepracatedZWorldOffset := -1;

    xZSTEPS_PER_MM := xIniAccess.ReadFloat('XYZStepsPerMillimeter', 'ZSTEPperMM', -1);
    xHZSTEPS_PER_MM := xIniAccess.ReadFloat('XYZStepsPerMillimeter', 'HZSTEPperMM', -1);
    xHZSTEPS_PER_ZSTEPS := xHZSTEPS_PER_MM / xZSTEPS_PER_MM;

    xDistX_H_Tip1 := xIniAccess.ReadInteger('Handler_ReferenceTip_Distances_Steps', 'DistX_H_Tip1', -1);
    xDistY_H_Tip1 := xIniAccess.ReadInteger('Handler_ReferenceTip_Distances_Steps', 'DistY_H_Tip1', -1);
    xDistZ_H_Tip1 := xIniAccess.ReadInteger('Handler_ReferenceTip_Distances_Steps', 'DistZ_H_Tip1', -1);

    if DepractedWorldOffsetAllowed(xDistX_H_Tip1, amtXMotor) then
    begin
        fDepracatedXWorldOffset := xDistX_H_Tip1;
        fDepracatedXReverse := TAppSettings.IsSias;
        // so far we have had only one gripper and this has always been XReversed
    end;

    if DepractedWorldOffsetAllowed(xDistY_H_Tip1, amtYMotor) then
    begin
        fDepracatedYWorldOffset := xDistY_H_Tip1;
    end;

    if DepractedWorldOffsetAllowed(xDistZ_H_Tip1, amtZMotor) then
    begin
        fDepracatedZWorldOffset := CalcDepracatedHZStepsFromZSteps(xDistZ_H_Tip1, xHZSTEPS_PER_ZSTEPS);
    end;

    // CRS-Arm!

    // in V 7.0.9 eigentlich von TRobotArmDevice abgeleitet, hier nur TCompositeDevice
    inherited Create(aName, dptCmpGrpArm); // , 1, xHZSTEPS_PER_MM );

    FToolVarispanFactor := 0;
    fIsXBasedMotorSystem := false;
end;

{ TBCTurntableDevice }

constructor TBCTurntableDevice.Create(aName, aAreaName: string; aMotor: MOTOR;
    aTurnSteps, aTurnStepsWrapAt, aBit: integer);
begin
    inherited Create(aName, aAreaName, dptTurntable);

    FTurnSteps := aTurnSteps;
    fTurnStepsWrapAt := aTurnStepsWrapAt;
    fMotor := aMotor;
    fBit := aBit;
    FIntf := gCommManager.AddBCTurntable(aName, aMotor, aBit);
    fExecID := 1;
end;

procedure TBCTurntableDevice.WriteDeviceData(aSettingDP: TDataProvider);
var
    xDriverName: string;
begin
    xDriverName := fName + 'Driver';

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'TurnSteps',
        IntToStr(fTurnSteps));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'TurnStepsWrapAt',
        IntToStr(fTurnStepsWrapAt));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Driver', xDriverName);

    if (fBit > 0) then
    begin
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'BCTurntableSwitch');
        fIntf.Switch_WriteDriverData(aSettingDP, xDriverName, fMotor.Adr, IntToStr(fBit), '-1', '-1', false,
            0, 0, false, false);
    end
    else
    begin
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'BCTurntableMotor');
        fIntf.Motor_WriteDriverData(aSettingDP, xDriverName, fExecID, fMotor.Adr, fMotor.Speed, fMotor.Ramp,
            fMotor.InitSpeed);
    end;
end;

{ TReadingDevice }

class function TReadingDevice.IniReadBCRPosition(aSettingDP: TDataProvider; const aIniKey: string;
    aCheckValueExists: boolean; var vPos: BCRPOSITION): boolean;
var
    xIniAccess: ISimpleIniAccess;
begin
    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, 'ROBOT');
    result := true;
    if aCheckValueExists then
        result := xIniAccess.ReadString(STR_ISEC_BARCODE, aIniKey, '') <> '';

    if not result then
        EXIT;

    vPos := xIniAccess.ReadBcrposition(STR_ISEC_BARCODE, aIniKey);
    // if an old Offset setting is found for X,Y or Z then convert this position from Motor pos to World Pos
    if vPos.xPos >= 0 then
        vPos.XPos := CalcWorldPosFromPos(aSettingDP, vPos.XPos, amtXMotor);
    if vPos.YPos >= 0 then
        vPos.YPos := CalcWorldPosFromPos(aSettingDP, vPos.YPos, amtYMotor);
    if vPos.ZPos >= 0 then
        vPos.ZPos := CalcWorldPosFromPos(aSettingDP, vPos.ZPos, amtZMotor);
end;

procedure TReadingDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    inherited;

    if Assigned(FBCReader) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fname, 'BCReader', FBCReader.Name);

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'XPos',
        IntToStr(FBCPosition.XPos));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'YPos',
        IntToStr(FBCPosition.YPos));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'ZPos',
        IntToStr(FBCPosition.ZPos));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'RPos',
        IntToStr(FBCPosition.RPos));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'PosName', FBCPosition.PosName);

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'XScanRange',
        IntToStr(FBCScanRange.XScanRange));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'XScanSteps',
        IntToStr(FBCScanRange.XScanSteps));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'YScanRange',
        IntToStr(FBCScanRange.YScanRange));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'YScanSteps',
        IntToStr(FBCScanRange.YScanSteps));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'ZScanRange',
        IntToStr(FBCScanRange.ZScanRange));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'ZScanSteps',
        IntToStr(FBCScanRange.ZScanSteps));

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'BCDelLastPos',
        IntToStr(self.FBCDelLastPos));
end;

{ TTubeReadingDevice }

constructor TTubeReadingDevice.Create(aBCReader: TBCReaderDevice; aBCPosition: BCRPOSITION;
    aBCScanRange: BCSCANRANGE; aBCDelLastPos: Byte; aTurntable: TBCTurntableDevice; aNoOfTurns: integer;
    aTurnSwitch: TSwitchDevice);
begin
    inherited Create('TubeBCReadingSystem', dptCmpBCReadSystem);

    FBCReader := aBCReader;
    FDeviceList.Add(aBCReader);

    FTurnDevice := aTurntable;
    if Assigned(aTurntable) then
        FDeviceList.Add(aTurntable);

    FBCPosition := aBCPosition;
    FBCScanRange := aBCScanRange;
    FBCDelLastPos := aBCDelLastPos;
    FNoOfTurns := aNoOfTurns;
    fTurnSwitch := aTurnSwitch;
end;

function TTubeReadingDevice.HasTurnDevice: boolean;
begin
    result := (FTurnDevice <> nil);
end;

procedure TTubeReadingDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    inherited;

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'TUBEBCREADER');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'NoOfTurns',
        IntToStr(FNoOfTurns));

    if Assigned(FTurnDevice) then
        TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'TurnDevice',
            FTurnDevice.Name);
end;

{ TRackReadingDevice }

constructor TRackReadingDevice.Create(aBCReader: TBCReaderDevice; aBCPosition: BCRPOSITION;
    aBCScanRange: BCSCANRANGE; aCodeFilter: string; aBCDelLastPos: Byte);
begin
    inherited Create('RackBCReadingSystem', dptCmpBCReadSystem);

    FCodeFilter := aCodeFilter;
    FBCReader := aBCReader;
    FDeviceList.Add(aBCReader);

    FBCPosition := aBCPosition;
    FBCScanRange := aBCScanRange;
    FBCDelLastPos := aBCDelLastPos;
end;

function TRackReadingDevice.GetCodeFilter: string;
begin
    if (FCodeFilter = STR_USE_BCR_FILTER) then
        result := FBCReader.CodeFilter
    else
        result := FCodeFilter;
end;

procedure TRackReadingDevice.WriteDeviceData(aSettingDP: TDataProvider);
begin
    inherited;

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type', 'RACKBCREADER');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'CodeFilter', self.GetCodeFilter);
end;

{ TRackDllBCReaderDevice }

constructor TRackDllBCReaderDevice.Create(aReadExtDLLName, aReadExtDLLFunc, aReadExtDLLParam: string);
var
    xBCPosition: BCRPOSITION; // Dummy
begin
    xBCPosition.XPos := 0;
    xBCPosition.YPos := 0;

    inherited Create('ExternRackBCReader', '', dptBCReader);
    // aReadExtDLLName,
    FDLLReadBcFunc := aReadExtDLLFunc;
    FDLLReadBcPara := aReadExtDLLParam;
end;

{ TZTravelManager }

constructor TZTravelManager.Create(aDefault: MPos);
begin
    inherited Create();
    fDefault := aDefault;
end;

function TZTravelManager.GetNewValue(aValue_Steps: MPos; aBasicZTavelValue: TPosMM;
    aStepsPerUnit: extended): TPosMM;

begin
    result := aBasicZTavelValue - (aValue_Steps / aStepsPerUnit);
end;

procedure TZTravelManager.ZTravelManager_WriteDeviceData(const aName: string; aSettingDP: TDataProvider;
    aBasicZTavelValue: TPosMM; aStepsPerUnit: extended);
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'ZTravel',
        FloatToStr(GetNewValue(fDefault, aBasicZTavelValue, aStepsPerUnit)));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'ToolZTravel',
        FloatToStr(GetNewValue(0, aBasicZTavelValue, aStepsPerUnit)));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'PipToolZTravel',
        FloatToStr(GetNewValue(0, aBasicZTavelValue, aStepsPerUnit)));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'PipToolMinZTravel',
        FloatToStr(GetNewValue(0, aBasicZTavelValue, aStepsPerUnit)));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'TubeMinZTravel',
        FloatToStr(GetNewValue(0, aBasicZTavelValue, aStepsPerUnit)));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'RackMoveZTravel',
        FloatToStr(GetNewValue(0, aBasicZTavelValue, aStepsPerUnit)));
end;

{ TGripZTravelManager }

constructor TGripZTravelManager.Create(aSettingDP: TDataProvider; aDefault: MPos);
var
    xIniAccess: ISimpleIniAccess;
begin
    inherited Create(aDefault);

    xIniAccess := TDevSimpleIniAccess.Create(aSettingDP, 'ROBOT');
    fPipTool := xIniAccess.ReadInteger('Module', 'PipToolZTravel', 0);
    fPipToolMin := xIniAccess.ReadInteger('Module', 'MinZTravelPipTool', 0);
    fTubeMin := xIniAccess.ReadInteger('Module', 'MinZTravelTubes', 0);
    fTool := xIniAccess.ReadInteger('Module', 'ToolZTravel', 0);
end;

procedure TGripZTravelManager.ZTravelManager_WriteDeviceData(const aName: string; aSettingDP: TDataProvider;
    aBasicZTavelValue: TPosMM; aStepsPerUnit: extended);
begin
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'ZTravel',
        FloatToStr(GetNewValue(fDefault, aBasicZTavelValue, aStepsPerUnit)));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'ToolZTravel',
        FloatToStr(GetNewValue(fTool, aBasicZTavelValue, aStepsPerUnit)));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'PipToolZTravel',
        FloatToStr(GetNewValue(fPipTool, aBasicZTavelValue, aStepsPerUnit)));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'PipToolMinZTravel',
        FloatToStr(GetNewValue(FPipToolMin, aBasicZTavelValue, aStepsPerUnit)));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'TubeMinZTravel',
        FloatToStr(GetNewValue(fTubeMin, aBasicZTavelValue, aStepsPerUnit)));
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', aName, 'RackMoveZTravel',
        FloatToStr(GetNewValue(0, aBasicZTavelValue, aStepsPerUnit)));
end;

{ TPneumaticGripOpenCloseSwitchDevice }

procedure TPneumaticGripOpenCloseSwitchDevice.WriteDeviceData(aSettingDP: TDataProvider);
var
    xDriverName: string;
begin
    xDriverName := fName + 'Driver';

    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Type',
        'PNEUMATICGRIPOPENCLOSESWITCH');
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Driver', xDriverName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'RackName', fRackName);
    TSettingsTableProviderUtilsV0.AppendRecord(aSettingDP, 'DEVICE', fName, 'Delay', IntToStr(fDelay));

    fIntf.Switch_WriteDriverData(aSettingDP, xDriverName, fAdr, fBit, fDefaultBit, fDelayedBit, fReverse,
        fSwitchOffDelay, fSwitchOnDelay, fNeverResetAtInit, fNeverResetAtReset);
end;


end.
