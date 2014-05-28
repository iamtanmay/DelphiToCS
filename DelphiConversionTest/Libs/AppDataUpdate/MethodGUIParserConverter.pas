{ --------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  19.02.11 wl                               TN5480    initial revision
  21.02.11 wl  DeviceActionOptionsFromPortActionStr  TN5455   neu für Port-Actions
  14.09.11 wl  ListToOptionStr                       TN5672   Keine Compiler-Warnung mehr
  29.11.11 wl  ListToOptionStr                       TN5752   durch TN5672 entstandenen Fehler gefixt
  29.11.11 wl  VortexerFixationToDeviceActionOptionsFromStrToStr TN5752  Aus FIX-Options werden DEVICE-Options
  19.12.11 wl  TImportColDefUtils.ReplaceGlobalVarName           TN5771  ändert Variablen-Namen in Import-Definitionen in lokale Variablen
  19.12.11 wl  GenericImportOptions..                            TN5771  liest und schreibt jetzt auch Import-Optionen
  14.02.12 wl  CallDllFuncActionOptionsFromStrOld                TN5443  Bei DLL-Parametern werden die &-Zeichen entfernt
  14.02.12 wl  AddMethodActionOptionsFromStrToStr                TN5443  Bei ADDM-Parametern werden die &-Zeichen entfernt
  -------------------------------------------------------------------------------------------------- }

unit MethodGUIParserConverter;


interface


uses
    Classes;

const
    STR_OPTION_DELIM = ',';
    STR_OPTION_DELIM2 = ';';

    INT_ACTIONNAME_MAX_LENGTH = 5;

    // Alphabetisch, um doppelte Bezeichner zu vermeiden:
    STR_ACTION_NAME_PIPETTE = 'PIPET';
    STR_ACTION_NAME_ASPIRATE = 'ASP';
    STR_ACTION_NAME_DISPENSE = 'DSP';
    STR_ACTION_NAME_BUILDSUBMETHOD = 'ADDM';
    STR_ACTION_NAME_BUILDSEQUENCE = 'ADSEQ';
    STR_ACTION_NAME_LIVEMETHODBUILD = 'BUILD';
    STR_ACTION_NAME_BARCODEENTER = 'BCENT';
    STR_ACTION_NAME_BUILDCONFIG = 'BCFG';
    STR_ACTION_NAME_BUILDCALC = 'CALC';
    STR_ACTION_NAME_CANBUSCOMMAND = 'CANCM';
    STR_ACTION_NAME_CHECKB = 'CHKB';
    STR_ACTION_NAME_RACKCHECK = 'CHECK';
    STR_ACTION_NAME_CHECKE = 'CHKE';
    STR_ACTION_NAME_CHANGETIPS = 'CHTIP';
    STR_ACTION_NAME_DETECTEDPOSCANCEL = 'CLDET';
    STR_ACTION_NAME_DETECTEDPOSREWRITE = 'CLREW';
    STR_ACTION_NAME_COMMAND = 'COMMA';
    STR_ACTION_NAME_FILECOPY = 'COPYF';
    STR_ACTION_NAME_SENSORCHECK = 'CSENS';
    STR_ACTION_NAME_VORTEXERTEMPCHECK = 'CTEMP';
    STR_ACTION_NAME_DELAY = 'DELAY';
    STR_ACTION_NAME_PARAMDELETE = 'DELET';
    STR_ACTION_NAME_DEVICEACTION = 'DEVIC';
    STR_ACTION_NAME_CALLDEVFUNCTION = 'DEVFU';
    STR_ACTION_NAME_CALLDLLFUNCTION = 'DLLFU';
    STR_ACTION_NAME_LAYOUTEDIT = 'EDITL';
    STR_ACTION_NAME_RACKEXPORT = 'EXPRA';
    STR_ACTION_NAME_FILLRACKWITHCAPS = 'FILLC';
    STR_ACTION_NAME_FILLDITIRACKS = 'FILLD';
    STR_ACTION_NAME_VORTEXERFIXATION = 'FIX';
    STR_ACTION_NAME_FLUSH = 'FLUSH';
    STR_ACTION_NAME_FREEBALANCE = 'FREEB';
    STR_ACTION_NAME_GROUP = 'GROUP'; // this is a runtime action inserted by the software
    STR_ACTION_NAME_IMPORTINTOTABLE = 'IMPOR';
    STR_ACTION_NAME_RACKIMPORT = 'IMPRA';
    STR_ACTION_NAME_INIT = 'INIT';
    STR_ACTION_NAME_INITDEVICE = 'INITD';
    STR_ACTION_NAME_LAYOUTRELOAD = 'LOADL';
    STR_ACTION_NAME_MANUALFILL = 'MANUA';
    STR_ACTION_NAME_MARKER = 'MARK'; // used internally by software
    STR_ACTION_NAME_MOTORMOVE = 'MMOVE';
    STR_ACTION_NAME_VOLMOTOR = 'MOTOR';
    STR_ACTION_NAME_RACKMOVE = 'MOVER';
    STR_ACTION_NAME_MOVETUBE = 'MOVET';
    STR_ACTION_NAME_MOVEZTRAVEL = 'MOVEZ';
    STR_ACTION_NAME_MESSAGE = 'MSG';
    STR_ACTION_NAME_PHOTOMETER = 'PHOTO';
    STR_ACTION_NAME_PORT = 'PORT';
    STR_ACTION_NAME_POWDERDETECTION = 'PWDET';
    STR_Action_Name_PumpAspirate = 'PUMPA';
    STR_Action_Name_PumpDispense = 'PUMPD';
    STR_ACTION_NAME_QUADRACOMMAND = 'QUADR';
    STR_ACTION_NAME_READB = 'READB';
    STR_ACTION_NAME_READE = 'READE';
    STR_ACTION_NAME_READTUBE = 'READT';
    STR_ACTION_NAME_REAGENTRUNLOAD = 'REAG';
    STR_ACTION_NAME_REMARK = 'REMAR';
    STR_ACTION_NAME_READANDWEIGHTTUBE = 'RWGHT';
    STR_ACTION_NAME_RUNLOAD = 'RUN';
    STR_ACTION_NAME_RUNSTART = 'RUNST';
    STR_ACTION_NAME_POWDERSHAKERSWITCH = 'SHSTA';
    STR_ACTION_NAME_SQLSELECT = 'SLSQL';
    STR_ACTION_NAME_VORTEXERSPEED = 'SPEED';
    STR_ACTION_NAME_VORTEXERTEMPSET = 'STEMP';
    STR_ACTION_NAME_PARAMSTORE = 'STORE';
    STR_ACTION_NAME_TARE = 'TARE';
    STR_ACTION_NAME_TIMERSET = 'TIMER';
    STR_ACTION_NAME_TIPSGET = 'TIPSG';
    STR_ACTION_NAME_TIPSRETURN = 'TIPSR';
    STR_ACTION_NAME_GRIPPERTOOLGET = 'TOOLG';
    STR_ACTION_NAME_GRIPPERTOOLRETURN = 'TOOLR';
    STR_ACTION_NAME_SQLUPDATE = 'USQL';
    STR_ACTION_NAME_VIRTUALRACKMOVE = 'VRMOV';
    STR_ACTION_NAME_TIMERWAIT = 'WAIT';
    STR_ACTION_NAME_BALANCEWAIT = 'WAITB';
    STR_ACTION_NAME_WASHPROG = 'WASHP';
    STR_ACTION_NAME_WASHTIPS = 'WASHT';
    STR_ACTION_NAME_WAITFORSLAVE = 'WFSLA';
    STR_ACTION_NAME_WEIGHPOSITION = 'WGHP';
    STR_ACTION_NAME_WEIGHTTUBE = 'WGHT';
    STR_ACTION_NAME_WEIGHWIZARD = 'WGWIZ';
    STR_ACTION_NAME_SQLWRITEFILE = 'WRSQL';
    STR_ACTION_NAME_XYMOVEMENT = 'XYMOV';
    STR_ACTION_NAME_ZPOSMOVEMENT = 'ZPOSM';

type
    TStringArray = array of string;
    TTernary = (tNull, tTrue, tFalse);

    TCallDllType = (cdDLL, cdCommand, cdRun);

    TCallDllStruct = record
        DLLType: TCallDllType;
        DLLName, DLLFunction, Parameter, StoreKeyName: string;
    end;

    TSlotStructStr = record
        Carrier, Slot, Rotation: string;
    end;

    TSQLUpdateActionOptions = record
        FileName: string;
        ArgsStr: string;
    end;

    TSQLWriteActionOptions = record
        SQLFileName: string;
        SQLArgsStr: string;
        OutputFileName: string;
        SubFilePath: string;
        TimeStamp: string;
        Show: string;
    end;

    TTubeActionOptions = record
        GripperArmName: string;
        ToolName: string;
        BitOptions: string;
        EvBeforeGet: string;
        EvAfterGet: string;
        EvBeforePut: string;
        EvAfterPut: string;
        BCEvBeforeRead: string;
        BCEvAfterRead: string;
    end;

    TDeviceActionOptions = record
        DeviceName: string;
        State: string;
    end;

    TFlushActionOptions = record
        Cycles: string;
        Volume: string;
        BitOptions: string;
        ToolSyrMap: string;
        TipMap: string;
        ArmName: string;
    end;

    TParamStoreActionOptions = record
        IsOldStyle: boolean;
        Key: string;
        Value: string;
    end;

    TCallDllFuncActionOptions = record
        DllCall: string;
    end;

    TDelayActionOptions = record
        TimeInSecs: string;
        Caption: string;
    end;

    TVortexerTempSetActionOptions = record
        DeviceName: string;
        Temperature: string;
    end;

    TVortexerTempCheckActionOptions = record
        DeviceName: string;
        MinTemperature: string;
        MaxTemperature: string;
        RequiredTimeInSecs: string;
    end;

    TVortexerSpeedActionOptions = record
        DeviceName: string;
        Speed: string;
        OnPulse: string;
        OffPulse: string;
        WaitFor: string;
    end;

    TVortexerFixationActionOptions = record
        DeviceName: string;
        State: string;
    end;

    TFileCopyActionOptions = record
        SourceFile: string;
        SourcePath: string;
        DestFile: string;
        DestPath: string;
        Overwrite: string;
        DeleteSource: string;
    end;

    TTimerSetActionOptions = record
        TimerName: string;
        WaitTimeInSec: string;
    end;

    TTimerWaitActionOptions = record
        TimerName: string;
        WaitTimeInSec: string;
        DialogText: string;
    end;

    TRackActionOptions = record
        GripperArmName: string;
        BitOptions: string;
        Destination: string;
        EvBeforeGet: string;
        EvAfterGet: string;
        EvBeforePut: string;
        EvAfterPut: string;
        BCEvBeforeRead: string;
        BCEvAfterRead: string;
    end;

    TMessageActionOptions = record
        Text: string;
        PauseRun: string;
    end;

    TAspirateDiluentActionOptions = record
        // Events für Aspirate werden nur einmal definiert
    end;

    TAspirateSampleActionOptions = record
        BeforeAsp: string;
        BeforePickLq: string;
        AfterPickLq: string;
        AfterAsp: string;
    end;

    TDispenseActionOptions = record
        BeforeDispense: string;
        BeforeDispLq: string;
        AfterDispLq: string;
        AfterDispense: string;
        TransAir: string;
    end;

    TAddMethodActionOptions = record
        name: string;
        Params: string;
        GroupID: string;
        ReadMode: string;
    end;

    TPipetteActionOptions = record
        AspD: TAspirateDiluentActionOptions;
        AspS: TAspirateSampleActionOptions;
        Disp: TDispenseActionOptions;
    end;

    TGenericImportOptions = record
        DefName: string;
        Filter: string;
        CursorMove: string;
        AllRecs: string;
        SourceFileName: string;
    end;

    TImportColDefUtils = record
    public
        class function ReplaceGlobalVarName(const aName: string): string; static;
    end;

    TCommonGUIParser = class
    private
        class function ValueContainsDelimiter(aValue: string): boolean;
        class function ConcatOptions(const aPart1, aPart2: string): string;
        class function ListToOptionStr(aList: TStrings): string;
        class function AddKeysAndValuesToOptions(aOptions: string;
            const aKeys, aValues: array of string): string;
        class procedure CommonOptionsStrToList(aList: TStrings; aStr: string);
        class function ConvertValueToSublistValue(aValue: string): string;
        class procedure OptionsStrToList(aList: TStrings; aStr: string);
        class function MakeSlotString(aSlotStruct: TSlotStructStr): string;

        class function MakeCallDllString(aCallStruct: TCallDllStruct): string;
        class function GetDllCallOld(aOptions: string; aEventName: string): string;
        class procedure GetContainerDllCallsOld(aOptions: string; out oEvBeforeGet, oEvAfterGet, oEvBeforePut,
            oEvAfterPut, oBCEvBeforeRead, oBCEvAfterRead: string);
        class procedure GetAspirateDllCallsOld(aOptions: string;
            out oEvBeforeAsp, oEvBeforePickLq, oEvAfterPickLq, oEvAfterAsp: string);
        class procedure GetDispenseDllCallsOld(aOptions: string;
            out oEvBeforeDisp, oEvBeforeDispLq, oEvAfterDispLq, oEvAfterDisp: string);
    public
        class function FindValuesForKeysInOptions(aOptions: string; out oValues: TStringArray;
            const aKeys: array of string): boolean;
        class function ActionComparableName(const aActionName: string): string;

        // SQLUpdate
        class function SQLUpdateActionOptionsFromStrToStr(aOptions: string): string;
        class function SQLUpdateActionOptionsToStr(aOptions: TSQLUpdateActionOptions): string;

        // SQLWrite
        class function SQLWriteActionOptionsFromStrToStr(aOptions: string): string;
        class function SQLWriteActionOptionsFromStrOld(aOptions: string): TSQLWriteActionOptions;
        class function SQLWriteActionOptionsToStr(aOptions: TSQLWriteActionOptions): string;

        // Tube actions
        class function TubeActionOptionsFromStrToStr(aOptions: string): string;
        class function TubeActionOptionsFromStrOld(aOptions: string): TTubeActionOptions;
        class function TubeActionOptionsToStr(aOptions: TTubeActionOptions): string;

        // Device
        class function DeviceActionOptionsFromStrOld(aOptions: string): TDeviceActionOptions;
        class function DeviceActionOptionsFromPortActionStr(aOptions: string): TDeviceActionOptions;
        class function DeviceActionOptionsFromStrToStr(aOptions: string): string;
        class function DeviceActionOptionsToStr(aOptions: TDeviceActionOptions): string;

        // Flush
        class function FlushActionOptionsFromStrToStr(aOptions: string): string;
        class function FlushActionOptionsFromStrOld(aOptions: string): TFlushActionOptions;
        class function FlushActionOptionsToStr(aOptions: TFlushActionOptions): string;

        // ParamStore
        class function ParamStoreActionOptionsFromStrToStr(aOptions: string): string;
        class function ParamStoreActionOptionsFromStrOld(aOptions: string): TParamStoreActionOptions;
        class function ParamStoreActionOptionsToStr(aOptions: TParamStoreActionOptions): string;

        // CallDllFunc
        class function CallDllFuncActionParamsClean(aParams: string): string;
        class function CallDllFuncActionSingleParamClean(aParam: string): string;
        class function CallDllFuncActionOptionsFromStrToStr(aOptions: string): string;
        class function CallDllFuncActionOptionsFromStrOld(aOptions: string): TCallDllFuncActionOptions;
        class function CallDllFuncActionOptionsToStr(aOptions: TCallDllFuncActionOptions): string;

        // Delay
        class function DelayActionOptionsFromStrOld(aOptions: string): TDelayActionOptions;
        class function DelayActionOptionsFromStrToStr(aOptions: string): string;
        class function DelayActionOptionsToStr(aOptions: TDelayActionOptions): string;

        // VortexerTempSet
        class function VortexerTempSetActionOptionsFromStrOld(aOptions: string)
            : TVortexerTempSetActionOptions;
        class function VortexerTempSetActionOptionsFromStrToStr(aOptions: string): string;
        class function VortexerTempSetActionOptionsToStr(aOptions: TVortexerTempSetActionOptions): string;

        // VortexerTempCheck
        class function VortexerTempCheckActionOptionsFromStrOld(aOptions: string)
            : TVortexerTempCheckActionOptions;
        class function VortexerTempCheckActionOptionsFromStrToStr(aOptions: string): string;
        class function VortexerTempCheckActionOptionsToStr(aOptions: TVortexerTempCheckActionOptions): string;

        // VortexerSpeed
        class function VortexerSpeedActionOptionsFromStrOld(aOptions: string): TVortexerSpeedActionOptions;
        class function VortexerSpeedActionOptionsFromStrToStr(aOptions: string): string;
        class function VortexerSpeedActionOptionsToStr(aOptions: TVortexerSpeedActionOptions): string;

        // VortexerFixation
        class function VortexerFixationActionOptionsFromStr(aOptions: string): TVortexerFixationActionOptions;
        class function VortexerFixationActionOptionsFromStrOld(aOptions: string)
            : TVortexerFixationActionOptions;
        class function VortexerFixationToDeviceActionOptionsFromStrToStr(aOptions: string): string;

        // FileCopy
        class function FileCopyActionOptionsFromStrOld(aOptions: string): TFileCopyActionOptions;
        class function FileCopyActionOptionsFromStrToStr(aOptions: string): string;
        class function FileCopyActionOptionsToStr(aOptions: TFileCopyActionOptions): string;

        // TimerSet
        class function TimerSetActionOptionsFromStrOld(aOptions: string): TTimerSetActionOptions;
        class function TimerSetActionOptionsFromStrToStr(aOptions: string): string;
        class function TimerSetActionOptionsToStr(aOptions: TTimerSetActionOptions): string;

        // TimerWait
        class function TimerWaitActionOptionsFromStrToStr(aOptions: string): string;
        class function TimerWaitActionOptionsToStr(aOptions: TTimerWaitActionOptions): string;

        // Rack Actions
        class function RackActionOptionsFromStrOld(aOptions: string): TRackActionOptions;
        class function RackActionOptionsFromStrToStr(aOptions: string): string;
        class function RackActionOptionsToStr(aOptions: TRackActionOptions): string;

        // Message
        class function MessageActionOptionsFromStrToStr(aOptions: string): string;
        class function MessageActionOptionsFromStrOld(aOptions: string): TMessageActionOptions;
        class function MessageActionOptionsToStr(aOptions: TMessageActionOptions): string;

        // Pipette action
        class function PipetteActionOptionsFromStrOld(aOptions: string): TPipetteActionOptions;
        class function PipetteActionOptionsFromStrToStr(aOptions: string): string;
        class function PipetteActionOptionsToStr(aOptions: TPipetteActionOptions): string;

        // generic import options (MethodImport)
        class function GenericImportOptionsFromStr(aOptions: string): TGenericImportOptions;
        class function GenericImportOptionsAddToStr(const aOptionStr: string;
            aImportOptions: TGenericImportOptions): string;

        // Message
        class function AddMethodActionOptionsParamsClear(aParams: string): string;
        class function AddMethodActionOptionsFromStrToStr(aOptions: string): string;
        class function AddMethodActionOptionsFromStr(aOptions: string): TAddMethodActionOptions;
        class function AddMethodActionOptionsToStr(aOptions: TAddMethodActionOptions): string;

        // neu für den Updater
        class function RemoveConcatAtBeginAndEnd(const aOptions: string): string;
    end;

    TMethodGUIParser = class(TCommonGUIParser)
    end;


implementation


uses
    SysUtils,
    ArrayUtils;

const
    STR_DELIM_KEYVALUE = '=';
    STR_DELIM_SEPARATOR = ',';
    STR_DELIM_SUBLIST_BEGIN = '{';
    STR_DELIM_SUBLIST_END = '}';

    STR_DECIMALSEPARATOR = '.';

    STR_PASRSER_STRING = '"';

    STR_YES = 'YES';
    STR_NO = 'NO';

    STR_OPTION_SUBKEY_CALLDLL_DLLNAME = 'NAME';
    STR_OPTION_SUBKEY_CALLDLL_DLLFUNCTION = 'FUNC';
    STR_OPTION_SUBKEY_CALLDLL_PARAMETER = 'PARAM';
    STR_OPTION_SUBKEY_CALLDLL_STORENAME = 'STORENAME';

    STR_OPTION_SUBKEY_SLOT_CARRIER = 'CARRIER';
    STR_OPTION_SUBKEY_SLOT_SLOTNO = 'SLOT';
    STR_OPTION_SUBKEY_SLOT_ROTATION = 'ROTATION';

    STR_OPTION_KEY_USQL_PREFIX = 'USQL';
    STR_OPTION_KEY_USQL_FILE = STR_OPTION_KEY_USQL_PREFIX + 'FILE';
    STR_OPTION_KEY_USQL_PARAMS = STR_OPTION_KEY_USQL_PREFIX + 'PARAMS';

    STR_OPTION_KEY_WRSQL_PREFIX = 'WRSQL';
    STR_OPTION_KEY_WRSQL_SQLFILE = STR_OPTION_KEY_WRSQL_PREFIX + 'FILE';
    STR_OPTION_KEY_WRSQL_SQLARGS = STR_OPTION_KEY_WRSQL_PREFIX + 'PARAMS';
    STR_OPTION_KEY_WRSQL_OUTFILE = STR_OPTION_KEY_WRSQL_PREFIX + 'OUTFILE';
    STR_OPTION_KEY_WRSQL_PATH = STR_OPTION_KEY_WRSQL_PREFIX + 'PATH';
    STR_OPTION_KEY_WRSQL_STAMP = STR_OPTION_KEY_WRSQL_PREFIX + 'STAMP';
    STR_OPTION_KEY_WRSQL_SHOW = STR_OPTION_KEY_WRSQL_PREFIX + 'SHOW';

    STR_OPTION_KEY_TUBE_PREFIX = 'TUBE';
    STR_OPTION_KEY_TUBE_ARM = STR_OPTION_KEY_TUBE_PREFIX + 'ARM';
    STR_OPTION_KEY_TUBE_BITOPTIONS = STR_OPTION_KEY_TUBE_PREFIX + 'BITOPTIONS';
    STR_OPTION_KEY_TUBE_TOOL = STR_OPTION_KEY_TUBE_PREFIX + 'TOOL';
    STR_OPTION_KEY_TUBE_BEFOREGET = STR_OPTION_KEY_TUBE_PREFIX + 'BGET';
    STR_OPTION_KEY_TUBE_AFTERGET = STR_OPTION_KEY_TUBE_PREFIX + 'AGET';
    STR_OPTION_KEY_TUBE_BEFOREPUT = STR_OPTION_KEY_TUBE_PREFIX + 'BPUT';
    STR_OPTION_KEY_TUBE_AFTERPUT = STR_OPTION_KEY_TUBE_PREFIX + 'APUT';
    STR_OPTION_KEY_TUBEBC_BEFOREREAD = STR_OPTION_KEY_TUBE_PREFIX + 'BREADBC';
    STR_OPTION_KEY_TUBEBC_AFTERREAD = STR_OPTION_KEY_TUBE_PREFIX + 'AREADBC';

    STR_OPTION_KEY_RACK_PREFIX = 'RACK';
    STR_OPTION_KEY_RACK_ARM = STR_OPTION_KEY_RACK_PREFIX + 'ARM';
    STR_OPTION_KEY_RACK_BITOPTIONS = STR_OPTION_KEY_RACK_PREFIX + 'BITOPTIONS';
    STR_OPTION_KEY_RACK_DESTINATION = STR_OPTION_KEY_RACK_PREFIX + 'DEST';
    STR_OPTION_KEY_RACK_BEFOREGET = STR_OPTION_KEY_RACK_PREFIX + 'BGET';
    STR_OPTION_KEY_RACK_AFTERGET = STR_OPTION_KEY_RACK_PREFIX + 'AGET';
    STR_OPTION_KEY_RACK_BEFOREPUT = STR_OPTION_KEY_RACK_PREFIX + 'BPUT';
    STR_OPTION_KEY_RACK_AFTERPUT = STR_OPTION_KEY_RACK_PREFIX + 'APUT';
    STR_OPTION_KEY_RACKBC_BEFOREREAD = STR_OPTION_KEY_RACK_PREFIX + 'BREADBC';
    STR_OPTION_KEY_RACKBC_AFTERREAD = STR_OPTION_KEY_RACK_PREFIX + 'AREADBC';

    STR_OPTION_KEY_DEVICE_PREFIX = 'DEVIC';
    STR_OPTION_KEY_DEVICE_NAME = STR_OPTION_KEY_DEVICE_PREFIX + 'NAME';
    STR_OPTION_KEY_DEVICE_STATE = STR_OPTION_KEY_DEVICE_PREFIX + 'STATE';
    STR_OPTION_KEY_DEVICE_STATE_ON = 'ON';
    STR_OPTION_KEY_DEVICE_STATE_OFF = 'OFF';
    STR_OPTION_KEY_DEVICE_STATE_DEF = 'DEFAULT';
    STR_OPTION_KEY_DEVICE_STATE_BOTH = 'BOTH';

    STR_OPTION_KEY_FLUSH_PREFIX = 'FLUSH';
    STR_OPTION_KEY_FLUSH_VOLUME = STR_OPTION_KEY_FLUSH_PREFIX + 'VOLUME';
    STR_OPTION_KEY_FLUSH_CYCLES = STR_OPTION_KEY_FLUSH_PREFIX + 'CYCLES';
    STR_OPTION_KEY_FLUSH_BITOPTIONS = STR_OPTION_KEY_FLUSH_PREFIX + 'BITOPTIONS';
    STR_OPTION_KEY_FLUSH_TOOLSYRMAP = STR_OPTION_KEY_FLUSH_PREFIX + 'TOOLSYRMAP';
    STR_OPTION_KEY_FLUSH_TIPMAP = STR_OPTION_KEY_FLUSH_PREFIX + 'TIPMAP';
    STR_OPTION_KEY_FLUSH_ARM = STR_OPTION_KEY_FLUSH_PREFIX + 'ARM';
    INT_OPTION_KEY_FLUSH_OPT_USEPERI = 1;
    INT_OPTION_KEY_FLUSH_OPT_USECH1 = 2;
    INT_OPTION_KEY_FLUSH_OPT_USECH2 = 4;
    INT_OPTION_KEY_FLUSH_OPT_DEFAULT = 7;
    INT_OPTION_KEY_FLUSH_TIPMAP_DEFAULT = 255;

    STR_OPTION_KEY_PARAMSTORE_PREFIX = 'STORE';
    STR_OPTION_KEY_PARAMSTORE_KEY = STR_OPTION_KEY_PARAMSTORE_PREFIX + 'KEY';
    STR_OPTION_KEY_PARAMSTORE_VALUE = STR_OPTION_KEY_PARAMSTORE_PREFIX + 'VALUE';

    STR_OPTION_KEY_CALLDLL_PREFIX = 'DLL';
    STR_OPTION_KEY_CALLDLL_CALL = STR_OPTION_KEY_CALLDLL_PREFIX + 'CALL';

    STR_OPTION_KEY_DELAY_PREFIX = 'DELAY';
    STR_OPTION_KEY_DELAY_TIME = STR_OPTION_KEY_DELAY_PREFIX + 'TIME';
    STR_OPTION_KEY_DELAY_CAPTION = STR_OPTION_KEY_DELAY_PREFIX + 'CAPTION';

    STR_OPTION_KEY_VORTEXERTEMPSET_PREFIX = 'STEMP';
    STR_OPTION_KEY_VORTEXERTEMPSET_DEVICE = STR_OPTION_KEY_VORTEXERTEMPSET_PREFIX + 'DEVICE';
    STR_OPTION_KEY_VORTEXERTEMPSET_TEMPERATURE = STR_OPTION_KEY_VORTEXERTEMPSET_PREFIX + 'TEMP';

    STR_OPTION_KEY_VORTEXERTEMPCHECK_PREFIX = 'CTEMP';
    STR_OPTION_KEY_VORTEXERTEMPCHECK_DEVICE = STR_OPTION_KEY_VORTEXERTEMPCHECK_PREFIX + 'DEVICE';
    STR_OPTION_KEY_VORTEXERTEMPCHECK_MINTEMPERATURE = STR_OPTION_KEY_VORTEXERTEMPCHECK_PREFIX + 'MINTEMP';
    STR_OPTION_KEY_VORTEXERTEMPCHECK_MAXTEMPERATURE = STR_OPTION_KEY_VORTEXERTEMPCHECK_PREFIX + 'MAXTEMP';
    STR_OPTION_KEY_VORTEXERTEMPCHECK_REQUIREDTIME = STR_OPTION_KEY_VORTEXERTEMPCHECK_PREFIX + 'REQTIME';

    STR_OPTION_KEY_VORTEXERSPEED_PREFIX = 'SPEED';
    STR_OPTION_KEY_VORTEXERSPEED_DEVICE = STR_OPTION_KEY_VORTEXERSPEED_PREFIX + 'DEVICE';
    STR_OPTION_KEY_VORTEXERSPEED_SPEED = STR_OPTION_KEY_VORTEXERSPEED_PREFIX + 'SPEED';
    STR_OPTION_KEY_VORTEXERSPEED_ONPULSE = STR_OPTION_KEY_VORTEXERSPEED_PREFIX + 'ONPULSE';
    STR_OPTION_KEY_VORTEXERSPEED_OFFPULSE = STR_OPTION_KEY_VORTEXERSPEED_PREFIX + 'OFFPULSE';
    STR_OPTION_KEY_VORTEXERSPEED_WAIT = STR_OPTION_KEY_VORTEXERSPEED_PREFIX + 'WAIT';
    STR_OPTION_KEY_VORTEXERSPEED_WAIT_NO = 'NO';
    STR_OPTION_KEY_VORTEXERSPEED_WAIT_YES = 'YES';
    STR_OPTION_KEY_VORTEXERSPEED_WAIT_IFSPEEDISNULL = 'IFSPEEDISNULL';

    STR_OPTION_KEY_VORTEXERFIXATION_PREFIX = 'FIX';
    STR_OPTION_KEY_VORTEXERFIXATION_DEVICE = STR_OPTION_KEY_VORTEXERFIXATION_PREFIX + 'DEVICE';
    STR_OPTION_KEY_VORTEXERFIXATION_STATE = STR_OPTION_KEY_VORTEXERFIXATION_PREFIX + 'STATE';
    STR_OPTION_KEY_VORTEXERFIXATION_STATE_ON = 'ON';
    STR_OPTION_KEY_VORTEXERFIXATION_STATE_OFF = 'OFF';

    STR_OPTION_KEY_FILECOPY_PREFIX = 'COPYF';
    STR_OPTION_KEY_FILECOPY_SOURCEFILE = STR_OPTION_KEY_FILECOPY_PREFIX + 'SRCFILE';
    STR_OPTION_KEY_FILECOPY_SOURCEPATH = STR_OPTION_KEY_FILECOPY_PREFIX + 'SRCPATH';
    STR_OPTION_KEY_FILECOPY_DESTFILE = STR_OPTION_KEY_FILECOPY_PREFIX + 'DESTFILE';
    STR_OPTION_KEY_FILECOPY_DESTPATH = STR_OPTION_KEY_FILECOPY_PREFIX + 'DESTPATH';
    STR_OPTION_KEY_FILECOPY_OVERWRITE = STR_OPTION_KEY_FILECOPY_PREFIX + 'OVERWRITE';
    STR_OPTION_KEY_FILECOPY_DELETESRC = STR_OPTION_KEY_FILECOPY_PREFIX + 'DELETESRC';

    STR_OPTION_KEY_TIMERSET_PREFIX = 'TIMER';
    STR_OPTION_KEY_TIMERSET_TIMERNAME = STR_OPTION_KEY_TIMERSET_PREFIX + 'NAME';
    STR_OPTION_KEY_TIMERSET_WAITTIME = STR_OPTION_KEY_TIMERSET_PREFIX + 'TIME';

    STR_OPTION_KEY_TIMERWAIT_PREFIX = 'WAIT';
    STR_OPTION_KEY_TIMERWAIT_TIMERNAME = STR_OPTION_KEY_TIMERWAIT_PREFIX + 'NAME';
    STR_OPTION_KEY_TIMERWAIT_WAITTIME = STR_OPTION_KEY_TIMERWAIT_PREFIX + 'TIME';
    STR_OPTION_KEY_TIMERWAIT_TEXT = STR_OPTION_KEY_TIMERWAIT_PREFIX + 'TEXT';

    STR_OPTION_KEY_MESSAGE_PREFIX = 'MSG';
    STR_OPTION_KEY_MESSAGE_TEXT = STR_OPTION_KEY_MESSAGE_PREFIX + 'TEXT';
    STR_OPTION_KEY_MESSAGE_PAUSERUN = STR_OPTION_KEY_MESSAGE_PREFIX + 'PAUSERUN';

    STR_OPTION_KEY_PIP_PREFIX = 'PIP';
    STR_OPTION_KEY_PIP_EVBEFOREASP = STR_OPTION_KEY_PIP_PREFIX + 'EVBASP';
    STR_OPTION_KEY_PIP_EVBEFOREPICKLQ = STR_OPTION_KEY_PIP_PREFIX + 'EVBPICKLQ';
    STR_OPTION_KEY_PIP_EVAFTERPICKLQ = STR_OPTION_KEY_PIP_PREFIX + 'EVAPICKLQ';
    STR_OPTION_KEY_PIP_EVAFTERASP = STR_OPTION_KEY_PIP_PREFIX + 'EVAASP';
    STR_OPTION_KEY_PIP_EVBEFOREDISP = STR_OPTION_KEY_PIP_PREFIX + 'EVBDISP';
    STR_OPTION_KEY_PIP_EVBEFOREDISPLQ = STR_OPTION_KEY_PIP_PREFIX + 'EVBDISPLQ';
    STR_OPTION_KEY_PIP_EVAFTERDISPLQ = STR_OPTION_KEY_PIP_PREFIX + 'EVADISPLQ';
    STR_OPTION_KEY_PIP_EVAFTERDISP = STR_OPTION_KEY_PIP_PREFIX + 'EVADISP';

    STR_OPTION_KEY_ADDM_PREFIX = 'ADDM';
    STR_OPTION_KEY_ADDM_NAME = STR_OPTION_KEY_ADDM_PREFIX + 'NAME';
    STR_OPTION_KEY_ADDM_PARAMS = STR_OPTION_KEY_ADDM_PREFIX + 'PARAMS';
    STR_OPTION_KEY_ADDM_READ = STR_OPTION_KEY_ADDM_PREFIX + 'READ';
    STR_OPTION_KEY_ADDM_GROUPID = STR_OPTION_KEY_ADDM_PREFIX + 'GROUPID';

    // --------------------------------------------------------------------------------------------------
function gmReadSubString(var aCurrentChar: integer; aText, aDelimiter: string): string;
// --------------------------------------------------------------------------------------------------
// Achtung! geänderte Funktion! Bei neuen Funktionen sollte diese Funktion bevorzugt werden!
// Problem: Ist nicht 100% kompatibel, da ActPos am Schluß an anderer Stelle steht als bei der alten Funktion
//
// Liest aus string S einen SubString, bis der Delimiter (z.B. ',') erreicht ist.
// Die aktuelle Position (ActualChar) wird hinter den nächsten Delimiter verschoben.
// --------------------------------------------------------------------------------------------------
begin
    result := '';
    if (aCurrentChar < 1) then
        aCurrentChar := 1; // bei 0 passiert das ungeheure
    while (aCurrentChar <= Length(aText)) and (aText[aCurrentChar] <> aDelimiter) do
    begin
        result := result + aText[aCurrentChar];
        inc(aCurrentChar);
    end;
    inc(aCurrentChar);
end;

// --------------------------------------------------------------------------------------------------
function gmReadSubStr(var ActualChar: integer; S, Delimiter: string): string;
// --------------------------------------------------------------------------------------------------
// Alte Funktion!
// Problem: Liest falsch, wenn an erster Stelle nichts steht (String beginnt mit Delimiter)
//
// Liest aus string S einen SubString, bis der Delimiter (z.B. ',') erreicht ist.
// Die aktuelle Position (ActualChar) wird hinter den nächsten Delimiter verschoben.
// --------------------------------------------------------------------------------------------------
begin
    result := '';
    if (ActualChar < 1) then
        ActualChar := 1; // bei 0 passiert das ungeheure
    if ActualChar > length(S) then
        exit;
    repeat
        if S[ActualChar] <> Delimiter then
            result := result + S[ActualChar];
        inc(ActualChar);
    until (S[ActualChar] = Delimiter) or (ActualChar > length(S));
end;

// ------------------------------------------------------------------------------
function gmIntToStrExt(var UsedString: string): integer;
// ------------------------------------------------------------------------------
var
    i: integer;
    SubStr: string;
    ex: boolean;
begin
    i := 1;
    ex := false;
    SubStr := '';
    repeat
        case UsedString[i] of
            '0' .. '9', '+', '-':
                SubStr := SubStr + UsedString[i];
            ' ':
                begin
                end;
            else
                ex := true;
        end;
        inc(i);
    until ex;
    try
        result := StrToInt(SubStr);
        UsedString := Copy(UsedString, i - 1, 255);
    except
        result := 0;
    end;
end;

{ TImportColDefUtils }

class function TImportColDefUtils.ReplaceGlobalVarName(const aName: string): string;
begin
    result := Trim(aName);

    if Pos('_', result) = 1 then
    begin
        if (Pos('$', result) = 2) or (Pos('!', result) = 2) then
        begin
            result := '_$0' + Copy(result, 3, Length(result) - 2);
        end
        else
        begin
            result := '_$0' + Copy(result, 2, Length(result) - 1);
        end;

    end;
end;

{ TCommonGUIParser }

class function TCommonGUIParser.FindValuesForKeysInOptions(aOptions: string; out oValues: TStringArray;
    const aKeys: array of string): boolean;
var
    xList: TStrings;
    i: integer;
    xKey, xValue: string;
begin
    SetLength(oValues, high(aKeys) + 1);

    result := false;
    xList := TStringList.Create;
    try
        OptionsStrToList(xList, aOptions);
        for i := 0 to high(aKeys) do
        begin
            xValue := '';
            xKey := aKeys[i];
            if (xList.IndexOfName(xKey) > -1) then
                xValue := xList.Values[xKey];
            if (xValue <> '') then
                result := true; // result = true: mindestens ein Parameter ist gesetzt
            oValues[i] := xValue;
        end;
    finally
        xList.Free;
    end;
end;

class function TCommonGUIParser.ValueContainsDelimiter(aValue: string): boolean;
var
    i: integer;
    xSubListFound: integer;
    xChar, xNextChar: char;
    xLen: integer;
begin
    result := false;
    xSubListFound := 0;
    i := 0;
    xLen := Length(aValue);
    if xLen = 0 then
        EXIT;

    while true do
    begin
        Inc(i);
        if i > xLen then
            BREAK;
        xChar := aValue[i];
        xNextChar := #0;
        if i + 1 <= xLen then
            xNextChar := aValue[i + 1];

        if (xChar = STR_DELIM_SUBLIST_BEGIN) then
            inc(xSubListFound)
        else if (xChar = STR_DELIM_SUBLIST_END) then
        begin
            if (xSubListFound = 0) then
                raise Exception.CreateFmt('Close braket without open bracket %s', [aValue]);
            dec(xSubListFound)
        end
        else if (xSubListFound > 0) then
        begin
            // nicht innerhalb der Klammern testen!
        end
        // avoid cases of : <=, >=, ==
        else if (xNextChar = STR_DELIM_KEYVALUE) and
            ((xChar = STR_DELIM_KEYVALUE) or (xChar = '<') or (xChar = '>')) then
        begin
            Inc(i);
        end
        else if (xChar = STR_DELIM_SEPARATOR) then
        begin
            result := true;
            BREAK;
        end
        else if (xChar = STR_DELIM_KEYVALUE) then
        begin
            result := true;
            BREAK;
        end;

    end;

    if (xSubListFound > 0) then
        raise Exception.CreateFmt('Open bracket without close bracket %s', [aValue]);
end;

class function TCommonGUIParser.ConvertValueToSublistValue(aValue: string): string;
begin
    result := STR_DELIM_SUBLIST_BEGIN + aValue + STR_DELIM_SUBLIST_END;
end;

class function TCommonGUIParser.AddKeysAndValuesToOptions(aOptions: string;
    const aKeys, aValues: array of string): string;
var
    xList: TStrings;
    i: integer;
    xValue: string;
begin
    result := aOptions;
    xList := TStringList.Create;
    try
        OptionsStrToList(xList, aOptions);
        for i := 0 to high(aKeys) do
        begin
            xValue := aValues[i];
            if (xValue = '') then
                CONTINUE;

            xList.Values[aKeys[i]] := xValue;
        end;
        result := ListToOptionStr(xList);
    finally
        xList.Free;
    end;
end;

class function TCommonGUIParser.ConcatOptions(const aPart1, aPart2: string): string;
begin
    if aPart1 = '' then
        result := aPart2
    else if aPart2 = '' then
        result := aPart1
    else
        result := aPart1 + STR_DELIM_SEPARATOR + aPart2;
end;

class function TCommonGUIParser.ListToOptionStr(aList: TStrings): string;
var
    i: integer;
    xKey, xValue: string;
    xKeyAndDelim: string;
    xNewOption: string;
    xInt: integer;
begin
    result := '';
    for i := 0 to aList.Count - 1 do
    begin
        xKey := aList.Names[i];

        // if an integer then it was a Keyless Value so omit the key and the keyvalue delimiter
        if TryStrToInt(xKey, xInt) then
            xKeyAndDelim := ''
        else
            xKeyAndDelim := xKey + STR_DELIM_KEYVALUE;

        xValue := aList.Values[xKey];
        if ValueContainsDelimiter(xValue) then
            xValue := ConvertValueToSublistValue(xValue);
        xNewOption := xKeyAndDelim + xValue;
        result := ConcatOptions(result, xNewOption);
    end;
end;

class procedure TCommonGUIParser.OptionsStrToList(aList: TStrings; aStr: string);
begin
    CommonOptionsStrToList(aList, aStr);
end;

class function TCommonGUIParser.RemoveConcatAtBeginAndEnd(const aOptions: string): string;
var
    xList: TStrings;
    i: integer;
    xValue: string;
begin
    result := aOptions;
    xList := TStringList.Create;
    try
        OptionsStrToList(xList, aOptions);
        for i := 0 to xList.Count - 1 do
        begin
            xValue := xList.ValueFromIndex[i];
            if (xValue = '') then
                CONTINUE;

            // Parser-Änderung: am Anfang und am Schluß werden &-Zeichen nicht mehr benötigt
            if (xValue[1] = '&') then
                xValue := Copy(xValue, 2, Length(xValue) - 1);
            if (xValue[Length(xValue)] = '&') then
                xValue := Copy(xValue, 1, Length(xValue) - 1);

            xList.ValueFromIndex[i] := xValue;
        end;
        result := ListToOptionStr(xList);
    finally
        xList.Free;
    end;
end;

class procedure TCommonGUIParser.CommonOptionsStrToList(aList: TStrings; aStr: string);
var
    i: integer;
    xKey: string;
    xValue: string;
    xSubListFound: integer;
    xChar, xNextChar: char;
    xLen: integer;
    xAccum: string;
    xKeylessValueCount: integer;

    procedure AddValue();
    begin
        xValue := xAccum;
        xAccum := '';
        // if xKey = '' then
        // raise Exception.CreateFmt( 'Value %s is not associated with a proper key', [xValue] );
        if xKey = '' then
        begin
            Inc(xKeylessValueCount);
            xKey := IntToStr(xKeylessValueCount); // create a unique, fake key
        end;
        aList.Values[xKey] := xValue;
        xKey := '';
    end;

begin

    xSubListFound := 0;
    xKeylessValueCount := 0;
    i := 0;
    xLen := Length(aStr);
    if xLen = 0 then
        EXIT;

    while true do
    begin
        Inc(i);
        if i > xLen then
            Break;
        xChar := aStr[i];
        xNextChar := #0;
        if i + 1 <= xLen then
            xNextChar := aStr[i + 1];
        if (xChar = STR_DELIM_SUBLIST_BEGIN) then
        begin
            if (xSubListFound > 0) then
                xAccum := xAccum + xChar;
            inc(xSubListFound);
        end
        else if (xChar = STR_DELIM_SUBLIST_END) then
        begin
            if (xSubListFound = 0) then
                raise Exception.CreateFmt('Close braket without open bracket %s', [aStr]);
            dec(xSubListFound);
            if (xSubListFound > 0) then
                xAccum := xAccum + xChar;
        end
        else if (xSubListFound > 0) then
        begin
            xAccum := xAccum + xChar;
        end
        else if (xChar = STR_DELIM_SEPARATOR) then
        begin
            AddValue();
        end
        // avoid cases of : <=, >=, ==
        else if (xNextChar = STR_DELIM_KEYVALUE) and
            ((xChar = STR_DELIM_KEYVALUE) or (xChar = '<') or (xChar = '>')) then
        begin
            xAccum := xAccum + xChar + xNextChar;
            Inc(i);
        end
        else if xChar = STR_DELIM_KEYVALUE then
        begin
            xKey := xAccum;
            xAccum := '';
        end
        else
        begin
            xAccum := xAccum + xChar;
        end;

        if i = xLen then
        begin
            AddValue();
            BREAK;
        end
    end;

    if (xSubListFound > 0) then
        raise Exception.CreateFmt('Open bracket without close bracket %s', [aStr]);
end;

class function TCommonGUIParser.ActionComparableName(const aActionName: string): string;
begin
    result := Copy(aActionName, 1, INT_ACTIONNAME_MAX_LENGTH);
    result := UpperCase(result);
end;

class function TCommonGUIParser.MakeCallDllString(aCallStruct: TCallDllStruct): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_SUBKEY_CALLDLL_DLLNAME,
        STR_OPTION_SUBKEY_CALLDLL_DLLFUNCTION, STR_OPTION_SUBKEY_CALLDLL_PARAMETER,
        STR_OPTION_SUBKEY_CALLDLL_STORENAME], [aCallStruct.DLLName, aCallStruct.DLLFunction,
        aCallStruct.Parameter, aCallStruct.StoreKeyName]);
end;

class function TCommonGUIParser.GetDllCallOld(aOptions: string; aEventName: string): string;
var
    xDllCall: TCallDllStruct;
    xActPos: integer;
    xReadEvent: string;
begin
    result := '';
    xActPos := Pos(aEventName, UpperCase(aOptions));
    if (xActPos <= 0) then
        EXIT;

    // Auslesen Option feld
    xReadEvent := gmReadSubString(xActPos, aOptions, ',');
    if (UpperCase(xReadEvent) <> aEventName) then
        EXIT; // sonst wäre keine Unterscheidung zwischen EV_DISP und EV_DISPLQ möglich

    xDllCall.DLLName := gmReadSubString(xActPos, aOptions, ',');
    xDllCall.DllFunction := gmReadSubString(xActPos, aOptions, ',');
    xDllCall.Parameter := gmReadSubString(xActPos, aOptions, ';');
    result := MakeCallDllString(xDllCall);
end;

class procedure TCommonGUIParser.GetContainerDllCallsOld(aOptions: string;
    out oEvBeforeGet, oEvAfterGet, oEvBeforePut, oEvAfterPut, oBCEvBeforeRead, oBCEvAfterRead: string);
const
    STR_EV_BEFORE_GETCONTAINER = 'EV_BGET';
    STR_EV_AFTER_GETCONTAINER = 'EV_AGET';
    STR_EV_BEFORE_PUTCONTAINER = 'EV_BPUT';
    STR_EV_AFTER_PUTCONTAINER = 'EV_APUT';
    STR_EV_BEFORE_READBC = 'EV_BREADB';
    STR_EV_AFTER_READBC = 'EV_AREADB';
begin
    oEvBeforeGet := GetDllCallOld(aOptions, STR_EV_BEFORE_GETCONTAINER);
    oEvAfterGet := GetDllCallOld(aOptions, STR_EV_AFTER_GETCONTAINER);
    oEvBeforePut := GetDllCallOld(aOptions, STR_EV_BEFORE_PUTCONTAINER);
    oEvAfterPut := GetDllCallOld(aOptions, STR_EV_AFTER_PUTCONTAINER);
    oBCEvBeforeRead := GetDllCallOld(aOptions, STR_EV_BEFORE_READBC);
    oBCEvAfterRead := GetDllCallOld(aOptions, STR_EV_AFTER_READBC);
end;

class procedure TCommonGUIParser.GetAspirateDllCallsOld(aOptions: string;
    out oEvBeforeAsp, oEvBeforePickLq, oEvAfterPickLq, oEvAfterAsp: string);
const
    STR_EV_BEFORE_ASPIRATE = 'EV_BASP';
    STR_EV_BEFORE_PICKLIQUID = 'EV_BPICKLQ';
    STR_EV_AFTER_PICKLIQUID = 'EV_APICKLQ';
    STR_EV_AFTER_ASPIRATE = 'EV_AASP';
begin
    oEvBeforeAsp := GetDllCallOld(aOptions, STR_EV_BEFORE_ASPIRATE);
    oEvBeforePickLq := GetDllCallOld(aOptions, STR_EV_BEFORE_PICKLIQUID);
    oEvAfterPickLq := GetDllCallOld(aOptions, STR_EV_AFTER_PICKLIQUID);
    oEvAfterAsp := GetDllCallOld(aOptions, STR_EV_AFTER_ASPIRATE);
end;

class procedure TCommonGUIParser.GetDispenseDllCallsOld(aOptions: string;
    out oEvBeforeDisp, oEvBeforeDispLq, oEvAfterDispLq, oEvAfterDisp: string);
const
    STR_EV_BEFORE_DISPENSE = 'EV_BDISP';
    STR_EV_BEFORE_DISPLIQUID = 'EV_BDISPLQ';
    STR_EV_AFTER_DISPLIQUID = 'EV_ADISPLQ';
    STR_EV_AFTER_DISPENSE = 'EV_ADISP';
begin
    oEvBeforeDisp := GetDllCallOld(aOptions, STR_EV_BEFORE_DISPENSE);
    oEvBeforeDispLq := GetDllCallOld(aOptions, STR_EV_BEFORE_DISPLIQUID);
    oEvAfterDispLq := GetDllCallOld(aOptions, STR_EV_AFTER_DISPLIQUID);
    oEvAfterDisp := GetDllCallOld(aOptions, STR_EV_AFTER_DISPENSE);
end;

class function TCommonGUIParser.MakeSlotString(aSlotStruct: TSlotStructStr): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_SUBKEY_SLOT_CARRIER, STR_OPTION_SUBKEY_SLOT_SLOTNO,
        STR_OPTION_SUBKEY_SLOT_ROTATION], [aSlotStruct.Carrier, aSlotStruct.Slot, aSlotStruct.Rotation]);
end;

// ******************************************* SQLUpdate ***********************************************
class function TCommonGUIParser.SQLUpdateActionOptionsFromStrToStr(aOptions: string): string;
var
    xValues: TStringArray;
    xPos: integer;
    xOptions: TSQLUpdateActionOptions;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues,
        [STR_OPTION_KEY_USQL_FILE, STR_OPTION_KEY_USQL_PARAMS]) then
    begin

        xPos := 1;
        xOptions.FileName := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);
        xOptions.ArgsStr := '';
        result := SQLUpdateActionOptionsToStr(xOptions);
        EXIT;
    end;
    result := aOptions;
end;

class function TCommonGUIParser.SQLUpdateActionOptionsToStr(aOptions: TSQLUpdateActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_USQL_FILE, STR_OPTION_KEY_USQL_PARAMS],
        [aOptions.FileName, aOptions.ArgsStr]);
end;

// ******************************************* SQLWrite ***********************************************
class function TCommonGUIParser.SQLWriteActionOptionsFromStrToStr(aOptions: string): string;
var
    xValues: TStringArray;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_WRSQL_SQLFILE,
        STR_OPTION_KEY_WRSQL_SQLARGS, STR_OPTION_KEY_WRSQL_OUTFILE, STR_OPTION_KEY_WRSQL_PATH,
        STR_OPTION_KEY_WRSQL_STAMP, STR_OPTION_KEY_WRSQL_SHOW]) then
    begin

        result := SQLWriteActionOptionsToStr(SQLWriteActionOptionsFromStrOld(aOptions));
        EXIT;
    end;
    result := aOptions;
end;

class function TCommonGUIParser.SQLWriteActionOptionsFromStrOld(aOptions: string): TSQLWriteActionOptions;
var
    xOption: string;
    xTemp: string;
    xPos: integer;
begin

    xOption := aOptions;
    xPos := 1;
    result.SQLArgsStr := '';
    result.Show := STR_NO;
    result.TimeStamp := STR_NO;
    result.SQLFileName := gmReadSubStr(xPos, xOption, STR_OPTION_DELIM);
    result.OutputFileName := '';

    xTemp := gmReadSubStr(xPos, xOption, STR_OPTION_DELIM);
    if UPPERCASE(xTemp) = 'SHOW' then
        result.Show := STR_YES;

    xTemp := gmReadSubStr(xPos, xOption, STR_OPTION_DELIM);
    if (UPPERCASE(xTemp) = 'TIMESTAMP') or (UPPERCASE(xTemp) = 'TS') then
        result.TimeStamp := STR_YES;
end;

class function TCommonGUIParser.SQLWriteActionOptionsToStr(aOptions: TSQLWriteActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_WRSQL_SQLFILE, STR_OPTION_KEY_WRSQL_SQLARGS,
        STR_OPTION_KEY_WRSQL_OUTFILE, STR_OPTION_KEY_WRSQL_PATH, STR_OPTION_KEY_WRSQL_STAMP,
        STR_OPTION_KEY_WRSQL_SHOW], [aOptions.SQLFileName, aOptions.SQLArgsStr, aOptions.OutputFileName,
        aOptions.SubFilePath, aOptions.TimeStamp, aOptions.Show]);
end;

// ***************************************** Tube actions *******************************************
class function TCommonGUIParser.TubeActionOptionsFromStrToStr(aOptions: string): string;
var
    xValues: TStringArray;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_TUBE_BITOPTIONS,
        STR_OPTION_KEY_TUBE_TOOL, STR_OPTION_KEY_TUBE_BEFOREGET, STR_OPTION_KEY_TUBE_AFTERGET,
        STR_OPTION_KEY_TUBE_BEFOREPUT, STR_OPTION_KEY_TUBE_AFTERPUT, STR_OPTION_KEY_TUBEBC_BEFOREREAD,
        STR_OPTION_KEY_TUBEBC_AFTERREAD, STR_OPTION_KEY_TUBE_ARM]) then
    begin

        result := TubeActionOptionsToStr(TubeActionOptionsFromStrOld(aOptions));
        EXIT;
    end;
    result := aOptions;
end;

class function TCommonGUIParser.TubeActionOptionsToStr(aOptions: TTubeActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_TUBE_BITOPTIONS, STR_OPTION_KEY_TUBE_TOOL,
        STR_OPTION_KEY_TUBE_BEFOREGET, STR_OPTION_KEY_TUBE_AFTERGET, STR_OPTION_KEY_TUBE_BEFOREPUT,
        STR_OPTION_KEY_TUBE_AFTERPUT, STR_OPTION_KEY_TUBEBC_BEFOREREAD, STR_OPTION_KEY_TUBEBC_AFTERREAD,
        STR_OPTION_KEY_TUBE_ARM], [aOptions.BitOptions, aOptions.ToolName, aOptions.EvBeforeGet,
        aOptions.EvAfterGet, aOptions.EvBeforePut, aOptions.EvAfterPut, aOptions.BCEvBeforeRead,
        aOptions.BCEvAfterRead, aOptions.GripperArmName]);
end;

class function TCommonGUIParser.TubeActionOptionsFromStrOld(aOptions: string): TTubeActionOptions;
var
    strPos: integer;
begin
    strPos := 1;
    // Parameter zuerst aus Options lesen
    result.BitOptions := gmReadSubStr(strPos, aOptions, ',');
    result.ToolName := gmReadSubStr(strPos, aOptions, ',');
    result.GripperArmName := '';

    GetContainerDllCallsOld(aOptions, result.EvBeforeGet, result.EvAfterGet, result.EvBeforePut,
        result.EvAfterPut, result.BCEvBeforeRead, result.BCEvAfterRead);
end;

// **************************************** Device **************************************************
class function TCommonGUIParser.DeviceActionOptionsFromStrToStr(aOptions: string): string;
var
    xValues: TStringArray;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_DEVICE_NAME,
        STR_OPTION_KEY_DEVICE_STATE]) then
    begin

        result := DeviceActionOptionsToStr(DeviceActionOptionsFromStrOld(aOptions));
        EXIT;
    end;
    result := aOptions;
end;

class function TCommonGUIParser.DeviceActionOptionsFromPortActionStr(aOptions: string): TDeviceActionOptions;
var
    xAdr, xPos, xBit: integer;
    xOn: boolean;
    xStr: string;
begin
    xBit := gmIntToStrExt(aOptions);

    // ------------------------------------------------------------ ON oder OFF ?
    xPos := 1;
    xStr := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);
    xOn := false;
    if Pos('ON', UpperCase(xStr)) > 0 then
        xOn := true;
    // ----------------------------------------------------------- PortAdresse angegeben
    xStr := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);

    xAdr := StrToIntDef(xStr, 0);
    result.DeviceName := 'Relay' + IntToStr(xAdr) + 'Port' + IntToStr(xBit);

    if xOn then
        result.State := STR_OPTION_KEY_DEVICE_STATE_ON
    else
        result.State := STR_OPTION_KEY_DEVICE_STATE_OFF;
end;

class function TCommonGUIParser.DeviceActionOptionsFromStrOld(aOptions: string): TDeviceActionOptions;
var
    xPos: integer;
    // xStateText : string;
begin

    xPos := 1;

    result.DeviceName := gmReadSubStr(xPos, aOptions, '=');
    xPos := xPos + 1;
    result.State := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);

    // result.State := OnOffStrToTOnOff( xStateText );
end;

class function TCommonGUIParser.DeviceActionOptionsToStr(aOptions: TDeviceActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_DEVICE_NAME, STR_OPTION_KEY_DEVICE_STATE],
        [aOptions.DeviceName, aOptions.State]);
end;

// **************************************** Flush ***************************************************
class function TCommonGUIParser.FlushActionOptionsFromStrToStr(aOptions: string): string;
var
    xValues: TStringArray;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_FLUSH_CYCLES,
        STR_OPTION_KEY_FLUSH_VOLUME, STR_OPTION_KEY_FLUSH_BITOPTIONS, STR_OPTION_KEY_FLUSH_TOOLSYRMAP,
        STR_OPTION_KEY_FLUSH_TIPMAP, STR_OPTION_KEY_FLUSH_ARM]) then
    begin

        result := FlushActionOptionsToStr(FlushActionOptionsFromStrOld(aOptions));
        EXIT;
    end;
    result := aOptions;
end;

class function TCommonGUIParser.FlushActionOptionsFromStrOld(aOptions: string): TFlushActionOptions;
var
    xBitOptions: integer;
    xBitOptionStr: string;
begin
    aOptions := UpperCase(aOptions);

    // Set Bit Options
    xBitOptions := 0;
    xBitOptionStr := Copy(aOptions, 1, 6);
    if (xBitOptionStr <> 'NOPERI') then
        xBitOptions := xBitOptions or INT_OPTION_KEY_FLUSH_OPT_USEPERI;
    if (xBitOptionStr <> 'NOTCH1') then
        xBitOptions := xBitOptions or INT_OPTION_KEY_FLUSH_OPT_USECH1;
    if (xBitOptionStr <> 'NOTCH2') then
        xBitOptions := xBitOptions or INT_OPTION_KEY_FLUSH_OPT_USECH2;
    result.BitOptions := IntToStr(xBitOptions);

    // Flush Cycles: 2 Ziffern hinter dem '='
    result.Cycles := '0';
    result.Volume := '0';
    if Pos('CYCLES', UpperCase(aOptions)) > 0 then
        result.Cycles := Copy(aOptions, Pos('=', aOptions) + 1, 2);
    if Pos('VOL', Uppercase(aOptions)) > 0 then
        result.Volume := Copy(aOptions, Pos('=', aOptions) + 1, 7);

    result.ToolSyrMap := ''; // alle Tips
    result.TipMap := ''; // keine Add.Dilutors
end;

class function TCommonGUIParser.FlushActionOptionsToStr(aOptions: TFlushActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_FLUSH_CYCLES, STR_OPTION_KEY_FLUSH_VOLUME,
        STR_OPTION_KEY_FLUSH_BITOPTIONS, STR_OPTION_KEY_FLUSH_TOOLSYRMAP, STR_OPTION_KEY_FLUSH_TIPMAP,
        STR_OPTION_KEY_FLUSH_ARM], [aOptions.Cycles, aOptions.Volume, aOptions.BitOptions,
        aOptions.ToolSyrMap, aOptions.TipMap, aOptions.ArmName]);
end;

// **************************************** ParamStore **********************************************
class function TCommonGUIParser.ParamStoreActionOptionsFromStrToStr(aOptions: string): string;
var
    xValues: TStringArray;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_PARAMSTORE_KEY,
        STR_OPTION_KEY_PARAMSTORE_VALUE]) then
    begin

        result := ParamStoreActionOptionsToStr(ParamStoreActionOptionsFromStrOld(aOptions));
        EXIT;
    end;
    result := aOptions;
end;

class function TCommonGUIParser.ParamStoreActionOptionsFromStrOld(aOptions: string): TParamStoreActionOptions;
var
    xPos: integer;
begin
    aOptions := UpperCase(aOptions);
    xPos := 1;
    result.Key := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);
    xPos := xPos + 1;
    result.Value := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM2);
    result.IsOldStyle := true;
end;

class function TCommonGUIParser.ParamStoreActionOptionsToStr(aOptions: TParamStoreActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_PARAMSTORE_KEY, STR_OPTION_KEY_PARAMSTORE_VALUE],
        [aOptions.Key, aOptions.Value]);
end;

// **************************************** CallDllFunc *************************************************
class function TCommonGUIParser.CallDllFuncActionOptionsFromStrToStr(aOptions: string): string;
var
    xValues: TStringArray;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_CALLDLL_CALL]) then
    begin

        result := CallDllFuncActionOptionsToStr(CallDllFuncActionOptionsFromStrOld(aOptions));
        EXIT;
    end;
    result := aOptions;
end;

class function TCommonGUIParser.CallDllFuncActionSingleParamClean(aParam: string): string;
begin
    if Length(aParam) < 2 then
        EXIT(aParam);

    if (Length(aParam) > 2) and (aParam[1] = '&') and (aParam[Length(aParam)] = '&') then
        EXIT(Copy(aParam, 2, Length(aParam) - 2));
    if (aParam[1] = '&') then
        EXIT(Copy(aParam, 2, Length(aParam) - 1));
    if (aParam[Length(aParam)] = '&') then
        EXIT(Copy(aParam, 1, Length(aParam) - 1));
    EXIT(aParam);
end;

class function TCommonGUIParser.CallDllFuncActionParamsClean(aParams: string): string;
var
    x: integer;
    xArr: TArray<string>;
begin
    result := '';
    xArr := TArrayUtils.StringToStringArray(aParams, ',');
    for x := 0 to high(xArr) do
    begin
        if x > 0 then
            result := result + ',';
        result := result + CallDllFuncActionSingleParamClean(xArr[x]);
    end;
end;

class function TCommonGUIParser.CallDllFuncActionOptionsFromStrOld(aOptions: string)
    : TCallDllFuncActionOptions;
var
    xPos: integer;
    xCall: TCallDllStruct;
begin
    xPos := 1;
    xCall.DLLName := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);
    xCall.DLLFunction := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);
    xCall.Parameter := CallDllFuncActionParamsClean(Copy(aOptions, xPos + 1, 254));

    result.DllCall := MakeCallDllString(xCall);
end;

class function TCommonGUIParser.CallDllFuncActionOptionsToStr(aOptions: TCallDllFuncActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_CALLDLL_CALL], [aOptions.DllCall]);
end;

// ******************************************** Delay ***********************************************
class function TCommonGUIParser.DelayActionOptionsFromStrToStr(aOptions: string): string;
var
    xValues: TStringArray;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues,
        [STR_OPTION_KEY_DELAY_TIME, STR_OPTION_KEY_DELAY_CAPTION]) then
    begin

        result := DelayActionOptionsToStr(DelayActionOptionsFromStrOld(aOptions));
        EXIT;
    end;
    result := aOptions;
end;

class function TCommonGUIParser.DelayActionOptionsFromStrOld(aOptions: string): TDelayActionOptions;
var
    xPos: integer;
begin
    xPos := 1;
    result.TimeInSecs := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);
    result.Caption := '';
end;

class function TCommonGUIParser.DelayActionOptionsToStr(aOptions: TDelayActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_DELAY_TIME, STR_OPTION_KEY_DELAY_CAPTION],
        [aOptions.TimeInSecs, aOptions.Caption]);
end;

// ******************************************** VORTEXERTEMPSET ***********************************************
class function TCommonGUIParser.VortexerTempSetActionOptionsFromStrToStr(aOptions: string): string;
var
    xValues: TStringArray;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_VORTEXERTEMPSET_DEVICE,
        STR_OPTION_KEY_VORTEXERTEMPSET_TEMPERATURE]) then
    begin

        result := VortexerTempSetActionOptionsToStr(VortexerTempSetActionOptionsFromStrOld(aOptions));
        EXIT;
    end;
    result := aOptions;
end;

class function TCommonGUIParser.VortexerTempSetActionOptionsFromStrOld(aOptions: string)
    : TVortexerTempSetActionOptions;
var
    xPos: integer;
begin
    xPos := 1;
    result.DeviceName := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);
    result.Temperature := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);
end;

class function TCommonGUIParser.VortexerTempSetActionOptionsToStr
    (aOptions: TVortexerTempSetActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_VORTEXERTEMPSET_DEVICE,
        STR_OPTION_KEY_VORTEXERTEMPSET_TEMPERATURE], [aOptions.DeviceName, aOptions.Temperature]);
end;

// ******************************************** VORTEXERTEMPCHECK*******************************************
class function TCommonGUIParser.VortexerTempCheckActionOptionsFromStrToStr(aOptions: string): string;
var
    xValues: TStringArray;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_VORTEXERTEMPCHECK_DEVICE,
        STR_OPTION_KEY_VORTEXERTEMPCHECK_MINTEMPERATURE, STR_OPTION_KEY_VORTEXERTEMPCHECK_MAXTEMPERATURE,
        STR_OPTION_KEY_VORTEXERTEMPCHECK_REQUIREDTIME]) then
    begin

        result := VortexerTempCheckActionOptionsToStr(VortexerTempCheckActionOptionsFromStrOld(aOptions));
        EXIT;
    end;
    result := aOptions;
end;

class function TCommonGUIParser.VortexerTempCheckActionOptionsFromStrOld(aOptions: string)
    : TVortexerTempCheckActionOptions;
var
    xPos: integer;
begin
    xPos := 1;
    result.DeviceName := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);
    result.MinTemperature := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);
    result.MaxTemperature := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);
    result.RequiredTimeInSecs := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);
end;

class function TCommonGUIParser.VortexerTempCheckActionOptionsToStr
    (aOptions: TVortexerTempCheckActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_VORTEXERTEMPCHECK_DEVICE,
        STR_OPTION_KEY_VORTEXERTEMPCHECK_MINTEMPERATURE, STR_OPTION_KEY_VORTEXERTEMPCHECK_MAXTEMPERATURE,
        STR_OPTION_KEY_VORTEXERTEMPCHECK_REQUIREDTIME], [aOptions.DeviceName, aOptions.MinTemperature,
        aOptions.MaxTemperature, aOptions.RequiredTimeInSecs]);
end;

// ******************************************** VortexerSpeed ***********************************************
class function TCommonGUIParser.VortexerSpeedActionOptionsFromStrToStr(aOptions: string): string;
var
    xValues: TStringArray;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_VORTEXERSPEED_DEVICE,
        STR_OPTION_KEY_VORTEXERSPEED_SPEED, STR_OPTION_KEY_VORTEXERSPEED_ONPULSE,
        STR_OPTION_KEY_VORTEXERSPEED_OFFPULSE, STR_OPTION_KEY_VORTEXERSPEED_WAIT]) then
    begin

        result := VortexerSpeedActionOptionsToStr(VortexerSpeedActionOptionsFromStrOld(aOptions));
        EXIT;
    end;
    result := aOptions;
end;

class function TCommonGUIParser.VortexerSpeedActionOptionsFromStrOld(aOptions: string)
    : TVortexerSpeedActionOptions;
var
    xPos: integer;
begin
    xPos := 1;
    result.DeviceName := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);
    result.Speed := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);
    result.OnPulse := '0';
    result.OffPulse := '0';
    result.WaitFor := STR_OPTION_KEY_VORTEXERSPEED_WAIT_IFSPEEDISNULL;
end;

class function TCommonGUIParser.VortexerSpeedActionOptionsToStr
    (aOptions: TVortexerSpeedActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_VORTEXERSPEED_DEVICE,
        STR_OPTION_KEY_VORTEXERSPEED_SPEED, STR_OPTION_KEY_VORTEXERSPEED_ONPULSE,
        STR_OPTION_KEY_VORTEXERSPEED_OFFPULSE, STR_OPTION_KEY_VORTEXERSPEED_WAIT],
        [aOptions.DeviceName, aOptions.Speed, aOptions.OnPulse, aOptions.OffPulse, aOptions.WaitFor]);
end;

// ******************************************** VortexerFixation ***********************************************
class function TCommonGUIParser.VortexerFixationToDeviceActionOptionsFromStrToStr(aOptions: string): string;
var
    xOptions1: TVortexerFixationActionOptions;
    xOptions2: TDeviceActionOptions;
begin
    xOptions1 := VortexerFixationActionOptionsFromStr(aOptions);

    xOptions2.DeviceName := xOptions1.DeviceName + 'Fixation';
    xOptions2.State := xOptions1.State;

    EXIT(DeviceActionOptionsToStr(xOptions2));
end;

class function TCommonGUIParser.VortexerFixationActionOptionsFromStr(aOptions: string)
    : TVortexerFixationActionOptions;
var
    xValues: TStringArray;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_VORTEXERFIXATION_DEVICE,
        STR_OPTION_KEY_VORTEXERFIXATION_STATE]) then
    begin

        result := VortexerFixationActionOptionsFromStrOld(aOptions);
        EXIT;
    end;
    result.DeviceName := xValues[0];
    result.State := xValues[1];
end;

class function TCommonGUIParser.VortexerFixationActionOptionsFromStrOld(aOptions: string)
    : TVortexerFixationActionOptions;
var
    xPos: integer;
begin
    xPos := 1;
    result.DeviceName := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);
    result.State := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM);
end;

// ******************************************** FileCopy ***********************************************
class function TCommonGUIParser.FileCopyActionOptionsFromStrToStr(aOptions: string): string;
var
    xValues: TStringArray;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_FILECOPY_SOURCEFILE,
        STR_OPTION_KEY_FILECOPY_SOURCEPATH, STR_OPTION_KEY_FILECOPY_DESTFILE,
        STR_OPTION_KEY_FILECOPY_DESTPATH, STR_OPTION_KEY_FILECOPY_OVERWRITE,
        STR_OPTION_KEY_FILECOPY_DELETESRC]) then
    begin

        result := FileCopyActionOptionsToStr(FileCopyActionOptionsFromStrOld(aOptions));
        EXIT;
    end;
    result := aOptions;
end;

class function TCommonGUIParser.FileCopyActionOptionsFromStrOld(aOptions: string): TFileCopyActionOptions;
var
    xPos: integer;
begin
    xPos := 1;
    result.SourceFile := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM); // Dateiname
    result.SourcePath := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM); // Quellpfad
    result.DestFile := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM); // ZielDateiName
    result.DestPath := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM); // Zielpfad
    result.Overwrite := '';
    result.DeleteSource := '';
end;

class function TCommonGUIParser.FileCopyActionOptionsToStr(aOptions: TFileCopyActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_FILECOPY_SOURCEFILE,
        STR_OPTION_KEY_FILECOPY_SOURCEPATH, STR_OPTION_KEY_FILECOPY_DESTFILE,
        STR_OPTION_KEY_FILECOPY_DESTPATH, STR_OPTION_KEY_FILECOPY_OVERWRITE,
        STR_OPTION_KEY_FILECOPY_DELETESRC], [aOptions.SourceFile, aOptions.SourcePath, aOptions.DestFile,
        aOPtions.DestPath, aOptions.Overwrite, aOptions.DeleteSource]);
end;

// ******************************************** TimerSet ***********************************************
class function TCommonGUIParser.TimerSetActionOptionsFromStrToStr(aOptions: string): string;
var
    xValues: TStringArray;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_TIMERSET_TIMERNAME,
        STR_OPTION_KEY_TIMERSET_WAITTIME]) then
    begin

        result := TimerSetActionOptionsToStr(TimerSetActionOptionsFromStrOld(aOptions));
        EXIT;
    end;
    result := aOptions;
end;

class function TCommonGUIParser.TimerSetActionOptionsFromStrOld(aOptions: string): TTimerSetActionOptions;
var
    xPos: integer;
begin
    xPos := 1;
    result.TimerName := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM); // Dateiname
    result.WaitTimeInSec := gmReadSubStr(xPos, aOptions, STR_OPTION_DELIM); // Quellpfad
end;

class function TCommonGUIParser.TimerSetActionOptionsToStr(aOptions: TTimerSetActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_TIMERSET_TIMERNAME,
        STR_OPTION_KEY_TIMERSET_WAITTIME], [aOptions.TimerName, aOptions.WaitTimeInSec]);
end;

// ******************************************** TimerWait *******************************************
class function TCommonGUIParser.TimerWaitActionOptionsFromStrToStr(aOptions: string): string;
var
    xValues: TStringArray;
    xOptions: TTimerWaitActionOptions;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_TIMERWAIT_TIMERNAME,
        STR_OPTION_KEY_TIMERWAIT_WAITTIME, STR_OPTION_KEY_TIMERWAIT_TEXT]) then
    begin

        xOptions.TimerName := aOptions;
        xOptions.WaitTimeInSec := '';
        xOptions.DialogText := '';
        result := TimerWaitActionOptionsToStr(xOptions);
        EXIT;
    end;
    result := aOptions;
end;

class function TCommonGUIParser.TimerWaitActionOptionsToStr(aOptions: TTimerWaitActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_TIMERWAIT_TIMERNAME,
        STR_OPTION_KEY_TIMERWAIT_WAITTIME, STR_OPTION_KEY_TIMERWAIT_TEXT],
        [aOptions.TimerName, aOptions.WaitTimeInSec, aOptions.DialogText]);
end;

// ******************************************* AddMethod ***********************************************

class function TCommonGUIParser.AddMethodActionOptionsParamsClear(aParams: string): string;
var
    x: integer;
    xList: TStringList;
    xKeyVal: string;
begin
    result := '';
    xList := TStringList.Create;
    try
        self.OptionsStrToList(xList, aParams);
        for x := 0 to xList.Count - 1 do
        begin
            if x > 0 then
                result := result + ',';

            xKeyVal := xList[x];

            xKeyVal := StringReplace(xKeyVal, '=&', '=', []);

            if (xKeyVal[Length(xKeyVal)] = '&') then
                xKeyVal := Copy(xKeyVal, 1, Length(xKeyVal) - 1);

            xList[x] := xKeyVal;
        end;
        EXIT(self.ListToOptionStr(xList));
    finally
        FreeAndNil(xList);
    end;
end;

class function TCommonGUIParser.AddMethodActionOptionsFromStrToStr(aOptions: string): string;
var
    xOptions: TAddMethodActionOptions;
begin
    xOptions := AddMethodActionOptionsFromStr(aOptions);
    xOptions.Params := AddMethodActionOptionsParamsClear(xOptions.Params);
    EXIT(self.AddMethodActionOptionsToStr(xOptions));
end;

class function TCommonGUIParser.AddMethodActionOptionsFromStr(aOptions: string): TAddMethodActionOptions;
var
    xValues: TStringArray;
begin
    FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_ADDM_NAME, STR_OPTION_KEY_ADDM_PARAMS,
        STR_OPTION_KEY_ADDM_READ, STR_OPTION_KEY_ADDM_GROUPID]);

    result.Name := xValues[0];
    result.Params := xValues[1];
    result.ReadMode := xValues[2];
    result.GroupID := xValues[3];
end;

class function TCommonGUIParser.AddMethodActionOptionsToStr(aOptions: TAddMethodActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_ADDM_NAME, STR_OPTION_KEY_ADDM_PARAMS,
        STR_OPTION_KEY_ADDM_READ, STR_OPTION_KEY_ADDM_GROUPID], [aOptions.Name, aOptions.Params,
        aOptions.ReadMode, aOptions.GroupID]);
end;

// ******************************************** Rack Actions ****************************************
class function TCommonGUIParser.RackActionOptionsFromStrToStr(aOptions: string): string;
var
    xValues: TStringArray;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_RACK_DESTINATION,
        STR_OPTION_KEY_RACK_BITOPTIONS, STR_OPTION_KEY_RACK_BEFOREGET, STR_OPTION_KEY_RACK_AFTERGET,
        STR_OPTION_KEY_RACK_BEFOREPUT, STR_OPTION_KEY_RACK_AFTERPUT, STR_OPTION_KEY_RACKBC_BEFOREREAD,
        STR_OPTION_KEY_RACKBC_AFTERREAD, STR_OPTION_KEY_RACK_ARM]) then
    begin

        result := RackActionOptionsToStr(RackActionOptionsFromStrOld(aOptions));
        EXIT;
    end;
end;

function gmParseOldSlotStr(aSlotString: string): TSlotStructStr;
// ------------------------------------------------------------------------------
// Liest das Format: 'Carrier=                         Slot=    '
// ------------------------      ----
// ------------------------------------------------------------------------------
var
    StartC, StartS, StartR: integer;
begin

    StartC := Pos('Carrier=', aSlotString);
    StartS := Pos('Slot=', aSlotString);
    StartR := Pos('Rotation=', aSlotString);

    if (StartC > 0) then
        result.Carrier := Trim(Copy(aSlotString, StartC + 8, StartS - StartC - 9));

    if (StartS > 0) then
    begin
        result.Slot := Trim(Copy(aSlotString, StartS + 5, 4));
    end;

    if (StartR > 0) then
    begin
        result.Rotation := Trim(Copy(aSlotString, StartR + 9, 3));
    end;
end;

class function TCommonGUIParser.RackActionOptionsFromStrOld(aOptions: string): TRackActionOptions;
var
    xSlotStr: TSlotStructStr;
begin
    xSlotStr := gmParseOldSlotStr(aOptions);
    result.Destination := MakeSlotString(xSlotStr);
    result.BitOptions := '';
    result.GripperArmName := '';

    GetContainerDllCallsOld(aOptions, result.EvBeforeGet, result.EvAfterGet, result.EvBeforePut,
        result.EvAfterPut, result.BCEvBeforeRead, result.BCEvAfterRead);
end;

class function TCommonGUIParser.RackActionOptionsToStr(aOptions: TRackActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_RACK_DESTINATION, STR_OPTION_KEY_RACK_BITOPTIONS,
        STR_OPTION_KEY_RACK_BEFOREGET, STR_OPTION_KEY_RACK_AFTERGET, STR_OPTION_KEY_RACK_BEFOREPUT,
        STR_OPTION_KEY_RACK_AFTERPUT, STR_OPTION_KEY_RACKBC_BEFOREREAD, STR_OPTION_KEY_RACKBC_AFTERREAD,
        STR_OPTION_KEY_RACK_ARM], [aOptions.Destination, aOptions.BitOptions, aOptions.EvBeforeGet,
        aOptions.EvAfterGet, aOptions.EvBeforePut, aOptions.EvAfterPut, aOptions.BCEvBeforeRead,
        aOptions.BCEvAfterRead, aOptions.GripperArmName]);
end;

// ******************************************** Message ***********************************************
class function TCommonGUIParser.MessageActionOptionsFromStrToStr(aOptions: string): string;
var
    xValues: TStringArray;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_MESSAGE_TEXT,
        STR_OPTION_KEY_MESSAGE_PAUSERUN]) then
    begin

        result := MessageActionOptionsToStr(MessageActionOptionsFromStrOld(aOptions));
        EXIT;
    end;
    result := aOptions;
end;

class function TCommonGUIParser.MessageActionOptionsFromStrOld(aOptions: string): TMessageActionOptions;
begin
    result.Text := aOptions;
    result.PauseRun := '';
end;

class function TCommonGUIParser.MessageActionOptionsToStr(aOptions: TMessageActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_MESSAGE_TEXT, STR_OPTION_KEY_MESSAGE_PAUSERUN],
        [aOptions.Text, aOptions.PauseRun]);
end;

// ************************************ Pipette Action (Kür) ****************************************
class function TCommonGUIParser.PipetteActionOptionsFromStrToStr(aOptions: string): string;
var
    xValues: TStringArray;
begin
    if not FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_PIP_EVBEFOREASP,
        STR_OPTION_KEY_PIP_EVBEFOREPICKLQ, STR_OPTION_KEY_PIP_EVAFTERPICKLQ, STR_OPTION_KEY_PIP_EVAFTERASP,
        STR_OPTION_KEY_PIP_EVBEFOREDISP, STR_OPTION_KEY_PIP_EVBEFOREDISPLQ, STR_OPTION_KEY_PIP_EVAFTERDISPLQ,
        STR_OPTION_KEY_PIP_EVAFTERDISP]) then
    begin

        result := PipetteActionOptionsToStr(PipetteActionOptionsFromStrOld(aOptions));
        EXIT;
    end;
    result := aOptions;
end;

class function TCommonGUIParser.PipetteActionOptionsFromStrOld(aOptions: string): TPipetteActionOptions;
begin
    GetAspirateDllCallsOld(aOptions, result.AspS.BeforeAsp, result.AspS.BeforePickLq, result.AspS.AfterPickLq,
        result.AspS.AfterAsp);

    GetDispenseDllCallsOld(aOptions, result.Disp.BeforeDispense, result.Disp.BeforeDispLq,
        result.Disp.AfterDispLq, result.Disp.AfterDispense);
end;

class function TCommonGUIParser.PipetteActionOptionsToStr(aOptions: TPipetteActionOptions): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_KEY_PIP_EVBEFOREASP,
        STR_OPTION_KEY_PIP_EVBEFOREPICKLQ, STR_OPTION_KEY_PIP_EVAFTERPICKLQ, STR_OPTION_KEY_PIP_EVAFTERASP,
        STR_OPTION_KEY_PIP_EVBEFOREDISP, STR_OPTION_KEY_PIP_EVBEFOREDISPLQ, STR_OPTION_KEY_PIP_EVAFTERDISPLQ,
        STR_OPTION_KEY_PIP_EVAFTERDISP], [aOptions.AspS.BeforeAsp, aOptions.AspS.BeforePickLq,
        aOptions.AspS.AfterPickLq, aOptions.AspS.AfterAsp, aOptions.Disp.BeforeDispense,
        aOptions.Disp.BeforeDispLq, aOptions.Disp.AfterDispLq, aOptions.Disp.AfterDispense]);
end;

// ************ Generic Import Options (MethodImport)
const
    // Generic Import Method Step Options
    STR_OPTION_KEY_METHIMP_PREFIX = 'IMP';
    STR_OPTION_KEY_METHIMP_NAME = STR_OPTION_KEY_METHIMP_PREFIX + 'NAME';
    STR_OPTION_KEY_METHIMP_FILTER = STR_OPTION_KEY_METHIMP_PREFIX + 'FILTER';
    STR_OPTION_KEY_METHIMP_CURSORMOVE = STR_OPTION_KEY_METHIMP_PREFIX + 'CURSORMOVE';
    STR_OPTION_KEY_METHIMP_ALLRECS = STR_OPTION_KEY_METHIMP_PREFIX + 'ALLRECS';
    STR_OPTION_KEY_METHIMP_SOURCEFILE = STR_OPTION_KEY_METHIMP_PREFIX + 'SOURCEFILE';

class function TCommonGUIParser.GenericImportOptionsFromStr(aOptions: string): TGenericImportOptions;
var
    xValues: TStringArray;
begin
    FindValuesForKeysInOptions(aOptions, xValues, [STR_OPTION_KEY_METHIMP_NAME, STR_OPTION_KEY_METHIMP_FILTER,
        STR_OPTION_KEY_METHIMP_CURSORMOVE, STR_OPTION_KEY_METHIMP_ALLRECS,
        STR_OPTION_KEY_METHIMP_SOURCEFILE]);

    result.DefName := xValues[0];
    result.Filter := xValues[1];
    result.CursorMove := xValues[2];
    result.AllRecs := xValues[3];
    result.SourceFileName := xValues[4];
end;

class function TCommonGUIParser.GenericImportOptionsAddToStr(const aOptionStr: string;
    aImportOptions: TGenericImportOptions): string;
begin
    result := AddKeysAndValuesToOptions(aOptionStr, [STR_OPTION_KEY_METHIMP_NAME,
        STR_OPTION_KEY_METHIMP_FILTER, STR_OPTION_KEY_METHIMP_CURSORMOVE, STR_OPTION_KEY_METHIMP_ALLRECS,
        STR_OPTION_KEY_METHIMP_SOURCEFILE], [aImportOptions.DefName, aImportOptions.Filter,
        aImportOptions.CursorMove, aImportOptions.AllRecs, aImportOptions.SourceFileName]);
end;


end.
