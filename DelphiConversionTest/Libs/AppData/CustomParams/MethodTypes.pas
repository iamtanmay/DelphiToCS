{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Project      : New Editor
  Author       : Wolfgang Lyncke (wl)
  Description  : allconstants and types from Method GUI Parsing
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  04.11.04 wl                               TN2213    initial version
  08.11.04 wl  TContainerActionDllCalls     TN2213    fasst alle Events für Rack- und Tube-Handling zusammen
  08.11.04 wl  TPipetteActionRunRec         TN2213    enthält bisher nur Events für TPipetteAction
  08.11.04 wl  gmParse/MakeDllCallStruct    TN2213    --> MethodGUIParsing
  16.11.04 pk                               TN2231    Types for TRunStartAction
  22.11.04 wl                               TN2213    viele neue Option/RunRec-records für MethodGUIParsing
  22.11.04 wl  TRunMarkerActionRunRec       TN2213    entspricht TRunMarkerActionOptions, Options: string statt TRunMarker
  22.11.04 pk                               TN2237    type for new WASHT ( TWashTipsAction ) action
  24.11.04 wl  STR_ACTION_NAME_PIPETTE      TN2213    aufgeteilt in STR_ACTION_NAME_PIPETTE_OLD und STR_ACTION_NAME_PIPETTE_NEW
  25.11.04 pk  TBuildConfigActionRunRec     TN2240    New
  07.12.04 wl                               TN2247.1  neu: TActionListEntry
  19.01.05 pk                               TN2281.0  TStringArray, TIntArray from AppTypes to GeneralTypes
  15.02.05 pk  STR_ACTION_NAME_GROUP        TN2314.1  New
  18.02.05 pk                               TN2322    types for DEVFU Action
  04.03.05 pk                               TN2330.1  uses LiqHDataAdaptor
  08.03.05 pk                               TN2337    New constants STR_CALLDLL_TYPE_COMMAND, STR_CALLDLL_TYPE_RUN
  19.04.05 pk  Group                        TN2390,TN2391 New constants for ADDM/GROUP action options
  20.04.05 wl  TFlushActionRunRec           TN2379    Flush-Action um 2 Parameter erweitert
  20.04.05 wl                               TN2377    neue Actions 'CALLI' und 'MASSP'
  20.04.05 pk                               TN2390    New GROUPID constants STR_GROUPID_BEGIN, STR_GROUPID_END, ...
  04.05.05 wl  STR_ACTION_NAME_MANUALFILL   TN2410    "MANUAL"-Schritt als eigene Action
  15.06.05 pk  Flush, MoveZ, WashProg       TN2464.1   new ArmName constant
  15.06.05 pk  TPipetteActionRunRec         TN2464.2   PipOptions, CalliType, WorkType, etc
  20.06.05 wl  STR_COMMENT_IDENT_..         TN2441     für das neue Feld "Comment"
  24.06.05 wl  MessageWithBeeper-Records    TN2459    entfernt
  18.08.05 pk                               TN2566.0  new pip options : STR_OPTION_PIP_SOURCEVOL, STR_OPTION_PIP_DTRANSAIR
  22.08.05 pk  RunLoad                      TN2546    New option : DoRepeat
  26.08.05 pk                               TN2566    New : constants for Aspirate and Dispense actions
  14.09.05 wl  STR_ACTION_NAME_BALANCEWAIT  TN2570    Action, die auf das Ende einer Einwaage wartet
  23.09.05 wl  STR_OPTION_SUBKEY_CALLDLL_STORENAME  TN2627   neu
  11.10.05 wl  STR_ACTION_NAME_GRIPPERTOOLGET       TN2658    neue Rack-Action TOOLG
  20.10.05 wl  TMethodActionGroup                   TN2659   Unterscheidung MoveTubeAction und SimpleTubeAction
  20.10.05 wl  STR_ACTION_NAME_TIPSGET              TN2659   neue Action zum Aufnehmen von Spitzen
  20.10.05 wl  STR_ACTION_NAME_GRIPPERTOOLRETURN    TN2660   neue Action zum Abwerfen von Spitzen
  03.11.05 wl  TGenericImportOptions                TN2724   Import-Optionen für alle Actions außer IMPOR-Action
  03.11.05 wl  STR_OPTION_KEY_METHIMP...            TN2724   Import-Optionen für alle Actions außer IMPOR-Action
  03.11.05 wl  STR_OPTION_KEY_IMPIN...              TN2724   Import-Optionen für IMPOR-Action ("Import into table")
  03.11.05 wl  STR_OPTION_KEY_METHIMP_ALLRECS       TN2722   neue Import-Option: Alle Records/Zeilen importieren
  03.11.05 wl  STR_OPTION_KEY_METHIMP_SOURCEFILE    TN2723   neue Import-Option: Dateiname aus ImportFileDef ersetzen
  08.11.05 wl  STR_OPTION_KEY_XYMOVEMENT_..         TN2728   für neue Action XYMovement
  08.11.05 wl  STR_OPTION_KEY_ZPOSMOVEMENT_..       TN2728   für neue Action ZPosMovement
  08.11.05 wl  STR_OPTION_KEY_GENBUILD_             TN2728   Generic Build Options: Werden nur für Build verwendet, im Run nicht mehr
  08.11.05 wl  TDeviceActionRunRec                  TN2435   fState ist vom typ TSwitchState
  17.11.05 wl  TTubeActionRunRec/Options            TN2771   neuer Parameter GripperArmName
  17.11.05 wl  TRackActionRunRec/Options            TN2771   neuer Parameter GripperArmName
  20.11.05 wl  STR_ACTION_NAME_CHANGETIPS           TN2784   für neue Action ChangeTips
  23.11.05 wl  TZPosMovementActionRunRec            TN2786   ZSubmerge ist extended statt integer
  28.11.05 wl  STR_OPTION_KEY_DEVICE_STATE_BOTH     TN2812   = 'BOTH';
  15.12.05 wl  FileCopyAction..                     TN2856   neue Parameter Overwrite, DeleteSource
  02.01.06 wl  TSQLWriteFileAction                  TN2876    neuer Parameter OutputFileName
  18.01.06 wl  STR_ACTION_NAME_SQLSELECT            TN2885   neue Action SLSQL
  18.01.06 wl  TSQLSelectFileAction                 TN2885   neuer Record für SLSQL
  20.01.06 pk  TCalliTubeRunRec                     TN2891   new for calli
  02.02.06 wl  TSQLSelectFileAction                 TN2885.1  neu: DefaultValues
  28.02.06 thr WeighPosition-,Tare-,WaitBAction     TN2941   new Balancename
  15.03.06 wl  TZPosMovementAction                  TN2966   neu: DisableError
  25.03.06 pk  TRunLoadActionOptions                TN2998   New Parameter: NumLinesToRepeat, repeat more than just the RUN line
  25.03.06 pk  TInitDeviceActionOptions             TN2997   New
  29.03.06 wl  TXYMovementAction...                 TN3005   Neue Parameter: XSpeed,xRamp,YSpeed,YRamp
  29.03.06 wl  TZPosMovementAction...               TN3006   Neuer Parameter: ZRamp
  03.04.06 thr WeighPosition- and ,TareAction       TN3007   neue Parameter Deviation, NumValues, Target
  24.04.06 pk  TParamStoreActionOptions             TN3058   New parameter IsOldStyle - true if oldstyle of parsing is used
  06.06.06 wl  TTimerWait                           TN3128   neue Parameter
  14.07.06 pk  TActionListEntry                     TN3164   new parameter : SessionName
  04.09.06 pk  STR_GROUP_ID                         TN3280   New constant
  07.09.06 wl  TWeighWizardActionOptions            TN3287   neu: unterscheidet sich jetzt von TPipetteActionOptions
  07.09.06 wl                                       TN3288   Options für Calli action entfernt
  07.09.06 wl  STR_ACTION_NAME_WEIGHWIZARD          TN3287    Neu: WGWIZ statt MASSP !!!
  12.09.06 wl                                       TN3285   uses LiqClassDataAdaptor
  18.09.06 pk  STR_OPTION_KEY_VORTEXERF.._STATE_ON  TN3248   New constant
  20.09.06 wl  STR_ACTION_NAME_MOTORMOVE            TN3318   neu: MOTOR action
  20.09.06 wl  TMotorMoveActionOptions              TN3318   enthält schon alle notwendigen Parameter
  31.10.06 pk  TMessageActionOptions                TN3391    New parameter: PauseRun
  04.11.06 wl  TSensorCheckActionOptions            TN3394    Neue Action: CSENS
  27.11.06 wl  TWeighWizardAction..                 TN3362   neu: TeachVolume & TeachParam
  27.11.06 wl  TWeighWizardActionOptions            TN3419    neuer Parameter UseLastWeightAsTare
  28.11.06 wl  ImportIntoTableActionOptions         TN3397    Import-Action: Parameter nur noch für neuen Import
  05.12.06 wl                                       TN3448    Actions WASH und WASH? entfernt
  18.01.07 wl  STR_OPTION_KEY_VORTEXERSPEED_WAIT    TN3507    VortexerSpeed: neuer Parameter WaitFor
  18.01.07 pk  STR_ACTION_NAME_BUILDSEQUENCE        TN3482   New
  19.02.07 wl  TVirtualRackMoveAction...            TN3585   neu: Action VRMOV
  21.02.07 wl  TTipsGetActionOptions                TN3588    neu
  12.03.07 pk  TRackMoveAction                      TN3629    new RackMoveOptions
  17.07.07 pk  STR_OPTION_KEY_ADSEQ_OPTIMIZATION    TN3653   New constants for ADSEQ
  25.07.07 wl  TWashProgActionOptions               TN3792   jetzt mit UsedTips und UsedTipType
  26.07.07 pk  TCommandActionOptions                TN3805   New
  30.08.07 pk                                       TN3840.2 Uses LiqHTypes
  31.08.07 wl  TPowderDetectionAction               TN3844   Neue Action: PWDET
  08.01.08 wl                                       TN3972   viele Action-spezifische Typen entfernt
  08.01.08 wl  INT_IM_INDEX_..                      TN3972   von ViewItems hierher
  08.01.08 wl  STR_OPTION_KEY_PIPBASICS_            TN3972   neu: statt der Method.db-Felder wird jetzt geparst
  08.01.08 wl                                       TN3972   endgültig entfernt: CANCM,CHKE,EDITL,EXPRA,IMPRA,PHOTO,PORT,QUADR,READE,WFSLA
  29.01.08 wl                                       TN3980   MREAD, RWRIT, TRIG
  07.02.08 wl                                       TN4009    Trennung: Scheduling Options und Iterate
  27.02.08 wl  TZTipMovementActionOptions           TN4011    new action ZTIPM
  01.04.08 wl  STR_Action_Name_PumpAspirate/Dispense TN4050    neu
  01.04.08 wl  STR_ACTION_NAME_MOTORMOVE            TN4051    ist jetzt MMOVE, 'MOTOR' ist jetzt STR_ACTION_NAME_VolMotor
  03.07.08 wl                                         TN4157
  11.07.08 wl  INT_IM_INDEX_Workspace,..              TN4164    für neue Nodes im Designer: Workspace, Rack, Carrier, TipType
  21.07.08 pk  STR_ACTION_NAME_ChangeCarrierType    TN4179   New
  31.07.08 pk  STR_ACTION_NAME_ChangeRackType       TN4193   New
  20.09.08 pk                                       TN4215    uneeded types, constants removed
  25.09.08 wl                                       TN4242    CALLDLL-Konstanten entfernt
  06.10.08 pk  RunStartOptions                      TN4258   New
  15.10.08 pk  RunStartOptions                      TN4258   Moved to AppTypes
  25.11.08 wl  TDialogInputKeyParamArray            TN4310    new
  06.04.09 pk  INT_IM_INDEX_DisplayComponent        TN4503   New
  04.12.10 pk  TDialogInputKeyValueList             TN5049   New
  31.05.10 wl  INT_ACTIONNAME_MAX_LENGTH            TN5120   entfernt
  09.06.10 wl  INT_IM_INDEX_FAVOURITES              TN5116   neu
  29.09.10 pk                                       TN5283   Short and Long method step comments combined
  08.10.10 pk                                       TN5295   New Icons
  06.12.10 pk                                       TN5382   New Icon
  11.02.11 wl                                       TN5474   ImageIndex-Konstanten bereinigt
  23.02.11 wl                                       TN5486   neue Icons
  30.06.11 wl                                       TN5620   neue Icons
  20.07.11 wl                                       TN5614.1 neue Icons
  29.08.11 wl                                       TN5614.2 neue Icons
  14.12.11 wl                                       TN5765   aufgeräumt
  01.03.12 wl  TRunStartOptions                     TN5822   von AppTypes hierher
  30.03.12 wl  cImageIndexAction...                 TN5852   neue Icons
  13.09.12 wl  TDialogInputKeyValueItem             TN5981   --> DialogInputRunStep
  14.11.12 wl                                       TN6018   Neue Icons für IF, FOR, WHILE
  30.11.12 pp  Add new const                        TN6039   cImageIndexActionUserProtection = 83, new Icon
  11.02.13 wl  TStoreOrDelete                       TN6078   neu
  11.02.13 wl  TMethodRackNameType                  TN6078   von RunStepBuilderHelper hierher
  07.03.13 ts                                       TN6092   neu INT_IM_INDEX_STARTABLEMETHOD
  11.04.13 wl  TKeyValueParam                       TN6045   von GeneralTypes hierher
  12.07.13 ts                                       TN6200   new Icons for RETURN - cImageIndexActionReturn
  03.04.14 ts                                       TN6387   new cImageIndexMethodEditableInRunner
  -------------------------------------------------------------------------------------------------- }

unit MethodTypes;


interface


uses
    Streamable;

const
    STR_OPTION_DELIM = ',';
    STR_OPTION_DELIM2 = ';';

    INT_IM_INDEX_UNKNOWN = -999;

    // Entspricht den Indices der ZADesignMain-ImageList

    // View Items:
    INT_IM_INDEX_FOLDER = 0;
    INT_IM_INDEX_METHOD = 1;
    INT_IM_INDEX_SEQUENCE = 2;
    INT_IM_INDEX_WASHPROG = 3;
    INT_IM_INDEX_LAYOUT = 4;
    INT_IM_INDEX_ACTION = 5;
    INT_IM_INDEX_TipType = 6;
    INT_IM_INDEX_LIQUIDPAR = 7;
    INT_IM_INDEX_POWDERPAR = 8;
    INT_IM_INDEX_SQLTERM = 9;
    INT_IM_INDEX_DEVICE = 10;
    INT_IM_INDEX_DRIVER = 11;
    INT_IM_INDEX_CONNECTION = 12;
    INT_IM_INDEX_Workspace = 13;
    INT_IM_INDEX_Carrier = 14;
    INT_IM_INDEX_Rack = 15;
    INT_IM_INDEX_REAGENT = 16;
    INT_IM_INDEX_REAGENTRACK = 17;
    INT_IM_INDEX_DisplayComponent = 18;
    INT_IM_INDEX_FAVOURITES = 19;
    INT_IM_INDEX_FAVFOLDER = 20;

    INT_IM_INDEX_ImportFileDef = 21;
    INT_IM_INDEX_TableImportDef = 22;
    INT_IM_INDEX_VarImportDef = 23;

    // Actions die fremde Icons benutzen:
    cImageIndexActionDataset = 23;
    cImageIndexActionImportIntoTable = 22;
    INT_IM_INDEX_ACTION_REMARK = 5;

    // Actions:
    INT_IM_INDEX_ACTION_UNKNOWN = 24;
    INT_IM_INDEX_ACTION_DEVICE = 25;

    // Container actions
    INT_IM_INDEX_ACTION_GRIPO = 26;
    INT_IM_INDEX_ACTION_GRIPC = 27;
    INT_IM_INDEX_ACTION_MOVER = 28;
    INT_IM_INDEX_ACTION_TOOLG = 29;
    INT_IM_INDEX_ACTION_TOOLR = 30;
    cImageIndexActionReadBarcode = 31;
    cImageIndexActionReadTube = 32;
    cImageIndexActionMoveTube = 33;
    // 34-35 for container actions

    INT_IM_INDEX_ACTION_XYMOV = 36;
    INT_IM_INDEX_ACTION_ZPOSM = 37;
    cImageIndexActionDialogMessage = 38;
    INT_IM_INDEX_ACTION_SQLTERM = 39;

    INT_IM_INDEX_ACTION_INIT = 40;
    INT_IM_INDEX_ACTION_DELAY = 41;
    INT_IM_INDEX_ACTION_TIMERSET = 42;
    INT_IM_INDEX_ACTION_TIMERWAIT = 43;
    cImageIndexActionThreadStart = 44;

    INT_IM_INDEX_ACTION_PIPET = 45;
    INT_IM_INDEX_ACTION_ASP = 46;
    INT_IM_INDEX_ACTION_DSP = 47;
    cImageIndexActionChangeTips = 48;
    cImageIndexActionSqlWrite = 49;
    cImageIndexActionSqlUpdate = 50;

    cImageIndexActionBlockStart = 51;
    cImageIndexActionBlockEnd = 52;
    cImageIndexActionPipetteBlockStart = 51;
    cImageIndexActionPipetteBlockEnd = 52;

    cImageIndexActionSensorCheck = 53;
    cImageIndexActionChangeRackType = 54;
    cImageIndexActionFlush = 55;
    cImageIndexActionMemoryDevice = 56;
    cImageIndexActionGetTemp = 57;
    cImageIndexActionSetTemp = 58;
    cImageIndexActionBalanceTare = 59;
    cImageIndexActionWeighPosition = 60;
    cImageIndexActionWeighTube = 61;

    cImageIndexActionThreadWait = 62;
    cImageIndexActionDisplayInfo = 63;
    cImageIndexActionTrigger = 64;
    cImageIndexActionResourceAcquire = 65;
    cImageIndexActionResourceRelease = 66;
    cImageIndexActionFileCopy = 67;
    cImageIndexActionFileFind = 68;

    cImageIndexActionMotorMove = 69;
    cImageIndexActionPumpAspirate = 70;
    cImageIndexActionPumpDispense = 71;
    cImageIndexActionCarrierInfo = 72;
    cImageIndexActionRackInfo = 73;
    cImageIndexActionChangeCarrierType = 74;
    cImageIndexActionLevelDetect = 75;
    cImageIndexActionPipetteListAdd = 76;
    cImageIndexActionPipetteListCheck = 77;
    cImageIndexActionPipetteListRework = 78;
    cImageIndexActionParamStore = 79;

    cImageIndexActionIf = 80;
    cImageIndexActionFor = 81;
    cImageIndexActionWhile = 82;
    cImageIndexActionUserProtection = 83;

    INT_IM_INDEX_STARTABLEMETHOD = 84;

    cImageIndexActionReturn = 85;

    cImageIndexMethodEditableInRunner = 86;

    STR_YES = 'YES';
    STR_NO = 'NO';

    STR_OPTION_SUBKEY_SLOT_CARRIER = 'CARRIER';
    STR_OPTION_SUBKEY_SLOT_SLOTNO = 'SLOT';
    STR_OPTION_SUBKEY_SLOT_ROTATION = 'ROTATION';

    STR_RACKNAME_SYSTEM = 'SYSTEM';

    STR_GROUPID_NOGROUP = 'NOGROUP';

    cOptionKeyDialogInputPrefix = 'KEY';
    cOptionKeyDialogInputDescription = cOptionKeyDialogInputPrefix + 'DESC';
    cOptionKeyDialogInputDefaultValue = cOptionKeyDialogInputPrefix + 'DEF';
    cOptionKeyDialogInputMinValue = cOptionKeyDialogInputPrefix + 'MIN';
    cOptionKeyDialogInputMixValue = cOptionKeyDialogInputPrefix + 'MAX';
    cOptionKeyDialogInputEditType = cOptionKeyDialogInputPrefix + 'ETYP';
    cOptionKeyDialogInputDropdownList = cOptionKeyDialogInputPrefix + 'LIST';

type
    TTernary = (tNull, tTrue, tFalse);
    TStoreOrDelete = (sdNull, sdStore, sdDelete);

    TMethodRackNameType = (rnOther, rnSource, rnDestination, rnDiluent);

    TDialogInputValues = record
        Description: string;
        DefaultValue: string;
        MinValue: string;
        MaxValue: string;
        EditType: string;
        DropdownList: string;
    end;

    TDialogInputKeyParam = record
        Key: string;
        V: TDialogInputValues;
    end;

    TDialogInputKeyParamArray = TArray<TDialogInputKeyParam>;

    TDialogInputItem = class(TStreamable)
    private
        fDescription: string;
        fDefaultValue: string;
        fMinValue: string;
        fMaxValue: string;
        fEditType: string;
        fDropdownList: string;
    published
        property Description: string read fDescription write fDescription;
        property DefaultValue: string read fDefaultValue write fDefaultValue;
        property MinValue: string read fMinValue write fMinValue;
        property MaxValue: string read fMaxValue write fMaxValue;
        property EditType: string read fEditType write fEditType;
        property DropdownList: string read fDropdownList write fDropdownList;
    end;

    TKeyArgValueList = TStreamableKeyValueList;

    TRunStartOptions = class(TStreamable)
    private
        fRunName: string;
        fParams: TKeyArgValueList;
    public
        constructor Create(); override;
        destructor Destroy(); override;
    published
        property RunName: string read fRunName write fRunName;
        property Params: TKeyArgValueList read fParams write fParams;
    end;

    TKeyValueParam = record
        Key: string;
        Value: string;
    end;

    TKeyValueParamArray = array of TKeyValueParam;


implementation


uses
    SysUtils;

{ TRunStartOptions }

constructor TRunStartOptions.Create;
begin
    inherited;
    fParams := TKeyArgValueList.Create();
end;

destructor TRunStartOptions.Destroy;
begin
    FreeAndNil(fParams);
    inherited;
end;


end.
