{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : functions used for parsing the method GUI
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  ---------------------------       -------- -----------------------------------------------
  08.06.04 pk                                    TN1976   initial version
  08.06.04 pk  MethodActionIsUnsafe              TN1976   new: from TAppSettings
  09.06.04 pk  DetermineDilRackType              TN1978   from ActionFactory
  14.06.04 pk  LayoutEditActionOptionsFromStr    TN1979   parse LayoutEdit options
  15.06.04 pk  WRSQLActionOptionsFromStr         TN1980.0 parse WRSQL options
  15.06.04 pk  USQLActionOptionsFromStr          TN1980.0 parse USQL options
  17.06.04 pk                                    TN1974   parse MARK options
  17.06.04 pk                                    TN1991   parse WASHP options
  17.06.04 pk                                    TN1993   parse REAG options
  18.06.04 pk  LayoutEditActionOptionsFromStr    TN1979   parsing changed
  18.06.04 pk  WRSQLActionOptionsFromStr         TN1996   new sub option - FilePath
  05.07.04 pk                                    TN2018   parse Tube Action, DEVIC
  06.06.04 pk                                    TN2018   GUIRec changed to RunRec for most functions
  07.07.04 pk  TubeActionBitOptionsFromInt       TN2018   code moved to PosTools
  07.07.04 pk  OptionsStrToList                  TN2024   Keyless Values should be parsed as well
  02.08.04 pk                                    TN2069   Parse DELAY
  04.08.04 pk                                    TN2080   Parse Rack Action
  09.08.04 wl  RackActionRunRecFromRunRec        TN2084   neu: DefaultSlotStr als Parameter
  04.11.04 wl  all Const values and records      TN2213   --> MethodTypes
  04.11.04 wl                                    TN2213   many new parsing functions for TMethodStep
  04.11.04 wl  ImportIntoTableActionRec1FromStr  TN2213   entspricht  ParseImportKeysInOptions
  04.11.04 wl  ImportIntoTableActionRunRecFromRunRec TN2213  entspricht ImportActionRackFromGUIRec
  04.11.04 wl  SQLUpdateActionRunRecFromRunRec   TN2213  entspricht USQLActionOptionsFromStr
  04.11.04 wl  SQLWriteActionRunRecFromRunRec    TN2213  entspricht WRSQLActionOptionsFromStr
  08.11.04 wl  GetDllCallOld                     TN2213   parst Events so wie bisher TEventList
  08.11.04 wl  GetContainer/Aspirate/DispenseDllCallsOld  TN2213  lädt alle Events für die jeweiligen Actions (enthält STR_EV-Konstanten)
  08.11.04 wl  PipetteAction..                   TN2213   neue Funktionen für TPipetteAction
  08.11.04 wl  Rack/TubeActionOptionsFromStr     TN2213   liest auch Events
  10.11.04 wl  Parse/MakeDllCallStruct           TN2213   von MethodTypes.pas hierher
  16.11.04 pk                                    TN2231   Parse new RunStart action
  22.11.04 wl                                    TN2213   enthält jetzt alle Actions
  22.11.04 pk                                    TN2237   parse new WASHT ( washtips ) action
  24.11.04 wl  IsPipetteAction                   TN2213   erkennt STR_ACTION_NAME_PIPETTE_OLD und STR_ACTION_NAME_PIPETTE_NEW
  24.11.04 wl  OptionsStrToList                  TN2213   neue Syntax: erkennt jetzt auch verschachtelte geschweifte Klammern
  24.11.04 wl  AddKeysAndValuesToOption          TN2213   neue Syntax: setzt geschweifte Klammern wenn "," oder "=" im String enthalten sind
  24.11.04 wl  MakeSlotString,ParseSlotStruct    TN2213   neue Syntax: Auslesen von Carrier/Slot/Rotation nochmal geändert
  25.11.04 pk                                    TN2240   Parse new BCFG ( Build Config ) action
  07.12.04 wl  ActionListEntry..                 TN2247.1 ActionListEntry wird wie Option geparst
  09.12.04 wl  MethodParamsStrFromList           TN2247.2 neu für Methodenparamter editieren
  09.12.04 wl  GetMethodActionGroup              TN2255    neu für Methodenparamter editieren
  19.01.05 pk                                    TN2281.0  TStringArray, TIntArray from AppTypes to GeneralTypes
  18.02.05 pk                                    TN2322   Parse DEVFU Action
  08.03.05 pk  ParseCallDLLStruct                TN2337   Calls CallDLLTypeFromStr to determine a Type from the DLLName
  08.03.05 pk  ListToOptionStr                   TN2338   Puts every value inside of brackets
  31.03.05 pk  ListToOptionStr,AddKeysAndValues  TN2367   Call to ValueContainsDelimiter moved from AddKeysAndValuesToOption to ListToOptionStr
  19.04.05 pk  Group                             TN2390,TN2391 Parse GROUP action
  20.04.05 wl  FlushAction..                     TN2379    Flush-Action um 2 Parameter erweitert
  20.04.05 wl  IsPipetteAction                   TN2377    neue Actions 'CALLI' und 'MASSP' geben auch true zurück
  20.04.05 wl  IsCalliAction                     TN2377    gibt den Typ der Calli-Action zurück
  20.04.05 pk  AddGroupID                        TN2390    Add a group id to an existing groupid
  04.05.05 wl  IsManualFillAction                TN2410    Erkennt ManualFill-Action
  15.06.05 pk  MoveZ, Flush, WashProg            TN2464.1  Parse ArmName
  15.06.05 pk  PipetteActionRunRecFromRunRec     TN2464.2  Parse worktype, callitype, perivol
  15.06.05 wl  RackActionParseSlot               TN2465    ersetzt gmParseSlot aus posTools, und versteht auch die neue Syntax
  20.06.05 wl  CommentsFromStr,CommentsToStr     TN2441     für das neue Feld "Comment"
  24.06.05 wl  IsMessageAction                   TN2459    true bei MSG oder MSGB
  24.06.05 wl  MessageWithBeeper-Methoden        TN2459    entfernt
  05.07.05 pk  OptionStrToList                   TN2491    remove quotation marks - a hack to make new syntax work in Designer
  06.07.05 wl  GetMethodActionGroup              TN2493    STR_ACTION_NAME_DETECTEDPOSCANCEL ist SimpleAction und nciht RackAction
  01.08.05 wl  IsPipetteAction                   TN2503    ungenaue Abfrage aufgeteilt in 3 IsPipette...-Methoden
  15.08.05 pk  TCommonGUIParser                  TN2560    Previously named TMethodGUIParser
  15.08.05 pk  TRunGUIParser                     TN2560    Responsible for parsing the run
  15.08.05 pk  OptionsStrToList                  TN2560    overriden in TMethodGUIParser and TRunGUIParser
  18.08.05 pk  ExtractRunFieldsFromOptions       TN2566.0  New : extracts options which are then written to run fields
  22.08.05 pk  RunLoadActionRunRecFromRunRec     TN2546    New option : DoRepeat
  26.08.05 pk  PipetteActionOptionsFromStr       TN2566    Parse TransAir options
  14.09.05 wl  GetMethodActionGroup              TN2570    mit STR_ACTION_NAME_BALANCEWAIT
  23.09.05 wl  MakeCallDllString                 TN2627   neu: StoreKeyName
  05.10.05 wl  GetMethodNameFromAction           TN2575    von MethodStepManager hierher verschoben
  11.10.05 wl  GetMethodActionGroup              TN2658    neue Rack-Action TOOLG
  13.10.05 wl  GetBinaryStrings                  TN2546.1  neu: liefert die strings für YES und NO zurück
  20.10.05 wl  GetMethodActionGroup              TN2659    aufgeräumt
  20.10.05 wl  IsSimple/MoveTubeAction           TN2659    Unterscheidung MoveTubeAction und SimpleTubeAction
  03.11.05 wl  ImportIntoTableAction...          TN2724    benutzt STR_OPTION_KEY_IMPIN...
  03.11.05 wl  GenericImportOptions...           TN2724    neu: Import-Optionen für alle Actions außer IMPOR-Action
  03.11.05 wl  GenericImportOptions...           TN2722    STR_OPTION_KEY_METHIMP_ALLRECS: neu: Alle Records/Zeilen importieren
  03.11.05 wl  GenericImportOptions...           TN2723    STR_OPTION_KEY_METHIMP_SOURCEFILE: neu: Dateiname aus ImportFileDef ersetzen
  08.11.05 wl  XYMovementAction...               TN2728   für neue Action XYMovement
  08.11.05 wl  ZPosMovementAction...             TN2728   für neue Action ZPosMovement
  08.11.05 wl  GenericBuildOptions...            TN2728   Generic Build Options: Werden nur für Build verwendet, im Run nicht mehr
  08.11.05 wl  DeviceActionRunRec                TN2435   fState ist vom typ TSwitchState
  17.11.05 wl  TubeAction..                      TN2771   neuer Parameter GripperArmName
  17.11.05 wl  RackAction..                      TN2771   neuer Parameter GripperArmName
  18.11.05 pk  TMethGUIParser.OptionsStrToList   TN2783   Do not call the RemoveQuotes function - too dangerous
  20.11.05 wl  ChangeTipsAction...               TN2784   für neue Action ChangeTips
  23.11.05 wl  ParseStrToFloatDef                TN2786   entspricht StrToFloatDef (UtilLib), der DecimalSeparator ist aber immer '.'
  23.11.05 wl  ZPosMovementRunRecFromRunRec      TN2786   ZSubmerge benutzt ParseStrToFloatDef
  23.11.05 wl  VortexerTempSet/CheckActionRunRecFromRunRec       TN2786   Alle Float-Werte werden mit ParseStrToFloatDef konvertiert
  23.11.05 wl  ReagentRunLoadActionRunRecFromRunRec       TN2786   Alle Float-Werte werden mit ParseStrToFloatDef konvertiert
  23.11.05 wl  FlushActionRunRecFromRunRec       TN2786   Alle Float-Werte werden mit ParseStrToFloatDef konvertiert
  26.11.05 pk  CommonOptionsStrToList            TN2790   Handle cases of <= >= == as normal strings and not as keyvalue delimiter
  28.11.05 wl  TOnOffToOnOffStr                  TN2812   Neu: Behandlung von BOTH
  08.12.05 wl  RunStartActionRunRecFromRunRec    TN2842   Default für RUNSTDELETE ist jetzt NO
  15.12.05 wl  FileCopyAction..                  TN2856   neue Parameter Overwrite, DeleteSource
  02.01.06 wl  SQLWriteFileAction...             TN2876    neuer Parameter OutputFileName
  18.01.06 wl  TSQLSelectFileAction              TN2885   neue Methoden für SLSQL
  20.01.06 pk  ListToOptionStr                   TN2891   call concatoptions
  20.01.06 pk  PipetteActionRunRecFromRunRec     TN2891   also parse calli options
  20.01.06 pk  GetTubeActionRec                  TN2891   calls GetBasicTubeActionRecFromTubeOptions
  20.01.06 pk                                    TN2891   parse calli options
  20.01.06 pk  SQLSelectActionOptionsFromStr     TN2885   unneeded var xPos removed
  02.02.06 wl  TSQLSelectFileAction              TN2885.1  neu: DefaultValues
  08.03.06 thr Tare/Waitb/WghpActionOptions      TN2941   neue Methoden für Tare, Waitb und WghP
  15.03.06 wl  TZPosMovementActionOptions        TN2966   neu: DisableError
  25.03.06 pk  TRunLoadAction                    TN2998   New Parameter: NumLinesToRepeat, repeat more than just the RUN line
  25.03.06 pk  TInitDeviceActioN                 TN2997   New
  21.03.06 wl  IsRackAction                      TN2984    CLDET zählt jetzt zu den Rack actions
  29.03.06 wl  TXYMovementAction...              TN3005    Neue Parameter: XSpeed,xRamp,YSpeed,YRamp
  29.03.06 wl  TZPosMovementAction...            TN3006    Neuer Parameter: ZRamp
  03.04.06 thr TBalanceTareMethodStep            TN3007   Additional parameters
  11.04.06 pk  ZPosMovementRunRecFromRunRec      TN3006   Bug ZRamp = ZSpeed
  24.04.06 pk  ParamStoreActionOptionsFromStr    TN3058   if is old style parsing set isoldStyle to true
  02.05.06 pk  GroupActionRunRecFromRunRec       TN3081   ReadMode is now Live by default
  04.05.06 pk  GroupActionOptionsFromAddMethod.. TN3081   Set ReadMode to empty if <= -1
  06.06.06 wl  TTimerWait                        TN3128   neue Parameter
  14.07.06 pk  ActionListEntryFromStr            TN3164   new parameter : Session
  04.09.06 pk  GroupIDTypeFromGroupIDStr         TN3280   parse GroupID=NOGroup
  06.09.06 pk  IsNonSchedulableAction            TN3283.1 true if action cannot be scheduled
  07.09.06 wl  DeterminCalliType                 TN3288    entfernt
  13.09.06 wl  WeighWizardAction...              TN3287    neue, eigenständige Funktionen
  20.09.06 wl  MotorMoveAction...                TN3318   enthält schon alle notwendigen Parameter
  31.10.06 pk  MessageAction...                  TN3391    New parameter: PauseRun
  06.11.06 wl  SensorCheckAction...              TN3394    Erzeugt Neue SensorCheckAction
  27.11.06 wl  WeighWizardAction..               TN3362   neu: TeachVolume & TeachParam
  27.11.06 wl  WeighWizardAction..               TN3419    neuer Parameter UseLastWeightAsTare
  28.11.06 wl  ImportIntoTableAction..           TN3397    Import-Action: Parameter nur noch für neuen Import
  05.12.06 wl                                    TN3448    Actions WASH und WASH? entfernt
  07.12.06 wl  ActionHasGenericMethodImportOption  TN3456  neu: für MethodImport
  18.01.07 wl  VortexerSpeedAction..             TN3507    VortexerSpeed: neuer Parameter WaitFor
  18.01.07 wl  StrToVortexerSpeedWaitForType     TN3507    parst VortexerSpeedWaitForType
  18.01.07 pk  AddSequenceAction..               TN3482    New:
  19.02.07 wl  VirtualRackMoveAction...          TN3585   neu: Action VRMOV
  21.02.07 wl  TipsGetAction...                  TN3588    neu für TIPSG-Parameter
  21.02.07 wl  IsTipMovementAction               TN3588    TIPSG gehört jetzt dazu
  12.03.07 pk  TRackMoveAction                   TN3629    new RackMoveOptions
  17.07.07 pk  AddSequenceActionOptions...       TN3653    New Optimization option
  24.07.07 pk  ListToOptionStr                   TN3785    moved to public
  25.07.07 wl  WashProgActionOptions..           TN3792   jetzt mit UsedTips und UsedTipType
  26.07.07 pk  CommandActionOptions...           TN3805   New
  28.09.07 pk  TRunGUIParser                     TN3921   --> RunGUIParsing
  13.11.07 wl  PowderDetectionAction..           TN3844   Neue Action: PWDET
  08.01.08 wl                                    TN3972   bis auf RackAction.. alle Action-spezifischen Methoden entfernt
  07.02.08 wl  IterateOption..                   TN4009    Trennung: Scheduling Options und Iterate
  18.02.08 wl  ParseStrToFloatDef                TN4009    Inhalt --> TValueConverter
  21.02.08 wl  ParseStrToFloatDef                TN4009    jetzt auch mit Rückgabewert
  25.04.08 wl  StrToPumpChannel                  TN4050    new
  03.07.08 wl                                    TN4157
  09.07.08 pk  ListToOptionStr                   TN4160    Changed so that no internal exceptions are raised
  20.09.08 pk                                    TN4215    uneeded functions removed
  25.09.08 wl                                    TN4242    DllStruct-Funktionen entfernt
  06.10.08 pk  RunStartOptionsToStr              TN4258    New
  15.10.08 pk                                    TN4258    New functions for dealing with KeyValueParamArray
  25.11.08 wl  TDialogInputKeyParamArray         TN4310    new
  04.11.09 pk                               	    TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  01.02.10 pk  CommonOptionsStrToList            TN4968    format the key so that the order remains the way it is
  13.04.10 wl                                    TN5044   uses StringUtilities
  07.05.10 pk                                    TN5092    RunStartOptionsToStr removed
  31.05.10 wl  ActionComparableName,IsActionName TN5120   entfernt
  12.08.10 wl  StrToPumpNumber                   TN5227   Begrenzung auf 2 Pumpen entfernt
  29.09.10 pk                                    TN5283    Short and Long method step comments combined
  29.11.10 wl                                    TN5370  Bugfix
  14.12.10 wl  ParseStrToFloatDef                TN5411   arbeitet mit TFormatSettings
  14.02.11 wl  GetPositionArray                  TN5468   neu
  08.07.11 wl  GetVolumeArray,GetNewPositionArray,GetRackNameArray,GetTipArray  TN5626   neu
  29.08.11 wl  GetStringArrayOfPos               TN5673   ersetzt GetRackNameArray
  08.09.11 wl  GetVolumeArray,GetNewPositionArray   TN5678   können jetzt auch mit '' umgehen
  21.10.11 wl  GetInteger-,DoubleArrayOfPos         TN5723   neu
  17.11.11 wl  GetNewPositionArray                  TN5725   Ergebnis ist jetzt TArray<string>
  14.12.11 wl                                       TN5765   aufgeräumt
  06.02.12 wl  GetInteger-,DoubleArrayOfPos         TN5794   jetzt mit Default-Wert
  06.02.12 wl  GetLiqHArrayOfPos                    TN5794   ersetzt '' mit 'DEFAULT'
  27.03.12 wl  GetRackPositionArray                 TN5847   Ermittelt Rack- und Positionsarray aus den Action-Parametern
  27.03.12 wl  GetInteger-,Double-,StringArrayOfPos TN5847   wenn das Array nicht genügend Werte hat, gibt es eine Exception
  27.03.12 wl  GetStringArrayDefLength              TN5847   entspricht der Funktion GetNewPositionArray
  ---------------------------------------------------------------------------------------------------------------------- }

unit MethodGUIParsing;


interface


uses
    ListClasses,
    GeneralTypes,
    Streamable,
    AppTypes,
    MethodTypes;

const
    STR_DELIM_KEYVALUE = '=';
    STR_DELIM_SEPARATOR = ',';
    STR_DELIM_SUBLIST_BEGIN = '{';
    STR_DELIM_SUBLIST_END = '}';

    STR_PASRSER_STRING = '"';

type
    TMethodGUIParser = class
    private
        class function CheckChoice(aChoice: integer; aOption: string): integer;

        class function ConvertValueToSublistValue(aValue: string): string;
        class procedure CommonOptionsStrToList(const aList: TStringKeyStringValueList; aStr: string);
        class procedure GetCoord(out oCol, oRow: integer; aMaxRows, aPos: integer);
        class function OffsetMatrixCoordBy(aMatrixCoord: string; aOffset: integer): string;
        class procedure ParseMatrixCoord(aMatrixCoord: string; out aPart1, aPart2, aPart3: integer);
    public
        class function OptionStrFromChoice(aChoice: integer; const aChoices: array of string): string;
        class function OptionStrToChoice(aOption: string; const aChoices: array of string;
            aCheckChoice: boolean = true): integer;
        class function TernaryDefault(aValue, aDefault: TTernary): TTernary;
        class function ParseStrToFloatDef(aStr: string; aDefault: extended): extended;
        class function FindValuesForKeysInOptions(aOptions: string; out oValues: TStringArray;
            const aKeys: array of string): boolean;

        class function CreateKeyValueList(): TStringKeyStringValueList;
        class function ListToOptionStr(const aList: TStringKeyStringValueList): string;
        class function ValueContainsDelimiter(aValue: string): boolean;
        class procedure OptionsStrToList(const aList: TStringKeyStringValueList; aStr: string);
        class function ConcatOptions(const aPart1, aPart2: string): string;
        class function GetBinaryStrings(): TStringArray;
        class function TernaryToYesNoStr(aValue: TTernary; aDefault: TTernary = tNull): string;
        class function YesNoStrToTernary(aYesNoStr: string; aDefault: TTernary = tNull): TTernary;
        class function TernaryToBinary(aTernary: TTernary; aDefault: boolean = false): boolean;
        class function YesNoStrToBool(aYesNoStr: string; aDefault: boolean = false): boolean;
        class function BoolToYesNoStr(aValue: boolean): string;

        class function StrToPumpNumber(const aPumpNumber: string): integer;

        class function ParseSlotStruct(aSlotString: string): TSlotStructStr;
        class function MakeSlotString(aSlotStruct: TSlotStructStr): string;
        class function MakeSlotStructFromSlotStructStr(aSlotStructStr: TSlotStructStr;
            aDefaultCarrierName: string = ''): TSlotStruct;
        class function MakeSlotStruct(aCarrierName: string; aSlotNr: integer = 0;
            aRotation: TRotationValue = rotation_0): TSlotStruct;
        class function RemoveKeysAndValuesInOptions(const aOptions: string;
            const aKeys: array of string): string;
        class function AddKeysAndValuesToOptions(aOptions: string;
            const aKeys, aValues: array of string): string;
        class function ExtractKeysAndValuesInOptions(aOptions: string; out oValues: TStringArray;
            const aKeys: array of string): string;
        class procedure MethodParamsStrToList(aParams: string; aParamList: TStringKeyStringValueList);
        class function MethodParamsStrFromList(aParamList: TStringKeyStringValueList): string;

        class function KeyValueParamArrayToStr(const aParamArray: TKeyValueParamArray): string;
        class function KeyValueParamArrayFromStr(const aParams: string): TKeyValueParamArray;
        class function KeyValueParamArrayFromList(aList: TStringKeyStringValueList): TKeyValueParamArray;

        class function DialogInputValuesFromStr(const aOptions: string): TDialogInputValues;
        class function DialogInputValuesToStr(const aDialogInputValues: TDialogInputValues): string;
        class function DialogInputKeyParamArrayFromStr(const aDialogInputParams: string)
            : TDialogInputKeyParamArray;
        class function DialogInputKeyParamArrayToStr(const aDialogInputParamArray
            : TDialogInputKeyParamArray): string;
        // Define actions:
        class function DetermineDilRackType(aDilRackName: string): TDilRackType;

        // Rack Actions
        class function RackActionOptionsFromInt(aOptionInt: integer): TRackOptions;

        class function GetDefaultRackMoveOptions(): TRackMoveOptions;

        class function GetCoordType(aCoord: string): TCoordType;
        class function MatrixCoordCurrentPos(aPosCnt: integer; aFirstCoord, aLastCoord: string;
            aRackRows, aRackCols: integer; aRotationIndex: Byte): integer;
        class function IntegerCoordCurrentPos(aPosCnt: integer; aFirstCoord: string; aRackRows: integer;
            aRotationIndex: Byte): integer;
        class function GetLastPos(aFirst, aLast: string): integer;
        class function OffsetCoordBy(aCoord: string; aOffset: integer): string;
        class function GetPositionArray(const aFirstPosStr, aLastPosStr: string;
            aRackRows, aRackCols: integer; aRotationIndex: Byte): TIntArray;
        class function GetCoordString(aMaxRows, aPos: integer): string;

        // Array-Magic-Funktionen
        class function GetStringArrayDefLength(const aValue: string; out oPosArray: TArray<string>)
            : boolean; static;
        class function GetRackPositionArray(const aRacks, aPositions: string;
            out oRackArray, oPosArray: TArray<string>): integer; static;
        class function GetTipArrayOfPos(const aValue: string; const aNoOfPos: integer)
            : TArray<integer>; static;
        class function GetStringArrayOfPos(const aValue: string; const aNoOfPos: integer)
            : TArray<string>; static;
        class function GetLiqHArrayOfPos(const aValue: string; const aNoOfPos: integer)
            : TArray<string>; static;
        class function GetIntegerArrayOfPos(const aValue: string; const aNoOfPos: integer;
            aDefault: integer = 0): TArray<integer>; static;
        class function GetDoubleArrayOfPos(const aValue: string; const aNoOfPos: integer;
            aDefault: double = 0): TArray<double>; static;
    end;


implementation


uses
    SysUtils,
    AppSettings,
    SamGlobe,
    StringUtilities,
    ArrayUtils;

{ TMethodGUIParser }

class function TMethodGUIParser.TernaryDefault(aValue: TTernary; aDefault: TTernary): TTernary;
begin
    if aValue = tNull then
        result := aDefault
    else
        result := aValue;
end;

class function TMethodGUIParser.TernaryToYesNoStr(aValue: TTernary; aDefault: TTernary = tNull): string;
begin
    result := '';
    if aValue = aDefault then
        EXIT;

    if aValue = tTrue then
        result := STR_YES
    else if aValue = tFalse then
        result := STR_NO;

end;

class function TMethodGUIParser.YesNoStrToTernary(aYesNoStr: string; aDefault: TTernary = tNull): TTernary;
begin
    result := aDefault;
    aYesNoStr := UpperCase(aYesNoStr);
    if aYesNoStr = STR_YES then
        result := tTrue
    else if aYesNoStr = STR_NO then
        result := tFalse;
end;

class function TMethodGUIParser.TernaryToBinary(aTernary: TTernary; aDefault: boolean = false): boolean;
begin
    result := aDefault;
    case aTernary of
        tTrue:
            result := true;
        tFalse:
            result := false;
    end;
end;

class function TMethodGUIParser.GetBinaryStrings(): TStringArray;
begin
    SetLength(result, 2);
    result[0] := STR_YES;
    result[1] := STR_NO;
end;

class function TMethodGUIParser.YesNoStrToBool(aYesNoStr: string; aDefault: boolean = false): boolean;
var
    xTernary: TTernary;
begin
    xTernary := YesNoStrToTernary(aYesNoStr, tNull);
    result := TernaryToBinary(xTernary, aDefault);
end;

class function TMethodGUIParser.BoolToYesNoStr(aValue: boolean): string;
begin
    if aValue then
        result := STR_YES
    else
        result := STR_NO;
end;

class function TMethodGUIParser.ParseStrToFloatDef(aStr: string; aDefault: extended): extended;
begin
    result := StrToFloatDef(aStr, aDefault, TFormatUtils.GetSettingsEnglishUS);
    // als Decimal Separator ist nur der Punkt erlaubt
end;

class procedure TMethodGUIParser.OptionsStrToList(const aList: TStringKeyStringValueList; aStr: string);
begin
    CommonOptionsStrToList(aList, aStr);
end;

class procedure TMethodGUIParser.CommonOptionsStrToList(const aList: TStringKeyStringValueList; aStr: string);

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
            // dont take out the %.4d because the list is sorted
            xKey := Format('%.4d', [xKeylessValueCount]); // create a unique, fake key
        end;
        aList.AddValue(xKey, xValue);
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
                raise Exception.Create('Close braket without open bracket ' + aStr);
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
            ((xChar = STR_DELIM_KEYVALUE) or (xChar = ':') or (xChar = '<') or (xChar = '>')) then
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
        raise Exception.Create('Open bracket without close bracket ' + aStr);
end;

class function TMethodGUIParser.ConcatOptions(const aPart1, aPart2: string): string;
begin
    if aPart1 = '' then
        result := aPart2
    else if aPart2 = '' then
        result := aPart1
    else
        result := aPart1 + STR_DELIM_SEPARATOR + aPart2;
end;

class function TMethodGUIParser.CreateKeyValueList(): TStringKeyStringValueList;
begin
    result := TStringKeyStringValueList.Create(dupIgnore, false);
end;

class function TMethodGUIParser.ListToOptionStr(const aList: TStringKeyStringValueList): string;
var
    i: integer;
    xKey, xValue: string;
    xKeyAndDelim: string;
    xNewOption: string;
    xDummy: integer;
begin
    result := '';
    for i := 0 to aList.Count - 1 do
    begin
        xKey := aList[i];

        // if the Key is an integer it means there was NO key so omit the key and the keyvalue delimiter
        xKeyAndDelim := '';
        if not TryStrToInt(xKey, xDummy) then
        begin
            xKeyAndDelim := xKey + STR_DELIM_KEYVALUE
        end;

        aList.TryGetValue(xKey, xValue);
        if ValueContainsDelimiter(xValue) then
            xValue := ConvertValueToSublistValue(xValue);
        xNewOption := xKeyAndDelim + xValue;
        result := ConcatOptions(result, xNewOption);
    end;

end;

class function TMethodGUIParser.FindValuesForKeysInOptions(aOptions: string; out oValues: TStringArray;
    const aKeys: array of string): boolean;
var
    xList: TStringKeyStringValueList;
    i: integer;
    xKey, xValue: string;
begin
    SetLength(oValues, high(aKeys) + 1);

    result := false;
    xList := CreateKeyValueList();
    try
        OptionsStrToList(xList, aOptions);
        for i := 0 to high(aKeys) do
        begin
            xValue := '';
            xKey := aKeys[i];
            if xList.ContainsKey(xKey) then
                xList.TryGetValue(xKey, xValue);
            if (xValue <> '') then
                result := true; // result = true: mindestens ein Parameter ist gesetzt
            oValues[i] := xValue;
        end;
    finally
        xList.Free;
    end;
end;

class function TMethodGUIParser.RemoveKeysAndValuesInOptions(const aOptions: string;
    const aKeys: array of string): string;
var
    xList: TStringKeyStringValueList;
    xKey: string;
    i: integer;
begin
    result := aOptions;
    xList := CreateKeyValueList();
    try
        OptionsStrToList(xList, aOptions);
        for i := 0 to high(aKeys) do
        begin
            xKey := aKeys[i];
            if not xList.ContainsKey(xKey) then
                CONTINUE;
            xList.Remove(xKey);
        end;
        result := ListToOptionStr(xList);
    finally
        xList.Free;
    end;
end;

class function TMethodGUIParser.ExtractKeysAndValuesInOptions(aOptions: string; out oValues: TStringArray;
    const aKeys: array of string): string;
var
    xList: TStringKeyStringValueList;
    xKey: string;
    i: integer;
begin
    result := aOptions;
    SetLength(oValues, high(aKeys) + 1);
    xList := CreateKeyValueList();
    try
        OptionsStrToList(xList, aOptions);
        for i := high(aKeys) downto 0 do
        begin
            xKey := aKeys[i];
            if not xList.TryGetValue(xKey, oValues[i]) then
                CONTINUE;
            xList.Remove(xKey);
        end;
        result := ListToOptionStr(xList);
    finally
        xList.Free;
    end;
end;

class function TMethodGUIParser.ValueContainsDelimiter(aValue: string): boolean;
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
                raise Exception.Create('Close braket without open bracket ' + aValue);
            dec(xSubListFound)
        end
        else if (xSubListFound > 0) then
        begin
            // nicht innerhalb der Klammern testen!
        end
        // avoid cases of : <=, >=, ==
        else if (xNextChar = STR_DELIM_KEYVALUE) and
            ((xChar = STR_DELIM_KEYVALUE) or (xChar = ':') or (xChar = '<') or (xChar = '>')) then
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
        raise Exception.Create('Open bracket without close bracket ' + aValue);
end;

class function TMethodGUIParser.ConvertValueToSublistValue(aValue: string): string;
begin
    result := STR_DELIM_SUBLIST_BEGIN + aValue + STR_DELIM_SUBLIST_END;
end;

class function TMethodGUIParser.AddKeysAndValuesToOptions(aOptions: string;
    const aKeys, aValues: array of string): string;
var
    xList: TStringKeyStringValueList;
    i: integer;
    xValue: string;
begin
    result := aOptions;
    xList := CreateKeyValueList();
    try
        OptionsStrToList(xList, aOptions);
        for i := 0 to high(aKeys) do
        begin
            xValue := aValues[i];
            if (xValue = '') then
                CONTINUE;
            xList.AddOrSetValue(aKeys[i], xValue);
        end;
        result := ListToOptionStr(xList);
    finally
        xList.Free;
    end;
end;

class procedure TMethodGUIParser.MethodParamsStrToList(aParams: string;
    aParamList: TStringKeyStringValueList);
begin
    OptionsStrToList(aParamList, aParams);
end;

class function TMethodGUIParser.MethodParamsStrFromList(aParamList: TStringKeyStringValueList): string;
begin
    result := ListToOptionStr(aParamList);
end;

class function TMethodGUIParser.KeyValueParamArrayToStr(const aParamArray: TKeyValueParamArray): string;
var
    xList: TStringKeyStringValueList;
    x: integer;
begin
    xList := CreateKeyValueList();
    try
        for x := 0 to high(aParamArray) do
        begin
            xList.AddValue(aParamArray[x].Key, aParamArray[x].Value);
        end;
        result := ListToOptionStr(xList);
    finally
        xList.Free;
    end;
end;

class function TMethodGUIParser.KeyValueParamArrayFromList(aList: TStringKeyStringValueList)
    : TKeyValueParamArray;
var
    x: integer;
begin
    SetLength(result, aList.Count);
    for x := 0 to high(result) do
    begin
        result[x].Key := aList[x];
        result[x].Value := aList.Values[x];
    end;
end;

class function TMethodGUIParser.KeyValueParamArrayFromStr(const aParams: string): TKeyValueParamArray;
var
    xList: TStringKeyStringValueList;
begin
    xList := CreateKeyValueList();
    try
        OptionsStrToList(xList, aParams);
        result := KeyValueParamArrayFromList(xList);
    finally
        xList.Free;
    end;
end;

class function TMethodGUIParser.DialogInputValuesToStr(const aDialogInputValues: TDialogInputValues): string;
begin
    result := AddKeysAndValuesToOptions('', [cOptionKeyDialogInputDescription,
        cOptionKeyDialogInputDefaultValue, cOptionKeyDialogInputMinValue, cOptionKeyDialogInputMixValue,
        cOptionKeyDialogInputEditType, cOptionKeyDialogInputDropdownList],
        [aDialogInputValues.Description, aDialogInputValues.DefaultValue, aDialogInputValues.MinValue,
        aDialogInputValues.MaxValue, aDialogInputValues.EditType, aDialogInputValues.DropdownList]);
end;

class function TMethodGUIParser.DialogInputValuesFromStr(const aOptions: string): TDialogInputValues;
var
    xValues: TStringArray;
begin
    FindValuesForKeysInOptions(aOptions, xValues, [cOptionKeyDialogInputDescription,
        cOptionKeyDialogInputDefaultValue, cOptionKeyDialogInputMinValue, cOptionKeyDialogInputMixValue,
        cOptionKeyDialogInputEditType, cOptionKeyDialogInputDropdownList]);

    result.Description := xValues[0];
    result.DefaultValue := xValues[1];
    result.MinValue := xValues[2];
    result.MaxValue := xValues[3];
    result.EditType := xValues[4];
    result.DropdownList := xValues[5];
end;

class function TMethodGUIParser.DialogInputKeyParamArrayToStr(const aDialogInputParamArray
    : TDialogInputKeyParamArray): string;
var
    xParamArray: TKeyValueParamArray;
    x: integer;
begin
    SetLength(xParamArray, high(aDialogInputParamArray) + 1);
    for x := 0 to high(aDialogInputParamArray) do
    begin
        xParamArray[x].Key := aDialogInputParamArray[x].Key;
        xParamArray[x].Value := ConvertValueToSublistValue
            (DialogInputValuesToStr(aDialogInputParamArray[x].V));
    end;
    result := KeyValueParamArrayToStr(xParamArray);
end;

class function TMethodGUIParser.DialogInputKeyParamArrayFromStr(const aDialogInputParams: string)
    : TDialogInputKeyParamArray;
var
    xParamArray: TKeyValueParamArray;
    x: integer;
begin
    xParamArray := KeyValueParamArrayFromStr(aDialogInputParams);
    SetLength(result, high(xParamArray) + 1);
    for x := 0 to high(xParamArray) do
    begin
        result[x].Key := xParamArray[x].Key;
        result[x].V := DialogInputValuesFromStr(xParamArray[x].Value);
    end;
end;

class function TMethodGUIParser.MakeSlotString(aSlotStruct: TSlotStructStr): string;
begin
    result := AddKeysAndValuesToOptions('', [STR_OPTION_SUBKEY_SLOT_CARRIER, STR_OPTION_SUBKEY_SLOT_SLOTNO,
        STR_OPTION_SUBKEY_SLOT_ROTATION], [aSlotStruct.Carrier, aSlotStruct.Slot, aSlotStruct.Rotation]);
end;

class function TMethodGUIParser.ParseSlotStruct(aSlotString: string): TSlotStructStr;
var
    xValues: TStringArray;
begin
    FindValuesForKeysInOptions(aSlotString, xValues, [STR_OPTION_SUBKEY_SLOT_CARRIER,
        STR_OPTION_SUBKEY_SLOT_SLOTNO, STR_OPTION_SUBKEY_SLOT_ROTATION]);

    result.Carrier := xValues[0];
    result.Slot := xValues[1];
    result.Rotation := xValues[2];
end;

class function TMethodGUIParser.StrToPumpNumber(const aPumpNumber: string): integer;
var
    xErr: integer;
begin
    Val(aPumpNumber, result, xErr);

    // immer mindestens PumpNumber 1
    if (result < 1) then
        result := 1;
end;

class function TMethodGUIParser.OptionStrToChoice(aOption: string; const aChoices: array of string;
    aCheckChoice: boolean = true): integer;
var
    i: integer;
begin
    result := -1;
    for i := 0 to high(aChoices) do
    begin
        if UpperCase(aOption) = aChoices[i] then
        begin
            result := i;
            BREAK;
        end;
    end;
    result := CheckChoice(result, aOption);
end;

class function TMethodGUIParser.OptionStrFromChoice(aChoice: integer;
    const aChoices: array of string): string;
begin
    result := '';
    ASSERT(aChoice > -1);
    if aChoice > high(aChoices) then
        EXIT;
    result := aChoices[aChoice];

end;

class function TMethodGUIParser.CheckChoice(aChoice: integer; aOption: string): integer;
begin
    result := aChoice;
    if aChoice > -1 then
        EXIT;
    raise Exception.Create('The option [' + aOption + '] is invalid');
end;

class function TMethodGUIParser.GetDefaultRackMoveOptions(): TRackMoveOptions;
begin
    result.GetMoveToDoXBeforeZ := true;
    result.PutMoveToDoXBeforeZ := true;
    result.GetMoveFromDoXBeforeZ := false;
    result.PutMoveFromDoXBeforeZ := false;
end;

class function TMethodGUIParser.RackActionOptionsFromInt(aOptionInt: integer): TRackOptions;
var
    i: integer;
begin
    result := [];
    for i := 0 to Integer( high(TRackOption)) do
    begin
        if (aOptionInt and ARR_POWER_OF_TWO[i + 1]) = ARR_POWER_OF_TWO[i + 1] then
            result := result + [TRackOption(i)];
    end;
end;

class function TMethodGUIParser.MakeSlotStruct(aCarrierName: string; aSlotNr: integer = 0;
    aRotation: TRotationValue = rotation_0): TSlotStruct;
begin
    result.CarrierName := aCarrierName;
    result.SlotNr := aSlotNr;
    result.Rotation := aRotation;
end;

function gmGetRotationValue(aRotationDegree: integer): TRotationValue;
begin
    case aRotationDegree of
        90:
            result := rotation_90;
        180:
            result := rotation_180;
        270:
            result := rotation_270;
        else
            result := rotation_0;
    end;
end;

class function TMethodGUIParser.MakeSlotStructFromSlotStructStr(aSlotStructStr: TSlotStructStr;
    aDefaultCarrierName: string = ''): TSlotStruct;
var
    xRotation, xErrCode: integer;
begin
    result := MakeSlotStruct(aDefaultCarrierName);

    if aSlotStructStr.Carrier <> '' then
        result.CarrierName := aSlotStructStr.Carrier;

    if (aSlotStructStr.Slot <> '') then
        Val(aSlotStructStr.Slot, result.SlotNr, xErrCode);

    if (aSlotStructStr.Rotation <> '') then
    begin
        Val(aSlotStructStr.Rotation, xRotation, xErrCode);
        result.Rotation := gmGetRotationValue(xRotation);
    end;
end;

// **************************************************************************************************
// *************************************** General Functions ****************************************
// **************************************************************************************************

class function TMethodGUIParser.DetermineDilRackType(aDilRackName: string): TDilRackType;
begin
    if (aDilRackName = '') or (aDIlRackName = STR_RACKNAME_SYSTEM) then
        result := drSystem
    else
        result := drNormal;
end;

class procedure TMethodGUIParser.GetCoord(out oCol, oRow: integer; aMaxRows, aPos: integer);

// errechnen der Coordinaten als zahl aus der Position

begin
    oRow := (aPos - 1) mod aMaxRows;
    oCol := Trunc((aPos - 1) / aMaxRows);
end;

class function TMethodGUIParser.GetCoordString(aMaxRows, aPos: integer): string;

// Errechnen der Koordinaten aus der Position, Rückgabe als String (A1,A2,...)
// --> Grenzüberschreitungen werden abgefangen, und als '' zurückgegeben

var
    xCol, xRow: integer;
begin
    if aPos > 0 then
    begin
        GetCoord(xCol, xRow, aMaxRows, aPos);
        if (xRow <= 26) then // Reihen werden maximal mit 'Z' bezeichnet
            result := chr(65 + xRow) + Inttostr(xCol + 1)
        else
            result := '';
    end
    else
        result := '';
end;

class function TMethodGUIParser.OffsetMatrixCoordBy(aMatrixCoord: string; aOffset: integer): string;

begin
    result := Format('%s%s%d', [aMatrixCoord, STR_MATRIXCOORD_OFFSET_DELIMITER, aOffset]);
end;

class procedure TMethodGUIParser.ParseMatrixCoord(aMatrixCoord: string; out aPart1, aPart2, aPart3: integer);

// A Matrix coord usually has the following format sdddd
// where s is in A..Z and d is 0..9
// Optionally a coord can have a format sdddd-i
// where i is any integer value
// If Matrixcoord has a format like A015 then
// aPart1 = Ord(A),  aPart2 = 15, aPart3 = 0
// If Matrixcoord has a format like A015-7
// aPart1 = Ord(A),  aPart2 = 15, aPart3 = 7
const
    INT_MATRIXCOORD_PART1_POS = 1;
    INT_MATRIXCOORD_PART2_START = 2;
    INT_MATRIXCOORD_PART2_NUMCHARS = 4;
var
    xPart1AndPart2, xPart3: string;
begin
    TStringUtilities.SplitStr(aMatrixCoord, STR_MATRIXCOORD_OFFSET_DELIMITER, xPart1AndPart2, xPart3);

    aPart1 := Ord(xPart1AndPart2[INT_MATRIXCOORD_PART1_POS]);
    aPart2 := StrToInt(Copy(xPart1AndPart2, INT_MATRIXCOORD_PART2_START, INT_MATRIXCOORD_PART2_NUMCHARS));
    aPart3 := StrToIntDef(xPart3, 0);
end;

class function TMethodGUIParser.GetCoordType(aCoord: string): TCoordType;

var
    xP1, xP2, xP3: integer;
begin
    result := ctNone;
    if aCoord = '' then
        EXIT;
    if TryStrToInt(aCoord, xP1) then
    begin
        result := ctInteger;
        EXIT;
    end;
    try
        ParseMatrixCoord(aCoord, xP1, xP2, xP3);
        result := ctMatrix;
        EXIT;
    except
    end;
end;

class function TMethodGUIParser.OffsetCoordBy(aCoord: string; aOffset: integer): string;

var
    xCoordType: TCoordType;
begin
    result := '';
    xCoordType := GetCoordType(aCoord);
    if xCoordType = ctMatrix then
        result := OffsetMatrixCoordBy(aCoord, aOffset)
    else if xCoordType = ctInteger then
        result := IntToStr(StrToInt(aCoord) + aOffset);
end;

class function TMethodGUIParser.MatrixCoordCurrentPos(aPosCnt: integer; aFirstCoord, aLastCoord: string;
    aRackRows, aRackCols: integer; aRotationIndex: Byte): integer;

//
const
    INT_ERROR_RESULT = -1;
    STR_MATRIXCOORD_PART1_MIN = Ord('A');
var
    xFirstCoordPart1, xFirstCoordPart2, xFirstCoordPart3: integer;
    xLastCoordPart1, xLastCoordPart2, xDummy: integer;
    xMatrixRows, xMatrixCols, xFirstMatrixRow, xFirstMatrixCol, xCurrentRow, xCurrentCol: Integer;
begin
    result := INT_ERROR_RESULT;

    ParseMatrixCoord(aFirstCoord, xFirstCoordPart1, xFirstCoordPart2, xFirstCoordPart3);

    if aFirstCoord = aLastCoord then
    begin
        if xFirstCoordPart3 > 0 then
        begin
            aPosCnt := aPosCnt + xFirstCoordPart3;
            aLastCoord := GetCoordString(aRackRows, aRackCols * aRackRows);
        end;
    end;

    ParseMatrixCoord(aLastCoord, xLastCoordPart1, xLastCoordPart2, xDummy);

    xMatrixRows := Abs(xLastCoordPart1 - xFirstCoordPart1) + 1;
    xMatrixCols := Abs(xLastCoordPart2 - xFirstCoordPart2) + 1;
    xFirstMatrixRow := xFirstCoordPart1 - Ord(STR_MATRIXCOORD_PART1_MIN);
    xFirstMatrixCol := xFirstCoordPart2 - 1;

    case aRotationIndex of
        1:
            begin // 90 Grad
                xCurrentRow := (aPosCnt - 1) mod xMatrixRows;
                xCurrentCol := trunc((aPosCnt - 1) / xMatrixRows);
                xCurrentCol := xFirstMatrixCol - xCurrentCol;
                xCurrentRow := xFirstMatrixRow + xCurrentRow + 1;
            end;
        2:
            begin // 180 Grad
                xCurrentRow := trunc((aPosCnt - 1) / xMatrixCols);
                xCurrentCol := (aPosCnt - 1) mod xMatrixCols;
                xCurrentCol := xFirstMatrixCol - xCurrentCol;
                xCurrentRow := xFirstMatrixRow - xCurrentRow + 1;
            end;
        3:
            begin // 270 Grad
                xCurrentRow := (aPosCnt - 1) mod xMatrixRows;
                xCurrentCol := trunc((aPosCnt - 1) / xMatrixRows);
                xCurrentCol := xFirstMatrixCol + xCurrentCol;
                xCurrentRow := xFirstMatrixRow - xCurrentRow + 1
            end;
        else
            begin
                GetCoord(xCurrentCol, xCurrentRow, xMatrixRows, aPosCnt);
                xCurrentCol := xFirstMatrixCol + xCurrentCol;
                xCurrentRow := xFirstMatrixRow + xCurrentRow + 1;
            end;
    end;

    if (xCurrentRow > aRackRows) then
        EXIT;
    result := (xCurrentCol) * aRackRows + xCurrentRow;
end;

class function TMethodGUIParser.IntegerCoordCurrentPos(aPosCnt: integer; aFirstCoord: string;
    aRackRows: integer; aRotationIndex: Byte): integer;

begin
    case aRotationIndex of
        1:
            result := StrToInt(aFirstCoord) - (Pred(aPosCnt) * aRackRows); // 90 Grad
        2:
            result := StrToInt(aFirstCoord) - Pred(aPosCnt); // 180 Grad
        3:
            result := StrToInt(aFirstCoord) + (Pred(aPosCnt) * aRackRows); // 270 Grad
        else
            result := StrToInt(aFirstCoord) + aPosCnt - 1;
    end;
end;

class function TMethodGUIParser.GetLastPos(aFirst, aLast: string): integer;

// First            = 1. MatrixPosition Pipettierbereich z.B.  B1
// Last             = Letzte MatrixPosition Pipettierbereich z.B.  H11
// Result           = Anzahl Positionen im Pipettierbereich

var
    xLastPos, xFirstPos, xError: integer;
    xRows, xCols: LongInt;
    xFirstCol, xLastCol: integer;
    xFirstRowOrd, xLastRowOrd: integer;
    xFirstOffset, xLastOffset: integer;

    xFirstType, xLastType: TCoordType;

begin
    result := 0;
    xFirstType := GetCoordType(aFirst);
    xLastType := GetCoordType(aLast);

    if (xFirstType = ctNone) or (xLastType = ctNone) then
        Exit;

    if (xFirstType = ctMatrix) and (xLastType = ctMatrix) then
    begin
        try
            ParseMatrixCoord(aFirst, xFirstRowOrd, xFirstCol, xFirstOffset);
            ParseMatrixCoord(aLast, xLastRowOrd, xLastCol, xLastOffset);
        except
            Exit;
        end;
        // ----------------------------------- Bestimmung der Spalten- und Zeilenanzahl
        xRows := xLastRowOrd - xFirstRowOrd + 1;
        xCols := xLastCol - xFirstCol + 1;
        // --------------------------------------- Result: Spaltenanzahl * Zeilenanzahl
        if (xRows <= 0) or (xCols <= 0) then
            Exit;
        result := xRows * xCols
    end
    else
    begin
        Val(aFirst, xFirstPos, xError);
        if (xError <> 0) then
            Exit;
        Val(aLast, xLastPos, xError);
        if (xError <> 0) then
            Exit;
        result := xLastPos - xFirstPos + 1;
    end;
end;

class function TMethodGUIParser.GetLiqHArrayOfPos(const aValue: string; const aNoOfPos: integer)
    : TArray<string>;
var
    x: integer;
begin
    result := GetStringArrayOfPos(aValue, aNoOfPos);
    for x := 0 to high(result) do
    begin
        if (result[x] = '') then
            result[x] := 'DEFAULT';
    end;
end;

class function TMethodGUIParser.GetPositionArray(const aFirstPosStr, aLastPosStr: string;
    aRackRows, aRackCols: integer; aRotationIndex: Byte): TIntArray;
var
    xPos, xNoOfPos: integer;
    xPosCounter: integer;
    xFirstCoordType, xLastCoordType, xCoordType: TCoordType;
begin
    SetLength(result, 0);

    xFirstCoordType := TMethodGUIParser.GetCoordType(aFirstPosStr);
    xLastCoordType := TMethodGUIParser.GetCoordType(aLastPosStr);
    xNoOfPos := TMethodGUIParser.GetLastPos(aFirstPosStr, aLastPosStr);

    if (xFirstCoordType = ctMatrix) and (xLastCoordType = ctMatrix) then
    begin
        xCoordType := ctMatrix;
    end
    else if (xFirstCoordType = ctInteger) and (xLastCoordType = ctInteger) then
    begin
        xCoordType := ctInteger;
    end
    else
    begin
        EXIT;
    end;

    SetLength(result, xNoOfPos);
    for xPosCounter := 0 to xNoOfPos - 1 do
    begin

        if (xCoordType = ctMatrix) then
            xPos := TMethodGUIParser.MatrixCoordCurrentPos(xPosCounter + 1, aFirstPosStr, aLastPosStr,
                aRackRows, aRackCols, aRotationIndex)
        else
            xPos := TMethodGUIParser.IntegerCoordCurrentPos(xPosCounter + 1, aFirstPosStr, aRackRows,
                aRotationIndex);

        result[xPosCounter] := xPos;
    end;
end;

class function TMethodGUIParser.GetDoubleArrayOfPos(const aValue: string; const aNoOfPos: integer;
    aDefault: double): TArray<double>;
var
    x: integer;
begin
    if POS('[', aValue) > 0 then
    begin
        // das Ergebnis ist ein Array
        result := TArrayUtils.StringToDoubleArrayDef(Copy(aValue, 2, Length(aValue) - 2), ',', aDefault,
            TFormatUtils.GetSettingsEnglishUS);
        if (Length(result) < aNoOfPos) then
            raise Exception.Create('Array does not fit to number of positions: ' + aValue);
    end
    else
    begin
        // Das Ergebnis ist ein Einzelwert
        SetLength(result, aNoOfPos);
        for x := 0 to high(result) do
            result[x] := ParseStrToFloatDef(aValue, aDefault);
    end;
end;

class function TMethodGUIParser.GetRackPositionArray(const aRacks, aPositions: string;
    out oRackArray, oPosArray: TArray<string>): integer;
begin
    if GetStringArrayDefLength(aPositions, oPosArray) then
    begin
        // Positionen sind ein Array: Arraylänge wird durch die Positionen bestimmt
        oRackArray := GetStringArrayOfPos(aRacks, Length(oPosArray));
        EXIT(Length(oPosArray));
    end
    else
    begin
        if GetStringArrayDefLength(aRacks, oRackArray) then
        begin
            // Neubestimung des Positions-Arrays: Arraylänge wird durch das Rack-Array bestimmt
            oPosArray := GetStringArrayOfPos(aPositions, Length(oRackArray));
        end;

        EXIT(Length(oPosArray));
    end;
end;

class function TMethodGUIParser.GetStringArrayDefLength(const aValue: string;
    out oPosArray: TArray<string>): boolean;
begin
    if POS('[', aValue) > 0 then
    begin
        // das Ergebnis ist ein Array
        oPosArray := TArrayUtils.StringToStringArray(Copy(aValue, 2, Length(aValue) - 2), ',');
        EXIT(true);
    end
    else
    begin
        // Das Ergebnis ist ein Einzelwert
        SetLength(oPosArray, 1);
        oPosArray[0] := aValue;
        EXIT(false);
    end;
end;

class function TMethodGUIParser.GetStringArrayOfPos(const aValue: string; const aNoOfPos: integer)
    : TArray<string>;
var
    x: integer;
begin
    if POS('[', aValue) > 0 then
    begin
        // das Ergebnis ist ein Array
        result := TArrayUtils.StringToStringArray(Copy(aValue, 2, Length(aValue) - 2), ',');
        if (Length(result) < aNoOfPos) then
            raise Exception.Create('Array does not fit to number of positions: ' + aValue);
    end
    else
    begin
        // Das Ergebnis ist ein Einzelwert
        SetLength(result, aNoOfPos);
        for x := 0 to high(result) do
            result[x] := aValue;
    end;
end;

class function TMethodGUIParser.GetIntegerArrayOfPos(const aValue: string; const aNoOfPos: integer;
    aDefault: integer): TArray<integer>;

var
    x: integer;
begin
    if POS('[', aValue) > 0 then
    begin
        // das Ergebnis ist ein Array
        result := TArrayUtils.StringToIntArrayDef(Copy(aValue, 2, Length(aValue) - 2), ',', aDefault);
        if (Length(result) < aNoOfPos) then
            raise Exception.Create('Array does not fit to number of positions: ' + aValue);
    end
    else
    begin
        // Das Ergebnis ist ein Einzelwert
        SetLength(result, aNoOfPos);
        for x := 0 to high(result) do
            result[x] := StrToIntDef(aValue, aDefault);
    end;
end;

class function TMethodGUIParser.GetTipArrayOfPos(const aValue: string; const aNoOfPos: integer)
    : TArray<integer>;
var
    x: integer;
begin
    if POS('[', aValue) > 0 then
    begin
        // das Ergebnis ist ein Array
        result := TArrayUtils.StringToIntArray(Copy(aValue, 2, Length(aValue) - 2), ',');
        if (Length(result) < aNoOfPos) then
            raise Exception.Create('Array does not fit to number of positions: ' + aValue);

        // von 1-basiert auf 0-basiert herabsetzen
        for x := 0 to high(result) do
            result[x] := result[x] - 1;

    end
    else
    begin
        SetLength(result, aNoOfPos);

        // Werte = -1
        for x := 0 to high(result) do
            result[x] := -1;
    end;
end;

{ TRunGUIParser }

function gmMakeRackPos(aRackName: string; aRackPos: integer): TRackPosition;
begin
    result.Rack := aRackName;
    result.Pos := aRackPos;
end;


end.
