{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Uses a Run Record to create a TAction Object.
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  04.02.04 pk                                TN1719   New
  11.02.04 pk  CreatePortAction              TN1719   Parsing bug corrected
  19.02.04 pk  CreateCallDLLFuncAction       TN1719   Parsing bug corrected
  02.03.04 wl  CreateTubeAction              Tn1773   EventList for tube event is createted and filled with info from Option-string
  02.03.04 wl  CreateTubeAction              Tn1773   EventList.Free -->  TTubeAction.Destroy (Action.pas)
  04.03.04 pk  CreateAction                  TN1789   if action is in gDontPutBackToolForActions remove the atPutTool attribute
  02.04.04 wl  GeneratePipSeq                TN1788   xUsedArm wird zunächst mit gmDefineArmAndWorktype ermittelt
  02.04.04 wl  CheckSkipTips                 TN1788   Abfrage (aUsedArm is TPipArmDevice) statt IsHandlerTipmap
  02.04.04 wl  CheckSkipTips                 TN1788   TipOK wird mit xUsedArm-Instanz aufgerufen
  19.04.04 wl  GeneratePipSeq                TN1788   alle Unterfunktionen herausgelöst
  19.04.04 wl  GeneratePipSeq                TN1788   verschiedene Arme können nicht zusammengefasst werden
  19.04.04 wl  CheckDefinedTip               TN1788   Prüfung auf gleichen TipType -> GeneratePipSeq
  20.04.04 wl  CheckSkipTips                 TN1788   benutzt WB.GetRackFromName
  23.04.04 wl  CheckDefinedTip               TN1788.8 LogDisplay: 'CheckDefinedTip =true' wird nicht angezeigt (false dagegen schon)
  30.04.04 wl  CreateLayoutReloadAction      TN1858   neu: 'LOADL' (Reload layout)
  30.04.04 wl  CreateGripperToolReturnAction TN1891   neu: 'TOOLR' (Return gripper tool)
  03.05.04 wl  GeneratePipSeq                TN1788   wenn (Action <> '') darf UsedArm gar nicht bestimmt werden
  10.05.04 pk  CreateFillDitiRackAction      TN1851   Bug: used SRack instead of Options
  10.05.04 pk  CreateFillRackWithCapsAction  TN1851   Bug: used Options instead of SRack
  10.05.04 pk  CheckSkipTips                 TN1889.2 uses WB.FindXRackPos
  15.05.04 pk  GeneratePipSeq                TN1889.1 uses racknames and rackpos to make a TDilutionRackPositions record
  15.05.04 pk  GeneratePipSeq                TN1934   raise exception when sequence could not be generated
  17.05.04 pk  GeneratePipSeq                TN1937   when the next step is not a pipette action don't raise an Assertion
  18.05.04 pk  CreateImportFileIntoTableAction TN1941 Creates TImportDataIntoTableAction if the Import Wizard options are included in the options field
  03.06.04 pk  CreateDetectedPosCancelAction TN1969   Pass WB.Layoutname as argument
  08.06.04 pk                                TN1976   Action name constants moved to TMethodGUIParsing
  08.06.04 pk  CreateLiveMethodBuildAction   TN1974.0 new
  08.06.04 pk  CreateRunLoadAction           TN1974.0 new
  08.06.04 pk  CreateAction                  TN1976   uses TMethodGUIParser.MethodActionIsUnsafe
  09.06.04 pk  DetermineDilRackType          TN1978   --> MethodGUIParsing
  14.06.04 pk  CreateLayoutEditAction        TN1979   new
  15.06.04 pk  CreateSQLUpdateAction         TN1980.0 parsing now done in MethodGUIParsing
  15.06.04 pk  CreateSQLWriteFileAction      TN1980.0 parsing now done in MethodGUIParsing
  17.06.04 pk  CreateWashProgAction          TN1991   new
  17.06.04 pk  CreateReagentRunLoadAction    TN1993   new
  17.06.04 pk  CreateRunMarkerAction         TN1974   new
  17.06.04 pk  ObtainDilutionRackPositions   TN1995   Change the Diluent when a Diluent is found in the DilRack
  18.06.04 pk  CreateSQLWriteFileAction      TN1996   uses new FileSubPath parameter
  29.06.04 pk                                TN2009.8 Uses ActionLow
  05.06.04 pk                                TN2018   Parsing of CreateDeviceAction, CreateTubeAction --> MethodGUIParsing
  06.06.04 pk  CreateDeviceAction            TN2018   Changes due to Changes in MethodGUIParsing
  02.08.04 pk  CreateDelayAction             TN2069   Creates the Sophas delay action
  03.08.04 wl  CreateRackMoveAction          TN2063.1 xEventList wird aus Options geparst und weitergegeben
  03.08.04 wl  CreateReadRackBarcodeAction, CreateCheckRackBarcodeAction, CreateRackCheckAction   TN2063.1 dito
  04.08.04 pk  CreateReadRackBarcodeAction   TN2080   uses TMethodGUIParser to parse run record
  09.08.04 wl  CreateReadRackBarcodeAction   TN2084   DefaultSlot = STR_CARRIER_SAME, da Ziel nicht angegeben werden muss
  08.09.04 pk  CreateImportFileIntoTableAction TN2126 passes ImportFilter option to action
  11.09.04 wl  GeneratePipSeq                TN2123   Geänderte property von Arm: FirstUsedTipName statt FirstUsedTip
  16.09.04 wl  CreateAction                  TN2136.2 TRunRecHandle durch TRunRecord ersetzt, da man sonst nicht debuggen kann
  04.11.04 wl                                TN2213   uses MethodTypes
  04.11.04 wl  CreateImportFileIntoTableAction  TN2213   benutzt ImportIntoTableActionRunRecFromRunRec statt ImportActionRackFromGUIRec
  04.11.04 wl  CreateSQLUpdateAction         TN2213   benutzt SQLUpdateActionRunRecFromRunRec statt USQLActionOptionsFromStr
  04.11.04 wl  CreateSQLWriteFileAction      TN2213   benutzt SQLWriteActionRunRecFromRunRec statt WRSQLActionOptionsFromStr
  04.11.04 wl  CreateTubeAction              TN2213   TTubeActionRunRec ersetzt TTubeParameters
  08.11.04 wl  CreateTubeAction,CreateReadRackBarcodeAction        TN2213   TSamplerEventList wird hier nicht mehr erzeugt
  08.11.04 wl  CreateRackCheckAction,CreateCheckRackBarcodeAction  TN2213   TSamplerEventList wird hier nicht mehr erzeugt
  08.11.04 wl                                TN2213   WB.FindXRackPos - ohne RecCnt-Parameter
  10.11.04 wl  CreateCallDllFuncAction       TN2213   Parsing --> MethodGUIParsing
  10.11.04 wl  CreateFlushAction             TN2213   Parsing --> MethodGUIParsing
  10.11.04 wl  CreateWashAction              TN2213   Parsing --> MethodGUIParsing
  10.11.04 wl  CreateParamStoreAction        TN2213   Parsing --> MethodGUIParsing
  16.11.04 pk  CreateRunStartAction          TN2231   New: create TRunStartAction
  17.11.04 pk                                TN2231    uses RunDataAdaptorExt
  22.11.04 pk  CreateNextAction              TN2237   from TBasicLiveRunAction
  22.11.04 pk  CreateWashTipsAction          TN2237   New: create TWashTipsAction
  22.11.04 pk                                TN2213   New function names used from MethodGUIParsing
  29.11.04 wl  CreateVortexerTempSetAction   TN2246.1 Parsing --> MethodGUIParsing
  29.11.04 wl  CreateVortexerTempCheckAction TN2246.1 Parsing --> MethodGUIParsing
  29.11.04 wl  CreateVortexerSpeedAction     TN2246.1 Parsing --> MethodGUIParsing
  29.11.04 wl  CreateVortexerFixationAction  TN2246.1 Parsing --> MethodGUIParsing
  29.11.04 wl  CreateFileCopyAction          TN2246.1 Parsing --> MethodGUIParsing
  29.11.04 wl  CreateWaitForSlaveAction      TN2246.1 Parsing --> MethodGUIParsing
  29.11.04 wl  CreateTimerSetAction          TN2246.1 Parsing --> MethodGUIParsing
  29.11.04 wl  CreateMessageWithBeeperAction TN2246.1 Parsing --> MethodGUIParsing
  18.02.05 pk  CreateCallDeviceFuncAction    TN2322   New : Create TCallDeviceFuncAction
  28.02.05 pk  CreateGroupActionFromRunStep  TN2314.1 New
  28.02.05 pk  GeneratePipSeq                TN2314.1 Param changed from TRunDataAdaptor to IRunRecIterator
  08.03.05 pk  CreateCallDLLFuncAction       TN2337   Passes TCallDllStruct arg to TCallDLLFuncAction.Create
  16.03.05 pk  Create                        TN2353   New : TActionFactory can now be intantiated
  16.03.05 pk                                TN2353   functions used to create action from runstep moved to ExecFactory
  16.03.05 pk  CreateRunLoadAction           TN2352.1 requires TRunCallStack
  06.04.05 pk                                TN2373   Create all actions with a TRunStep
  11.04.05 pk CreatePipetteActionFromRunStep TN2373   Create PipetteList using JobArray
  15.04.05 wl  CreateFlushAction             TN2379   Flush-Action um 2 Parameter erweitert
  20.04.05 wl  CreatePipetteAction           TN2377   erzeugt TPipetteAction, TCalliAction oder TSimpleCalliAction
  20.04.05 wl                                TN2377   casts für TPipetteActions geändert in TJobArrayAction
  04.05.05 wl  CreateManualFillAction        TN2410   "MANUAL"-Schritt als eigene Action
  04.05.05 wl  CreateNextAction              TN2410   Manual-Action wird nicht wie Pipettieraktion behendelt, sondern als Action
  14.06.05 pk  GeneratePipSeq                TN2464.2 Returns PipRunRec, UsedArm, LiqH, Multipip parameters
  14.06.05 pk  GeneratePipSeq                TN2464.2 Contains code which was previously in DilutionManager
  14.06.05 pk  CreateFlushAction, etc.       TN2464.1 Pass UsedArm option to action constructors
  20.06.05 tbh SumUpDilSteps                 TN2385   neu: summiert Zielvolumen für Volumenkorrektur im Multi-Dispense-Modus
  20.06.05 tbh CreatePipetteAction           TN2385   ruft SumUpDilSteps
  22.06.05 tbh SumUpDilSteps                 TN2385   Prüfung auf Volumekorrektur früher
  23.06.05 tbh SumUpDilSteps                 TN2385   Volumenkorrektur nur für existierende Pumpen
  24.06.05 wl  CreateActionFromArgs          TN2459   bei MSGB wird TMessageAction statt TMessageWithBeeperAction gestartet
  27.06.05 pk  GeneratePipSeq                TN2477   New parameter : LastDilution - Set LastDilution = true when last record reached
  27.06.05 pk  CreatePipetteAction           TN2477   Check for last record moved to GeneratePipSeq because MovePrior in GeneratePipSeq resets the last record flag before we checked it 27.06.05 pk  GeneratePipSeq                TN2477   New parameter : LastDilution - Set LastDilution = true when last record reached
  01.08.05 wl                                TN2503   IsPipetteOrCalliAction/IsPipetteOrCalliOrManualFillAction ersetzen ungenaue Abfrage IsPipetteAction
  15.08.05 pk                                TN2560   TMethodGUIParser changed to TRunGUIParser
  18.08.05 pk  CheckDefinedTip               TN2556.1 Changed to allow Source and Destvols to be different
  22.08.05 pk  CreateRunLoadAction           TN2546   New option : DoRepeat
  22.08.05 wl                                TN2558.8  uses ScheUtil entfernt, ScheUtil-Methoden werden mit TDMScript. oder global. aufgerufen
  26.08.05 pk  GeneratePipSeq                TN2566   Set the DTransAir property for each job if the TransAir option is set
  14.09.05 wl  CreateBalanceWaitAction       TN2570   erzeugt TBalanceWaitAction
  11.10.05 wl  CreateGripperToolGetAction    TN2658  new action that gets a rack with the gripper as tool
  15.10.05 wl  ChangeWashOrWasteDestRackPos  TN2672   benutzt die seltsame Funktion TRackDefinitions.RackIsMaybeWashOrWaste
  20.10.05 wl  CreateGripperToolGetAction    TN2673   jetzt mit Events: aEvBeforeGet, aEvAfterGet
  20.10.05 wl  CreateGripperToolReturnAction TN2673   mit Events, auch ein Tool-Name kann angegeben werden
  20.10.05 wl  CreateTipsGetAction           TN2659   neue Action zum Aufnehmen von Spitzen
  20.10.05 wl  CreateTipsReturnAction        TN2660   neue Action zum Abwerfen von Spitzen
  08.11.05 wl  CreateXYMovementAction        TN2728   für neue Action
  08.11.05 wl  CreateZposMovementAction      TN2728   für neue Action
  17.11.05 wl                                TN2771   alle RackActions mit GripperArmName
  20.11.05 wl  CreateChangeTipsAction        TN2784   creates ChangeTipsAction
  24.11.05 pk                                TN2805    ObjSampl replaced by ZARunnerObjects
  13.12.05 wl  CreateFileCopyAction          TN2856   neue Parameter Overwrite, DeleteSource
  02.01.06 wl  CreateSQLWriteFileAction      TN2876    neuer Parameter OutputFileName
  05.01.06 pk                                TN2877   New : RackActionRunRecFromRunRec, VerifyArmName
  18.01.06 wl  CreateSQLSelectAction         TN2885    neu: SLSQL
  02.02.06 wl  CreateSQLSelectAction         TN2885.1  neu: DefaultVaues
  08.03.06 thr CreateBalanceTareAction       TN2941    BalanceName
  08.03.06 thr CreateBalanceWaitAction       TN2941    BalanceName
  08.03.06 thr CreateWeighPositionAction     TN2941    BalanceName
  15.03.06 wl  CreateZPosMovementAction      TN2966    neuer Parameter: DisableError
  25.03.06 pk  CreateInitDeviceAction        TN2997    Create a TInitDeviceAction
  25.03.06 pk  CreateRunLoadAction           TN2998    New parameter: NumLinesToRepeat
  25.03.06 pk  CreateNextAction, etc         TN2999    changed from class function to regular - avoid passing DilutionManager and CallStack as arguments
  29.03.06 wl  CreateTimerWaitAction         TN3002    WaitTime wird aus Feld Sourcerack gelesen
  29.03.06 wl  CreateXYMovementAction        TN3005    Neue Parameter: XSpeed,xRamp,YSpeed,YRamp
  29.03.06 wl  CreateZPosMovementAction      TN3006    Neuer Parameter: ZRamp
  03.04.06 thr CreateWeighPositionAction     TN3007    Additional Parameters
  03.04.06 thr CreateBalanceTareAction       TN3007    Additional Parameters
  10.04.06 pk  CreateInitAction              TN3031    pass true as FullInit argument
  12.04.06 pk  CheckSkipTips                 TN2958    MAXTIPS replaced by aPipDevice.TipCount
  20.04.06 pk  GenerateMoveSeq               TN3054    Used by CreateXYMov and CreateZPos to do action for more than one tip
  21.04.06 wl  SumUpDilSteps                 TN3051    VolCorrFactor ist extended
  24.04.06 pk  CreateFlushAction             TN3067    uses Tipmap instead of UsedArm
  08.05.06 pk  CreateMultiTipActionFromRunStep TN3087  replaces CreatePipActionFromRunStep and also creates XYMov and ZPos actions
  08.05.06 pk  CreateXYMovementAction        TN3087    use IRunRecIterator instead of RunDataAdaptor
  16.05.06 wl  CreateTimerWaitAction         TN3101    Runname wird nicht mehr übergeben
  16.05.06 wl  CreateTimerSetAction          TN3101    Runname wird nicht mehr übergeben
  29.05.06 pk  CreateRunStartAction          TN3120    Dont build if pipflag = RE
  30.05.06 pk  CreateAction                  TN3120    Create runstep using runstepfactory
  06.06.06 wl  CreateTimerWaitAction         TN3128    benutzt DialogText, Priority
  07.09.06 wl  CreateWeighWizardAction       TN3287    unterscheidet sich jetzt von CreatePipetteAction
  07.09.06 wl  CreatePipetteAction           TN3287    Worktype "Calli" entferntn, SimpleCalli -> CreateWeighWizardAction
  20.09.06 wl  CreateMotorMoveAction         TN3318    Neu für MOTOR action
  26.10.06 pk  CheckSkipTips                 TN3383    Check if desination position is reachable
  31.10.06 pk  CreateMessageAction           TN3391    New parameter: PauseRun
  06.11.06 wl  CreateSensorCheckAction       TN3394    Für Neue Action: CSENS
  28.11.06 wl  CreateImportIntoTableAction   TN3397    neue Parameter SourceFileName und StoreKeyNumVal
  05.12.06 wl                                TN3448    Actions WASH und WASH? entfernt
  07.12.06 wl  CreateActionFromArgs          TN3409    MethodActionIsUnsafe entfernt
  09.12.06 pk  ErrBoxPip                     TN3458    Error should be shown in errorbox instead of raising exception
  18.01.07 wl  CreateVortexerSpeedAction     TN3507    VortexerSpeed: neuer Parameter WaitFor
  19.02.07 wl  CreateVirtualRackMoveAction   TN3585    erzeugt TVirtualRackMoveAction
  21.02.07 wl  CreateTipsGetAction           TN3588    Kann jetzt für Ditis benutzt werden
  06.03.07 wl  CheckSkipTips                 TN3621    SkipAllowed-Funktion entfernt
  07.03.07 wl                                TN3620    TCustomArmDevice durch TRobotArmDevice ersetzt
  12.03.07 pk  CreateMoveRackAction          TN3629    new MoveOptions parameter
  23.03.07 pk  GetTipsNeedingWash            TN3546    removed
  23.07.07 wl  CreateWashProgAction          TN3792    benutzt jetzt Einzelschritte
  24.07.07 wl  GenerateWashProgSeq           TN3792    neu: Kann mehrere Schritte zur Laufzeit zusammenfassen
  25.07.07 wl  GenerateWashProgSeq           TN3792    Fehler korrigiert
  26.07.07 pk  CreateCommandAction           TN3805    uses TCommandActionRunRec
  30.08.07 pk                                TN3840.2  Uses LiqHTypes
  09.11.07 pk                                TN3923    class references to TErrorMessageFactory changed to gErrorMessageFactory
  13.11.07 wl  CreatePowderDetectionAction   TN3844    neu für PWDET
  09.01.08 wl                                TN3972    endgültig entfernt: CANCM,CHKE,EDITL,EXPRA,IMPRA,PHOTO,PORT,QUADR,READE,WFSLA
  09.01.08 wl                                TN3972    statt TRunGUIParser-Funktionen werden RunStepInfo-Funktionen benutzt
  29.01.08 wl                                TN3980    neu: TriggerRunStep, MemDeviceReadRunStep, MemDeviceWriteRunStep
  19.02.08 wl  GeneratePipSeq                TN4009    Action muss PIPET, ASP oder DISP sein, sonst Abbruch
  27.02.08 wl  CreateZTipMovementAction      TN4011    new for ZTIPM action
  07.04.08 wl  CreateZTipMovementAction      TN4057    new parameter ErrMsg
  25.04.08 wl  PumpAspirate/DispenseMethodStep   TN4050    neu für PUMPA, PUMPD
  25.04.08 wl  VolMotorMethodStep            TN4051    neu für MOTOR (auf VolMotor reduziert)
  25.04.08 wl  MotorMoveMethodStep           TN4051    für neue Action MMOVE
  30.04.08 wl  GenerateMoveSeq               TN4091    Break up if the same tip is used twice
  16.05.08 pk  GenerateMoveSeq               TN4111    Set DRack to SRack for Asp action, and SRack to DRack for Dsp Action
  19.05.08 wl  GenerateMoveSeq               TN4117    oMultiPip could be undefined and was sometimes set to true without reason - fixed
  20.06.08 pk                                TN4139    WB global object replaced by LayoutManager
  03.07.08 wl                                TN4157    all actions changed that uses USEDTIPS -> USEDTIPS & USEDPIPDEVICE
  09.07.08 pk  GeneratePipSeq                TN4157    Bug fixes
  09.07.08 pk  GenerateMoveSeq               TN4157    Bug fixes
  21.07.08 pk  CreateChangeCarrierTypeAction TN4179    New
  31.07.08 pk  CreateChangeRackTypeAction    TN4193    New
  11.08.08 pk  CreateChangeTipsAction        TN4157    TipIndex + 1
  04.11.09 pk                                TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  04.02.10 pk                                TN4972    Changes for Restart
  14.08.13 wl                                TN6218   verwendet TRunStepListIterator
  -------------------------------------------------------------------------------------------------- }

unit RunActionFactory;


interface


uses
    AppTypes,
    Action,
    RunAction,
    RunStep,
    RunStepFactory,
    RunActionTypeInfo;

type
    TRunActionFactory = class
    public
        constructor Create();
        function CreateNextAction(aRunStepListIterator: TRunStepListIterator): TRunAction;
        class function FindTypeInfoByRunStep(aRunStep: TRunStep): TRunActionTypeInfo;
    end;


implementation


uses
    SysUtils,
    MethodGUIParsing,
    ErrorManager,
    TypeInfo,
    RunActionTypeDictionary;

{ TRunActionFactory }

constructor TRunActionFactory.Create();
begin
    inherited Create;
end;

class function TRunActionFactory.FindTypeInfoByRunStep(aRunStep: TRunStep): TRunActionTypeInfo;
var
    xTypeInfo: TTypeInfo;
    xRunActionTypeInfo: TRunActionTypeInfo;
begin
    result := nil;
    for xTypeInfo in TRunActionTypeDictionary.Instance.TypeInfos do
    begin
        xRunActionTypeInfo := xTypeInfo as TRunActionTypeInfo;
        if xRunActionTypeInfo.RunActionCreator.IsValideCreatorForRunStep(aRunStep) then
        begin
            result := xRunActionTypeInfo;
            EXIT;
        end;
    end;
end;

function TRunActionFactory.CreateNextAction(aRunStepListIterator: TRunStepListIterator): TRunAction;
var
    xRunStep: TRunStep;
    xTypeInfo: TRunActionTypeInfo;
    xAction: TRunAction;
begin
    result := nil;
    if aRunStepListIterator.IsEof then
        EXIT;

    xRunStep := aRunStepListIterator.CurrentStep;
    if not Assigned(xRunStep) then
        EXIT;
    xTypeInfo := FindTypeInfoByRunStep(xRunStep);
    ASSERT(Assigned(xTypeInfo), 'Invalid Runstep Detected');

    xAction := xTypeInfo.RunActionCreator.CreateRunActionFromStepList(aRunStepListIterator);

    if gErrorManager.IsGlobalErr() then
        EXIT;

    ASSERT(Assigned(xAction), 'Invalid Action Detected');

    result := xAction;
end;


end.
