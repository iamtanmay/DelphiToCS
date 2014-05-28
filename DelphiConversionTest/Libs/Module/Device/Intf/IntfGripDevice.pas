{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : (common) interdevice for Gripper devices, EntireGripper - device
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  08.06.04 wl                               TN1963   initial version
  15.06.04 wl  ReadBcAndScan/Turn           TN1963   neu
  15.06.04 wl  OpenGripper/CloseGripper     TN1963   ersetzen MoveGripper/MoveGripperErrDisabled
  21.06.04 wl  MoveTubeTo/FromPos           TN1963   mit Leben gefüllt
  01.07.04 pk  MoveTubeToPos/FromPos        TN1963   Use H_XStart value
  01.07.04 pk  DeviceInit                   TN1963   New: initializes interface
  08.07.04 wl  ZMotorDisableError,HasError  TN2019   neu: für PR-Tips/Disp.Tips
  13.07.04 tbh SingleExecute                TN2019   neu: für PR-Tips/Disp.Tips
  26.07.04 pk                               TN2052   New functions: HasRMotor, GetCurrentRMotorPos_Steps, MoveToPosition, MoveInDimension
  26.07.04 pk                               TN2053   DoGetPlate, DoPutPlate : Implemented
  02.08.04 pk  DeviceGetLocationSize        TN2053   Do not check index when error exists
  03.08.04 wl  DoGetPlate,DoPutPlate          TN2063.1 --> DevicesGrpArm
  03.08.04 wl  MovePlateToPos1,ToPos2,FromPos TN2063.1  jetzt public (neu in IGripperSystem)
  03.08.04 wl  PlateCheck,RetakePlateAfterGet TN2063.1  neu! In TEntireGripperDevice nur Dummy
  11.09.04 wl  MoveZ,MoveZ_WashRetract      TN2123   neu: Parameter aDest als MPosArray!!!
  28.09.04 wl  MoveTubeToPos2,-FromPos1     TN2154   aToolData.ZDrop wird bei PutTube mitgerechnet
  05.10.04 pk  MoveTubeFromPos2             TN2171   Set Global error if gripper error is detected
  08.10.04 pk  MoveTubeFromPos2             TN2171   Call Sam Error box
  18.10.04 wl  MoveAwayNecessary            TN2183   Abfrage, ob der konkurrierende Arm weggefahren werden muß
  27.10.04 pk  MoveTubeFromPos2             TN2197   Check Grip Sensor state and Set Global error if not ON
  10.11.04 wl  MoveToHomePosition           TN2214   neu: alle Motoren werden auf 0 gefahren (wie Init, aber mit Barrier-Movement)
  29.03.05 pk                               TN2358   New: AssignMotorEvents, IsXMotorConflict
  03.06.05 wl  HasXReverseCoord             TN2436   hier nur Dummy
  21.06.05 pk  HasXReverseCoord             TN2464.3  Removed
  21.06.05 pk                               TN2464.3  XConflict --> DevicesConflictManager
  21.06.05 pk  GetZTravelSteps              TN2464.3  Changed to GetWorldZTravelSteps
  09.11.05 wl  MoveHXY                      TN2728    Neuer Parameter aOptions: TMoveXYMovementOptions statt UseCorridor
  09.11.05 wl  CalculatePipSteps            TN2728    Rack und RackPos werden hier auch beschrieben
  05.12.05 wl  DeviceGetLocationSize        TN2816    kein ASSERT im Simulationsmodus
  25.03.06 pk  Init                         TN2997    Init is now in TDevice so must use override
  31.03.06 pk                               TN3009    Gripping values changed from steps to mm
  31.03.06 pk  TPneumaticGripDevice         TN3009    New
  03.04.06 pk  Init                         TN2997   new parameter : aInitID
  06.04.06 pk  GetCurrentDistance           TN3009   Determine distance based on switch states
  07.04.06 pk  Move, Read                   TN2958   New : needed by MoveCtrl
  05.05.06 pk  Init                         TN2958   OpenFull changed to CloseFull
  08.05.06 pk  GetCurrentDistance           TN2958   Take the case into account where the switch is in an off state
  09.05.06 pk  DetermineClosestDest         TN2958   always return a result
  05.07.06 pk  DetermineClosestDestFromDistSwitch    TN3180 keep track of switch with max/min distance in order to find correct switch
  26.01.07 pk  TSimpleGripDevice            TN3503   removed
  12.03.07 pk  GetCurrentDistance           TN3628   handel the case where no default port
  09.11.07 pk  LocalToWorld, WorldToLocal   TN3924   New
  07.01.08 pk  SetToMinPosIfIsBeyondMin     TN3864   New
  03.06.08 pk  IsGripMoveAllowed            TN4133   New
  17.11.08 pk  GetToolOwnerID               TN4280   New
  12.12.08 wl                               TN4363   Kommentar eingefügt
  13.03.09 wl  TGrippedContainerInfo        TN4460   Neue Klasse, die Infos zu gegriffenen Container zu speichern
  13.03.09 wl  IGripDevice.GrippedContainer TN4460   neue Property
  17.03.09 wl  TGrippedContainerInfo        TN4460   neu: CurrentGripDevicePosition
  24.03.09 pk  IGripDevice                  TN4460   New IsGripperAvailable
  12.09.09 wl                               TN4740   MPOS durch integer ersetzt
  19.05.10 wl  IGripDevice                  TN5115   neue Property DefinedIncrementd
  02.11.10 wl  RemoveGrippedContainer       TN5323   neu
  04.04.11 wl  HasGripperMotorWithThisMotorID  TN5525    neu
  28.06.12 wl  GetPosition                  TN5527   ersetzt Read-Funktion: Get und Read möglich
  04.03.14 tp  SetZPos, EnableVSpanCorrection, SetVSpanFactor,
  Move(aDest: TPosMM; aDirection: integer; aExec: EXEC_MODE) TN6367   ZPosition updater, Flag und Factor fuer Varispan Korrigierung.
  06.03.14 tp Move(aDest: TPosMM; aDirection: integer; aExec: EXEC_MODE; aSpeed: integer)
  GetVariSpanCorrectionSpeed()              TN6372   Wegen Geschwindigkeit andergung von Varispan Korrigierung
  -------------------------------------------------------------------------------------------------- }

unit IntfGripDevice;


interface


uses
    IntfDevice,
    Driver,
    AppTypes,
    CommonTypes,
    IntfMotorDevice;

type
    TGripDevicePosition = (gcpApproach, gcpStartPosition, gcpGripPosition);

    TGrippedContainerInfo = class
    private
        fCurrentGripDevicePosition: TGripDevicePosition;
    public
        constructor Create();
        property CurrentGripDevicePosition: TGripDevicePosition read fCurrentGripDevicePosition
            write fCurrentGripDevicePosition;
    end;

    TGrippedRackInfo = class(TGrippedContainerInfo)
    private
        fSourceRackName: string;
    public
        constructor Create(aSourceRackName: string);
        property SourceRackName: string read fSourceRackName;
    end;

    TGrippedTubeInfo = class(TGrippedContainerInfo)
    private
        fSourceRackName: string;
        fSourcePosition: integer;

        fCurrentRackName: string;
        fCurrentPosition: integer;
        fCurrentUseZPutOffset: boolean;
        fCurrentRotation: TPosMM;
        fCurrentAddZOffset: TPosMM;
    public
        constructor Create(aSourceRackName: string; aSourcePosition: integer; aUseZPutOffset: boolean;
            aRotation: TPosMM; aAddZOffset: TPosMM);

        procedure Refresh(aCurrentRackName: string; aCurrentPosition: integer; aUseZPutOffset: boolean;
            aRotation: TPosMM; aAddZOffset: TPosMM);

        property SourceRackName: string read fSourceRackName;
        property SourcePosition: integer read fSourcePosition;
        property CurrentRackName: string read fCurrentRackName;
        property CurrentPosition: integer read fCurrentPosition;
        property CurrentUseZPutOffset: boolean read fCurrentUseZPutOffset;
        property CurrentRotation: TPosMM read fCurrentRotation;
        property CurrentAddZOffset: TPosMM read fCurrentAddZOffset;
    end;

    IGripDevice = interface(IDevice)
        ['{55E7CD93-F126-4184-81A4-E4CF21DE1B1E}']
        function GetGrippedContainer(): TGrippedContainerInfo;
        procedure SetGrippedContainer(aValue: TGrippedContainerInfo);
        procedure RemoveGrippedContainer();
        function GetToolData: TTubeToolData;
        function GetToolIsRack: boolean;
        function GetVCheck(): integer;
        function GetVariSpanCorrectionSpeed(): integer;
        function HasTool(): boolean;
        function GetToolOwnerID(): cardinal;
        procedure ResetToolData;
        procedure SetToolData(aNewToolName: string; aToolIsRack: boolean; aToolData: TTubeToolData;
            aToolOwnerID: cardinal);
        procedure CloseGripper(aDest: TPosMM; aRack: boolean);
        procedure OpenGripper(aDest: TPosMM; aErrDisabled: boolean);
        procedure OpenFull(aErrDisabled: boolean);
        procedure CloseFull();
        function IsGripError(): boolean;
        function IsGripped: boolean;
        function IsPosWithinRange(aPos: TPosMM): boolean;
        function IsGripperAvailable(): boolean;
        function GetIsGripMoveAllowed(): TOnMoveAllowed;
        function HasGripperMotorWithThisMotorID(aMotorID: integer): boolean;
        function GetDefinedIncrement: TPosMM;
        procedure SetDefinedIncrement(aValue: TPosMM);
        property DefinedIncrement: TPosMM read GetDefinedIncrement write SetDefinedIncrement;

        property Tool: TTubeToolData read GetToolData;
        property ToolIsRack: boolean read GetToolIsRack;
        property VCheck: integer read GetVCheck;
        property IsGripMoveAllowed: TOnMoveAllowed read GetIsGripMoveAllowed;
        property ToolOwnerID: cardinal read GetToolOwnerID;
        property GrippedContainer: TGrippedContainerInfo read GetGrippedContainer write SetGrippedContainer;

        // Methoden, die nur in MoveControl benötigt werden:
        procedure Move(aDest: TPosMM; aDirection: integer); overload;
        procedure Move(aDest: TPosMM; aDirection: integer; aExec: EXEC_MODE); overload;
        procedure Move(aDest: TPosMM; aDirection: integer; aExec: EXEC_MODE; aSpeed: integer); overload;
        procedure SetZPos(aZPosSteps: integer);
        procedure EnableVSpanCorrection(aEnable: boolean);
        procedure SetVSpanFactor(aFactor: integer);
        function GetPosition(aRead: boolean): TPosMM;
        function SetToMinPosIfIsBeyondMin(var vValue: TPosMM): boolean;
        function SetToMaxPosIfIsBeyondMax(var vValue: TPosMM): boolean;
        function GetUnitsFromSteps(aSteps: integer): TPosMM;
        function LocalToWorld(aUnits: TPosMM): TPosMM;
        function WorldToLocal(aUnits: TPosMM): TPosMM;
        function GetStepsFromUnits(aUnits: TPosMM): integer;
    end;


implementation


{ TGrippedContainerInfo }

constructor TGrippedContainerInfo.Create;
begin
    inherited Create;
    fCurrentGripDevicePosition := gcpApproach;
end;

{ TGrippedRackInfo }

constructor TGrippedRackInfo.Create(aSourceRackName: string);
begin
    inherited Create;
    fSourceRackName := aSourceRackName;
end;

{ TGrippedTubeInfo }

constructor TGrippedTubeInfo.Create(aSourceRackName: string; aSourcePosition: integer;
    aUseZPutOffset: boolean; aRotation: TPosMM; aAddZOffset: TPosMM);
begin
    inherited Create;
    fSourceRackName := aSourceRackName;
    fSourcePosition := aSourcePosition;

    fCurrentRackName := aSourceRackName;
    fCurrentPosition := aSourcePosition;
    fCurrentUseZPutOffset := aUseZPutOffset;
    fCurrentRotation := aRotation;
    fCurrentAddZOffset := aAddZOffset;
end;

procedure TGrippedTubeInfo.Refresh(aCurrentRackName: string; aCurrentPosition: integer;
    aUseZPutOffset: boolean; aRotation, aAddZOffset: TPosMM);
begin
    fCurrentRackName := aCurrentRackName;
    fCurrentPosition := aCurrentPosition;
    fCurrentUseZPutOffset := aUseZPutOffset;
    fCurrentRotation := aRotation;
    fCurrentAddZOffset := aAddZOffset;
end;


end.
